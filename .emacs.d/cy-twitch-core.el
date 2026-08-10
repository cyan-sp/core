;;; cy-twitch-core.el --- Twitch integration (emotes, live status)  -*- lexical-binding: t -*-

(defvar cy/twitch-emote-dir (expand-file-name "twitch-emotes" user-emacs-directory))
(defvar cy/twitch-emotes (make-hash-table :test 'equal))        ; name -> id
(defvar cy/twitch-emote-images (make-hash-table :test 'equal))  ; id -> image object
(defvar cy/twitch-emote-regex nil)
(defvar cy/twitch-global-emotes-loaded nil)
(defvar cy/twitch-loaded-channels (make-hash-table :test 'equal))

(defconst cy/twitch-client-id "7gwvlykn7fxwyt88v1qay84pu0fjzl")
(defconst cy/twitch-token "2w30g9i0jj0hgqu0f7ez3jojxt7bnv")

(defun cy/twitch-headers ()
  `(("Client-Id" . ,cy/twitch-client-id)
    ("Authorization" . ,(concat "Bearer " cy/twitch-token))))

(defun cy/twitch-emote-path (id)
  (expand-file-name (concat id ".png") cy/twitch-emote-dir))

(defun cy/twitch-register-emotes (emotes)
  (seq-do (lambda (e)
            (puthash (alist-get 'name e) (alist-get 'id e) cy/twitch-emotes))
          emotes)
  (cy/twitch-build-regex))

(defun cy/twitch-build-regex ()
  (when (> (hash-table-count cy/twitch-emotes) 0)
    (let (names)
      (maphash (lambda (name _) (push (regexp-quote name) names)) cy/twitch-emotes)
      (setq cy/twitch-emote-regex
            (concat "\\b\\(" (mapconcat #'identity names "\\|") "\\)\\b")))))

(defun cy/twitch-fetch-image (id)
  (unless (file-exists-p (cy/twitch-emote-path id))
    (make-directory cy/twitch-emote-dir t)
    (url-retrieve
     (format "https://static-cdn.jtvnw.net/emoticons/v2/%s/default/dark/1.0" id)
     (lambda (_)
       (goto-char (point-min))
       (re-search-forward "\r?\n\r?\n")
       (write-region (point) (point-max) (cy/twitch-emote-path id) nil 'silent)))))

(defun cy/twitch-fetch-global-emotes ()
  (unless cy/twitch-global-emotes-loaded
    (setq cy/twitch-global-emotes-loaded t)
    (let ((url-request-extra-headers (cy/twitch-headers)))
      (url-retrieve
       "https://api.twitch.tv/helix/chat/emotes/global"
       (lambda (_)
         (goto-char (point-min))
         (re-search-forward "\r?\n\r?\n")
         (cy/twitch-register-emotes
          (alist-get 'data (json-parse-buffer :object-type 'alist))))))))

(defun cy/twitch-fetch-channel-emotes (channel)
  (let ((url-request-extra-headers (cy/twitch-headers)))
    (url-retrieve
     (format "https://api.twitch.tv/helix/users?login=%s" channel)
     (lambda (_)
       (goto-char (point-min))
       (re-search-forward "\r?\n\r?\n")
       (let* ((data (alist-get 'data (json-parse-buffer :object-type 'alist)))
              (url-request-extra-headers (cy/twitch-headers)))
         (when (> (length data) 0)
           (url-retrieve
            (format "https://api.twitch.tv/helix/chat/emotes?broadcaster_id=%s"
                    (alist-get 'id (aref data 0)))
            (lambda (_)
              (goto-char (point-min))
              (re-search-forward "\r?\n\r?\n")
              (cy/twitch-register-emotes
               (alist-get 'data (json-parse-buffer :object-type 'alist)))))))))))

(defun cy/twitch-ensure-channel-emotes (channel)
  (unless (gethash channel cy/twitch-loaded-channels)
    (puthash channel t cy/twitch-loaded-channels)
    (cy/twitch-fetch-channel-emotes channel)))

(defun cy/twitch-in-twitch-p ()
  (let ((server-buf (and (fboundp 'erc-server-buffer) (erc-server-buffer))))
    (when server-buf
      (with-current-buffer server-buf
        (and (bound-and-true-p erc-session-server)
             (string-match-p "twitch" erc-session-server))))))

(defun cy/twitch-replace-emotes ()
  (when (and cy/twitch-emote-regex (cy/twitch-in-twitch-p))
    (let ((channel (erc-default-target)))
      (when (and channel (string-prefix-p "#" channel))
        (cy/twitch-ensure-channel-emotes (substring channel 1))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward cy/twitch-emote-regex nil t)
        (let* ((name (match-string 0))
               (id (gethash name cy/twitch-emotes))
               (path (cy/twitch-emote-path id)))
          (if (file-exists-p path)
              (add-text-properties
               (match-beginning 0) (match-end 0)
               `(display ,(or (gethash id cy/twitch-emote-images)
                              (let ((img (create-image path nil nil :height 20)))
                                (puthash id img cy/twitch-emote-images)
                                img))))
            (cy/twitch-fetch-image id)))))))

(defvar cy/twitch-live-process nil)
(defvar cy/twitch-current-channel nil)


(defun cy/twitch-kill ()
  "Kill the currently playing Twitch stream."
  (interactive)
  (when (and cy/twitch-live-process (process-live-p cy/twitch-live-process))
    (delete-process cy/twitch-live-process)
    (setq cy/twitch-live-process nil))
  (shell-command "taskkill.exe /F /IM vlc.exe 2>/dev/null")
  (message "Stream stopped."))

(defun cy/twitch-live ()
  "Show live followed Twitch channels, open chat or stream."
  (interactive)
  (let ((url-request-extra-headers (cy/twitch-headers)))
    (url-retrieve
     "https://api.twitch.tv/helix/streams/followed?user_id=939991592&first=100"
     (lambda (_)
       (goto-char (point-min))
       (re-search-forward "\r?\n\r?\n")
       (let* ((json (json-parse-buffer :object-type 'alist))
              (streams (alist-get 'data json)))
         (if (zerop (length streams))
             (message "No followed channels are live.")
           (let* ((choices (mapcar (lambda (s)
                                     (format "%-20s  %s"
                                             (alist-get 'user_name s)
                                             (truncate-string-to-width
                                              (alist-get 'title s) 60 nil nil t)))
                                   streams))
                  (choice (completing-read "Live: " choices nil t))
                  (idx (cl-position choice choices :test #'equal))
                  (stream (aref streams idx))
                  (user (alist-get 'user_login stream))
                  (action (read-char "o=chat  s=stream  q=quit: ")))
             (cond
              ((eq action ?o)
               (setq cy/twitch-current-channel user)
               (cy/twitch-chat user))
              ((eq action ?s)
               (let* ((quality (completing-read "Quality: "
                                                '("best" "1080p60" "720p60" "720p" "480p" "360p" "worst")
                                                nil t nil nil "best"))
                      (opacity (read-string "Opacity (0.1-1.0): " "0.8")))
                 (setq cy/twitch-live-process
                       (start-process "streamlink" nil
                                      "streamlink"
                                      "--player" "/mnt/c/Program Files/VideoLAN/VLC/vlc.exe"
                                      "--player-args" (format "--qt-minimal-view --video-on-top --qt-opacity=%s" opacity)
                                      (concat "https://www.twitch.tv/" user)
                                      quality))))))))))))
(defun cy/twitch-remove-emotes ()
  "Remove all emote image overlays from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (get-text-property (point) 'display)
        (remove-text-properties (point) (1+ (point)) '(display nil)))
      (forward-char 1))))

;;;###autoload
(define-minor-mode cy/twitch-emotes-mode
  "Toggle inline Twitch emote images in ERC buffers."
  :lighter " Emotes"
  (if cy/twitch-emotes-mode
      (progn
        (add-hook 'erc-insert-modify-hook #'cy/twitch-replace-emotes)
        (add-hook 'erc-after-connect #'cy/twitch--on-connect)
        (message "Twitch emotes enabled"))
    (remove-hook 'erc-insert-modify-hook #'cy/twitch-replace-emotes)
    (remove-hook 'erc-after-connect #'cy/twitch--on-connect)
    (cy/twitch-remove-emotes)
    (message "Twitch emotes disabled")))

(defun cy/twitch--on-connect (&rest _)
  (when (cy/twitch-in-twitch-p)
    (cy/twitch-fetch-global-emotes)))

(provide 'cy-twitch-core)
;;; cy-twitch-core.el ends here

(defun cy/twitch-chat (channel)
    "Open Twitch chat for CHANNEL in ERC."
    (interactive "sChannel: ")
    (let ((ch (concat "#" channel))
          (server-buf (get-buffer "twitch")))
      (if (and server-buf
               (with-current-buffer server-buf (erc-server-process-alive)))
          (with-current-buffer server-buf
            (erc-join-channel ch))
        (let ((password (funcall (plist-get
                                  (car (auth-source-search :host "irc.chat.twitch.tv" :user "cyan_vm"))
                                  :secret))))
          (erc-tls :server "irc.chat.twitch.tv"
                   :port 6697
                   :nick "cyan_vm"
                   :password password
                   :id 'twitch)
          (run-with-timer 5 nil
                          (lambda ()
                            (with-current-buffer "twitch"
                              (erc-join-channel ch))))))))
