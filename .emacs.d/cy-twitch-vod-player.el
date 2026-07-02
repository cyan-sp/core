;;; cy-twitch-vod-player.el --- Browse and play Twitch VODs  -*- lexical-binding: t -*-

(defun cy/twitch-vods (channel)
  "List recent VODs for CHANNEL and stream or download selected one."
  (interactive "sChannel: ")
  (let ((url-request-extra-headers
         `(("Client-Id" . ,cy/twitch-client-id)
           ("Authorization" . ,(concat "Bearer " cy/twitch-token)))))
    (url-retrieve
     (format "https://api.twitch.tv/helix/users?login=%s" channel)
     (lambda (_)
       (goto-char (point-min))
       (re-search-forward "\r?\n\r?\n")
       (let* ((data (alist-get 'data (json-parse-buffer :object-type 'alist)))
              (url-request-extra-headers
               `(("Client-Id" . ,cy/twitch-client-id)
                 ("Authorization" . ,(concat "Bearer " cy/twitch-token)))))
         (when (> (length data) 0)
           (let ((user-id (alist-get 'id (aref data 0))))
             (url-retrieve
              (format "https://api.twitch.tv/helix/videos?user_id=%s&type=archive&first=20" user-id)
              (lambda (_)
                (goto-char (point-min))
                (re-search-forward "\r?\n\r?\n")
                (let* ((json  (json-parse-buffer :object-type 'alist))
                       (vods  (alist-get 'data json))
                       (choices (mapcar (lambda (v)
                                          (format "%-12s  %-8s  %s"
                                                  (substring (alist-get 'created_at v) 0 10)
                                                  (alist-get 'duration v)
                                                  (truncate-string-to-width
                                                   (alist-get 'title v) 60 nil nil t)))
                                        vods)))
                  (let* ((choice  (completing-read (format "%s VODs: " channel) choices nil t))
                         (idx     (cl-position choice choices :test #'equal))
                         (vod     (aref vods idx))
                         (url     (alist-get 'url vod))
                         (title   (alist-get 'title vod))
                         (quality (completing-read "Quality: "
                                                   '("best" "1080p60" "720p60" "720p" "480p" "worst")
                                                   nil t nil nil "best")))
                    (start-process "streamlink-vod" nil
                                   "streamlink"
                                   "--player" "/mnt/c/Program Files/VideoLAN/VLC/vlc.exe"
                                   "--player-args" "--qt-minimal-view --video-on-top"
                                   url quality)
                    (message "Playing: %s" title))))))))))))

(provide 'cy-twitch-vod-player)
;;; cy-twitch-vod-player.el ends here
