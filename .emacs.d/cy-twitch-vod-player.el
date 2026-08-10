;;; cy-twitch-vod-player.el --- Browse and play Twitch VODs  -*- lexical-binding: t -*-

(defun cy/twitch--fetch-vods (user-login user-id)
  "Fetch VODs for USER-ID, prompt to select one, then download it showing progress."
  (let ((url-request-extra-headers (cy/twitch-headers)))
    (url-retrieve
     (format "https://api.twitch.tv/helix/videos?user_id=%s&type=archive&first=20" user-id)
     (lambda (_)
       (goto-char (point-min))
       (re-search-forward "\r?\n\r?\n")
       (let* ((vods    (alist-get 'data (json-parse-buffer :object-type 'alist)))
              (choices (mapcar (lambda (v)
                                 (format "%-12s  %-8s  %s"
                                         (substring (alist-get 'created_at v) 0 10)
                                         (alist-get 'duration v)
                                         (truncate-string-to-width
                                          (alist-get 'title v) 60 nil nil t)))
                               vods)))
         (let* ((choice  (completing-read (format "%s VODs: " user-login) choices nil t))
                (idx     (cl-position choice choices :test #'equal))
                (vod     (aref vods idx))
                (url     (alist-get 'url vod))
                (title   (alist-get 'title vod))
                (quality (completing-read "Quality: "
                                         '("best" "1080p60" "720p60" "720p" "480p" "worst")
                                         nil t nil nil "best"))
                (action  (read-char "s=stream  d=download: ")))
           (pcase action
             (?s
              (let* ((buf (format "*vterm:vod:%s*" user-login)))
                (vterm buf)
                (vterm-send-string
                 (format "streamlink --player-passthrough=hls --player %S --player-args %S %s %s\n"
                         "/mnt/c/ProgramData/chocolatey/lib/mpvio.install/tools/mpv.exe"
                         "--no-border --ontop --geometry=50%"
                         url quality))
                (message "Streaming: %s" title)))
             (?d
              (let* ((outfile (read-file-name "Save to: "
                                              "~/Videos/" nil nil
                                              (concat user-login "-"
                                                      (substring (alist-get 'created_at vod) 0 10)
                                                      ".mp4")))
                     (buf (format "*vterm:vod:%s*" user-login)))
                (vterm buf)
                (vterm-send-string
                 (format "streamlink --output %S %s %s\n" outfile url quality))
                (message "Downloading: %s" title))))))))))

(defun cy/twitch-vods ()
  "Pick a followed channel then browse and play its VODs."
  (interactive)
  (let ((url-request-extra-headers (cy/twitch-headers)))
    (url-retrieve
     "https://api.twitch.tv/helix/channels/followed?user_id=939991592&first=100"
     (lambda (_)
       (goto-char (point-min))
       (re-search-forward "\r?\n\r?\n")
       (let* ((channels (alist-get 'data (json-parse-buffer :object-type 'alist)))
              (choices  (mapcar (lambda (c) (alist-get 'broadcaster_name c)) channels))
              (choice   (completing-read "Channel VODs: " choices nil t))
              (idx      (cl-position choice choices :test #'equal))
              (channel  (aref channels idx))
              (login    (alist-get 'broadcaster_login channel))
              (uid      (alist-get 'broadcaster_id channel)))
         (cy/twitch--fetch-vods login uid))))))

(provide 'cy-twitch-vod-player)
;;; cy-twitch-vod-player.el ends here
