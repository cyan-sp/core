;;; cy-ordenaris.el --- personal utility functions

;;; Code:

(defun cy/region-to-cat-grep (beg end)
  "Convert a region of quoted hex IDs to individual cat+grep commands and copy them."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end))
         (ids (seq-filter #'identity
                          (mapcar (lambda (l)
                                    (when (string-match "[0-9a-fA-F]\\{8,\\}" l)
                                      (match-string 0 l)))
                                  (split-string text "\n"))))
         (cmds (mapconcat (lambda (id)
                            (format "cat -A /mnt/opt/tomcat/logs/catalina.out | grep -ie '%s'" id))
                          ids "\n")))
    (kill-new cmds)
    (message "Copied %d commands" (length ids))))

(defun cy/region-base64-to-sql-or (beg end)
  "Convert a region of base64 strings (one per line) to SQL OR uuid LIKE clauses."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end))
         (lines (seq-filter (lambda (l) (not (string-empty-p (string-trim l))))
                            (split-string text "\n")))
         (clauses (mapconcat (lambda (b64)
                               (format "        OR uuid LIKE concat(\"%%\", from_base64(\"%s\"))"
                                       (string-trim b64)))
                             lines "\n")))
    (goto-char end)
    (insert "\n\n" clauses)))

(defun cy/grep-log-to-tmp (pattern logfile &optional host)
  (let* ((short (substring pattern 0 8))
         (logbase (file-name-base (expand-file-name logfile)))
         (out (format "/tmp/esim-%s-%s%s.txt" short logbase (if host (format "-%s" host) "")))
         (ssh-aliases '(("paym156" . "sshpass -p 'S?Znl$T?w2ts5IJq&?n@6^+%' ssh -p 2223 cristhian.gutierrez@184.107.108.156")
                        ("paym195" . "sshpass -p '2{)s23iAE*Zy$7;miNG\\SKa<VL' ssh -p 2223 cristhian.gutierrez@184.107.108.195")
                        ("esim62"  . "sshpass -p 'Xx2uDA#9hAQLxAVbA@#vLw' ssh christian.gutierrez@10.11.10.62")
                        ("esim63"  . "sshpass -p '@ivHYMFQ79S59JbiAdYiw##' ssh christian.gutierrez@10.11.10.63")))
         (ssh-cmd (when host (cdr (assoc host ssh-aliases))))
         (cmd (if host
                  (format "%s \"grep -aie '%s' %s\"" ssh-cmd pattern logfile)
                (format "grep -aie '%s' %s" pattern (expand-file-name logfile)))))
    (with-temp-file out
      (insert (shell-command-to-string cmd)))
    (format "[[%s][%s]]" out out)))

(defun cy/region-to-json (beg end)
  "Convert [key:value, key:value] region to JSON and insert below."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end))
         ;; Strip the outer brackets [ and ]
         (inner (replace-regexp-in-string "\\`\\[\\|\\]\\'" "" (string-trim text)))
         (pairs '())
         (start 0))
    ;; Match keys (letters/numbers) followed by ':' and then anything up until
    ;; the next ', key:' or the end of the string.
    (while (string-match "\\([a-zA-Z0-9_]+\\):\\(.*?\\)\\(?, [a-zA-Z0-9_]+:\\|\\'\\)" inner start)
      (let ((key (match-string 1 inner))
            (val (match-string 2 inner)))
        ;; Clean up any stray commas, newlines, or spaces from the value
        (setq val (string-trim (replace-regexp-in-string ",\\'" "" (string-trim val))))
        (push (cons key val) pairs)
        ;; Move the marker forward, but step back slightly to catch the next key if needed
        (setq start (match-beginning 3))))
    
    (setq pairs (nreverse pairs))
    
    (when pairs
      (let ((json (concat "{\n"
                          (mapconcat (lambda (p)
                                       (format "  \"%s\": \"%s\""
                                               (car p) (cdr p)))
                                     pairs
                                     ",\n")
                          "\n}")))
        (goto-char end)
        (insert "\n\n" json)))))

(use-package jira
  :config
  (setq jira-base-url "https://acme.atlassian.net") ;; Jira instance URL
  (setq jira-username "johndoe@acme.com") ;; Jira username (usually, an email)
  ;; API token for Jira
  ;; See https://support.atlassian.com/atlassian-account/docs/manage-api-tokens-for-your-atlassian-account/
  (setq jira-token "foobar123123")
  (setq jira-token-is-personal-access-token nil)
  (setq jira-api-version 3) ;; Version 2 is also allowed
  ;; (Optional) API token for JIRA TEMPO plugin
  ;; See https://apidocs.tempo.io/
  (setq jira-tempo-token "foobar123123"))

(setenv "SQLCMDPASSWORD"
        (auth-source-pick-first-password :host "10.11.10.134" :user "christian.gutierrez"))



(defun my-sqlcmd-toggle ()
  (interactive)
  (if (equal (getenv "SQLCMDPASSWORD") "::toast22s")
      (progn
        (setenv "SQLCMDPASSWORD"
                (auth-source-pick-first-password :host "10.11.10.134" :user "christian.gutierrez"))
        (message "SQLCMDPASSWORD → prod"))
    (progn
      (setenv "SQLCMDPASSWORD" "::toast22s")
      (message "SQLCMDPASSWORD → local"))))

(my-sqlcmd-toggle) ;; starts as local, call again to switch to prod


(provide 'cy-ordenaris)

;;; cy-ordenaris.el ends here
