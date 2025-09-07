(setq user-full-name "cyan"
      user-mail-address "cyan.mv@gmail.com")

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; add to $DOOMDIR/init.el
(defvar native-comp-deferred-compilation-deny-list nil)

(set-frame-parameter (selected-frame) 'alpha '(100 100))

(set-face-attribute 'default nil :family "ubuntu mono" :weight 'normal    :height 165)
(set-face-attribute 'variable-pitch nil :family "ubuntu sans" :weight 'normal    :height 165)

(setq custom-file (concat user-emacs-directory "to-be-dumped.el")) ;; Dump custom-set-variables

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(straight-use-package 'org)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(global-hl-line-mode 1)

(scroll-bar-mode -1)                ; Disable visible scrollbar
(tool-bar-mode -1)                  ; Disable the toolbar
(tooltip-mode -1)                       ; Disable tooltips
(set-fringe-mode 0)                ; Give some breathing room
(menu-bar-mode -1)                      ; Disable the menu bar


(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))) ;; Move Emacs Backups

;; Revert buffers when the underlying file has changed
;; (global-auto-revert-mode 1)
;; Remember and restore the last cursor location of opened files
(save-place-mode 1)					

;; Set up the visible bell
(setq visible-bell t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; (unbind-key "C-r")
;; (global-set-key (kbd "M-o") 'other-window) ;;Better than capitalize word
;; (global-set-key (kbd "M-c") 'capitalize-dwim)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (global-set-key [remap yank-pop] 'consult-yank-from-kill-ring)
;; (global-set-key [remap project-switch-project] 'projectile-persp-switch-project)
;; (global-set-key [remap set-mark-command] 'my-contract-region) ;https://emacs.stackexchange.com/questions/40613/override-c-spc-set-mark-command

(setq time-stamp-active t          ; Do enable time-stamps.
      time-stamp-line-limit 10     ; Check first 10 buffer lines for Time-stamp: <>
      time-stamp-format "Last changed %Y-%02m-%02d %02H:%02M:%02S by %u")
;; (add-hook 'write-file-hooks 'time-stamp)

(add-hook 'before-save-hook 'time-stamp)

(setq use-dialog-box nil)		; dont use gui boxes

(straight-use-package '(monet :type git :host github :repo "stevemolitor/monet"))

(use-package vterm :ensure t)

(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main" :depth 1
                   :files ("*.el" (:exclude "images/*")))
  :bind-keymap
  ("C-c c" . claude-code-command-map) ;; or your preferred key
  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode))
  :config
  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)
  (claude-code-mode))

(add-hook 'claude-code-process-environment-functions #'monet-start-server-function) ; i think it is not neccessary

(setq claude-code-toggle-auto-select t)

(monet-mode 1)

(setq claude-code-terminal-backend 'vterm)

(use-package vertico
  :straight t
  :bind (:map vertico-map
              :map minibuffer-local-map
              ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package orderless									; vertico best friend (regexp)
  :init
  (setq
   ;; completion-styles '(orderless basic)
   completion-styles '(substring orderless basic)
   completion-category-defaults nil
   completion-category-overrides '((file (styles partial-completion)))))

(use-package modus-themes :ensure t)

;; As above, but with a purple style
(setq modus-themes-common-palette-overrides
      '((comment yellow-faint)
	(bg-prose-block-delimiter  unspecified)  ; source code block (top face)
	(bg-prose-block-contents   unspecified) ; sourec code block (background face)
	(fg-line-number-inactive unspecified)
        (fg-line-number-active fg-main)
        (bg-line-number-inactive unspecified)
        (bg-line-number-active unspecified)
	(region ((t :extend nil)))
	(fg-region unspecified)))

(setq modus-themes-italic-constructs t)

(setq modus-themes-bold-constructs t)

(modus-themes-load-theme 'modus-vivendi)

(use-package ef-themes)
  
(defun my-ef-themes-mode-line ()
  "Tweak the style of the mode lines."
  (ef-themes-with-colors
   (custom-set-faces
    `(mode-line ((,c :background ,bg-active :foreground ,fg-main :box (:line-width 1 :color ,fg-dim))))
    ;; `(mode-line ((,c :background ,bg-mode-line :foreground ,fg-mode-line :box (:line-width 1 :color ,fg-dim))))
    `(mode-line-inactive ((,c :box (:line-width 1 :color ,bg-active)))))))

(add-hook 'ef-themes-post-load-hook #'my-ef-themes-mode-line)

(ef-themes-load-theme 'ef-rosa)

(use-package minions)

(minions-mode)

(use-package svg-lib)

(use-package meow
  :custom
  (meow-use-clipboard t)
  ;; (meow--end-kmacro-on-exit t)
  (meow-expand-hint-counts
   '((word . 10)
     (line . 10)
     (block . 10)
     (find . 10)
     (till . 10))))

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(run-at-time nil (* 5 60) 'recentf-save-list)
(global-set-key "\C-x\ \C-r" 'recentf-open-files) ;; (Note: Emacs records file names. Therefore, if you move or rename a file outside of Emacs, it won't automatically update the list. You'll have to open the renamed file with the normal CTRL-X CTRL-F method.)

(use-package expand-region :ensure t)
(global-set-key (kbd "C-;") 'er/expand-region)
(use-package jump-char)

(defun me-meow-change ()
  (interactive)
  (if (region-active-p)
      (meow-change)
    (progn
      (meow-mark-word 1)
      (meow-change))))

(defun my/meow-mark-word ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'er/expand-region)
    (meow-mark-word 1)
    (if (not (region-active-p))
	(call-interactively 'er/expand-region))))

(defun xah-forward-quote-smart ()
    "Move cursor to the current or next string quote.
Place cursor at the position after the left quote.
Repeated call will find the next string.
URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-11-22"
    (interactive)
    (let (($pos (point)))
        (if (nth 3 (syntax-ppss))
    (progn
        (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
        (forward-sexp)
        (re-search-forward "\\\"" nil t))
            (progn (re-search-forward "\\\"" nil t)))
        (when (<= (point) $pos)
            (progn (re-search-forward "\\\"" nil t)))))


(use-package multiple-cursors)

(define-key meow-normal-state-keymap (kbd "M-n") #'forward-whitespace)
(define-key meow-insert-state-keymap (kbd "M-n") #'forward-whitespace)
(define-key meow-insert-state-keymap (kbd "C-.")#'mc/mark-next-like-this)
(define-key meow-normal-state-keymap (kbd "C-.")#'mc/mark-next-like-this)
(global-set-key (kbd "M-o") 'meow-to-block)

(global-set-key (kbd "M-i") (lambda () (interactive) (forward-whitespace -1)))
(global-set-key (kbd "M-u") 'backward-up-list)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak)
  (meow-motion-overwrite-define-key
   '("e" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   '("?" . meow-cheatsheet)
   ;; To execute the originally e in MOTION state, use SPC e.
   '("e" . "H-e")
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)		
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("-" . negative-argument)
   '(";" . repeat)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("?" . meow-visit)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . me-meow-change)
   ;; '("d" . nil) ;; '("d" . meow-delete)
   '("e" . meow-prev)
   '("E" . meow-prev-expand)
   '("f" . jump-char-forward) ;;meow-find
   '("g" . xah-forward-quote-smart)
   '("G" . meow-grab)			; TODO : what is this 
   '("h" . meow-right)
   ;; '("H" . meow-left-expand)
   '("i" . meow-line)
   ;; '("I" . meow-right-expand)
   '("j" . meow-join)
   '("k" . meow-left)
   ;; '("l" . meow-join)
   ;; '("L" . meow-goto-line)
   '("m" . my/meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   ;; '("N" . meow-next-expand)
   '("o" . meow-block)
   ;; '("O" . meow-to-block)
   ;; '("p" . meow-clipboard-yank)
   '("q" . meow-quit)
   ;; '("r" . meow-kill)
   '("s" . meow-insert)
   '("S" . meow-open-above)
   '("t" . meow-till)
   '("T" . nt-negative-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-search)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("z" . meow-pop-selection)
   '("'" . meow-reverse)
   '("<escape>" . ignore)
   '("/" . consult-line)
   '("E" . centaur-tabs-forward-tab)
   '("N" . centaur-tabs-backward-tab)
   ))


;; Must set before enable `meow-global-mode`
(setq meow-use-cursor-position-hack t
      meow-use-enhanced-selection-effect t)    ;; optional, for visual effect

(meow-global-mode t)

(add-to-list 'meow-mode-state-list '(recentf-dialog-mode . motion))
(add-to-list 'meow-mode-state-list '(help-mode . motion))
(add-to-list 'meow-mode-state-list '(browse-url-of-file . motion))
(add-to-list 'meow-mode-state-list '(pdf-view-mode . motion))
;; (add-to-list 'meow-mode-state-list '(tmr-tabulated-mode . motion))

(meow-setup)
(meow-global-mode 1)
(pending-delete-mode +1)								;; typed text replaces the selection

(setq meow-keypad-ctrl-meta-prefix nil)	;

(meow-leader-define-key
 '("u" . vundo)
 ;; '("r" . jump-to-register)
 '("e" . elfeed)
 '("g" . magit-status)
 '("a" . org-agenda)
 '("f" . project-find-file)
 '("s" . consult-grep))

(defun indent-whole-buffer ()
  "Indent the entire buffer without affecting point or mark."
  (interactive)
  (save-excursion
    (save-restriction
      (indent-region (point-min) (point-max)))))

(setq-default show-trailing-whitespace nil)
(setq whitespace-display-mappings '((space-mark 32 [?·])))
;; (setq whitespace-style '(space-mark))
;; (setq whitespace-display-mappings '((space-mark 32 [183] [46])))
;; (setq whitespace-space 'cfw:face-disable) ; <- insert the face
(setq whitespace-style '(face spaces space-mark))

(use-package groovy-mode :ensure t)

(use-package uv :straight (uv :type git :host github :repo "johannes-mueller/uv.el"))

(use-package markdown-mode)

(use-package py-isort)

;; Python environment
(use-package pyvenv
  :straight t
  :config
  (pyvenv-mode 1))

;; Install SLIME via straight
(straight-use-package 'slime)

;; Install slime-star via straight (from GitHub)
(straight-use-package 
 '(slime-star :type git :host github :repo "mmontone/slime-star"))

(add-to-list 'load-path "~/.core/.emacs.d/swanky-python/slimy-python")
(push 'slime-py slime-contribs)

(slime-setup slime-contribs)

(bind-key* "C-x C-e" 
  (lambda ()
    (interactive)
    (if (and (eq major-mode 'python-mode) 
             (fboundp 'slime-py-eval-statement-at-point))
        (slime-py-eval-statement-at-point)
      (eval-last-sexp nil))))

(use-package centaur-tabs
  :demand
  :config
  (setq centaur-tabs-set-bar 'over)
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(use-package posframe
  :ensure t)

(defvar my-posframe-timer nil "Timer for hiding the posframe.")

(add-hook 'eyebrowse-post-window-switch-hook
          (lambda ()
            (when (posframe-workable-p)
              ;; Cancel any existing timer
              (when my-posframe-timer
                (cancel-timer my-posframe-timer))
	      ;; Use run-with-idle-timer to ensure the switch is complete
              (run-with-idle-timer 0.1 nil
                (lambda ()
                  (let* ((slot (number-to-string (eyebrowse--get 'current-slot nil)))
                         (nyan (cond ((string= slot "1") "一つ")
                                     ((string= slot "2") "二つ")
                                     ((string= slot "3") "三つ")
                                     ((string= slot "4") "四つ")
                                     ((string= slot "5") "五つ")
                                     ((string= slot "6") "六つ")
                                     ((string= slot "7") "七つ")
                                     ((string= slot "8") "八つ")
                                     ((string= slot "9") "九つ")
                                     (t slot))) ; fallback
                         ;; Check if we're in a project first
                         (in-project-p (or
                                        ;; Check projectile
                                        (and (fboundp 'projectile-project-p)
                                             (ignore-errors (projectile-project-p)))
                                        ;; Check built-in project.el
                                        (and (fboundp 'project-current)
                                             (project-current))))
                         ;; Get project name or current directory
                         (context-name (if in-project-p
                                           ;; We're in a project - get project name
                                           (or
                                            ;; Method 1: projectile (if available)
                                            (and (fboundp 'projectile-project-name)
                                                 (ignore-errors (projectile-project-name)))
                                            ;; Method 2: built-in project.el
                                            (and (fboundp 'project-current)
                                                 (project-current)
                                                 (file-name-nondirectory 
                                                  (directory-file-name 
                                                   (project-root (project-current)))))
                                            ;; Fallback to current directory name
                                            (file-name-nondirectory 
                                             (directory-file-name default-directory)))
                                         ;; Not in a project - get current directory name
                                         (file-name-nondirectory 
                                          (directory-file-name default-directory))))
                         ;; Construct display string
                         (display-string (if context-name
                                             (concat " " nyan " - " context-name " ")
                                           (concat " " nyan " "))))
                    
                    (posframe-show "*my-posframe-buffer*"
                                   :string display-string
                                   :poshandler #'posframe-poshandler-frame-center
                                   :border-width 1
                                   :border-color "magenta")
                    
                    ;; Set timer to hide after 3 seconds
                    (setq my-posframe-timer
                          (run-with-timer 3 nil
                                          (lambda ()
                                            (posframe-hide "*my-posframe-buffer*")
                                            (setq my-posframe-timer nil))))))))))
(use-package eyebrowse
  :bind
  ("M-0" . (lambda () (interactive) (eyebrowse-last-window-config))
   )
  ("M-1" . me/eyebrowse-switch-1)
  ("M-2" . me/eyebrowse-switch-2)
  ("M-3" . me/eyebrowse-switch-3)
  ("M-4" . me/eyebrowse-switch-4)
  ("M-5" . me/eyebrowse-switch-5)
  ("M-6" . me/eyebrowse-switch-6)
  ("M-7" . me/eyebrowse-switch-7)
  ("M-8" . me/eyebrowse-switch-8)
  ("M-9" . me/eyebrowse-switch-9)
  :hook
  (after-init . eyebrowse-mode)
  :custom
  ;; (eyebrowse-tagged-slot-format "%t")
  (eyebrowse-mode-line-style 'current)
  ;; (eyebrowse-mode-line-left-delimiter "")
  ;; (eyebrowse-mode-line-right-delimiter "")
  (eyebrowse-new-workspace t))

;; Create workspaces from 1-9
(dotimes (n 9)
  (let* ((n (1+ n))
         (name (intern (format "me/eyebrowse-switch-%s" n)))
         (documentation
          (format "Switch to configuration %s or to the last visited." n)))
    (eval `(defun ,name ()
             ,documentation
             (interactive)
             (me/eyebrowse-switch ,n))
          t)))

(defvar last-buffers-eye-browse '() "docstring")

(defun me/eyebrowse-switch (n)
  "Switch to configuration N or to the last visited."
  (if (eq (eyebrowse--get 'current-slot) n)
      (eyebrowse-last-window-config)
    (funcall (intern (format "eyebrowse-switch-to-window-config-%s" n)))))

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        ;; '("\\*Messages\\*"
        ;;   "Output\\*$"
        ;;   "\\*Async Shell Command\\*"
        ;;   help-mode
        ;;   compilation-mode)
	'("*slime-repl uv-python*" "*claude:~/ord/b-esim/:default*"))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(use-package sqlite3
  :ensure t)

;; (defcustom org-roam-db-location (expand-file-name (locate-user-emacs-file "org-roam.db")))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/roam/"))
  ;; Fix: Set absolute path for database location
  (org-roam-db-location (expand-file-name (locate-user-emacs-file "org-roam.db"))) ;; https://github.com/org-roam/org-roam/issues/2488
  :bind (;; ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n n" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
	 ("C-c n t" . org-roam-dailies-find-today)
         ;; ("C-c n i" . org-roam-node-insert)
         ;; ("C-c n c" . org-roam-capture)
	 ;; ("C-c n j" . org-roam-dailies-capture-today)
	 )
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

;; Add this to your config temporarily, evaluate it, then remove it
;; (delete-file "~/.emacs.d/org-roam.db")

(use-package puni
  :defer t
  :init
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode))

(use-package pulsar)

(setq pulsar-pulse-functions
      ;; NOTE 2022-04-09: The commented out functions are from before
      ;; the introduction of `pulsar-pulse-on-window-change'.  Try that
      ;; instead.
      '(recenter-top-bottom
	move-to-window-line-top-bottom
	reposition-window
	bookmark-jump
	other-window
	delete-window
	;; delete-other-windows
	forward-page
	backward-page
	scroll-up-command
	scroll-down-command
	;; windmove-right
	;; windmove-left
	;; windmove-up
	;; windmove-down
	;; windmove-swap-states-right
	;; windmove-swap-states-left
	;; windmove-swap-states-up
	;; windmove-swap-states-down
	;; tab-new
	tab-close
	;; tab-next
	org-next-visible-heading
	org-previous-visible-heading
	org-forward-heading-same-level
	org-backward-heading-same-level
	outline-backward-same-level
	outline-forward-same-level
	outline-next-visible-heading
	outline-previous-visible-heading
	outline-up-heading))

(setq pulsar-pulse-on-window-change t)
(setq pulsar-pulse t)
(setq pulsar-delay 0.055)
(setq pulsar-iterations 10)
(setq pulsar-face ' pulsar-generic)
(setq pulsar-highlight-face 'pulsar-yellow)

(pulsar-global-mode 1)

(add-hook 'minibuffer-setup-hook #'pulsar-pulse-line)

(use-package magit  :custom  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1) ;fulllscreent
  ) 

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

(add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))

(use-package lab)

(setq lab-host "https://gitlab.ordenaris.com/")

(setq lab-token "glpat-SwfBnMxrysFYGDMbTpiU")

;; (use-package magit-todos  :after magit :config (magit-todos-mode 1))

(use-package hl-todo)

(use-package diff-hl
            :init
            (add-hook 'prog-mode-hook #'diff-hl-mode)
            (add-hook 'org-mode-hook #'diff-hl-mode)
            (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
            (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
            :config
            (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
            (setq diff-hl-margin-side 'left)
            (diff-hl-mode t))

(global-diff-hl-mode)

(diff-hl-margin-mode)

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(diff-hl-flydiff-mode)

(use-package blamer
  :straight (:host github :repo "artawower/blamer.el")
  :bind (("s-i" . blamer-show-posframe-commit-info))
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  (blamer-type 'posframe-popup)
  (blamer-show-avatar-p t)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t)))
  :config
  (global-blamer-mode 1))

(defun emacs-solo/switch-git-status-buffer ()
  "Parse git status from an expanded path and switch to a file.
The completion candidates include the Git status of each file."
  (interactive)
  (require 'vc-git)
  (let ((repo-root (vc-git-root default-directory)))
    (if (not repo-root)
	(message "Not inside a Git repository.")
      (let* ((expanded-root (expand-file-name repo-root))
	     (command-to-run (format "git -C %s status --porcelain=v1"
				     (shell-quote-argument expanded-root)))
	     (cmd-output (shell-command-to-string command-to-run))
	     (target-files
	      (let (files)
		(dolist (line (split-string cmd-output "\n" t) (nreverse files))
		  (when (> (length line) 3)
		    (let ((status (substring line 0 2))
			  (path-info (substring line 3)))
		      ;; Handle rename specially
		      (if (string-match "^R" status)
			  (let* ((paths (split-string path-info " -> " t))
				 (new-path (cadr paths)))
			    (when new-path
			      (push (cons (format "R %s" new-path) new-path) files)))
			;; Modified only (exclude untracked)
			(when (string-match "M" status)
			  (push (cons (format "%s %s" status path-info) path-info) files)))))))))
	(if (not target-files)
	    (message "No modified (tracked) or renamed files found.")
	  (let* ((candidates target-files)
		 (selection (completing-read "Switch to buffer (Git modified): "
					     (mapcar #'car candidates) nil t)))
	    (when selection
	      (let ((file-path (cdr (assoc selection candidates))))
		(when file-path
		  (find-file (expand-file-name file-path expanded-root)))))))))))

(global-set-key (kbd "C-x C-g") 'emacs-solo/switch-git-status-buffer)

(setq hl-todo-keyword-faces
      '(("TODO"   . error)
        ("FIXME"  . error)
        ("DEBUG"  . warning)
        ("GOTCHA" . font-lock-warning-face)
        ("STUB"   . font-lock-keyword-face)))

(use-package vundo
  :straight (:host github :repo "casouri/vundo")
  :bind ("C-x u" . vundo)
  :hook ((vundo-mode . my/vundo-setup))
  :init
  (progn
    (setq vundo-window-max-height 5))
  :config
  (progn
    (setq vundo-glyph-alist vundo-unicode-symbols)
    (defun my/vundo-setup ()
      "Remove mode-line and header-line."
      (setq mode-line-format nil)
      (setq header-line-format nil))))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		pdf-view-mode-hook
		doc-view-mode-hook
		tetris-mode-hook
		elfeed-show-mode-hook
		elfeed-search-mode-hook
		ebuku-mode-hook 
		nov-mode-hook
		dired-mode-hook
		vterm-mode-hook
		org-agenda-mode-hook
		image-mode-hook
		pdf-view-mode
		))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

(use-package org
  :defer t
  :hook
  (org-mode . efs/org-mode-setup)
  ;; (org-mode . efs/org-level-face-setup-simple)
  ;; (org-mode . org-appear-mode)
  :custom
  (org-return-follows-link t)
  (org-confirm-babel-evaluate nil)
  ;; (org-hide-leading-stars t)
  ;; (org-hide-emphasis-markers t)
  ;; (org-confirm-babel-evaluate nil)
  ;; (org-directory "~/.note/RoamNotes/OrgFiles/")
  ;; :custom
  ;; (org-hide-block-startup t)
  )

(with-eval-after-load 'org
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   (shell . t)
   (python . t)
   )))

(setq org-babel-python-command "python3")

(defun efs/org-mode-setup ()
  (org-modern-mode 1)
  ;; (buffer-face-set :height 165)
  )

(use-package consult)			;consult line

(setq x-underline-at-descent-line t)

(use-package org-modern
  :init
  (setq org-modern-list nil)		; lists like +, -
  (setq org-modern-checkbox nil)
  (setq org-modern-hide-stars nil)
  (setq org-modern-table nil)
  (setq org-modern-star nil)
  (setq org-modern-timestamp nil)
  :ensure t)

(defun now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "[%-H:%M]")))

(show-paren-mode +1)
(electric-pair-mode 1)
(setq electric-pair-pairs '((?\{ . ?\})))			;support "{}"

(use-package crux)

(use-package mwim
    :straight (:host github :repo "alezost/mwim.el"))
 
(global-set-key (kbd "C-e") 'mwim-end-of-code-or-line) ;end-of-line

;; (use-package smudge)
;; 
;; (use-package smudge
;;   :bind-keymap ("C-c ." . smudge-command-map)
;;   :custom
;;   (smudge-oauth2-client-secret "...")
;;   (smudge-oauth2-client-id "...")
;;   ;; optional: enable transient map for frequent commands
;;   (smudge-player-use-transient-map t)
;;   :config
;;   ;; optional: display current song in mode line
;;   (global-smudge-remote-mode))

;; (use-package smudge
;;   :bind-keymap ("C-c ." . smudge-command-map)
;;   :custom
;;   ;; (smudge-oauth2-client-secret "997590170108423fab44a5567fe1dc55")
;;   ;; (smudge-oauth2-client-id "24935f3a3b274b9da7f20b10b4e9c3f0")
;;   ;; optional: enable transient map for frequent commands
;;   (smudge-player-use-transient-map t)
;;   ;; :config
;;   ;; ;; optional: display current song in mode line
;;   (global-smudge-remote-mode))

(setq smudge-transport 'connect)

;; (setq smudge-oauth2-client-secret "997590170108423fab44a5567fe1dc55")
;; (setq smudge-oauth2-client-id "24935f3a3b274b9da7f20b10b4e9c3f0")

(use-package yaml-mode)

;; (global-smudge-remote-mode)

(use-package vterm)
(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  ;; If you have use-package-hook-name-suffix set to nil, uncomment and use the
  ;; line below instead:
  ;; :hook (eshell-mode-hook . esh-autosuggest-mode)
  :ensure t)

(use-package eshell-prompt-extras)
(use-package exec-path-from-shell) ;for swank to work
(exec-path-from-shell-initialize)	;for swank to work

(use-package meow-vterm :straight (:type git :host github :repo "accelbread/meow-vterm"))
(meow-vterm-enable)

(winner-mode)

;; (use-package yasnippet
;;   :bind (:map yas-minor-mode-map
;;               ("TAB" . nil)
;;               ("<tab>" . nil))
;;   :config
;;   (setq yas-snippet-dirs '("~/.emacs.d/snippets/"))
;;   (setq yas-snippet-dirs
;;         `(,(concat (expand-file-name user-emacs-directory) "snippets")
;;           yasnippet-snippets-dir))
;;   (setq yas-triggers-in-field t)
;;   (yas-global-mode 1))

(defun insert-groovy-debug-line ()
  "Insert a Groovy println statement with current file and line number."
  (interactive)
  (let ((filename (file-name-nondirectory (buffer-file-name)))
        (line-num (line-number-at-pos)))
    (insert (format "println \"\\n(O.o) %s::%d ${}\\n\"" filename line-num))))

;; Bind to a key if desired
(define-key groovy-mode-map (kbd "C-c d") 'insert-groovy-debug-line)

(use-package org-ql)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun isearch-with-selection (direction)
  "Start isearch with selected text in the given DIRECTION.
DIRECTION should be 'forward or 'backward."
  (let ((selected-text (buffer-substring-no-properties (region-beginning) (region-end))))
    (deactivate-mark)
    (goto-char (if (eq direction 'forward) (region-end) (region-beginning)))
    (if (eq direction 'forward)
        (isearch-forward nil 1)
      (isearch-backward nil 1))
    (isearch-yank-string selected-text)))

;; Deadgrep with selection support

(use-package deadgrep)

(defun deadgrep-with-selection ()
  "Run deadgrep with selected text."
  (interactive)
  (if (use-region-p)
      (let ((selected-text (buffer-substring-no-properties (region-beginning) (region-end))))
        (deactivate-mark)
        (deadgrep selected-text))
    (call-interactively #'deadgrep)))

;; Consult-grep with selection support
(defun consult-grep-with-selection ()
  "Run consult-grep with selected text."
  (interactive)
  (if (use-region-p)
      (let ((selected-text (buffer-substring-no-properties (region-beginning) (region-end))))
        (deactivate-mark)
        (consult-grep nil selected-text))
    (call-interactively #'consult-grep)))

;; Enhanced emulation alist
(defconst may-emulation-alist
  `((mark-active
     ,@(let ((m (make-sparse-keymap)))
	 (define-key m (kbd "C-r") (lambda () (interactive) (isearch-with-selection 'backward)))
	 (define-key m (kbd "C-s") (lambda () (interactive) (isearch-with-selection 'forward)))
         ;; Search bindings
         (define-key m (kbd "C-c o") #'deadgrep-with-selection)            ; Alt+s o for deadgrep
         (define-key m (kbd "C-c s") #'consult-grep-with-selection)        ; Alt+s g for consult-grep  
	 (define-key m (kbd "\"") (lambda () (interactive) (surround-quotes)))
         m))))

(add-to-list 'emulation-mode-map-alists 'may-emulation-alist)

(global-set-key (kbd "M-s o") 'deadgrep) ; unbinds occur

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rainbow-delimiters)
 
(add-hook 'prog-mode-hook (lambda () (rainbow-delimiters-mode +1)
			    (hl-todo-mode +1)
			    ))

(set-register ?i (cons 'file "~/.core/.emacs.d/init.el"))

(use-package diredfl
    :after dired
    :config
    (diredfl-global-mode 1))		; colors !

(add-hook 'dired-load-hook (function (lambda () (load "dired-x")))) ; good for untar 

;; Do not extend `region' background past the end of the line.
;; (custom-set-faces
;;  '(region ((t :extend nil))))

(use-package elfeed
  :bind ((:map elfeed-search-mode-map
               ("U" . elfeed-update))
         (:map elfeed-show-mode-map
               ("o" . elfeed-show-visit))
	 (:map elfeed-show-mode-map
               ("q" . elfeed-search-quit-window)))
  :config
  ;; (setq elfeed-db "~/.emacs.d/.elfeed/")
  (setq elfeed-feeds
        '("https://protesilaos.com/news.xml"
          "https://protesilaos.com/codelog.xml"
          "https://protesilaos.com/advice.xml"
          "https://protesilaos.com/interpretations.xml"
          "https://nullprogram.com/feed/"
          "https://www.reddit.com/r/emacs/.rss"
          "https://simblob.blogspot.com/feeds/posts/default" ;; Red blob games 
          "https://planet.emacslife.com/atom.xml"
          ;; "https://www.reddit.com/r/orgmode.rss"
          "https://www.reddit.com/r/Anki.rss"
          ;; "https://ag91.github.io/rss.xml" ;; Where parallels cross
          "https://www.rousette.org.uk/index.xml" ;; But she is a girl
          "https://karthinks.com/index.xml"
          "https://emacstil.com/feed.xml"
          "https://www.reddit.com/r/lisp.rss"
          "https://olddeuteronomy.github.io/index.xml"
          "https://www.reddit.com/r/AnkiComputerScience.rss"
          "https://whhone.com/index.xml"      ;; https://whhone.com/posts/para-org-mode/
          ;; "https://emacs.stackexchange.com/feeds"
          ;; "https://clojure.stackexchange.com/feeds"
          "https://www.reddit.com/r/Clojure.rss"
          "https://www.reddit.com/r/Clojurescript.rss"
          "https://www.reddit.com/r/lisp.rss"
          "http://gigasquidsoftware.com/atom.xml"
          "https://studio.tymoon.eu/api/studio/gallery/atom?tag&user=shinmera"
          ;; "https://tumblr.shinmera.com/rss"
	  "https://www.reddit.com/r/menitrust.rss"
	  "https://xkcd.com/atom.xml"
          "https://shaarli.lain.li/feed/atom?"
	  "https://endlessparentheses.com/atom.xml"
	  "https://www.cyan.sh/blog/feed.xml"
	  )))

(use-package nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(use-package org-remark
  :bind (;; :bind keyword also implicitly defers org-remark itself.
         ;; Keybindings before :map is set for global-map. Adjust the keybinds
         ;; as you see fit.
         ("C-c n m" . org-remark-mark)
         ("C-c n l" . org-remark-mark-line)
         :map org-remark-mode-map
         ("C-c n o" . org-remark-open)
         ("C-c n ]" . org-remark-view-next)
         ("C-c n [" . org-remark-view-prev)
         ("C-c n r" . org-remark-remove)
         ("C-c n d" . org-remark-delete)))

(use-package org-transclusion)

(defun my/transclude-region-or-file (language)
  "Create a transclusion link for current file.
If region is active, transclude only selected lines."
  (interactive "sLanguage: ")
  (let* ((current-file (buffer-file-name))
         (transclude-line (format "#+transclude: [[file:%s]] :src %s" 
                                  current-file language)))
    
    (when (use-region-p)
      (let ((start-line (line-number-at-pos (region-beginning)))
            (end-line (line-number-at-pos (region-end))))
        (setq transclude-line (concat transclude-line 
                                      (format " :lines %d-%d" start-line end-line)))))
    
    (kill-new transclude-line)
    (message "Transclusion link copied to clipboard: %s" transclude-line)))

(with-eval-after-load 'nov  (org-remark-nov-mode +1))

;; Disable startup screen
;; Disable startup screen and set initial buffer
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-buffer-choice t)

;; Custom centered scratch message
(defun my-scratch-message ()
  "Generate custom scratch message."
  (concat
   ";; This buffer is for text that is not saved, and for Lisp evaluation.\n"
   ";; To create a file, visit it with C-x C-f and enter text in its buffer.\n"
   ";;\n"
   ";; 　　　　　 　r /\n"
   ";; 　 ＿＿ , --ヽ!-- .､＿\n"
   ";; 　! 　｀/::::;::::ヽ l\n"
   ";; 　!二二!::／}::::丿ハﾆ|\n"
   ";; 　!ﾆニ.|:／　ﾉ／ }::::}ｺ\n"
   ";; 　L二lイ　　0´　0 ,':ﾉｺ\n"
   ";; 　lヽﾉ/ﾍ､ ''　▽_ノイ ソ\n"
   ";;  　ソ´ ／}｀ｽ /￣￣￣￣/\n"
   ";; 　　　.(_:;つ/  0401 /　ｶﾀｶﾀ\n"
   ";;  ￣￣￣￣￣＼/＿＿＿＿/\n\n"))

(setq initial-scratch-message (my-scratch-message))


(defun vterm-highlight-logs-modus ()
  "Simple highlighting for vterm logs using Modus theme colors."
  (interactive)
  (when (eq major-mode 'vterm-mode)
    (let ((red-color (modus-themes-get-color-value 'red-intense))
          (green-color (modus-themes-get-color-value 'green-intense))
          (blue-color (modus-themes-get-color-value 'blue-intense))
          (yellow-color (modus-themes-get-color-value 'yellow-intense))
          (magenta-color (modus-themes-get-color-value 'magenta)))
      
      ;; Define custom faces using Modus colors - foreground only
      (defface vterm-modus-error-face
        `((t (:foreground ,red-color :weight bold)))
        "Error face for vterm using Modus colors.")
      
      (defface vterm-modus-success-face
        `((t (:foreground ,green-color :weight bold)))
        "Success face for vterm using Modus colors.")
      
      (defface vterm-modus-timestamp-face
        `((t (:foreground ,blue-color :weight normal)))
        "Timestamp face for vterm using Modus colors.")
      
      (defface vterm-modus-uuid-face
        `((t (:foreground ,yellow-color)))
        "UUID face for vterm using Modus colors.")
      
      (defface vterm-modus-service-face
        `((t (:foreground ,magenta-color :weight normal)))
        "Service name face for vterm using Modus colors.")
      
      ;; Apply highlights
      (highlight-regexp "Permission denied\\|ERROR\\|FAIL" 'vterm-modus-error-face)
      (highlight-regexp "BUILD SUCCESSFUL\\|CONFIGURE SUCCESSFUL\\|asignada\\|terminado" 'vterm-modus-success-face)
      (highlight-regexp "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}" 'vterm-modus-timestamp-face)
      (highlight-regexp "[a-f0-9]\\{32\\}" 'vterm-modus-uuid-face)
      (highlight-regexp "esimBait\\|WebHook\\|Esim\\|Grails" 'vterm-modus-service-face)
      
      (message "Vterm Modus theme highlighting applied"))))

(defun vterm-clear-highlights ()
  "Clear all highlights in vterm buffer."
  (interactive)
  (unhighlight-regexp t)
  (message "Vterm highlights cleared"))

(modus-themes-get-color-value 'red-intense)

(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")

(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework/"
  :straight (eaf
             :type git
             :host github
             :repo "emacs-eaf/emacs-application-framework"
             :files ("*.el" "*.py" "core" "app" "*.json")
             :includes (eaf-pdf-viewer eaf-browser) ; Add other apps as needed
             ;; :pre-build (("python3" "install-eaf.py" "--install" "pdf-viewer" "browser" "--ignore-sys-deps"))
	     ))

(require 'eaf-browser)

