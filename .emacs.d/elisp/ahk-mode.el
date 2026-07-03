;; (use-package minibuffer-line)

;;; ahk-mode.el --- Major mode for AutoHotkey scripts -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Your Name
;; Keywords: languages, autohotkey, ahk
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; A simple major mode for editing AutoHotkey (.ahk) files.
;; Provides syntax highlighting, indentation, and basic navigation.

;;; Code:

(defgroup ahk nil
  "Major mode for editing AutoHotkey files."
  :group 'languages)

(defcustom ahk-indent-offset 4
  "Number of spaces for each indentation level in AHK mode."
  :type 'integer
  :group 'ahk)

;; Keywords and constants
(defconst ahk-keywords
  '("if" "else" "while" "for" "loop" "break" "continue" "return" "goto"
    "gosub" "exitapp" "exit" "suspend" "reload" "pause" "hotkey"
    "input" "inputbox" "msgbox" "traytip" "tooltip" "splashimage"
    "splashtexton" "splashtextoff" "progress" "winactivate" "winclose"
    "winexist" "winhide" "winkill" "winmaximize" "winminimize" "winmove"
    "winrestore" "winset" "winshow" "wintitle" "winwait" "winwaitactive"
    "winwaitclose" "winwaitnotactive" "send" "sendinput" "sendplay"
    "sendraw" "click" "mousemove" "mouseclick" "mouseclickdrag"
    "mousegetpos" "blockinput" "setcapslockstate" "setnumlockstate"
    "setscrolllockstate" "getkeystate" "random" "transform" "formattime"
    "envget" "envset" "envupdate" "regread" "regwrite" "regdelete"
    "iniread" "iniwrite" "inidelete" "fileappend" "filecopy" "filecopydir"
    "filecreatedir" "filecreateshortcut" "filedelete" "filegetattrib"
    "filegetsize" "filegettime" "filegetversion" "fileinstall" "filemove"
    "filemovedir" "fileread" "filereadline" "filerecycle" "filerecycleempty"
    "fileremovedir" "fileselectfile" "fileselectfolder" "filesetattrib"
    "filesettime" "driveget" "drivespacefree" "soundbeep" "soundget"
    "soundplay" "soundset" "urldownloadtofile" "runwait" "run" "process"
    "shutdown" "sort" "stringcasesense" "stringgetpos" "stringleft"
    "stringlen" "stringlower" "stringmid" "stringreplace" "stringright"
    "stringsplit" "stringtrimleft" "stringtrimright" "stringupper"
    "setworkingdir" "splitpath" "ifwinactive" "ifwinnotactive" "ifwinexist"
    "ifwinnotexist" "settimer" "sleep" "clipwait")
  "AutoHotkey keywords.")

(defconst ahk-builtin-variables
  '("A_AhkPath" "A_AhkVersion" "A_AppData" "A_AppDataCommon" "A_AutoTrim"
    "A_BatchLines" "A_CaretX" "A_CaretY" "A_ComputerName" "A_ControlDelay"
    "A_Cursor" "A_DD" "A_DDD" "A_DDDD" "A_DefaultMouseSpeed" "A_Desktop"
    "A_DesktopCommon" "A_DetectHiddenText" "A_DetectHiddenWindows" "A_EndChar"
    "A_EventInfo" "A_ExitReason" "A_FormatFloat" "A_FormatInteger" "A_Gui"
    "A_GuiEvent" "A_GuiControl" "A_GuiControlEvent" "A_GuiHeight" "A_GuiWidth"
    "A_GuiX" "A_GuiY" "A_Hour" "A_IconFile" "A_IconHidden" "A_IconNumber"
    "A_IconTip" "A_Index" "A_IPAddress1" "A_IPAddress2" "A_IPAddress3"
    "A_IPAddress4" "A_ISAdmin" "A_IsCompiled" "A_IsCritical" "A_IsPaused"
    "A_IsSuspended" "A_KeyDelay" "A_Language" "A_LastError" "A_LineFile"
    "A_LineNumber" "A_LoopField" "A_LoopFileAttrib" "A_LoopFileDir"
    "A_LoopFileExt" "A_LoopFileFullPath" "A_LoopFileLongPath" "A_LoopFileName"
    "A_LoopFileShortName" "A_LoopFileShortPath" "A_LoopFileSize"
    "A_LoopFileSizeKB" "A_LoopFileSizeMB" "A_LoopFileTimeAccessed"
    "A_LoopFileTimeCreated" "A_LoopFileTimeModified" "A_LoopReadLine"
    "A_LoopRegKey" "A_LoopRegName" "A_LoopRegSubkey" "A_LoopRegTimeModified"
    "A_LoopRegType" "A_MDAY" "A_Min" "A_MM" "A_MMM" "A_MMMM" "A_Mon"
    "A_MouseDelay" "A_MSec" "A_MyDocuments" "A_Now" "A_NowUTC" "A_NumBatchLines"
    "A_OSType" "A_OSVersion" "A_PriorHotkey" "A_PriorKey" "A_ProgramFiles"
    "A_Programs" "A_ProgramsCommon" "A_ScreenHeight" "A_ScreenWidth" "A_ScriptDir"
    "A_ScriptFullPath" "A_ScriptName" "A_Sec" "A_Space" "A_StartMenu"
    "A_StartMenuCommon" "A_Startup" "A_StartupCommon" "A_StringCaseSense"
    "A_Tab" "A_Temp" "A_ThisFunc" "A_ThisHotkey" "A_ThisLabel" "A_ThisMenu"
    "A_ThisMenuItem" "A_ThisMenuItemPos" "A_TickCount" "A_TimeIdle"
    "A_TimeIdlePhysical" "A_TimeSincePriorHotkey" "A_TimeSinceThisHotkey"
    "A_TitleMatchMode" "A_TitleMatchModeSpeed" "A_UserName" "A_WDay" "A_WinDelay"
    "A_WinDir" "A_WorkingDir" "A_YDay" "A_YEAR" "A_YWeek" "A_YYYY"
    "Clipboard" "ClipboardAll" "ComSpec" "ErrorLevel" "ProgramFiles")
  "AutoHotkey built-in variables.")

(defconst ahk-operators
  '("=" ":=" "+=" "-=" "*=" "/=" "//=" ".=" "|=" "&=" "^=" ">>=" "<<="
    "==" "!=" "<" ">" "<=" ">=" "&&" "||" "!" "~" "&" "|" "^" "<<" ">>"
    "+" "-" "*" "/" "//" "." "?" ":" "++", "--")
  "AutoHotkey operators.")

;; Font-lock (syntax highlighting)
(defconst ahk-font-lock-keywords
  `(
    ;; Comments
    ("\\(;.*\\)" 1 font-lock-comment-face)
    
    ;; Hotkeys and hotstrings
    ("^\\([~*!+^#<>]*[a-zA-Z0-9_]+\\)::" 1 font-lock-function-name-face)
    ("^\\([~*!+^#<>]*SC[0-9A-Fa-f]+\\)::" 1 font-lock-function-name-face)
    ("^\\([~*!+^#<>]*VK[0-9A-Fa-f]+\\)::" 1 font-lock-function-name-face)
    
    ;; Labels
    ("^\\([a-zA-Z0-9_]+\\):" 1 font-lock-constant-face)
    
    ;; Keywords
    (,(concat "\\<\\(" (mapconcat 'identity ahk-keywords "\\|") "\\)\\>")
     1 font-lock-keyword-face)
    
    ;; Built-in variables
    (,(concat "\\<\\(" (mapconcat 'identity ahk-builtin-variables "\\|") "\\)\\>")
     1 font-lock-variable-name-face)
    
    ;; Strings
    ("\"[^\"]*\"" . font-lock-string-face)
    ("'[^']*'" . font-lock-string-face)
    
    ;; Numbers
    ("\\<[0-9]+\\(?:\\.[0-9]+\\)?\\>" . font-lock-constant-face)
    ("\\<0x[0-9A-Fa-f]+\\>" . font-lock-constant-face)
    
    ;; Directives (preprocessor-like)
    ("^#[a-zA-Z]+" . font-lock-preprocessor-face)
    
    ;; Functions (word followed by opening parenthesis)
    ("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*(" 1 font-lock-function-name-face)
    
    ;; Variables (simple heuristic)
    ("\\<%[a-zA-Z_][a-zA-Z0-9_]*%\\>" . font-lock-variable-name-face))
  "Font lock keywords for AHK mode.")

;; Indentation
(defun ahk-indent-line ()
  "Indent current line as AHK code."
  (interactive)
  (let ((indent-col 0)
        (cur-indent (current-indentation)))
    (save-excursion
      (beginning-of-line)
      (when (not (bobp))
        (forward-line -1)
        (setq indent-col (current-indentation))
        ;; Increase indent after certain keywords or opening braces
        (when (looking-at ".*\\(if\\|else\\|while\\|for\\|loop\\|{\\).*$")
          (setq indent-col (+ indent-col ahk-indent-offset)))
        ;; Decrease indent for closing braces
        (save-excursion
          (forward-line 1)
          (when (looking-at "^\\s-*}")
            (setq indent-col (max 0 (- indent-col ahk-indent-offset)))))))
    ;; Apply indentation
    (when (not (= cur-indent indent-col))
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to indent-col))))

;; Syntax table
(defvar ahk-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?\; "<" st)
    (modify-syntax-entry ?\n ">" st)
    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?' "\"" st)
    ;; Operators
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?* "." st)
    (modify-syntax-entry ?/ "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?^ "." st)
    (modify-syntax-entry ?! "." st)
    (modify-syntax-entry ?~ "." st)
    st)
  "Syntax table for AHK mode.")

;; Mode map
(defvar ahk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'ahk-run-script)
    (define-key map (kbd "C-c C-r") 'ahk-reload-script)
    map)
  "Keymap for AHK mode.")

;; Utility functions
(defun ahk-run-script ()
  "Run the current AHK script."
  (interactive)
  (save-buffer)
  (start-process "ahk" nil "autohotkey" (buffer-file-name))
  (message "Running AHK script: %s" (buffer-name)))

(defun ahk-reload-script ()
  "Reload the current AHK script."
  (interactive)
  (save-buffer)
  (call-process "autohotkey" nil nil nil "/restart" (buffer-file-name))
  (message "Reloaded AHK script: %s" (buffer-name)))

;; Mode definition
;;;###autoload
(define-derived-mode ahk-mode prog-mode "AHK"
  "Major mode for editing AutoHotkey scripts."
  :syntax-table ahk-mode-syntax-table
  ;; Font lock
  (setq-local font-lock-defaults '(ahk-font-lock-keywords nil t))
  ;; Comments
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local comment-start-skip ";+\\s-*")
  ;; Indentation
  (setq-local indent-line-function 'ahk-indent-line)
  (setq-local tab-width ahk-indent-offset)
  ;; Case sensitivity
  (setq-local case-fold-search t))

;; Auto mode
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ahk\\'" . ahk-mode))

(provide 'ahk-mode)
;;; ahk-mode.el ends here
