;;;; -*- mode: Emacs-Lisp; eldoc-mode:t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Bruce C. Miller - bm3719@gmail.com
;;;; Time-stamp: <2014-07-29 10:56:15 (bmiller)>
;;;;
;;;; This init was created for GNU Emacs 24.3.1 for FreeBSD, GNU/Linux, and
;;;; Windows, but all or parts of this file should work with older GNU Emacs
;;;; versions, on other OSes, or even on XEmacs with minor adjustments.
;;;;
;;;; External addons used: pabbrev, pretty-symbols.el, volatile-highlights.el,
;;;; slime, marmalade via package.el (clojure-mode, clojure-test-mode, CIDER),
;;;; ac-nrepl, rainbow-delimiters, haskell-mode, python-mode, helm, ruby-mode,
;;;; groovy-mode, auctex, web-mode, flymake-cursor, js2-mode, flymake-jshint,
;;;; markdown-mode, cedet, gtags, elscreen, elscreen-w3m (+ flim, apel),
;;;; emacs-w3m (development branch), multi-term, lusty-explorer, emms,
;;;; wombat-custom-theme.el, darcsum, psvn, magit (+ git-modes), lojban-mode (+
;;;; lojban.el), malyon, redo+.el, htmlize.el.
;;;;
;;;; External applications used: Gauche, aspell, SBCL, Clojure, GHC, GNU
;;;; Global, python-doc-html, pyflakes, Ruby, Maxima, mutt, w3m, xpp (*nix
;;;; only), Ghostscript/GSView (Windows only), Consolas font (Windows only).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initial Startup

;; Getting rid of the toolbar first prevents it from showing in the few seconds
;; needed for the rest of this stuff to load, though disabling it in .Xdefaults
;; is even better.
(when window-system
    (tool-bar-mode -1))

;; Store boolean values for various system-specific settings.
(defvar *freebsd-system* (string-match "freebsd" system-configuration))
(defvar *linux-system* (string-match "linux" system-configuration))
(defvar *nt-system* (string-match "nt" system-configuration))
(defvar *osx-system* (string-match "darwin" system-configuration))

;; Font face: Requires appropriate fonts to be installed.  
(if *nt-system*
  (set-default-font
   "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")
  (when window-system
    (set-face-attribute 'default nil :font "dejavu sans mono-12")))

(setq inhibit-startup-message t)   ; Disable splash screen.
(when window-system
  (set-scroll-bar-mode 'right)     ; If turned on, use right scrollbars.
  (scroll-bar-mode -1)             ; Hide the scroll bar.
  (tooltip-mode 0))                ; Disable tooltips.
(menu-bar-mode -1)                 ; Hide the menu bar.

;; Rearrange the menubars, so it goes tools | buffers | help.
(setq menu-bar-final-items '(tools buffer help-menu))

;; Remove wasted pixels left of col1.
(when (fboundp 'set-fringe-mode)   ; Added in >22.
    (set-fringe-mode 2))           ; Space in pixels.

(global-font-lock-mode 1)          ; Turn on font lock mode everywhere.
(blink-cursor-mode nil)            ; Disable cursor blinking.
(setq visible-bell t)              ; Make bell visible, not aural.

;; Add the directory containing .el files in into the default load path.
(setq load-path (cons "~/.emacs.d" load-path))

;; Shut off message buffer.  To debug Emacs, comment these out so you can see
;; what's going on.
(setq message-log-max nil)
;; Check if message buffer exists before killing (not doing so errors
;; eval-buffers of a .emacs file).
(when (not (eq nil (get-buffer "*Messages*")))
      (kill-buffer "*Messages*"))

;; Provide a useful error trace if loading this .emacs fails.
(setq debug-on-error t)

;; Change backup behavior to save in a directory, not in a miscellany of files
;; all over the place, and disable autosaves completely.
(setq make-backup-files t           ; Do make backups.
      backup-by-copying t           ; Don't clobber symlinks.
      backup-directory-alist
      '(("." . "~/.emacs.d/saves")) ; Don't litter my fs tree.
      delete-old-versions t         ; Get rid of old versions of files.
      kept-new-versions 4
      kept-old-versions 2
      version-control t             ; Use versioned backups.
      auto-save-default nil)        ; Normal backups are enough for me.

;; Specify UTF-8 for a few addons that are too dumb to default to it.
(set-default-coding-systems 'utf-8-unix)

;; Load Common Lisp features.
(require 'cl)

;; Provides zap-up-to-char (M-z), different than the default zap-to-char which
;; includes deleting the argument character.
(load-library "misc")

;; Work-around for a bug in w32 Emacs 23.
(when *nt-system*
  (and (= emacs-major-version 23)
       (defun server-ensure-safe-dir (dir) "Noop" t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic Key Bindings

;; General convenience remappings.
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-xw" 'kill-region)
(global-set-key "\C-xs" 'save-buffer)
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-G" 'goto-char)
(global-set-key "\M-?" 'help-for-help)
(global-set-key "\C-x\C-k" 'kill-this-buffer)  ; Bypasses the C-x k prompt.
(global-set-key "\C-r" 'isearch-backward)
(global-set-key "\C-c\C-r" 'revert-buffer)
(global-set-key "\C-x\C-i" 'indent-region)
(global-set-key "\C-c\C-i" 'indent-region)
(global-set-key "\C-ce" 'fixup-whitespace)
(global-set-key "\C-x\C-u" 'undo)
(global-set-key "\C-cg" 'replace-string)
(global-set-key "\C-c;" 'comment-region)
(global-set-key "\C-c'" 'uncomment-region)
(global-set-key "\M-/" 'hippie-expand)         ; Superior to dabbrev-expand.
(global-set-key "\M-z" 'zap-up-to-char)        ; Mimic vim delete to char.
(global-set-key "\M-o" 'other-window)
(global-set-key "\C-x\M-a" 'align-regexp)
;; Move set-fill-column from C-x f to C-x M-f, as it's easy to hit this when
;; intending to do a find-file.
(global-set-key "\C-xf" 'find-file)
(global-set-key "\C-x\M-f" 'set-fill-column)

;; For quick macro running
(global-set-key [f9] 'start-kbd-macro)
(global-set-key [f10] 'edit-kdb-macro)
(global-set-key [f11] 'end-kbd-macro)
(global-set-key [f12] 'call-last-kbd-macro)

;; Cycle through buffers with Ctrl-Tab.
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; M-x compile and M-x grep mnemonics.
(global-set-key [f5] 'compile)
(global-set-key "\C-cn" 'next-error)
(global-set-key "\C-cp" 'previous-error)

;; My KVM switch uses scroll lock, and Emacs complains about it.
(global-set-key [Scroll_Lock] 'ignore)
;; Silence drag-mouse-9 complaints
(global-set-key [mouse-9] 'ignore)
(global-set-key [double-mouse-9] 'ignore)
(global-set-key [drag-mouse-9] 'ignore)

;; Disable C-z on X11 sessions.
(when window-system
  (global-unset-key "\C-z"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General Text Editing

;; Set fill width to 79 (default was 70).
(setq-default fill-column 79)

;; Takes a multi-line paragraph and makes it into a single line of text.
(defun bcm-unfill-paragraph ()
  "Un-fill paragraph at point."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(global-set-key "\M-p" 'bcm-unfill-paragraph)

;; Heretical tab settings.  Emacs is smart enough to auto-disable this when
;; editing makefiles.
(setq-default indent-tabs-mode nil)
;; Using a tab-stop-list will preserve 8-space tabs for documents that have
;; them, but make my own tabs 2 spaces.
(setq tab-stop-list '(2 4 6 8 10 12 14 16 18))
;(setq-default tab-width 2)

;; Always flash for parens.
(show-paren-mode 1)

;; Enable narrowing of regions.
(put 'narrow-to-region 'disabled nil)

;; Allow a command to erase an entire buffer.
(put 'erase-buffer 'disabled nil)

;; Disable over-write mode.
(defun overwrite-mode (arg) (interactive "p"))

;; Modify hippie-expand functions.
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
;; Don't expand symbols.
(setq hippie-expand-dabbrev-as-symbol nil)

;; Use cursor color to indicate some modes.  Modified version that ignores
;; overwrite.  Original snippet from:
;; http://www.emacswiki.org/emacs/EmacsNiftyTricks#toc4
(setq bcm-set-cursor-color-color "")
(setq bcm-set-cursor-color-buffer "")
(defun bcm-set-cursor-color-according-to-mode ()
  "Change cursor color according to some minor modes."
  ;; Set-cursor-color is somewhat costly, so we only call it when needed.
  (let ((color (if buffer-read-only "red" "DarkSlateGray")))
    (unless (and
      (string= color bcm-set-cursor-color-color)
      (string= (buffer-name) bcm-set-cursor-color-buffer))
      (set-cursor-color (setq bcm-set-cursor-color-color color))
      (setq bcm-set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'bcm-set-cursor-color-according-to-mode)

;; Alias to prompt for a regex and a replacement string.
(defalias 'qrr 'query-replace-regexp)

;; Don't bother entering search and replace args if the buffer is read-only.
(defadvice query-replace-read-args (before barf-if-buffer-read-only activate)
  "Signal a `buffer-read-only' error if the current buffer is read-only."
  (barf-if-buffer-read-only))

;; Change pasting behavior.  Normally, it pastes where the mouse is at, which
;; is not necessarily where the cursor is.  This changes things so whether they
;; be middle-click, C-y, or menu, all paste at the cursor.
(setq mouse-yank-at-point t)

;; SavePlace: This puts the cursor in the last place you edited a particular
;; file.  A very useful default vim feature.
(require 'saveplace)
(setq-default save-place t)

;; I use sentences.  Like this.
(setq sentence-end-double-space t)

;; Highlight regions so one can see what one is doing.
;; Defaults on in >23.
(transient-mark-mode 1)

;; Allow for mark ring traversal without popping them off the stack.
(setq set-mark-command-repeat-pop t)

;; Text files supposedly end in new lines, or they should.
(setq require-final-newline t)

;; Defines a function to kill text from point to beginning of line.
(defun bcm-backward-kill-line (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (kill-line 0))
(global-set-key "\M-\C-k" 'bcm-backward-kill-line)

;; Copy a line without killing it.
(defun bcm-copy-line (&optional arg)
  "Do a kill-line but copy rather than kill."
  (interactive "p")
  (toggle-read-only 1)
  (kill-line arg)
  (toggle-read-only 0))
;; Replace error message on read-only kill with an echo area message.
(setq-default kill-read-only-ok t)
;(global-set-key "\C-c\C-k" 'bcm-copy-line)

;; For composing in Emacs then pasting into a word processor, this un-fills all
;; the paragraphs (i.e. turns each paragraph into one very long line) and
;; removes any blank lines that previously separated paragraphs.
(defun bcm-wp-munge ()
  "Un-fill paragraphs and remove blank lines."
  (interactive)
  (let ((save-fill-column fill-column))
    (set-fill-column 1000000)
    (mark-whole-buffer)
    (fill-individual-paragraphs (point-min) (point-max))
    (delete-matching-lines "^$")
    (set-fill-column save-fill-column)))

;; Add a function to strip DOS endlines.
(defun bcm-cut-ctrlm ()
  "Cut all visible ^M."
  (interactive)
  (beginning-of-buffer)
  (while (search-forward "\r" nil t)
    (replace-match "" nil t)))

;; Insert a date string in the format I most commonly use in textfiles.
(defun bcm-date ()
  "Insert an ISO 8601 formatted date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))
;; Insert a UTC datetime string in ISO 8601 format.
(defun bcm-datetime ()
  "Insert an ISO 8601 formatted datetime string, with time in UTC."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer Navigation

;; Shift-arrow keys to move between windows.
(windmove-default-keybindings)

;; Scrolling
;; Fix the whole huge-jumps-scrolling-between-windows nastiness.
(setq scroll-conservatively 4)
;; Don't hscroll unless needed.
(setq hscroll-margin 1)
;; Start scrolling when 2 lines from top/bottom.  Set to 0 on systems where I
;; use ansi-term or multi-term a lot.
(setq scroll-margin 2)
;; Keeps the cursor in the same relative row during pgups and downs.
(setq scroll-preserve-screen-position t)

;; Make cursor stay in the same column when scrolling using pgup/dn.
;; Previously pgup/dn clobbers column position, moving it to the beginning of
;; the line.
;; http://www.dotemacs.de/dotfiles/ElijahDaniel.emacs.html
(defadvice scroll-up (around ewd-scroll-up first act)
  "Keep cursor in the same column."
  (let ((col (current-column)))
    ad-do-it
    (move-to-column col)))
(defadvice scroll-down (around ewd-scroll-down first act)
  "Keep cursor in the same column."
  (let ((col (current-column)))
    ad-do-it
    (move-to-column col)))

;; M-dn and M-up do nothing.  Let's make them do something, like M-left and
;; M-right do.
(global-set-key [M-down]
  '(lambda () (interactive) (progn (forward-line 4) (recenter))))
(global-set-key [M-up]
  '(lambda () (interactive) (progn (forward-line -4) (recenter))))

;; Change C-x C-b behavior so it uses bs; shows only interesting buffers.
(global-set-key "\C-x\C-b" 'bs-show)

;; The first invocation of Home/End moves to the beginning of the *text* line.
;; A second invocation moves the cursor to beginning of the *absolute* line.
;; Most of the time this won't matter even be noticeable, but when it does (in
;; comments, for example) it will quite convenient.  By sw77@cornell.edu.
(global-set-key [home] 'bcm-my-smart-home)
(global-set-key [end] 'bcm-my-smart-end)
(defun bcm-my-smart-home ()
  "Odd home to beginning of line, even home to beginning of text/code."
  (interactive)
  (if (and (eq last-command 'bcm-my-smart-home)
           (/= (line-beginning-position) (point)))
      (beginning-of-line)
      (beginning-of-line-text)))
(defun bcm-my-smart-end ()
  "Odd end to end of line, even end to begin of text/code."
  (interactive)
  (if (and (eq last-command 'bcm-my-smart-end)
           (= (line-end-position) (point)))
      (bcm-end-of-line-text)
      (end-of-line)))
(defun bcm-end-of-line-text ()
  "Move to end of current line and skip comments and trailing space."
  (interactive)
  (end-of-line)
  (let ((bol (line-beginning-position)))
    (unless (eq font-lock-comment-face (get-text-property bol 'face))
      (while (and (/= bol (point))
                  (eq font-lock-comment-face
                      (get-text-property (point) 'face)))
        (backward-char 1))
      (unless (= (point) bol)
        (forward-char 1) (skip-chars-backward " \t\n")))))
;; But what about the normal use for home and end?  We can still have them!
;; Just prefixed with control.
(global-set-key [\C-home] 'beginning-of-buffer)
(global-set-key [\C-end] 'end-of-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous Customizations

;; This sets garbage collection to hundred times of the default; supposedly
;; significantly speeds up startup time.  Disable this if RAM is limited.
(setq gc-cons-threshold 50000000)

;; Warn only when opening files bigger than 100MB (default is 10MB).
(setq large-file-warning-threshold 100000000)

;; Prevent windows from getting too small.
(setq window-min-height 3)

;; Show column number in mode line.
(setq column-number-mode t)

;; Variables to mark as safe.
(setq safe-local-variable-values '((outline-minor-mode . t)
                                   (eldoc-mode . t)))

;; Set shells.
(when *freebsd-system*
  (setq shell-file-name "/usr/local/bin/zsh")
  (setq tex-shell-file-name "/usr/local/bin/zsh"))
(when *linux-system*
  (setq shell-file-name "/usr/bin/zsh")
  (setq tex-shell-file-name "/usr/bin/zsh"))
(when *nt-system*
  (setq shell-file-name "/usr/bin/bash")
  (setq tex-shell-file-name "/usr/bin/bash"))
(when *osx-system*
  (setq shell-file-name "/bin/zsh")
  (setq tex-shell-file-name "/bin/zsh"))

;; Answer 'y' or <CR> for yes and 'n' for no at minibar prompts.
(defalias 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

;; Always use the echo area instead of dialog boxes in console mode.
(when (not window-system)
  (setq use-dialog-box nil))

;; Don't echo passwords when communicating with interactive programs.
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; Gets rid of disabled commands prompting.
(setq disabled-command-hook nil)

;; Allow seamless editing of files in a tar/jar/zip file.
(auto-compression-mode 1)

;; We can also get completion in the mini-buffer as well.
(icomplete-mode t)

;; Completion ignores case.
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
;; In Emacs <23, use the following.
;(setq completion-ignore-case t)

;; Completion ignores filenames ending in any string in this list.
(setq completion-ignored-extensions
  '(".o" ".elc" ".class" "java~" ".ps" ".abs" ".mx" ".~jv" ".bak" ))

;; Startup message with Emacs version.  Modified from original at:
;; http://www.emacswiki.org/emacs/DotEmacsChallenge
(defun bcm-emacs-reloaded ()
  "Display animated startup message."
  (animate-string (concat ";; Initialization successful.  Welcome to "
      (substring (emacs-version) 0 16)
      ".")
    0 0)
  (newline-and-indent)  (newline-and-indent))
(add-hook 'after-init-hook 'bcm-emacs-reloaded)

;; Call this function to increase/decrease font size.
(defun bcm-zoom (n)
  "With positive N, increase the font size, otherwise decrease it."
  (set-face-attribute 'default (selected-frame) :height
                      (+ (face-attribute 'default :height)
                         (* (if (> n 0) 1 -1) 10))))
;; Add some zoom keybindings.
(global-set-key (kbd "C-+") '(lambda () (interactive) (bcm-zoom 1)))
(global-set-key [C-kp-add] '(lambda () (interactive) (bcm-zoom 1)))
(global-set-key (kbd "C--") '(lambda () (interactive) (bcm-zoom -1)))
(global-set-key [C-kp-subtract] '(lambda () (interactive) (bcm-zoom -1)))

;; time-stamps
;; When there is a "Time-stamp: <>" in the first 10 lines of the file,
;; Emacs will write time-stamp information there when saving.
(setq time-stamp-active t          ; Do enable time-stamps.
      time-stamp-line-limit 10     ; Check first 10 buffer lines for stamp.
      time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; Date format.
(add-hook 'write-file-hooks 'time-stamp) ; Update when saving.

;; I always compile my .emacs, saving about two seconds startup time.  But that
;; only helps if the .emacs.elc is newer than the .emacs.  So, compile .emacs
;; if it's not.
(defun bcm-autocompile ()
  "Compile self in ~/.emacs.d/build"
  (interactive)
  (require 'bytecomp)
  (if (string= (buffer-file-name)
               (expand-file-name
                (concat default-directory "~/.emacs.d/build")))
      (byte-compile-file (buffer-file-name))))
(add-hook 'after-save-hook 'bcm-autocompile)

;; A function to close all buffers except scratch.
(defun bcm-cleanup ()
  "Kill all buffers except *scratch*."
  (interactive)
  (mapcar (lambda (x) (kill-buffer x)) (buffer-list)) (delete-other-windows))

;; Indents the entire buffer according to whatever indenting rules are present.
(defun bcm-indent ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
;; This is so commonly used, binding to F4.
(global-set-key (kbd "<f4>") 'bcm-indent)

;; Convenience function for formatting JSON.  Requires Python.
(defun bcm-json-format ()
  "Format a region of JSON."
  (interactive)
  (save-excursion
   (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name)
   t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Built-in Modes

;; Color themes are now integrated into Emacs 24.
;; Define where to find themes for M-x load-theme and load wombat-custom.
(when (and (>= emacs-major-version 24) window-system)
 (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
 (load-theme 'wombat-custom t nil))

;; scheme-mode
;; Bind M-x run-scheme to Gauche.
;; TODO: Change to use CCL on Windows.
(when (or *freebsd-system* *linux-system*)
  (defvar scheme-program-name "gosh"
    "*Program invoked by the run-scheme command"))
;; Spell-check comments.
(add-hook 'scheme-mode-hook 'flyspell-prog-mode)

;; emacs-lisp-mode
;; Spell-check comments.
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; c-mode
;; Resize the compilation window so that it doesn't take up half the frame.
(setq compilation-window-height 16)
;; Always scroll the compilation window.
(setq compilation-scroll-output t)
;; If there were no errors, there's not much to look at in a compilation
;; buffer, so make it go away in 2 seconds.
(setq compilation-finish-function
      (lambda (buf str)
        (if (or (string-match "exited abnormally" str)
                 (string-match (buffer-name buf) "*grep*"))
            ;; There were errors.
            (message "Compilation errors, press C-x ` to visit.")
            ;; No errors; make the compilation window go away in 2 seconds.
            (run-at-time 2 nil 'delete-windows-on buf)
            (message "Build Succeeded."))))
;; Use c-mode for flex files (cc-mode is probably better for this though).
(setq auto-mode-alist
      (append '(("\\.l$" . c-mode))
                auto-mode-alist))
;; Change default indent style from "gnu".  I actually use 1TBS, but BSD style
;; auto-indents properly.
(setq c-default-style "bsd"
      c-basic-offset 4)
;; Spell-check comments.
(add-hook 'c-mode-hook 'flyspell-prog-mode)

;; java-mode
;; This mode doesn't properly indent Java out of the box.  This combined with
;; the C settings above fixes that.
(add-hook 'java-mode-hook
          (lambda ()
            "Treat Java 1.5 @-style annotations as comments."
            (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;; sql-mode
;; This adds a connection for my local l1j-en test database on MySQL, with the
;; ability to add others later by appending to sql-connection-alist.
(setq sql-connection-alist
      '((pool-a
         (sql-product 'mysql)
         (sql-server "127.0.0.1")
         (sql-user "root")
         (sql-password "lintest")
         (sql-database "l1jdb")
         (sql-port 3306))))
(defun sql-connect-preset (name)
  "Connect to a predefined SQL connection listed in `sql-connection-alist'."
  (eval `(let ,(cdr (assoc name sql-connection-alist))
           (flet ((sql-get-login (&rest what)))
             (sql-product-interactive sql-product)))))
;; Execute this function to log in.
(defun sql-pool-a ()
  "Connect to SQL pool 0."
  (interactive)
  (sql-connect-preset 'pool-a))
;; Use sql-mode for .script files (used by Jetty and Tomcat).
(add-to-list 'auto-mode-alist '("\\.script$" . sql-mode))
;; Add an auto-mode for the HiveQL extension I use.
(add-to-list 'auto-mode-alist '("\\.hql$" . sql-mode))

;; flymake
;; See flymake-cursor entry for minibuffer fix.
(global-set-key "\C-c[" 'flymake-goto-prev-error)
(global-set-key "\C-c]" 'flymake-goto-next-error)

;; prolog-mode
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(setq prolog-system 'swi)
(setq prolog-program-switches
      '((sicstus ("-i")) (swi ("-L0" "-G0" "-T0" "-A0")) (t nil)))
;; Since .pl is more commonly used as an extension for Perl, I use .plg.
(add-to-list 'auto-mode-alist '("\\.plg$" . prolog-mode))
;; Add auto-mode for Mercury source, which is close enough to Prolog to benefit
;; from syntax highlighting.  This overrides the default ObjC auto-mode for .m.
(setq auto-mode-alist (cons '("\\.m$" . prolog-mode) auto-mode-alist))

;; cperl-mode
;; Always use cperl-mode instead of perl-mode.
(defalias 'perl-mode 'cperl-mode)
(add-to-list 'auto-mode-alist '("\\.pl$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.cgi$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pm$" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

;; nxml-mode
;; Included in Emacs 23.  Using nXhtml for .xhtml files.
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\)\\'" . nxml-mode)
            auto-mode-alist))

;; conf-mode
;; Ignore single quote highlighting in .properties files.
(add-hook 'conf-javaprop-mode-hook
          '(lambda () (conf-quote-normal nil)))

;; shell-mode
;; Use ANSI colors within shell-mode.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; flyspell
;; Turn on flyspell mode for text editing.
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
;; aspell > ispell
;; Suggestion mode tuned to fastest possible.
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))
;; Solves aspell startup problem on some Linuxes.
(setq flyspell-issue-welcome-flag nil)

;; org-mode: Now included with >22.1.
;; Initiate org-mode when opening .org files.
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; C-c l stores links, C-c C-l calls them.
(define-key global-map "\C-cl" 'org-store-link)
;; org-agenda displays this week's scheduled items.
(define-key global-map "\C-ca" 'org-agenda)
;; Change default TODO keywords and coloring.
(setq
 org-src-fontify-natively t
 org-todo-keywords (quote ((sequence
                            "TODO(t)"
                            "STARTED(s!)"
                            "|"
                            "DONE(d!/!)"
                            "CANCELED(c!)")))
 org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("STARTED" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("CANCELED" :foreground "light sky blue" :weight bold))))
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; remember-mode: Now included in Emacs 23.
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
;; Setup for C-c r note taking.
(setq remember-data-file "~/notes.org")
(global-set-key (kbd "\C-cr") 'org-remember)

;; add-log
;; Auto-add new entry to CHANGELOG found up parent dir hierarchy with C-x 4 a.
(setq user-mail-address "bm3719@gmail.com")  ; Default: user@host
(setq change-log-default-name "CHANGELOG")   ; Default: ChangeLog

;; savehist-mode
;; Mode requires customizations set prior to enabling.
(setq savehist-additional-variables
      '(search-ring regexp-search-ring)    ; Save search entries.
      savehist-file "~/.emacs.d/savehist") ; Keep this out of ~.
(savehist-mode t)                          ; Turn savehist-mode on.

;; calendar
;; Add calendar control-navigation.
(add-hook 'calendar-load-hook
  '(lambda ()
    (define-key calendar-mode-map "\C-x>" 'scroll-calendar-right)
    (define-key calendar-mode-map "\C-x<" 'scroll-calendar-left)))
;; Change some self-explanatory calendar settings.
(setq
 mark-holidays-in-calendar t
 all-christian-calendar-holidays t
 all-islamic-calendar-holidays nil
 all-hebrew-calendar-holidays nil
 display-time-24hr-format t)

;; diary
(setq diary-file "~/.emacs.d/.diary")    ; Might as well keep this out of ~.
(setq mark-diary-entries-in-calendar t)  ; Add entries to calendar.

;; tetris
(setq tetris-score-file "~/.emacs.d/tetris-scores") ; Moved from ~.

;; rmail
(setq mail-archive-file-name "~/Mail/sent")  ; Reuse the gnus mail dir.
(defconst user-mail-address "bm3719@gmail.com")

;; Alias man to woman, since the latter offers completion.
(defalias 'man 'woman)

;; Emacs bookmarks
;; NOTE: C-x r m: create new bookmark, C-x r b: navigate to bookmark, C-x r l:
;;       list bookmarks.
(setq
 bookmark-default-file "~/.emacs.d/bookmarks" ; Moved from ~.
 bookmark-save-flag 1)                        ; Autosave each change.

;; Extra miscellaneous mode associations.
(setq auto-mode-alist (cons '("\\.plan$" . text-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.project$" . text-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.doc$" . text-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.zsh$" . sh-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\CHANGELOG$" . text-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\INSTALL$" . text-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\README$" . text-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\TODO$" . text-mode) auto-mode-alist))      

;; Custom generic mode for arff files (Used with Weka).
(require 'generic)
(define-generic-mode 'arff-file-mode
    (list ?%)
  (list "attribute" "relation" "end" "data")
  '(("\\('.*'\\)" 1 'font-lock-string-face)
    ("^\\@\\S-*\\s-\\(\\S-*\\)" 1 'font-lock-string-face)
    ("^\\@.*\\(real\\)" 1 'font-lock-type-face)
    ("^\\@.*\\(integer\\)" 1 'font-lock-type-face)
    ("^\\@.*\\(numeric\\)" 1 'font-lock-type-face)
    ("^\\@.*\\(string\\)" 1 'font-lock-type-face)
    ("^\\@.*\\(date\\)" 1 'font-lock-type-face)
    ("^\\@.*\\({.*}\\)" 1 'font-lock-type-face)
    ("^\\({\\).*\\(}\\)$" (1 'font-lock-reference-face)
     (2 'font-lock-reference-face))
    ("\\(\\?\\)" 1 'font-lock-reference-face)
    ("\\(\\,\\)" 1 'font-lock-keyword-face)
    ("\\(-?[0-9]+?.?[0-9]+\\)" 1 'font-lock-constant-face)
    ("\\(\\@\\)" 1 'font-lock-preprocessor-face))
  (list "\.arff?")
  (list
   (function
    (lambda ()
     (setq font-lock-defaults
           (list 'generic-font-lock-defaults nil t   ; case insensitive
                 (list (cons ?* "w") (cons ?- "w"))))
     (turn-on-font-lock)))) "Mode for arff-files.")

;; Use file<pathname> instead of file<n> to uniquify buffer names.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; server-mode: This starts up a server automatically, allowing emacsclient to
;; connect to a single Emacs instance.  If a server already exists, it is
;; killed.
(server-force-delete)
(server-start)

;; comint-mode
;; Various comint settings.
(setq comint-scroll-to-bottom-on-input t
      comint-scroll-to-bottom-on-output nil
      comint-scroll-show-maximum-output t
      ;; Match most shells' insert of space/slash after file completion.
      comint-completion-addsuffix t
      comint-buffer-maximum-size 100000
      comint-input-ring-size 5000)

;; TRAMP
(when *nt-system*
  (setq shell-file-name "bash")
  (setq explicit-shell-file-name shell-file-name))
(setq tramp-default-method "scp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External Addons

;; Add all ~/.emacs.d subfolders to load path.
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
        (let* ((my-lisp-dir "~/.emacs.d/")
              (default-directory my-lisp-dir))
           (add-to-list 'load-path my-lisp-dir)
           (normal-top-level-add-subdirs-to-load-path)))

;; pabbrev: Add this to the mode-hook for any major modes I want this
;; lightweight completion auto-activated.
;; http://homepages.cs.ncl.ac.uk/phillip.lord/download/emacs/pabbrev.el
(require 'pabbrev)
;; Disable minibuffer message when expansion occurs.
(setq pabbrev-idle-timer-verbose nil)

;; pretty-symbols.el: Converts various mathematical symbols and Greek letters
;; to their Unicode versions.  Useful for Lisp-variants, ML-variants, and
;; Haskell.
(require 'pretty-symbols)

;; volatile-highlights.el
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; SLIME
;; http://common-lisp.net/project/slime/
(when (or *freebsd-system* *osx-system*) ; FreeBSD CVS version.
  (setq inferior-lisp-program "/usr/local/bin/sbcl"
        common-lisp-hyperspec-root
        "file:///usr/local/share/doc/clisp-hyperspec/HyperSpec/"))
(when *linux-system*   ; Linux CVS version (only using with remote SBCL).
  (setq inferior-lisp-program "/usr/bin/sbcl"
        common-lisp-hyperspec-root "file:///home/bm3719/doc/HyperSpec/"))
(when *nt-system*      ; Windows CVS version.
  (setq inferior-lisp-program "sbcl.exe"
        common-lisp-hyperspec-root "file:///C:/bm3719/doc/HyperSpec/"))
;; Common SLIME setup.
(setq lisp-indent-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-startup-animation t
      slime-complete-symbol*-fancy t)
(require 'slime)

;; Startup SLIME when a Lisp file is open.
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda ()
                                     (inferior-slime-mode t)))
(global-set-key "\C-cs" 'slime-selector)

;; SLIME contribs.
(slime-setup '(slime-autodoc       ; Show information about symbols near point.
               slime-fancy         ; Some fancy SLIME contribs.
               slime-banner        ; Persistent header line, startup animation.
               slime-asdf          ; ASDF support.
               slime-indentation)) ; Customizable indentation.
;; Indentation customizations.
(setq lisp-lambda-list-keyword-parameter-alignment t)
(setq lisp-lambda-list-keyword-alignment t)
;; SLIME contribs init.
(slime-banner-init)          ; Sets banner function to slime-startup-message.
(slime-asdf-init)            ; Hooks slime-asdf-on-connect.
;; Spell-check comments.
(add-hook 'slime-mode-hook 'flyspell-prog-mode)
;; Enable pretty-symbols for Greek letters.
;(add-hook 'slime-mode-hook 'pretty-greek)

;; Translates from Emacs buffer to filename on remote machine.
(setf slime-translate-to-lisp-filename-function
  (lambda (file-name)
    (subseq file-name (length "/ssh:[userid]:")))
  slime-translate-from-lisp-filename-function
    (lambda (file-name)
    (concat "/[userid]:" file-name)))

;; Fontify *slime-description* buffer.
(defun slime-description-fontify ()
  "Fontify sections of SLIME Description."
  (with-current-buffer "*slime-description*"
    (highlight-regexp
     (concat "^Function:\\|"
             "^Macro-function:\\|"
             "^Its associated name.+?) is\\|"
             "^The .+'s arguments are:\\|"
             "^Function documentation:$\\|"
             "^Its.+\\(is\\|are\\):\\|"
             "^On.+it was compiled from:$")
     'hi-blue)))
(defadvice slime-show-description (after slime-description-fontify activate)
  "Fontify sections of SLIME Description."
  (slime-description-fontify))

;; Improve usability of slime-apropos: slime-apropos-minor-mode
(defvar slime-apropos-anchor-regexp "^[^ ]")
(defun slime-apropos-next-anchor ()
  "Navigate to next SLIME apropos anchor."
  (interactive)
  (let ((pt (point)))
    (forward-line 1)
    (if (re-search-forward slime-apropos-anchor-regexp nil t)
        (goto-char (match-beginning 0))
      (goto-char pt)
      (error "anchor not found"))))
(defun slime-apropos-prev-anchor ()
  "Navigate to previous SLIME apropos anchor."
  (interactive)
  (let ((p (point)))
    (if (re-search-backward slime-apropos-anchor-regexp nil t)
        (goto-char (match-beginning 0))
      (goto-char p)
      (error "anchor not found"))))
(defvar slime-apropos-minor-mode-map (make-sparse-keymap))
(define-key slime-apropos-minor-mode-map "\C-m" 'slime-describe-symbol)
(define-key slime-apropos-minor-mode-map "l" 'slime-describe-symbol)
(define-key slime-apropos-minor-mode-map "j" 'slime-apropos-next-anchor)
(define-key slime-apropos-minor-mode-map "k" 'slime-apropos-prev-anchor)
(define-minor-mode slime-apropos-minor-mode "")
(defadvice slime-show-apropos (after slime-apropos-minor-mode activate)
  ""
  (when (get-buffer "*SLIME Apropos*")
    (with-current-buffer "*SLIME Apropos*" (slime-apropos-minor-mode 1))))
 
;; clojure-mode and CIDER (via Marmalade).
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(defvar my-packages '(clojure-mode
                      clojure-test-mode
                      cider))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
;; CIDER
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'flyspell-prog-mode)
(defun cider-reset ()
  "Sends (refresh) to the remote CIDER REPL buffer.  Only works
in M-x cider buffers connected to localhost."
  (interactive)
  (set-buffer "*cider-repl 127.0.0.1*")
  (goto-char (point-max))
  (insert "(refresh)")
  (cider-repl-return))
;(define-key cider-mode-map "\C-c\C-o" 'cider-reset)
;; kibit
;; https://github.com/jonase/kibit
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
             '(kibit "At \\([^:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0))
(add-to-list 'compilation-error-regexp-alist 'kibit)
(defun kibit ()
  "Run kibit on the current project.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile "lein kibit")
  ;; This will clobber the custom function set above.
  (setq compilation-finish-function '()))
(defun kibit-current-file ()
  "Run kibit on the current file.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile (concat "lein kibit " buffer-file-name)))
;; rainbow-delimiters.el
;; https://github.com/jlr/rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;; ac-nrepl: In-buffer completion for Clojure projects.
;; https://github.com/clojure-emacs/ac-nrepl
(require 'ac-nrepl)
(defun clojure-auto-complete ()
  (interactive)
  (let ((ac-sources
          `(ac-source-nrepl-ns
            ac-source-nrepl-vars
            ac-source-nrepl-ns-classes
            ac-source-nrepl-all-classes
            ac-source-nrepl-java-methods
            ac-source-nrepl-static-methods
            ,@ac-sources)))
    (auto-complete)))
(defun bcm-clojure-hook ()
  (auto-complete-mode 1)
  (define-key clojure-mode-map
      (kbd "<backtab>") 'clojure-auto-complete))
(add-hook 'clojure-mode-hook 'bcm-clojure-hook)

;; scala-mode
;; https://github.com/scala/scala-dist/tree/master/tool-support/src/emacs
(require 'scala-mode-auto)
;; ENSIME
;; https://github.com/aemoncannon/ensime
(require 'ensime)
(add-hook 'scala-mode-hook
          (lambda ()
            (local-set-key "\C-c\C-t\C-b" 'scala-eval-buffer)
            (local-set-key "\C-c\C-t\C-r" 'scala-eval-region)
            'ensime-scala-mode-hook))

;; haskell-mode
;; https://github.com/haskell/haskell-mode/
(cond ((or *freebsd-system* *linux-system* *osx-system*) ; FreeBSD/Linux/OSX CVS versions.
       (load "~/.emacs.d/haskell-mode/haskell-site-file.el"))
      (*nt-system*                          ; NT manual install.
       (load "C:\\bm3719\\.emacs.d\\haskell-mode\\haskell-site-file.el")))
;; Append haskell extensions to auto-mode association list.
(setq auto-mode-alist
  (append auto-mode-alist
    '(("\\.[hg]s$" . haskell-mode)
      ("\\.hi$" . haskell-mode)
      ("\\.l[hg]s$" . literate-haskell-mode))))
(autoload 'haskell-mode "haskell-mode"
   "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
   "Major mode for editing literate Haskell scripts." t)
;; Haskell flymake
(defun flymake-get-haskell-cmdline (source base-dir)
  "Handles command line GHC call."
  (list "ghc"
        (list "--make" "-fbyte-code"
              ;; Expand for additional -i options as in the Perl script.
              (concat "-i" base-dir)
              source)))
(defun flymake-haskell-init ()
  "Initialize flymake-haskell."
  (flymake-simple-make-init-impl
   'flymake-create-temp-with-folder-structure nil nil
   (file-name-nondirectory buffer-file-name)
   'flymake-get-haskell-cmdline))
(defvar multiline-flymake-mode nil)
(defvar flymake-split-output-multiline nil)
;; This needs to be advised as flymake-split-string is used in other places and
;; I don't know of a better way to get at the caller's details.
(defadvice flymake-split-output
    (around flymake-split-output-multiline activate protect)
  (if multiline-flymake-mode
      (let ((flymake-split-output-multiline t))
        ad-do-it)
      ad-do-it))
(defadvice flymake-split-string
    (before flymake-split-string-multiline activate)
  (when flymake-split-output-multiline
    (ad-set-arg 1 "^\\s *$")))
(eval-after-load "flymake"
  '(progn
    (add-to-list 'flymake-allowed-file-name-masks
     '("\\.l?hs$" flymake-haskell-init flymake-simple-java-cleanup))
    (add-to-list 'flymake-err-line-patterns
     '("^\\(.+\\.l?hs\\):\\([0-9]+\\):\\([0-9]+\\):\\(\\(?:.\\|\\W\\)+\\)"
       1 2 3 4))))
;; haskell-mode-hook customizations.
(add-hook 'haskell-mode-hook
          '(lambda ()
            (turn-on-haskell-decl-scan)
            (turn-on-haskell-doc-mode)
            (turn-on-font-lock)
            ;; Mutually exclusive indenting modes - choose one.
            (turn-on-haskell-indentation)
            ;; (turn-on-haskell-indent)
            ;; (turn-on-haskell-simple-indent)
            ;; Spell-check comments.
            (flyspell-prog-mode)
            ;; Highlight trailing whitespace.
            (setq show-trailing-whitespace t)
            ;; Enable Greek letters and math symbols.
            ;(pretty-greek)
            ;; flymake (GHC is a bit slow, so disable this on old machines).
            (set (make-local-variable 'multiline-flymake-mode) t)
            (flymake-mode 1)))
;; literate-haskell-mode hook customization.
(add-hook 'literate-haskell-mode
          '(lambda ()
            (flyspell-mode 1)
            (pretty-greek)
            (haskell-unicode)
            (flymake-mode 1)))
;; Write literate Haskell in LaTeX style (default is Bird style).  This
;; requires code blocks to be between \begin{code} and \end{code}.
;; http://www.haskell.org/haskellwiki/Literate_programming
;(setq haskell-literate-default 'latex)
;; Get rid of file dialogs in GUI mode.  This only shows up for me in GHCI
;; errors, so putting it here.
(setq use-dialog-box nil)

;; python-mode: Replaces the built-in python.el, though I'm no longer using its
;; integrated iPython support.
;; http://launchpad.net/python-mode/
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
;; NOTE: python-describe-symbol requires the python-doc-html package and the
;;       PYTHONDOCS environment variable to be set.  This isn't valid in
;;       python-mode though, only in python.el.
(add-hook 'python-mode-hook
          (lambda ()
            (set (make-variable-buffer-local 'beginning-of-defun-function)
                 'py-beginning-of-def-or-class)
            (setq outline-regexp "def\\|class ")
            (pabbrev-mode)
            (flyspell-prog-mode)
            (flymake-mode)
            (local-set-key "\C-cL" 'py-execute-buffer)))
;; Replaced pylint with pyflakes, as it's super fast.  However, it doesn't
;; catch a lot of style problems, so it's still a good idea to pylint it later.
;; http://www.emacswiki.org/emacs/PythonProgrammingInEmacs#toc9
(defun flymake-pyflakes-init ()
  "Initialize Flymake for Python, using pyflakes."
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "pyflakes" (list local-file))))
(when (load "flymake" t)
  (push '("\\.py\\'" flymake-pyflakes-init)
        flymake-allowed-file-name-masks))

;; helm: An incremental completion and selection narrowing framework.
;; https://github.com/emacs-helm/helm
(require 'helm-config)

;; ruby-mode
;; http://www.emacswiki.org/emacs/RubyMode
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
         '(lambda ()
           (inf-ruby-keys)
           (flyspell-prog-mode)))

;; groovy-mode
;; https://raw.githubusercontent.com/nealford/emacs/master/groovy-mode.el
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
(autoload 'groovy-mode "groovy-mode" "Groovy mode." t)
(defconst groovy-block-mid-re "Need something here or it blows up.")
;; Add auto-indenting on newline.
(add-hook 'groovy-mode-hook
          (lambda ()
            (local-set-key "\C-m" 'reindent-then-newline-and-indent)))

;; AUCTeX
;; http://www.gnu.org/software/auctex/
;; FreeBSD ports, Linux apt-get version.
(when (not *nt-system*)
  (load "auctex.el" nil t t)
  (load "preview-latex.el" nil t t)
  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  ;; Enable this when working with multi-file document structures.
  ;(setq-default TeX-master nil)
  ;; Enable document parsing.
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  ;; Full section options.  See Sectioning page in AUCTeX info.
  (setq LaTeX-section-hook
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label)))

;; web-mode: An autonomous major-mode for editing web templates (HTML documents
;; embedding parts (CSS/JavaScript) and blocks (client/server side).
;; https://github.com/fxbois/web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; flymake-cursor
;; http://www.emacswiki.org/emacs/download/flymake-cursor.el
(require 'flymake-cursor)

;; js2-mode
;; https://github.com/mooz/js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))

;; flymake-jshint
;; https://github.com/daleharvey/jshint-mode/blob/master/flymake-jshint.el
(require 'flymake-jshint)
;; Leaving flymake-jshint off by default due to bugs.
;(add-hook 'js2-mode-hook (lambda () (flymake-jshint)))

;; gnuplot-mode
;; https://raw.github.com/mkmcc/gnuplot-mode/master/gnuplot-mode.el
(require 'gnuplot-mode)
(add-hook 'gnuplot-mode-hook
          '(lambda ()
            (flyspell-prog-mode)
            (add-hook 'before-save-hook
             'whitespace-cleanup nil t)))
;; .gp is my personally-designated Gnuplot extension.
(add-to-list 'auto-mode-alist '("\\.gp$" . gnuplot-mode))

;; markdown-mode
;; git://jblevins.org/git/markdown-mode.git
(autoload 'markdown-mode "markdown-mode"
          "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; CEDET
;; Included in Emacs >=23.2.
(setq semantic-load-turn-useful-things-on t)
;; Keep semantic.cache files from littering my FS.
(setq semanticdb-default-save-directory "~/.emacs.d/saves/semantic.cache")
(require 'cedet)

;; Maxima support
;; NOTE: Gnuplot on Windows not setup yet.
(setq auto-mode-alist (cons '("\\.max" . maxima-mode) auto-mode-alist))
(when *freebsd-system*
  (setq load-path (cons "/usr/local/share/maxima/5.31.3/emacs" load-path)))
(when *linux-system*
  (setq load-path (cons "/usr/share/maxima/5.20.1/emacs" load-path)))
(when *nt-system*
  (setq load-path
        (cons "C:\\bin\\utils\\maxima\\share\\maxima\\5.20.1\\emacs"
              load-path)))
(autoload 'maxima "maxima" "Running Maxima interactively" t)
(autoload 'maxima-mode "maxima" "Maxima editing mode" t)

;; Mutt client integration.
;; This associates file whose name contains "/mutt" to be in mail-mode and the
;; "It's All Text" FF add-on (Windows only).
(add-to-list 'auto-mode-alist
             '("/mutt-\\|itsalltext.*mail\\.google" . mail-mode))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)
;; Use C-c C-c to complete mutt message buffers without prompting for saving.
(add-hook
 'mail-mode-hook
 (lambda ()
   (define-key mail-mode-map [(control c) (control c)]
     (lambda ()
       (interactive)
       (save-buffer)
       (server-edit)))))

;; gtags: Requires an install of GNU Global.  Currently only using for c-mode.
(when *freebsd-system*
  (setq load-path (cons "/usr/local/share/gtags" load-path))
  (autoload 'gtags-mode "gtags" "" t)
  (setq c-mode-hook '(lambda () (gtags-mode 1))))

;; elscreen
;; https://raw.githubusercontent.com/shosti/elscreen/master/elscreen.el
(require 'elscreen)
(elscreen-start)
;; F7 creates a new elscreen, F8 kills it.
(global-set-key (kbd "<f7>") 'elscreen-create)
(global-set-key (kbd "<f8>") 'elscreen-kill)

;; emacs-w3m
;; http://emacs-w3m.namazu.org/
;; FreeBSD: ports w3m-m17n; Linux: apt-get w3m w3m-el; Windows: CVS, Cygwin w3m
;; NOTE: I also modify the local copies of w3m.el and w3m-search.el.  See
;;       projects.org for details.
(autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
;; Use w3m for all URLs (deprecated code to use available GUI browser).
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; Optional keyboard short-cut.
(global-set-key "\C-x\M-m" 'browse-url-at-point)
;; Tabs: create: C-c C-t close: C-c C-w nav: C-c C-[np] list: C-c C-s
(setq w3m-use-tab t)
(setq w3m-use-cookies t)
;; Add some extra search engine URIs.
(eval-after-load "w3m-search"
  '(progn (add-to-list 'w3m-search-engine-alist
                       '("hoogle" "http://haskell.org/hoogle/?q=%s" nil)
                       '("ports" "http://freebsd.org/cgi/ports.cgi/?query=%s"
                         nil))
          (add-to-list 'w3m-uri-replace-alist
                       '("\\`h:" w3m-search-uri-replace "hoogle")
                       '("\\`p:" w3m-search-uri-replace "ports"))))

;; multi-term
;; http://www.emacswiki.org/emacs/download/multi-term.el
(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)
(when *freebsd-system*
  (setq multi-term-program "/usr/local/bin/zsh"))
(when *linux-system*
  (setq multi-term-program "/usr/bin/zsh"))
(when *nt-system*
  (setq multi-term-program "/usr/bin/bash"))
(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term)

;; lusty-explorer
;; http://www.emacswiki.org/emacs/download/lusty-explorer.el
(require 'lusty-explorer)
(global-set-key (kbd "C-x C-f") 'lusty-file-explorer)
(global-set-key (kbd "C-x b") 'lusty-buffer-explorer)

;; EMMS
;; http://www.gnu.org/software/emms/
;; Currently using mplayer backend - seems superior to mpg321, which doesn't
;; support seeking.
(require 'emms-setup)
(emms-standard)
(emms-default-players)
(push 'emms-player-mplayer emms-player-list)
;; Show the current track each time EMMS starts to play a track with "NP: ".
(add-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "NP: %s")
;; When asked for emms-play-directory, always start from this one.
(setq emms-source-file-default-directory "~/snd/")
;; Some global playlist management keybindings.
(global-set-key (kbd "<kp-subtract>") 'emms-previous)
(global-set-key (kbd "<kp-add>") 'emms-next)
(global-set-key (kbd "<insert>") 'emms-pause)
(global-set-key (kbd "<kp-insert>") 'emms-pause)
(global-set-key (kbd "<f3>") 'emms)
(global-set-key (kbd "<kp-right>") 'emms-seek-forward)
(global-set-key (kbd "<kp-left>") 'emms-seek-backward)

;; Darcsum: A pcl-cvs like interface for managing darcs patches.
;; http://chneukirchen.org/repos/darcsum/
(require 'darcsum)
;(autoload 'darcs-mode "~/.emacs.d/darcsum/darcsum.el"
;  "Minor mode for dealing with a darcs repository." t)

;; psvn.el: SVN VC
;; http://www.xsteve.at/prg/vc_svn/
;; M-x svn-examine DIR, M-x svn-status DIR
(require 'psvn)

;; Magit
;; https://github.com/magit/magit
;; Note: On FreeBSD, this currently requires using the ports version due to
;; Makefile parsing errors.
(require 'magit)

;; lojban-mode: Requires lojban.el.
;; http://www.emacswiki.org/cgi-bin/wiki/download/lojban-mode.el
;; http://www.emacswiki.org/emacs/download/lojban.el
(autoload 'lojban-parse-region "lojban" nil t)
(autoload 'lojban-mode "lojban-mode" nil t)

;; malyon: Z-machine interpreter.
;; http://www.ifarchive.org/if-archive/infocom/interpreters/emacs/malyon.el
(require 'malyon)

;; redo+.el: An extended version of XEmacs' redo package.
;; http://www.emacswiki.org/emacs/download/redo%2b.el
;; TODO: Consider replacing this with undo-tree-mode.  I'm sticking with this
;; for now since it's considerably more intuitive and the need for undo-trees
;; hasn't ever yet come up.
(require 'redo+)
(global-set-key "\C-x\M-_" 'redo)

;; htmlize.el: Converts buffer to HTML.
;; http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el.cgi
;; TODO: Check if htmlfontify.el (being added in 23.2) is the same as this.
(require 'htmlize)

;; Printing
;; Requires install of Ghostscript and GSView native ports on Windows.
(when *nt-system*
  (progn
    (setq-default ps-lpr-command
                  (expand-file-name
                   "C:\\bin\\utils\\gs\\gsview\\gsview\\gsprint.exe"))
    (setq-default ps-printer-name t)
    (setq-default ps-printer-name-option nil)
    (setq ps-lpr-switches '("-query"))   ; Show printer dialog.
    (setq ps-right-header
          '("/pagenumberstring load" ps-time-stamp-mon-dd-yyyy))))
;; Remap lpr-command to xpp on FreeBSD.  Requires print/xpp port.
(when *freebsd-system*
  (setq lpr-command "xpp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Final init

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((eldoc-mode . t) (outline-minor-mode . t))))
 '(which-function-mode nil)
 '(semantic-complete-inline-analyzer-displayor-class (quote semantic-displayor-tooltip))
 '(semantic-complete-inline-analyzer-idle-displayor-class (quote semantic-displayor-tooltip))
 '(semantic-idle-scheduler-verbose-flag nil)
 '(semantic-imenu-sort-bucket-function (quote semantic-sort-tags-by-name-increasing)))

;; Replace echo area startup message
(run-with-timer 1 nil #'yow)
