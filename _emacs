;;;; -*- mode: Emacs-Lisp; eldoc-mode:t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Bruce C. Miller - bm3719@gmail.com
;;;; Time-stamp: <2012-05-25 21:23:38 (bm3719)>
;;;;
;;;; This init was created for GNU Emacs 23.1.1 for FreeBSD, GNU/Linux, and
;;;; Windows, but all or parts of this file should work with older GNU Emacs
;;;; versions, on other OSes, or even on XEmacs with minor adjustments.
;;;;
;;;; External addons used: pabbrev, pretty-symbols.el, slime, clojure-mode,
;;;; swank-clojure, paredit.el, haskell-mode, agda-mode, gtags, python-mode,
;;;; ipython, ruby-mode, auctex, nxhtml, espresso, flymake-jslint, moz.el,
;;;; batch-mode, sqlplus, cedet, ecb, jdee, jde-eclipse-compiler-server, ess,
;;;; elscreen, elscreen-w3m, w3m (+ flim, apel), multi-term, lusty-explorer,
;;;; emms, color-theme, color-theme-wombat, darcsum, psvn, egg, lojban-mode (+
;;;; lojban.el), lambdacalc, malyon, keywiz, redo+.el, htmlize.el.
;;;;
;;;; External applications used: Gauche, aspell, SBCL, Clojure, GHC, Agda, GNU
;;;; Global, python-doc-html, iPython, pyflakes, Ruby, Rhino, MozRepl, JDK,
;;;; ECJ, R, Maxima, mutt, w3m, xpp (*nix only), Ghostscript/GSView (Windows
;;;; only), Consolas font (Windows only).

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
(global-set-key "\C-x\C-a" 'align-regexp)
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
(defun overwrite-mode (arg) (interactive "P"))

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
  (interactive "P")
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
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Built-in Modes

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

;; sql-mode
;; This adds a connection for my local l1j-en test database on mysql, with the
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
;; http://www.emacswiki.org/cgi-bin/wiki/FlymakeHaskell#toc9
;; Minibuffer fix.  Without this, you can't see the flymake warning text in
;; console mode.  Not ideal, but better than nothing.
(when (fboundp 'resize-minibuffer-mode)
  (resize-minibuffer-mode)
  (setq resize-minibuffer-window-exactly nil))
(defun bcm-flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer."
  (interactive)
  (let* ((line-no (flymake-current-line-no))
         (line-err-info-list (nth 0 (flymake-find-err-info
                                     flymake-err-info line-no)))
         (count (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file (flymake-ler-full-file (nth (1- count)
                                                      line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)))
      (setq count (1- count)))))
;; flymake keybindings.
(global-set-key "\C-c[" 'flymake-goto-prev-error)
(global-set-key "\C-c]" 'flymake-goto-next-error)
(global-set-key "\C-c\\" 'bcm-flymake-display-err-minibuf)

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
(org-remember-insinuate)
(setq remember-annotation-functions '(org-remember-annotation))
;(setq remember-handler-functions '(org-remember-handler))
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

;; server-mode
;; This starts up a server automatically, allowing emacsclient to connect to
;; a single Emacs instance.  If a server already exists, it is killed.
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

;; pabbrev
;; http://homepages.cs.ncl.ac.uk/phillip.lord/download/emacs/pabbrev.el
;; Add this to the mode-hook for any major modes I want this auto-activated.
;; NOTE: Both pabbrev and YASsnippet bind to TAB, so only activate one of them.
(require 'pabbrev)

;; pretty-symbols.el
;; Converts various mathematical symbols and Greek letters to their Unicode
;; versions.  Useful for Lisp-variants, ML-variants, and Haskell.
(require 'pretty-symbols)

;; ;; SLIME
;; ;; http://common-lisp.net/project/slime/
;; (when *freebsd-system* ; FreeBSD CVS version.
;;   (setq inferior-lisp-program "/usr/local/bin/sbcl"
;;         common-lisp-hyperspec-root
;;         "file:///usr/local/share/doc/clisp-hyperspec/HyperSpec/"))
;; (when *linux-system*   ; Linux CVS version (only using with remote sbcl).
;;   (setq inferior-lisp-program "/usr/bin/sbcl"
;;         common-lisp-hyperspec-root "file:///home/bm3719/doc/HyperSpec/"))
;; (when *nt-system*      ; Windows CVS version.
;;   (setq inferior-lisp-program "sbcl.exe"
;;         common-lisp-hyperspec-root "file:///C:/bm3719/doc/HyperSpec/"))
;; ;; Common SLIME setup.
;; (setq lisp-indent-function 'common-lisp-indent-function
;;       slime-complete-symbol-function 'slime-fuzzy-complete-symbol
;;       slime-startup-animation t
;;       slime-complete-symbol*-fancy t)
;; (require 'slime)

;; ;; Startup SLIME when a Lisp file is open.
;; (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;; (global-set-key "\C-cs" 'slime-selector)

;; ;; SLIME contribs.
;; (slime-setup '(slime-autodoc       ; Show information about symbols near point.
;;                slime-fancy         ; Some fancy SLIME contribs.
;;                slime-banner        ; Persistent header line, startup animation.
;;                slime-asdf          ; ASDF support.
;;                slime-indentation)) ; Customizable indentation.
;; ;; Indentation customizations.
;; (setq lisp-lambda-list-keyword-parameter-alignment t)
;; (setq lisp-lambda-list-keyword-alignment t)
;; ;; SLIME contribs init.
;; (slime-banner-init)          ; Sets banner function to slime-startup-message.
;; (slime-asdf-init)            ; Hooks slime-asdf-on-connect.
;; ;; Spell-check comments.
;; (add-hook 'slime-mode-hook 'flyspell-prog-mode)
;; ;; Enable pretty-symbols for Greek letters.
;; (add-hook 'slime-mode-hook 'pretty-greek)

;; ;; Translates from Emacs buffer to filename on remote machine.
;; (setf slime-translate-to-lisp-filename-function
;;   (lambda (file-name)
;;     (subseq file-name (length "/ssh:[userid]:")))
;;   slime-translate-from-lisp-filename-function
;;     (lambda (file-name)
;;     (concat "/[userid]:" file-name)))

;; ;; Fontify *SLIME Description* buffer for SBCL.
;; (defun slime-description-fontify ()
;;   "Fontify sections of SLIME Description."
;;   (with-current-buffer "*SLIME Description <sbcl>*"
;;     (highlight-regexp
;;      (concat "^Function:\\|"
;;              "^Macro-function:\\|"
;;              "^Its associated name.+?) is\\|"
;;              "^The .+'s arguments are:\\|"
;;              "^Function documentation:$\\|"
;;              "^Its.+\\(is\\|are\\):\\|"
;;              "^On.+it was compiled from:$")
;;      'hi-blue)))
;; (defadvice slime-show-description (after slime-description-fontify activate)
;;   "Fontify sections of SLIME Description."
;;   (slime-description-fontify))

;; ;; Improve usability of slime-apropos: slime-apropos-minor-mode
;; (defvar slime-apropos-anchor-regexp "^[^ ]")
;; (defun slime-apropos-next-anchor ()
;;   "Navigate to next SLIME apropos anchor."
;;   (interactive)
;;   (let ((pt (point)))
;;     (forward-line 1)
;;     (if (re-search-forward slime-apropos-anchor-regexp nil t)
;;         (goto-char (match-beginning 0))
;;       (goto-char pt)
;;       (error "anchor not found"))))
;; (defun slime-apropos-prev-anchor ()
;;   "Navigate to previous SLIME apropos anchor."
;;   (interactive)
;;   (let ((p (point)))
;;     (if (re-search-backward slime-apropos-anchor-regexp nil t)
;;         (goto-char (match-beginning 0))
;;       (goto-char p)
;;       (error "anchor not found"))))
;; (defvar slime-apropos-minor-mode-map (make-sparse-keymap))
;; (define-key slime-apropos-minor-mode-map "\C-m" 'slime-describe-symbol)
;; (define-key slime-apropos-minor-mode-map "l" 'slime-describe-symbol)
;; (define-key slime-apropos-minor-mode-map "j" 'slime-apropos-next-anchor)
;; (define-key slime-apropos-minor-mode-map "k" 'slime-apropos-prev-anchor)
;; (define-minor-mode slime-apropos-minor-mode "")
;; (defadvice slime-show-apropos (after slime-apropos-minor-mode activate)
;;   ""
;;   (when (get-buffer "*SLIME Apropos*")
;;     (with-current-buffer "*SLIME Apropos*" (slime-apropos-minor-mode 1))))

;; clojure-mode
;; http://github.com/technomancy/clojure-mode
(require 'clojure-mode)
;; ;; swank-clojure
;; ;; http://github.com/technomancy/swank-clojure
;; ;; NOTE: Using older version, since current SLIME is incompatible with the
;; ;;       latest swank-clojure.  Be sure to copy clojure-contrib directory.
;; (when *nt-system*
;;     (setq swank-clojure-jar-path "C:\\bin\\java\\clojure-1.1.0\\clojure.jar"
;;           swank-clojure-classpath "C:\\bin\\java\\clojure-1.1.0"))
;; (when *freebsd-system*
;;   (setq swank-clojure-jar-path "/usr/local/share/java/classes/clojure.jar"
;;         swank-clojure-classpath "/usr/local/share/java/classes"))
;; (when *linux-system*
;;   (setq swank-clojure-jar-path "/usr/lib/clojure.jar"
;;         swank-clojure-classpath "/usr/lib"))
;; (require 'swank-clojure-autoload)
;; (setq swank-clojure-extra-classpaths (list
;;                                       "~/.emacs.d/clojure"
;;                                       "~/.emacs.d/clojure-contrib"))
;; ;; Restore SBCL as default SLIME Lisp implementation.  To specify clojure on
;; ;; SLIME start, run with: M-- M-x slime clojure
;; (add-to-list 'slime-lisp-implementations '(sbcl ("sbcl")))

;; paredit (beta)
;; http://mumble.net/~campbell/emacs/paredit-beta.el
(require 'paredit)
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; haskell-mode
;; http://projects.haskell.org/haskellmode-emacs/
(cond ((or *freebsd-system* *linux-system*) ; FreeBSD/Linux CVS versions.
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
              ;; Can be expanded for additional -i options as in the Perl
              ;; script.
              (concat "-i"base-dir)
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

;; agda-mode: Cabal version.
(when *freebsd-system*
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate")))
  (autoload 'agda-mode "agda2-mode.el"
    "Major mode for Agda files" t)
  (unless (assoc "\\.agda" auto-mode-alist)
    (setq auto-mode-alist
          (nconc '(("\\.agda" . agda2-mode)
                   ("\\.alfa" . agda2-mode)) auto-mode-alist))))

;; gtags
;; Requires an install of GNU Global.  Currently only using for c-mode.
(when *freebsd-system*
  (setq load-path (cons "/usr/local/share/gtags" load-path))
  (autoload 'gtags-mode "gtags" "" t)
  (setq c-mode-hook '(lambda () (gtags-mode 1))))

;; python-mode: Replaces the built-in python.el.  Currently this is better,
;; since it supports iPython.
;; http://launchpad.net/python-mode/
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
;; NOTE: python-describe-symbol requires the python-doc-html package and
;;       the PYTHONDOCS environment variable to be set.
(add-hook 'python-mode-hook
          (lambda ()
            (set (make-variable-buffer-local 'beginning-of-defun-function)
                 'py-beginning-of-def-or-class)
            (setq outline-regexp "def\\|class ")
            (pabbrev-mode)
            (flyspell-prog-mode)))
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
;; ipython: Support for a replacement to the default Python shell.  Requires an
;; install of the iPython application.  Run with M-x py-shell.
(require 'ipython)
(global-set-key "\C-cL" 'py-execute-buffer)

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

;; nXhtml
;; http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html
;; Includes MuMaMo.
;; TODO: Consider turning off or changing the ugly block-coloring.
;; NOTE: This needs to be run before espresso, since parts of its JavaScript
;;       setup need to get clobbered.
(load "~/.emacs.d/nxhtml/autostart.el")
;; Turn on spell-checking for markup files.
(dolist (hook '(nxhtml-mode-hook))
  (add-hook hook (lambda () (flyspell-prog-mode))))
;; Get rid of the default top-level block color.
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil))))

;; espresso
;; http://download.savannah.gnu.org/releases-noredirect/espresso/espresso.el
;; NOTE: Will eventually be incorporated into Emacs as js-mode.
(autoload #'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

;; flymake-jslint
;; http://www.emacswiki.org/emacs/FlymakeJavaScript
;; This setup requires Rhino (a JavaScript engine), a flymake-jslint.el file in
;; ~/.emacs.d, and JSLint (available from http://www.jslint.com).
;; TODO: Try to get lintnode working instead of this method at some later date.
;;       Last attempt on 05/10/2010 failed.
(require 'flymake-jslint)

;; moz.el
;; http://download.savannah.gnu.org/releases-noredirect/espresso/moz.el
;; NOTE: Requires json.el on <23.
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
;; Now that all JavaScript stuff is setup, create full espresso-mode hook.
(add-hook 'espresso-mode-hook
          '(lambda ()
            (moz-minor-mode 1)
            (pabbrev-mode)
            (flymake-mode)
            (flyspell-prog-mode)))

;; batch-mode
;; http://www.emacswiki.org/emacs/download/batch-mode.el
(require 'batch-mode)

;; sqlplus
;; http://www.emacswiki.org/emacs/SqlPlus
;; Add plsql.el later if needed.
(require 'sqlplus)
(add-to-list 'auto-mode-alist '("\\.sqp\\'" . sqlplus-mode))
;; NOTE: This auto-logs me into SQL*Plus, but doesn't execute commands sent
;;       from the current .sqp file for some reason.  A working method is to
;;       open the .sqp file, and run commands with C-Ret.
(defun bcm-sqlplus ()
  "Start an interactive SQL*Plus session, with pre-defined connection string."
  (interactive)
  (sqlplus "rtrg/rtrg@//localhost:1521/orcl"))

;; CEDET
;; http://cedet.sourceforge.net/
;; NOTE: Included in Emacs 23.2.
(when *nt-system*       ; NT manual install.
  (load-file "~/.emacs.d/cedet-1.0pre6/common/cedet.el"))
(setq semantic-load-turn-useful-things-on t)
;; On FreeBSD and Linux, ECB drags this in as a dependency.
(require 'cedet)

;; ECB
;; http://ecb.sourceforge.net/
;; Windows manual install.
(when *freebsd-system*  ; FreeBSD ports version.
  (add-to-list 'load-path "/usr/local/share/emacs/22.3/site-lisp/ecb"))
(when *linux-system*    ; Linux apt-get version.
  (add-to-list 'load-path "/usr/share/emacs22/site-lisp/ecb"))
(require 'ecb)
;; Keep semantic.cache files from littering my FS.
(setq semanticdb-default-save-directory "~/.emacs.d/saves/semantic.cache")
;; Disable tip of the day (especially annoying in GUI mode).
(setq ecb-tip-of-the-day nil)
;; Ensure the options save buffer doesn't appear.  I left ecb-options-version
;; in the custom-set-variables block, since that's often overwritten when
;; upgrading, in which case that buffer shows up anyway.
(setq ecb-gzip-setup (quote cons)
      ecb-layout-name "leftright1"
      ecb-source-path (quote ("~/"))
      ecb-tar-setup (quote cons)
      ecb-wget-setup (quote cons))

;; JDEE
;; http://jdee.sourceforge.net/
(require 'jde)
;; Kill the bsh buffer created on init.
(when (not (eq nil (get-buffer "*JDEE bsh*")))
  (kill-buffer "*JDEE bsh*"))
;; This fixes the "The JDE does not recognize JDK 1.6 javac." issue.
(when *freebsd-system*
  (setq jde-jdk-registry '(("1.6.0" . "/usr/local/diablo-jdk1.6.0"))))
(when *linux-system*
  ;; Handle differences between Arch and Ubuntu.
  (if (file-exists-p "/usr/lib/jvm/java-6-openjdk")
      (setq jde-jdk-registry '(("1.6.0" . "/usr/lib/jvm/java-6-openjdk")))
      (setq jde-jdk-registry '(("1.6.0" . "/usr/lib/jvm/java-6-sun")))))      
(when *nt-system*
  ;; Handle differences between the 32-bit and 64-bit JDK.
  (if (file-exists-p "C:\\Program Files\\Java\\jdk1.6.0_18")
      (setq jde-jdk-registry
            '(("1.6.0" . "C:\\Program Files\\Java\\jdk1.6.0_18")))
      (setq jde-jdk-registry
            '(("1.6.0" . "C:\\Program Files (x86)\\Java\\jdk1.6.0_18")))))
;; Various JDEE settings.
(setq
 ;; Set the JDK version.
 jde-jdk '("1.6.0")
 ;; Ideally, I'd prefer K&R, but hardly anyone programs Java this way.  I
 ;; toggle this on for my own code though.
 jde-gen-k&r nil
 ;; Enable control flow abbreviations.
 jde-enable-abbrev-mode t
 ;; Set JDEE to show a completion menu instead of just completing with the
 ;; first thing it finds.  Only works in GUI mode.  In console mode, just run
 ;; C-c C-v C-. multiple times to scroll through them.
 jde-complete-function 'jde-complete-menu
 ;; Define defaults for jde-sourcepath and jde-global-classpath.  These can be
 ;; redefined in in prj.el.
 jde-sourcepath '(("."))
 jde-global-classpath '("."))
;; Looks like the `main' control flow template is missing (or maybe never
;; existed), so this is my own version of it.
(jde-gen-define-abbrev-template
 "main"
 '('> "public static void main (String[] args) {" '> 'n '> 'r 'n '> "}" '>))
;; JDEE flymake, using ECJ (the Eclipse batch compiler).  This can not be a
;; relative path.
(when (or *freebsd-system* *linux-system*)
  (defvar ecj-path "/home/bm3719/.ant/lib/ecj-3.6.jar"))
;; Copy this jar from an Eclipse install.  Make sure the version numbers match.
(when *nt-system*
  (defvar ecj-path
    "C:\\bm3719\\bin\\org.eclipse.jdt.core_3.5.2.v_981_R35x.jar"))
;; http://www.emacswiki.org/emacs/jde-eclipse-compiler-server.el
(require 'jde-eclipse-compiler-server)
(setq jde-compiler (list (list "eclipse java compiler server" ecj-path)))
(setq jde-ecj-command-line-args (list "-d" "none"
                                      "-target" "1.6" "-source" "1.6"
                                      "-proceedOnError"))
(push '(".+\\.java$" jde-ecj-flymake-init jde-ecj-flymake-cleanup)
      flymake-allowed-file-name-masks)
(push '("\\(.*?\\):\\([0-9]+\\): error: \\(.*?\\)\n" 1 2 nil 2 3
        (6 compilation-error-face)) compilation-error-regexp-alist)
(push '("\\(.*?\\):\\([0-9]+\\): warning: \\(.*?\\)\n" 1 2 nil 1 3
        (6 compilation-warning-face)) compilation-error-regexp-alist)
;; Define mode hook.  Note that spell-checking comments doesn't operate on doc
;; comments.
(add-hook 'jde-mode-hook '(lambda ()
                           (flymake-mode)
                           (flyspell-prog-mode)))
(global-set-key (kbd "C-c C-v TAB") 'jde-complete-menu)

;; Emacs Speaks Statistics (ESS)
;; http://ess.r-project.org/
(require 'ess-site)
;; Provides the useful ess-rutils-rmall and ess-rutils-rsitesearch.
(require 'ess-rutils)
(setq ess-ask-for-ess-directory nil
      ess-local-process-name "R"
      ess-imenu-use-S t
      ess-language "R"
      ess-pdf-viewer-pref "xpdf"
      ess-ps-viewer-pref "gs")
;; Start R if it's not running when S-RET is hit.
(defun bcm-ess-start-R ()
  "Start R."
  (interactive)
  (when (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
    (delete-other-windows)
    (setq w1 (selected-window))
    (setq w1name (buffer-name))
    (setq w2 (split-window w1))
    (R)
    (set-window-buffer w2 "*R*")
    (set-window-buffer w1 w1name)))
(defun bcm-ess-eval ()
  "Eval ESS region."
  (interactive)
  (bcm-ess-start-R)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
      (call-interactively 'ess-eval-line-and-step)))
;; ESS/iESS mode hooks, defining some extra ESS keybindings.
(add-hook 'ess-mode-hook
          '(lambda ()
            (local-set-key [(shift return)] 'bcm-ess-eval)
            (local-set-key [?\C-c ?\M-r] 'ess-rutils-rmall)
            (local-set-key [?\C-c ?\C-f] 'ess-rutils-rsitesearch)))
(add-hook 'inferior-ess-mode-hook
          '(lambda ()
            (local-set-key [C-up] 'comint-previous-input)
            (local-set-key [C-down] 'comint-next-input)
            (local-set-key [?\C-c ?\M-r] 'ess-rutils-rmall)
            (local-set-key [?\C-c ?\C-f] 'ess-rutils-rsitesearch)))

;; Maxima support
;; NOTE: Gnuplot on Windows not setup yet.
(setq auto-mode-alist (cons '("\\.max" . maxima-mode) auto-mode-alist))
(when *freebsd-system*
  (setq load-path (cons "/usr/local/share/maxima/5.20.1/emacs" load-path)))
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

;; elscreen
;; ftp://ftp.morishima.net/pub/morishima.net/naoto/ElScreen/
;; Screen navigation: C-z [np0-9].
;; Requires Apel, which will need to be manuallly installed on Windows/Linux
(load "elscreen" "ElScreen" t)
;; F7 creates a new elscreen, F8 kills it.
(global-set-key (kbd "<f7>") 'elscreen-create)
(global-set-key (kbd "<f8>") 'elscreen-kill)
;; elscreen-w3m
(require 'elscreen-w3m)

;; emacs-w3m
;; http://emacs-w3m.namazu.org/
;; FreeBSD: ports w3m-m17n; Linux: apt-get w3m w3m-el; Windows: CVS, Cygwin w3m
;; NOTE: I also modify the local copies of w3m.el and w3m-search.el.  See
;;       projects.org for details.
;; Only use w3m as default browser if in text mode.
(if window-system
    (progn
      (setq browse-url-browser-function 'browse-url-generic)
      (when *nt-system*
        (setq browse-url-generic-program
              "C:\\Program Files\\Mozilla Firefox\\firefox.exe"))
      (when *freebsd-system*
        (setq browse-url-generic-program "/usr/local/bin/conkeror"))
      (when *linux-system*
        (setq browse-url-generic-program "/home/bm3719/bin/conkeror")))
    (setq browse-url-browser-function 'w3m-browse-url))
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; Optional keyboard short-cut.
(global-set-key "\C-x\M-m" 'browse-url-at-point)
;; Tabs: create: C-c C-t close: C-c C-w nav: C-c C-[np] list: C-c C-s
(setq w3m-use-tab t)
(setq w3m-use-cookies t)

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
(add-to-list 'load-path "~/.emacs.d/emms/")
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

;; color-theme
;; http://download.savannah.nongnu.org/releases/color-theme/
(require 'color-theme)
(color-theme-initialize)
;; color-theme-wombat: Custom version of wombat color theme, with few colors
;; changed from the original.
(require 'color-theme-wombat)
(when window-system
  (color-theme-wombat))

;; Darcsum: A pcl-cvs like interface for managing darcs patches.
;; http://chneukirchen.org/repos/darcsum/
(require 'darcsum)
;(autoload 'darcs-mode "~/.emacs.d/darcsum/darcsum.el"
;  "Minor mode for dealing with a darcs repository." t)

;; psvn.el: SVN VC
;; http://www.xsteve.at/prg/vc_svn/
;; M-x svn-examine DIR, M-x svn-status DIR
(require 'psvn)

;; egg: Git VC
;; http://github.com/bogolisk/egg
(require 'egg)
;; For some reason, Egg thinks it's a good idea to create this buffer at init.
;; Probably a better course of action for this, but in the meantime, this gets
;; rid of it.
(when (not (eq nil (get-buffer "*Egg:Select Action*")))
  (kill-buffer "*Egg:Select Action*"))

;; lojban-mode: Requires lojban.el.
;; http://www.emacswiki.org/cgi-bin/wiki/download/lojban-mode.el
;; http://www.emacswiki.org/emacs/download/lojban.el
(autoload 'lojban-parse-region "lojban" nil t)
(autoload 'lojban-mode "lojban-mode" nil t)

;; lambdacalc.el
;; http://mwolson.org/static/dist/elisp/lambdacalc.el
(require 'lambdacalc)

;; malyon: Z-machine interpreter.
;; http://www.ifarchive.org/if-archive/infocom/interpreters/emacs/malyon.el
(require 'malyon)

;; keywiz: An educational game which tests you on esoteric keystrokes.
;; http://www.ifa.au.dk/~harder/keywiz.el
(require 'keywiz)

;; redo+.el: An extended version of XEmacs' redo package.
;; http://www.emacswiki.org/emacs/download/redo%2b.el
;; TODO: Consider replacing this with undo-tree-mode.  I'm sticking with this
;; for now since it's considerably more intuitive and the need for undo-trees
;; hasn't ever yet come up.
(require 'redo+)
(global-set-key "\C-x\M-_" 'redo)

;; htmlize.el: Converts buffer to HTML.
;; http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el
;; TODO: Check if htmlfontify.el (being added in 23.2) is the same as this.
(require 'htmlize)

;; Font face
;; Requires an install of Consolas on Windows.  Using Inconsolata on FreeBSD,
;; but defined that in .Xdefaults.
(when *nt-system*
  (set-default-font
   "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1"))

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

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.32")
 '(global-semantic-decoration-mode nil nil (semantic-decorate-mode))
 '(global-semantic-highlight-edits-mode nil nil (semantic-util-modes))
 '(global-semantic-highlight-func-mode nil nil (semantic-util-modes))
 '(global-semantic-idle-completions-mode t nil (semantic-idle))
 '(global-semantic-idle-scheduler-mode nil nil (semantic-idle))
 '(global-semantic-idle-summary-mode nil nil (semantic-idle))
 '(global-semantic-idle-tag-highlight-mode nil nil (semantic-idle))
 '(global-semantic-show-parser-state-mode nil nil (semantic-util-modes))
 '(global-semantic-show-unmatched-syntax-mode nil nil (semantic-util-modes))
 '(global-semantic-stickyfunc-mode nil nil (semantic-util-modes))
 '(global-senator-minor-mode t nil (senator))
 '(safe-local-variable-values (quote ((eldoc-mode . t) (outline-minor-mode . t))))
 '(semantic-complete-inline-analyzer-displayor-class (quote semantic-displayor-tooltip))
 '(semantic-complete-inline-analyzer-idle-displayor-class (quote semantic-displayor-tooltip))
 '(semantic-idle-scheduler-verbose-flag nil)
 '(semantic-imenu-sort-bucket-function (quote semantic-sort-tags-by-name-increasing))
 '(semanticdb-global-mode t nil (semanticdb))
 '(which-function-mode nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Final init

;; Replace echo area startup message
(run-with-timer 1 nil #'yow)
