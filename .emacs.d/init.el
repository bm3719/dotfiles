;;;; -*- mode: Emacs-Lisp; eldoc-mode:t; lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Bruce C. Miller <bm3719@gmail.com>
;;;;
;;;; This init was created for GNU Emacs 29.3 and 28.2 for GNU/Linux, OpenBSD,
;;;; and Windows, but all or parts of this file should work with older GNU
;;;; Emacs versions or on other OSes.
;;;;
;;;; Top-level addons: use-package, gnu-elpa-keyring-update, diminish, counsel,
;;;; ivy-prescient, swiper, volatile-highlights, which-key, dash, powerline,
;;;; pinentry, vundo, org-bullets, org-present, ob-restclient, magit,
;;;; git-gutter, eshell-prompt-extras, lsp-mode, lsp-ivy, aggressive-indent,
;;;; company, smartparens, clojure-mode, cider, flycheck-clj-kondo,
;;;; haskell-mode, lsp-haskell, proof-general, auctex, web-mode, rainbow-mode,
;;;; json-mode, pyvenv, anaconda-mode, company-anaconda, lsp-pyright,
;;;; markdown-mode, gnuplot-mode, w3m, gptel, ob-dall-e-shell, htmlize.
;;;;
;;;; System packages used: aspell, aspell-en, Leiningen, clj-kondo, cljfmt,
;;;; Babashka, GHC, HLS, fzf, rg, mutt, w3m, ollama, Fira Code font.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initial Startup

;; Getting rid of the toolbar first prevents it from showing in the few seconds
;; needed for the rest of this stuff to load, though disabling it in .Xdefaults
;; is even better.
(when (display-graphic-p)
  (tool-bar-mode -1))

;; Font face: Always default to Fira Code or Roboto Mono, if available.
;; Otherwise use Consolas on Windows and go through a priority list of
;; preferred fonts for other OSes.
(when (display-graphic-p)
  (cond
   ((find-font (font-spec :name "Fira Code"))
    (set-frame-font "Fira Code-17"))
   ((find-font (font-spec :name "Roboto Mono"))
    (set-frame-font "Roboto Mono-16"))
   ((eq system-type 'windows-nt)
    (set-frame-font "Consolas-16"))
   ((find-font (font-spec :name "DejaVu Sans Mono"))
    (set-frame-font "DejaVu Sans Mono-15"))
   ((find-font (font-spec :name "Lucida Console"))
    (set-frame-font "Lucida Console-15"))
   ((find-font (font-spec :name "courier"))
    (set-frame-font "courier-16"))))

(setq inhibit-startup-message t)   ; Disable splash screen.
(when (display-graphic-p)
  (set-scroll-bar-mode 'right)     ; If turned on, use right scrollbars.
  (scroll-bar-mode -1)             ; Hide the scroll bar.
  (tooltip-mode 0))                ; Disable tooltips.
(menu-bar-mode -1)                 ; Hide the menu bar.

;; Rearrange the menubars, so it goes tools | buffers | help.
(setq menu-bar-final-items '(tools buffer help-menu))

;; Remove wasted pixels left of col1.
(when (fboundp 'set-fringe-mode)   ; Added in >22.
  (set-fringe-mode 2))             ; Space in pixels.

(global-font-lock-mode 1)          ; Turn on font lock mode everywhere.
(blink-cursor-mode nil)            ; Disable cursor blinking.
(setq visible-bell t)              ; Make bell visible, not aural.

;; ;; Shut off message buffer.  Uncomment on "stable" systems, like laptops.
;; (setq message-log-max nil)
;; ;; Check if message buffer exists before killing (not doing so errors
;; ;; eval-buffer of init file).
;; (when (get-buffer "*Messages*")
;;   (kill-buffer "*Messages*"))

;; Provide a useful error trace if loading this init fails.
(setq debug-on-error t)

;; Set garbage collection to maximum, speeding up startup time.  Disable this
;; if RAM is limited, or set to a fixed amount.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
;; Reset garbage collection to sane defaults after init complete.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1048576 32) ; 32MB.
                  gc-cons-percentage 0.1))) ; GC when heap 10% allocated.

;; Change backup behavior to save in a directory, not in a miscellany of files
;; all over the place, and disable autosaves completely.
(setq make-backup-files t           ; Do make backups.
      backup-by-copying t           ; Don't clobber symlinks.
      backup-directory-alist
      '(("." . "~/.emacs.d/saves")) ; Don't litter my FS tree.
      delete-old-versions t         ; Get rid of old versions of files.
      kept-new-versions 4
      kept-old-versions 2
      version-control t             ; Use versioned backups.
      auto-save-default nil)        ; Normal backups are enough for me.

;; Specify UTF-8 for a few addons that are too dumb to default to it.
(set-default-coding-systems 'utf-8-unix)

;; Sends settings written to custom-set-variables and custom-set-faces to a
;; separate file (instead of appending them here), where they will be ignored.
;; IMPORTANT: This will cause anything done in the customize-* functions to
;; have no effect unless loaded.
(setq custom-file (concat user-emacs-directory "/custom.el"))
;; Enable this if cust-edit output should be loaded.
;; (load-file custom-file)

;; Use fullscreen in GUI mode.
(when (display-graphic-p)
  (toggle-frame-maximized))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic Key Bindings

;; Define a function to auto-delete trailing whitespace upon save.
(defun bcm/delete-ws-save ()
  "Delete trailing whitespace and save."
  (interactive)
  (delete-trailing-whitespace)
  (save-buffer))

;; Provides zap-up-to-char (M-z), different than the default zap-to-char which
;; includes deleting the argument character.
(load-library "misc")

;; Global key (re-)mappings.
(global-set-key (kbd "C-w")     'backward-kill-word)  ; Match the shell's C-w.
(global-set-key (kbd "C-x w")   'kill-region)
(global-set-key (kbd "C-x s")   'bcm/delete-ws-save)
(global-set-key (kbd "C-m")     'newline-and-indent)
(global-set-key (kbd "M-g")     'goto-line)
(global-set-key (kbd "M-G")     'goto-char)
(global-set-key (kbd "C-x C-k") 'kill-current-buffer) ; Bypasses the C-x k prompt.
(global-set-key (kbd "C-x C-v") 'revert-buffer)
(global-set-key (kbd "C-x TAB") 'indent-region)
(global-set-key (kbd "C-c M-e") 'fixup-whitespace)
(global-set-key (kbd "C-c g")   'replace-string)
(global-set-key (kbd "C-c ;")   'comment-region)
(global-set-key (kbd "C-c '")   'uncomment-region)
(global-set-key (kbd "M-/")     'hippie-expand)       ; Instead of dabbrev-expand.
(global-set-key (kbd "M-z")     'zap-up-to-char)      ; Mimic Vim delete to char.
(global-set-key (kbd "M-o")     'other-window)
(global-set-key (kbd "C-x M-a") 'align-regexp)
;; Move set-fill-column from C-x f to C-x M-f, as it's easy to hit this when
;; intending to do a find-file.
(global-set-key (kbd "C-x f")   'find-file)
(global-set-key (kbd "C-x M-f") 'set-fill-column)
(global-set-key (kbd "C-x C-s") 'bcm/delete-ws-save)

;; For quick macro running.
(global-set-key (kbd "<f10>")   'start-kbd-macro)
(global-set-key (kbd "<f11>")   'end-kbd-macro)
(global-set-key (kbd "<f12>")   'call-last-kbd-macro)

;; Cycle through buffers.
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; M-x compile and M-x grep mnemonics.
(global-set-key (kbd "<f5>")    'compile)
(global-set-key (kbd "C-c n")   'next-error)
(global-set-key (kbd "C-c p")   'previous-error)

;; My KVM switch uses scroll lock, and Emacs complains about it.
(global-set-key (kbd "<Scroll_Lock>") 'ignore)
;; Silence *-mouse-9 complaints.
(global-set-key (kbd "<mouse-9>") 'ignore)
(global-set-key (kbd "<double-mouse-9>") 'ignore)
(global-set-key (kbd "<drag-mouse-9>") 'ignore)

;; Disable over-write mode.
(global-unset-key (kbd "<insert>"))
(global-unset-key (kbd "<insertchar>"))

;; Disable suspend-frame on Xorg sessions.
(when (display-graphic-p)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General Text Editing

;; Set fill width to 79 (default was 70).
(setq-default fill-column 79)

;; Heretical tab settings.  Emacs is smart enough to auto-disable this when
;; editing Make files.
(setq-default indent-tabs-mode nil)
;; Using a tab-stop-list will preserve 8-space tabs for documents that have
;; them, but make my own tabs 2 spaces.
(setq tab-stop-list '(2 4 6 8 10 12 14 16 18))
;; (setq-default tab-width 2)

;; Always flash for parens.
(show-paren-mode 1)

;; Enable narrowing of regions.
(put 'narrow-to-region 'disabled nil)

;; Allow a command to erase an entire buffer.
(put 'erase-buffer 'disabled nil)

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
(setq bcm/set-cursor-color-color "")
(setq bcm/set-cursor-color-buffer "")
(defun bcm/set-cursor-color-according-to-mode ()
  "Change cursor color according to some minor modes."
  ;; Set-cursor-color is somewhat costly, so we only call it when needed.
  (let ((color (if buffer-read-only "red" "DarkSlateGray")))
    (unless (and
             (string= color bcm/set-cursor-color-color)
             (string= (buffer-name) bcm/set-cursor-color-buffer))
      (set-cursor-color (setq bcm/set-cursor-color-color color))
      (setq bcm/set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'bcm/set-cursor-color-according-to-mode)

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
;; file.  A very useful default Vim feature.
(save-place-mode 1)

;; I use sentences.  Like this.
(setq sentence-end-double-space t)

;; Allow for mark ring traversal without popping them off the stack.
(setq set-mark-command-repeat-pop t)

;; Text files supposedly end in new lines, or they should.
(setq require-final-newline t)

;; Defines a function to kill text from point to beginning of line.
(defun bcm/backward-kill-line ()
  "Kill chars backward until encountering the beginning of a line."
  (interactive)
  (kill-line 0))
(global-set-key (kbd "M-C-k") 'bcm/backward-kill-line)

;; Replace error message on read-only kill with an echo area message.
(setq-default kill-read-only-ok t)

;; Takes a multi-line paragraph and makes it into a single line of text.
(defun bcm/unfill-paragraph ()
  "Un-fill paragraph at point."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(global-set-key (kbd "M-p") 'bcm/unfill-paragraph)

;; For composing in Emacs then pasting into a word processor, this un-fills all
;; the paragraphs (i.e. turns each paragraph into one very long line) and
;; removes any blank lines that previously separated paragraphs.
(defun bcm/wp-munge ()
  "Un-fill paragraphs and remove blank lines."
  (interactive)
  (let ((save-fill-column fill-column))
    (set-fill-column 1000000)
    (push-mark)
    (push-mark (point-max) nil t)
    (goto-char (point-min))
    (fill-individual-paragraphs (point-min) (point-max))
    (delete-matching-lines "^$")
    (set-fill-column save-fill-column)))

;; Add a function to strip DOS endlines.
(defun bcm/cut-ctrlm ()
  "Cut all visible ^M."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (replace-match "" nil t)))

;; Insert a date string in the format I most commonly use in text files.
(defun bcm/date ()
  "Insert an ISO 8601 formatted date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))
(global-set-key (kbd "C-x C-d") 'bcm/date)
;; Insert a UTC datetime string in ISO 8601 format.
(defun bcm/datetime ()
  "Insert an ISO 8601 formatted datetime string, with time in UTC."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil 1)))

;; I type a lot of λs.  Clobbers reposition-window.
(global-set-key (kbd "C-M-l") (lambda ()
                                (interactive) (insert-char ?λ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer Navigation

;;; Scrolling
;; Fix the whole huge-jumps-scrolling-between-windows nastiness.
(setq scroll-conservatively 4)
;; Don't hscroll unless needed.
(setq hscroll-margin 1)
;; Start scrolling when 2 lines from top/bottom.  Set to 0 on systems where I
;; use ansi-term a lot.  Eshell is okay with this.  Disabling by default.
;; (setq scroll-margin 2)
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

;;; Mouse wheel scrolling
;; Scroll in 1-line increments for the buffer under pointer.
(setq mouse-wheel-follow-mouse t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Change C-x C-b behavior so it uses bs; shows only interesting buffers.  The
;; `a' key will toggle visibility of all.
(global-set-key (kbd "C-x C-b") 'bs-show)

;; The first invocation of Home/End moves to the beginning of the *text* line.
;; A second invocation moves the cursor to beginning of the *absolute* line.
;; Most of the time this won't even be noticeable, but when it does (in
;; comments, for example) it will quite convenient.  By sw77@cornell.edu.
(global-set-key (kbd "<home>") 'bcm/my-smart-home)
(global-set-key (kbd "<end>") 'bcm/my-smart-end)
(defun bcm/my-smart-home ()
  "Odd home to beginning of line, even home to beginning of text/code."
  (interactive)
  (if (and (eq last-command 'bcm/my-smart-home)
           (/= (line-beginning-position) (point)))
      (beginning-of-line)
    (beginning-of-line-text)))
(defun bcm/my-smart-end ()
  "Odd end to end of line, even end to begin of text/code."
  (interactive)
  (if (and (eq last-command 'bcm/my-smart-end)
           (= (line-end-position) (point)))
      (bcm/end-of-line-text)
    (end-of-line)))
(defun bcm/end-of-line-text ()
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
;; Normal home/end prefixed with control.
(global-set-key (kbd "C-<home>") 'beginning-of-buffer)
(global-set-key (kbd "C-<end>") 'end-of-buffer)

;; https://www.emacswiki.org/emacs/TransposeWindows
(defun bcm/transpose-windows ()
  "Transpose the buffers shown in two windows.  Error if
count-windows is not 2."
  (interactive)
  (unless (= 2 (count-windows))
    (error "There are not 2 windows."))
  (let* ((windows (window-list))
         (w1 (car windows))
         (w2 (nth 1 windows))
         (w1b (window-buffer w1))
         (w2b (window-buffer w2)))
    (set-window-buffer w1 w2b)
    (set-window-buffer w2 w1b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous Customization

;; Warn only when opening files bigger than 100MB (default is 10MB).
(setq large-file-warning-threshold 100000000)

;; Prevent windows from getting too small.
(setq window-min-height 3)

;; Show column number in mode line.
(setq column-number-mode t)

;; When opening a file, always follow symlinks.
(setq vc-follow-symlinks t)

;; Auto revert files on change.  When something changes a file, auto-refresh
;; the buffer so they can't get out of sync.
(global-auto-revert-mode t)

;; File variables to mark as safe.
(setq safe-local-variable-values '((outline-minor-mode . t)
                                   (eldoc-mode . t)))

;; Set shells.
(pcase system-type
  ('berkeley-unix (setq shell-file-name "/usr/local/bin/zsh"
                        tex-shell-file-name "/usr/local/bin/zsh"))
  ('gnu/linux     (setq shell-file-name "/bin/zsh"
                        tex-shell-file-name "/bin/zsh"))
  ('windows-nt    (setq shell-file-name "/usr/bin/bash"
                        tex-shell-file-name "/usr/bin/bash")))

;; Answer `y' or RET for yes and `n' for no at minibar prompts.
(defalias 'yes-or-no-p 'y-or-n-p)
;; In y-or-n-p, query-replace, or query-replace-regexp operation, use return to
;; perform action.
(define-key query-replace-map (kbd "RET") 'act)

;; Always use the echo area instead of dialog boxes in console mode.
(setq use-dialog-box nil)

;; Don't echo passwords when communicating with interactive programs.
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; Gets rid of disabled commands prompting.
(setq disabled-command-function nil)

;; Allow seamless editing of files in a tar/jar/zip file.
(auto-compression-mode 1)

;; Completion ignores case.
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; Completion ignores filenames ending in any string in this list.
(let ((exts '(".obj" "java~" ".ps" ".abs" ".mx" ".~jv" ".bak")))
  (dolist (ext exts)
    (unless (member ext completion-ignored-extensions)
      (push ext completion-ignored-extensions))))

;; Add file register to quickly get to this init file.
(set-register ?i '(file . "~/.emacs.d/init.el"))

;; Follow the compilation buffer scroll instead of remaining at the top line.
(setq compilation-scroll-output t)

;;; Time-stamp support
;; When there is a "Time-stamp: <>" in the first 10 lines of the file,
;; Emacs will write time-stamp information there when saving.
(setq time-stamp-active t          ; Do enable time-stamps.
      time-stamp-line-limit 10     ; Check first 10 buffer lines for stamp.
      ;; Date format.  Note that this is a >=27.x format.
      time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S (%u)")
(add-hook 'write-file-functions 'time-stamp) ; Update when saving.

;; Startup message with Emacs version.  Modified from original at:
;; http://www.emacswiki.org/emacs/DotEmacsChallenge
(defun bcm/emacs-reloaded ()
  "Display animated startup message."
  (animate-string
   (concat "# Initialization successful.  Welcome to "
           (substring (emacs-version) 0 14) ".") 0 0)
  (newline-and-indent)  (newline-and-indent))
(add-hook 'after-init-hook 'bcm/emacs-reloaded)

;; Call this function to increase/decrease font size.
(defun bcm/zoom (n)
  "With positive N, increase the font size, otherwise decrease it."
  (set-face-attribute 'default (selected-frame) :height
                      (+ (face-attribute 'default :height)
                         (* (if (> n 0) 1 -1) 10))))
;; Add some zoom keybindings.
(global-set-key (kbd "C-+") (lambda () (interactive) (bcm/zoom 1)))
(global-set-key (kbd "C-<kp-add>") (lambda () (interactive) (bcm/zoom 1)))
(global-set-key (kbd "C--") (lambda () (interactive) (bcm/zoom -1)))
(global-set-key (kbd "C-<kp-subtract>") (lambda () (interactive) (bcm/zoom -1)))

;; If I've edited init.el, byte compile it.  Saves some startup time.
(defun bcm/autocompile-init ()
  "Compile init.el in ~/.emacs.d/."
  (interactive)
  (require 'bytecomp)
  (when (string= (buffer-file-name)
                 (expand-file-name
                  (concat default-directory "init.el")))
    (let ((init-path (expand-file-name "init.el" user-emacs-directory)))
      (byte-compile-file init-path)
      (message "Compiled %s" init-path))))
(add-hook 'after-save-hook (lambda ()
                             (if (string-match-p "/init.el\\'"
                                                 (buffer-file-name))
                                 (bcm/autocompile-init))))

;; A function to close all buffers except scratch.
(defun bcm/cleanup ()
  "Kill all buffers except *scratch*."
  (interactive)
  (mapc (lambda (x) (kill-buffer x)) (buffer-list)) (delete-other-windows))

;; Indents the entire buffer according to whatever indenting rules are present.
(defun bcm/indent ()
  "Indent whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
;; This is so commonly used, binding to F4.
(global-set-key (kbd "<f4>") 'bcm/indent)

;; A function useful for screen-sharing.  Rerun when done to undo.
(defun bcm/screenshare ()
  "Toggle line numbers and git gutter mode (which interferes with
linum-mode)."
  (interactive)
  (if (bound-and-true-p global-display-line-numbers-mode)
      (global-display-line-numbers-mode -1)
    (global-display-line-numbers-mode))
  (if (bound-and-true-p global-git-gutter-mode)
      (global-git-gutter-mode -1)
    (global-git-gutter-mode)))

;; Supporting functions to read API keys from external file.
(defun bcm/strip-trailing-crlf (string)
  "Removes trailing CR/LF characters from a string if they exist."
  (when (string-match "[\r\n]+$" string)
    (setq string (substring string 0 (match-beginning 0))))
  string)
(defun bcm/read-file-contents (filename)
  "Read the contents of file FILENAME and return as a string.
If the file doesn't exist, return an empty string."
  (if (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (buffer-substring-no-properties (point-min) (point-max)))
    ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package System Initialization

;;; package
(require 'package)

;; Read init before initializing packages.
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(package-initialize)

;; If a fresh install, update the repos index.
(unless package-archive-contents
  (package-refresh-contents))
;; Install 'use-package' if necessary.  Needed in <29.x.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Additional debug settings for when use-package config fails.  Only activate
;; when Emacs is flagged with `--debug-init'.
(defvar use-package-verbose)
(defvar use-package-expand-minimally)
(defvar use-package-compute-statistics)
(when init-file-debug
  (setq use-package-verbose t
        use-package-expand-minimally nil
        ;; Pair with `use-package-report' to see results.
        use-package-compute-statistics t))

;; Provides :bind variants.
(require 'bind-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External Addons

;; Always temporarily disable signature checking when updating keyring.
(let ((old package-check-signature))
  (use-package gnu-elpa-keyring-update
    :ensure t
    :preface (setq package-check-signature nil)
    :config (setq package-check-signature old)))

;; Required early to provide :diminish keyword.
(use-package diminish
  :ensure t
  ;; Modify any built-in mode modeline strings that don't respect diminish.
  :custom (eldoc-minor-mode-string " λdoc")
  :hook
  ((emacs-lisp-mode . (lambda () (setq mode-name "e-λ")))
   (clojure-mode . (lambda () (setq mode-name "cλj")))))

(use-package counsel
  :ensure t
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  :bind
  ("C-x C-f"   . counsel-find-file)
  ("C-x b"     . ivy-switch-buffer)
  ("M-x"       . counsel-M-x)
  ("<f1> f"    . counsel-describe-function)
  ("C-h f"     . counsel-describe-function)
  ("<f1> v"    . counsel-describe-variable)
  ("C-h v"     . counsel-describe-variable)
  ("<f1> l"    . counsel-find-library)
  ("C-h l"     . counsel-find-library)
  ("<f2> i"    . counsel-info-lookup-symbol)
  ("C-h i"     . counsel-info-lookup-symbol)
  ("<f2> u"    . counsel-unicode-char)
  ("<f9>"      . counsel-unicode-char)
  ("<f2> j"    . counsel-set-variable)
  ("M-y"       . counsel-yank-pop)
  ("C-c v"     . ivy-push-view)
  ("C-c V"     . ivy-pop-view)
  ("C-x l"     . counsel-locate)
  ("C-x C-l"   . counsel-fzf)
  ("C-x g"     . counsel-rg)
  ("C-x M-s"   . counsel-outline)
  ("C-x C-M-f" . counsel-recentf))

(use-package ivy-prescient
  :ensure t
  :custom (prescient-sort-length-enable nil)
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper-isearch)
  ("C-r" . swiper-isearch-backward))

(use-package volatile-highlights
  :ensure t
  :diminish
  :init (volatile-highlights-mode t))

(use-package which-key
  :ensure t
  :diminish
  :custom
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-idle-delay 1.2)
  :config (which-key-mode))

;; Add elisp equivalents of Clojure's threading macros, `->' and `->>'.
(use-package dash
  :ensure t
  :demand t
  :init (global-dash-fontify-mode))

(use-package powerline
  :ensure t
  :demand t
  :config (powerline-default-theme))

;; Manually installing pinentry from gnu, since `:pin gnu' seems to do nothing.
(unless (package-installed-p 'pinentry)
  (package-install 'pinentry))
(use-package pinentry
  :if (not (eq system-type 'windows-nt))
  :custom (epa-pinentry-mode 'loopback)
  :config (pinentry-start))

(use-package vundo
  :ensure t
  :custom (vundo-glyph-alist vundo-unicode-symbols)
  :bind ("C-x u" . vundo))

(use-package org-bullets
  :ensure t
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

;; Reminder: Use arrow keys to navigate, C-c C-q to quit.
(use-package org-present
  :ensure t
  :custom
  ;; Reduce the huge upscaling of text.  This amount is more reasonable for my
  ;; laptop, but reconsider it for larger displays.
  (org-present-text-scale 2)
  :hook
  ((org-present-mode
    . (lambda () (when (display-graphic-p) (org-display-inline-images))))
   (org-present-mode-quit
    . (lambda () (when (display-graphic-p) (org-remove-inline-images))))))

(use-package ob-restclient
  :ensure t
  :defer t
  :mode "\\.rest$"
  :custom (restclient-inhibit-cookies t)) ; Don't implicitly send cookies.

(use-package magit
  :ensure t
  :defer 3
  ;; Idiomatic fill-column setting for commit messages.
  :hook (git-commit-mode . (lambda () (set-fill-column 72)))
  :init (global-set-key (kbd "<f3>") 'magit-status))

(use-package git-gutter
  :ensure t
  :diminish "Git↓"
  :custom (git-gutter:update-interval 2)
  :config
  (global-git-gutter-mode 1)
  (global-set-key (kbd "C-x M-g") 'git-gutter:toggle)
  (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk))

(use-package eshell-prompt-extras
  :ensure t
  :custom
  (eshell-highlight-prompt nil)
  (eshell-prompt-function 'epe-theme-lambda)
  :config (autoload 'epe-theme-lambda "eshell-prompt-extras"))

(use-package lsp-mode
  :ensure t
  :custom
  (lsp-completion-provider :none)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-snippet nil)
  (lsp-enable-folding nil)
  (lsp-enable-links nil)
  (lsp-enable-dap-auto-configure nil)
  ;; Kill workspace when last workspace buffer is closed.
  (lsp-keep-workspace-alive nil)
  ;; Disable lsp-mode indent, which conflicts with aggressive-indent-mode.
  (lsp-enable-indentation nil)
  ;; Disable these if they get annoying.
  ;; (lsp-modeline-code-actions-enable nil)
  :commands lsp
  ;; Set prefix for lsp-command-keymap.  Needs to be in :init and not :custom.
  :init (setq lsp-keymap-prefix "C-c l")
  :hook (;; Supported language major modes
         (clojure-mode . lsp)
         ;; which-key integration
         (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ivy
  :ensure t
  :after lsp-mode
  :commands lsp-ivy-workspace-symbol
  :hook (lsp-mode . (lambda ()
                      (define-key lsp-mode-map [remap xref-find-apropos]
                                  #'lsp-ivy-workspace-symbol))))

(use-package aggressive-indent
  :ensure t
  :init
  (global-aggressive-indent-mode 1)
  ;; Add any modes I want to exclude from this minor mode.
  (add-to-list 'aggressive-indent-excluded-modes 'web-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'haskell-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'java-mode))

(use-package company
  :ensure t
  :pin gnu
  :diminish " ċ"
  :hook (emacs-lisp-mode . company-mode))

(use-package smartparens
  :ensure t
  :diminish "(ϛ)"
  :hook
  ((emacs-lisp-mode . smartparens-strict-mode)
   (eval-expression-minibuffer-setup . smartparens-strict-mode)
   (scheme-mode . smartparens-strict-mode)
   (emacs-lisp-mode . smartparens-strict-mode)
   (lisp-mode . smartparens-strict-mode)
   (ielm-mode . smartparens-strict-mode))
  :init (require 'smartparens-config)
  :bind
  (:map smartparens-mode-map
        ("M-)" . sp-forward-slurp-sexp)
        ("M-(" . sp-backward-slurp-sexp)
        ("C-c M-)" . sp-forward-barf-sexp)
        ("C-c M-(" . sp-backward-barf-sexp)
        ("M-s" . sp-unwrap-sexp)
        ("M-r" . sp-raise-sexp)))

(use-package clojure-mode
  :ensure t
  :diminish "cλj"
  :hook (clojure-mode . smartparens-strict-mode)
  :bind (:map clojure-mode-map
              ("C-w" . sp-backward-kill-word)))

(use-package cider
  :ensure t
  :defer 3
  :custom (cider-repl-pop-to-buffer-on-connect t)
  :hook
  ((cider-mode . flyspell-prog-mode)
   (cider-mode . which-key-mode)
   (cider-mode . company-mode)
   (cider-repl-mode . smartparens-strict-mode)
   (cider-repl-mode . company-mode)
   ;; Auto-run cljfmt on buffer at save.  Need to do in a lambda to make it
   ;; buffer-local to cider-mode.
   (cider-mode . (lambda ()
                   (add-hook 'before-save-hook 'cider-format-buffer nil t))))
  :init (setq nrepl-log-messages 1) ; Fix missing *nrepl-messages* buffer.
  :bind (:map cider-repl-mode-map
              ("C-c C-l" . cider-repl-clear-buffer)
              ("C-w" . sp-backward-kill-word)))

(use-package flycheck-clj-kondo
  :ensure t
  :defer 7
  :hook (clojure-mode . flycheck-mode))

(use-package haskell-mode
  :ensure t
  :defer 5
  ;; Doesn't work, for unknown reasons.
  :diminish (haskell-mode . "λ≫ ")
  :hook
  ((haskell-mode . interactive-haskell-mode)
   (haskell-mode . haskell-doc-mode)
   (haskell-mode . (lambda () (setq mode-name "λ≫ "))))
  :config
  ;; Enable prettify-symbols-mode symbols-alists in buffers.  Only load after
  ;; package load.
  (add-hook 'haskell-mode-hook 'bcm/haskell-prettify-enable)
  ;; Add ghcup directory location of GHC binaries to PATH and exec-path.
  (setenv "PATH" (concat (getenv "PATH") ":~/.ghcup/bin"))
  (add-to-list 'exec-path "~/.ghcup/bin"))

(use-package lsp-haskell
  :ensure t
  :defer t
  :hook
  ((haskell-mode . lsp)
   (haskell-literate-mode . lsp)))

(use-package proof-general
  :ensure t
  :defer t
  :custom (proof-splash-enable nil))

(use-package auctex
  :ensure t
  :defer t
  :custom
  ;; Enable document parsing.
  (TeX-auto-save t)
  (TeX-parse-self t)
  :hook
  ;; Note: On OSX, install BasicTeX package, then add its location to $PATH.
  ((LaTeX-mode . turn-on-auto-fill)
   (LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . which-key-mode))
  :init
  ;; Enable this when working with multi-file document structures.
  ;; (setq-default TeX-master nil)
  ;; Full section options.  See Sectioning page in AUCTeX info.
  (setq LaTeX-section-hook
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label)))

(use-package web-mode
  :ensure t
  :defer t
  :mode
  ("\\.html?\\'" "\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[gj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.php\\'")
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  :hook (web-mode . flyspell-mode))

(use-package rainbow-mode
  :ensure t
  :hook
  ((css-mode . (lambda () (rainbow-mode 1)))
   (html-mode . (lambda () (rainbow-mode 1)))
   (web-mode . (lambda () (rainbow-mode 1)))))

;; Note: C-c C-f reformats, C-c C-p displays path to object at point.
(use-package json-mode
  :ensure t
  :defer t
  :mode "\\.json\\'")

(use-package pyvenv
  :ensure t
  :config (pyvenv-mode 1))

(use-package anaconda-mode
  :ensure t
  :defer t
  :diminish " α"
  :after (python pyvenv)
  :hook ((python-mode . anaconda-mode)
         (python-mode . (lambda () (setq mode-name "Py")))
         (python-mode . company-mode))
  :config (require 'pyvenv))

(use-package company-anaconda
  :ensure t
  :defer t
  :after (company python-mode))

(use-package lsp-pyright
  :ensure t
  :defer t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

;; Note: Install textproc/markdown to integrate compilation commands.
(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.markdown\\'" "\\.md\\'"))

(use-package gnuplot-mode
  :ensure t
  :defer t
  :mode "\\.gp$"
  :hook (gnuplot-mode . (lambda ()
                          (flyspell-prog-mode)
                          (add-hook 'before-save-hook
                                    'whitespace-cleanup nil t))))

;; Tabs: create: C-c C-t, close: C-c C-w, nav: C-c C-[np], list: C-c C-s
(use-package w3m
  :ensure t
  :defer t
  :custom
  (w3m-use-cookies t)
  (w3m-use-favicon nil)
  ;; Use w3m for all URLs (default is browse-url-default-browser).
  (browse-url-browser-function 'w3m-browse-url)
  ;; Use new w3m tab when doing a browse-url operation.  Disable for GUI
  ;; browsers, since it should open a new tab automatically.
  (browse-url-new-window-flag t)
  ;; Lite version of DDG is built-in.
  (w3m-search-default-engine "duckduckgo")
  ;; Don't prompt when going from HTTPS -> HTTP.
  (w3m-confirm-leaving-secure-page nil)
  ;; Activate Conkeror-style link selection (toggle with `f' key).
  :hook (w3m-mode . w3m-lnum-mode)
  :init
  (require 'w3m-load nil t)
  ;; To use w3m-search, hit `s' for same buffer or `S' for new buffer.  Prefix
  ;; with C-u to specify engine.
  (require 'w3m-search)
  ;; Add some extra search engine URIs.
  (nconc
   w3m-search-engine-alist
   '(("hoogle" "http://haskell.org/hoogle/?q=%s")
     ("wiby" "https://wiby.me/?q=%s" nil)
     ("wikipedia" "http://en.m.wikipedia.org/wiki/Special:Search?search=%s" nil)
     ("clojuredocs" "https://clojuredocs.org/search?q=%s")))
  ;; Default to the last manually specified search engine when calling the prefix
  ;; version of the function.
  (defadvice w3m-search (after change-default activate)
    (let ((engine (nth 1 minibuffer-history)))
      (when (assoc engine w3m-search-engine-alist)
        (setq w3m-search-default-engine engine))))
  :bind ("C-x M-m" . browse-url-at-point))

(use-package gptel
  :ensure t
  :hook (gptel-mode . visual-line-mode)
  :custom
  (gptel-api-key (bcm/strip-trailing-crlf
                  (bcm/read-file-contents "~/.emacs.d/openai.key")))
  (gptel-default-mode 'org-mode)
  :config
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  ;; Register local ollama backend.
  (gptel-make-ollama "ollama"
    :host "localhost:11434"
    :stream t
    :models '("llama3" "dolphin-llama3:8b")))

(use-package ob-dall-e-shell
  :ensure t
  :custom
  ((dall-e-shell-openai-key
    (bcm/strip-trailing-crlf
     (bcm/read-file-contents "~/.emacs.d/openai.key")))
   (dall-e-shell-image-output-directory "~/img")
   (dall-e-shell-model-version "dall-e-3"))
  :config (ob-dall-e-shell-setup)) ; Activate `dall-e-shell' as src block type.

;; Needed to support code block syntax highlighting in org-export to HTML.
(use-package htmlize
  :ensure t
  :defer t
  :custom (htmlize-output-type 'inline-css))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Built-in Modes

;;; color-theme
;; Using wombat theme, but with a black background instead of default #242424.
(load-theme 'wombat t nil)
(set-face-attribute 'default nil :background "#000000")

;;; icomplete
;; Disable icomplete, since I prefer using ivy for this and don't want both
;; enabled.
(icomplete-mode 0)

;;; desktop
;; Save buffers, file names, major-modes, etc, and restore on restart.  Will
;; prompt on first save.  Probably want to put it in `~/.emacs.d/'.
(desktop-save-mode 1)
(setq desktop-buffers-not-to-save
      (concat "\\("
              "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
	      "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
(add-to-list 'desktop-modes-not-to-save 'erc-mode)
(add-to-list 'desktop-modes-not-to-save 'image-mode)

;;; elisp-mode
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
;;; IELM
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;; prettify-symbols-mode
;; Build a symbols-alist for Haskell (which is all I'm using this for
;; currently).  I might split these off into a different file if I create more
;; for other languages.
(defvar haskell-prettify-symbols-alist
  '(;; Double-struck letters
    ("|A|" . ?𝔸) ("|B|" . ?𝔹) ("|C|" . ?ℂ) ("|D|" . ?𝔻) ("|E|" . ?𝔼)
    ("|F|" . ?𝔽) ("|G|" . ?𝔾) ("|H|" . ?ℍ) ("|I|" . ?𝕀) ("|J|" . ?𝕁)
    ("|K|" . ?𝕂) ("|L|" . ?𝕃) ("|M|" . ?𝕄) ("|N|" . ?ℕ) ("|O|" . ?𝕆)
    ("|P|" . ?ℙ) ("|Q|" . ?ℚ) ("|R|" . ?ℝ) ("|S|" . ?𝕊) ("|T|" . ?𝕋)
    ("|U|" . ?𝕌) ("|V|" . ?𝕍) ("|W|" . ?𝕎) ("|X|" . ?𝕏) ("|Y|" . ?𝕐)
    ("|Z|" . ?ℤ) ("|gamma|" . ?ℽ) ("|Gamma|" . ?ℾ) ("|pi|" . ?ℼ) ("|Pi|" . ?ℿ)
    ;; Types
    ("::" . ?∷)
    ;; Quantifiers
    ("forall" . ?∀) ("exists" . ?∃)
    ;; Arrows
    ("->" . ?→) ("-->" . ?⟶) ("<-" . ?←) ("<--" . ?⟵) ("<->" . ?↔)
    ("<-->" . ?⟷)
    ;; Double arrows
    ("=>" . ?⇒) ("==>" . ?⟹) ("<==" . ?⟸) ("<=>" . ?⇔) ("<==>" . ?⟺)
    ;; Bar arrows
    ("|->" . ?↦) ("|-->" . ?⟼) ("<-|" . ?↤) ("<--|" . ?⟻)
    ;; Double bar arrows
    ("|=>" . ?⤇) ("|==>" . ?⟾) ("<=|" . ?⤆) ("<==|" . ?⟽)
    ;; Squiggle arrows
    ("~>" . ?⇝) ("<~" . ?⇜)
    ;; Tail arrows
    (">->" . ?↣) ("<-<" . ?↢) ("->>" . ?↠) ("<<-" . ?↞)
    ;; Two-headed tail arrows
    (">->>" . ?⤖) ("<<-<" . ?⬻)
    ;; Open-headed arrows
    ("<|-" . ?⇽) ("-|>" . ?⇾) ("<|-|>" . ?⇿)
    ;; Arrows with stroke
    ("<-/-" . ?↚) ("-/->" . ?↛)
    ;; Arrows with vertical stroke
    ("<-|-" . ?⇷) ("-|->" . ?⇸) ("<-|->" . ?⇹)
    ;; Arrows with double vertical stroke
    ("<-||-" . ?⇺) ("-||->" . ?⇻) ("<-||->" . ?⇼)
    ;; Circle arrows
    ("-o->" . ?⇴) ("<-o-" . ?⬰)
    ;; Boolean operators
    ("not" . ?¬) ("&&" . ?∧) ("||" . ?∨)
    ;; Relational operators
    ("==" . ?≡) ("/=" . ?≠) ("<=" . ?≤) (">=" . ?≥) ("/<" . ?≮) ("/>" . ?≯)
    ;; Containers / Collections
    ("++" . ?⧺) ("+++" . ?⧻) ("|||" . ?⫴) ("empty" . ?∅) ("elem" . ?∈)
    ("notElem" . ?∉) ("member" . ?∈) ("notMember" . ?∉) ("union" . ?∪)
    ("intersection" . ?∩) ("isSubsetOf" . ?⊆) ("isProperSubsetOf" . ?⊂)
    ;; Other
    ("<<" . ?≪) (">>" . ?≫) ("<<<" . ?⋘) (">>>" . ?⋙) ("<|" . ?⊲) ("|>" . ?⊳)
    ("><" . ?⋈) ("mempty" . ?∅) ("mappend" . ?⊕) ("<*>" . ?⊛) ("undefined" . ?⊥)
    (":=" . ?≔) ("=:" . ?≕) ("=def" . ?≝) ("=?" . ?≟) ("..." . ?…)))
(defun bcm/haskell-prettify-enable ()
  "Enable prettification for Haskell symbols."
  (prettify-symbols-mode -1)
  (setq-local prettify-symbols-alist (append prettify-symbols-alist
                                             haskell-prettify-symbols-alist))
  (prettify-symbols-mode))

;;; c-mode
;; Resize the compilation window so that it doesn't take up half the frame.
(setq compilation-window-height 16)
;; Always scroll the compilation window.
(setq compilation-scroll-output t)
;; If there were no errors, there's not much to look at in a compilation
;; buffer, so make it go away in 2 seconds.
(setq compilation-finish-functions
      (lambda (buf str)
        (if (or (string-match "exited abnormally" str)
                (string-match (buffer-name buf) "*grep*"))
            ;; There were errors.
            (message "Compilation errors, press C-x ` to visit.")
          ;; No errors; make the compilation window go away in 2 seconds.
          (run-at-time 2 nil 'delete-windows-on buf)
          (message "Build Succeeded."))))
;; Use c-mode for flex files (cc-mode is probably better for this though).
(add-to-list 'auto-mode-alist '("\\.l$" . c-mode))
;; Change default indent style from "gnu".  I actually use 1TBS, but BSD style
;; auto-indents properly.
(setq c-default-style "bsd"
      c-basic-offset 4)
;; Spell-check comments.
(add-hook 'c-mode-hook 'flyspell-prog-mode)

;;; java-mode
;; This mode doesn't properly indent Java out of the box.  This combined with
;; the C settings above fixes that.
(add-hook 'java-mode-hook
          (lambda ()
            "Treat Java 1.5 @-style annotations as comments."
            (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

;;; sql-mode
;; This adds a connection for my local (and only locally-accessible) l1j-en
;; test database on MySQL, with the ability to add others later by appending to
;; sql-connection-alist.
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

;;; prolog-mode
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(setq prolog-system 'swi)
(setq prolog-program-switches
      '((sicstus ("-i")) (swi ("-L0" "-G0" "-T0" "-A0")) (t nil)))
;; By default, .pl is linked to perl-mode.
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))
;; Add auto-mode for Mercury source, which is close enough to Prolog to benefit
;; from syntax highlighting.  This overrides the default ObjC auto-mode for .m.
(setq auto-mode-alist (cons '("\\.m$" . prolog-mode) auto-mode-alist))

;;; cperl-mode
;; Always use cperl-mode instead of perl-mode.
(defalias 'perl-mode 'cperl-mode)
;; (add-to-list 'auto-mode-alist '("\\.pl$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.cgi$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pm$" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

;;; nxml-mode
;; Using nXhtml for .xhtml files instead of XHTML (an sgml-mode mode).
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
            auto-mode-alist))

;;; shell-mode
;; Use ANSI colors within shell-mode.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; flyspell
;; Turn on flyspell mode for text editing.
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
;; aspell > ispell
(setq
 ;; Suggestion mode tuned to fastest possible.
 ispell-program-name "aspell"
 ispell-extra-args '("--sug-mode=ultra")
 ;; Solves aspell startup problem on some GNU/Linux distros.
 flyspell-issue-welcome-flag nil)

;;; org-agenda
;; Note: Needs to eval before Org config, for `org-agenda-files'.
;; Display main agenda dispatch.
(global-set-key (kbd "C-c a") 'org-agenda)
;; Set the files I want org-agenda to pull from.
(when (file-directory-p "~/src/docs")
  (setq org-agenda-files
        (seq-filter 'file-exists-p
                    (append (directory-files "~/src/docs" t "^archive-.*\\.org$")
                            '("~/src/docs/agenda.org"
                              "~/src/docs/recur.org")))))
(setq
 ;; Increase space for habit description (default 40).
 org-habit-graph-column 50
 ;; Custom agenda views.
 org-agenda-custom-commands'(("A" "Agenda and STRT tasks"
                              ((agenda "")
                               (todo "STRT"))))
 ;; Custom time grid styling.
 org-agenda-current-time-string
 "◀── now ────────────────────────────────────────────"
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))

;;; Org
;; Initiate org-mode when opening .org files.
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; Stores links.  In an org-mode file, C-c C-l calls them and creates links.
(global-set-key (kbd "C-x M-l") 'org-store-link)
(setq
 ;; Set *scratch* buffer to org-mode instead of lisp-interaction-mode.
 initial-major-mode 'org-mode
 ;; Change default TODO keywords and coloring.
 org-src-fontify-natively t
 org-todo-keywords '((sequence
                      "TODO(t)" "STRT(s!)" "INAC(i)" "BLCK(b!)" "|"
                      "DONE(d!)" "CNCL(c!)"))
 org-todo-keyword-faces'(("INAC" :foreground "gray25" :weight bold)
                         ("TODO" :foreground "red" :weight bold)
                         ("STRT" :foreground "light sky blue" :weight bold)
                         ("BLCK" :foreground "purple" :weight bold)
                         ("DONE" :foreground "forest green" :weight bold)
                         ("CNCL" :foreground "dark blue" :weight bold))
 ;; Configure org-refile to target other files.
 org-refile-targets '((org-agenda-files . (:maxlevel . 1)))
 org-refile-use-outline-path 'file
 ;; Show all empty lines between collapsed trees.
 org-cycle-separator-lines 1
 ;; Preserve indentation when exiting the `org-edit-src-code' editor.
 org-src-preserve-indentation t
 ;; Customize optional modules.
 org-modules (append org-modules '(org-habit))
 ;; Increase scale of inline rendered LaTeX.
 org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
(add-hook 'org-mode-hook 'turn-on-auto-fill)
;; Change colors for level 2, and 3.  Defaults are yellow, and light sky blue.
(custom-theme-set-faces 'user '(org-level-2 ((t (:foreground "light sky blue")))))
(custom-theme-set-faces 'user '(org-level-3 ((t (:foreground "deep sky blue")))))
;; Match the colors of statistics cookies.
(custom-theme-set-faces 'user '(org-done ((t (:foreground "forest green")))))
(custom-theme-set-faces 'user '(org-todo ((t (:foreground "red")))))
;; Activate org-tempo for block completion using <s TAB (and others).
(require 'org-tempo)
;; Add some custom structure templates.
(nconc org-structure-template-alist
       '(("el"  . "src emacs-lisp")
         ("clj" . "src clojure :backend babashka")
         ("py"  . "src python")
         ("hs"  . "src haskell")
         ("sh"  . "src shell")
         ("d"   . "src dall-e-shell :version dall-e-3")))
;; Also add a better binding for template insertion.
(org-defkey org-mode-map (kbd "C-c M-t") 'org-insert-structure-template)
;; Use org-return-and-maybe-indent instead of org-return to prevent
;; auto-indenting lists and other select structured content.
(org-defkey org-mode-map (kbd "RET") 'org-return-and-maybe-indent)
;; Activate org modules.
(eval-after-load 'org
  '(org-load-modules-maybe t))
;; Function to copy a link from an Org link.  Useful if you want to use the
;; link target somewhere else and not just open it.
(defun bcm/org-copy-url ()
  "Copy URL from an org-mode link under the point."
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (let* ((link (org-link-unescape (match-string-no-properties 1)))
             (path (when (string-match "\\(.*\\)\\(::.*\\)" link)
                     (substring link 0 (match-beginning 2)))))
        (kill-new (or path link)))))
;; Quick fix for swiper + Org unfold bug.
;; https://github.com/abo-abo/swiper/issues/3015
;; TODO: Remove this later.
(defun bcm/org-fix-unfold ()
  "Reset isearch overlays for active buffer."
  (interactive)
  (org-fold-core--clear-isearch-overlays))
;; Clobbers `org-forward-heading-same-level'.
(org-defkey org-mode-map (kbd "C-c C-f") 'bcm/org-fix-unfold)

;;; org-publish
;; Location of personal site header.
(setq blog-header-file "~/public_html/inc/header.html")
;; Load personal site header.
(defun bcm/load-blog-header ()
  "Insert blog header into buffer."
  (with-temp-buffer
    (insert-file-contents blog-header-file)
    (buffer-string)))
;; Define projects that feed content into main personal site.
(setq org-publish-project-alist
      '(("blog"
         :base-directory "~/public_html"
         :recursive t
         :publishing-directory "~/public_html"
         :publishing-function org-html-publish-to-html
         :with-author nil
         :with-broken-links t
         :with-creator nil
         :html-validation-link nil
         :html-postamble nil
         :html-preamble bcm/load-blog-header)
        ("main"
         :base-directory "~/src/macroexpand-main"
         :recursive nil
         :publishing-directory "~/src/macroexpand-main"
         :publishing-function org-html-publish-to-html
         :with-author nil
         :with-broken-links t
         :with-creator nil
         :html-validation-link nil
         :html-postamble nil
         :html-preamble nil
         :exclude ".*"
         :include ["index.org"])))

;;; org-babel
;; Enable specific languages.
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)
                               (emacs-lisp . t)
                               (clojure . t)
                               (shell . t)
                               (haskell . t)
                               (ditaa . t)
                               (restclient . t))))
;; Turn off prompt for confirmation before evaluation.
(setq org-confirm-babel-evaluate nil)
;; Path to ditaa JAR.
(setq org-ditaa-jar-path "~/bin/ditaa0_6b.jar")
;; Default ob-clojure backend to CIDER.  Override with `:backend babashka'.
(setq org-babel-clojure-backend 'cider)

;;; org-capture: On-the-fly note taking.
(setq org-default-notes-file "~/src/docs/capture.org")
;; Global keybinding for org-capture.
(global-set-key (kbd "C-c r") 'org-capture)
;; Custom templates.
(setq org-capture-templates
      '(("t" "Task" entry (file org-default-notes-file)
         "* TODO\n%U\n")
        ("h" "Habit" entry (file org-default-notes-file)
         (concat "* TODO %?\nSCHEDULED: <%<%Y-%m-%d %a .+1d>>\n"
                 ":PROPERTIES:\n:CREATED: %U\n:STYLE: habit\n:END:\n%U\n"))))

;;; add-log
;; Auto-add new entry to CHANGELOG found up parent dir hierarchy with C-x 4 a.
(setq user-mail-address "bm3719@gmail.com")  ; Default: user@host
(setq change-log-default-name "CHANGELOG")   ; Default: ChangeLog

;;; savehist-mode
;; Mode requires customization set prior to enabling.
(setq savehist-additional-variables
      '(search-ring regexp-search-ring)    ; Save search entries.
      savehist-file "~/.emacs.d/savehist") ; Keep this out of ~.
(savehist-mode t)                          ; Turn savehist-mode on.

;;; calendar
;; Change some self-explanatory calendar settings.
(setq mark-holidays-in-calendar t
      calendar-holidays (append holiday-general-holidays
                                holiday-local-holidays
                                holiday-solar-holidays)
      display-time-24hr-format t)
;; Remove Flag Day, since I don't want it appearing on org-agenda.
(setq calendar-holidays
      (remove (reverse (assoc "Flag Day"
                              (mapcar 'reverse calendar-holidays)))
              calendar-holidays))
;; Start weeks on Monday instead of Sunday.
(setq calendar-week-start-day 1)

;;; dired
;; List directories first and use ISO8601 dates.
(setq dired-listing-switches
      ;; Do simpler listing if on BSD.
      (if (eq system-type 'berkeley-unix)
          "-lFh" "-AhFlv --group-directories-first --time-style=+%Y-%m-%d"))
;; Make up-navigation "reuse" the old buffer (actually kill the old one).
(eval-after-load 'dired
  '(defun dired-up-directory ()
     "Run Dired on parent directory of current directory."
     (interactive)
     (let* ((dir (dired-current-directory))
            (orig (current-buffer))
            (up (file-name-directory (directory-file-name dir))))
       (or (dired-goto-file (directory-file-name dir))
           ;; Only try dired-goto-subdir if buffer has more than one dir.
           (and (cdr dired-subdir-alist)
                (dired-goto-subdir up))
           (progn
             (kill-buffer orig)
             (dired up)
             (dired-goto-file dir))))))
;; More convenient than `^'.
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "b") 'dired-up-directory)))
;; Always delete and copy recursively.
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;;; diary
(setq diary-file "~/.emacs.d/.diary")    ; Might as well keep this out of ~.
(setq mark-diary-entries-in-calendar t)  ; Add entries to calendar.

;;; tetris
(setq tetris-score-file "~/.emacs.d/tetris-scores") ; Moved from ~.

;;; rmail
(setq mail-archive-file-name "~/Mail/sent")  ; Reuse the gnus mail dir.

;;; woman
;; Alias man to woman, since the latter offers completion.
(defalias 'man 'woman)

;;; Emacs bookmarks
;; C-x r m: create new bookmark
;; C-x r b: navigate to bookmark
;; C-x r l: list bookmarks.
(setq
 bookmark-default-file "~/.emacs.d/bookmarks" ; Moved from ~.
 bookmark-save-flag 1)                        ; Autosave each change.

;;; so-long
;; Disables selected minor modes only in minified and similar files with
;; excessively long lines, which tend to result in poor performance.
(global-so-long-mode 1)
;; Max line length before so-long activates.
(setq so-long-threshold 5000)
;; Add additional minor modes to exclude.
(nconc so-long-minor-modes
       '(eldoc-mode
         rainbow-mode
         lsp-mode
         company-mode
         rainbow-mode
         aggressive-indent-mode))

;; Extra miscellaneous mode associations.
(nconc auto-mode-alist
       '(("\\.plan$" . text-mode)
         ("\\.project$" . text-mode)
         ("\\.doc$" . text-mode)
         ("\\.zsh$" . sh-mode)
         ("\\CHANGELOG$" . text-mode)
         ("\\INSTALL$" . text-mode)
         ("\\README$" . text-mode)
         ("\\TODO$" . text-mode)))

;; Custom generic mode for ARFF files (Used with Weka).
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

;;; server-mode
;; This starts up a server automatically, allowing emacsclient to connect to a
;; single Emacs instance.  If a server already exists, it is killed.
(server-force-delete)
(server-start)

;;; comint-mode
;; Various comint settings.
(setq comint-scroll-to-bottom-on-input t
      comint-scroll-to-bottom-on-output nil
      comint-scroll-show-maximum-output t
      ;; Match most shells' insert of space/slash after file completion.
      comint-completion-addsuffix t
      comint-buffer-maximum-size 100000
      comint-input-ring-size 5000)

;;; TRAMP
(when (eq system-type 'windows-nt)
  (setq shell-file-name "bash")
  (setq explicit-shell-file-name shell-file-name))
(setq tramp-default-method "ssh")

;;; tab-bar-mode
;; Allow C-z to be a prefix key.
(define-key global-map (kbd "C-z") (make-sparse-keymap))
;; Default new tabs to *scratch* buffer.
(setq tab-bar-new-tab-choice "*scratch*")
;; Always add new tabs on the far right.
(setq tab-bar-new-tab-to 'rightmost)
;; Tweak tab faces to match powerline theme.
(set-face-attribute 'tab-bar-tab-inactive nil :background "#1C1C1C"
                    :foreground "#87875F" :family "Monospace" :weight 'bold)
(set-face-attribute 'tab-bar nil :background "#3A3A3A" :foreground "#B2B2B2"
                    :family "Monospace" :weight 'bold)
(set-face-attribute 'tab-bar-tab nil :background "#626262" :foreground "#EEEEEE"
                    :family "Monospace" :weight 'bold)
;; Add keybindings to match elscreen defaults.
(global-set-key (kbd "<f7>") 'tab-new)
(global-set-key (kbd "<f8>") 'tab-close)
(global-set-key (kbd "C-z n") 'tab-next)
(global-set-key (kbd "C-z p") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-z r") 'tab-rename)

;;; ERC
(require 'erc)
;; Can move the remainder of this to ~/.emacs.d/.ercrc.el later if including a
;; user/pass.
(defun bcm/erc ()
  "Start ERC and connect to Libera with pre-set credentials."
  (interactive)
  (erc :server "irc.libera.chat" :port 6667))
(setq erc-interpret-mirc-color t
      erc-kill-buffer-on-part t
      erc-kill-server-buffer-on-quit t)
(setq erc-autojoin-channels-alist
      '(("libera" "#haskell")))
(setq erc-channel-hide-list
      '(("#clojure" "JOIN" "PART" "QUIT" "NICK")
        ("#haskell" "JOIN" "PART" "QUIT" "NICK")))
(mapc (lambda (var) (set var "nft_slut"))
      '(erc-nick erc-user-full-name erc-email-userid))

;;; Mutt client integration.
;; This associates file whose name contains "/mutt" to be in mail-mode.
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)
;; Define otherwise free variable.
(eval-when-compile (defvar mail-mode-map))
;; Use C-c C-c to complete mutt message buffers without prompting for saving.
(add-hook
 'mail-mode-hook
 (lambda ()
   (define-key mail-mode-map (kbd "C-c C-c")
               (lambda ()
                 (interactive)
                 (save-buffer)
                 (server-edit)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End package load

;; Replace echo area startup message.
(run-with-timer 1 nil (lambda () (message "I have SEEN the CONSING!!")))
