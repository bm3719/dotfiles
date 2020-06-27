;;;; -*- mode: Emacs-Lisp; eldoc-mode:t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Bruce C. Miller - bm3719@gmail.com
;;;; Time-stamp: <2020-06-26 22:18:24 (bm3719)>
;;;;
;;;; This init was created for GNU Emacs 26.3 for GNU/Linux, OpenBSD, and
;;;; Windows, but all or parts of this file should work with older GNU Emacs
;;;; versions, or on other OSes.
;;;;
;;;; ELPA addons: volatile-highlights, smartparens, which-key, clojure-mode,
;;;; cider, rainbow-delimiters, ac-cider, flycheck-clj-kondo, intero,
;;;; proof-general, auctex, web-mode, restclient, rainbow-mode, js2-mode,
;;;; json-mode, python-mode, gnuplot, markdown-mode, aggressive-indent,
;;;; eshell-git-prompt, elscreen, w3m, lusty-explorer, emms, magit, git-gutter,
;;;; org-bullets, org-present, wttrin, htmlize, pinentry, powerline, diminish.
;;;;
;;;; External applications used: aspell, aspell-en, Leiningen, clj-kondo,
;;;; stack, mutt, w3m, Fira Code font.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initial Startup

;; Getting rid of the toolbar first prevents it from showing in the few seconds
;; needed for the rest of this stuff to load, though disabling it in .Xdefaults
;; is even better.
(when window-system
  (tool-bar-mode -1))

;; Font face: Always default to Fira Code or Roboto Mono, if available.
;; Otherwise use Consolas on Windows and go through a priority list of
;; preferred fonts for other OSes.
(when window-system
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
(when window-system
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

;; Shut off message buffer.  To debug Emacs, comment these out so you can see
;; what's going on.
(setq message-log-max nil)
;; Check if message buffer exists before killing (not doing so errors
;; eval-buffer of an init file).
(when (not (eq nil (get-buffer "*Messages*")))
  (kill-buffer "*Messages*"))

;; Provide a useful error trace if loading this init fails.
(setq debug-on-error t)

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

;; Load Common Lisp features.
(require 'cl-lib)

;; Use fullscreen in GUI mode.
(when window-system
  (toggle-frame-maximized))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic Key Bindings

;; Define a function to auto-delete trailing whitespace upon save.
(defun bcm/delete-ws-save ()
  (interactive)
  (progn (delete-trailing-whitespace)
         (save-buffer)))

;; Provides zap-up-to-char (M-z), different than the default zap-to-char which
;; includes deleting the argument character.
(load-library "misc")

;; Global key (re-)mappings.
(global-set-key (kbd "C-w") 'backward-kill-word)   ; Match the shell's C-w.
(global-set-key (kbd "C-x w") 'kill-region)
(global-set-key (kbd "C-x s") 'bcm/delete-ws-save)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-G") 'goto-char)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer) ; Bypasses the C-x k prompt.
(global-set-key (kbd "C-x C-v") 'revert-buffer)
(global-set-key (kbd "C-x TAB") 'indent-region)
(global-set-key (kbd "C-c M-e") 'fixup-whitespace)
(global-set-key (kbd "C-c g") 'replace-string)
(global-set-key (kbd "C-c ;") 'comment-region)
(global-set-key (kbd "C-c '") 'uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)        ; Instead of dabbrev-expand.
(global-set-key (kbd "M-z") 'zap-up-to-char)       ; Mimic Vim delete to char.
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x M-a") 'align-regexp)
(global-set-key (kbd "<f9>") 'insert-char)
;; Move set-fill-column from C-x f to C-x M-f, as it's easy to hit this when
;; intending to do a find-file.
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-x M-f") 'set-fill-column)
(global-set-key (kbd "C-x C-s") 'bcm/delete-ws-save)

;; For quick macro running.
(global-set-key (kbd "<f10>") 'start-kbd-macro)
(global-set-key (kbd "<f11>") 'end-kbd-macro)
(global-set-key (kbd "<f12>") 'call-last-kbd-macro)

;; Cycle through buffers.
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; M-x compile and M-x grep mnemonics.
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "C-c n") 'next-error)
(global-set-key (kbd "C-c p") 'previous-error)

;; My KVM switch uses scroll lock, and Emacs complains about it.
(global-set-key (kbd "<Scroll_Lock>") 'ignore)
;; Silence *-mouse-9 complaints.
(global-set-key (kbd "<mouse-9>") 'ignore)
(global-set-key (kbd "<double-mouse-9>") 'ignore)
(global-set-key (kbd "<drag-mouse-9>") 'ignore)

;; Disable suspend-frame on Xorg sessions.
(when window-system
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General Text Editing

;; Set fill width to 79 (default was 70).
(setq-default fill-column 79)

;; Takes a multi-line paragraph and makes it into a single line of text.
(defun bcm/unfill-paragraph ()
  "Un-fill paragraph at point."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(global-set-key (kbd "M-p") 'bcm/unfill-paragraph)

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
(add-hook 'post-command-hook #'bcm/set-cursor-color-according-to-mode)

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
(defun bcm/backward-kill-line (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (kill-line 0))
(global-set-key (kbd "M-C-k") 'bcm/backward-kill-line)

;; Replace error message on read-only kill with an echo area message.
(setq-default kill-read-only-ok t)

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

;; I type a lot of λs.
(global-set-key (kbd "C-M-l") (lambda ()
                                (interactive)
                                (insert-char ?λ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer Navigation

;; Shift-arrow keys to move between windows.
(windmove-default-keybindings)

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

;;; Mouse wheel scrolling
;; Scroll in 1-line increments for the buffer under pointer.
(setq mouse-wheel-follow-mouse t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

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

;; Change C-x C-b behavior so it uses bs; shows only interesting buffers.  The
;; `a' key will toggle visibility of all.
(global-set-key (kbd "C-x C-b") 'bs-show)

;; The first invocation of Home/End moves to the beginning of the *text* line.
;; A second invocation moves the cursor to beginning of the *absolute* line.
;; Most of the time this won't matter even be noticeable, but when it does (in
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous Customization

;; This sets garbage collection to maximum, speeding up startup time.  Disable
;; this if RAM is limited, or set to a fixed amount.
(setq gc-cons-threshold most-positive-fixnum)

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

;; Variables to mark as safe.
(setq safe-local-variable-values '((outline-minor-mode . t)
                                   (eldoc-mode . t)))

;; Set shells.
(when (eq system-type 'berkeley-unix)
  (setq shell-file-name "/usr/local/bin/zsh")
  (setq tex-shell-file-name "/usr/local/bin/zsh"))
(when (eq system-type 'gnu/linux)
  (setq shell-file-name "/bin/zsh")
  (setq tex-shell-file-name "/bin/zsh"))
(when (eq system-type 'windows-nt)
  (setq shell-file-name "/usr/bin/bash")
  (setq tex-shell-file-name "/usr/bin/bash"))

;; Answer `y' or RET for yes and `n' for no at minibar prompts.
(defalias 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map (kbd "RET") 'act)

;; Always use the echo area instead of dialog boxes in console mode.
(when (not window-system)
  (setq use-dialog-box nil))

;; Don't echo passwords when communicating with interactive programs.
(add-hook 'comint-output-filter-functions #'comint-watch-for-password-prompt)

;; Gets rid of disabled commands prompting.
(setq disabled-command-function nil)

;; Allow seamless editing of files in a tar/jar/zip file.
(auto-compression-mode 1)

;; Completion ignores case.
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; Completion ignores filenames ending in any string in this list.
(setq completion-ignored-extensions
      '(".o" ".elc" ".class" "java~" ".ps" ".abs" ".mx" ".~jv" ".bak" ))

;; Startup message with Emacs version.  Modified from original at:
;; http://www.emacswiki.org/emacs/DotEmacsChallenge
(defun bcm/emacs-reloaded ()
  "Display animated startup message."
  (animate-string
   (concat ";; Initialization successful.  Welcome to "
           (substring (emacs-version) 0 14) ".") 0 0)
  (newline-and-indent)  (newline-and-indent))
(add-hook 'after-init-hook #'bcm/emacs-reloaded)

;; Call this function to increase/decrease font size.
(defun bcm/zoom (n)
  "With positive N, increase the font size, otherwise decrease it."
  (set-face-attribute 'default (selected-frame) :height
                      (+ (face-attribute 'default :height)
                         (* (if (> n 0) 1 -1) 10))))
;; Add some zoom keybindings.
(global-set-key (kbd "C-+") '(lambda () (interactive) (bcm/zoom 1)))
(global-set-key (kbd "C-<kp-add>") '(lambda () (interactive) (bcm/zoom 1)))
(global-set-key (kbd "C--") '(lambda () (interactive) (bcm/zoom -1)))
(global-set-key (kbd "C-<kb-subtract>") '(lambda () (interactive) (bcm/zoom -1)))

;;; Time-stamp support
;; When there is a "Time-stamp: <>" in the first 10 lines of the file,
;; Emacs will write time-stamp information there when saving.
(setq time-stamp-active t          ; Do enable time-stamps.
      time-stamp-line-limit 10     ; Check first 10 buffer lines for stamp.
      time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; Date format.
(add-hook 'write-file-hooks #'time-stamp) ; Update when saving.

;; Follow the compilation buffer scroll instead of remaining at the top line.
(setq compilation-scroll-output t)

;; If I've edited init.el, byte compile it.  Saves some startup time.
(defun bcm/autocompile-init ()
  "Compile init.el in ~/.emacs.d/"
  (interactive)
  (require 'bytecomp)
  (if (and (string= (buffer-file-name)
                    (expand-file-name
                     (concat default-directory "init.el")))
           ;; Exclude my repo version of the same file.
           (not (string-match-p "dotfiles" (buffer-file-name))))
      (byte-compile-file (buffer-file-name))))
(add-hook 'after-save-hook #'bcm/autocompile-init)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Built-in Modes

;;; color-theme
;; Using wombat theme, but with a black background instead of default #242424.
(load-theme 'wombat t nil)
(set-face-attribute 'default nil :background "#000000")

;;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook #'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook #'turn-on-eldoc-mode)
;;; IELM
(add-hook 'ielm-mode-hook #'turn-on-eldoc-mode)

;;; prettify-symbols-mode
;; Build a symbols-alist for Haskell (which is all I'm using this for
;; currently). I might split these off into a different file if I create more
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
(setq auto-mode-alist
      (append '(("\\.l$" . c-mode))
              auto-mode-alist))
;; Change default indent style from "gnu".  I actually use 1TBS, but BSD style
;; auto-indents properly.
(setq c-default-style "bsd"
      c-basic-offset 4)
;; Spell-check comments.
(add-hook 'c-mode-hook #'flyspell-prog-mode)

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

;;; conf-mode
;; Ignore single quote highlighting in .properties files.
(add-hook 'conf-javaprop-mode-hook
          '(lambda () (conf-quote-normal nil)))

;;; shell-mode
;; Use ANSI colors within shell-mode.
(add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)

;;; flyspell
;; Turn on flyspell mode for text editing.
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
;; aspell > ispell
;; Suggestion mode tuned to fastest possible.
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))
;; Solves aspell startup problem on some GNU/Linux distros.
(setq flyspell-issue-welcome-flag nil)

;;; Org
;; Initiate org-mode when opening .org files.
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; Stores links.  In an org-mode file, C-c C-l calls them and creates links.
(global-set-key (kbd "C-x M-l") 'org-store-link)
;; Change default TODO keywords and coloring.
(setq
 org-src-fontify-natively t
 org-todo-keywords (quote ((sequence
                            "INACTIVE(i)"
                            "TODO(t)"
                            "STARTED(s!)"
                            "BLOCKED(b!)"
                            "|"
                            "DONE(d!)"
                            "CANCELED(c!)")))
 org-todo-keyword-faces
 (quote (("INACTIVE" :forground "dark slate gray" :weight bold)
         ("TODO" :foreground "red" :weight bold)
         ("STARTED" :foreground "light sky blue" :weight bold)
         ("BLOCKED" :foreground "purple" :weight bold)
         ("DONE" :foreground "forest green" :weight bold)
         ("CANCELED" :foreground "dark blue" :weight bold))))
(add-hook 'org-mode-hook #'turn-on-auto-fill)
;; Change colors for level 2, and 3.  Defaults are yellow, and light sky blue.
(custom-theme-set-faces 'user `(org-level-2 ((t (:foreground "light sky blue")))))
(custom-theme-set-faces 'user `(org-level-3 ((t (:foreground "deep sky blue")))))
;; Match the colors of statistics cookies.
(custom-theme-set-faces 'user `(org-done ((t (:foreground "forest green")))))
(custom-theme-set-faces 'user `(org-todo ((t (:foreground "red")))))

;;; org-agenda
;; Display this week's scheduled items.  Clobbers hotkey for read-only-mode.
(global-set-key (kbd "C-x C-q") 'org-agenda)

;;; org-publish
;; Location of personal site header.
(setq blog-header-file "~/public_html/inc/header.html")
;; Load personal site header.
(defun bcm/load-blog-header (arg)
  (with-temp-buffer
    (insert-file-contents blog-header-file)
    (buffer-string)))
;; Define projects that feed content into main personal site.
(setq org-publish-project-alist
      `(("blog"
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
        ("docs"
         :base-directory "~/src/docs"
         :recursive nil
         :publishing-directory "~/public_html"
         :publishing-function org-html-publish-to-html
         :with-author nil
         :with-broken-links t
         :with-creator nil
         :html-validation-link nil
         :html-postamble nil
         :html-preamble bcm/load-blog-header
         :exclude ".*"
         :include ["projects.org" "archive.org"])))

;;; org-babel
;; Enable specific languages.
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)
                               (emacs-lisp . t)
                               (clojure . t)
                               (ditaa . t)
                               (restclient . t))))

;;; ob-clojure
(require 'ob-clojure)

;;; org-capture: On-the-fly note taking.
(setq org-default-notes-file "~/notes.org")
;; Global keybinding for idea capture.
(global-set-key (kbd "C-c r") 'org-capture)

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
;; Add calendar control-navigation.
(add-hook 'calendar-load-hook
          (lambda ()
            (define-key calendar-mode-map (kbd "C-x >") 'scroll-calendar-right)
            (define-key calendar-mode-map (kbd "C-x <") 'scroll-calendar-left)))
;; Change some self-explanatory calendar settings.
(setq mark-holidays-in-calendar t
      all-christian-calendar-holidays t
      all-islamic-calendar-holidays nil
      all-hebrew-calendar-holidays nil
      display-time-24hr-format t)

;;; diary
(setq diary-file "~/.emacs.d/.diary")    ; Might as well keep this out of ~.
(setq mark-diary-entries-in-calendar t)  ; Add entries to calendar.

;;; tetris
(setq tetris-score-file "~/.emacs.d/tetris-scores") ; Moved from ~.

;;; rmail
(setq mail-archive-file-name "~/Mail/sent")  ; Reuse the gnus mail dir.
(defconst user-mail-address "bm3719@gmail.com")

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

;; Extra miscellaneous mode associations.
(setq auto-mode-alist (cons '("\\.plan$" . text-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.project$" . text-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.doc$" . text-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.zsh$" . sh-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\CHANGELOG$" . text-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\INSTALL$" . text-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\README$" . text-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\TODO$" . text-mode) auto-mode-alist))

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

;;; icomplete
;; Disable icomplete, since I prefer using lusty-explorer for this and don't
;; want both enabled.
(icomplete-mode 0)

;;; Mutt client integration.
;; This associates file whose name contains "/mutt" to be in mail-mode.
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-hook 'mail-mode-hook #'turn-on-auto-fill)
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
;;; External Addons (ELPA)

;;; package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives
;;              '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((volatile-highlights . "melpa-stable")
          (smartparens . "melpa-stable")
          (which-key . "melpa-stable")
          (clojure-mode . "melpa-stable")
          (cider . "melpa-stable")
          (rainbow-delimiters . "melpa-stable")
          (ac-cider . "melpa-stable")
          (flycheck-clj-kondo . "melpa-stable")
          (intero . "melpa-stable")
          (proof-general . "melpa") ;; Switch to melpa-stable later.
          (auctex . "gnu")
          (web-mode . "melpa-stable")
          (restclient . "melpa")
          (rainbow-mode . "gnu")
          (js2-mode . "melpa-stable")
          (json-mode . "mepla-stable")
          (python-mode . "melpa-stable")
          (gnuplot . "mepla-stable")
          (markdown-mode . "melpa-stable")
          (aggressive-indent . "melpa-stable")
          (eshell-git-prompt . "melpa-stable")
          (elscreen . "melpa-stable")
          (w3m . "mepla")
          (lusty-explorer . "melpa-stable")
          (emms . "melpa-stable")
          (magit . "melpa-stable")
          (git-gutter . "melpa-stable")
          (org-bullets . "melpa-stable")
          (ob-restclient . "melpa")
          (org-present . "melpa")
          (wttrin . "melpa-stable")
          (htmlize . "melpa-stable")
          (pinentry . "gnu")
          (powerline . "melpa-stable")
          (diminish . "melpa-stable"))))
(defvar my-packages '(volatile-highlights
                      smartparens
                      which-key
                      clojure-mode
                      cider
                      rainbow-delimiters
                      ac-cider
                      flycheck-clj-kondo
                      intero
                      proof-general
                      auctex
                      web-mode
                      restclient
                      rainbow-mode
                      js2-mode
                      json-mode
                      python-mode
                      gnuplot
                      markdown-mode
                      aggressive-indent
                      eshell-git-prompt
                      elscreen
                      w3m
                      lusty-explorer
                      emms
                      magit
                      git-gutter
                      org-bullets
                      org-present
                      ob-restclient
                      wttrin
                      htmlize
                      pinentry
                      powerline
                      diminish))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; volatile-highlights
;; https://github.com/k-talo/volatile-highlights.el
(require 'volatile-highlights)
(volatile-highlights-mode t)

;;; smartparens
;; https://github.com/Fuco1/smartparens
(require 'smartparens-config)
(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-strict-mode)
(add-hook 'ielm-mode-hook #'smartparens-strict-mode)
(add-hook 'lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'scheme-mode-hook #'smartparens-strict-mode)
;; Keybindings for the features being used so far.
(define-key smartparens-mode-map (kbd "M-)") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "M-(") 'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "M-s") 'sp-unwrap-sexp)

;;; which-key
(setq which-key-popup-type 'side-window)
(setq which-key-side-window-location 'bottom)
(setq which-key-idle-delay 1.2) ;; Default 1.0.
(add-hook 'org-mode-hook #'which-key-mode)

 ;;; clojure-mode
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)

;;; CIDER
(require 'cider)
(add-hook 'cider-mode-hook #'flyspell-prog-mode)
(add-hook 'cider-mode-hook #'which-key-mode)
(add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
(setq cider-repl-pop-to-buffer-on-connect t)
(defun cider-reset ()
  "Sends (refresh) to the remote CIDER REPL buffer.  Only works
in M-x cider buffers connected to localhost."
  (interactive)
  (set-buffer "*cider-repl 127.0.0.1*")
  (goto-char (point-max))
  (insert "(refresh)")
  (cider-repl-return))
;; Have org-babel use CIDER.
(setq org-babel-clojure-backend 'cider)

;;; rainbow-delimiters
;; https://github.com/jlr/rainbow-delimiters
(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

;;; ac-cider: In-buffer completion for Clojure projects.
;; https://github.com/clojure-emacs/ac-cider
(require 'ac-cider)
(add-hook 'cider-mode-hook #'ac-flyspell-workaround)
(add-hook 'cider-mode-hook #'ac-cider-setup)
(add-hook 'cider-repl-mode-hook #'ac-cider-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook #'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook #'set-auto-complete-as-completion-at-point-function)

(defun bcm/clojure-hook ()
  (auto-complete-mode 1)
  (define-key clojure-mode-map (kbd "<S-tab>") 'auto-complete)
  ;; (define-key cider-mode-map (kbd "C-c C-o") 'cider-reset)
  (define-key clojure-mode-map (kbd "C-w") 'sp-backward-kill-word))
(add-hook 'clojure-mode-hook #'bcm/clojure-hook)
(add-hook 'cider-repl-mode-hook
          (lambda ()
            (define-key cider-repl-mode-map
              (kbd "C-w") 'sp-backward-kill-word)))
;; Fix missing *nrepl-messages* buffer.
(setq nrepl-log-messages 1)

;; flycheck-clj-kondo: Requires clj-kondo installed to $PATH.
;; https://github.com/borkdude/flycheck-clj-kondo
(require 'flycheck-clj-kondo)
;; Add to clojure-mode-hook.
(add-hook 'clojure-mode-hook #'flycheck-mode)

;;; intero: A complete developer environment for Haskell.
;; https://commercialhaskell.github.io/intero/
(add-hook 'haskell-mode-hook #'intero-mode)
;; Enable prettify-symbols-mode symbols-alists in buffers.
(add-hook 'haskell-mode-hook #'bcm/haskell-prettify-enable)
(add-hook 'intero-repl-mode-hook #'bcm/haskell-prettify-enable)
;; Variables from haskell-customize.el
(defvar haskell-process-auto-import-loaded-modules nil)
(defvar haskell-process-log nil)
(defvar haskell-process-suggest-remove-import-lines nil)
(setq
 ;; Import the modules reported by GHC to have been loaded.
 haskell-process-auto-import-loaded-modules t
 ;; Enable debug logging to *haskell-process-log* buffer.
 haskell-process-log t
 ;; Suggest removing import lines as warned by GHC.
 haskell-process-suggest-remove-import-lines t)

;;; Proof General
;; TODO: Add some stuff here, maybe.

;;; AUCTeX
;; http://www.gnu.org/software/auctex/
;; Note: On OSX, install BasicTeX package, then add its location to $PATH.
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'which-key-mode)
;; Enable this when working with multi-file document structures.
;; (setq-default TeX-master nil)
;; Enable document parsing.
(setq TeX-auto-save t)
(setq TeX-parse-self t)
;; Full section options.  See Sectioning page in AUCTeX info.
(setq LaTeX-section-hook
      '(LaTeX-section-heading
        LaTeX-section-title
        LaTeX-section-toc
        LaTeX-section-section
        LaTeX-section-label))

;;; web-mode: An autonomous major-mode for editing web templates (HTML
;;; documents embedding parts (CSS/JavaScript) and blocks (client/server side).
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
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(add-hook 'web-mode-hook #'flyspell-mode)

;;; restclient
;; https://github.com/pashky/restclient.el
;; Inhibit restclient from sending cookies implicitly.
(setq restclient-inhibit-cookies t)
;; Designate .rest as the file extension for this mode.
(add-to-list 'auto-mode-alist '("\\.rest$" . restclient-mode))

;;; rainbow-mode: Adds color hinting for colors in hex, RBG, and named.
;; https://github.com/emacsmirror/rainbow-mode
(require 'rainbow-mode)
(add-hook 'css-mode-hook (lambda () (rainbow-mode 1)))
(add-hook 'html-mode-hook (lambda () (rainbow-mode 1)))

;;; js2-mode
;; https://github.com/mooz/js2-mode
;; TODO: Replace with js-mode when Emacs 27 comes out.
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js2-basic-offset 2)

;;; json-mode
;; Includes json-reformat and json-snatcher.
;; Note: Use C-c C-f reformats, C-c C-p displays path to object at point.
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

;;; python-mode
;; http://launchpad.net/python-mode/
;; Super-minimal Python infrastructure.  Restore docs navigation and integrate
;; flycheck if needed later.
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;;; gnuplot-mode
;; https://raw.github.com/mkmcc/gnuplot-mode/master/gnuplot-mode.el
(require 'gnuplot)
(add-hook 'gnuplot-mode-hook
          (lambda ()
            (flyspell-prog-mode)
            (add-hook 'before-save-hook
                      #'whitespace-cleanup nil t)))
;; .gp is my personally-designated Gnuplot extension.
(add-to-list 'auto-mode-alist '("\\.gp$" . gnuplot-mode))

;;; markdown-mode
;; https://github.com/jrblevin/markdown-mode
;; Note: Install textproc/markdown to integrate compilation commands.
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; aggressive-indent: On-the-fly indenting.
;; https://github.com/Malabarba/aggressive-indent-mode
(require 'aggressive-indent)
(global-aggressive-indent-mode 1)
;; Add any modes I want to exclude from this minor mode.
(add-to-list 'aggressive-indent-excluded-modes 'web-mode)
(add-to-list 'aggressive-indent-excluded-modes 'haskell-mode)
(add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode)

;;; eshell-git-prompt
;; https://github.com/xuchunyang/eshell-git-prompt
(eshell-git-prompt-use-theme 'robbyrussell)

;;; elscreen
;; https://github.com/knu/elscreen
(require 'elscreen)
(elscreen-start)
;; F7 creates a new elscreen, F8 kills it.
(global-set-key (kbd "<f7>") 'elscreen-create)
(global-set-key (kbd "<f8>") 'elscreen-kill)

;;; w3m (also called emacs-w3m)
;; http://w3m.sourceforge.net/
(autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
;; Use w3m for all URLs (deprecated code to use available GUI browser).
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; Optional keyboard short-cut.
(global-set-key (kbd "C-x M-m") 'browse-url-at-point)
;; Tabs: create: C-c C-t close: C-c C-w nav: C-c C-[np] list: C-c C-s
(setq w3m-use-tab t)
(setq w3m-use-cookies t)
;; Activate Conkeror-style link selection (toggle with f key).
(add-hook 'w3m-mode-hook #'w3m-lnum-mode)
;; To use w3m-search, hit S in w3m.  Do a C-u S to specify engine.
(require 'w3m-search)
;; Add some extra search engine URIs.
(add-to-list 'w3m-search-engine-alist
             '("hoogle" "http://haskell.org/hoogle/?q=%s"))
(add-to-list 'w3m-search-engine-alist
             '("ports" "http://freebsd.org/cgi/ports.cgi/?query=%s" nil))
(add-to-list 'w3m-search-engine-alist
             '("wikipedia" "http://en.m.wikipedia.org/wiki/Special:Search?search=%s" nil))
(add-to-list 'w3m-search-engine-alist
             '("duckduckgo" "http://www.duckduckgo.com/?q=%s" nil))
(setq w3m-search-default-engine "duckduckgo")
;; Default to the last manually specified search engine when calling the prefix
;; version of the function.
(defadvice w3m-search (after change-default activate)
  (let ((engine (nth 1 minibuffer-history)))
    (when (assoc engine w3m-search-engine-alist)
      (setq w3m-search-default-engine engine))))

;;; lusty-explorer
;; https://github.com/sjbach/lusty-emacs
(require 'lusty-explorer)
(global-set-key (kbd "C-x C-f") 'lusty-file-explorer)
(global-set-key (kbd "C-x b") 'lusty-buffer-explorer)

;;; EMMS
;; http://www.gnu.org/software/emms/
;; Currently using mplayer backend - seems superior to mpg321, which doesn't
;; support seeking.
(require 'emms-setup)
(emms-all)
(emms-default-players)
(push 'emms-player-mplayer emms-player-list)
;; Show the current track each time EMMS starts to play a track with "NP: ".
(add-hook 'emms-player-started-hook #'emms-show)
(setq emms-show-format "NP: %s")
;; When asked for emms-play-directory, always start from this one.
(setq emms-source-file-default-directory "~/snd/")
;; Some global playlist management keybindings.
(global-set-key (kbd "<kp-subtract>") 'emms-previous)
(global-set-key (kbd "<kp-add>") 'emms-next)
(global-set-key (kbd "<insert>") 'emms-pause)
(global-set-key (kbd "<kp-insert>") 'emms-pause)
(global-set-key (kbd "<kp-right>") 'emms-seek-forward)
(global-set-key (kbd "<kp-left>") 'emms-seek-backward)

;;; Magit
;; https://github.com/magit/magit
(require 'magit)
;; Idiomatic fill-column setting for commit messages.
(add-hook 'git-commit-mode-hook
          (lambda () (set-fill-column 72)))
(global-set-key (kbd "<f3>") 'magit-status)

;;; git-gutter
;; https://github.com/syohex/emacs-git-gutter
(require 'git-gutter)
(global-git-gutter-mode 1)
(setq git-gutter:update-interval 2)
(global-set-key (kbd "C-x M-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

;;; org-bullets
;; https://github.com/sabof/org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;; org-present
;; https://github.com/rlister/org-present
;; Note: Use arrow keys to navigate, C-c C-q to quit.
(autoload 'org-present "org-present" nil t)
;; Reduce the huge upscaling of text.  This amount is more reasonable for my
;; laptop, but reconsider it for larger displays.
(setq org-present-text-scale 2)
(add-hook 'org-present-mode-hook
          (lambda ()
            (org-display-inline-images)))
(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (org-remove-inline-images)))

;;; wttrin: Get a weather report.
;; https://github.com/bcbcarl/emacs-wttrin
;; Note: Requires xterm-color.
(require 'wttrin)
(setq wttrin-default-cities '("Slanesville"))
(setq wttrin-default-accept-language '("Accept-Language" . "en-US"))

;;; htmlize: Converts buffer to HTML.
;; https://github.com/hniksic/emacs-htmlize
;; TODO: Check if htmlfontify.el (being added in 23.2) is the same as this.
(require 'htmlize)

;;; pinentry: Needed for minibuffer prompt integration with GnuPG 2.1.5+ and
;;; Pinentry 0.9.5+.
(when (not (eq system-type 'windows-nt))
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

;;; powerline: Mode line replacement.
;; (when window-system
;;   (require 'powerline)
;;   (powerline-default-theme))
(require 'powerline)
(powerline-default-theme)

;;; diminish: mode-line shortening
;; https://www.eskimo.com/~seldon/diminish.el
(when (require 'diminish nil 'noerror)
  (eval-after-load "git-gutter" '(diminish 'git-gutter-mode "Git↓"))
  (eval-after-load "smartparens" '(diminish 'smartparens-mode "(ϛ)")))
;; Non-diminish major mode mode-line shortening.
(add-hook 'haskell-mode-hook
          (lambda () (setq mode-name "λ≫")))
(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq mode-name "e-λ")))
(add-hook 'clojure-mode-hook
          (lambda () (setq mode-name "cλj")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End external package load

;; Replace echo area startup message.
(run-with-timer 1 nil (lambda () (message "I have SEEN the CONSING!!")))
