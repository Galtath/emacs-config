(require 'cl)

;; Load paths
(add-to-list 'load-path "~/.emacs.d/colors/")
(add-to-list 'load-path "~/.emacs.d/colors/color-theme-6.6.0/")
(add-to-list 'load-path "~/.emacs.d/colors/color-theme-molokai/")
(add-to-list 'load-path "~/.emacs.d/bundle/")
(add-to-list 'load-path "~/.emacs.d/bundle/evil/")
(add-to-list 'load-path "~/.emacs.d/bundle/yasnippet/")
(add-to-list 'load-path "~/.emacs.d/bundle/smex/")
(add-to-list 'load-path "~/.emacs.d/bundle/powerline/")
(add-to-list 'load-path "~/.emacs.d/colors/color-theme-neverland/")
(add-to-list 'load-path "~/.emacs.d/bundle/Saaxy/")
(add-to-list 'load-path "~/.emacs.d/bundle/ace-jump-mode/")
(add-to-list 'load-path "~/.emacs.d/bundle/multiple-cursors/")
(add-to-list 'load-path "~/.emacs.d/bundle/slime/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/colors/")

;; Modes

(ido-mode 1)
(setq ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-everywhere t)
(show-paren-mode 1)

;; Minimal emacs

(require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

;; scrolling (decided I like default scrolling better)
;; (setq
;;  scroll-margin 1
;;  scroll-conservatively 1000000
;;  scroll-preserve-screen-position 1)
;; ;; scroll one line at a time (less "jumpy" than defaults)
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 1) ;; keyboard scroll one line at a time

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Deletes whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; vim
;; (require 'evil)
;; (evil-mode 1)
;;  (defun my-move-key (keymap-from keymap-to key)
;;    "Moves key binding from one keymap to another, deleting from the old location. "
;;    (define-key keymap-to key (lookup-key keymap-from key))
;;    (define-key keymap-from key nil))
;;  (my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
;;  (my-move-key evil-motion-state-map evil-normal-state-map " ")
;; (define-key evil-normal-state-map "\C-u" 'evil-scroll-up)

;; menu and stuff
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; syntax
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; or no syntax, I guess
(defun decolorize-font-lock ()
  "remove all colors from font-lock faces except comment and warning"
  (let ((fg (face-attribute 'default :foreground))
	(bg (face-attribute 'default :background)))
    (mapc (lambda (face)
	    (when face
	      (set-face-attribute face nil
				  :foreground fg
				  :background bg)))
	  (mapcar (lambda (f)
		    (if (and (string-match "^font-lock" (symbol-name f))
			     (not (string-match "-comment\\|-warning" (symbol-name f))))
			f
		      nil))
		  (face-list)))))

;(decolorize-font-lock)

;; smex
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; yasnippet stuff (code completion)
(require 'yasnippet)
(yas/load-directory "~/.emacs.d/bundle/yasnippet/snippets")

;; Colors
(require 'rainbow-mode)
(require 'color-theme)
(require 'face-list)
(color-theme-initialize)
;(load-theme 'ir-black t)
(require 'color-theme-neverland)
(color-theme-neverland)
;(color-theme-molokai)

;; font
;; Note: if you are using emacs --daemon
;; Put: Emacs.font: Gohufont 8
;; in your .Xresources, otherwise the daemon will just use default font, like a bitch.
(set-default-font "gohufont 8")


;; powerline
(require 'powerline)
(powerline-default-theme)

;; c
(setq c-default-style "linux"
      c-basic-offset 4)

;; slime
(setq inferior-lisp-program "/usr/bin/clisp")
(require 'slime)
(slime-setup '(slime-fancy))


;; scheme
(require 'cmuscheme)
(setq scheme-program-name "csi")
(define-key scheme-mode-map "\C-c\C-l" 'scheme-load-current-file)
(define-key scheme-mode-map "\C-c\C-k" 'scheme-compile-current-file)

(defun scheme-load-current-file (&optional switch)
  (interactive "P")
  (let ((file-name (buffer-file-name)))
    (comint-check-source file-name)
    (setq scheme-prev-l/c-dir/file (cons (file-name-directory    file-name)
					 (file-name-nondirectory file-name)))
    (comint-send-string (scheme-proc) (concat "(load \""
					      file-name
					      "\"\)\n"))
    (if switch
      (switch-to-scheme t)
      (message "\"%s\" loaded." file-name) ) ) )

(defun scheme-compile-current-file (&optional switch)
  (interactive "P")
  (let ((file-name (buffer-file-name)))
    (comint-check-source file-name)
    (setq scheme-prev-l/c-dir/file (cons (file-name-directory    file-name)
					 (file-name-nondirectory file-name)))
    (message "compiling \"%s\" ..." file-name)
    (comint-send-string (scheme-proc) (concat "(compile-file \""
					      file-name
					      "\"\)\n"))
    (if switch
      (switch-to-scheme t)
      (message "\"%s\" compiled and loaded." file-name) ) ) )

;; paredit
(require 'paredit)
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

(defun override-slime-repl-bindings-with-paredit () ;; Stop SLIME's REPL from grabbing DEL,
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)


(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))
(add-hook 'chicken-scheme-mode-hook   (lambda () (paredit-mode +1)))


;; saaxy
(require 'saaxy)

;; set ace-line-mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(global-set-key (kbd "C-/") 'ace-jump-mode)

;; multiple cursors
(require 'multiple-cursors)

;; Set f-keys
(global-set-key [f2] 'other-window) ; window switching
(global-set-key [f5] 'shell-pop) ; eshell
(global-set-key [f8] 'erc-track-switch-buffer) ; erc activity

;;; Shell-Pop is a utility which helps you pop up and pop out shell buffer
;;; window easily.
(require 'shell-pop)

(shell-pop-set-internal-mode "eshell") ; shell, ansi-term, terminal, eshell
(shell-pop-set-internal-mode-shell "/bin/bash")
(shell-pop-set-window-height 40)         ; the number as a percentage
(shell-pop-set-window-position "bottom") ; pop-up position. (top or bottom)
(global-set-key [f7] 'shell-pop)

;;; Shell scripts

(add-to-list 'auto-mode-alist '("bashrc$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_profile$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_aliases$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_local$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_completion$" . sh-mode))

(add-hook 'shell-mode-hook (lambda () (setq tab-width 8)))
;; ansi-term
; load-up ansi-term
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'ansi-term-mode-hook 'ansi-color-for-comint-mode-on)


;; extra defuns
(defun iwb()
  "Indent Whole Buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun delete-other-windows-replacement (&optional p)
  "Make the selected window fill its frame.  If called with PREFIX,
kill all other visible buffers."
  (interactive "P")
  (if p
      (dolist (window (window-list))
        (unless (equal (window-buffer window) (current-buffer))
          (kill-buffer (window-buffer window)))))
  (delete-other-windows))

;; Quickly jump back and forth between matching parens/brackets
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))


;; ERC ;;----------------------

(require 'erc)

;; keeps prompt at bottom
;(add-hook 'erc-mode-hook 'erc-add-scroll-to-bottom)

(setq  erc-server-coding-system '(utf-8 . utf-8))

;; wrap after word
(add-hook 'erc-mode-hook 'visual-line-mode)
;; Truncate buffer to under so many characters
(erc-truncate-mode)

;; Log
(erc-log-mode 1)
(setq erc-log-channels-directory "~/.erc/logs/")
(setq erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t)

;; (setq erc-save-buffer-on-part t)
;; (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
;; (save-some-buffers t (lambda () (when (eq major-mode 'erc-mode) t))))


;; fix autofill
(add-hook 'window-configuration-change-hook
	   '(lambda ()
	      (setq erc-fill-column (- (window-width) 2))))


;; Kill buffers for channels after /part
 (setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
 (setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
 (setq erc-kill-server-buffer-on-quit t)


(setq erc-timestamp-only-if-changed-flag nil
      erc-timestamp-format "%H:%M "
      erc-fill-prefix "      "
      erc-insert-timestamp-function 'erc-insert-timestamp-left)

(setq erc-inputface "#FFFFFF")

(and
 (require 'erc-highlight-nicknames)
 (add-to-list 'erc-modules 'highlight-nicknames)
 (erc-update-modules))

(setq erc-autojoin-channels-alist
          '(("freenode.net" "#slackware" "#emacs" "#lisp" "#slackbuilds" "##crawl" "#alsa" "#unicycle" "##reddit-roguelikes" "#dwarffortress" "#chicken" "#eccada")
;	    ("telecomix.org" "#telecomix")
	    ("quakenet.org" "#xonotic" "#xonotic.pickup" "#smokebreak" "#mon.xonotic" "#pb.xonotic" "#quakelive" "#qwrookie" "#QW.NA" "#quakeworld" "#teamliquidbw")
	    ("oftc.net" "#tor" "#tor-dev" "#nottor" "#tails")
	    ))

;; connect too networks, instead of erc
(defun irc-maybe ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "IRC? ")
    (erc-tls :server "irc.freenode.net" :port 6697
	     :nick "CampinSam" :full-name "CampinSam")
    (erc :server "servercentral.il.us.quakenet.org" :port 6667
    	 :nick "Galtath" :full-name "Galt")
    ;; (erc :server "irc.telecomix.org" :port 6667
    ;; 	 :nick "Galt" :full-name "Noah")
    (erc-tls :server "irc.oftc.net" :port 6697
	     :nick "CampinSam" :full-name "CampinSam")
    ))


;; ERC END ;; ----------------

;; MAIL?
;; removed stuff for git

;; custom file for custom-set-variables and etc
(setq custom-file "~/.emacs-custom.el") ;; is some font-face changes, and some email settings.
(load custom-file)
