;; mine :
;; keybinding
(global-set-key [(meta g)] 'goto-line)
(global-set-key [(ctrl tab)] 'other-window)
(global-set-key [(ctrl o)] 'ff-find-other-file)

(fset 'yes-or-no-p 'y-or-n-p)
;; Set cursor and mouse-pointer colours
(set-cursor-color "red")
(set-mouse-color "goldenrod")
(setq mouse-wheel-scroll-amount '(4))
(setq mouse-wheel-progressive-speed nil)
(mwheel-install)


 ;; "ctrl - left click" buffer menu: increase number of items shown
;; set max length of this list. default 20. see next.
(setq mouse-buffer-menu-maxlen 40)
;; set # buffer in a mode before grouping begins. takes precedence over previous
;; set to 1 to always group by mode. default 4
(setq mouse-buffer-menu-mode-mult 20)


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (expand-file-name "~/.emacs.d/elpa/package.el")
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("marmalade" . "https://marmalade-repo.org/packages/")
                           ("melpa-stable" . "https://stable.melpa.org/packages/"))
  )
)
;; 

;; varnish style
(defun des-knf ()
  (interactive)

  ;; Basic indent is 8 spaces
  (make-local-variable 'c-basic-offset)
  (setq c-basic-offset 8)

  ;; Continuation lines are indented 4 spaces
  (make-local-variable 'c-offsets-alist)
  (c-set-offset 'arglist-cont 4)
  (c-set-offset 'arglist-cont-nonempty 4)
  (c-set-offset 'statement-cont 4)

  ;; Labels are flush to the left
  (c-set-offset 'label [0])

  ;; Fill column
  (make-local-variable 'fill-column)
  (setq fill-column 74))

(defun des-programming-keys ()
  (local-set-key "\C-m" 'newline-and-indent)
  (local-set-key [RET] 'newline-and-indent))

(defun des-c-mode-hook ()
  (des-knf)
  (des-programming-keys))

(add-hook 'c-mode-common-hook 'des-c-mode-hook)


;; Varnish mode
 (setq auto-mode-alist
 (cons '("\\.vcl$" . c-mode)
 auto-mode-alist))

(load "php-mode")
(load "cmake-mode")
(load "vcl-mode")

(add-to-list 'auto-mode-alist
             '("\\.php[34]?\\'\\|\\.phtml\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.module\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

(add-to-list 'auto-mode-alist '("\\.vcl\\'" . vcl-mode))
(add-to-list 'auto-mode-alist '("\\.vtc\\'" . vcl-mode))
(add-to-list 'auto-mode-alist '("\\.vcl.tmpl\\'" . vcl-mode))

(require 'flymake-php)
  (add-hook 'php-mode-hook 'flymake-php-load)

;; 
(add-to-list 'load-path "~/.emacs.d/vendor")
(load "jinja")
(add-to-list 'load-path "~/.emacs.d/lisp")
(autoload 'completion-ignored-build-mode
  "completion-ignored-build" nil t)
(autoload 'completion-ignored-build-enable-setup
  "completion-ignored-build")
(add-hook 'minibuffer-setup-hook
	  'completion-ignored-build-enable-setup)

;; Ignore extensions in completion
(setq completion-ignored-extensions
      '(".o" ".lo" ".mh" ".elc" "~"
	".bin" ".lbin" ".fasl" ".dvi" ".toc"   ".aux" ".lof" ".blg" ".bbl"
	".glo" ".idx" ".lot" ".pyc" ))

(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 150))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) 300)
                             (frame-char-height)))))))

(set-frame-size-according-to-resolution)

; don't show the startup screen
(setq inhibit-startup-screen t)
; don't show the menu bar
(menu-bar-mode nil)
; don't show the tool bar
(require 'tool-bar)
(tool-bar-mode nil)
; don't show the scroll bar
(scroll-bar-mode nil)

; require final newlines in files when they are saved
(setq require-final-newline t)
; always use spaces, not tabs, when indenting
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

; display line numbers to the right of the window
(global-linum-mode t)
; show the current line and column numbers in the stats bar as well
(line-number-mode t)
(column-number-mode t)

; highlight parentheses when the cursor is next to them
(require 'paren)
(show-paren-mode t)


; use the "Subtle Hacker" color theme as a base for the custom scheme
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(color-theme-subtle-hacker)

;:background gray2
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:overline nil :inherit nil :stipple nil :background "#242424" :foreground "#FFF991" :inverse-video nil :box nil :strike-through nil :underline nil :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(border ((t nil)))
 '(cursor ((t (:background "firebrick1" :foreground "black"))))
 '(font-lock-comment-delimiter-face ((default (:inherit font-lock-comment-face :weight ultra-bold)) (((class color) (min-colors 16)) nil)))
 '(font-lock-comment-face ((t (:foreground "lime green"))))
 '(font-lock-doc-face ((t (:foreground "tomato" :slant italic))))
 '(font-lock-function-name-face ((t (:foreground "deep sky blue" :underline t :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "gold" :weight bold))))
 '(font-lock-string-face ((t (:foreground "tomato" :slant italic))))
 '(fringe ((nil (:background "black"))))
 '(highlight ((t (:background "khaki1" :foreground "black" :box (:line-width -1 :color "firebrick1")))))
 '(highlight-current-line-face ((t (:inherit highlight))))
 '(lazy-highlight ((t (:background "paleturquoise" :foreground "black"))))
 '(link ((t (:foreground "DodgerBlue3" :underline t))))
 '(menu ((t (:background "gray2" :foreground "#FFF991"))))
 '(minibuffer-prompt ((t (:foreground "royal blue"))))
 '(mode-line ((t (:background "dark olive green" :foreground "dark blue" :box (:line-width -1 :color "gray75") :weight bold))))
 '(mode-line-buffer-id ((t (:background "dark olive green" :foreground "beige"))))
 '(mode-line-highlight ((((class color) (min-colors 88)) nil)))
 '(mode-line-inactive ((t (:background "dark olive green" :foreground "dark khaki" :weight light))))
 '(mouse ((t (:background "Grey" :foreground "black"))))
 '(trailing-whitespace ((((class color) (background dark)) (:background "firebrick1")))))

; make sure the frames have the dark background mode by default
(setq default-frame-alist (quote (
  (frame-background-mode . dark)
)))


(require 'auto-complete)
(global-auto-complete-mode t)

(require 'auto-complete-c-headers)
(add-to-list 'ac-sources 'ac-source-c-headers)


;; debug python:
(defun add-py-debug ()  
      "add debug code and move line down"  
    (interactive)  
    (move-beginning-of-line 1)  
    (insert "import pdb; pdb.set_trace();\n"))  

(global-set-key [(f9)] 'add-py-debug)


;; alternative to C-x C-w
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))


;; elpa
(elpy-enable)
(defvar myPackages
  '(better-defaults
    elpy
    flycheck ;; add the flycheck package
    material-theme))

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (elpy jedi dockerfile-mode markdown-mode org js2-mode yaml-mode vcl-mode puppet-mode php-mode magit haml-mode groovy-mode flymake-php color-theme cmake-mode auto-complete-c-headers))))
