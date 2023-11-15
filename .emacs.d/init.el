;; my init

(setq byte-compile-warnings '(cl-functions))

;; mine :
;; keybinding
(global-set-key [(meta g)] 'goto-line)
(global-set-key [(ctrl tab)] 'other-window)
(global-set-key [(ctrl o)] 'ff-find-other-file)

(fset 'yes-or-no-p 'y-or-n-p)
;; Set cursor and mouse-pointer colours
(set-cursor-color "red")
(set-mouse-color "goldenrod")

(setq-default
 mouse-wheel-scroll-amount '(4 ((shift) 10) ((control) . nil))
 mouse-wheel-progressive-speed nil)

(setq-default smooth-scroll-margin 1)


 ;; "ctrl - left click" buffer menu: increase number of items shown
;; set max length of this list. default 20. see next.
(setq mouse-buffer-menu-maxlen 40)
;; set # buffer in a mode before grouping begins. takes precedence over previous
;; set to 1 to always group by mode. default 4
(setq mouse-buffer-menu-mode-mult 20)


(require 'package)
(add-to-list 'package-archives
             '("MELPA Stable" . "http://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))


;; Installs packages
;;
;; myPackages contains a list of package names
(defvar myPackages
  '(
    exec-path-from-shell
    elpy
    flycheck
    py-autopep8
    auto-complete
    magit
    jedi
    material-theme
    cmake-mode
    dockerfile-mode
    graphviz-dot-mode
    groovy-mode
    markdown-mode
    puppet-mode
    php-mode
    vcl-mode
    yaml-mode
    use-package
    python-isort
    python-black
    org-download
    flymake-php
    )
  )

;; Scans the list in myPackages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)
;; 

;; external files:
(load "~/.emacs.d/python.el")
(load "~/.emacs.d/orgsetup.el")
(load "~/.emacs.d/markdown.el")

;; debug python:
(defun add-py-debug ()  
      "add debug code and move line down"  
    (interactive)  
    (move-beginning-of-line 1)  
    (insert "import pdb; pdb.set_trace();\n"))  

(global-set-key [(f9)] 'add-py-debug)


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
  (setq fill-column 79))

(setq-default fill-column 80)

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


;:background gray2
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:overline nil :inherit nil :stipple nil :background "#242424" :foreground "#FFF991" :inverse-video nil :box nil :strike-through nil :underline nil :foundry "nil" :family "DejaVu Sans Mono" :slant normal :weight semi-light :height 120 :width normal))))
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



(when (eq system-type 'darwin)
  (setq x-alt-keysym 'meta)
  (setq mac-option-modifier nil  ;; if not set, special chars "option+(" => {[ are broken
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

  ;; enable copy-paste in clipboard when in terminal
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx)

  ;; enable emacs region (selection) to be pasted in mac osx clipboard
  (setq mouse-drag-copy-region t)
  )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(package-selected-packages
   '(gitlab-ci-mode-flycheck gitlab-ci-mode terraform-mode hcl-mode plantuml-mode rnc-mode rfc-mode python-isort python-black lsp-ui lsp-mode org-contrib org-jira org-download elpy exec-path-from-shell yaml-mode vcl-mode tox py-autopep8 puppet-mode php-mode org material-theme markdown-mode magit js2-mode jedi haml-mode groovy-mode graphviz-dot-mode flymake-php flycheck dockerfile-mode color-theme cmake-mode blacken better-defaults auto-complete-c-headers))
 '(show-paren-mode t))


(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.backup/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; 
