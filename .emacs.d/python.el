;; elpy requirements:
;; pip install flake8 pylint mypy
;; flycheck requirements:
;; pip install autopep8 jedi



;; python
(exec-path-from-shell-initialize)

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(require 'python-isort)
(add-hook 'python-mode-hook 'python-isort-on-save-mode)

;; Replace flymake with flycheck
(global-flycheck-mode)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8
;;(require 'py-autopep8)
;;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;; tweak M-. to goto definition
(defun elpy-goto-definition-or-rgrep ()
  "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
    (interactive)
    (ring-insert find-tag-marker-ring (point-marker))
    (condition-case nil (elpy-goto-definition)
        (error (elpy-rgrep-symbol
                (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))
(define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition-or-rgrep)

;; debug python:
(defun add-py-debug ()  
      "add debug code and move line down"  
    (interactive)  
    (move-beginning-of-line 1)  
    (insert "import pdb; pdb.set_trace();\n"))  

(global-set-key [(f9)] 'add-py-debug)

