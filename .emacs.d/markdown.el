;;; start

;; enable 80 column in org mode

(add-hook 'markdown-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'markdown-mode-hook 'auto-fill-mode)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)
