;;; start

;; enable 80 column in org mode

(add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (python . t)))


(use-package org-download
    :after org
    :defer nil
    :custom
    (org-download-method 'directory)
    (org-download-image-dir "images")
    (org-download-heading-lvl nil)
    (org-download-timestamp "%Y%m%d-%H%M%S_")
    (org-image-actual-width 300)
    (org-download-screenshot-method "/usr/local/bin/pngpaste %s")
    :bind
    ("C-M-y" . org-download-screenshot)
    :config
    (require 'org-download))

;; open with inlines images displayed

(setq org-display-inline-images t)
(setq org-redisplay-inline-images t)
(setq org-startup-with-inline-images t)

;; open with headers collapsed
(setq org-startup-folded t)


;; org-jira
(setq jiralib-url "https://jira.refinitiv.com")

(setq jiralib-token
    (cons "Authorization"
          (concat "Bearer " (auth-source-pick-first-password
              :host "jira.refinitiv.com"))))

;;; end
