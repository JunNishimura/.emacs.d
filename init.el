;; package-initialization
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

;; load-path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; display setting
(menu-bar-mode -1) ;; hide menu bar
(tool-bar-mode -1) ;; hide tool bar
(scroll-bar-mode -1) ;; hide scroll bar

;; make emacs fullscreen when to open
(set-frame-parameter nil 'fullscreen 'maximized)

;; semitransparent the screen
(if window-system
    (progn
      (set-frame-parameter nil 'alpha 95)))

;; display line number
(global-display-line-numbers-mode)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; theme
(load-theme 'gruvbox-dark-medium t)

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)
(straight-use-package 'use-package)

;; auto-save
(use-package auto-save-buffers-enhanced
  :config
  (setq auto-save-buffers-enhanced-interval 10)
  (auto-save-buffers-enhanced t))

;; GitHub copilot
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :hook
  (prog-mode . copilot-mode)
  :config
  (setq warning-suppress-log-types '((copilot copilot-exceeds-max-char)))
  :bind
  (:map copilot-completion-map
	("S-<tab>" . 'copilot-accept-completion)
        ("S-TAB" . 'copilot-accept-completion)
        ("C-TAB" . 'copilot-accept-completion-by-word)
        ("C-<tab>" . 'copilot-accept-completion-by-word)))

;; neotree
(use-package neotree
  :init
  (setq-default neo-keymap-style 'concise)
  :config
  (setq neo-smart-open t)
  (setq neo-create-file-auto-open t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (bind-key [f8] 'neotree-toggle))

;; git-gutter
(use-package git-gutter
  :custom
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  :custom-face
  (git-gutter:modified ((t (:background "#f1fa8c"))))
  (git-gutter:added ((t (:background "#508a7b"))))
  (git-gutter:deleted ((t (:background "#ff79c6"))))
  :config
  (global-git-gutter-mode +1))

;; point-undo
(use-package point-undo
  :bind (("M-[" . point-undo)
         ("M-]" . point-redo)))

;; company
(use-package company
  :config
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence)))
(use-package company-box
  :hook (company-mode . company-box-mode))

;; lsp-mode
(use-package lsp-mode)
(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-max-width 150)
  (setq lsp-ui-doc-max-height 30)
  (setq lsp-ui-peek-enable t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; go-mode
(defun go-mode-omnibus ()
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
	   "go build -v && go test -v && go vet")))
(use-package go-mode
  :hook
  (go-mode . lsp-deferred)
  (go-mode . go-mode-omnibus))

(use-package slime
  :if (file-exists-p "~/.roswell/helper.el")
  :ensure slime-company
  :init (load (expand-file-name "~/.roswell/helper.el") t)
  :custom (inferior-lisp-program "ros -Q run")
  :config (slime-setup '(slime-fancy slime-banner slime-company)))
