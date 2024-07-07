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
(setq my-theme (list 'gruvbox-dark-medium 'nord 'dracula))
(defun toggle-theme ()
  (interactive)
  (disable-theme (car my-theme))
  (setq my-theme (append (cdr my-theme) (list (car my-theme))))
  (load-theme (car my-theme) t))
(global-set-key [f7] 'toggle-theme)
(load-theme (car my-theme) t)

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

;; slime
(use-package slime
  :if (file-exists-p "~/.roswell/helper.el")
  :ensure slime-company
  :init (load (expand-file-name "~/.roswell/helper.el") t)
  :custom (inferior-lisp-program "ros -Q run")
  :config (slime-setup '(slime-fancy slime-banner slime-company)))

;; org-mode
(use-package org
  :config
  (setq org-log-done 'time)
  (setq org-todo-keywords
	'((sequence "TODO(t)" "DOING(d)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))))
(use-package org-agenda
  :config
  (define-key global-map "\C-ca" 'org-agenda))
(use-package org-capture
  :config
  (define-key global-map "\C-cc" 'org-capture)
  (if (file-directory-p "~/Dropbox/org")
      (setq org-directory "~/Dropbox/org")
    (setq org-directory "~/org"))
  (setq todo-path (concat org-directory "/Plans/todo.org"))
  (setq mtg-path (concat org-directory "/Inbox/mtg.org"))
  (setq proj-path (concat org-directory "/Projects/kintai-performance.org"))
  (setq org-capture-templates
	       '(("t" "Todo" entry (file+headline todo-path "Tasks")
		 "** TODO %^{Task Title} [/]\n DEADLINE: %^t\n %?\n")
		 ("m" "Meeting" entry (file+headline mtg-path "Meetings")
		  "** %^{Meeting Title} %^g\n %U\n")
		 ("p" "Project" entry (file+headline proj-path "Memo")
		  "** %^{Project Title} %^g\n")))
  (setq org-agenda-files (list todo-path mtg-path proj-path))
  (defun show-todo-buffer (file)
    (interactive)
    (if (get-buffer file)
	(let ((buffer (get-buffer file)))
	  (switch-to-buffer buffer)
	  (message "%s" file))
      (find-file file)))
  (global-set-key (kbd "C-M-t") '(lambda () (interactive) (show-todo-buffer todo-path))))

;; window-move
(defun counter-other-window ()
 (interactive)
  (other-window -1))
(global-set-key (kbd "C-;") 'other-window)
(global-set-key (kbd "C-:") 'counter-other-window)

;; tab-management
(use-package iflipb
  :config
  (global-set-key (kbd "M-o") 'iflipb-next-buffer)
  (global-set-key (kbd "M-O") 'iflipb-previous-buffer)
  (setq iflipb-ignore-buffers (list "^[*]" "^magit"))
  (setq iflipb-wrap-around t))

;; warning
(setq warning-minimum-level :error)

;; all-the-icons
(use-package all-the-icons
  :demand t)

;; dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  :init
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-items '((recents  . 5)
		     (bookmarks . 5)
		     (projects . 5)
		     (agenda . 5)))
  (setq dashboard-item-shortcuts '((recents . "r")
			      (bookmarks . "b")
			      (projects . "p")
			      (agenda . "a")))
  (setq dashboard-icon-type 'all-the-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))

;; modeline
(use-package moody
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (setq x-underline-at-descent-line t)
  (when (eq system-type 'darwin)
  (setq moody-slant-function 'moody-slant-apple-rgb)))
(use-package minions
  :config
  (minions-mode)
  (setq minions-mode-line-lighter "[+]"))
(column-number-mode)

;; ident highlight
(use-package highlight-indentation
  :config
  (add-hook 'prog-mode-hook 'highlight-indentation-mode)
  (set-face-background 'highlight-indentation-face "#3e4446")
  (set-face-background 'highlight-indentation-current-column-face "#2b2b2b"))

;; volatile-highlights
(use-package volatile-highlights
  :hook
  (after-init . volatile-highlights-mode)
  :custom-face
  (vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD")))))

;; beacon 
(use-package beacon
  :custom
  (beacon-color "#f1fa8c")
  :config
  (beacon-mode 1))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; minimap
;; (use-package minimap
;;   :commands
;;   (minimap-create minimap-kill minimap-bufname)
;;   :custom
;;   (minimap-major-modes '(prog-mode))
;;   (minimap-window-location 'right)
;;   (minimap-update-delay 0.2)
;;   (minimap-minimum-width 20)
;;   :bind
;;   ("C-c m" . jun/toggle-minimap)
;;   :preface
;;   (defun jun/toggle-minimap ()
;;     "Toggle minimap for current buffer."
;;     (interactive)
;;     (if (null (minimap-bufname))
;; 	(minimap-create)
;;       (minimap-kill)))
;;   :config
;;   (custom-set-faces
;;    '(minimap-active-region-background
;;      ((((background dark))
;;        (:background "#555555555555"))
;;       (t (:background "#C5C8C6C8C8C6"))) :group 'minimap)))
  (use-package minimap
    :commands
    (minimap-create minimap-kill minimap-active-current-buffer-p)
    :custom
    (minimap-major-modes '(prog-mode))
    (minimap-window-location 'right)
    (minimap-update-delay 0.2)
    (minimap-minimum-width 20)
    :bind
    ("C-c m" . toggle-minimap)
    :preface
    (defun toggle-minimap ()
      "Toggle minimap for current buffer."
      (interactive)
      (if (minimap-active-current-buffer-p)
	  (minimap-kill)
	(minimap-create)))
    :config
    (custom-set-faces
     '(minimap-active-region-background
      ((((background dark)) (:background "#555555555555"))
    (t (:background "#C847D8FEFFFF"))) :group 'minimap)))
