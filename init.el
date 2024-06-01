;; package-initialization
(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")))

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
  :bind
  (:map copilot-completion-map
	("S-<tab>" . 'copilot-accept-completion)
        ("S-TAB" . 'copilot-accept-completion)
        ("C-TAB" . 'copilot-accept-completion-by-word)
        ("C-<tab>" . 'copilot-accept-completion-by-word)))
