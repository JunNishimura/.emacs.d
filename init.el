;; make emacs fullscreen when to open
(set-frame-parameter nil 'fullscreen 'maximized)

;; semitransparent the screen
(if window-system
    (progn
      (set-frame-parameter nil 'alpha 95)))