(defun maximize-frame (&optional frame)
  "Maximize the selected FRAME."
  (interactive)
  (or frame
    (setq frame (selected-frame)))
  (let ((pixels-per-col (/ (float (frame-pixel-width))
			  (frame-width)))
	 (pixels-per-row (/ (float
			      (frame-pixel-height)) (frame-height))))
    (set-frame-size frame
      ;; truncate or round?
      (truncate (/
		  (x-display-pixel-width) pixels-per-col))
      ;; reduce size to account for the toolbar
      (- (truncate (/ (x-display-pixel-height) pixels-per-row)) 7))

    (set-frame-position frame 0 0)))

(global-set-key  "\C-f" 'maximize-frame)

(load-library "color-theme")
(color-theme-initialize)

(defun color-theme-lg-dark ()
  "dark color theme created by lg, Jan. 2009."
  (interactive)
  (color-theme-install
    '(color-theme-lg-dark
       ((foreground-color . "#a9eadf")
	 (background-color . "black")
	 (background-mode . dark))
       (bold ((t (:bold t))))
       (bold-italic ((t (:italic t :bold t))))
       (default ((t (nil))))

       (font-lock-builtin-face ((t (:italic t :foreground "#a96da0"))))
       (font-lock-comment-face ((t (:italic t :foreground "#bbbbbb"))))
       (font-lock-comment-delimiter-face ((t (:foreground "#666666"))))
       (font-lock-constant-face ((t (:bold t :foreground "#197b6e"))))
       (font-lock-doc-string-face ((t (:foreground "#3041c4"))))
       (font-lock-doc-face ((t (:foreground "gray"))))
       (font-lock-reference-face ((t (:foreground "white"))))
       (font-lock-function-name-face ((t (:foreground "#356da0"))))
       (font-lock-keyword-face ((t (:bold t :foreground "#bcf0f1"))))
       (font-lock-preprocessor-face ((t (:foreground "#e3ea94"))))
       (font-lock-string-face ((t (:foreground "#ffffff"))))
       (font-lock-type-face ((t (:bold t :foreground "#364498"))))
       (font-lock-variable-name-face ((t (:foreground "#7685de"))))
       (font-lock-warning-face ((t (:bold t :italic nil :underline nil))))

       (hl-line ((t (:background "#262626" :inherit t))))
       (mode-line ((t (:foreground "#ffffff" :background "#333333"))))
       (region ((t (:foreground "#012345" :background "#555555"))))
       (show-paren-match-face ((t (:bold t :foreground "#ffffff"
				    :background "#050505")))))))


(if (and window-system (fboundp 'info-mode)) (progn (color-theme-goldenrod) (maximize-frame)))
(if (and window-system (not (fboundp 'info-mode))) (progn (color-theme-lg-dark) (maximize-frame)))
(if (not window-system) (color-theme-charcoal-black) (color-theme-goldenrod))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(color-theme-load-all-themes nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#ffffff" :foreground "#1f1c1b" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 120 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(acl2-doc-link-face ((t (:foreground "#0000FF"))) t))
