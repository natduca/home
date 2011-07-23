(when (and
       (file-exists-p "/home/nduca/.emacs.d/")
       (file-exists-p "/home/nduca/.emacs.d/site-lisp/"))
  (let ((default-directory "/home/nduca/.emacs.d/site-lisp/"))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path)))
;;(setq js2-mode-dev-mode-p 1)

(let ((home-el-dir (expand-file-name "~/home/elisp")))
  (when (file-exists-p home-el-dir)
    (let ((default-directory home-el-dir))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))))

; all modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(fill-column 80)
 '(compilation-skip-threshold 1)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t))
(setq-default indent-tabs-mode nil)

(when (fboundp 'toggle-show-trailing-whitespace-show-ws)
  (toggle-show-trailing-whitespace-show-ws))
(defun all-mode-hook()
  (setq truncate-lines 1)
  (setq indent-tabs-mode nil)
  (local-set-key [(control r) (control v)] 'revert-buffer)
  (local-set-key [(control r) (control a)] 'mark-whole-buffer)
  (local-set-key [(control r) j] 'eval-region)
  (when (and
         (fboundp 'column-marker-1)
         (not (is-webkit)))
    (column-marker-1 80))
  (when (fboundp 'show-ws-highlight-trailing-whitespace)
    (show-ws-highlight-trailing-whitespace))
  )

(defun save-and-compile()
 (interactive "")
 (save-buffer 0)
 (compile "/bin/bash -l -c \"do_g1_make\"")
 )

(defun next-error-and-center()
  (interactive "")
  (next-error)
)

(global-set-key "\C-c\C-v" 'save-and-compile)
(global-set-key "\C-c\C-f" 'next-error-and-center)

(defun is-webkit ()
  (string-match "third_party/WebKit/" (buffer-file-name)))

; c/c++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-c-common-hook ()
  (all-mode-hook)
  (when (is-webkit)
    (message "Detected a webkit file. Using 4-space indentation.")
    (setq c-basic-offset 4))
  )
(add-hook 'c++-mode-common-hook 'my-c-common-hook)
(add-hook 'c-mode-common-hook 'my-c-common-hook)
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))

; html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-html-mode-hook ()
  (local-set-key "\C-c\C-v" 'save-and-compile)
  )
(add-hook 'html-mode-hook 'my-html-mode-hook)

; todoo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-todoo-mode-hook ()
  (auto-fill-mode '0)
  )
(add-hook 'todoo-mode-hook 'my-todoo-mode-hook)

; css
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-css-mode-hook ()
  (local-set-key "\C-c\C-v" 'save-and-compile)
  (setq 'css-indent-offset 2)
  )
(add-hook 'css-mode-hook 'my-css-mode-hook)


; changelogs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-change-log-hook()
  (all-mode-hook)
  (setq nxml-child-indent 4)
  )
(add-hook 'change-log-mode-hook 'my-change-log-hook)

; js2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-js2-mode-hook()
  (all-mode-hook)
  (setq js2-mirror-mode nil)
  (setq js2-escape-quotes nil)
  (setq js2-bounce-indent-flag nil)
  (setq js2-basic-offset 2)
  )
(add-hook 'js2-mode-hook 'my-js2-mode-hook)

; javascript (non-js2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-js-mode-hook()
  (all-mode-hook)
  (setq js-indent-level 2)
  )
(add-hook 'js-mode-hook 'my-js-mode-hook)

; java
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-java-mode-hook()
  (all-mode-hook)
  (setq c-basic-offset 4)
  )
(add-hook 'java-mode-hook 'my-java-mode-hook)


; python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-python-mode-hook()
  (all-mode-hook)
  (setq python-indent 2)
  )
(add-hook 'python-mode-hook 'my-python-mode-hook)

; Latex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-latex-common-hook ()
  (setq tex-dvi-view-command "xdvi")
  (local-set-key "\C-c\C-v" 'save-and-compile)
  (local-set-key "\C-c\C-c" 'tex-view)
  (local-set-key "\C-c\C-m" 'next-error)
  )
(add-hook 'latex-mode-hook 'my-latex-common-hook)


; Child files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(load-file "/home/build/public/eng/elisp/google.el")


; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [f4] 'kill-this-buffer)
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
(global-set-key [f9] 'shrink-window-horizontally)
(global-set-key [f10] 'enlarge-window-horizontally)
(global-set-key [f11] 'enlarge-window)
(global-set-key [f12] 'shrink-window)


(global-set-key (quote [C-tab]) 'other-window)
(global-set-key [f2] 'other-frame)

(global-set-key "\C-s" 'isearch-forward-regexp)
(global-unset-key "\C-r")

(global-set-key "\M-i" 'indent-region)

(global-unset-key "\C-r")
(global-set-key "\C-r\C-e" 'replace-string)
(global-set-key "\C-r\C-q" 'query-replace)
(global-set-key "\C-r\C-r" 'replace-regexp)


; Look n feel
(set-frame-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
(set-background-color "black")
(set-foreground-color "white")
(set-cursor-color "white")

(setq default-frame-alist
      (append default-frame-alist
       '((background-color . "black")
         (foreground-color . "white")
         (cursor-color . "white")
         (font . "-unknown-DejaVu Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
         )))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
  
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode 0))


(global-font-lock-mode '1) ;syntax highlight

(transient-mark-mode '1) ;mark

(mouse-avoidance-mode 'exile) ;mouse

(global-hl-line-mode 1) ; show current line
(set-face-background 'hl-line "#0F0F0F")

(when (fboundp 'global-linum-mode)
  (global-linum-mode 1)  ;line numbers
  (set-face-background 'linum "#0F0F0F")
  )

(when (fboundp 'set-fringe-mode)
  (setq fringe-style 'minimal) ; shrink the fringe
  (set-fringe-mode 1)
  (set-face-background 'fringe "#0F0F0F")
  )

(setq make-backup-files nil) ;backups

; Things that don't quite work yet
(global-set-key [f6] (lambda ()
                       (interactive "")
                       (message "making frame on 10.98.8.119:0")
                       (make-frame-on-display "10.98.8.119:0")
                       ))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


;; Shift the selected region right if distance is postive, left if
;; negative
(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))

(global-set-key [C-S-left] 'shift-left)
(global-set-key [C-S-right] 'shift-right)

(global-set-key [M-n] 'next-error)
(global-set-key [M-p] 'previous-error)

;;  if possible...
(when (locate-library "goog")
  (load-library "goog")
  )

(when (locate-library "column-marker")
  (load-library "column-marker")
  )

(require 'quickopen)
