;; Search paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (and
       (file-exists-p (expand-file-name "~/.emacs.d/"))
       (file-exists-p (expand-file-name "~/.emacs.d/site-lisp/")))
  (let ((default-directory (expand-file-name "~/.emacs.d/site-lisp/")))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path)))

(let ((home-el-dir (expand-file-name "~/home/elisp")))
  (when (file-exists-p home-el-dir)
    (let ((default-directory home-el-dir))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))))

;; Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun is-webkit ()
  (when (buffer-file-name)
    (string-match "third_party/WebKit/" (buffer-file-name))))

(defun check-webkit-style()
  (interactive "")
  (let ((old_cd default-directory))
    (cd (concat (get_g1_dir) "third_party/WebKit"))
    (compile "check-webkit-style")
    (message old_cd)
    (cd old_cd)))

;; Util functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun strrchr(x y)
  (with-temp-buffer
    (insert x)
    (condition-case ex
        (progn
          (search-backward y)
          (- (point) 1))
      ('error -1))))

(defun splitext(x)
  (let ((i (strrchr x ".")))
    (if (/= i -1)
        (cons
         (substring x 0 i)
         (substring x i))
      (cons x ""))))

(unless (fboundp 'find-if)
  (defun find-if(predicate list)
    "Find first item satisfying PREDICATE in LIST."
    (let (result)
      (while (and list (not result))
        (if (funcall predicate (car list))
            (setq result (car list)))
        (setq list (cdr list)))
      result)))

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
  (nyan-mode 1)
  (when (and
         (fboundp 'column-marker-1)
         (not (is-webkit)))
    (column-marker-1 80))
  (when (fboundp 'show-ws-highlight-trailing-whitespace)
    (show-ws-highlight-trailing-whitespace))
  )

; c/c++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-c-common-hook ()
  (all-mode-hook)
  (when (is-webkit)
    (setq c-basic-offset 4))
  )
(add-hook 'c++-mode-common-hook 'my-c-common-hook)
(add-hook 'c-mode-common-hook 'my-c-common-hook)
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.gyp\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.gypi\\'" . python-mode))

; html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-html-mode-hook ()
  (all-mode-hook)
  (local-set-key "\C-c\C-v" 'save-and-compile)
  )
(add-hook 'html-mode-hook 'my-html-mode-hook)

; lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-lisp-mode-hook ()
  (all-mode-hook)
  (local-set-key "\C-c\C-d" 'eval-defun)
  (local-set-key "\C-c\C-r" 'eval-region)
  )
(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)


; todoo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-todoo-mode-hook ()
  (auto-fill-mode '0)
  )
(add-hook 'todoo-mode-hook 'my-todoo-mode-hook)

; css
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-css-mode-hook ()
  (all-mode-hook)
  (local-set-key "\C-c\C-v" 'save-and-compile)
  (setq 'css-indent-offset 2)
  )
(add-hook 'css-mode-hook 'my-css-mode-hook)

; fundamental
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-fundamental-mode-hook ()
  (all-mode-hook)
  )
(add-hook 'fundamental-mode-hook 'my-fundamental-mode-hook)


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

;; Other file, next-file, revert-all-buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun other-file(x)
  (let* ((split (splitext x))
         (basename (car split))
         (basename_without_test_suffix
          (if (/= (strrchr basename "_test") -1)
              (substring basename 0 (strrchr basename "_test"))
            nil))
         (basename_has_test_suffix (not (not basename_without_test_suffix)))
         (ext (downcase (cdr split))))
    (defun goto(exts)
      (let ((fileopts (mapcar
                         (lambda (ex) (concat basename ex))
                         exts)))
        (find-if 'file-exists-p fileopts)
        ))
    (defun goto-fullname(fullname)
      (message (format "checking %s" fullname))
      (if (file-exists-p fullname)
          fullname
        nil))
    (cond
     ((member ext '(".c" ".cpp" ".cc"))
      (goto '(".h")))
     ((member ext '(".h"))
      (goto '(".inl" ".cpp" ".cc" ".c")))
     ((member ext '(".inl"))
      (goto '(".cpp" ".cc" ".c")))
     ((member ext '(".py"))
      (message (format "processing with basename=%s" basename))
      (if basename_has_test_suffix
          (goto-fullname (concat basename_without_test_suffix ext))
        (goto-fullname (concat basename "_test.py")))
      )
     )
    )
  )

(defun find-other-file()
 (interactive "")
 (if (and (buffer-file-name)
          (file-exists-p (buffer-file-name)))
     (let ((target (other-file (buffer-file-name))))
       (if target
           (find-file target)
         (message "No match found")))
   (message "Not a file")))
(defun find-other-file-other-window()
 (interactive "")
 (if (and (buffer-file-name)
          (file-exists-p (buffer-file-name)))
     (let ((target (other-file (buffer-file-name))))
       (if target
           (find-file-other-window target)
         (message "No match found")))
   (message "Not a file")))
(defun find-other-file()
 (interactive "")
 (if (and (buffer-file-name)
          (file-exists-p (buffer-file-name)))
     (let ((target (other-file (buffer-file-name))))
       (if target
           (find-file target)
         (message "No match found")))
   (message "Not a file")))
(global-set-key (kbd "M-o") 'find-other-file)
(global-set-key (kbd "M-i") 'find-other-file-other-window)

(global-set-key (kbd "M-n")
 (lambda ()
  (interactive "")
  (bury-buffer (current-buffer))
  (switch-to-buffer (other-buffer (current-buffer) 1)) 1))

(global-set-key (kbd "C-M-o")
                (lambda ()
                  (interactive "")
                  (view-buffer-other-window "*compilation*")
                  ))

(global-set-key (kbd "<C-s-268632079>")
                (lambda ()
                  (interactive "")
                  (view-buffer-other-window "*compilation*")
                  ))

(defun revert-all-buffers ()
  (interactive "")
   (mapcar (lambda (b)
             (with-current-buffer b
                   (when (buffer-file-name)
                     (message (format "Reverting %s" (buffer-file-name)))
                     (revert-buffer 1 1)
                     )))
           (buffer-list))
   (message "REVERT ALL THE BUFFERS!!!!"))

;; Shiftingx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; Compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-file-contents (f)
  (with-temp-buffer
    (insert-file-contents f)
    (replace-string "\n" "")
    (buffer-substring (point-min) (point-max))))

;; Get the m1 mark and add a trailing slash if needed
(defun get_g1_dir()
  (let ((g1 (read-file-contents (expand-file-name "~/.markutils/m1"))))
    (if (string= (substring g1 -1) "/")
        g1
      (concat g1 "/"))))

(defun get_g1_make_type()
  (read-file-contents (expand-file-name "~/.markutils/g1_make_type")))

;; This function is a bit gross because my "make it make" scripts are split
;; across elisp and shell scripts. Basically, this function duplicates the logic
;; that do_g1_make performs in order to figure out what directory do_g1_make
;; will make from. Its usually get_g1_dir, but with ninja, it changes a bit.
(defun get_g1_make_dir()
  (let ((md (get_g1_dir))
        (mt (get_g1_make_type)))
    (if (file-exists-p (concat md "out/" mt "/build.ninja"))
        (concat md "out/" mt "/")
      md)))

(defun previous-error-and-center()
  (interactive "")
  (let ((old_cd default-directory))
    (previous-error)
    ))
(defun next-error-and-center()
  (interactive "")
  (let ((old_cd default-directory))
    (message (format "Old cd %s" old_cd))
    (next-error)
    ))

(defun save-and-compile()
  (interactive "")
  (save-buffer 0)
  (with-temp-buffer
    (let ((old_cd default-directory))
      (setq default-directory (get_g1_make_dir))
      (compile "/bin/bash -l -c \"do_g1_make\"")
    )))


(global-set-key "\C-c\C-b" 'previous-error-and-center)
(global-set-key "\C-c\C-f" 'next-error-and-center)
(global-set-key "\C-c\C-v" 'save-and-compile)


; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [f4] 'kill-this-buffer)
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
(global-set-key [f9] 'shrink-window-horizontally)
(global-set-key [f10] 'enlarge-window-horizontally)
(global-set-key [f11] 'enlarge-window)
(global-set-key [f12] 'shrink-window)


(global-set-key [C-tab] 'other-window)
(global-set-key (kbd "s-]") 'other-window)
(global-set-key (kbd "s-[")
                (lambda ()
                  (interactive "")
                  (other-window -1)))

(global-set-key "\C-s" 'isearch-forward-regexp)

(global-unset-key "\C-r")
(global-set-key "\C-r\C-e" 'replace-string)
(global-set-key "\C-r\C-q" 'query-replace)
(global-set-key "\C-r\C-r" 'replace-regexp)


; Look n feel
(set-frame-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")

(if (fboundp 'window-system)
    (progn
      (set-background-color "black")
      (set-foreground-color "white")
      (set-cursor-color "white")
      ))

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

(if (fboundp 'window-system)
    (progn
      (global-hl-line-mode 1) ; show current line
      (set-face-background 'hl-line "#0F0F0F")
      ))

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


(global-set-key [C-S-left] 'shift-left)
(global-set-key [C-S-right] 'shift-right)

;;  if possible...
(when (locate-library "goog")
  (load-library "goog")
  )

(when (locate-library "column-marker")
  (load-library "column-marker")
  )

(require 'quickopen)
(require 'nyan-mode)
(scroll-bar-mode nil)
