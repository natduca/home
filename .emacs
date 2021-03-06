;; Make M-q behave sanely
(setq sentence-end-double-space nil)
(require 'compile)
(setq compilation-error-regexp-alist  (delete 'watcom compilation-error-regexp-alist))
;;(mapcar (lambda (x) (setq compilation-error-regexp-alist  (delete x compilation-error-regexp-alist)))
 ;;       '(watcom
  ;;        )
   ;;i     )

          ;; gnu
          ;; gcc-include
          ;; lcc

;; Dabbrev should treat foo_ as a single word.
(setq dabbrev-abbrev-char-regexp "\\(\\sw\\|\\s_\\)_?")

(defun nd-startswith (str substr)
  (interactive "")
  (if (>= (length str) (length substr))
      (string-equal
       (substring str 0 (length substr))
       substr
       )
    nil)
  )

;; OSX environment "fun"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Care of http://emacswiki.org/emacs/EmacsApp
(when (eq system-type 'darwin)
  (let ((os-version
         (shell-command-to-string "/usr/bin/sw_vers -productVersion")
         ))
    (when (nd-startswith os-version "10.8")
      )

  (unless (getenv "TERM_PROGRAM")
    (setenv "PATH"
            (shell-command-to-string "source $HOME/.profile && printf $PATH")))
  )
  )


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
;; Turn off VC git for chrome
(when (locate-library "vc")
  (defadvice vc-registered (around nochrome-vc-registered (file))
    (message (format "nochrome-vc-registered %s" file))
    (if (string-match ".*chrome/src.*" file)
        (progn
          (message (format "Skipping VC mode for %s" file))
          (setq ad-return-value nil)
          )
      ad-do-it)
    )
  (ad-activate 'vc-registered)
  )

(defun is-fourspace-indent ()
  (when (buffer-file-name)
    (or
     (string-match "third_party/WebKit/" (buffer-file-name))
     )
    )
  )

(defun check-webkit-style()
  (interactive "")
  (let ((old_cd default-directory))
    (cd (concat (get_g1_dir) "third_party/WebKit"))
    (compile "check-webkit-style")
    (message old_cd)
    (cd old_cd)))

;; Util functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun has-gui ()
  (when (fboundp 'window-system)
    (when window-system
      1)))

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

(unless (fboundp 'filter)
  (defun filter(predicate list)
    "Return items in LIST satisfying."
    (delq nil
          (mapcar (lambda (x)
                    (and (funcall predicate x)
                         x
                         )
                    )
                  list
                  )
          )
    )
  )

; all modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-skip-threshold 1)
 '(elisp-cache-byte-compile-files t)
 '(fill-column 80)
 '(ido-enable-flex-matching t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(quickopen-curses-fullscreen nil)
 '(vc-follow-symlinks t))

(setq-default indent-tabs-mode nil)
(setq comment-empty-lines t)

(setq create-lockfiles nil) ; interlock files
(setq auto-save-default nil) ; backups

(defun all-mode-hook()
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (setq make-backup-files nil) ; backups
  (setq truncate-lines 1)
  (setq indent-tabs-mode nil)
  (local-set-key [(control r) (control v)] 'revert-buffer)
  (local-set-key [(control r) (control a)] 'mark-whole-buffer)
  (column-number-mode 1)
  (when (and
         (fboundp 'nyan-mode)
         (has-gui)
         )
    (nyan-mode 1)
    )
  (when (and
         (fboundp 'column-marker-1)
         (not (is-fourspace-indent)))
    (column-marker-1 80))
  )

; c/c++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/home/google-c-style.el")

(defun my-c-common-hook ()
  (all-mode-hook)
  (google-make-newline-indent)
  (google-set-c-style)
  (when (is-fourspace-indent)
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
  (local-unset-key "\C-c\C-f")
  )
(add-hook 'html-mode-hook 'my-html-mode-hook)

; term
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-term-mode-hook ()
  (when (fboundp 'global-linum-mode)
    (linum-mode 0) ; disable line numbers
    (set-face-background 'linum "#0F0F0F")
    )
  )
(add-hook 'term-mode-hook 'my-term-mode-hook)

; lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-lisp-mode-hook ()
  (all-mode-hook)
  (local-set-key "\C-c\C-d" 'eval-defun)
  (local-set-key "\C-c\C-r" (lambda ()
                              (interactive "")
                              (eval-region (region-beginning) (region-end))
                              (message "Region evaluated")
                              )
                 )
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
  (setq css-indent-offset 2)
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
  (local-unset-key "\C-c\C-f")
  )
(add-hook 'js2-mode-hook 'my-js2-mode-hook)

; javascript (non-js2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun open-line-and-indent (n)
  "Insert a newline, indent it, and leave point indented before it."
  (interactive "*p")
  (open-line n)
  (indent-according-to-mode)
  )

(defun my-js-mode-hook()
  (all-mode-hook)
  (setq js-indent-level 2)
  (local-set-key "\C-o" 'open-line-and-indent)
  (local-set-key "\C-m" 'newline-and-indent)
  (local-set-key [ret] 'newline-and-indent)
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
  (local-set-key "\C-c\C-v" 'save-and-compile)
  (local-unset-key (kbd "C-c C-f"))
  (local-set-key (kbd "C-c C-r") 'save-and-recompile)

  (setq running-next-error-hook nil)
  (add-hook 'next-error-hook (lambda ()
                               (with-current-buffer "*compilation*"
                                 (unless running-next-error-hook
                                   (setq running-next-error-hook t)
                                   (unwind-protect
                                       (python-compilation-goto-last-error-in-stack)
                                     (setq running-next-error-hook nil)
                                     )
                                   )
                                 )
                               )
            t t)
  )
(add-hook 'python-mode-hook 'my-python-mode-hook)

(add-hook 'next-error-hook 'my-python-mode-hook)

(defun -python-compilation-get-current-line()
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)
   )
  )
(defun -python-compilation-is-on-stack-trace-line()
  (let ((line-text (-python-compilation-get-current-line)))
    (or
     (nd-startswith line-text "  File \"")
     )
    )
  )

(defun python-compilation-goto-last-error-in-stack ()
  (interactive "")
  (unless (-python-compilation-is-on-stack-trace-line)
    ;(message "buffer=%s line=%s" (buffer-name (current-buffer)) (-python-compilation-get-current-line))
    (error "Must start at a stack trace")
    )
  (while (-python-compilation-is-on-stack-trace-line)
    (next-line)
    (next-line)
    ;(message "Now at %s" (-python-compilation-get-current-line))
    )
  ;(message "Done, at %s" (-python-compilation-get-current-line))
  (previous-line)
  (previous-line)
  ;(message "Commit at %s" (-python-compilation-get-current-line))
  (compile-goto-error)
  (recenter-top-bottom)
  )

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
(global-set-key (kbd "M-o") (lambda ()
                              (interactive "")
                              (ff-find-other-file)
                              ))
(global-set-key (kbd "M-i") (lambda ()
                              (interactive "")
                              (ff-find-other-file t)
                              ))
(global-set-key (kbd "M-u") (lambda ()
                              (interactive "")
                              (ffap)
                              ))

(global-set-key (kbd "M-n")
 (lambda ()
  (interactive "")
  (bury-buffer (current-buffer))
  (switch-to-buffer (other-buffer (current-buffer) 1)) 1))

; reverting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun revert-all-buffers ()
  (interactive "")
   (mapcar (lambda (b)
             (with-current-buffer b
                   (when (buffer-file-name)
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
(defadvice linum-on (after compilation-after-linum-on)
  (when (string= "*compilation*" (buffer-name))
    (linum-mode 0)
    )
  )
(ad-activate 'linum-on)

(global-set-key (kbd "C-M-o")
                (lambda ()
                  (interactive "")
                  (toggle-compilation-window-layout)
                  ))

(global-set-key (kbd "A-M-o")
                (lambda ()
                  (interactive "")
                  (toggle-compilation-window-layout)
                  ))

(global-set-key (kbd "<C-s-268632079>")
                (lambda ()
                  (interactive "")
                  (toggle-compilation-window-layout)
                  ))

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
    (if (file-exists-p (concat md mt "/build.ninja"))
        (concat md mt "/")
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
(defun get-visible-compilation-window()
  (let ((frame-with-compilation
         (find-if (lambda (f)
                    (and
                     (frame-visible-p f)
                     (compilation-window-layout-active f)
                     )
                    ) (frame-list))))
    (if frame-with-compilation
        (frame-parameter frame-with-compilation 'compilation-layout-active)
      nil)
    )
  )

(defun compilation-window-layout-active(&optional frame)
  (if (frame-parameter frame 'compilation-layout-active)
      (window-live-p (frame-parameter frame 'compilation-layout-active))
    nil))

(defun compilation-window-is-visible-and-selected(&optional frame)
  ;; This currently doesn't consider whether the window is off on another
  ;; desktop. So, just assume its visible for now. You'd think
  ;;    (frame-visible-p frame)
  ;; would work, but it does't on OSX with multiple desktops. Puzzles.
  (if nil
      (eq (selected-window) (frame-parameter frame 'compilation-layout-active))
    t)
  )

(defvar compilation-window-layout-window-height 15)

(defun toggle-compilation-window-layout ()
  (if (compilation-window-layout-active)
      (if (compilation-window-is-visible-and-selected)
          (progn
            (delete-window (frame-parameter nil 'compilation-layout-active))
            (set-frame-parameter nil 'compilation-layout-active nil)
            )
        (progn
          (select-window (frame-parameter nil 'compilation-layout-active))
          (goto-char (point-max))
          )
        )
    (progn
      (delete-other-windows)
      (let ((compilation-window (selected-window)))
        (let ((content-window (split-window compilation-window compilation-window-layout-window-height)))
          (set-frame-parameter nil 'compilation-layout-active compilation-window)
          (select-window compilation-window)
          (switch-to-buffer "*compilation*" t)
          (select-window content-window)
          ;; note: hook split-window-preferred-function in order to prevent
          ;; it from splitting compilation-window.
          (set-window-dedicated-p compilation-window t)
          )
        )
      )
    )
  )

(defun my-split-window-function (w)
  (message "split")
  (if (string= (buffer-name (window-buffer w)) "*compilation*")
      nil
    (split-window-sensibly w)))
(setq split-window-preferred-function 'my-split-window-function)
(setq pop-up-frames nil)
(setq display-buffer-reuse-frames nil)

(defun move-point-to-end-for-buffer (b)
  (mapcar (lambda (w)
            (with-selected-window w
              (goto-char (point-max))
              )
            )
          (get-buffer-window-list b)
          )
  )

(defmacro with-compilation-buffer (&rest body)
  `(if (get-visible-compilation-window)
       (with-selected-frame (window-frame (get-visible-compilation-window))
         (with-temp-buffer
           (prog1
               ,@body
             (with-current-buffer "*compilation*"
              (set-variable 'truncate-lines 1)
              )
             (move-point-to-end-for-buffer "*compilation*")
             )
           )
         )
     (progn
       (toggle-compilation-window-layout)
       (with-temp-buffer
         (prog1
             ,@body
           (with-current-buffer "*compilation*"
             (set-variable 'truncate-lines 1)
             )
           (move-point-to-end-for-buffer "*compilation*")
           )
         )
       )
     )
  )

(defun git-get-top-dir (dir)
  "Retrieve the top-level directory of a git tree. Returns nil on error or if
not a git repository.."
  ;; Borrowed from https://github.com/eglaysher/find-things-fast/blob/master/find-things-fast.el
  (with-temp-buffer
    (if (eq 0 (call-process "git" nil t nil "rev-parse"))
        (let ((cdup (with-output-to-string
                      (with-current-buffer standard-output
                        (cd dir)
                        (call-process "git" nil t nil
                                      "rev-parse" "--show-cdup")))))
          (expand-file-name (concat (file-name-as-directory dir)
                                    (car (split-string cdup "\n")))))
      nil)))

(defvar git-grep-initial-value nil)
(defvar git-grep-filtered-values '())
(defvar git-grep-directory-in-which-to-run-grep nil)
(defvar git-grep-directory-to-actually-grep nil)

(defun -git-grep-impl (directory-in-which-to-run-grep grep-cmd)
  (setq compilation-environment '("PAGER=cat"))
  (with-compilation-buffer
   (setq default-directory directory-in-which-to-run-grep)
   (cd directory-in-which-to-run-grep)
   (compile grep-cmd)
   )
  )

(defun -git-update-grep ()
  (let ((cmd nil)
        )
    (setq cmd (format "git grep --no-color -n -- %s"
                      (shell-quote-argument git-grep-initial-value)
                      )
          )
    (when git-grep-directory-to-actually-grep
      (setq cmd (format "%s %s" cmd git-grep-directory-to-actually-grep))
      )
    (when (> (length git-grep-filtered-values) 0)
      (setq cmd (format "%s | %s" cmd
                        (mapconcat
                         (lambda (x)
                           (format "grep -v %s" x)
                           )
                         git-grep-filtered-values
                         " | "
                         )
                        )
            )
      )
    (-git-grep-impl git-grep-directory-in-which-to-run-grep cmd)
    )
  )

(defun git-grep (grep-command)
  (interactive (list
                (read-from-minibuffer "git grep: " git-grep-initial-value)
                )
               )
  (setq git-grep-directory-in-which-to-run-grep (git-get-top-dir default-directory))
  (setq git-grep-initial-value grep-command)
  (setq git-grep-filtered-values '())
  (-git-update-grep)
  )

(defun git-change-grep-dir (grep-dir)
  (interactive (list
                (read-directory-name "grep-dir : " git-grep-directory-in-which-to-run-grep)
                )
               )
  (if (eq grep-dir "")
      (setq git-grep-directory-to-actually-grep nil)
    (setq git-grep-directory-to-actually-grep grep-dir)
    )
  (-git-update-grep)
  )

(defun git-refine-grep (grep-filter)
  (interactive (list
                (read-from-minibuffer "grep -v : " nil)
                )
               )
  (when (null git-grep-initial-value)
    (error "Must have a grep in progress")
    )

  (setq git-grep-filtered-values
        (append git-grep-filtered-values
                (list grep-filter)))
  (-git-update-grep)
  )


(defun save-all ()
  (interactive "")
  (mapcar (lambda (b)
            (with-current-buffer b
              (when (buffer-file-name)
                (save-buffer 0)
                )))
          (buffer-list))
  (message "Saved all open buffers")
  )

(defun save-and-compile ()
  (interactive "")
  (save-all)
  (with-compilation-buffer
   (setq default-directory (get_g1_make_dir))
   (when (get-buffer-process (current-buffer))
     (kill-compilation)
     )
   (compile (expand-file-name "~/home/bin/do_g1_make"))
   )
  )

(defun save-and-recompile ()
  (interactive "")
  (save-all)
  (with-compilation-buffer
   (when (get-buffer-process (current-buffer))
     (kill-compilation)
     )
   (recompile)
   )
  )


(global-set-key "\C-c\C-b" 'previous-error-and-center)
(global-set-key "\C-c\C-f" 'next-error-and-center)
(global-set-key "\C-c\C-r" 'save-and-recompile)
(global-set-key "\C-c\C-v" 'save-and-compile)
(global-set-key "\C-c\C-g" 'git-grep)
(global-set-key "\C-c\C-e" 'git-refine-grep)

;; Renames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rename (new-file-name)
  """Renames the current buffer's file."""
  (interactive "FNew file name: ")
  (unless (buffer-file-name (current-buffer))
    (error "Can not rename buffers that have no underlying file.")
    )
  (save-buffer)
  (rename-file (buffer-file-name (current-buffer))
               new-file-name)
  (kill-buffer)
  (find-file new-file-name)
  )

; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [f4] (lambda ()
                       (interactive "")
                       (kill-buffer (current-buffer))))
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

(when (has-gui)
  (set-background-color "black")
  (set-foreground-color "white")
  (set-cursor-color "white")
  (blink-cursor-mode nil)
  )

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

(when (has-gui)
  (global-hl-line-mode 1) ; show current line
  (set-face-background 'hl-line "#0F0F0F")
)

(when (fboundp 'global-linum-mode)
  (global-linum-mode 1)  ;line numbers
  (set-face-background 'linum "#0F0F0F")
  )

(when (fboundp 'set-fringe-mode)
  (setq fringe-style 'minimal) ; shrink the fringe
  (set-fringe-mode 1)
  (set-face-background 'fringe "#0F0F0F")
  )


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

(when (fboundp 'scroll-bar-mode)
  (require 'nyan-mode)
  (scroll-bar-mode 0)
  (set-variable 'nyan-bar-length 16)
  )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(defun pylint-crc()
  (interactive)
  (let ((crc_dir (format "%s/%s" (get_g1_dir) "tools/chrome_remote_control")))
    (cd crc_dir)
    (compile "~/depot_tools/pylint -f parseable chrome_remote_control -dR0923,R0201,E1101")
    )
  )

(defun run-tests-telemetry(extra_args)
  (interactive "Mextra_args: ")
  (compile (format "%s/tools/telemetry/run_tests %s" (get_g1_dir) extra_args))
  )

(defun run-tests-trace-viewer(extra_args)
  (interactive "Mextra_args: ")
  (compile (format "%s/third_party/trace-viewer/run_tests %s" (get_g1_dir) extra_args))
  )

(defun run-tests-perf(extra_args)
  (interactive "Mextra_args: ")
  (compile (format "%s/tools/perf/run_tests %s" (get_g1_dir) extra_args))
  )

(defun gjslint-telemetry()
  (interactive)
  (compile (format "%s" (expand-file-name "~/home/bin/gjslint_telemetry")))
  )

(defun gjslint-tracing()
  (interactive)
  (let ((gjslint (concat
            (read-file-contents (expand-file-name "~/.markutils/m1"))
            "/third_party/trace-viewer/trace_viewer/build/gjslint")
                 ))
    (if (file-exists-p gjslint)
        (compile gjslint)
      (error "Could not find gjslint at %s" gjslint)
      )
    )
  )


;; Yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/home/yasnippet")
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/home/snippets")
(yas-global-mode 1)
(yas-reload-all)
(defun yas-html-mode-hook ()
  (all-mode-hook)
  (local-set-key (kbd "C-i") 'indent-for-tab-command)
  )
(add-hook 'html-mode-hook 'yas-html-mode-hook)


;; Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (has-gui) ; Why yes, this is indeed ghetto.
  (server-start)
  )
