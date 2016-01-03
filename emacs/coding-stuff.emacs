;;====================================================================
;; coding stuff
;;====================================================================

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))

(load "clang-format.el")

(require 'tags-view)

;;====================================================================
;; Auto complete.
;;====================================================================

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;;====================================================================
;; Git stuff.
;;====================================================================

(require 'git)
(require 'git-blame)

;;====================================================================
;; Tags.
;;====================================================================

(autoload 'gtags-mode "gtags" "" t)
(eval-after-load "gtags"
  '(progn
     (define-key gtags-mode-map (kbd "<f9>") 'gtags-find-pattern)
     (define-key gtags-mode-map (kbd "<S-f9>") 'gtags-find-with-grep)
     (define-key gtags-mode-map (kbd "<f10>") 'gtags-find-tag)
     (define-key gtags-mode-map (kbd "<S-f10>") 'gtags-find-rtag)
     (define-key gtags-mode-map (kbd "<f11>") 'gtags-find-symbol)
     (define-key gtags-mode-map (kbd "<S-f11>") 'gtags-pop-stack)
     (define-key gtags-mode-map (kbd "<f12>") 'speedbar)))
(defun mwc/turn-on-gtags ()
  "Turn `gtags-mode' on if a global tags file has been generated.
This function asynchronously runs 'global -u' to update global
tags. When the command successfully returns, `gtags-mode' is
turned on."
  (interactive)
  (let ((process (start-process "global -u"
                                "*global output*"
                                "global" "-u"))
        (buffer  (current-buffer)))
    (set-process-sentinel
     process
     `(lambda (process event)
        (when (and (eq (process-status process) 'exit)
                   (eq (process-exit-status process) 0))
          (with-current-buffer ,buffer
            (message "Activating gtags-mode")
            (gtags-mode 1)))))))

(add-hook 'c-mode-common-hook 'mwc/turn-on-gtags)

;;====================================================================
;; OpenCL code.
;;====================================================================

(setq auto-mode-alist (cons '("\.cl$" . c-mode) auto-mode-alist))

;;====================================================================
;; Indent argist by indent instead of lining up with open paren.
;;====================================================================

(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '+))
(add-hook 'c++-mode-common-hook 'my-indent-setup)
(add-hook 'after-init-hook 'global-company-mode)

;;====================================================================
;; Python stuff.
;;====================================================================

(load-library "python")

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(setq interpreter-mode-alist
      (cons '("python" . python-mode)
            interpreter-mode-alist)
      python-mode-hook
      '(lambda () (progn
                    (set-variable 'py-indent-offset 4)
                    (set-variable 'indent-tabs-mode nil))))

;; Highlight character at "fill-column" position.
(require 'column-marker)
(set-face-background 'column-marker-1 "red")
(add-hook 'python-mode-hook
          (lambda () (interactive)
            (column-marker-1 fill-column)))

; Setup for Flymake code checking.
(require 'flymake)
(load-library "flymake-cursor")

;; Script that flymake uses to check code. This script must be
;; present in the system path.
(setq pycodechecker "pychecker")

(when (load "flymake" t)
  (defun flymake-pycodecheck-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list pycodechecker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pycodecheck-init)))

(add-hook 'python-mode-hook 'flymake-mode)

;; Remove trailing whitespace manually by typing C-t C-w.
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-t C-w")
                           'delete-trailing-whitespace)))

;; Automatically remove trailing whitespace when file is saved.
(add-hook 'python-mode-hook
      (lambda()
        (add-hook 'local-write-file-hooks
              '(lambda()
                 (save-excursion
                   (delete-trailing-whitespace))))))

;; Use M-SPC (use ALT key) to make sure that words are separated by
;; just one space. Use C-x C-o to collapse a set of empty lines
;; around the cursor to one empty line. Useful for deleting all but
;; one blank line at end of file. To do this go to end of file (M->)
;; and type C-x C-o.

;;====================================================================
;; C-mode variables and bindings:
;;====================================================================

(set-variable 'c-tab-always-indent t)
;; indent from LHS only Note: c-mode-common-hook occurs during both C
;; and C++ autoloads, c-mode-hook occurs during C autoloads only.
;; c-mode-base-map affects both C and C++ bindings, c-mode-map affects
;; C bindings only.
(add-hook 'c-mode-common-hook 'c-mode-site)
(defun c-mode-site ()
  "Perform site-specific c-mode customization"
  ;; substatement-open: indentation of { following a control statement
  ;; (see lisp/progmodes/cc-styles.el for more options)
  (c-add-style  "gnu2" '("gnu" (c-offsets-alist
                                . ((substatement-open . 0)
                                   (arglist-intro . +)
                                   (arglist-cont-nonempty . c-lineup-arglist)
                                   (arglist-cont . 0)
                                   (arglist-close . 0)
                                   (innamespace . [0])
                                   ))) t)
  (defun insert-debug-line ()
    "Insert debug line at cursor point."
    (interactive)
    (insert "std::cerr << __FUNCTION__ << std::endl;"))
  (define-key c-mode-base-map (kbd "<f1>") 'clang-format-buffer)
  (define-key c-mode-base-map (kbd "<f2>") 'clang-format-region)
  (define-key c-mode-base-map (kbd "<f5>") 'gdb)
  (define-key c-mode-base-map (kbd "<f7>") 'compile)
  (define-key c-mode-base-map (kbd "C-c C-f") 'insert-debug-line)
  (define-key c-mode-base-map (kbd "M-r") 'query-replace))

;;====================================================================
;; Debug stuff
;;====================================================================

