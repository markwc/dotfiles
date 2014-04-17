;;====================================================================
;; coding stuff
;;====================================================================

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))
(require 'tags-view)

;;====================================================================
;; Auto complete.
;;====================================================================

(require 'company)

;;====================================================================
;; Git stuff.
;;====================================================================

(require 'git)
(require 'git-blame)

;;====================================================================
;; ClearCase stuff.
;;====================================================================

(load "clearcase")
;; (load "vc-clearcase")
;; (load "vc-clearcase-auto")

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
  (define-key c-mode-base-map (kbd "<f5>") 'gdb)
  (define-key c-mode-base-map (kbd "<f7>") 'compile)
  (define-key c-mode-base-map (kbd "C-c C-f") 'insert-debug-line)
  (define-key c-mode-base-map (kbd "M-r") 'query-replace))

;;====================================================================
;; Debug stuff
;;====================================================================

