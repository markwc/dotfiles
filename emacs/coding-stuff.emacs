;;====================================================================
;; coding stuff
;;====================================================================

;;=== I hate tabs!
(setq-default indent-tabs-mode nil)

;;=== Treat *.h as C++ files.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;=== Auto complete.

(require 'company)

;;=== Indent argist by indent instead of lining up with open paren.

(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '+))
(add-hook 'c++-mode-common-hook 'my-indent-setup)
(add-hook 'after-init-hook 'global-company-mode)
;;(add-hook 'c++-mode-common-hook (lambda () (company-mode)))

;;=== C-mode variables and bindings:

(set-variable 'c-tab-always-indent t)
;; indent from LHS only Note: c-mode-common-hook occurs during both C
;; and C++ autoloads, c-mode-hook occurs during C autoloads only.
;; c-mode-base-map affects both C and C++ bindings, c-mode-map affects
;; C bindings only.
(add-hook 'c-mode-common-hook 'c-mode-site)
;; (add-hook 'c-mode-common-hook 'google-set-c-style)
(defun c-mode-site ()
  "Perform site-specific c-mode customization"
  ;; substatement-open: indentation of { following a control statement
  ;; (see lisp/progmodes/cc-styles.el for more options)
  (c-add-style  "gnu2"  '("gnu" (c-offsets-alist . (
						    (substatement-open . 0)
                                                    (arglist-intro . +)
                                                    (arglist-cont-nonempty . +)
						    (arglist-cont . 0)
						    (arglist-close . 0)
						    )))
                t)
  ;;=== Uncomment the following for case sensitive searches:
  ;;(define-key c-mode-base-map "\M-r" 'query-replace-case-sensitive)
  ;;(define-key c-mode-base-map "\C-s" 'isearch-forward-case-sensitive)
  ;;(define-key c-mode-base-map "\C-r" 'isearch-backward-case-sensitive)
  ;;=== Uncomment the following for NON-case sensitive searches:
  (define-key c-mode-base-map "\M-r" 'query-replace)
  (define-key c-mode-base-map "\C-s" 'isearch-forward)
  (define-key c-mode-base-map "\C-r" 'isearch-backward)
  )

