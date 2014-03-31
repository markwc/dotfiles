;;====================================================================
;; org-mode stuff
;;====================================================================

(require 'org-install)
(require 'org-capture)

(setq org-directory "~/.org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files (list (concat org-directory "/organizer.org")))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

(setq org-tags-column 80)

;;====================================================================
;; Global key bindings.
;;====================================================================

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)

;;====================================================================
;; TODO stuff
;;====================================================================

(setq org-log-done 'time)
(setq org-log-done 'note)

(setq org-agenda-start-on-weekday 6)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "pink" :weight bold)
              ("NEXT" :foreground "red" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-use-fast-todo-selection t)

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING" . t) ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-directory "~/.org")
(setq org-default-notes-file "~/.org/organizer.org")

;;====================================================================
;; Refile stuff.
;;====================================================================

(setq org-refile-targets '((nil :maxlevel . 2)
                                ; all top-level headlines in the
                                ; current buffer are used (first) as a
                                ; refile target
                           (org-agenda-files :maxlevel . 2)))

;;====================================================================
;; Capture stuff.
;;====================================================================

;;====================================================================
;; Auto publish to HTML.        
;;====================================================================

(defun wicked/org-publish-files-maybe ()
  "Publish this file."
  (org-export-as-html-batch)
  nil)
(add-hook 'org-mode-hook  ;; (1)
          (lambda () 
            (add-hook (make-local-variable 'after-save-hook) ;; (2)
                      'wicked/org-publish-files-maybe)))

;;====================================================================
;; Only publish to HTML file if file contains #+PUBLISH keyword.
;;====================================================================

(defun wicked/org-publish-files-maybe ()
  "Publish this file if it contains the #+PUBLISH: keyword"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward 
             "^#?[ \t]*\\+\\(PUBLISH\\)"
             nil t) 
        (org-export-as-html-batch)   
        nil))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook (make-local-variable 'after-save-hook)
                      'wicked/org-publish-files-maybe)))
