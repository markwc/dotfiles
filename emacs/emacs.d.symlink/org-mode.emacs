;;====================================================================
;; org-mode stuff
;;====================================================================

;;=== Initial configuration items.

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-directory "~/.org")
(setq org-default-notes-file "~/.org/organizer.org")

(setq org-modules '(org-bbdb 
                    org-contacts
                    org-gnus
                    org-drill
                    org-info
                    org-jsinfo
                    org-habit
                    org-irc
                    org-mouse
                    org-annotate-file
                    org-eval
                    org-expiry
                    org-interactive-query
                    org-man
                    org-panel
                    org-screen
                    org-toc))

(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "C-c k") 'org-cut-subtree)
     (setq org-yank-adjusted-subtrees t)))

;;=== Track progress per section.

(defun wicked/org-update-checkbox-count (&optional all)
  "Update the checkbox statistics in the current section.
This will find all statistic cookies like [57%] and [6/12] and update
them with the current numbers.  With optional prefix argument ALL,
do this for the whole buffer."
  (interactive "P")
  (save-excursion
    (let* ((buffer-invisibility-spec (org-inhibit-invisibility)) 
	   (beg (condition-case nil
		    (progn (outline-back-to-heading) (point))
		  (error (point-min))))
	   (end (move-marker
		 (make-marker)
		 (progn (or (outline-get-next-sibling) ;; (1)
			    (goto-char (point-max)))
			(point))))   
	   (re "\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)")
	   (re-box
	    "^[ \t]*\\(*+\\|[-+*]\\|[0-9]+[.)]\\) +\\(\\[[- X]\\]\\)")
	   b1 e1 f1 c-on c-off lim (cstat 0))
      (when all
	(goto-char (point-min))
	(or (outline-get-next-sibling) (goto-char (point-max))) ;; (2)
	(setq beg (point) end (point-max)))
      (goto-char beg)
      (while (re-search-forward re end t)
	(setq cstat (1+ cstat)
	      b1 (match-beginning 0)
	      e1 (match-end 0)
	      f1 (match-beginning 1)
	      lim (cond
		   ((org-on-heading-p)
		    (or (outline-get-next-sibling) ;; (3)
			(goto-char (point-max)))
		    (point))
		   ((org-at-item-p) (org-end-of-item) (point))
		   (t nil))
	      c-on 0 c-off 0)
	(goto-char e1)
	(when lim
	  (while (re-search-forward re-box lim t)
	    (if (member (match-string 2) '("[ ]" "[-]"))
		(setq c-off (1+ c-off))
	      (setq c-on (1+ c-on))))
	  (goto-char b1)
	  (insert (if f1
		      (format "[%d%%]" (/ (* 100 c-on)
					  (max 1 (+ c-on c-off))))
		    (format "[%d/%d]" c-on (+ c-on c-off))))
	  (and (looking-at "\\[.*?\\]")
	       (replace-match ""))))
      (when (interactive-p)
	(message "Checkbox statistics updated %s (%d places)"
		 (if all "in entire file" "in current outline entry")
		 cstat)))))
(defadvice org-update-checkbox-count (around wicked activate)
  "Fix the built-in checkbox count to understand headlines."
  (setq ad-return-value
	(wicked/org-update-checkbox-count (ad-get-arg 1))))

;;=== Auto publish to HTML.        

(defun wicked/org-publish-files-maybe ()
  "Publish this file."
  (org-export-as-html-batch)
  nil)
(add-hook 'org-mode-hook  ;; (1)
 (lambda () 
  (add-hook (make-local-variable 'after-save-hook) ;; (2)
            'wicked/org-publish-files-maybe)))

;;=== Only publish to HTML file if file contains #+PUBLISH keyword.

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

;;=== agenda stuff

(setq org-agenda-files '("~/.org/organizer.org"
                         "~/.org/people.org"
                         "~/.org/business.org"
                         "~/.org/routines.org"))

(setq org-agenda-span 2)
(setq org-agenda-show-log t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-time-grid
      '((daily today require-timed)
       "----------------"
       (800 1000 1200 1400 1600 1800)))
(setq org-columns-default-format "%50ITEM %12SCHEDULED %TODO %3PRIORITY %Effort{:} %TAGS")

;;=== TODO keywords.

(setq org-todo-keywords
 '((sequence
    "TODO(t)"  ; next action
    "TOBLOG(b)"  ; next action
    "STARTED(s)"
    "WAITING(w@/!)"
    "POSTPONED(p)" "SOMEDAY(s@/!)" "|" "DONE(x!)" "CANCELLED(c@)")
   (sequence "TODELEGATE(-)" "DELEGATED(d)" "COMPLETE(x)")))
