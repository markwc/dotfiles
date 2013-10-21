;;====================================================================
;; org-mode stuff
;;====================================================================

;;=== Initial configuration items.

(require 'org-install)
(require 'parse-time)

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

(bind-key "C-c r" 'org-capture)
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c l" 'org-store-link)
(bind-key "C-c L" 'org-insert-link-global)
(bind-key "C-c O" 'org-open-at-point-global)
(bind-key "<f9> <f9>" 'org-agenda-list)
(bind-key "<f9> <f8>" (lambda () (interactive) (org-capture nil "r")))
(eval-after-load 'org
  '(progn
     (bind-key "C-TAB" 'org-cycle org-mode-map)
     (bind-key "C-c v" 'org-show-todo-tree org-mode-map)
     (bind-key "C-c C-r" 'org-refile org-mode-map)
     (bind-key "C-c R" 'org-reveal org-mode-map)))

(eval-after-load 'org
  '(progn
     (bind-key "C-M-w" 'append-next-kill org-mode-map)))

(use-package org-agenda
  :init (bind-key "i" 'org-agenda-clock-in org-agenda-mode-map))

;;=== Templates

(setq org-capture-templates
      '(("t" "Tasks" entry 
         (file+headline "~/.org/organizer.org" "Tasks")
         "* TODO %^{Task}    %^g
SCHEDULED: %^t
%?
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:")
        ("b" "Business task" entry
         (file+headline "~/.org/business.org" "Tasks")
         "* TODO %^{Task}
SCHEDULED: %^t
%?
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:")
        ("p" "People task" entry
         (file+headline "~/.org/people.org" "Tasks")
         "* TODO %^{Task}
SCHEDULED: %^t
%?
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:")
        ("d" "Done task" entry
         (file+headline "~/.org/organizer.org" "Tasks")
         "* DONE %^{Task}
SCHEDULED: %^t
%?")
        ("q" "Quick note" item
         (file+headline "~/.org/organizer.org" "Quick notes"))
        ("l" "Ledger entries")
        ("lm" "MBNA" plain
         (file "~/.org/ledger")
         "%(org-read-date) %^{Payee}
  Liabilities:MBNA  
  Expenses:%^{Account}  $%^{Amount}
" :immediate-finish)
        ("ln" "No Frills" plain
         (file "~/.org/ledger")
         "%(let ((org-read-date-prefer-future nil)) (org-read-date)) * No Frills
  Liabilities:MBNA  
  Assets:Wayne:Groceries  $%^{Amount}
" :immediate-finish)    
        ("lc" "Cash" plain
         (file "~/.org/ledger")
         "%(org-read-date) * %^{Payee}
  Expenses:Cash 
  Expenses:%^{Account}  %^{Amount}
")             
        ("b" "Book" entry
         (file+datetree "~/.org/books.org" "Inbox")
         "* %^{Title}  %^g
%i
*Author(s):* %^{Author} \\\\
*ISBN:* %^{ISBN}

%?

*Review on:* %^t \\
%a
%U"
         :clock-in :clock-resume)
        ("c" "Contact" entry (file "~/.org/contacts.org")
         "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:")
        ("r" "Notes" entry
         (file+datetree "~/.org/organizer.org")
         "* %?\n\n%i\n"
         :clock-in :clock-resume)))
(global-set-key (kbd "C-M-r") 'org-capture)

;;=== Refiling

(setq org-reverse-note-order t)
(setq org-refile-use-outline-path nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets
      '(("~/.org/contacts.org" . (:maxlevel . 2))
        ("~/.org/decisions.org" . (:maxlevel . 3))
        ("~/.org/business.org" . (:maxlevel . 4))
        ("~/.org/organizer.org" . (:maxlevel . 4))
        ("~/.org/outline.org" . (:maxlevel . 3))))
(setq org-blank-before-new-entry nil)
(defun my/verify-refile-target ()
  "Exclude todo keywords with a DONE state from refile targets"
  (or (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (save-excursion (org-goto-first-child)))
(setq org-refile-target-verify-function 'my/verify-refile-target)

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

;;=== Projects

(setq org-tags-exclude-from-inheritance '("PROJECT"))

;;=== Tag tasks.

(setq org-tag-alist '(("@work" . ?b) 
                      ("@home" . ?h) 
                      ("@writing" . ?w)
                      ("@errands" . ?e) 
                      ("@drawing" . ?d)
                      ("@coding" . ?c)
                      ("@phone" . ?p)
                      ("@reading" . ?r)
                      ("@computer" . ?l)
                      ("quantified" . ?q)))

;;=== Enable filtering by estimates

(setq org-global-properties
      '(("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00")))

;;=== Track time.

(setq org-clock-idle-time nil)
(setq org-log-done 'time)
(defadvice org-clock-in (after wicked activate)
  "Mark STARTED when clocked in"
  (save-excursion
    (catch 'exit
      (org-back-to-heading t)
      (if (looking-at org-outline-regexp) (goto-char (1- (match-end 0))))
      (if (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
          (org-todo "STARTED")))))

(setq org-clock-idle-time nil)

;;=== Track tasks.

(add-hook 'org-clock-in-prepare-hook
          'my-org-mode-ask-effort)

(defun my-org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))

;;=== Modifying org agenda so that I can display a subset of tasks.

(defvar sacha/org-agenda-limit-items nil "Number of items to show in agenda to-do views; nil if unlimited.")
(defadvice org-agenda-finalize-entries (around sacha activate)
  (if sacha/org-agenda-limit-items
      (progn
        (setq list (mapcar 'org-agenda-highlight-todo list))
        (if nosort
            (setq ad-return-value
                  (subseq list 0 sacha/org-agenda-limit-items))
          (when org-agenda-before-sorting-filter-function
            (setq list (delq nil (mapcar org-agenda-before-sorting-filter-function list))))
          (setq ad-return-value
                (mapconcat 'identity
                           (delq nil 
                                 (subseq
                                  (sort list 'org-entries-lessp)
                                  0
                                  sacha/org-agenda-limit-items))
                           "\n"))))
    ad-do-it))

;;=== Display projects with associated subtasks.

(defun sacha/org-agenda-project-agenda ()
  "Return the project headline and up to `sacha/org-agenda-limit-items' tasks."
  (save-excursion
    (let* ((marker (org-agenda-new-marker))
           (heading
            (org-agenda-format-item "" (org-get-heading) (org-get-category) nil))
           (org-agenda-restrict t)
           (org-agenda-restrict-begin (point))
           (org-agenda-restrict-end (org-end-of-subtree 'invisible))
           ;; Find the TODO items in this subtree
           (list (org-agenda-get-day-entries (buffer-file-name) (calendar-current-date) :todo)))
      (org-add-props heading
          (list 'face 'defaults
                'done-face 'org-agenda-done
                'undone-face 'default
                'mouse-face 'highlight
                'org-not-done-regexp org-not-done-regexp
                'org-todo-regexp org-todo-regexp
                'org-complex-heading-regexp org-complex-heading-regexp
                'help-echo
                (format "mouse-2 or RET jump to org file %s"
                        (abbreviate-file-name
                         (or (buffer-file-name (buffer-base-buffer))
                             (buffer-name (buffer-base-buffer))))))
        'org-marker marker
        'org-hd-marker marker
        'org-category (org-get-category)
        'type "tagsmatch")
      (concat heading "\n"
              (org-agenda-finalize-entries list)))))

(defun sacha/org-agenda-projects-and-tasks (match)
  "Show TODOs for all `org-agenda-files' headlines matching MATCH."
  (interactive "MString: ")
  (let ((todo-only nil))
    (if org-agenda-overriding-arguments
        (setq todo-only (car org-agenda-overriding-arguments)
              match (nth 1 org-agenda-overriding-arguments)))
    (let* ((org-tags-match-list-sublevels
            org-tags-match-list-sublevels)
           (completion-ignore-case t)
           rtn rtnall files file pos matcher
           buffer)
      (when (and (stringp match) (not (string-match "\\S-" match)))
        (setq match nil))
      (setq matcher (org-make-tags-matcher match)
            match (car matcher) matcher (cdr matcher))
      (catch 'exit
        (if org-agenda-sticky
            (setq org-agenda-buffer-name
                  (if (stringp match)
                      (format "*Org Agenda(%s:%s)*"
                              (or org-keys (or (and todo-only "M") "m")) match)
                    (format "*Org Agenda(%s)*" (or (and todo-only "M") "m")))))
        (org-agenda-prepare (concat "TAGS " match))
        (org-compile-prefix-format 'tags)
        (org-set-sorting-strategy 'tags)
        (setq org-agenda-query-string match)
        (setq org-agenda-redo-command
              (list 'org-tags-view `(quote ,todo-only)
                    (list 'if 'current-prefix-arg nil `(quote ,org-agenda-query-string))))
        (setq files (org-agenda-files nil 'ifmode)
              rtnall nil)
        (while (setq file (pop files))
          (catch 'nextfile
            (org-check-agenda-file file)
            (setq buffer (if (file-exists-p file)
                             (org-get-agenda-file-buffer file)
                           (error "No such file %s" file)))
            (if (not buffer)
                ;; If file does not exist, error message to agenda
                (setq rtn (list
                           (format "ORG-AGENDA-ERROR: No such org-file %s" file))
                      rtnall (append rtnall rtn))
              (with-current-buffer buffer
                (unless (derived-mode-p 'org-mode)
                  (error "Agenda file %s is not in `org-mode'" file))
                (save-excursion
                  (save-restriction
                    (if org-agenda-restrict
                        (narrow-to-region org-agenda-restrict-begin
                                          org-agenda-restrict-end)
                      (widen))
                    (setq rtn (org-scan-tags 'sacha/org-agenda-project-agenda matcher todo-only))
                    (setq rtnall (append rtnall rtn))))))))
        (if org-agenda-overriding-header
            (insert (org-add-props (copy-sequence org-agenda-overriding-header)
                        nil 'face 'org-agenda-structure) "\n")
          (insert "Headlines with TAGS match: ")
          (add-text-properties (point-min) (1- (point))
                               (list 'face 'org-agenda-structure
                                     'short-heading
                                     (concat "Match: " match)))
          (setq pos (point))
          (insert match "\n")
          (add-text-properties pos (1- (point)) (list 'face 'org-warning))
          (setq pos (point))
          (unless org-agenda-multi
            (insert "Press `C-u r' to search again with new search string\n"))
          (add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure)))
        (org-agenda-mark-header-line (point-min))
        (when rtnall
          (insert (mapconcat 'identity rtnall "\n") ""))
        (goto-char (point-min))
        (or org-agenda-multi (org-agenda-fit-window-to-buffer))
        (add-text-properties (point-min) (point-max)
                             `(org-agenda-type tags
                                               org-last-args (,todo-only ,match)
                                               org-redo-cmd ,org-agenda-redo-command
                                               org-series-cmd ,org-cmd))
        (org-agenda-finalize)
        (setq buffer-read-only t)))))

;;=== Org agenda custom commands.

(defvar sacha/org-agenda-contexts
  '((tags-todo "+@phone/!-SOMEDAY")
    (tags-todo "+@work/!-SOMEDAY")
    (tags-todo "+@drawing/!-SOMEDAY")
    (tags-todo "+@coding/!-SOMEDAY")
    (tags-todo "+@writing/!-SOMEDAY")
    (tags-todo "+@computer/!-SOMEDAY")
    (tags-todo "+@home/!-SOMEDAY")
    (tags-todo "+@errands/!-SOMEDAY"))
  "Usual list of contexts.")
(defun sacha/org-agenda-skip-scheduled ()
  (org-agenda-skip-entry-if 'scheduled 'deadline
                            'regexp "\n]+>"))
(defun sacha/org-agenda-with-tip (arg)
  (org-agenda-list arg)
  (let ((inhibit-read-only t))
    (insert (sacha/random-keybinding) "\n")))
(setq org-agenda-custom-commands
      `(("a" "Agenda" sacha/org-agenda-with-tip)
        ("T" todo-tree "TODO")
        ("b" todo ""
         ((org-agenda-files '("~/.org/business.org"))))
        ("o" todo ""
         ((org-agenda-files '("~/.org/organizer.org"))))
        ("t" todo ""
         ((org-agenda-prefix-format "")
          (org-agenda-cmp-user-defined 'sacha/org-sort-agenda-items-todo)
          (org-agenda-view-columns-initially t)
          ))
        ;; Weekly review
        ("w" "Weekly review" agenda "" ((org-agenda-span 7) (org-agenda-log-mode 1)))
        ("g" . "GTD Contexts")
        ("gb" "Business" todo ""  
         ((org-agenda-files '("~/.org/business.org"))
          (org-agenda-view-columns-initially t)))
        ("gc" "Coding" tags-todo "@coding" 
         ((org-agenda-view-columns-initially t)))
        ("gw" "Writing" tags-todo "@writing"
         ((org-agenda-view-columns-initially t)))
        ("gp" "Phone" tags-todo "@phone"
         ((org-agenda-view-columns-initially t)))
        ("gd" "Drawing" tags-todo "@drawing"
         ((org-agenda-view-columns-initially t)))
        ("gh" "Home" tags-todo "@home"
         ((org-agenda-view-columns-initially t)))
        ("ge" "Errands" tags-todo "@errands"
         ((org-agenda-view-columns-initially t)))
        ("0" "Top 3 by context"
         ,sacha/org-agenda-contexts
         ((org-agenda-sorting-strategy '(priority-up effort-down))
          (sacha/org-agenda-limit-items 3)))
        (")" "All by context"
         ,sacha/org-agenda-contexts
         ((org-agenda-sorting-strategy '(priority-down effort-down))
          (sacha/org-agenda-limit-items nil)))
        ("9" "Unscheduled top 3 by context"
         ,sacha/org-agenda-contexts
         ((org-agenda-skip-function 'sacha/org-agenda-skip-scheduled)
          (org-agenda-sorting-strategy '(priority-down effort-down))
          (sacha/org-agenda-limit-items 3)))
        ("(" "All unscheduled by context"
         ,sacha/org-agenda-contexts
         ((org-agenda-skip-function 'sacha/org-agenda-skip-scheduled)
          (org-agenda-sorting-strategy '(priority-down effort-down))
          ))
        ("d" "Timeline for today" ((agenda "" ))
         ((org-agenda-ndays 1)
          (org-agenda-show-log t)
          (org-agenda-log-mode-items '(clock closed))
          (org-agenda-clockreport-mode t)
          (org-agenda-entry-types '())))
        ("w" "Waiting for" todo "WAITING")
        ("u" "Unscheduled tasks" alltodo ""
         ((org-agenda-skip-function 'sacha/org-agenda-skip-scheduled)
          (org-agenda-overriding-header "Unscheduled TODO entries: ")))
        ("P" "By priority"
         ((tags-todo "+PRIORITY=\"A\"")
          (tags-todo "+PRIORITY=\"B\"")
          (tags-todo "+PRIORITY=\"\"")
          (tags-todo "+PRIORITY=\"C\""))
         ((org-agenda-prefix-format "%-10c %-10T %e ")
          (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
        ("p" tags "+PROJECT"
         ((org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))) )
        ("2" "List projects with tasks" sacha/org-agenda-projects-and-tasks
         "+PROJECT"
         ((sacha/org-agenda-limit-items 3)))))

;;=== Sorting by date and priority.

(setq org-agenda-sorting-strategy
      '((agenda time-up priority-down user-defined-up todo-state-up effort-up)
        (todo user-defined-up todo-state-up priority-down effort-down)
        (tags user-defined-up)
        (search category-keep)))
(setq org-agenda-cmp-user-defined 'sacha/org-sort-agenda-items-user-defined)    
(require 'cl)
(defun sacha/org-get-context (txt)
  "Find the context."
  (car (member-if
        (lambda (item) (string-match "@" item))
        (get-text-property 1 'tags txt))))

(defun sacha/org-compare-dates (a b)
  "Return 1 if A should go after B, -1 if B should go after A, or 0 if a = b."
  (cond
   ((and (= a 0) (= b 0)) nil)
   ((= a 0) 1)
   ((= b 0) -1)
   ((> a b) 1)
   ((< a b) -1)
   (t nil)))

(defun sacha/org-complete-cmp (a b)
  (let* ((state-a (or (get-text-property 1 'todo-state a) ""))
         (state-b (or (get-text-property 1 'todo-state b) "")))
    (or
     (if (member state-a org-done-keywords-for-agenda) 1)
     (if (member state-b org-done-keywords-for-agenda) -1))))

(defun sacha/org-date-cmp (a b)
  (let* ((sched-a (or (get-text-property 1 'org-scheduled a) 0))
         (sched-b (or (get-text-property 1 'org-scheduled b) 0))
         (deadline-a (or (get-text-property 1 'org-deadline a) 0))
         (deadline-b (or (get-text-property 1 'org-deadline b) 0)))
    (or
     (sacha/org-compare-dates
      (sacha/org-min-date sched-a deadline-a)
      (sacha/org-min-date sched-b deadline-b)))))

(defun sacha/org-min-date (a b)
  "Return the smaller of A or B, except for 0."
  (funcall (if (and (> a 0) (> b 0)) 'min 'max) a b))

(defun sacha/org-sort-agenda-items-user-defined (a b)
  ;; compare by deadline, then scheduled date; done tasks are listed at the very bottom
  (or
   (sacha/org-complete-cmp a b)
   (sacha/org-date-cmp a b)))

(defun sacha/org-context-cmp (a b)
  "Compare CONTEXT-A and CONTEXT-B."
  (let ((context-a (sacha/org-get-context a))
        (context-b (sacha/org-get-context b)))
    (cond
     ((null context-a) +1)
     ((null context-b) -1)
     ((string< context-a context-b) -1)
     ((string< context-b context-a) +1)
     (t nil))))

(defun sacha/org-sort-agenda-items-todo (a b)
  (or
   (org-cmp-time a b)
   (sacha/org-complete-cmp a b)
   (sacha/org-context-cmp a b)
   (sacha/org-date-cmp a b)
   (org-cmp-todo-state a b)
   (org-cmp-priority a b)
   (org-cmp-effort a b)))
