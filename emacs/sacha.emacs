(setq user-full-name "Sacha Chua"
      user-mail-address "sacha@sachachua.com")
(add-to-list 'load-path "~/elisp")
(add-to-list 'load-path "~/elisp/org2blog")
(add-to-list 'load-path "~/elisp/use-package")
(require 'use-package)
(require 'bind-key)
(add-to-list 'load-path "~/elisp/artbollocks-mode")
(load-file "~/.emacs.secrets")
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(starter-kit-load "org")
(defun sacha/byte-recompile ()
  (interactive)
  (byte-recompile-directory "~/.emacs.d" 0)
  (byte-recompile-directory "~/elisp" 0))
(starter-kit-load "misc-recommended")
(require 'cl)
(show-paren-mode -1)
(remove-hook 'text-mode-hook 'turn-on-flyspell)
(defun sacha/package-install (package &optional repository)
  "Install PACKAGE if it has not yet been installed.
If REPOSITORY is specified, use that."
  (unless (package-installed-p package)
    (let ((package-archives (if repository
                                (list (assoc repository package-archives))
                              package-archives)))
      (package-install package))))
(defun sacha/org-share-emacs ()
  "Share my Emacs configuration."
  (interactive)
  (let* ((destination-dir "~/Dropbox/Public/")
         (destination-filename "sacha-emacs.org"))
    (save-restriction
      (save-excursion
        (widen)
        (write-region (point-min) (point-max) (expand-file-name destination-filename destination-dir))
        (with-current-buffer (find-file-noselect (expand-file-name
                                                  destination-filename destination-dir))
          (org-babel-tangle-file buffer-file-name 
                                 (expand-file-name
                                  "sacha-emacs.el" destination-dir) "emacs-lisp")
          (org-export-as-html nil)
          )))))
(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))
(use-package winner
  :config (winner-mode 1))
(use-package multiple-cursors
  :bind 
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-*" . mc/mark-all-like-this)))
(use-package all)
(use-package edit-list
  :commands edit-list)
(setq sentence-end-double-space nil)
(use-package helm
  :init
  (progn 
    (require 'helm-config) 
    (setq helm-input-idle-delay 0.1) ;; I want it now!
    (setq helm-candidate-number-limit 10)
    (helm-mode))
  :bind (("C-c h" . helm-mini)))
(ido-mode -1)
(use-package windmove
  :bind
  (("<f2> <right>" . windmove-right)
   ("<f2> <left>" . windmove-left)
   ("<f2> <up>" . windmove-up)
   ("<f2> <down>" . windmove-down)))
(global-set-key (kbd "RET") 'newline-and-indent)
(defun sacha/search-word-backward ()
  "Find the previous occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-backward "w_")
    (goto-char
     (if (re-search-backward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))
(defun sacha/search-word-forward ()
  "Find the next occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-forward "w_")
    (goto-char
     (if (re-search-forward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))
(global-set-key '[M-up] 'sacha/search-word-backward)
(global-set-key '[M-down] 'sacha/search-word-forward)
(defadvice search-for-keyword (around sacha activate)
  "Match in a case-insensitive way."
  (let ((case-fold-search t))
    ad-do-it))
(setq tramp-default-method "ssh")
(use-package miniedit
  :commands minibuffer-edit
  :init
  (progn
    (bind-key "\M-\C-e" 'miniedit minibuffer-local-map)
    (bind-key "\M-\C-e" 'miniedit minibuffer-local-ns-map)
    (bind-key "\M-\C-e" 'miniedit minibuffer-local-completion-map)
    (bind-key "\M-\C-e" 'miniedit minibuffer-local-must-match-map)))
(fset 'yes-or-no-p 'y-or-n-p)
(defadvice color-theme-alist (around sacha activate)
  (if (ad-get-arg 0)
      ad-do-it
    nil))
(sacha/package-install 'color-theme)
(defun sacha/setup-color-theme ()
  (interactive)
  (color-theme-hober)
  (set-face-foreground 'secondary-selection "darkblue")
  (set-face-background 'secondary-selection "lightblue")
  (set-face-background 'font-lock-doc-face "black")
  (set-face-foreground 'font-lock-doc-face "wheat")
  (set-face-background 'font-lock-string-face "black"))
(use-package color-theme
  :init
  (sacha/setup-color-theme))
(dolist
    (r `((?i (file . ,(expand-file-name (concat user-login-name ".org") starter-kit-dir)))
         (?o (file . "~/personal/organizer.org"))
         (?b (file . "~/personal/business.org"))
         ))
  (set-register (car r) (cadr r)))
(sacha/package-install 'browse-kill-ring)
(use-package browse-kill-ring
  :init 
  (progn 
    (browse-kill-ring-default-keybindings) ;; M-y
    (setq browse-kill-ring-quit-action 'save-and-restore)))
(use-package key-chord
  :init
  (progn 
    (key-chord-mode 1)
    (key-chord-define-global "cg"     'undo)
    (key-chord-define-global "yp"     'other-window)))
(use-package keyfreq
  :init
  (progn
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1)))
(use-package undo-tree
  :init (global-undo-tree-mode))
(defun ido-sort-mtime ()
  (setq ido-temp-list
        (sort ido-temp-list 
              (lambda (a b)
                (let ((ta (nth 5 (file-attributes (concat ido-current-directory a))))
                      (tb (nth 5 (file-attributes (concat ido-current-directory b)))))
                  (if (= (or (nth 0 ta) 0) (or (nth 0 tb) 0))
                      (> (or (nth 1 ta) 0) (or (nth 1 tb)))
                    (> (or (nth 0 ta) 0) (or (nth 0 tb) 0)))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (if (string-equal (substring x 0 1) ".") x))
              ido-temp-list))))
(setq tramp-default-method "plink")
(setq tramp-auto-save-directory "c:\\sacha\\tmp")
(getenv "PATH")
(global-set-key '[f15] 'execute-extended-command)
(use-package artbollocks-mode
  :init
  (progn
    (setq artbollocks-weasel-words-regex
          (concat "\\b" (regexp-opt
                         '("one of the"
                           "should"
                           "just"
                           "sort of"
                           "a lot"
                           "probably"
                           "maybe"
                           "perhaps"
                           "I think"
                           "really"
                           "pretty"
                           "nice"
                           "action"
                           "utilize"
                           "leverage") t) "\\b"))
    ;; Don't show the art critic words, or at least until I figure
    ;; out my own jargon
    (setq artbollocks-jargon nil)))
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-to-list 'load-path "~/elisp/dictionary-el")
(load-library "dictionary-init")
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
(setq org-goto-interface 'outline
      org-goto-max-level 10)
(require 'imenu)
(global-set-key (kbd "M-o") 'imenu)
(global-set-key (kbd "C-c j") 'org-clock-goto)
(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file "~/Dropbox/public/sharing/index.org")))
(setq org-cycle-include-plain-lists 'integrate)
(setq org-directory "~/personal")
(setq org-default-notes-file "~/personal/organizer.org")
(defun sacha/yank-more ()
  (interactive)
  (insert "[[")
  (yank)
  (insert "][more]]"))
(global-set-key (kbd "<f6>") 'sacha/yank-more)
(setq org-list-indent-offset 2)
(setq org-capture-templates
      '(("t" "Tasks" entry 
         (file+headline "~/personal/organizer.org" "Tasks")
         "* TODO %^{Task}    %^g
SCHEDULED: %^t
%?
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:")
        ("b" "Business task" entry
         (file+headline "~/personal/business.org" "Tasks")
         "* TODO %^{Task}
SCHEDULED: %^t
%?
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:")
        ("p" "People task" entry
         (file+headline "~/personal/people.org" "Tasks")
         "* TODO %^{Task}
SCHEDULED: %^t
%?
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:")
        ("d" "Done task" entry
         (file+headline "~/personal/organizer.org" "Tasks")
         "* DONE %^{Task}
SCHEDULED: %^t
%?")
        ("q" "Quick note" item
         (file+headline "~/personal/organizer.org" "Quick notes"))
        ("l" "Ledger entries")
        ("lm" "MBNA" plain
         (file "~/personal/ledger")
         "%(org-read-date) %^{Payee}
  Liabilities:MBNA  
  Expenses:%^{Account}  $%^{Amount}
" :immediate-finish)
        ("ln" "No Frills" plain
         (file "~/personal/ledger")
         "%(let ((org-read-date-prefer-future nil)) (org-read-date)) * No Frills
  Liabilities:MBNA  
  Assets:Wayne:Groceries  $%^{Amount}
" :immediate-finish)    
        ("lc" "Cash" plain
         (file "~/personal/ledger")
         "%(org-read-date) * %^{Payee}
  Expenses:Cash 
  Expenses:%^{Account}  %^{Amount}
")             
        ("b" "Book" entry
         (file+datetree "~/personal/books.org" "Inbox")
         "* %^{Title}  %^g
%i
*Author(s):* %^{Author} \\\\
*ISBN:* %^{ISBN}
%?
*Review on:* %^t \\
%a
%U"
         :clock-in :clock-resume)
        ("c" "Contact" entry (file "~/personal/contacts.org")
         "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:")
        ("r" "Notes" entry
         (file+datetree "~/personal/organizer.org")
         "* %?\n\n%i\n"
         :clock-in :clock-resume)))
(global-set-key (kbd "C-M-r") 'org-capture)
(setq org-reverse-note-order t)
(setq org-refile-use-outline-path nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets
      '(("~/personal/contacts.org" . (:maxlevel . 2))
        ("~/personal/decisions.org" . (:maxlevel . 3))
        ("~/personal/business.org" . (:maxlevel . 4))
        ("~/personal/organizer.org" . (:maxlevel . 4))
        ("~/personal/outline.org" . (:maxlevel . 3))))
(setq org-blank-before-new-entry nil)
(defun my/verify-refile-target ()
  "Exclude todo keywords with a DONE state from refile targets"
  (or (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (save-excursion (org-goto-first-child)))
(setq org-refile-target-verify-function 'my/verify-refile-target)
(require 'org-clock)
(defun sacha/org-entry-wpm ()
  (interactive)
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (let* ((words (count-words-region (point-min) (point-max)))
             (minutes (org-clock-sum-current-item))
             (wpm (/ words minutes)))
        (message "WPM: %d (words: %d, minutes: %d)" wpm words minutes)
        (kill-new (number-to-string wpm))))))
(setq org-todo-keywords
      '((sequence
         "TODO(t)"  ; next action
         "TOBLOG(b)"  ; next action
         "STARTED(s)"
         "WAITING(w@/!)"
         "POSTPONED(p)" "SOMEDAY(s@/!)" "|" "DONE(x!)" "CANCELLED(c@)")
        (sequence "TODELEGATE(-)" "DELEGATED(d)" "COMPLETE(x)")))
(setq org-fontify-done-headline t)
(custom-set-faces
 '(org-done ((t (:foreground "PaleGreen"   
                             :weight normal
                             :strike-through t))))
 '(org-headline-done 
   ((((class color) (min-colors 16) (background dark)) 
     (:foreground "LightSalmon" :strike-through t)))))
(setq org-tags-exclude-from-inheritance '("PROJECT"))
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
(setq org-global-properties
      '(("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00")))
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
(setq org-habit-graph-column 80)
(setq org-habit-show-habits-only-for-today nil)
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
(defun org-read-date-get-relative (s today default)
  "Check string S for special relative date string.
TODAY and DEFAULT are internal times, for today and for a default.
Return shift list (N what def-flag)
WHAT       is \"d\", \"w\", \"m\", or \"y\" for day, week, month, year.
N          is the number of WHATs to shift.
DEF-FLAG   is t when a double ++ or -- indicates shift relative to
           the DEFAULT date rather than TODAY."
  (require 'parse-time)
  (when (and
         (string-match
          (concat
           "\\`[ \t]*\\([-+]\\{0,2\\}\\)"
           "\\([0-9]+\\)?"
           "\\([hdwmy]\\|\\(" (mapconcat 'car parse-time-weekdays "\\|") "\\)\\)?"
           "\\([ \t]\\|$\\)") s)
         (or (> (match-end 1) (match-beginning 1)) (match-end 4)))
    (let* ((dir (if (> (match-end 1) (match-beginning 1))
                    (string-to-char (substring (match-string 1 s) -1))
                  ?+))
           (rel (and (match-end 1) (= 2 (- (match-end 1) (match-beginning 1)))))
           (n (if (match-end 2) (string-to-number (match-string 2 s)) 1))
           (what (if (match-end 3) (match-string 3 s) "d"))
           (wday1 (cdr (assoc (downcase what) parse-time-weekdays)))
           (date (if rel default today))
           (wday (nth 6 (decode-time date)))
           delta)
      (if wday1
          (progn
            (setq delta (mod (+ 7 (- wday1 wday)) 7))
            (if (= delta 0) (setq delta 7))
            (if (= dir ?-)
                (progn
                  (setq delta (- delta 7))
                  (if (= delta 0) (setq delta -7))))
            (if (> n 1) (setq delta (+ delta (* (1- n) (if (= dir ?-) -7 7)))))
            (list delta "d" rel))
        (list (* n (if (= dir ?-) -1 1)) what rel)))))
;; Get this from https://raw.github.com/chenfengyuan/elisp/master/next-spec-day.el
(load-file "~/elisp/next-spec-day.el")
(setq org-agenda-files '("~/personal/organizer.org"
                         "~/personal/people.org"
                         "~/personal/business.org"
                         "~/personal/routines.org"))
(setq org-agenda-span 2)
(setq org-agenda-show-log t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-time-grid
      '((daily today require-timed)
        "----------------"
        (800 1000 1200 1400 1600 1800)))
(setq org-columns-default-format "%50ITEM %12SCHEDULED %TODO %3PRIORITY %Effort{:} %TAGS")
(define-key org-agenda-mode-map "Y" 'org-agenda-todo-yesterday)
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
         ((org-agenda-files '("~/personal/business.org"))))
        ("o" todo ""
         ((org-agenda-files '("~/personal/organizer.org"))))
        ("t" todo ""
         ((org-agenda-prefix-format "")
          (org-agenda-cmp-user-defined 'sacha/org-sort-agenda-items-todo)
          (org-agenda-view-columns-initially t)
          ))
        ;; Weekly review
        ("w" "Weekly review" agenda "" ((org-agenda-span 7) (org-agenda-log-mode 1)))
        ("g" . "GTD Contexts")
        ("gb" "Business" todo ""  
         ((org-agenda-files '("~/personal/business.org"))
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
(defun sacha/org-agenda-done (&optional arg)
  "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-todo "DONE"))
;; Override the key definition for org-exit
(define-key org-agenda-mode-map "x" 'sacha/org-agenda-done)
(defun sacha/org-agenda-mark-done-and-add-followup ()
  "Mark the current TODO as done and add another task after it.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-todo "DONE")
  (org-agenda-switch-to)
  (org-capture 0 "t"))
;; Override the key definition
(define-key org-agenda-mode-map "X" 'sacha/org-agenda-mark-done-and-add-followup)
(defun sacha/org-agenda-new ()
  "Create a new note or task at the current agenda item.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))
;; New key assignment
(define-key org-agenda-mode-map "N" 'sacha/org-agenda-new)
(setq org-agenda-start-on-weekday 6)
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
(defun sacha/org-agenda-list-unscheduled (&rest ignore)
  "Create agenda view for tasks that are unscheduled and not done."
  (let* ((org-agenda-todo-ignore-with-date t)
         (org-agenda-overriding-header "List of unscheduled tasks: "))
    (org-agenda-get-todos)))
(setq org-stuck-projects
      '("+PROJECT-MAYBE-DONE"
        ("TODO")
        nil
        "\\<IGNORE\\>"))
(defun sacha/quantified-get-hours (category time-summary)
  "Return the number of hours based on the time summary."
  (if (stringp category)
      (if (assoc category time-summary) (/ (cdr (assoc category time-summary)) 3600.0) 0)
    (apply '+ (mapcar (lambda (x) (sacha/quantified-get-hours x time-summary)) category))))
(defun sacha/org-summarize-focus-areas ()
  "Summarize previous and upcoming tasks as a list."
  (interactive)
  (let ((base-date (apply 'encode-time (org-read-date-analyze "-fri" nil '(0 0 0))))
        business relationships life business-next relationships-next life-next string start end time-summary
        biz-time)
    (setq start (format-time-string "%Y-%m-%d" (days-to-time (- (time-to-number-of-days base-date) 6))))
    (setq end (format-time-string "%Y-%m-%d" (days-to-time (1+ (time-to-number-of-days base-date)))))
    (setq time-summary (quantified-summarize-time start end))
    (setq biz-time (sacha/quantified-get-hours "Business" time-summary))
    (save-window-excursion
      (org-agenda nil "w")
      (setq string (buffer-string))
      (with-temp-buffer
        (insert string)
        (goto-char (point-min))
        (while (re-search-forward "^  \\([^:]+\\): +\\(Sched[^:]+: +\\)?TODO \\(.*?\\)\\(?:[      ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$" nil t)
          (cond
           ((string= (match-string 1) "routines") nil) ; skip routine tasks
           ((string= (match-string 1) "business")
            (add-to-list 'business-next (concat "  - [ ] " (match-string 3))))
           ((string= (match-string 1) "people")
            (add-to-list 'relationships-next (concat "  - [ ] " (match-string 3))))
           (t (add-to-list 'life-next (concat "  - [ ] " (match-string 3))))))))
    (save-window-excursion
      (org-agenda nil "w")
      (org-agenda-later -1)
      (org-agenda-log-mode 16)
      (setq string (buffer-string))
      (with-temp-buffer
        (insert string)
        (goto-char (point-min))
        (while (re-search-forward "^  \\([^:]+\\): +.*?State:.*?\\(?:TODO\\|DONE\\) \\(.*?\\)\\(?:[       ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$" nil t)
          (cond
           ((string= (match-string 1) "routines") nil) ; skip routine tasks
           ((string= (match-string 1) "business")
            (add-to-list 'business (concat "  - [X] " (match-string 2))))
           ((string= (match-string 1) "people")
            (add-to-list 'relationships (concat "  - [X] " (match-string 2))))
           (t (add-to-list 'life (concat "  - [X] " (match-string 2))))))))
    (setq string
          (concat
           (format "- *Business* (%.1fh - %d%%)\n" biz-time (/ biz-time 1.68))
           (mapconcat 'identity (sort business 'string<) "\n") "\n"
           (mapconcat 'identity (sort business-next 'string<) "\n")
           "\n"
           (format "  - *Earn* (%.1fh - %d%% of Business)\n"
                   (sacha/quantified-get-hours "Business - Earn" time-summary)
                   (/ (sacha/quantified-get-hours "Business - Earn" time-summary) (* 0.01 biz-time)))
           (format "  - *Build* (%.1fh - %d%% of Business)\n"
                   (sacha/quantified-get-hours "Business - Build" time-summary)
                   (/ (sacha/quantified-get-hours "Business - Build" time-summary) (* 0.01 biz-time)))
           (format "    - *Quantified Awesome* (%.1fh)\n"
                   (sacha/quantified-get-hours "Business - Build - Quantified Awesome" time-summary))
           (format "    - *Drawing* (%.1fh)\n"
                   (sacha/quantified-get-hours '("Business - Build - Drawing" "Business - Build - Book review")  time-summary))
           (format "    - *Paperwork* (%.1fh)\n"
                   (sacha/quantified-get-hours "Business - Build - Paperwork"  time-summary))
           (format "  - *Connect* (%.1fh - %d%% of Business)\n"
                   (sacha/quantified-get-hours "Business - Connect" time-summary)
                   (/ (sacha/quantified-get-hours "Business - Connect" time-summary) (* 0.01 biz-time)))
           (format "- *Relationships* (%.1fh - %d%%)\n"
                   (sacha/quantified-get-hours '("Discretionary - Social" "Discretionary - Family") time-summary)
                   (/ (sacha/quantified-get-hours '("Discretionary - Social" "Discretionary - Family") time-summary) 1.68))
           (mapconcat 'identity (sort relationships 'string<) "\n") "\n"
           (mapconcat 'identity (sort relationships-next 'string<) "\n")
           "\n"
           (format "- *Discretionary - Productive* (%.1fh - %d%%)\n"
                   (sacha/quantified-get-hours "Discretionary - Productive" time-summary)
                   (/ (sacha/quantified-get-hours "Discretionary - Productive" time-summary) 1.68))
           (mapconcat 'identity (sort life 'string<) "\n") "\n"
           (mapconcat 'identity (sort life-next 'string<) "\n") "\n"
           (format "  - *Writing* (%.1fh)\n"
                   (sacha/quantified-get-hours "Discretionary - Productive - Writing" time-summary))
           (format "- *Discretionary - Play* (%.1fh - %d%%)\n"
                   (sacha/quantified-get-hours "Discretionary - Play" time-summary)
                   (/ (sacha/quantified-get-hours "Discretionary - Play" time-summary) 1.68))
           (format "- *Personal routines* (%.1fh - %d%%)\n"
                   (sacha/quantified-get-hours "Personal" time-summary)
                   (/ (sacha/quantified-get-hours "Personal" time-summary) 1.68))
           (format "- *Unpaid work* (%.1fh - %d%%)\n"
                   (sacha/quantified-get-hours "Unpaid work" time-summary)
                   (/ (sacha/quantified-get-hours "Unpaid work" time-summary) 1.68))
           (format "- *Sleep* (%.1fh - %d%% - average of %.1f per day)\n"
                   (sacha/quantified-get-hours "Sleep" time-summary)
                   (/ (sacha/quantified-get-hours "Sleep" time-summary) 1.68)
                   (/ (sacha/quantified-get-hours "Sleep" time-summary) 7)
                   )))
    (if (called-interactively-p 'any)
        (insert string)
      string)))
(defun sacha/org-add-line-item-task (task)
  (interactive "MTask: ")
  (org-insert-heading)
  (insert "[ ] " task)
  (let ((org-capture-entry '("t" "Tasks" entry
                             (file+headline "~/personal/organizer.org" "Tasks")
                             "")))
    (org-capture nil "t")
    (insert "TODO " task "\nSCHEDULED: <" (org-read-date) ">")))
(define-key org-mode-map (kbd "C-c t") 'sacha/org-add-line-item-task)
(defun sacha/org-prepare-weekly-review ()
  "Prepare weekly review template."
  (interactive)
  (let ((base-date (apply 'encode-time (org-read-date-analyze "-fri" nil '(0 0 0))))
        (org-agenda-files '("~/personal/organizer.org" "~/personal/business.org" "~/personal/people.org")))
    (insert
     (concat
      "*** Weekly review: Week ending " (format-time-string "%B %e, %Y" base-date) "  :weekly:\n"
      "*Blog posts*\n\n"
      "*Focus areas and time review*\n\n"
      (sacha/org-summarize-focus-areas)
      "\n"))))
(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "C-c k") 'org-cut-subtree)
     (setq org-yank-adjusted-subtrees t)))
(setq org-export-with-section-numbers nil)
(setq org-html-include-timestamps nil)
(require 'org-publish)
(setq org-publish-project-alist
      '(("public"
         :base-directory "c:/sacha/Dropbox/public"
         :base-extension "org"
         :publishing-directory "c:/sacha/Dropbox/public"
         :publishing-function org-publish-org-to-html
         )))
(sacha/package-install 'org2blog)
(require 'org2blog-autoloads)
(setq org-export-with-toc nil)
(setq org-export-htmlize-output-type 'css)
(setq org-export-html-style "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://sachachua.com/blog/wp-content/themes/sacha-v3/style.css\" />
<link rel=\"stylesheet\" type=\"text/css\" href=\"http://sachachua.com/org-export.css\" />")
(setq org-export-html-preamble nil)
(setq org-export-html-postamble nil)
(setq org-src-fontify-natively t)
(defun sacha/org-export-subtree-as-html-fragment ()
  (interactive)
  (org-export-region-as-html
   (org-back-to-heading)
   (org-end-of-subtree)
   t))
(setq org-structure-template-alist 
      '(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
        ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
        ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
        ("v" "#+BEGIN_VERSE\n?\n#+END_VERSE" "<verse>\n?\n</verse>")
        ("c" "#+BEGIN_COMMENT\n?\n#+END_COMMENT")
        ("l" "#+begin_src emacs-lisp\n?\n#+end_src" "<src lang=\"emacs-lisp\">\n?\n</src>")
        ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
        ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
        ("H" "#+html: " "<literal style=\"html\">?</literal>")
        ("a" "#+begin_ascii\n?\n#+end_ascii")
        ("A" "#+ascii: ")
        ("i" "#+index: ?" "#+index: ?")
        ("I" "#+include %file ?" "<include file=%file markup=\"?\">")))
(setq org-link-abbrev-alist
      '(("google" . "http://www.google.com/search?q=")
        ("gmap" . "http://maps.google.com/maps?q=%s")
        ("blog" . "http://sachachua.com/blog/p/")))
(use-package org-mobile
  :init
  (progn
    (autoload 'org-mobile-pull "org-mobile" nil t)
    (autoload 'org-mobile-push "org-mobile" nil t))
  :config
  (progn
    (setq org-mobile-directory "~/Dropbox/mobile")
    (setq org-mobile-inbox-for-pull "~/personal/mobileorg.org")
    (setq default-buffer-file-coding-system 'utf-8)
    (setq org-mobile-files '("/cygdrive/c/sacha/personal/organizer.org" "/cygdrive/c/sacha/personal/business.org" "/cygdrive/c/sacha/personal/books.org"))
    (setq org-mobile-agendas '("a"))))
(setq org-log-into-drawer "LOGBOOK")
(setq org-clock-into-drawer 1)
(setq org-use-effective-time t)
(add-to-list 'org-speed-commands-user '("x" org-todo "DONE"))
(add-to-list 'org-speed-commands-user '("y" org-todo-yesterday "DONE"))
(add-to-list 'org-speed-commands-user '("s" call-interactively 'org-schedule))
(setq org-attach-store-link-p t)
(setq tab-width 4)
                                        ;(setenv "PATH" (concat "c:/cygwin/bin;" (getenv "PATH")))
                                        ;(add-to-list 'exec-path "c:/cygwin/bin/" t) 
                                        ;(require 'cygwin-mount)
                                        ;(cygwin-mount-activate)
(add-hook 'comint-output-filter-functions
          'shell-strip-ctrl-m nil t)
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt nil t)
                                        ;(setq explicit-shell-file-name "cmdproxy.exe")
                                        ;(setq explicit-shell-file-name "bash.exe")
;; For subprocesses invoked via the shell
;; (e.g., "shell -c command")
                                        ;(setq shell-file-name explicit-shell-file-name)
(define-derived-mode drupal-mode php-mode "Drupal"
  "Major mode for Drupal source code.
\\{drupal-mode-map}"
  (setq case-fold-search t) 
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 2)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq fill-column 78)
  (c-set-offset 'arglist-cont 0)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'case-label 2)
  (c-set-offset 'arglist-close 0)
  (setq yas/buffer-local-condition 
        '(cond
          ((looking-at "\\w") nil)
          ((and
            (not (bobp))
            (or (equal "font-lock-comment-face"
                       (get-char-property (1- (point)) 'face))
                (equal "font-lock-string-face"
                       (get-char-property (1- (point)) 'face))))
           '(require-snippet-condition . force-in-comment))
          (t t))))
(define-key drupal-mode-map (kbd "TAB") 'indent-according-to-mode)
(add-hook 'drupal-mode-hook (lambda () (flymake-mode 1)))
(add-hook 'drupal-mode-hook (lambda () (yas/minor-mode 1)))
(add-to-list 'auto-mode-alist '("\\.\\(php\\|test\\|module\\|inc\\|install\\|engine\\|profile\\|.theme\\)$" . drupal-mode))
(add-to-list 'auto-mode-alist '("\\.tpl.php$" . html-helper-mode))
(define-key drupal-mode-map '[M-S-up] 'flymake-goto-prev-error)
(define-key drupal-mode-map '[M-S-down] 'flymake-goto-next-error)
(define-key drupal-mode-map (kbd "C-c C-c") 'comment-dwim)
(defun sacha/drupal-module-name ()
  "Return the Drupal module name for .module and .install files."    (file-name-sans-extension (file-name-nondirectory
                                                                                                (buffer-file-name))))
(add-to-list 'hs-special-modes-alist '(drupal-mode "{" "}" "/[*/]" nil hs-c-like-adjust-block-beginning))
(sacha/package-install 'expand-region)
(use-package expand-region
  :bind ("C-=" . er/expand-region))
(defun sacha/recursive-find-file (file &optional directory)
  "Find the first FILE in DIRECTORY or its parents."
  (setq directory (or directory (file-name-directory (buffer-file-name)) (pwd)))
  (if (file-exists-p (expand-file-name file directory))
      (expand-file-name file directory)
    (unless (string= directory "/")
      (sacha/recursive-find-file file (expand-file-name ".." directory)))))
(defun sacha/find-tags ()
  "Set the TAGS file."
  (set (make-variable-buffer-local 'tags-table-list) nil)
  (set (make-variable-buffer-local 'tags-file-name) 
       (sacha/recursive-find-file "TAGS")))
(eval-after-load 'drupal-mode
  '(progn
     (add-hook 'drupal-mode-hook 'sacha/find-tags)))
(setq edebug-trace t)
(autoload 'turn-on-eldoc-mode "eldoc" nil t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
;; C-c C-v l : elint current buffer in clean environment.
;; C-c C-v L : elint current buffer by multiple emacs binaries.
;;             See `erefactor-lint-emacsen'
;; C-c C-v r : Rename symbol in current buffer.
;;             Resolve `let' binding as long as i can.
;; C-c C-v R : Rename symbol in requiring modules and current buffer.
;; C-c C-v h : Highlight current symbol in this buffer
;;             and suppress `erefacthr-highlight-mode'.
;; C-c C-v d : Dehighlight all by above command.
;; C-c C-v c : Switch prefix bunch of symbols.
;;             ex: '(hoge-var hoge-func) -> '(foo-var foo-func)
;; C-c C-v ? : Display flymake elint warnings/errors
(use-package erefactor
  :config
  (define-key emacs-lisp-mode-map "\C-c\C-v" erefactor-map))
(define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function-at-point)
(use-package yasnippet-bundle
  :init
  (progn
    (yas/initialize)
    (yas/load-directory "~/elisp/snippets")
    (setq yas/key-syntaxes '("w_" "w_." "^ "))))
;;        (global-set-key (kbd "C-c y") (lambda () (interactive)
;;                                         (yas/load-directory "~/elisp/snippets")))
(column-number-mode 1)
(setq vc-diff-switches '("-b" "-B" "-u"))
(use-package js2-mode
  :commands js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))
(defun sacha/ledger-go-to-beginning-of-entry ()
  "Move to the beginning of the current entry."
  (while (and (not (bobp))
              (eq (ledger-context-line-type (ledger-context-at-point))
                  'acct-transaction))
    (forward-line -1)))
(defun sacha/ledger-entry-date ()
  "Returns the date of the entry containing point or nil."
  (save-excursion
    (sacha/ledger-go-to-beginning-of-entry)
    (let ((context-info (ledger-context-other-line 0)))
      (when (eq (ledger-context-line-type context-info) 'entry)
        (goto-char (line-beginning-position))
        (if (looking-at "\\([-0-9\\./]+\\)")
            (match-string-no-properties 1))))))
(defun sacha/ledger-guess-mbna ()
  "Adds a sub-account for the dates for my credit card transactions."
  (interactive)
  (save-excursion
    (sacha/ledger-go-to-beginning-of-entry)
    (forward-line 1)
    (let ((amount 0) (date (sacha/ledger-entry-date)) month)
      (if (string-match "[0-9]+[-\\.]\\([0-9]+\\)[-\\.]\\([0-9]+\\)" date)
          (setq month (string-to-number (match-string 1 date))))
      ;; Is this a payment or a charge?
      (save-excursion
        (while (and (eq (ledger-context-line-type (ledger-context-at-point))
                        'acct-transaction)
                    (not (eobp)))
          (let ((context (ledger-context-at-point)))
            (if (ledger-context-field-value context 'amount)
                (if (string-match "MBNA" (ledger-context-field-value context 'account))
                    (setq amount (string-to-number (ledger-context-field-value context 'amount)))
                  (setq amount (- (string-to-number (ledger-context-field-value context 'amount)))))))
          (forward-line 1)))
      (save-excursion
        (while (and (eq (ledger-context-line-type (ledger-context-at-point))
                        'acct-transaction)
                    (not (eobp)))
          (let ((context (ledger-context-at-point)))
            (if (string-match "MBNA" (ledger-context-field-value context 'account))
                (if (re-search-forward "\\(MBNA\\)[ \t]*[-$\.0-9]*[ \t]*$" (line-end-position) t)
                    (replace-match
                     (concat "MBNA:"
                             (elt
                              '("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December")
                              (% (+ (if (> amount 0) 10 11) month) 12)))
                     t t nil 1))))
          (forward-line 1))))))
(use-package typing
  :init
  (autoload 'typing-of-emacs "typing" nil t)
  :config
  (progn
    (setq toe-starting-length 6)
    (setq toe-starting-time-per-word 2)
    (setq toe-max-length 20)))
(use-package erc
  :config
  (setq erc-autojoin-channels-alist '(("freenode.net"
                                       "#org-mode"
                                       "#hacklabto"
                                       "#emacs"))
        erc-server "irc.freenode.net"
        erc-nick "sachac"))
(use-package keywiz)
(defun sacha/load-keybindings ()
  "Since we don't want to have to pass through a keywiz game each time..."
  (setq keywiz-cached-commands nil)
  (do-all-symbols (sym)
    (when (and (commandp sym)
               (not (memq sym '(self-insert-command
                                digit-argument undefined))))
      (let ((keys (apply 'nconc (mapcar
                                 (lambda (key)
                                   (when (keywiz-key-press-event-p key)
                                     (list key)))
                                 (where-is-internal sym)))))
        ;;  Politically incorrect, but clearer version of the above:
        ;;    (let ((keys (delete-if-not 'keywiz-key-press-event-p
        ;;                               (where-is-internal sym))))
        (and keys
             (push (list sym keys) keywiz-cached-commands))))))
(sacha/load-keybindings)
;; Might be good to use this in org-agenda...
(defun sacha/random-keybinding ()
  "Describe a random keybinding."
  (let* ((command (keywiz-random keywiz-cached-commands))
         (doc (and command (documentation (car command)))))
    (if command
        (concat (symbol-name (car command)) " "
                "(" (mapconcat 'key-description (cadr command) ", ") ")"
                (if doc
                    (concat ": " (substring doc 0 (string-match "\n" doc)))
                  ""))
      "")))
(sacha/package-install 'ess)                
(use-package ess-site
  :init (setq inferior-R-program-name "c:/progra~1/R/R-2.15.0/bin/x64/Rterm.exe")
  :commands R)
(defun sacha/artrage-export-png (directory &optional prefix)
  "Change an Artrage script file (arscript) to export images to DIRECTORY. 
    If PREFIX is specified, use that instead of image-."
  (interactive "MPath: ")
  (unless (file-directory-p directory)
    (make-directory directory t))
  (while (re-search-forward "[0-9\\.]+s" nil t)
    (replace-match "0.000s"))
  (goto-char (point-min))
  (while (search-forward "<StrokeEvent>" nil t)
    (replace-match (concat 
                    "EvType: Command    CommandID: ExportLayer    Idx: -1    Channels: NO    Path: \""
                    directory
                    "/" (or prefix "image-")
                    ".png\"
<StrokeEvent>") t t)))
(defvar sacha/workrave-file (expand-file-name ".\\Workrave\\historystats" (getenv "AppData")))
(defun sacha/workrave-transform-statistics (&optional file)
  (interactive (list sacha/workrave-file))
  (with-current-buffer (find-file-noselect file)
    ;; D day month-1 year hour min day month-1 year hour min
    (let ((result "Date\tStart\tEnd\tClicks\tKeystrokes\n"))
      (goto-char (point-min))
      (while (re-search-forward "^D \\(.*\\)" nil t)
        (let ((dates (split-string (match-string 1))))
          (if (re-search-forward "^m \\(.*\\)" nil t)
              (let ((info (split-string (match-string 1))))
                (setq result
                      (concat result
                              (format "%d-%d-%s\t%s:%02d\t%s:%02d\t%s\t%s\n"
                                      (+ 1900 (string-to-number (elt dates 2))) ; year
                                      (1+ (string-to-number (elt dates 1))) ; month
                                      (elt dates 0) ; day
                                      (elt dates 3) ; start hour
                                      (string-to-number (elt dates 4)) ; start min
                                      (elt dates 8) ; end hour
                                      (string-to-number (elt dates 9)) ; end min
                                      (elt info 5) ; clicks
                                      (elt info 6) ; keystrokes
                                      )))))))
      (if (interactive-p)
          (kill-new result)
        result))))
(defun sacha/strip-blog-share ()
  (interactive)
  (let (base)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "<div class=\"sharedaddy sd-sharing-enabled\">.*?<div class=\"sharing-clear\"></div></div></div></div>" nil t)
        (replace-match "")))))
(use-package quantified
  :init
  (defun sacha/org-quantified-track (category)
    "Create a tracking record using CATEGORY.
Default to the current task in the agenda, the currently-clocked
entry, or the current subtree in Org."
    (interactive
     (list
      (read-string "Category: "
                   (or
                    (if (derived-mode-p 'org-agenda-mode)
                        (let* ((marker (org-get-at-bol 'org-marker))
                               (buffer (marker-buffer marker))
                               (pos (marker-position marker)))
                          (with-current-buffer buffer
                            (save-excursion
                              (save-restriction
                                (widen)
                                (goto-char pos)
                                (org-entry-get-with-inheritance "QUANTIFIED"))))))       
                    (if org-clock-marker
                        (save-excursion
                          (org-clock-goto)
                          (org-entry-get-with-inheritance "QUANTIFIED")))
                    (if (derived-mode-p 'org-mode)
                        (org-entry-get-with-inheritance "QUANTIFIED"))))))))
(defadvice face-attribute (around sacha activate)
  (if (symbolp (ad-get-arg 0))
      ad-do-it))
(defadvice ido-sort-mtime (around sacha activate)
  (setq ido-temp-list
        (sort ido-temp-list 
              (lambda (a b)
                (let ((ta (or (nth 5 (file-attributes (concat ido-current-directory a))) '(0 0)))
                      (tb (or (nth 5 (file-attributes (concat ido-current-directory b))) '(0 0))))
                  (if (= (nth 0 ta) (nth 0 tb))
                      (> (nth 1 ta) (nth 1 tb))
                    (> (nth 0 ta) (nth 0 tb)))))))
  (setq ad-return-value
        (ido-to-end  ;; move . files to end (again)
         (delq nil (mapcar
                    (lambda (x) (if (string-equal (substring x 0 1) ".") x))
                    ido-temp-list)))))
                                        ;(setq eimp-mogrify-program "c:/Program Files/ImageMagick-6.8.3-Q16/mogrify.exe")
(find-file "~/personal/organizer.org")
(require 'org-compat)
(org-agenda nil "a")
(setq delete-old-versions -1)
(setq version-control t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list" t)))
(erc :server "irc.freenode.net" :port 6667 :nick "sachac")
(defun sacha/org-send-to-bottom-of-list ()
  "Send the current line to the bottom of the list."
  (interactive)
  (beginning-of-line)
  (let ((kill-whole-line t))
    (save-excursion
      (kill-liune 1)
      (org-end-of-item-list)
      (yank))))
(defvar sacha/org-quantified-categories 
  '(("Business" 
     ("Earn" . "Business - Earn") 
     ("E1" . "Business - Earn - Consulting - E1") 
     ("Connect" . "Business - Connect") 
     ("Build" . "Business - Build"))
    ("Discretionary"
     ("Social" . "Discretionary - Social")
     ("Productive" . "Discretionary - Productive")
     ("Writing" . "Discretionary - Productive - Writing")
     ("Emacs" . "Discretionary - Productive - Emacs")
     ("Play" . "Discretionary - Play"))
    ("Personal" ;("Biking" . "Personal - Bike")
     ("Routines" . "Personal - Routines"))
    ("Sleep" nil)
    ("Unpaid work" 
     ("Commuting" . "Unpaid work - Subway")
     ("Cook" . "Unpaid work - Cook")
     ("Tidy" . "Unpaid work - Tidy up")))
  "Categories for time summary.")

(defun sacha/org-summarize-time-use (&optional start end)
  (require 'quantified)
  (interactive)
  (unless start (setq start (format-time-string "%Y-%m-%d" (days-to-time (- (time-to-number-of-days base-date) 6)))))
  (unless end (setq end (format-time-string "%Y-%m-%d" (days-to-time (1+ (time-to-number-of-days base-date))))))
  (let ((time-summary (quantified-summarize-time start end))
        (categories sacha/org-quantified-categories)
        result)
    (setq result
          (mapconcat
           (lambda (a)
             (if (assoc (car a) time-summary)
                 (concat
                  (format "- %s: %.1f hours" (car a) (/ (cdr (assoc (car a) time-summary)) 3600.0))
                  (if (cdr a)
                      (let ((detail
                             (delq nil
                                   (mapcar (lambda (b)
                                             (if (assoc (cdr b) time-summary)
                                                 (format "%s: %.1f"
                                                         (car b)
                                                         (/ (cdr (assoc (cdr b) time-summary)) 3600.0))
                                               nil))
                                           (cdr a)))))
                        (if detail
                            (concat " (" (mapconcat 'identity detail ", ") ")")
                          ""))
                    "")
                  (if (string-equal (car a) "Sleep")
                      (format " - average of %.1f hours per day" (/ (cdr (assoc (car a) time-summary)) 3600.0 7.0))
                    "")
                  "\n")))
           categories ""))
    (if (called-interactively-p)
        (insert result)
      result)))
(defun sacha/org-summarize-upcoming-week ()
  "Summarize upcoming tasks as a list."
  (interactive)
  (org-agenda nil "w")
  (let ((string (buffer-string))
        business relationships life)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward "^  \\([^:]+\\): +\\(Sched[^:]+: +\\)?TODO \\(.*?\\)\\(?:[      ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$" nil t)
        (cond
         ((string= (match-string 1) "routines") nil) ; skip routine tasks
         ((string= (match-string 1) "business")
          (add-to-list 'business (concat "  - [ ] " (match-string 3))))
         ((string= (match-string 1) "people")
          (add-to-list 'relationships (concat "  - [ ] " (match-string 3))))
         (t (add-to-list 'life (concat "  - [ ] " (match-string 3)))))))
    (setq string
          (concat
           "*Plans for next week*\n"
           "- Business\n"
           (mapconcat 'identity business "\n")
           "\n- Relationships\n"
           (mapconcat 'identity relationships "\n")
           "\n- Life\n"
           (mapconcat 'identity life "\n")))
    (if (called-interactively-p)
        (kill-new string)
      string)))
(defun sacha/org-summarize-previous-week ()
  "Summarize previously-completed tasks as a list."
  (interactive)
  (save-window-excursion
    (org-agenda nil "w")
    (org-agenda-later -1)
    (org-agenda-log-mode 16)
    (let ((string (buffer-string))
          business relationships life)
      (with-temp-buffer
        (insert string)
        (goto-char (point-min))
        (while (re-search-forward "^  \\([^:]+\\): +.*?State:.*?\\(?:TODO\\|DONE\\) \\(.*?\\)\\(?:[       ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$" nil t)
          (cond
           ((string= (match-string 1) "routines") nil) ; skip routine tasks
           ((string= (match-string 1) "business")
            (add-to-list 'business (concat "  - " (match-string 2))))
           ((string= (match-string 1) "people")
            (add-to-list 'relationships (concat "  - " (match-string 2))))
           (t (add-to-list 'life (concat "  - " (match-string 2)))))))
      (setq string
            (concat
             "*Accomplished this week*\n\n"
             "- Business\n"
             (mapconcat 'identity business "\n")
             "\n- Relationships\n"
             (mapconcat 'identity relationships "\n")
             "\n- Life\n"
             (mapconcat 'identity life "\n")))
      (if (called-interactively-p)
          (kill-new string)
        string))))
(defun sacha/animate-emacs-chat ()
  (interactive)
  (text-scale-set 6)
  (erase-buffer)
  (sit-for 3)
  (let ((list '("Emacs Chat: Sacha Chua"
                "interviewed by Bastien Guerry"
                ""
                "July 24, 2013"
                "sachachua.com/emacs-chat"))
        (approx-width 41)
        (approx-height 16)
        row)
    (setq row (/ (- approx-height (length list)) 2))
    (mapcar
     (lambda (x)
       (animate-string x
                       row
                       (/ (- approx-width (length x)) 2))
       (setq row (1+ row)))
     list)))
(use-package iedit)
(use-package paredit
  :commands paredit-mode
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1))))
(use-package smartparens
  :init 
  (progn
    (require 'smartparens-config)
    (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
;;;;;;;;;;;;;;;;;;;;;;;;
    ;; keybinding management
    (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
    (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
    (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
    (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
    (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
    (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)
    (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
    (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
    (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
    (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)
    (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
    (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)
    (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
    (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
    (define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
    (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
    (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
    (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
    (define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
    (define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)
    (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
    (define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
    (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
    (define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)
    (define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
    (define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
    (define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)
    (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
    (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)
    (define-key sp-keymap (kbd "H-t") 'sp-prefix-tag-object)
    (define-key sp-keymap (kbd "H-p") 'sp-prefix-pair-object)
    (define-key sp-keymap (kbd "H-s c") 'sp-convolute-sexp)
    (define-key sp-keymap (kbd "H-s a") 'sp-absorb-sexp)
    (define-key sp-keymap (kbd "H-s e") 'sp-emit-sexp)
    (define-key sp-keymap (kbd "H-s p") 'sp-add-to-previous-sexp)
    (define-key sp-keymap (kbd "H-s n") 'sp-add-to-next-sexp)
    (define-key sp-keymap (kbd "H-s j") 'sp-join-sexp)
    (define-key sp-keymap (kbd "H-s s") 'sp-split-sexp)
;;;;;;;;;;;;;;;;;;
    ;; pair management
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
;;; markdown-mode
    (sp-with-modes '(markdown-mode gfm-mode rst-mode)
                   (sp-local-pair "*" "*" :bind "C-*")
                   (sp-local-tag "2" "**" "**")
                   (sp-local-tag "s" "```scheme" "```")
                   (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))
;;; tex-mode latex-mode
    (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
                   (sp-local-tag "i" "1d5f8e69396c521f645375107197ea4dfbc7b792quot;<" "1d5f8e69396c521f645375107197ea4dfbc7b792quot;>"))
;;; html-mode
    (sp-with-modes '(html-mode sgml-mode)
                   (sp-local-pair "<" ">"))
;;; lisp modes
    (sp-with-modes sp--lisp-modes
                   (sp-local-pair "(" nil :bind "C-("))))
