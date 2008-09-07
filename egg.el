;;; egg -- Emacs Got Git
;;; A magit fork

;; Copyright (C) 2008  Linh Dang
;; Copyright (C) 2008  Marius Vollmer
;;
;; Egg is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Egg is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary
;;;    This is my fork of Marius's excellent magit. his work is at:
;;;    http://zagadka.vm.bytemark.co.uk/magit
;;;

(require 'cl)
(require 'electric)
(require 'ediff)

(defgroup egg nil
  "Controlling Git from Emacs."
  :prefix "egg-"
  :group 'tools)

(defface egg-header
  '((t :weight bold :inherit variable-pitch :height 1.3))
  "Face for generic headers.

Many Egg faces inherit from this one by default."
  :group 'egg)

(defface egg-text-base
  '((((class color) (background light))
     :foreground "navy" :inherit variable-pitch)
    (((class color) (background dark))
     :foreground "SteelBlue" :inherit variable-pitch)
    (t))
  "Face for description text."
  :group 'egg)

(defface egg-text-1
  '((t :inherit egg-text-base :height 1.2))
  "Face for description text."
  :group 'egg)

(defface egg-text-2
  '((t :inherit egg-text-base :height 1.3))
  "Face for description text."
  :group 'egg)

(defface egg-text-3
  '((t :inherit egg-text-base :height 1.5))
  "Face for description text."
  :group 'egg)

(defface egg-text-4
  '((t :inherit egg-text-base :height 1.8))
  "Face for description text."
  :group 'egg)

(defface egg-electrict-choice
  '((((class color) (background light))
     :foreground "Blue" :inherit egg-text-base :weight bold :height 1.2)
    (((class color) (background dark))
     :foreground "Cyan" :inherit egg-text-base  :weight bold :height 1.2)
    (t))
  "Face for description text."
  :group 'egg)


(defface egg-section-title
  '((((class color) (background light))
     :foreground "DarkGoldenrod" :inherit egg-header :height 1.1)
    (((class color) (background dark))
     :foreground "PaleGreen" :inherit egg-header :height 1.1)
    (t :weight bold))
  "Face for generic header lines.

Many Egg faces inherit from this one by default."
  :group 'egg)

(defface egg-branch
  '((((class color) (background light))
     :foreground "SkyBlue" :inherit egg-header :height 1.4)
    (((class color) (background dark))
     :foreground "Yellow" :inherit egg-header :height 1.4)
    (t :weight bold))
  "Face for the current branch."
  :group 'egg)

(defface egg-branch-mono
  '((((class color) (background light))
     :foreground "SkyBlue" :inherit bold)
    (((class color) (background dark))
     :foreground "Yellow" :inherit bold)
    (t :weight bold))
  "Face for the current branch."
  :group 'egg)

(defface egg-tag-mono
  '((((class color) (background light))
     :foreground "GoldenRod" :inherit bold)
    (((class color) (background dark))
     :foreground "SkyBlue" :inherit bold)
    (t :weight bold))
  "Face for the current branch."
  :group 'egg)

(defface egg-remote-mono
  '((((class color) (background light))
     :foreground "Orchid" :inherit bold)
    (((class color) (background dark))
     :foreground "DarkSalmon" :inherit bold)
    (t :weight bold))
  "Face for the current branch."
  :group 'egg)

(defface egg-term
  '((((class color) (background light))
     :foreground "SkyBlue" :inherit bold)
    (((class color) (background dark))
     :foreground "Yellow" :inherit bold)
    (t :weight bold))
  "Face for an important term."
  :group 'egg)

(defface egg-warning
  '((((class color) (background light))
     :foreground "Red" :inherit bold)
    (((class color) (background dark))
     :foreground "Orange" :inherit bold)
    (t :weight bold))
  "Face for a warning."
  :group 'egg)

(defface egg-diff-file-header
  '((((class color) (background light))
     :foreground "SlateBlue" :inherit egg-header)
    (((class color) (background dark))
     :foreground "LightSlateBlue" :inherit egg-header)
    (t :weight bold))
  "Face for diff file headers."
  :group 'egg)

(defface egg-unmerged-diff-file-header
  '((((class color) (background light))
     :foreground "Red" :inherit egg-diff-file-header)
    (((class color) (background dark))
     :foreground "Orange" :inherit egg-diff-file-header)
    (t :weight bold))
  "Face for unmerged diff file headers."
  :group 'egg)

(defface egg-diff-hunk-header
  '((((class color) (background light))
     :background "grey85")
    (((class color) (background dark))
     :background "grey45"))
  "Face for diff hunk headers."
  :group 'egg)

(defface egg-diff-add
  '((((class color) (background light))
     :foreground "blue1")
    (((class color) (background dark))
     :foreground "white"))
  "Face for lines in a diff that have been added."
  :group 'egg)

(defface egg-diff-none
  '((((class color) (background light))
     :foreground "grey50")
    (((class color) (background dark))
     :foreground "grey70"))
  "Face for lines in a diff that are unchanged."
  :group 'egg)

(defface egg-diff-del
  '((((class color) (background light))
     :foreground "red")
    (((class color) (background dark))
     :foreground "OrangeRed"))
  "Face for lines in a diff that have been deleted."
  :group 'egg)

(defface egg-diff-conflict
  '((((class color) (background light))
     :foreground "Blue")
    (((class color) (background dark))
     :foreground "Orange"))
  "Face for lines in a diff that have been deleted."
  :group 'egg)

(defface egg-graph
  '((((class color) (background light))
     :foreground "grey90")
    (((class color) (background dark))
     :foreground "grey30"))
  "Face for graph."
  :group 'egg)

(defcustom egg-status-buffer-init-hiding-mode nil
  "Initial hiding mode for status buffer."
  :group 'egg
  :type '(choice :tag "Initial Hiding Mode"
		 (const :tag "Show Everything" nil)
		 (const :tag "Hide Everything" t)))

(defcustom egg-commit-buffer-init-hiding-mode nil
  "Initial hiding mode for commit log buffer."
  :group 'egg
  :type '(choice :tag "Initial Hiding Mode"
		 (const :tag "Show Everything" nil)
		 (const :tag "Hide Everything" t)))

;;;========================================================
;;; simple routines
;;;========================================================
(defsubst egg-prepend (str prefix &rest other-properties)
  (setq prefix (concat prefix (substring str 0 1)))
  (setq str (apply 'propertize str other-properties))
  (put-text-property 0 1 'display prefix str)
  str)


(defun egg-cmd-to-string-1 (program args)
  "Execute PROGRAM and return its output as a string.
ARGS is a list of arguments to pass to PROGRAM."
  (let (str code)
    (setq str 
	  (with-output-to-string
	    (with-current-buffer
		standard-output
	      (setq code (apply 'call-process program nil t nil args)))))
    (if (= code 0)
	str
      nil)))

(defun egg-cmd-1 (program args)
  "Execute PROGRAM with ARGS.
The return code and the output are return as a cons."
  (let (str code)
    (setq str 
	  (with-output-to-string
	    (with-current-buffer
		standard-output
	      (setq code (apply 'call-process program nil t nil args)))))
    (cons code str)))

(defsubst egg-cmd-to-string (program &rest args)
  "Execute PROGRAM and return its output as a string.
ARGS is a list of arguments to pass to PROGRAM."
  (egg-cmd-to-string-1 program args))


(defun egg-git-to-string (&rest args)
  (let* ((str (egg-cmd-to-string-1 "git" args))
	 (len (length str)))
    (when (> len 0)
      (if (eq (aref str (1- len)) ?\n)
	  (substring str 0 -1)
	str))))

(defsubst egg-cmd-ok (program buffer &rest args)
  (= (apply 'call-process program nil buffer nil args) 0))

(defsubst egg-git-ok (buffer &rest args)
  (= (apply 'call-process "git" nil buffer nil args) 0))

(defsubst egg-git-region-ok (start end &rest args)
  (= (apply 'call-process-region start end "git" t t nil args) 0))

(defsubst egg-wdir-clean () (egg-git-ok nil "diff" "--quiet"))
(defsubst egg-file-updated (file) 
  (egg-git-ok nil "diff" "--quiet" "--" file))
(defsubst egg-file-committed (file) 
  (egg-git-ok nil "diff" "--quiet" "HEAD" "--" file))
(defsubst egg-index-empty () (egg-git-ok nil "diff" "--cached" "--quiet"))

(defsubst egg-repo-clean () (and (egg-wdir-clean) (egg-index-empty)))

(defsubst egg-git-to-lines (&rest args)
  (save-match-data
    (split-string (egg-cmd-to-string-1 "git" args) "[\n]+" t)))

(defsubst egg-file-git-name (file)
  (car (egg-git-to-lines "ls-files" "--full-name" "--" file)))

(defsubst egg-buf-git-name (&optional buf)
  (egg-file-git-name (buffer-file-name buf)))

(defsubst egg-files-git-name (files)
  (delete-duplicates 
   (apply 'egg-git-to-lines "ls-files" "--full-name" "--" files)
   :test 'string-equal))

(defun egg-unmerged-files ()
  (delete-duplicates 
   (mapcar 'car 
	   (mapcar 'last
		   (mapcar
		    'split-string
		    (egg-git-to-lines "ls-files" "--full-name" "-u"))))
   :test 'string-equal))

(defsubst egg-local-branches ()
  "Get a list of local branches. E.g. (\"master\", \"wip1\")."
  (egg-git-to-lines "rev-parse" "--symbolic" "--branches"))

(defsubst egg-local-refs ()
  "Get a list of local refs. E.g. (\"master\", \"wip1\")."
  (egg-git-to-lines "rev-parse" "--symbolic" "--branches" "--tags"))

(defun egg-remote-branches (&optional raw)
  "Get a list of remote branches. E.g. (\"origin/master\", \"joe/fork1\")."
  (let ((lst (egg-git-to-lines "rev-parse" "--symbolic" "--remotes")))
    (if raw lst
      (mapcar (lambda (full-name)
		(let ((tmp (save-match-data (split-string full-name "/"))))
		  (cons (cadr tmp) (car tmp))))
	      lst))))

(defsubst egg-rbranch-to-remote (rbranch)
  (and (stringp rbranch)
       (> (length rbranch) 0)
       (car (save-match-data (split-string rbranch "/")))))

(defsubst egg-rbranch-name (rbranch)
  (and (stringp rbranch) 
       (> (length rbranch) 0)
       (cadr (save-match-data (split-string rbranch "/")))))

(defsubst egg-push-refspec (lbranch rbranch)
   (setq rbranch (egg-rbranch-name rbranch))
   (if (or lbranch rbranch)
       (format "%s%s%s" (or lbranch "") (if rbranch ":" "") (or rbranch ""))))

(defun egg-file-as-string-raw (file-name)
  (with-temp-buffer
    (insert-file-contents-literally file-name)
    (buffer-string)))

(defun egg-pick-file-contents (file-name regexp &rest indices)
  (with-temp-buffer
    (insert-file-contents-literally file-name)
    (goto-char (point-min))
    (when (re-search-forward regexp nil t)
      (if (null indices)
	  (match-string-no-properties 0)
	(dolist (idx indices)
	  (if (match-beginning idx)
	      (return (match-string-no-properties idx))))))))

(defun egg-pick-file-records (file-name start-re end-re)
  (with-temp-buffer
    (insert-file-contents-literally file-name)
    (goto-char (point-min))
    (let ((beg (point-min)) 
	  (end (point-max))
	  lst)
      (save-match-data
	(while (and (> end beg)
		    (not (eobp))
		    (re-search-forward start-re nil t))
	  (setq beg (match-beginning 0))
	  (when (re-search-forward end-re nil t)
	    (setq end (match-beginning 0))
	    (if (> end beg)
		(setq lst (cons (buffer-substring-no-properties 
				 beg (match-beginning 0))
				lst)))
	    (goto-char end))))
      lst)))

(defun egg-file-as-string (file-name)
  (let ((str (egg-file-as-string-raw file-name)))
    (if (> (length str) 0)
	(substring str 0 -1)
      str)))

(defsubst egg-is-in-git ()
  (= (call-process "git" nil nil nil "rev-parse" "--git-dir") 0))

(defsubst egg-is-dir-in-git (dir)
  (let ((default-directory dir)) (egg-is-in-git)))

(defsubst egg-name-rev (rev)
  (egg-git-to-string "name-rev" "--always" "--name-only" rev))

(defsubst egg-sha1 (rev)
  (egg-git-to-string "rev-parse" (concat rev "~0")))

(defun egg-read-git-dir ()
  (let ((dir (egg-git-to-string "rev-parse" "--git-dir")))
    (if (stringp dir) 
	(expand-file-name dir))))

(defsubst egg-read-dir-git-dir (dir)
  (let ((default-directory dir)) (egg-read-git-dir)))

(defvar egg-git-dir nil)
(defsubst egg-git-dir ()
  (if (local-variable-p 'egg-git-dir)
      egg-git-dir
    (set (make-local-variable 'egg-git-dir) (egg-read-git-dir))
    (set (intern (concat "egg-" egg-git-dir "-HEAD")) " Egg")
    egg-git-dir))

(defsubst egg-buf-git-dir (buffer)
  (with-current-buffer buffer
    (egg-git-dir)))

(defun egg-HEAD ()
  (let* ((git-dir (egg-git-dir))) 
    (if git-dir
	(egg-pick-file-contents (concat git-dir "/HEAD")
				 "^ref: refs/heads/\\(.+\\)\\|^\\([0-9a-z]+\\)" 1 2))))

(defun egg-all-refs ()
  "Get a list of all refs."
  (append (egg-git-to-lines "rev-parse" "--symbolic"
			    "--branches" "--tags" "--remotes")
	  (delq nil
		(mapcar 
		 (lambda (head)
		   (if (file-exists-p (concat (egg-git-dir) "/" head))
		       head))
		 '("HEAD" "ORIG_HEAD" "MERGE_HEAD" "FETCH_HEAD")))))

(defun egg-sha1-ref-alist ()
  (mapcar (lambda (line)
	    (when (string-match "\\`\\(\\S-+\\) refs/\\(heads\\|tags\\|remotes\\)/\\(.+\\)\\'" line)
	      (list (match-string-no-properties 1 line)
		    (match-string-no-properties 3 line)
		    (match-string-no-properties 2 line))))
	  (egg-git-to-lines "show-ref")))

(defun egg-ref-type-alist ()
  (mapcar (lambda (line)
	    (when (string-match "\\`\\(?:\\S-+\\) refs/\\(?:\\(heads\\)\\|\\(tags\\)\\|\\(remotes\\)\\)/\\(.+\\)\\'" line)
	      (cons (match-string-no-properties 4 line)
		    (cond ((match-beginning 1) :head)
			  ((match-beginning 2) :tag)
			  ((match-beginning 3) :remote)))))
	  (egg-git-to-lines "show-ref")))

(defun egg-full-ref-decorated-alist (head-face head-keymap
					       tag-face tag-keymap
					       remote-site-face remote-rname-face remote-keymap)
  (mapcar (lambda (line)
	    (when (string-match "\\`\\(?:\\S-+\\) \\(refs/\\(?:\\(heads\\)\\|\\(tags\\)\\|\\(remotes\\)\\)/\\(\\([^/\n]+/\\)?[^/\n]+\\)\\)\\'" line)
	      (let ((full-name (match-string-no-properties 1 line))
		    (name (match-string-no-properties 5 line))
		    (remote (and (match-end 6)
				 (match-string-no-properties 6 line)))
		    (rname (and (match-end 6)
				(substring-no-properties line 
							 (match-end 6) 
							 (match-end 5))))) 
		(cond ((match-beginning 2) (cons full-name
						 (propertize name 
							     'face head-face 
							     'keymap head-keymap
							     :ref (cons name :head))))
		      ((match-beginning 3) (cons full-name
						 (propertize name 
							     'face tag-face 
							     'keymap tag-keymap
							     :ref (cons name :tag))))
		      ((match-beginning 4) (cons full-name
						 (concat
						  (propertize remote
							     'face remote-site-face
							     'keymap remote-keymap
							     :ref (cons name :remote))
						  (propertize rname
							     'face remote-rname-face
							     'keymap remote-keymap
							     :ref (cons name :remote)))))))))
	  (egg-git-to-lines "show-ref")))

(defun egg-full-ref-alist ()
  (mapcar (lambda (line)
	    (when (string-match "\\`\\(?:\\S-+\\) \\(refs/\\(?:\\(heads\\)\\|\\(tags\\)\\|\\(remotes\\)\\)/\\(.+\\)\\)\\'" line)
	      (let ((full-name (match-string-no-properties 1 line))
		    (name (match-string-no-properties 5 line)))
		(cons full-name name))))
	  (egg-git-to-lines "show-ref")))

(defun egg-decorate-ref (full-name)
  (save-match-data
    (let (name type)
      (if (not (string-match
	      "\\`refs/\\(heads\\|tags\\|remotes\\)/\\(.+\\)\\'"
	      full-name))
	full-name
	(setq name (match-string-no-properties 2 full-name)
	      type (match-string-no-properties 1 full-name))
	(cond ((string= type "heads")
	       (propertize name 'face 'egg-branch-mono
			   :ref (cons name :head)))
	      ((string= type "tags")
	       (propertize name 'face 'egg-tag-mono
			   :ref (cons name :tag)))
	      ((string= type "remotes")
	       (propertize
		(concat (propertize (file-name-directory name)
				    'face 'egg-remote-mono)
			(propertize (file-name-nondirectory name)
				    'face 'egg-branch-mono))
		:ref (cons name :remote))))))))

(defsubst egg-get-symbolic-HEAD (&optional file)
  (setq file (or file (concat (egg-git-dir) "/HEAD")))
  (egg-pick-file-contents file
			  "^ref: refs/heads/\\(.+\\)"
			  1))

(defsubst egg-get-current-sha1 ()
  (egg-git-to-string "rev-parse" "--verify" "-q" "HEAD"))

(defsubst egg-set-mode-info (state)
  (set (intern (concat "egg-" egg-git-dir "-HEAD"))
       (format " Git:%s" (cond ((plist-get state :merge-heads)
				"(merging)")
			       ((plist-get state :branch)
				(plist-get state :branch))
			       (t "(detached)")))))

(defvar egg-internal-current-state nil)
(defun egg-get-repo-state ()
  (let* ((git-dir (egg-git-dir))
	 (head-file (concat git-dir "/HEAD"))
	 (merge-file (concat git-dir "/MERGE_HEAD"))
	 (branch (egg-get-symbolic-HEAD head-file))
	 (sha1 (egg-get-current-sha1))
	 (merge-heads
	  (mapcar 'egg-name-rev 
		  (if (file-readable-p merge-file)
		      (egg-pick-file-records merge-file "^" "$"))))
	 (rebase-dir 
	  (if (file-directory-p (concat git-dir "/.dotest-merge"))
	      (concat git-dir "/.dotest-merge/")))
	 (rebase-head
	  (if rebase-dir 
	      (egg-name-rev 
	       (egg-file-as-string (concat rebase-dir "head-name")))))
	 (rebase-upstream
	  (if rebase-dir 
	      (egg-file-as-string (concat rebase-dir "onto_name"))))
	 (rebase-step
	  (if rebase-dir 
	      (egg-file-as-string (concat rebase-dir "msgnum"))))
	 (rebase-num
	  (if rebase-dir 
	      (egg-file-as-string (concat rebase-dir "end"))))
	 (state (list :gitdir git-dir
		      :branch branch :sha1 sha1 
		      :merge-heads merge-heads
		      :rebase-head rebase-head
		      :rebase-upstream rebase-upstream
		      :rebase-step rebase-step
		      :rebase-num rebase-num)))
    (egg-set-mode-info state)
    state))

(defsubst egg-repo-state ()
  (or egg-internal-current-state (egg-get-repo-state)))

(defsubst egg-current-branch (&optional state)
  (plist-get (or state (egg-repo-state)) :branch))

(defsubst egg-current-sha1 (&optional state)
  (plist-get (or state (egg-repo-state)) :sha1))

(defsubst egg-head (&optional state)
  (if (egg-git-dir)
      (let ((state (or state (egg-repo-state))))
	(cons (egg-current-sha1 state) 
	      (egg-current-branch state)))))

(defun egg-pretty-head-string (&optional state)
  (let* ((state (or state (egg-repo-state)))
	 (branch (plist-get state :branch))
	 (merge-heads (plist-get state :merge-heads))
	 (rebase-head (plist-get state :rebase-head))
	 (rebase-upstream (plist-get state :rebase-upstream))
	 (sha1 (plist-get state :sha1)))
    (cond ((and branch merge-heads)
	   (concat "Merging to " branch " from: "
		   (mapconcat 'identity merge-heads ",")))
	  (merge-heads 
	   (concat "Merging to " (egg-name-rev sha1) " from: "
		   (mapconcat 'identity merge-heads ",")))
	  ((and rebase-head rebase-upstream)
	   (format "Rebasing %s onto %s" rebase-head rebase-upstream))
	  (branch branch)
	  (t (concat "Detached HEAD: " (egg-name-rev sha1))))))


(defun egg-config-section-raw (type &optional name)
  (egg-pick-file-contents (concat (egg-git-dir) "/config")
			  (concat "^"
				  (if name (format "\\[%s \"%s\"\\]" type name)
				    (format "\\[%s\\]" type))
				  "\n"
				  "\\(\\(?:\t.+\n\\)+\\)")
			  1))

(defsubst egg-config-section (type &optional name)
  (save-match-data
    (mapcar 
     (lambda (line) 
       (split-string line "[ =]+" t))
     (split-string (or (egg-config-section-raw type name) "")
		   "[\t\n]+" t))))

(defun egg-config-get-all (file type)
  (interactive "fFilename: ")
  (save-match-data
    (mapcar (lambda (rec)
	      (let ((key (car rec))
		    (infos (cdr rec)))
		(cons (progn (string-match "\"\\(.+\\)\"" key)
			     (match-string-no-properties 1 key))
		      (mapcar (lambda (attr)
				(split-string attr "[ =]+" t))
			      infos))))
	    (mapcar (lambda (str)
		      (split-string str "[\t\n]+" t))
		    (egg-pick-file-records file
					   (concat "^\\[" type " \"")
					   "^\\[\\|\\'")))))

(defsubst egg-config-get-all-branches ()
  (egg-config-get-all (concat (egg-git-dir) "/config") "branch"))

(defsubst egg-config-get-all-remotes ()
  (egg-config-get-all (concat (egg-git-dir) "/config") "remote"))

(defsubst egg-config-get-all-remote-names ()
  (mapcar 'car (egg-config-get-all-remotes)))

(defsubst egg-config-get (type attr &optional name)
  (and (egg-git-dir)
       (cadr (assoc attr (egg-config-section type name)))))

(defun egg-bool-config (type attr &optional name)
  (let ((flag (egg-config-get type attr name)))
    (cond ((equal flag "true")
	   t)
	  ((equal flag "false")
	   nil)
	  (t (error "Unexpected contents of boolean config %s of %s.%s"
		    attr type name)))))

(defun egg-tracking-target (branch &optional mode)
  (let ((remote (egg-config-get "branch" "remote" branch))
	(rbranch (egg-config-get "branch" "merge" branch)))
    (when (stringp rbranch)
      (setq rbranch (file-name-nondirectory rbranch))
      (cond ((null mode) (concat remote "/" rbranch))
	    ((eq :name-only mode) rbranch)
	    (t (cons rbranch remote))))))

(defun egg-revs ()
  (apply 'nconc 
	 (mapcar (lambda (ref)
		   (cons ref 
			 (mapcar (lambda (suffix)
			     (concat ref suffix))
				 '("^" "^^" "^^^" "^^^^" "^^^^^"
				   "~0" "~1" "~2" "~3" "~4" "~5"))))
		 (egg-all-refs))))

(defun egg-read-rev (prompt &optional default special-rev)
  (unless (listp special-rev)
    (setq special-rev (list special-rev)))
  (completing-read prompt (nconc special-rev (egg-revs)) nil nil default))

(defun egg-read-remote (prompt &optional default)
  (completing-read prompt (egg-config-get-all-remote-names) nil t default))

;;;========================================================
;;; Async Git process
;;;========================================================

(defun egg-async-do (exit-code func-args args)
  "Run GIT asynchronously with ARGS.
if EXIT code is an exit-code from GIT other than zero but considered
success."
  (let ((dir (file-name-directory (egg-git-dir)))
	(buf (get-buffer-create "*egg-process*"))
	(inhibit-read-only inhibit-read-only)
	(accepted-msg (and (integerp exit-code)
			   (format "exited abnormally with code %d"
				   exit-code)))
	proc)
    (setq proc (get-buffer-process buf))
    (when (and (processp proc) 		;; is a process
	       (not (eq (process-status proc) 'exit)) ;; not finised
	       (= (process-exit-status proc) 0))      ;; still running
      (error "EGG: %s is already running!" (process-command proc)))
    (with-current-buffer buf
      (setq inhibit-read-only t)
      (setq default-directory dir)
      ;;(erase-buffer)
      (widen)
      (goto-char (point-max))
      (insert "EGG-GIT-CMD:\n")
      (insert (format "%S\n" args))
      (insert "EGG-GIT-OUTPUT:\n")
      (setq proc (apply 'start-process "egg-git" buf "git" args))
      (setq mode-line-process " git")
      (when (and (consp func-args) (functionp (car func-args)))
	(process-put proc :callback-func (car func-args))
	(process-put proc :callback-args (cdr func-args)))
      (when (stringp accepted-msg)
	(process-put proc :accepted-msg accepted-msg)
	(process-put proc :accepted-code exit-code))
      (process-put proc :cmds (cons "git" args))
      (set-process-sentinel proc #'egg-process-sentinel))))

(defvar egg-async-process nil)
(defvar egg-async-cmds nil)
(defvar egg-async-exit-msg nil)

(defun egg-process-sentinel (proc msg)
  (let ((exit-code (process-get proc :accepted-code))
	(accepted-msg (process-get proc :accepted-msg))
	(callback-func (process-get proc :callback-func))
	(callback-args (process-get proc :callback-args))
	(cmds (process-get proc :cmds)))
    (cond ((string= msg "finished\n")
	   (message "EGG: git finished."))
	  ((string= msg "killed\n")
	   (message "EGG: git was killed."))
	  ((string-match accepted-msg msg)
	   (message "EGG: git exited with code: %d." exit-code))
	  ((string-match "exited abnormally" msg)
	   (message "EGG: git failed."))
	  (t (message "EGG: git is weird!")))
    (with-current-buffer (process-buffer proc)
      (setq mode-line-process nil)
      (widen)
      (goto-char (point-max))
      (re-search-backward "^EGG-GIT-CMD:" nil t)
      (narrow-to-region (point) (point-max))
      (if (functionp callback-func)
	  (let ((egg-async-process proc)
		(egg-async-cmds cmds)
		(egg-async-exit-msg msg)) 
	    (apply callback-func callback-args))))))

;;;========================================================
;;; Diff/Hunk
;;;========================================================

(defconst egg-hide-show-map 
  (let ((map (make-sparse-keymap "Egg:HideShow")))
    (define-key map (kbd "h") 'egg-section-cmd-toggle-hide-show)
    (define-key map (kbd "H") 'egg-section-cmd-toggle-hide-show-children)
    map))

(defconst egg-section-map 
  (let ((map (make-sparse-keymap "Egg:Section")))
    (set-keymap-parent map egg-hide-show-map)
    (define-key map (kbd "n") 'egg-buffer-cmd-navigate-next)
    (define-key map (kbd "p") 'egg-buffer-cmd-navigate-prev)
    map))

(defconst egg-diff-section-map 
  (let ((map (make-sparse-keymap "Egg:Diff")))
    (set-keymap-parent map egg-section-map)
    (define-key map (kbd "RET") 'egg-diff-section-cmd-visit-file-other-window)
    (define-key map (kbd "f") 'egg-diff-section-cmd-visit-file)
    map))

(defconst egg-staged-diff-section-map 
  (let ((map (make-sparse-keymap "Egg:StagedDiff")))
    (set-keymap-parent map egg-diff-section-map)
    (define-key map (kbd "s") 'egg-diff-section-cmd-unstage)
    map))

(defconst egg-wdir-diff-section-map 
  (let ((map (make-sparse-keymap "Egg:WdirDiff")))
    (set-keymap-parent map egg-diff-section-map)
    (define-key map (kbd "u") 'egg-diff-section-cmd-undo)
    map))

(defconst egg-unstaged-diff-section-map 
  (let ((map (make-sparse-keymap "Egg:UnstagedDiff")))
    (set-keymap-parent map egg-wdir-diff-section-map)
    (define-key map (kbd "s") 'egg-diff-section-cmd-stage)
    map))

(defconst egg-unmerged-diff-section-map 
  (let ((map (make-sparse-keymap "Egg:UnmergedDiff")))
    (set-keymap-parent map egg-unstaged-diff-section-map)
    (define-key map (kbd "=") 'egg-diff-section-cmd-ediff3)
    map))

(defconst egg-hunk-section-map 
  (let ((map (make-sparse-keymap "Egg:Hunk")))
    (set-keymap-parent map egg-section-map)
    (define-key map (kbd "RET") 'egg-hunk-section-cmd-visit-file-other-window)
    (define-key map (kbd "f") 'egg-hunk-section-cmd-visit-file)
    map))

(defconst egg-staged-hunk-section-map 
  (let ((map (make-sparse-keymap "Egg:StagedHunk")))
    (set-keymap-parent map egg-hunk-section-map)
    (define-key map (kbd "s") 'egg-hunk-section-cmd-unstage)
    map))

(defconst egg-wdir-hunk-section-map 
  (let ((map (make-sparse-keymap "Egg:WdirHunk")))
    (set-keymap-parent map egg-hunk-section-map)
    (define-key map (kbd "u") 'egg-hunk-section-cmd-undo)
    map))

(defconst egg-unstaged-hunk-section-map 
  (let ((map (make-sparse-keymap "Egg:UnstagedHunk")))
    (set-keymap-parent map egg-wdir-hunk-section-map)
    (define-key map (kbd "s") 'egg-hunk-section-cmd-stage)
    map))

(defconst egg-unmerged-hunk-section-map 
  (let ((map (make-sparse-keymap "Egg:UnmergedHunk")))
    ;; no hunking staging in unmerged file
    (set-keymap-parent map egg-wdir-hunk-section-map)
    (define-key map (kbd "=") 'egg-diff-section-cmd-ediff3)
    map))

(defun list-tp ()
  (interactive)
  (message "tp: %S" (text-properties-at (point))))

(defun list-nav ()
  (interactive)
  (message "nav: %c:%s-%c:%s" 
	   (preceding-char)
	   (get-text-property (1- (point)) :navigation)
	   (following-char)
	   (get-text-property (point) :navigation)))

(defsubst egg-safe-search (re limit &optional no)
  (save-excursion
    (save-match-data
      (and (re-search-forward re limit t)
	   (match-beginning (or no 0))))))

(defsubst egg-safe-search-pickup (re &optional limit no)
  (save-excursion
    (save-match-data
      (and (re-search-forward re limit t)
	   (match-string-no-properties (or no 0))))))

(defsubst egg-decorate-diff-header (beg end line-beg line-end)
  (put-text-property line-beg (1+ beg)
		     'display 
		     (propertize
		      (concat "\n"
			      (buffer-substring-no-properties beg
							      (1+ beg)))
		      'face 'egg-diff-file-header))
  (put-text-property (1+ beg) end 'face 'egg-diff-file-header))

(defsubst egg-decorate-cc-diff-header (beg end line-beg line-end)
  (put-text-property line-beg (1+ beg)
		     'display 
		     (propertize
		      (concat "\n"
			      (buffer-substring-no-properties beg
							      (1+ beg)))
		      'face 'egg-unmerged-diff-file-header))
  (put-text-property (1+ beg) end 'face 'egg-unmerged-diff-file-header))

(defsubst egg-decorate-diff-index-line (beg end line-beg line-end)
  (put-text-property (1- line-beg) beg 'display "    -- ")
  (put-text-property beg end 'face 'egg-diff-none))

(defsubst egg-decorate-hunk-header (beg end line-beg line-end)
  (put-text-property beg end 'face 'egg-diff-hunk-header)
  (put-text-property end line-end 'face 'egg-diff-none))

(defun egg-compute-navigation (sect-type section beg end)
  (let ((current-nav (get-text-property beg :navigation))
	(desc (if (consp section)
		  (car section)
		section)))
    (format "%s-%s" current-nav desc)))

(defsubst egg-delimit-section (sect-type section beg end 
					  &optional inv-beg
					  keymap navigation)
  (let ((nav (cond ((functionp navigation)
		    (funcall navigation sect-type section beg end))
		   ((null navigation) beg)
		   (t navigation))))
    (put-text-property beg end :sect-type sect-type)
    (put-text-property beg end sect-type section)
    (put-text-property beg end :navigation nav)
    (when (keymapp keymap)
      (put-text-property beg end 'keymap keymap))
    (when (integer-or-marker-p inv-beg) 
      (let ((current-inv (get-text-property inv-beg 'invisible)))
	(add-to-list 'current-inv nav t)
	(put-text-property inv-beg (1- end) 'invisible current-inv)))))

(defsubst egg-make-hunk-info (name beg end diff)
  (let ((b (nth 1 diff)))
    (list name (- beg b) (- end b))))

(defsubst egg-make-diff-info (name beg end head-end)
  (let ((b (make-marker)))
    (set-marker b beg)
    (set-marker-insertion-type b t)
    (list name b (- end beg) (- head-end beg))))

(defun egg-decorate-diff-sequence (args)
  (let* ((beg		(plist-get args	:begin))
	 (end		(plist-get args	:end))
	 (diff-map 	(plist-get args	:diff-map))
	 (hunk-map 	(plist-get args	:hunk-map))
	 (cc-diff-map 	(plist-get args	:cc-diff-map))
	 (cc-hunk-map 	(plist-get args	:cc-hunk-map))
	 (conflict-map 	(plist-get args	:conflict-map))
	 (a 		(plist-get args	:src-prefix))
	 (b 		(plist-get args	:dst-prefix))

	 (diff-no	1)
	 (cc-diff-no	2)
	 (hunk-no	3)
	 (cc-hunk-no	4)
	 (src-no 	5)
	 (dst-no 	6)
	 (index-no	7)
	 (conf-beg-no	8)
	 (conf-div-no	9)
	 (conf-end-no	10)
	 (del-no 	11)
	 (add-no 	12)
	 (none-no	13)

	 (regexp
	  (concat "^\\(?:"
		  "diff --git " a ".+" b "\\(.+\\)\\|"	;1 diff header
		  "diff --cc \\(.+\\)\\|"		;2 cc-diff header
		  "\\(@@ .+@@\\).*\\|"			;3 hunk
		  "\\(@@@ .+@@@\\).*\\|"		;4 cc-hunk
		  "--- " a "\\(.+\\)\\|"		;5 src
		  "\\+\\+\\+ " b "\\(.+\\)\\|"		;6 dst
		  "index \\(.+\\)\\|"			;7 index
		  "++<<<<<<< \\(.+\\):.+\\|"		;8 conflict start
		  "\\(++=======\\)\\|"			;9 conflict div
		  "++>>>>>>> \\(.+\\):.+\\|"		;10 conflict end
		  "\\(-.*\\)\\|"			;11 del
		  "\\(\\+.*\\)\\|"			;12 add
		  "\\( .*\\)"				;13 none
		  "\\)$"))

	 (hunk-end-re "^\\(?:diff\\|@@\\)")
	 (diff-end-re "^diff ")
	 
	 sub-beg sub-end head-end m-b-0 m-e-0 m-b-x m-e-x 
	 last-diff last-cc)

    (save-match-data
      (save-excursion
	(goto-char beg)
	(while (re-search-forward regexp end t)
	  (setq sub-beg (match-beginning 0)
		m-b-0 sub-beg
		m-e-0 (match-end 0)) 
	  (cond ((match-beginning del-no) ;; del
		 (put-text-property m-b-0 m-e-0 'face 'egg-diff-del))

		((match-beginning add-no) ;; add
		 (put-text-property m-b-0 m-e-0 'face 'egg-diff-add))

		((match-beginning none-no) ;; unchanged
		 (put-text-property m-b-0 m-e-0 'face 'egg-diff-none))

		((match-beginning dst-no) ;; +++ b/file
		 (setq m-b-x (match-beginning dst-no)
		       m-e-x (match-end dst-no))
		 (put-text-property m-b-0 m-b-x 'face 'egg-diff-add)
		 (put-text-property m-b-x m-e-x 'face 'egg-diff-none))

		((match-beginning src-no) ;; --- a/file
		 (setq m-b-x (match-beginning src-no)
		       m-e-x (match-end src-no))
		 (put-text-property m-b-0 m-b-x 'face 'egg-diff-del)
		 (put-text-property m-b-x m-e-x 'face 'egg-diff-none))

		((match-beginning conf-beg-no)
		 (setq m-b-x (match-beginning conf-beg-no)
		       m-e-x (match-end conf-beg-no))
		 (put-text-property m-b-0 m-b-x 'face 'egg-diff-conflict)
		 (put-text-property m-b-x m-e-x 'face 'egg-branch-mono)
		 (put-text-property m-e-x m-e-0 'face 'egg-diff-none)
		 (setq sub-end (egg-safe-search "^++>>>>>>>.+$" end))
		 (put-text-property m-b-0 sub-end 'keymap
				    conflict-map))

		((match-beginning conf-end-no)
		 (setq m-b-x (match-beginning conf-end-no)
		       m-e-x (match-end conf-end-no))
		 (put-text-property m-b-0 m-b-x 'face 'egg-diff-conflict)
		 (put-text-property m-b-x m-e-x 'face 'egg-branch-mono)
		 (put-text-property m-e-x m-e-0 'face 'egg-diff-none))

		((match-beginning conf-div-no)
		 (put-text-property m-b-0 m-e-0 'face 'egg-diff-conflict))

		((match-beginning hunk-no) ;; hunk
		 (setq m-b-x (match-beginning hunk-no)
		       m-e-x (match-end hunk-no)
		       sub-end (or (egg-safe-search hunk-end-re end)
				   end))
		 (egg-decorate-hunk-header m-b-x m-e-x m-b-0 m-e-0)
		 (egg-delimit-section 
		  :hunk (egg-make-hunk-info 
			 (match-string-no-properties hunk-no)
			 sub-beg sub-end last-diff)
		  sub-beg sub-end m-e-0 hunk-map 
		  'egg-compute-navigation))

		((match-beginning cc-hunk-no) ;; cc-hunk
		 (setq m-b-x (match-beginning cc-hunk-no)
		       m-e-x (match-end cc-hunk-no)
		       sub-end (or (egg-safe-search hunk-end-re end)
				   end))
		 (egg-decorate-hunk-header m-b-x m-e-x m-b-0 m-e-0)
		 (egg-delimit-section 
		  :hunk (egg-make-hunk-info 
			 (match-string-no-properties cc-hunk-no)
			 sub-beg sub-end last-cc)
		  sub-beg sub-end m-e-0 cc-hunk-map 
		  'egg-compute-navigation))

		((match-beginning diff-no) ;; diff
		 (setq m-b-x (match-beginning diff-no)
		       m-e-x (match-end diff-no)
		       sub-end (or (egg-safe-search diff-end-re end) end)
		       head-end (or (egg-safe-search "^@@" end) end))
		 (egg-decorate-diff-header m-b-x m-e-x m-b-0 m-e-0)
		 (egg-delimit-section
		  :diff (setq last-diff
			      (egg-make-diff-info
			       (match-string-no-properties diff-no)
			       sub-beg sub-end head-end))
		  sub-beg sub-end m-e-0 diff-map 'egg-compute-navigation))

		((match-beginning cc-diff-no) ;; cc-diff
		 (setq m-b-x (match-beginning cc-diff-no)
		       m-e-x (match-end cc-diff-no)
		       sub-end (or (egg-safe-search diff-end-re end) end)
		       head-end (or (egg-safe-search "^@@@" end) end))
		 (egg-decorate-cc-diff-header m-b-x m-e-x m-b-0 m-e-0)
		 (egg-delimit-section
		  :diff (setq last-cc
			      (egg-make-diff-info
			       (match-string-no-properties cc-diff-no)
			       sub-beg sub-end head-end))
		  sub-beg sub-end m-e-0 cc-diff-map
		  'egg-compute-navigation))

		((match-beginning index-no) ;; index
		 (setq m-b-x (match-beginning index-no)
		       m-e-x (match-end index-no))
		 (egg-decorate-diff-index-line m-b-x m-e-x m-b-0 m-b-0))
		) ;; cond
	  ) ;; while
	) ;; save-excursion
      ) ;;; save -match-data

    nil))

(defun egg-decorate-diff-section (&rest args)
  (let ((beg (plist-get args	 :begin))
	(end (plist-get args	 :end))
	(a   (or (plist-get args :src-prefix) "a/"))
	(b   (or (plist-get args :dst-prefix) "b/"))
	(a-rev (plist-get args 	 :src-revision))
	(b-rev (plist-get args 	 :dst-revision)))
    (when (stringp a-rev)
      (put-text-property beg end :src-revision a-rev))
    (when (stringp b-rev)
      (put-text-property beg end :dst-revision b-rev))
    (egg-decorate-diff-sequence 
     (nconc (list :src-prefix a :dst-prefix b) args))))
  
(defun egg-diff-section-cmd-visit-file (file)
  (interactive (list (car (get-text-property (point) :diff))))
  (find-file file))

(defun egg-diff-section-cmd-visit-file-other-window (file)
  (interactive (list (car (get-text-property (point) :diff))))
  (find-file-other-window file))

(defun egg-diff-section-cmd-ediff3 (file)
  (interactive (list (car (get-text-property (point) :diff))))
  (find-file file)
  (egg-resolve-merge-with-ediff))

(defun egg-hunk-compute-line-no (hunk-header hunk-beg)
  (let ((limit (line-end-position))
	(line (string-to-number 
	       (nth 2 (save-match-data
			(split-string hunk-header "[ @,\+,-]+" t)))))
	(adjust 0))
    (save-excursion
      (goto-char hunk-beg)
      (forward-line 1)
      (end-of-line)
      (while (re-search-forward "^\\(?:\\+\\| \\).*" limit t)
	(setq adjust (1+ adjust))))
    (+ line adjust)))

(defsubst egg-hunk-info-at (pos)
  (let* ((diff-info (get-text-property pos :diff))
	 (head-beg (nth 1 diff-info))
	 (hunk-info (get-text-property pos :hunk))
	 (hunk-beg (+ (nth 1 hunk-info) head-beg))
	 (hunk-end (+ (nth 2 hunk-info) head-beg)))
    (list (car diff-info) (car hunk-info) hunk-beg hunk-end)))

(defun egg-hunk-section-cmd-visit-file (file hunk-header hunk-beg
					     &rest ignored)
  (interactive (egg-hunk-info-at (point)))
  (let ((line (egg-hunk-compute-line-no hunk-header hunk-beg)))
    (find-file file)
    (goto-line line)))

(defun egg-hunk-section-cmd-visit-file-other-window (file hunk-header hunk-beg
							  &rest ignored)
  (interactive (egg-hunk-info-at (point)))
  (let ((line (egg-hunk-compute-line-no hunk-header hunk-beg)))
    (find-file file)
    (goto-line line)))

(defun egg-section-cmd-toggle-hide-show (nav)
  (interactive (list (get-text-property (point) :navigation)))
  (if (assoc nav buffer-invisibility-spec)
      (remove-from-invisibility-spec (cons nav t))
    (add-to-invisibility-spec (cons nav t)))
  (force-window-update (current-buffer)))

(defun egg-section-cmd-toggle-hide-show-children (pos sect-type)
  (interactive (list (previous-single-property-change (1+ (point))
						      :navigation)
		     (get-text-property (point) :sect-type)))
  (unless pos
    (setq pos (point)))
  (let ((end (next-single-property-change pos sect-type))
	child-pos child-nav
	currently-hidden)
    (setq child-pos (next-single-property-change pos :navigation nil end))
    (when child-pos
      (setq child-nav (get-text-property child-pos :navigation))
      (setq currently-hidden (and child-nav
				  (assoc child-nav
					 buffer-invisibility-spec))))
    (setq child-pos pos)
    (while (< (setq child-pos (next-single-property-change child-pos :navigation nil end))
	      end)
      (setq child-nav (get-text-property child-pos :navigation))
      (if currently-hidden
	  (remove-from-invisibility-spec (cons child-nav  t))
	(add-to-invisibility-spec (cons child-nav t))))
    (force-window-update (current-buffer))))

(defun egg-diff-section-patch-string (&optional pos)
  (let* ((diff-info (get-text-property (or pos (point)) :diff))
	 (beg (nth 1 diff-info))
	 (end (+ (nth 2 diff-info) beg)))
    (buffer-substring-no-properties beg end)))

(defun egg-hunk-section-patch-string (&optional pos)
  (let* ((diff-info (get-text-property (or pos (point)) :diff))
	 (head-beg (nth 1 diff-info))
	 (head-end (+ (nth 3 diff-info) head-beg))
	 (hunk-info (get-text-property (or pos (point)) :hunk))
	 (hunk-beg (+ (nth 1 hunk-info) head-beg))
	 (hunk-end (+ (nth 2 hunk-info) head-beg)))
    (concat (buffer-substring-no-properties head-beg head-end)
	    (buffer-substring-no-properties hunk-beg hunk-end))))

;;;========================================================
;;; Buffer
;;;========================================================
(defvar egg-buffer-refresh-func nil)
(defvar egg-buffer-async-cmd-refresh-func nil)

(defsubst egg-buffer-async-do (accepted-code &rest args)
  (egg-async-do accepted-code 
		(cons (or egg-buffer-async-cmd-refresh-func
			  egg-buffer-refresh-func) 
		      (list (current-buffer)))
		args))

(defsubst egg-run-buffers-update-hook (&optional newly-read-state)
  (let ((egg-internal-current-state 
	 (or newly-read-state (egg-get-repo-state))))
    (run-hooks 'egg-buffers-refresh-hook)))

(defun egg-buffer-cmd-refresh ()
  (interactive)
  (when (and (egg-git-dir)
	     (functionp egg-buffer-refresh-func))
    (funcall egg-buffer-refresh-func (current-buffer))
    (recenter)))

(defun egg-buffer-cmd-navigate-next ()
  (interactive)
  (goto-char (or (next-single-property-change (point) :navigation)
		 (point))))

(defun egg-buffer-cmd-navigate-prev ()
  (interactive)
  (goto-char (previous-single-property-change (point) :navigation
					      nil (point-min))))

(defconst egg-buffer-mode-map
  (let ((map (make-sparse-keymap "Egg:Buffer")))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'egg-buffer-cmd-refresh)
    (define-key map (kbd "n") 'egg-buffer-cmd-navigate-next)
    (define-key map (kbd "p") 'egg-buffer-cmd-navigate-prev)
    map))

(defun egg-get-buffer (fmt create)
  (let* ((git-dir (egg-git-dir))
	 (dir (file-name-directory git-dir))
	 (dir-name (file-name-nondirectory
		    (directory-file-name dir)))
	 (buf-name (format fmt dir-name git-dir))
	 (default-directory dir)
	 (buf (get-buffer buf-name)))
    (unless (or (bufferp buf) (not create))
      (setq buf (get-buffer-create buf-name)))
    buf))

(defmacro define-egg-buffer (type name-fmt &rest body)
  (let* ((type-name (symbol-name type))
	 (get-buffer-sym (intern (concat "egg-get-" type-name "-buffer")))
	 (buffer-mode-sym (intern (concat "egg-" type-name "-buffer-mode")))
	 (buffer-mode-hook-sym (intern (concat "egg-" type-name "-buffer-mode-hook")))
	 (buffer-mode-map-sym (intern (concat "egg-" type-name "-buffer-mode-map")))
	 (update-buffer-no-create-sym (intern (concat "egg-update-" type-name "-buffer-no-create"))))
    `(progn
       (defun ,buffer-mode-sym ()
	 ,@body)

       (defun ,get-buffer-sym (&optional create)
	 (let ((buf (egg-get-buffer ,name-fmt create)))
	   (when (bufferp buf)
	     (with-current-buffer buf
	       (unless (eq major-mode ',buffer-mode-sym)
		 (,buffer-mode-sym))))
	   buf))
       ,(unless (string-match ":" type-name)
	  `(progn
	     (defun ,update-buffer-no-create-sym ()
	       (let ((buf (,get-buffer-sym)))
		 (when (bufferp buf)
		   (with-current-buffer buf
		     (when (functionp egg-buffer-refresh-func)
		       (funcall egg-buffer-refresh-func buf))))))
	     (add-hook 'egg-buffers-refresh-hook ',update-buffer-no-create-sym))))))


;; (cl-macroexpand '(define-egg-buffer diff "*diff-%s@egg:%s*"))
;; (cl-macroexpand ' (define-egg-buffer diff (buf) "*diff-%s@egg:%s*" (show-diff buf) ))



;;;========================================================
;;; Status Buffer
;;;========================================================

(defconst egg-status-buffer-rebase-map 
  (let ((map (make-sparse-keymap "Egg:StatusBufferRebase")))
    (set-keymap-parent map egg-section-map)
    (define-key map (kbd "x") 'egg-buffer-rebase-abort)
    (define-key map (kbd "u") 'egg-buffer-rebase-skip)
    (define-key map (kbd "RET") 'egg-buffer-rebase-continue)
    map))

(defun egg-buffer-do-rebase (upstream-or-action 
			     &optional old-base prompt)
  (let ((git-dir (egg-git-dir))
	modified-files res)
    (if (stringp upstream-or-action)
	(unless (egg-repo-clean)
	  (egg-status)
	  (error "Repo %s is not clean" git-dir))
      (unless (file-directory-p (concat git-dir "/.dotest-merge"))
	(error "No rebase in progress in directory %s"
	       (file-name-directory git-dir))))
    (setq res (egg-do-rebase-head upstream-or-action old-base prompt))
    (setq modified-files (plist-get res :files))
    (if modified-files
	(egg-revert-visited-files modified-files))
    (message "GIT-REBASE> %s" (plist-get res :message))
    (plist-get res :success)))

(defun egg-buffer-rebase-continue ()
  (interactive)
  (message "continue with current rebase")
  (unless (egg-buffer-do-rebase :continue)
    (egg-status)))

(defun egg-buffer-rebase-skip ()
  (interactive)
  (message "skip rebase's current commit")
  (unless (egg-buffer-do-rebase :skip)
    (egg-status)))

(defun egg-buffer-rebase-abort ()
  (interactive)
  (message "abort current rebase")
  (egg-buffer-do-rebase :abort)
  (egg-status))

(defun egg-sb-insert-repo-section ()
  (let* ((state (egg-repo-state))
	 (sha1 (plist-get state :sha1))
	 (beg (point))
	 (rebase-step (plist-get state :rebase-step))
	 (rebase-num (plist-get state :rebase-num))
	 context-beg context-end context-keymap
	 inv-beg)
    (insert (propertize (egg-pretty-head-string state) 'face 'egg-branch) 
		"\n"
		(propertize sha1 'face 'font-lock-string-face)
		"\n"
		(propertize (plist-get state :gitdir)
			    'face 'font-lock-constant-face)
		"\n")
    (setq context-beg (point))
    (setq inv-beg (1- context-beg))
    (if (null rebase-step)
	(call-process "git" nil t nil
		    "log" "--max-count=5"
		    "--abbrev-commit" "--pretty=oneline")
      (insert (format "Rebase: commit %s of %s\n" rebase-step rebase-num))
      (setq context-keymap egg-status-buffer-rebase-map))
    (setq context-end (point))
    (egg-delimit-section :section 'repo beg (point)
			 inv-beg egg-section-map 'repo)
    (if context-keymap
	(put-text-property context-beg context-end
			   'keymap context-keymap))))

(defun egg-sb-insert-untracked-section ()
  (let ((beg (point)) inv-beg)
    (insert (egg-prepend "Untracked Files:" "\n\n" 
			 'face 'egg-section-title)
	    "\n")
    (setq inv-beg (1- (point)))
    (call-process "git" nil t nil "ls-files" "--others" 
		  "--exclude-standard")
    (egg-delimit-section :section 'untracked beg (point)
			  inv-beg egg-section-map 'untracked)))

(defun egg-sb-insert-unstaged-section (title &rest extra-diff-options)
  (let ((beg (point)) inv-beg diff-beg)
    (insert (egg-prepend title "\n\n" 'face 'egg-section-title)
	    "\n")
    (setq diff-beg (point))
    (setq inv-beg (1- (point)))
    (apply 'call-process "git" nil t nil "diff" "--no-color"  "-p"
	   "--src-prefix=INDEX:/" "--dst-prefix=WORKDIR:/"
	   extra-diff-options)
    (egg-delimit-section :section 'unstaged beg (point)
			  inv-beg egg-section-map 'unstaged)
    (egg-decorate-diff-section :begin diff-beg 
			       :end (point) 
			       :src-prefix "INDEX:/"
			       :dst-prefix "WORKDIR:/"
			       :diff-map egg-unstaged-diff-section-map
			       :hunk-map egg-unstaged-hunk-section-map
			       :cc-diff-map egg-unmerged-diff-section-map
			       :cc-hunk-map egg-unmerged-hunk-section-map
			       :conflict-map egg-unmerged-hunk-section-map
			       )))

(defun egg-sb-insert-staged-section (title &rest extra-diff-options)
  (let ((beg (point)) inv-beg diff-beg)
    (insert (egg-prepend title "\n\n"
			  'face 'egg-section-title)
	    "\n")
    (setq diff-beg (point)
	  inv-beg (1- diff-beg))
    (apply 'call-process "git" nil t nil "diff" "--no-color" "--cached" "-p"
	   "--src-prefix=HEAD:/" "--dst-prefix=INDEX:/"
	   extra-diff-options)
    (egg-delimit-section :section 'staged beg (point)
			  inv-beg egg-section-map 'staged)
    (egg-decorate-diff-section :begin diff-beg 
			       :end (point) 
			       :src-prefix "HEAD:/"
			       :dst-prefix "INDEX:/"
			       :diff-map egg-staged-diff-section-map
			       :hunk-map egg-staged-hunk-section-map)))

(defun egg-checkout-ref (&optional default)
  (interactive (list (car (get-text-property (point) :ref))))
  (egg-do-checkout (completing-read "checkout: " (egg-all-refs)
				    nil nil (or default "HEAD"))))

(defconst egg-status-buffer-mode-map
  (let ((map (make-sparse-keymap "Egg:StatusBuffer")))
    (set-keymap-parent map egg-buffer-mode-map)
    (define-key map (kbd "c") 'egg-commit-log-edit)
    (define-key map (kbd "o") 'egg-checkout-ref)
    (define-key map (kbd "l") 'egg-log)
    (define-key map (kbd "S") 'egg-stage-all-files)
    map))

(defun egg-buffer-hide-all ()
  (let ((pos (point-min)))
    (goto-char (setq pos (point-min)))
    (while pos
      (add-to-invisibility-spec (cons pos t))
      (goto-char pos)
      (setq pos (next-single-property-change (point)
					     :navigation)))))

(defun egg-status-buffer-redisplay (buf &optional init)
  (with-current-buffer buf
    (let ((inhibit-read-only t)
	  (orig-pos (point)))
      (erase-buffer)
      (setq buffer-invisibility-spec nil)
      (egg-sb-insert-repo-section)
      (egg-sb-insert-unstaged-section "Unstaged Changes:")
      (egg-sb-insert-staged-section "Staged Changes:")
      (egg-sb-insert-untracked-section)
      
      (when (and init egg-status-buffer-init-hiding-mode)
	(egg-buffer-hide-all))
      (goto-char orig-pos))))

(define-egg-buffer status "*%s-status@%s*"
  "Major mode to display the egg status buffer."
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq major-mode 'egg-status-buffer-mode
	mode-name  "Egg-Status"
	mode-line-process ""
	truncate-lines t)
  (use-local-map egg-status-buffer-mode-map)
  (set (make-local-variable 'egg-buffer-refresh-func)
       'egg-status-buffer-redisplay)
  (setq buffer-invisibility-spec nil)
  (run-mode-hooks 'egg-status-buffer-mode-hook))

(defun egg-status (&optional no-update)
  (interactive "P")
  (let ((buf (egg-get-status-buffer 'create)))
    (unless no-update
      (with-current-buffer buf
	(egg-status-buffer-redisplay buf 'init)))
    (display-buffer buf t)))

;;;========================================================
;;; action
;;;========================================================

(defun egg-revert-visited-files (file-or-files)
  (let* ((git-dir (egg-git-dir))
	 (default-directory (file-name-directory git-dir))
	 (files (if (consp file-or-files) 
		   file-or-files
		 (list file-or-files))))
    (mapcar (lambda (file)
	      (let ((buf (get-file-buffer file)))
		(when (bufferp buf)
		  (with-current-buffer buf
		    (when (equal (egg-git-dir) git-dir)
		      (revert-buffer t t t))))))
	    files)))

(defun egg-revert-all-visited-files ()
  (let* ((git-dir (egg-git-dir))
	 (default-directory (file-name-directory git-dir))
	 bufs files)
    (setq files
	  (delq nil (mapcar (lambda (buf)
			      (with-current-buffer buf
				(when (and (buffer-file-name buf)
					   (equal (egg-git-dir) git-dir))
				  (buffer-file-name buf))))
			    (buffer-list))))
    (when (consp files)
      (setq files (mapcar 'expand-file-name
			  (apply 'egg-git-to-lines "ls-files" files)))
      (when (consp files)
	(egg-revert-visited-files files)))))

(defun egg-cmd-log-buffer ()
  (or (get-buffer (concat " *egg-cmd-logs@" (egg-git-dir) "*"))
      (let ((git-dir (egg-git-dir))
	    (default-directory default-directory)
	    dir)
	(unless git-dir
	  (error "Can't find git dir in %s" default-directory))
	(setq dir (file-name-nondirectory git-dir))
	(setq default-directory dir)
	(get-buffer-create (concat " *egg-cmd-logs@" git-dir "*")))))

(defsubst egg-cmd-log (&rest strings)
  (with-current-buffer (egg-cmd-log-buffer)
    (goto-char (point-max))
    (cons (current-buffer)
	  (prog1 (point)
	    (apply 'insert "LOG/" strings)))))

(defun egg-sync-handle-exit-code (ret accepted-codes logger)
  (let (output)
    (with-current-buffer (car logger)
      (save-excursion
	(goto-char (cdr logger))
	(forward-line 1)
	(setq output (buffer-substring-no-properties 
		      (point) (point-max)))))
    (egg-cmd-log (format "RET:%d\n" ret))
    (if (listp accepted-codes)
	(setq accepted-codes (cons 0 accepted-codes))
      (setq accepted-codes (list 0 accepted-codes)))
    (if (null (memq ret accepted-codes))
	(with-current-buffer (car logger)
	  (widen)
	  (narrow-to-region (cdr logger) (point-max))
	  (display-buffer (current-buffer) t)
	  nil)
      (egg-run-buffers-update-hook)
      output)))

(defun egg-sync-do (program stdin accepted-codes args)
  (let (logger ret)
    (setq logger (egg-cmd-log "RUN:" program " " (mapconcat 'identity args " ")
			      (if stdin " <REGION\n" "\n")))
    (setq ret 
	  (cond ((stringp stdin)
		 (with-temp-buffer
		   (insert stdin)
		   (apply 'call-process-region (point-min) (point-max)
				program nil (car logger) nil args)))
		((consp stdin)
		 (apply 'call-process-region (car stdin) (cdr stdin)
			program nil (car logger) nil args))
		((null stdin)
		 (apply 'call-process program nil (car logger) nil args)))) 
    (egg-sync-handle-exit-code ret accepted-codes logger)))

(defsubst egg-sync-do-region-0 (program beg end args)
  (egg-sync-do program (cons beg end) nil args))

(defsubst egg-sync-0 (&rest args)
  (egg-sync-do "git" nil nil args))

(defsubst egg-sync-do-region (program beg end &rest args)
  (egg-sync-do program (cons beg end) nil args))

(defsubst egg-sync-git-region (beg end &rest args)
  (egg-sync-do "git" (cons beg end) nil args))

(defun egg-sync-do-file (file program stdin accepted-codes args)
  (let ((default-directory (file-name-directory (egg-git-dir)))
	output)
    (setq file (expand-file-name file))
    (setq args (mapcar (lambda (word)
			 (if (string= word file) file word))
		       args))
    (when (setq output (egg-sync-do program stdin accepted-codes args))
      (cons file output))))

(defun egg-hunk-section-patch-cmd (pos program &rest args)
  (let ((patch (egg-hunk-section-patch-string pos))
	(file (car (get-text-property pos :diff))))
    (unless (stringp file)
      (error "No diff with file-name here!"))
    (egg-sync-do-file file program patch nil args)))

(defun egg-show-git-output (output line-no &optional prefix)
  (unless (stringp prefix) (setq prefix "GIT"))
  (if (consp output) (setq output (cdr output)))
  (when (and (stringp output) (> (length output) 1))
    (when (numberp line-no)
      (when (setq output (save-match-data (split-string output "\n" t)))
	(cond ((< line-no 0)
	       (setq line-no (1+ line-no))
	       (setq output (nth line-no (nreverse output))))
	      ((> line-no 0)
	       (setq line-no (1- line-no))
	       (setq output (nth line-no output)))
	      (t (setq output nil)))))
    (when (stringp output)
      (message "%s> %s" prefix output)
      t)))

(defun egg-hunk-section-cmd-stage (pos)
  (interactive (list (point)))
  (egg-show-git-output 
   (egg-hunk-section-patch-cmd pos "git" "apply" "--cached")
   -1 "GIT-APPLY"))

(defun egg-hunk-section-cmd-unstage (pos)
  (interactive (list (point)))
  (egg-show-git-output 
   (egg-hunk-section-patch-cmd pos "git" "apply" "--cached" "--reverse")
   -1 "GIT-APPLY"))

(defun egg-hunk-section-cmd-undo (pos)
  (interactive (list (point)))
  (let ((file (egg-hunk-section-patch-cmd pos "patch"
					  "-p1" "--quiet" "--reverse")))
    (if (consp file) (setq file (car file)))
    (when (stringp file)
      (egg-revert-visited-files file))))

(defun egg-diff-section-patch-cmd (pos accepted-codes &rest args)
  (let ((file (car (get-text-property pos :diff))))
    (unless (stringp file)
      (error "No diff with file-name here!"))
    (egg-sync-do-file file "git" nil accepted-codes
		      (append args (list file)))))

(defun egg-diff-section-cmd-stage (pos)
  (interactive (list (point)))
  (egg-diff-section-patch-cmd pos nil "add"))

(defun egg-diff-section-cmd-unstage (pos)
  (interactive (list (point)))
  (egg-show-git-output 
   (egg-diff-section-patch-cmd pos 1 "reset" "HEAD" "--")
   1  "GIT-RESET"))

(defun egg-diff-section-cmd-undo-old-no-revsion-check (pos)
  (interactive (list (point)))
  (let ((file (egg-diff-section-patch-cmd pos nil "checkout" "--")))
    (if (consp file) (setq file (car file)))
    (when (stringp file)
      (egg-revert-visited-files file))))

(defun egg-diff-section-cmd-undo (pos)
  (interactive (list (point)))
  (let ((file (car (or (get-text-property pos :diff)
		       (error "No diff with file-name here!"))))
	(src-rev (get-text-property pos :src-revision))
	args)
    (setq args
	  (if (stringp src-rev)
	      (list "checkout" src-rev "--" file)
	    (list "checkout" "--" file)))
    (when (setq file (egg-sync-do-file file "git" nil nil args))
      (if (consp file) (setq file (car file)))
      (when (stringp file)
	(egg-revert-visited-files file)))))

(defun egg-file-stage-current-file ()
  (interactive)
  (let ((git-dir (egg-git-dir))
	(file (buffer-file-name)))
    (when (egg-sync-do-file file "git" nil nil (list "add" "--" file))
	(message "staged %s modifications" file))))

(defun egg-stage-all-files ()
  (interactive)
  (let* ((git-dir (egg-git-dir))
	 (default-directory (file-name-directory git-dir)))
    (when (egg-sync-do "git" nil nil (list "add" "-u"))
	(message "staged all tracked files's modifications"))))


(defun egg-do-checkout (rev)
  (let* ((git-dir (egg-git-dir))
	 (default-directory (file-name-directory git-dir)))
    (if (egg-sync-do "git" nil nil (list "checkout" rev))
	(egg-revert-all-visited-files))))

(defun egg-do-tag (&optional rev prompt force)
  (let ((all-refs (egg-all-refs))
	(name (read-string (or prompt "new tag name: ")))
	(rev (or rev "HEAD")))
    (when (and (not force) (member name all-refs))
      (error "referene %s already existed!" name))
    (if force
	(egg-git-ok nil "tag" "-f" name rev)
      (egg-git-ok nil "tag" name rev))))

(defun egg-do-create-branch (&optional rev checkout prompt force)
  (let ((all-refs (egg-all-refs))
	(name (read-string (or prompt "create new branch: ")))
	(rev (or rev "HEAD")))
    (when (and (not force) (member name all-refs))
      (error "referene %s already existed!" name))
    (if (null checkout)
	(if force 
	    (egg-git-ok nil "branch" "-f" name rev)
	  (egg-git-ok nil "branch" name rev))
      (if force
	  (egg-sync-0 "checkout" "-b" "-f" name rev)
	(egg-sync-0 "checkout" "-b" name rev)))))

(defun egg-do-move-head (rev &optional update-wdir update-index)
  (when (egg-show-git-output
	 (cond (update-wdir (egg-sync-0 "reset" "--hard" rev))
	       (update-index (egg-sync-0 "reset" rev))
	       (t (egg-sync-0 "reset" "--soft" rev)))
	 -1 "GIT-RESET")
    (if update-wdir (egg-revert-all-visited-files))))

(defun egg-do-merge-to-head (rev &optional no-commit)
  (let ((msg (concat "merging in " rev))
	(commit-flag (if no-commit "--no-commit" "--commit"))
	(pre-merge (egg-get-current-sha1))
	merge-cmd-res modified-files res feed-back)
    (with-temp-buffer
      (setq merge-cmd-res (egg-git-ok (current-buffer)
		      "merge" "--log" commit-flag "-m" msg rev))
      (goto-char (point-min))
      (setq modified-files 
	    (egg-git-to-lines "diff" "--name-only" pre-merge))
      (setq feed-back
	    (save-match-data
	      (car (nreverse (split-string (buffer-string)
					   "[\n]+" t)))))
      (egg-run-buffers-update-hook)
      (list :success merge-cmd-res
	    :files modified-files
	    :message feed-back))))

(defun egg-do-rebase-head (upstream-or-action 
			   &optional old-base prompt)
  (let ((pre-merge (egg-get-current-sha1))
	cmd-res modified-files feed-back old-choices)
    (with-temp-buffer
      (when (and (stringp upstream-or-action) ;; start a rebase
		 (eq old-base t))	      ;; ask for old-base
	(unless (egg-git-ok (current-buffer) "rev-list"
			    "--topo-order" "--reverse"
			    (concat upstream-or-action "..HEAD^"))
	  (error "Failed to find rev between %s and HEAD^: %s"
		 upstream-or-action (buffer-string)))
	(unless (egg-git-region-ok (point-min) (point-max)
				   "name-rev" "--stdin")
	  (error "Failed to translate revisions: %s" (buffer-string)))
	(save-match-data 
	  (goto-char (point-min))
	  (while (re-search-forward "^.+(\\(.+\\))$" nil t)
	    (setq old-choices (cons (match-string-no-properties 1)
				    old-choices))))
	(setq old-base
	      (completing-read (or prompt "old base: ") old-choices))
	(erase-buffer))
      
      (setq cmd-res 
	    (cond ((and (stringp old-base) (stringp upstream-or-action))
		   (egg-git-ok (current-buffer) "rebase" "-m" "--onto"
			       upstream-or-action old-base))
		  ((eq upstream-or-action :abort)
		   (egg-git-ok (current-buffer) "rebase" "--abort"))
		  ((eq upstream-or-action :skip)
		   (egg-git-ok (current-buffer) "rebase" "--skip"))
		  ((eq upstream-or-action :continue)
		   (egg-git-ok (current-buffer) "rebase" "--continue"))
		  ((stringp upstream-or-action)
		   (egg-git-ok (current-buffer) "rebase" "-m" 
			       upstream-or-action))))
      (goto-char (point-min))
      (setq feed-back
	    (egg-safe-search-pickup 
	     "^\\(?:CONFLICT\\|All done\\|HEAD is now at\\|Fast-forwarded\\|You must edit all merge conflicts\\).+$")) 
      (setq modified-files 
	    (egg-git-to-lines "diff" "--name-only" pre-merge))
      (egg-run-buffers-update-hook)
      (list :success cmd-res
	    :message feed-back
	    :files modified-files))))

(defun egg-rm-ref (&optional force name prompt default)
  (let* ((refs-alist (egg-ref-type-alist))
	 (name (or name (completing-read (or prompt "remove ref: ")
					 refs-alist nil t
					 default)))
	 (type (cdr (assoc name refs-alist))))
    (unless (and name type)
      (error "Cannot find reference %s!" name))
    (egg-show-git-output
     (cond ((eq :tag type)
	    (egg-sync-0 "tag" "-vd" name))
	   ((eq :head type)
	    (egg-sync-0 "branch" (if force "-vD" "-vd") name))
	   ((eq :remote type)
	    (egg-sync-0 "branch" (if force "-vrD" "-vrd") name)))
     -1)))

;;;========================================================
;;; log message
;;;========================================================

(require 'derived)
(require 'ring)

(defvar egg-log-msg-ring (make-ring 32))
(defvar egg-log-msg-ring-idx nil)
(defvar egg-log-msg-action nil)
(defvar egg-log-msg-text-beg nil)
(defvar egg-log-msg-text-end nil)
(defvar egg-log-msg-diff-beg nil)
(define-derived-mode egg-log-msg-mode text-mode "Egg-LogMsg"
  "Major mode for editing Git log message.\n\n
\{egg-log-msg-mode-map}."
  (setq default-directory (file-name-directory (egg-git-dir)))
  (make-local-variable 'egg-log-msg-action)
  (set (make-local-variable 'egg-log-msg-ring-idx) nil)
  (set (make-local-variable 'egg-log-msg-text-beg) nil)
  (set (make-local-variable 'egg-log-msg-text-end) nil)
  (set (make-local-variable 'egg-log-msg-diff-beg) nil))

(define-key egg-log-msg-mode-map (kbd "C-c C-c") 'egg-log-msg-done)
(define-key egg-log-msg-mode-map (kbd "M-p") 'egg-log-msg-older-text)
(define-key egg-log-msg-mode-map (kbd "M-n") 'egg-log-msg-newer-text)
(define-key egg-log-msg-mode-map (kbd "C-l") 'egg-buffer-cmd-refresh)

(defun egg-log-msg-commit ()
  (let (output)
    (setq output 
	  (egg-sync-git-region egg-log-msg-text-beg egg-log-msg-text-end 
			       "commit" "-F" "-"))
    (when output
      (egg-show-git-output output -1 "GIT-COMMIT")
      (egg-run-buffers-update-hook))))

(defun egg-log-msg-amend-commit ()
  (let (output)
    (setq output 
	  (egg-sync-git-region egg-log-msg-text-beg egg-log-msg-text-end 
			       "commit" "--amend" "-F" "-"))
    (when output
      (egg-show-git-output output -1 "GIT-COMMIT-AMEND")
      (egg-run-buffers-update-hook))))

(defun egg-log-msg-done ()
  (interactive)
  (widen)
  (goto-char egg-log-msg-text-beg)
  (if (save-excursion (re-search-forward "\\sw\\|\\-" 
					 egg-log-msg-text-end t))
      (when (functionp egg-log-msg-action)
	(ring-insert egg-log-msg-ring 
		     (buffer-substring-no-properties egg-log-msg-text-beg
						     egg-log-msg-text-end))
	(funcall egg-log-msg-action)
	(bury-buffer))
    (message "Please enter a log message!")
    (ding)))

(defun egg-log-msg-hist-cycle (&optional forward)
  "Cycle through message log history."
  (let ((len (ring-length egg-log-msg-ring)))
    (cond ((<= len 0) 
	   ;; no history
	   (message "No previous log message.")
	   (ding))
	  ;; don't accidentally throw away unsaved text
	  ((and  (null egg-log-msg-ring-idx)
		 (> egg-log-msg-text-end egg-log-msg-text-beg)
		 (not (y-or-n-p "throw away current text? "))))
	  ;; do it
	  (t (delete-region egg-log-msg-text-beg egg-log-msg-text-end)
	     (setq egg-log-msg-ring-idx
		   (if (null egg-log-msg-ring-idx)
		       (if forward 
			   ;; 1st-time + fwd = oldest
			   (ring-minus1 0 len)
			 ;; 1st-time + bwd = newest
		       0)
		     (if forward 
			 ;; newer
			 (ring-minus1 egg-log-msg-ring-idx len)
		       ;; older
		       (ring-plus1 egg-log-msg-ring-idx len)))) 
	     (goto-char egg-log-msg-text-beg)
	     (insert (ring-ref egg-log-msg-ring egg-log-msg-ring-idx))))))

(defun egg-log-msg-older-text ()
  "Cycle backward through comment history."
  (interactive)
  (egg-log-msg-hist-cycle))

(defun egg-log-msg-newer-text ()
  "Cycle forward through comment history."
  (interactive)
  (egg-log-msg-hist-cycle t))

(defun egg-commit-log-buffer-show-diffs (buf &optional init)
  (with-current-buffer buf
    (let ((inhibit-read-only t) beg)
      (goto-char egg-log-msg-diff-beg)
      (delete-region (point) (point-max))
      (setq beg (point))
      (egg-sb-insert-staged-section "Changes to Commit:" "--stat")
      (egg-sb-insert-unstaged-section "Deferred Changes:")
      (egg-sb-insert-untracked-section)
      (put-text-property beg (point) 'read-only t)
      (put-text-property beg (point) 'front-sticky nil)
      (when (and init egg-commit-buffer-init-hiding-mode)
	(egg-buffer-hide-all))
      (force-window-update buf))))

(define-egg-buffer commit "*%s-commit@%s*"
  (egg-log-msg-mode)
  (setq major-mode 'egg-commit-buffer-mode
	mode-name "Egg-Commit"
	mode-line-process "")
  (set (make-local-variable 'egg-buffer-refresh-func)
       'egg-commit-log-buffer-show-diffs)
  (setq buffer-invisibility-spec nil)
  (run-mode-hooks 'egg-commit-buffer-mode-hook))

(defun egg-commit-log-edit (&optional amend)
  (interactive "P")
  (let* ((git-dir (egg-git-dir))
	 (default-directory (file-name-directory git-dir))
	 (buf (egg-get-commit-buffer 'create))
	 (head-info (egg-head))
	 (head (or (cdr head-info) 
		   (format "Detached HEAD! (%s)" (car head-info))))
	 (inhibit-read-only inhibit-read-only))
    (pop-to-buffer buf t)
    (setq inhibit-read-only t)
    (erase-buffer)
    (set (make-local-variable 'egg-log-msg-action)
	 (if amend 'egg-log-msg-amend-commit 'egg-log-msg-commit))
    (insert "Commiting into: " (propertize head 'face 'egg-branch) "\n"
	    "Repository: " (propertize git-dir 'face 'font-lock-constant-face) "\n"
	    (propertize "--------------- Commit Message (type C-c C-c when done) ---------------"
			'face 'font-lock-comment-face))
    (put-text-property (point-min) (point) 'read-only t)
    (put-text-property (point-min) (point) 'rear-sticky nil)
    (insert "\n")
    (set (make-local-variable 'egg-log-msg-text-beg) (point-marker))
    (set-marker-insertion-type egg-log-msg-text-beg nil)
    (put-text-property (1- egg-log-msg-text-beg) egg-log-msg-text-beg 
		       :navigation 'commit-log-text)
    (insert (propertize "\n------------------------ End of Commit Message ------------------------" 
			'read-only t 'front-sticky nil
			'face 'font-lock-comment-face))
    (set (make-local-variable 'egg-log-msg-diff-beg) (point-marker))
    (set-marker-insertion-type egg-log-msg-diff-beg nil)
    (egg-commit-log-buffer-show-diffs buf 'init)
    (goto-char egg-log-msg-text-beg)
    (when amend
      (egg-git-ok t "log" "--max-count=1" "--pretty=format:%s%n%n%b"
		  "HEAD"))
    (set (make-local-variable 'egg-log-msg-text-end) (point-marker))
    (set-marker-insertion-type egg-log-msg-text-end t)))

;;;========================================================
;;; diff-mode
;;;========================================================
(defvar egg-diff-buffer-info nil)

(defconst egg-diff-buffer-mode-map
  (let ((map (make-sparse-keymap "Egg:DiffBuffer")))
    (set-keymap-parent map egg-buffer-mode-map)
    map))

(defun egg-diff-buffer-insert-diffs (buffer)
  (with-current-buffer buffer
    (let ((args (plist-get egg-diff-buffer-info :args))
	  (title (plist-get egg-diff-buffer-info :title))
	  (prologue (plist-get egg-diff-buffer-info :prologue))
	  (src-prefix (plist-get egg-diff-buffer-info :src))
	  (dst-prefix (plist-get egg-diff-buffer-info :dst))
	  (inhibit-read-only t)
	  pos inv-beg)
      (erase-buffer)
      (insert (propertize title 'face 'egg-section-title) "\n")
      (setq inv-beg (point))
      (insert prologue "\n")
      (apply 'call-process "git" nil t nil "diff" args)
      (egg-delimit-section :section 'top-level (point-min) (point))
      (apply 'egg-decorate-diff-section
	     :begin (point-min)
	     :end (point)
	     :src-prefix src-prefix
	     :dst-prefix dst-prefix
	     egg-diff-buffer-info))))

(define-egg-buffer diff "*%s-diff@%s*"
  "Major mode to display the git diff output."
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq major-mode 'egg-diff-buffer-mode
	mode-name  "Egg-Diff"
	mode-line-process ""
	truncate-lines t)
  (use-local-map egg-diff-buffer-mode-map)
  (set (make-local-variable 'egg-buffer-refresh-func)
       'egg-diff-buffer-insert-diffs)
  (setq buffer-invisibility-spec nil)
  (run-mode-hooks 'egg-diff-buffer-mode-hook))

(defun egg-do-diff (diff-info)
  (let* ((git-dir (egg-git-dir))
	 (dir (file-name-directory git-dir))
	 (buf (egg-get-diff-buffer 'create)))
    (with-current-buffer buf
      (set (make-local-variable 'egg-diff-buffer-info) diff-info)
      (egg-diff-buffer-insert-diffs buf))
    buf))

(defun egg-build-diff-info (src dst &optional file)
  (let* ((git-dir (egg-git-dir))
	 (dir (file-name-directory git-dir))
	 info tmp)
    (setq info
	  (cond ((and (null src) (null dst))
		 (list :args (list "--no-color" "-p"
				   "--src-prefix=INDEX/"
				   "--dst-prefix=WORKDIR/")
		       :title (format "from INDEX to %s" dir)
		       :prologue "hunks can be removed or added into INDEX."
		       :src "INDEX/" :dst "WORKDIR/"
		       :diff-map egg-unstaged-diff-section-map
		       :hunk-map egg-unstaged-hunk-section-map))
		((and (equal src "HEAD") (equal dst "INDEX"))
		 (list :args (list "--no-color" "--cached" "-p"
				   "--src-prefix=INDEX/"
				   "--dst-prefix=WORKDIR/")
		       :title "from HEAD to INDEX" 
		       :prologue "hunks can be removed from INDEX."
		       :src "HEAD/" :dst "INDEX/"
		       :diff-map egg-staged-diff-section-map
		       :hunk-map egg-staged-hunk-section-map))
		((and (stringp src) (stringp dst))
		 (list :args (list "--no-color" "-p"
				   (concat src ".." dst))
		       :title (format "from %s to %s" src dst) 
		       :prologue (format "a: %s\nb: %s" src dst)
		       :src-revision src
		       :dst-revision dst
		       :diff-map egg-diff-section-map
		       :hunk-map egg-hunk-section-map))
		((and (stringp src) (null dst))
		 (list :args (list "--no-color" "-p" src)
		       :title (format "from %s to %s" src dir) 
		       :prologue (concat (format "a: %s\nb: %s\n" src dir)
					 "hunks can be removed???")
		       :src-revision src
		       :diff-map egg-wdir-diff-section-map
		       :hunk-map egg-wdir-hunk-section-map))))
    (when (stringp file)
      (setq file (list file)))
    (when (consp file) 
      (setq tmp (plist-get info :prologue))
      (setq tmp (concat "\n"
			     (mapconcat 'identity file "\n")
			     "\n\n"
			     tmp))
      (plist-put info :prologue tmp)
      (setq tmp (plist-get info :args))
      (setq tmp (append tmp (cons "--" file)))
      (plist-put info :args tmp))
    info))



;;;========================================================
;;; log browsing
;;;========================================================
(defvar egg-log-buffer-comment-column nil)
(defvar egg-internal-log-buffer-closure nil)

(defun egg-run-git-log-HEAD ()
  (egg-git-ok t "log" "--max-count=10000" "--graph" "--topo-order"
		"--pretty=oneline" "--decorate"))

(defun egg-run-git-log-all ()
  (egg-git-ok t "log" "--max-count=10000" "--graph" "--topo-order"
		"--pretty=oneline" "--decorate" "--all"))

(defun egg-run-git-log-pickaxe (string)
  (egg-git-ok t "log" "--pretty=oneline" "--decorate"
	      (concat "-S" string)))


(defun egg-log-buffer-insert-n-decorate-logs (log-insert-func)
  (let ((beg (point)))
    (funcall log-insert-func)
    (goto-char beg)
    (egg-decorate-log egg-log-commit-map 
		      egg-log-local-ref-map
		      egg-log-local-ref-map
		      egg-log-remote-ref-map)))

(defconst egg-log-commit-map 
  (let ((map (make-sparse-keymap "Egg:LogCommit")))
    (set-keymap-parent map egg-hide-show-map)
    (define-key map (kbd "SPC") 'egg-log-buffer-insert-commit)
    (define-key map (kbd "B") 'egg-log-buffer-create-new-branch)
    (define-key map (kbd "b") 'egg-log-buffer-start-new-branch)
    (define-key map (kbd "o") 'egg-log-buffer-checkout-commit)
    (define-key map (kbd "t") 'egg-log-buffer-tag-commit)
    (define-key map (kbd "a") 'egg-log-buffer-attach-head)
    (define-key map (kbd "m") 'egg-log-buffer-merge)
    (define-key map (kbd "r") 'egg-log-buffer-rebase)
    map))

(defconst egg-log-ref-map 
  (let ((map (make-sparse-keymap "Egg:LogRef")))
    (set-keymap-parent map egg-log-commit-map)
    (define-key map (kbd "x") 'egg-log-buffer-rm-ref)
    (define-key map (kbd "u") 'egg-log-buffer-push-to-local)
    map))

(defconst egg-log-local-ref-map 
  (let ((map (make-sparse-keymap "Egg:LogLocalRef")))
    (set-keymap-parent map egg-log-ref-map)
    (define-key map (kbd "U") 'egg-log-buffer-push-to-remote)
    (define-key map (kbd "d") 'egg-log-buffer-push-head-to-local)
    map))

(defconst egg-log-remote-ref-map 
  (let ((map (make-sparse-keymap "Egg:LogRemoteRef")))
    (set-keymap-parent map egg-log-ref-map)
    (define-key map (kbd "d") 'egg-log-buffer-fetch-remote-ref)
    map))

(defconst egg-log-diff-map 
  (let ((map (make-sparse-keymap "Egg:LogDiff")))
    (set-keymap-parent map egg-section-map)
    (define-key map (kbd "RET") 'egg-log-diff-cmd-visit-file-other-window)
    (define-key map (kbd "f") 'egg-log-diff-cmd-visit-file)
    map))

(defconst egg-log-hunk-map 
  (let ((map (make-sparse-keymap "Egg:LogHunk")))
    (set-keymap-parent map egg-section-map)
    (define-key map (kbd "RET") 'egg-log-hunk-cmd-visit-file-other-window)
    (define-key map (kbd "f") 'egg-log-hunk-cmd-visit-file)
    map))

(defconst egg-log-buffer-mode-map
  (let ((map (make-sparse-keymap "Egg:LogBuffer")))
    (set-keymap-parent map egg-buffer-mode-map)
    (define-key map "n" 'egg-log-buffer-next-ref)
    (define-key map "s" 'egg-status)
    (define-key map "p" 'egg-log-buffer-prev-ref)
    map))

(defun egg-decorate-log (&optional line-map head-map tag-map remote-map)
  (let ((start (point))
	(head-sha1 (egg-get-current-sha1)) 
	(ov (make-overlay (point-min) (point-min) nil t))
	(dec-ref-alist (egg-full-ref-decorated-alist
			'egg-branch-mono head-map 
			'egg-tag-mono tag-map
			'egg-remote-mono 'egg-branch-mono remote-map))
	(ref-alist (egg-full-ref-alist))
	(ref-string-len 0) 
	(dashes-len 0)
	(min-dashes-len 300)
	separator ref-string refs full-refs sha1
	line-props graph-len beg end)
    (save-excursion
      (while (re-search-forward "^\\(?:\\([ \\\\/*|.-]+\\) \\)?\\([0-9a-f]+\\) \\((\\(?:tag: \\)?\\([^)]+\\)) \\)?\\(.+\\)$" nil t)
	(setq beg (match-beginning 0)
	      end (1+ (match-end 0)))
	(setq graph-len (if (match-end 1) 
			    (- (match-end 1) (match-beginning 1)) 0))
	(setq sha1 (match-string-no-properties 2))
	(setq full-refs (if (match-beginning 4)
			    (save-match-data
			      (split-string 
			       (match-string-no-properties 4) "[, ]+" t))))
	(setq refs (mapcar (lambda (full-ref-name) 
			     (cdr (assoc full-ref-name ref-alist)))
			   full-refs))

	;; common line decorations
	(setq line-props (list :navigation sha1 :commit sha1))

	(if line-map
	    (setq line-props (nconc (list 'keymap line-map)
				    line-props)))
	(when refs
	  (setq line-props (nconc (list :references refs)
				  line-props)))

	
	(setq separator (apply 'propertize " " line-props))
	(setq ref-string
	      (if full-refs
		  (propertize
		   (mapconcat (lambda (full-ref-name)
				(cdr (assoc full-ref-name 
					    dec-ref-alist)))
			      full-refs separator)
		   :navigation sha1 :commit sha1
		   :references refs)))
	(setq ref-string-len (if ref-string (length ref-string)))

	;; entire line
	(add-text-properties beg end line-props)

	;; comment
	(put-text-property (match-beginning 5) (match-end 5) 
			   'face 'egg-text-2)
	;; delete refs list (they're already parsed)
	(when (match-beginning 3)
	  (delete-region (match-beginning 3) (match-end 3)))

	;; shorten sha
 	(delete-region (+ (match-beginning 2) 8) (match-end 2))
	(put-text-property (match-beginning 2) (+ (match-beginning 2) 8)
			   'face 'font-lock-constant-face)
	
	(setq dashes-len (- 300 graph-len 1 
			    (if refs (1+ ref-string-len) 0)))
;;; 	(setq dashes-len (- 300 graph-len
;;; 			    (if refs (1+ ref-string-len) 0)))
	(setq min-dashes-len (min min-dashes-len dashes-len))

	(goto-char (match-beginning 2))
	
	(insert
	 (concat (apply 'propertize 
			(make-string dashes-len ?-)
			(nconc (list 'face 'egg-graph)
			       line-props))
		 (if refs
		     (concat separator ref-string separator)
		   separator)))
	
	(when (string= sha1 head-sha1)
	  (overlay-put ov 'face 'region)
	  (overlay-put ov 'evaporate t)
	  (move-overlay ov beg (1+ (line-end-position)))))
      
      (goto-char start)

      (if (= min-dashes-len 300)
	  (insert (propertize "nothing found!" 'face 'egg-warning))

	;; compute how many dashes can be deleted while
	;; leaving at least 1 dash
	(setq min-dashes-len (1- min-dashes-len))

	;; before cut
	;; type a: 300 = graph spc dashes
	;; type b: 300 = graph spc dashes spc ref-string
	;;
	;; after cut
	;; type a: 300 - min-dashes-len = graph spc dashes
	;; type b: 300 - min-dashes-len = graph spc dashes spc ref-string
	;; 
	;; a: comment-column = graph spc dashes spc sha1-8 spc
	;; b: comment-column = graph spc dashes spc ref-string spc sha1-8 spc
	;; need to remove the 1st spc if graph-len = 0
	(set (make-local-variable 'egg-log-buffer-comment-column)
	     (+ (- 300 min-dashes-len (if (> graph-len 0) 0 1)) 1 8 1))

	(when (> min-dashes-len 0)
	  (let ((re (format "^\\(?:[^\n-]+ \\)?\\(-\\{%d\\}\\)" min-dashes-len)))
	    (while (re-search-forward re nil t)
	      (delete-region (match-beginning 1) (match-end 1)))))))))

(defun egg-log-diff-cmd-visit-file (file sha1)
  (interactive (list (car (get-text-property (point) :diff))
		     (get-text-property (point) :commit)))
  (pop-to-buffer (egg-file-get-other-version file sha1 nil t)))

(defun egg-log-diff-cmd-visit-file-other-window (file sha1)
  (interactive (list (car (get-text-property (point) :diff))
		     (get-text-property (point) :commit)))
  (pop-to-buffer (egg-file-get-other-version file sha1 nil t) t))

(defun egg-log-hunk-cmd-visit-file (sha1 file hunk-header hunk-beg &rest ignored)
  (interactive (cons (get-text-property (point) :commit)
		     (egg-hunk-info-at (point))))
  (let ((line (egg-hunk-compute-line-no hunk-header hunk-beg)))
    (pop-to-buffer (egg-file-get-other-version file sha1 nil t))
    (goto-line line)))

(defun egg-log-hunk-cmd-visit-file-other-window (sha1 file hunk-header hunk-beg &rest ignored)
  (interactive (cons (get-text-property (point) :commit)
		     (egg-hunk-info-at (point))))
  (let ((line (egg-hunk-compute-line-no hunk-header hunk-beg)))
    (pop-to-buffer (egg-file-get-other-version file sha1 nil t) t)
    (goto-line line)))

(defun egg-log-buffer-get-rev-at (pos &optional symbolic)
  (let* ((commit (get-text-property pos :commit))
	 (refs (get-text-property pos :references))
	 (first-head (if (stringp refs) refs (car (last refs))))
	 (ref-at-point (car (get-text-property pos :ref)))
	 (head-sha1 (egg-get-current-sha1)))
    (if (string= head-sha1 commit) 
	"HEAD"
      (or ref-at-point first-head 
	  (if symbolic (egg-name-rev commit) commit)))))

(defun egg-log-buffer-merge (pos &optional no-commit)
  (interactive "d\nP")
  (let ((rev (egg-log-buffer-get-rev-at pos))
	res modified-files buf)
    (unless (egg-repo-clean)
      (egg-status) 
      (error "Repo is not clean!"))
    (if  (null (y-or-n-p (format "merge %s to HEAD? " rev)))
	(message "cancel merge from %s to HEAD!" rev)
      (setq res (egg-do-merge-to-head rev no-commit))
      (setq modified-files (plist-get res :files))
      (if modified-files
	  (egg-revert-visited-files modified-files)) 
      (message "GIT-MERGE> %s" (plist-get res :message))
      (unless (and (plist-get res :success) (null no-commit))
	(egg-status)))))

(defun egg-log-buffer-rebase (pos &optional move)
  (interactive "d\nP")
  (let ((rev (egg-log-buffer-get-rev-at pos))
	res modified-files buf)
    (if  (null (y-or-n-p (format "rebase HEAD to %s? " rev)))
	(message "cancel rebase HEAD to %s!" rev)
      (unless (egg-buffer-do-rebase 
	       rev (if move t)
	       (if move
		   (format "starting point to rebase HEAD onto %s: "
			   rev)))
	(egg-status)))))

(defun egg-log-buffer-checkout-commit (pos)
  (interactive "d")
  (egg-do-checkout 
   (completing-read "checkout: " (egg-all-refs) nil nil 
		    (egg-log-buffer-get-rev-at pos))))

(defun egg-log-buffer-tag-commit (pos &optional force)
  (interactive "d\nP")
  (let ((rev (egg-log-buffer-get-rev-at pos)))
    (when (egg-do-tag rev (format "tag %s with name: " rev) force)
      (funcall egg-buffer-refresh-func (current-buffer)))))

(defun egg-log-buffer-create-new-branch (pos &optional force)
  (interactive "d\nP")
  (let ((rev (egg-log-buffer-get-rev-at pos)))
    (when (egg-do-create-branch
	   rev nil
	   (format "create new branch at %s with name: " rev) 
	   force)
      (funcall egg-buffer-refresh-func (current-buffer)))))

(defun egg-log-buffer-start-new-branch (pos &optional force)
  (interactive "d\nP")
  (let ((rev (egg-log-buffer-get-rev-at pos)))
    (when (egg-do-create-branch
	   rev 'checkout
	   (format "start new branch at %s with name: " rev)
	   force))))

(defun egg-log-buffer-attach-head (pos &optional strict-level)
  (interactive "d\np")
  (let* ((rev (egg-name-rev (egg-log-buffer-get-rev-at pos t)))
	 (update-index (> strict-level 3))
	 (update-wdir (> strict-level 15))
	 (prompt (format "attach HEAD to %s%s? " rev
			 (cond (update-wdir " (and update workdir)")
			       (update-index " (and update index)")
			       (t "")))))
    (if (y-or-n-p prompt)
	(egg-do-move-head rev update-wdir update-index))))


(defun egg-log-buffer-rm-ref (pos &optional force)
  (interactive "d\nP")
  (let ((refs (get-text-property pos :references))
	(ref-at-point (car (get-text-property pos :ref)))
	victim)
    (unless ref-at-point
      (setq ref-at-point (last refs)))
    (setq victim (completing-read "remove reference: " refs
				  nil nil ref-at-point))
    (when (egg-rm-ref force victim)
      (funcall egg-buffer-refresh-func (current-buffer)))))

(defun egg-log-buffer-fetch-remote-ref (pos)
  (interactive "d")
  (let* ((ref-at-point (get-text-property pos :ref))
	 (ref (car ref-at-point))
	 (type (cdr ref-at-point))
	 name remote)
    (unless (eq type :remote)
      (error "Nothing to fetch from here!"))
    (setq name (file-name-nondirectory ref)
	  remote (directory-file-name
		  (file-name-directory ref)))
    (when (and remote name)
      (egg-buffer-async-do nil "fetch" remote 
			   (format "refs/heads/%s:refs/remotes/%s"
				   name ref)))))

(defun egg-log-buffer-push-to-local (pos &optional non-ff)
  (interactive "d\nP")
  (let* ((src (car (get-text-property pos :ref)))
	 dst)
    (unless src
      (error "Nothing to push here!"))
    (setq dst (completing-read (format "use %s to update: " src) 
			       (egg-local-refs) nil t))
    (when (egg-show-git-output
	   (egg-sync-0 "push" "." (if non-ff "-vf" "-v")
		       (concat src ":" dst))
	   -1 "GIT-PUSH")
      (funcall egg-buffer-refresh-func (current-buffer)))))

(defun egg-log-buffer-push-head-to-local (pos &optional non-ff)
  (interactive "d\nP")
  (let* ((dst (car (get-text-property pos :ref))))
    (unless dst
      (error "Nothing here to push to!"))
    (if (y-or-n-p (format "update %s with HEAD? " dst))
    (when (egg-show-git-output
	   (egg-sync-0 "push" "." (if non-ff "-vf" "-v")
		       (concat "HEAD:" dst))
	   -1 "GIT-PUSH")
      (funcall egg-buffer-refresh-func (current-buffer))))))

(defun egg-log-buffer-push-to-remote (pos &optional non-ff)
  (interactive "d\nP")
  (let* ((ref-at-point (get-text-property pos :ref))
	 (lref (car ref-at-point))
	 (type (cdr ref-at-point))
	 rref tracking remote spec)
    (unless ref-at-point
      (error "Nothing to push here!"))
    (cond ((eq type :remote)		;; delete a remote head
	   (setq rref (file-name-nondirectory lref))
	   (setq remote (directory-file-name
			 (file-name-directory lref)))
	   (setq lref ""))
	  ((eq type :head)
	   (setq tracking (egg-tracking-target lref :remote))
	   (if (consp tracking)
	       (setq rref (car tracking) remote (cdr tracking))
	     (setq remote (egg-read-remote
			   (format "push branch %s to remote: " lref)))
	     (setq rref (read-string 
			 (format "push branch % to %s as: " lref remote)
				     lref))))
	  ((eq type :tag)
	   (setq remote (egg-read-remote "push to remote: "))
	   (setq rref (read-string 
		       (format "remote tag to push at (on %s): " remote)
		       lref))
	   (setq lref (read-string 
		       (format "local tag to push at %s/%s (empty mean delete the remote tag) : "
			       remote rref)
		       rref))))
    (unless (> (length lref) 0)
      (unless (y-or-n-p (format "delete %s/%s? " remote rref))
	(message "cancel removal of %s/%s" remote rref)
	(setq remote nil rref nil lref nil)))
    (when (and remote rref lref)
      (setq spec (concat lref ":" rref))
      (egg-buffer-async-do nil "push" (if non-ff "-vf" "-v") 
			   remote spec))))

(defun egg-log-buffer-goto-pos (pos)
  (goto-char pos)
  (goto-char (line-beginning-position))
  (let ((sha1 (get-text-property (point) :commit)))
    (when (stringp sha1)
      (setq sha1 (substring sha1 0 6))
      (save-match-data
	(if (looking-at (concat "^.* \\(" sha1 "\\)"))
	    (goto-char (match-beginning 1)))))))

(defun egg-log-buffer-next-ref (pos)
  (interactive "d")
  (let ((current-ref (get-text-property pos :references))
	(n-pos (next-single-property-change pos :references))
	n-ref)
    (when n-pos
      (setq n-ref (get-text-property n-pos :references))
      (if n-ref
	  (egg-log-buffer-goto-pos n-pos)
	(if (setq n-pos (next-single-property-change n-pos :references))
	    (egg-log-buffer-goto-pos n-pos))))))

(defun egg-log-buffer-prev-ref (pos)
  (interactive "d")
  (let ((current-ref (get-text-property pos :references))
	(p-pos (previous-single-property-change pos :references))
	p-ref)
    (when p-pos
      (setq p-pos (1- p-pos))
      (setq p-ref (get-text-property p-pos :references))
      (if (and p-ref (not (equal p-ref current-ref)))
	  (egg-log-buffer-goto-pos p-pos)
	(when (setq p-pos (previous-single-property-change p-pos :references))
	  (setq p-pos (1- p-pos))
	  (egg-log-buffer-goto-pos p-pos))))))

(defun egg-log-buffer-do-insert-commit (pos)
  (save-excursion
    (let ((sha1 (get-text-property pos :commit))
	  (ref (get-text-property pos :references))
	  (nav (get-text-property pos :navigation))
	  (inhibit-read-only t)
	  (indent-column egg-log-buffer-comment-column)
	  (indent-spaces (make-string egg-log-buffer-comment-column ? ))
	  beg end)
      (goto-char pos)
      (goto-char (1+ (line-end-position)))
      (setq beg (point))
      (unless (egg-git-ok t "log" "--max-count=1" "-p"
			  (concat
			     "--pretty=format:"
			     indent-spaces "%ai%n"
			     indent-spaces "%an%n%n"
			     "%b%n" 
			     ) 
			  sha1)
	(error "error calling git log %s!" ref))
      (setq end (point))
      (egg-delimit-section :commit sha1 beg end (1- beg) nil nav)
      (put-text-property beg end 'keymap egg-section-map)
      (egg-decorate-diff-section :begin beg
				 :end end
				 :diff-map egg-log-diff-map
				 :hunk-map egg-log-hunk-map)
      (goto-char beg)
      (setq end (next-single-property-change beg :diff))
      (put-text-property beg (+ indent-column beg) 'face 'egg-diff-none)
      (put-text-property (+  indent-column beg) (line-end-position)
			 'face 'egg-text-2)
      (forward-line 1)
      (put-text-property (point) (+ indent-column (point)) 'face 'egg-diff-none)
      (put-text-property (+ indent-column (point)) end 'face 'egg-text-2)
      (set-buffer-modified-p nil))))

(defun egg-log-buffer-insert-commit (pos)
  (interactive "d")
  (let* ((next (next-single-property-change pos :diff))
	 (sha1 (and next (get-text-property next :commit))))
    (unless (equal (get-text-property pos :commit) sha1)
      (egg-log-buffer-do-insert-commit pos))))

(defun egg-generic-display-logs (data)
  (buffer-disable-undo)
  (setq buffer-invisibility-spec nil)
  (let ((title (plist-get data :title))
	(subtitle (plist-get data :subtitle))
	(git-dir (egg-git-dir))
	(desc (plist-get egg-internal-log-buffer-closure :description))
	(closure (plist-get egg-internal-log-buffer-closure :closure))
	(inhibit-read-only t)
	inv-beg beg)
      (erase-buffer)
      (insert title
	      (if subtitle (concat "\n" subtitle "\n") "\n")
	      (propertize (egg-git-dir) 'face 'font-lock-constant-face)
	      (if desc (concat "\n" desc "\n") "\n")
	      "\n")
      (setq beg (point))
      (setq inv-beg (- beg 2))
      (funcall closure)
      (goto-char beg)))

(defun egg-log-buffer-redisplay (buffer)
  (with-current-buffer buffer
    (let* ((state (egg-repo-state))
	   (sha1 (plist-get state :sha1)))
      (plist-put egg-internal-log-buffer-closure :title
		 (propertize (egg-pretty-head-string state) 'face 'egg-branch))
      (plist-put egg-internal-log-buffer-closure :subtitle
		 (propertize sha1 'face 'font-lock-string-face))
      (egg-generic-display-logs egg-internal-log-buffer-closure))))

(defun egg-log-buffer-redisplay-from-command (buffer)
  ;; in process buffer
  (when (and (processp egg-async-process)
	     (equal (current-buffer) (process-buffer egg-async-process)))
    (or 
     (save-excursion
       (goto-char (point-min))
       (save-match-data
	 (when (re-search-forward "^\\(?:From\\|To\\) \\(.+\\)\n\\(.+\\)$" nil t)
	   (message "%s: %s" (match-string-no-properties 1)
		    (match-string-no-properties 2)))))))
  ;; update log buffer
  (egg-log-buffer-redisplay buffer))

(define-egg-buffer log "*%s-log@%s*"
  "Major mode to display the output of git log.\\<egg-log-buffer-mode-map>
Each line with a shorten sha1 representing a commit in the repo's history.
\\[egg-log-buffer-next-ref] move the cursor to the next commit with a ref
\\[egg-log-buffer-prev-ref] move the cursor to the previous commit line with a ref.
\\[egg-buffer-cmd-refresh] refresh the display of the log buffer
\\[egg-status] shows the repo's current status.

\\{egg-log-buffer-mode-map}

Each line representing a commit has extra keybindings:\\<egg-log-commit-map>
\\[egg-log-buffer-insert-commit] fetch and show the commit's details.
\\[egg-section-cmd-toggle-hide-show] hide/show the current commit's details
\\[egg-section-cmd-toggle-hide-show-children] hide all the sub-blocks of the current commit's details.
\\[egg-log-buffer-create-new-branch] create a new branch starting from the current commit.
\\[egg-log-buffer-start-new-branch] start in a new branch from the current commit.
\\[egg-log-buffer-checkout-commit] checkout the current commit.
\\[egg-log-buffer-tag-commit] create a new lightweight tag pointing at the current commit.
\\[egg-log-buffer-attach-head] move HEAD (and maybe the current branch tip) to the 
current commit (the underlying git command is `reset --soft'.
C-u \\[egg-log-buffer-attach-head] move HEAD (and maybe the current branch tip) as well as
the index to the current commit (the underlying git command
is `reset --mixed'.)
C-u C-u \\[egg-log-buffer-attach-head] move HEAD (and maybe the current branch tip) and
the index to the current commit, the work dir will also be
updated (the underlying git command is `reset --hard').
\\[egg-log-buffer-merge] will merge the current commit into HEAD.
C-u \\[egg-log-buffer-merge] will merge the current commit into HEAD but will not
auto-commit if the merge was successful.

\\{egg-log-commit-map}

Each ref on the commit line has extra extra keybindings:\\<egg-log-ref-map>
\\[egg-log-buffer-rm-ref] delete the ref under the cursor.
\\[egg-log-buffer-push-to-local] update another local ref using the ref under the cursor.

Each local ref on the commit line has extra extra extra keybindings:\\<egg-log-local-ref-map>
\\[egg-log-buffer-push-to-remote] upload to a remote the ref under the cursor.
  for a remote-tracking local branch this would updating the tracking target.
  for other local refs this  means uploading (or deleting) the local value
   of the ref to the remote repository.
\\[egg-log-buffer-push-head-to-local] update the local ref under the cursor with the current HEAD.

Each remote ref on the commit line has extra extra extra keybindings:\\<egg-log-remote-ref-map>
\\[egg-log-buffer-fetch-remote-ref] download the new value of the ref from the remote repo.
."
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq major-mode 'egg-log-buffer-mode
	mode-name  "Egg-Log"
	mode-line-process ""
	truncate-lines t)
  (use-local-map egg-log-buffer-mode-map)
  (set (make-local-variable 'egg-buffer-refresh-func)
       'egg-log-buffer-redisplay)
  (set (make-local-variable 'egg-buffer-async-cmd-refresh-func)
       'egg-log-buffer-redisplay-from-command)
  (set (make-local-variable 'egg-log-buffer-comment-column) 0)
  (setq buffer-invisibility-spec nil)
  (run-mode-hooks 'egg-log-buffer-mode-hook))

(defun egg-log (&optional all)
  (interactive "P")
  (let* ((git-dir (egg-git-dir))
	 (default-directory (file-name-directory git-dir))
	 (buf (egg-get-log-buffer 'create)))
    (with-current-buffer buf
      (set 
       (make-local-variable 'egg-internal-log-buffer-closure)
       (if all
	   (list :description (concat 
			       (propertize "history of: " 'face 'egg-text-2)
			       (propertize "all refs" 'face 'egg-term))
		 :closure (lambda ()
			    (egg-log-buffer-insert-n-decorate-logs
			     'egg-run-git-log-all)))
	 (list :description (concat 
			     (propertize "history of: " 'face 'egg-text-2)
			     (propertize "HEAD" 'face 'egg-term))
	       :closure (lambda ()
			  (egg-log-buffer-insert-n-decorate-logs
			   'egg-run-git-log-HEAD)))))
      (egg-log-buffer-redisplay buf))
    (pop-to-buffer buf t)))
;;;========================================================
;;; commit search
;;;========================================================
(defconst egg-query:commit-commit-map 
  (let ((map (make-sparse-keymap "Egg:LogCommit")))
    (set-keymap-parent map egg-hide-show-map)
    (define-key map (kbd "SPC") 'egg-log-buffer-insert-commit)
    (define-key map (kbd "o") 'egg-log-buffer-checkout-commit)
    (define-key map (kbd "a") 'egg-log-buffer-attach-head)
    (define-key map (kbd "RET") 'egg-query:commit-locate)
    (define-key map (kbd "C-c C-c") 'egg-query:commit-locate)
    map))

(defun egg-query:commit-locate (pos)
  (interactive "d")
  (let ((sha1 (get-text-property pos :commit))
	(buf (egg-get-log-buffer 'create)))
    (with-current-buffer buf
      (set (make-local-variable 'egg-internal-log-buffer-closure)
	   (list :description 
		 (concat (propertize "history of: " 'face 'egg-text-2)
			 (propertize "HEAD" 'face 'egg-term)
			 (propertize " and " 'face 'egg-text-2)
			 (propertize sha1 'face 'egg-term))
		 :closure 
		 (lambda ()
		   (egg-log-buffer-insert-n-decorate-logs
		    `(lambda ()
		       (egg-git-ok t "log" "--max-count=10000" "--graph"
				   "--topo-order" "--pretty=oneline"
				   "--decorate" "HEAD" sha1)))))) 
      (egg-log-buffer-redisplay buf)
      (setq pos (point-min))
      (while (and pos
		  (not (equal (get-text-property pos :commit) sha1)))
	(setq pos (next-single-property-change pos :commit))))
    (pop-to-buffer buf t)
    (egg-log-buffer-goto-pos pos)
    (recenter)))

(defun egg-query:commit-buffer-rerun (buffer)
  (interactive (list (current-buffer)))
  (with-current-buffer buffer
    (plist-put egg-internal-log-buffer-closure :title
	       (propertize "History Search" 'face 'egg-branch))
    (egg-generic-display-logs egg-internal-log-buffer-closure)))

(define-egg-buffer query:commit "*%s-query:commit@%s*"
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq major-mode 'egg-query:commit-buffer-mode
	mode-name  "Egg-Query:Commit"
	mode-line-process ""
	truncate-lines t)
  (use-local-map egg-buffer-mode-map)
  (set (make-local-variable 'egg-internal-log-buffer-closure) nil)
  (set (make-local-variable 'egg-buffer-refresh-func)
       'egg-query:commit-buffer-rerun)
  ;; re use log-buffer redrawing
  (set (make-local-variable 'egg-log-buffer-comment-column) 0)
  (setq buffer-invisibility-spec nil)
  (run-mode-hooks 'egg-query:commit-buffer-mode-hook))

(defun egg-insert-n-decorate-pickaxed-logs (string)
  (let ((beg (point)))
    (egg-git-ok t "log" "--pretty=oneline" "--decorate" 
		(concat "-S" string))
    (goto-char beg)
    (egg-decorate-log egg-query:commit-commit-map)))

(defun egg-search-changes (string)
  (interactive "ssearch history for changes containing: ")
  (let* ((git-dir (egg-git-dir))
	 (default-directory (file-name-directory git-dir))
	 (buf (egg-get-query:commit-buffer 'create))
	 (desc (concat (propertize "Commits containing: "
				   'face 'egg-text-2)
		       (propertize string 'face 'egg-term)))
	 (func `(lambda ()
		  (egg-insert-n-decorate-pickaxed-logs ,string))))
    (with-current-buffer buf
      (set (make-local-variable 'egg-internal-log-buffer-closure)
	 (list :description desc :closure func))
      (egg-query:commit-buffer-rerun buf))			 
    (pop-to-buffer buf t)))
;;;========================================================
;;; minor-mode
;;;========================================================
(defun egg-file-diff (&optional ask)
  "Diff the current file in another window."
  (interactive "P")
  (unless (buffer-file-name)
    (error "Current buffer has no associated file!"))
  (let ((git-file (egg-buf-git-name))
	(src-rev (and ask (egg-read-rev "diff against: " "HEAD")))
	buf)
    (setq buf (egg-do-diff (egg-build-diff-info src-rev nil git-file))) 
    (pop-to-buffer buf t)))

(defun egg-file-checkout-other-version (&optional no-confirm)
  "Checkout HEAD's version of the current file.
if CONFIRM-P was not null, then ask for confirmation if the
current file contains unstaged changes."
  (interactive "P")
  (unless (buffer-file-name)
    (error "Current buffer has no associated file!"))
  (let* ((file (buffer-file-name))
	 (file-modified (not (egg-file-committed (buffer-file-name))))
	 rev)
    (when file-modified
      (unless (y-or-n-p (format "ignored uncommitted changes in %s? " file))
	(error "File %s contains uncommitted changes!" file)))
    (setq rev (egg-read-rev (format "checkout %s version: " file) "HEAD"))
    (when (egg-sync-do-file file "git" nil nil (list "checkout" rev "--" file))
      (revert-buffer t t t))))

(defun egg-file-cancel-modifications (&optional no-confirm)
  "Checkout INDEX's version of the current file.
if CONFIRM-P was not null, then ask for confirmation if the
current file contains unstaged changes."
  (interactive "P")
  (unless (buffer-file-name)
    (error "Current buffer has no associated file!"))
  (let* ((file (buffer-file-name))
	 (file-modified (not (egg-file-updated (buffer-file-name))))
	 rev)
    (when (and file-modified (not no-confirm))
      (unless (y-or-n-p (format "ignored unstaged changes in %s? " file))
	(error "File %s contains unstaged changes!" file)))
    (when (egg-sync-do-file file "git" nil nil (list "checkout" "--" file))
      (revert-buffer t t t))))

(defun egg-start-new-branch ()
  (interactive)
  (egg-do-create-branch nil 'checkout "start new branch with name: "))

(defun egg-file-get-other-version (file &optional rev prompt same-mode)
  (let* ((mode (assoc-default file auto-mode-alist 'string-match))
	 (git-dir (egg-git-dir))
	 (lbranch (egg-current-branch))
	 (rbranch (and git-dir (or (egg-tracking-target lbranch)
				   rev ":0")))
	 (prompt (or prompt (format "%s's version: " file)))
	 (rev (or rev (egg-read-rev prompt rbranch ":0")))
	 (canon-name (egg-file-git-name file))
	 (git-name (concat rev ":" canon-name))
	 (buf (get-buffer-create (concat "*" git-name "*"))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
	(erase-buffer)
	(unless (= (call-process "git" nil buf nil "show" git-name)
		   0)
	  (error "Failed to get %s's version: %s" file rev))
	(when (and (functionp mode) same-mode)
	  (funcall mode))
	(set-buffer-modified-p nil)
	(setq buffer-read-only t)))
    buf))

(defun egg-file-version-other-window (&optional ask)
  "Show other version of the current file in another window."
  (interactive "P")
  (unless (buffer-file-name)
    (error "Current buffer has no associated file!"))
  (let ((buf (egg-file-get-other-version
	      (buffer-file-name) (if ask nil ":0")
	      (format "show %s's version:" (buffer-file-name))
	      t)))
    (unless (bufferp buf)
      (error "Oops! can't get %s older version" (buffer-file-name)))
    (pop-to-buffer buf t)))

(defun egg-file-ediff (&optional ask-for-dst)
  (interactive "P")
  (unless (buffer-file-name)
    (error "Current buffer has no associated file!"))
  (let* ((file buffer-file-name)
	 (dst-buf (if ask-for-dst
		      (egg-file-get-other-version
		       (buffer-file-name) nil
		       (format "(ediff) %s's newer version: " file)
		       t)
		    (current-buffer)))
	 (src-buf (egg-file-get-other-version 
		   (buffer-file-name)
		   nil
		   (format "(ediff) %s's older version: " file)
		   t)))
    (unless (and (bufferp dst-buf) (bufferp src-buf))
      (error "Ooops!"))
    (ediff-buffers dst-buf src-buf)))

(defun egg-resolve-merge-with-ediff (&optional what)
  (interactive "P")
  (unless (buffer-file-name)
    (error "Current buffer has no associated file!"))
  (let* ((file buffer-file-name)
	 (ours (egg-file-get-other-version file ":2" nil t))
	 (theirs (egg-file-get-other-version file ":3" nil t)))
    (unless (and (bufferp ours) (bufferp theirs))
      (error "Ooops!"))
    (ediff-buffers3 ours (current-buffer) theirs)))

(defconst egg-key-action-alist 
  '((?b :new-branch "start new [b]ranch" "Create and switch to a new branching starting from HEAD.")
    (?s :status "show repo's [s]tatus" "Browse the current status of the repo" )
    (?f :stage-file "stage current [f]ile" "Stage current file's changes")
    (?a :stage-all "stage [a]ll files" "Stage all current changes inside this repo.")
    (?d :diff-file "[d]iff current file" "Compare the current file against the staged snapshot.")
    (?c :commit "[c]ommit staged changes" "Process to commit current staged changes onto HEAD.")
    (?? :more-options "[?] more options" nil)
    (?q :quit "[q] quit" nil)))

(defconst egg-action-function-alist
  '((:new-branch . egg-start-new-branch)
    (:status     . egg-status)
    (:stage-file . egg-file-stage-current-file)
    (:stage-all  . egg-stage-all-files)
    (:diff-file  . egg-file-diff)
    (:commit     . egg-commit-log-edit)
    (:quit 	 . (lambda () (message "do nothing now! later.") (ding) nil))))

(defcustom egg-confirm-next-action t
  "Always prompt for confirmation while guessing the next logical action ."
  :group 'egg
  :type 'boolean)

(defconst egg-electrict-select-action-buffer 
  (get-buffer-create "*Egg:Select Action*"))

(defun egg-select-action-run ()
  (interactive)
  (let (action)
    (save-excursion
      (with-current-buffer egg-electrict-select-action-buffer
	(beginning-of-line)
	(when (boundp 'egg-electric-in-progress-p)
	  (setq action (get-text-property (point) :action))
	  (if action
	      (throw 'egg-select-action action)
	    (ding)))))))

(defun egg-select-action-quit ()
  (interactive)
  (let (action)
    (save-excursion
      (with-current-buffer egg-electrict-select-action-buffer 
	(beginning-of-line)
	(when (boundp 'egg-electric-in-progress-p)
	  (throw 'egg-select-action nil))))))

(defconst egg-electric-select-action-map 
  (let ((map (make-sparse-keymap "Egg:SelectAction")))
    (define-key map "q" 'egg-select-action-quit)
    (define-key map (kbd "RET") 'egg-select-action-run)
    (define-key map (kbd "SPC") 'egg-select-action-run)
    (define-key map (kbd "C-l") 'recenter)
    map))

(defun egg-electric-select-action (default desc)
  (let ((egg-electric-in-progress-p t)
	(old-buffer (current-buffer))
	(buf egg-electrict-select-action-buffer)
	(action-alist
	 (delq nil (mapcar (lambda (entry)
			     (if (cadddr entry)
				 (cons (cadr entry)
				       (cadddr entry))))
			   egg-key-action-alist)))
	action default-entry beg)
    (setq default-entry (assq default action-alist))
    (setq action-alist
	  (cons default-entry (remq default-entry action-alist)))
    (unwind-protect
	(setq action
	      (catch 'egg-select-action
		(save-window-excursion
		  (with-current-buffer buf
		    (let ((inhibit-read-only t))
		      (erase-buffer)
		      (insert (propertize "Select Action\n" 'face
					  'egg-section-title))
		      (insert (propertize desc 'face 'egg-text-1) "\n\n")
		      (insert (propertize "select an action:" 
					  'face 'egg-text-1)
			      "\n\n")
		      (put-text-property (point-min) (point)
					 'intangible t)
		      (setq beg (point))
		      (insert 
		       (mapconcat
			(lambda (entry)
			  (propertize (concat "- " (cdr entry))
				      :action (car entry)
				      'face 'egg-electrict-choice))
			action-alist
			"\n")
		       "\n")
		      (goto-char beg)
		      (set-buffer-modified-p nil) 
		      (setq buffer-read-only t))
		    (setq major-mode 'egg-select-action)
		    (setq mode-name "Egg-Select")
		    (use-local-map egg-electric-select-action-map)) 
		  (Electric-pop-up-window egg-electrict-select-action-buffer)
		  (goto-char beg)
		  (Electric-command-loop 'egg-select-action
					 "select next action> "))))
      (bury-buffer buf))
    (when (and action (symbolp action))
      action)))

(defun egg-guess-next-action (file-is-modified
			      wdir-is-modified
			      index-is-clean)
  (if file-is-modified
      :stage-file 
    ;; file is unchanged
    (if wdir-is-modified
	:stage-all
      ;; wdir is clean
      (if index-is-clean
	  :new-branch
	:commit))))

(defun egg-build-key-prompt (prefix default alternatives)
  (let ((action-desc-alist (mapcar 'cdr egg-key-action-alist)))
    (concat prefix " default: "
	    (nth 1 (assq default action-desc-alist))
	  ". alternatives:  "
	  (mapconcat 'identity 
		     (mapcar (lambda (action)
			       (nth 1 (assq action action-desc-alist)))
			     (remq default alternatives)) ", "))))

(defun egg-prompt-next-action (file-modified
			       wdir-modified
			       index-clean)
  (let ((default (egg-guess-next-action file-modified
					wdir-modified
					index-clean))
	desc key action alternatives)
    (setq alternatives (list default :status :more-options))
    (while (null action)
      (setq key (read-key-sequence 
		 (egg-build-key-prompt "next action?"
				       default alternatives)))
      (setq key (string-to-char key))
      (setq action 
	    (if  (memq key '(?\r ?\n ?\ ))
		default 
	      (cadr (assq key egg-key-action-alist))))
      (when (eq action :more-options)
	(setq desc
	      (format "%s %s\n%s %s\nINDEX %s"
		      (buffer-file-name)
		      (if file-modified 
			  "contains unstaged changes"
			"is not modified")
		      (file-name-directory (egg-git-dir))
		      (if wdir-modified
			  "contains unstaged changes"
			"has no unstaged changes")
		      (if index-clean "is identical to HEAD"
			"contains staged changes to commit")))
	(setq action (egg-electric-select-action default desc)))
      (when (null action)
	(ding)))
    action))

(defun egg-next-action (&optional ask)
  (interactive "P")
  (save-some-buffers nil 'egg-is-in-git)
  (let ((file-modified (not (egg-file-updated (buffer-file-name))))
	(wdir-modified (not (egg-wdir-clean)))
	(index-clean (egg-index-empty))
	action default)
     (setq action (if (or ask egg-confirm-next-action)
		      (egg-prompt-next-action file-modified
					      wdir-modified
					      index-clean)
		    (egg-guess-next-action file-modified
					   wdir-modified
					   index-clean)))
     
     (funcall (cdr (assq action egg-action-function-alist)))))

(defvar egg-minor-mode nil)
(defvar egg-minor-mode-map (make-sparse-keymap "Egg"))
(defvar egg-file-cmd-map (make-sparse-keymap "Egg:File"))

(defun egg-mode-key-prefix-set (var val)
  (define-key egg-minor-mode-map (read-kbd-macro val) egg-file-cmd-map)
  (custom-set-default var val))

(let ((map egg-file-cmd-map))
  (define-key map (kbd "a") 'egg-blame)
  (define-key map (kbd "b") 'egg-start-new-branch)
  (define-key map (kbd "d") 'egg-status)
  (define-key map (kbd "c") 'egg-commit-log-edit)
  (define-key map (kbd "i") 'egg-file-stage-current-file)
  (define-key map (kbd "l") 'egg-log)
  (define-key map (kbd "o") 'egg-file-checkout-other-version)
  (define-key map (kbd "s") 'egg-status)
  (define-key map (kbd "u") 'egg-file-cancel-modifications)
  (define-key map (kbd "v") 'egg-next-action)
  (define-key map (kbd "w") 'egg-commit-log-edit)
  (define-key map (kbd "=") 'egg-file-diff)
  (define-key map (kbd "~") 'egg-file-version-other-window))

(defcustom egg-mode-key-prefix "C-x v"
  "Prefix keystrokes for egg minor-mode commands."
  :group 'egg
  :type 'string
  :set 'egg-mode-key-prefix-set)

(defvar egg-minor-mode-name " Git")

;;;###autoload
(defun egg-minor-mode (&optional arg)
  "Turn-on egg-minor-mode which would enable key bindings for
egg in current buffer.\\<egg-minor-mode-map>
\\[egg-start-new-branch] start a new branch from the current HEAD.
\\[egg-status] shows the repo's current status
\\[egg-commit-log-edit] start editing the commit message for the current staged changes.
\\[egg-file-stage-current-file] stage new changes of the current file 
\\[egg-log] shows repo's history 
\\[egg-file-checkout-other-version] checkout another version of the current file 
\\[egg-file-cancel-modifications] delete unstaged modifications in the current file
\\[egg-next-action] perform the next logical action
\\[egg-file-diff] compare file with index or other commits
\\[egg-file-version-other-window] show other version of the current file.

\\{egg-minor-mode-map}
"
  (interactive "p")
  (setq egg-minor-mode (if (null arg)
			     (not egg-minor-mode)
			   (> arg 0)))
  (when egg-minor-mode
    (if (boundp 'vc-mode)
	(set 'vc-mode nil))
    (make-local-variable 'egg-minor-mode-name) 
    (setq egg-minor-mode-name 
	  (intern (concat "egg-" (egg-git-dir) "-HEAD")))))

;;;###autoload
(defun egg-minor-mode-find-file-hook ()
  (when (egg-is-in-git)
    (make-local-variable 'egg-minor-mode)
    (egg-minor-mode 1)))

(or (assq 'egg-minor-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(egg-minor-mode egg-minor-mode-name) minor-mode-alist)))

(setcdr (or (assq 'egg-minor-mode minor-mode-map-alist)
	    (car (setq minor-mode-map-alist
		       (cons (list 'egg-minor-mode)
			     minor-mode-map-alist))))
	egg-minor-mode-map)

(if (and (boundp 'vc-handled-backends)
	 (listp (symbol-value 'vc-handled-backends)))
    (set 'vc-handled-backends
	 (delq 'Git (symbol-value 'vc-handled-backends))))


(add-hook 'find-file-hook 'egg-git-dir)
(add-hook 'find-file-hook 'egg-minor-mode-find-file-hook)

(provide 'egg)
