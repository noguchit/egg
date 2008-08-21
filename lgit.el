;;; lgit -- a magit fork

;; Copyright (C) 2008  Marius Vollmer
;; Copyright (C) 2008  Linh Dang
;;
;; LGit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; LGit is distributed in the hope that it will be useful, but WITHOUT
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

(require 'parse-time)
(require 'autorevert)

(defgroup lgit nil
  "Controlling Git from Emacs."
  :prefix "lgit-"
  :group 'tools)

(defface lgit-header
  '((t :weight bold :inherit variable-pitch :height 1.3))
  "Face for generic headers.

Many Lgit faces inherit from this one by default."
  :group 'lgit)

(defface lgit-section-title
  '((((class color) (background light))
     :foreground "DarkGoldenrod" :inherit lgit-header :height 1.1)
    (((class color) (background dark))
     :foreground "PaleGreen" :inherit lgit-header :height 1.1)
    (t :weight bold))
  "Face for generic header lines.

Many Lgit faces inherit from this one by default."
  :group 'lgit)

(defface lgit-branch
  '((((class color) (background light))
     :foreground "SkyBlue" :inherit lgit-header :height 1.4)
    (((class color) (background dark))
     :foreground "Yellow" :inherit lgit-header :height 1.4)
    (t :weight bold))
  "Face for the current branch."
  :group 'lgit)

(defface lgit-diff-file-header
  '((((class color) (background light))
     :foreground "SlateBlue" :inherit lgit-header)
    (((class color) (background dark))
     :foreground "LightSlateBlue" :inherit lgit-header)
    (t :weight bold))
  "Face for diff file headers."
  :group 'lgit)

(defface lgit-diff-hunk-header
  '((((class color) (background light))
     :background "grey85")
    (((class color) (background dark))
     :background "grey45"))
  "Face for diff hunk headers."
  :group 'lgit)

(defface lgit-diff-add
  '((((class color) (background light))
     :foreground "blue1")
    (((class color) (background dark))
     :foreground "white"))
  "Face for lines in a diff that have been added."
  :group 'lgit)

(defface lgit-diff-none
  '((((class color) (background light))
     :foreground "grey50")
    (((class color) (background dark))
     :foreground "grey30"))
  "Face for lines in a diff that are unchanged."
  :group 'lgit)

(defface lgit-diff-del
  '((((class color) (background light))
     :foreground "red")
    (((class color) (background dark))
     :foreground "OrangeRed"))
  "Face for lines in a diff that have been deleted."
  :group 'lgit)

(defface lgit-item-highlight
  '((((class color) (background light))
     :foreground "gray95")
    (((class color) (background dark))
     :foreground "gray30"))
  "Face for highlighting the current item."
  :group 'lgit)

(defcustom lgit-diff-init-hiding-mode nil
  "Initial hiding mode for diff results."
  :group 'lgit
  :type '(choice :tag "Initial Hiding Mode"
		 (const :tag "Hide Nothing" nil)
		 (const :tag "Hide Everything" t)))

;;;========================================================
;;; simple routines
;;;========================================================

(defun lgit-cmd-to-string-1 (program args)
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

(defsubst lgit-cmd-to-string (program &rest args)
  "Execute PROGRAM and return its output as a string.
ARGS is a list of arguments to pass to PROGRAM."
  (lgit-cmd-to-string-1 program args))


(defun lgit-git-to-string (&rest args)
  (let* ((str (lgit-cmd-to-string-1 "git" args))
	 (len (length str)))
    (when (> len 0)
      (if (eq (aref str (1- len)) ?\n)
	  (substring str 0 -1)
	str))))

(defsubst lgit-git-to-lines (&rest args)
  (split-string (substring (lgit-cmd-to-string-1 "git" args) 0 -1)
		"\n"))

(defsubst lgit-local-branches ()
  "Get a list of local branches. E.g. (\"master\", \"wip1\")."
  (lgit-git-to-lines "rev-parse" "--symbolic" "--branches"))

(defun lgit-remote-branches (&optional raw-p)
  "Get a list of local branches. E.g. (\"origin/master\", \"joe/fork1\")."
  (let ((lst (lgit-git-to-lines "rev-parse" "--symbolic" "--remotes")))
    (if raw-p lst
      (mapcar (lambda (full-name)
		(let ((tmp (split-string full-name "/")))
		  (cons (cadr tmp) (car tmp))))
	      lst))))

(defsubst lgit-rbranch-to-remote (rbranch)
  (and (stringp rbranch)
       (> (length rbranch) 0)
       (car (split-string rbranch "/"))))

(defsubst lgit-rbranch-name (rbranch)
  (and (stringp rbranch) 
       (> (length rbranch) 0)
       (cadr (split-string rbranch "/"))))

(defsubst lgit-push-refspec (lbranch rbranch)
   (setq rbranch (lgit-rbranch-name rbranch))
   (if (or lbranch rbranch)
       (format "%s%s%s" (or lbranch "") (if rbranch ":" "") (or rbranch ""))))

(defun lgit-file-as-string-raw (file-name)
  (with-temp-buffer
    (insert-file-contents-literally file-name)
    (buffer-string)))

(defun lgit-pick-file-contents (file-name regexp &rest indices)
  (with-temp-buffer
    (insert-file-contents-literally file-name)
    (goto-char (point-min))
    (when (re-search-forward regexp nil t)
      (if (null indices)
	  (match-string-no-properties 0)
	(dolist (idx indices)
	  (if (match-beginning idx)
	      (return (match-string-no-properties idx))))))))

(defun lgit-file-as-string (file-name)
  (let ((str (lgit-file-as-string-raw file-name)))
    (if (> (length str) 0)
	(substring str 0 -1)
      str)))

(defun lgit-parse-config-contents (config-as-string)
  (save-match-data
    (mapcar 
     (lambda (chunk) 
       (let* ((parts (split-string chunk "[\t\n]+" t))
	      (key (car parts))
	      (info-list (cdr parts))
	      type info)
	 (when (string-match "\\`\\(\\S-+\\)\\(?: +\"\\(.+\\)\"\\)?\\'" key)
	   (setq type (if (match-beginning 2)
			 (cons (intern (match-string 1 key))
			       (match-string 2 key)) 
		       (intern (match-string 1 key)))))
	 (setq info
	       (mapcar (lambda (entry)
			 (split-string entry "[ =]+" t))
		       info-list))
	 (cons type info)))
     (split-string (subst-char-in-string ?\] ?\t
					 (progn
					   (set-text-properties 0 (length config-as-string)
								nil
								config-as-string)
					   config-as-string))
		   "\\[" t))))

(defsubst lgit-is-in-git ()
  (= (call-process "git" nil nil nil "rev-parse" "--git-dir") 0))

(defsubst lgit-is-dir-in-git (dir)
  (let ((default-directory dir)) (lgit-is-in-git)))

(defsubst lgit-name-rev (rev)
  (lgit-git-to-string "name-rev" "--always" "--name-only" rev))

(defun lgit-read-git-dir ()
  (let ((dir (lgit-git-to-string "rev-parse" "--git-dir")))
    (if (stringp dir) 
	(expand-file-name dir))))

(defsubst lgit-read-dir-git-dir (dir)
  (let ((default-directory dir)) (lgit-read-git-dir)))

(defvar lgit-git-dir nil)
(defsubst lgit-git-dir ()
  (if (stringp lgit-git-dir)
      lgit-git-dir
    (set (make-local-variable 'lgit-git-dir) (lgit-read-git-dir))))

(defsubst lgit-buf-git-dir (buffer)
  (with-current-buffer buffer
    (lgit-git-dir)))

(defun lgit-HEAD ()
  (let* ((git-dir (lgit-git-dir))) 
    (if git-dir
	(lgit-pick-file-contents (concat git-dir "/HEAD")
				 "^ref: refs/heads/\\(.+\\)\\|^\\([0-9a-z]+\\)" 1 2))))

(defsubst lgit-current-branch ()
  (let* ((git-dir (lgit-git-dir))) 
    (if (stringp git-dir)
	(lgit-pick-file-contents (concat git-dir "/HEAD")
				 "^ref: refs/heads/\\(.+\\)" 1))))

(defsubst lgit-current-sha1 ()
  (lgit-git-to-string "rev-parse" "--verify" "-q" "HEAD"))

(defsubst lgit-head ()
  (if (lgit-git-dir)
      (cons (lgit-current-sha1) (lgit-current-branch))))

(defun lgit-config-section-raw (type &optional name)
  (lgit-pick-file-contents (concat (lgit-git-dir) "/config")
			   (concat "^"
				   (if name (format "\\[%s \"%s\"\\]" type name)
				     (format "\\[%s\\]" type))
				   "\n"
				   "\\(\\(:?\t.+\n\\)+\\)")
			   1))

(defsubst lgit-config-section (type &optional name)
  (mapcar 
   (lambda (line) (split-string line "[ =]+" t))
   (split-string (lgit-config-section-raw type name) "[\t\n]+" t)))

(defsubst lgit-config-get (type attr &optional name)
  (and (lgit-git-dir)
       (cadr (assoc attr (lgit-config-section type name)))))

(defun lgit-bool-config (type attr &optional name)
  (let ((flag (lgit-config-get type attr name)))
    (cond ((equal flag "true")
	   t)
	  ((equal flag "false")
	   nil)
	  (t (error "Unexpected contents of boolean config %s of %s.%s"
		    attr type name)))))


;;;========================================================
;;; hooks
;;;========================================================

(add-hook 'find-file-hook 'lgit-git-dir)

;;;========================================================
;;; Async Git process
;;;========================================================

(defun lgit-async-do (exit-code func-args args)
  "Run GIT asynchronously with ARGS.
if EXIT code is an exit-code from GIT other than zero but considered
success."
  (let ((dir default-directory)
	(buf (get-buffer-create "*lgit-process*"))
	(inhibit-read-only inhibit-read-only)
	(accepted-msg (and (integerp exit-code)
			   (format "exited abnormally with code %d"
				   exit-code)))
	proc)
    (setq proc (get-buffer-process buf))
    (when (and (processp proc) 		;; is a process
	       (not (eq (process-status proc) 'exit)) ;; not finised
	       (= (process-exit-status proc) 0))      ;; still running
      (error "LGIT: %s is already running!" (process-command proc)))
    (with-current-buffer buf
      (setq inhibit-read-only t)
      (setq default-directory dir)
      ;;(erase-buffer)
      (widen)
      (goto-char (point-max))
      (insert "LGIT-GIT-CMD:\n")
      (insert (format "%S\n" args))
      (insert "LGIT-GIT-OUTPUT:\n")
      (setq proc (apply 'start-process "lgit-git" buf "git" args))
      (setq mode-line-process " git")
      (when (and (consp func-args) (functionp (car func-args)))
	(process-put proc :callback-func (car func-args))
	(process-put proc :callback-args (cdr func-args)))
      (when (stringp accepted-msg)
	(process-put proc :accepted-msg accepted-msg)
	(process-put proc :accepted-code exit-code))
      (process-put proc :cmds (cons "git" args))
      (set-process-sentinel proc #'lgit-process-sentinel))))

(defun lgit-process-sentinel (proc msg)
  (let ((exit-code (process-get proc :accepted-code))
	(accepted-msg (process-get proc :accepted-msg))
	(callback-func (process-get proc :callback-func))
	(callback-args (process-get proc :callback-args))
	(cmds (process-get proc :cmds)))
    (cond ((string= msg "finished\n")
	   (message "LGIT: git finished."))
	  ((string= msg "killed\n")
	   (message "LGIT: git was killed."))
	  ((string-match accepted-msg msg)
	   (message "LGIT: git exited with code: %d." exit-code))
	  ((string-match "exited abnormally" msg)
	   (message "LGIT: git failed."))
	  (t (message "LGIT: git is weird!")))
    (with-current-buffer (process-buffer proc)
      (setq mode-line-process nil)
      (widen)
      (goto-char (point-max))
      (re-search-backward "^LGIT-GIT-CMD:" nil t)
      (narrow-to-region (point) (point-max))
      (if (functionp callback-func)
	  (apply callback-func proc cmds callback-args)))))

(defun lgit-async-callback-single-file (proc cmds))

;;;========================================================
;;; status buffer
;;;========================================================
(defun lgit-get-status-buffer-create ()
  (let* ((git-dir (lgit-git-dir))
	 (dir (file-name-directory git-dir))
	 (dir-name (file-name-nondirectory (directory-file-name dir)))
	 (buf-name (format "*%s@lgit:%s*" dir-name dir))
	 (default-directory dir))
    (get-buffer-create buf-name)))

(defun list-tp ()
  (interactive)
  (message "tp: %S" (text-properties-at (point))))

(defun lgit-safe-search (re limit &optional no)
  (save-excursion
    (save-match-data
      (and (re-search-forward re limit t)
	   (match-beginning (or no 0))))))

(defun lgit-decorate-diff-header (no)
  (put-text-property (match-beginning 0)
		     (match-end 0)
		     'display
		     (propertize (concat "\n" (match-string-no-properties no))
				 'face
				 'lgit-diff-file-header)))

(defun lgit-decorate-diff-index-line (no)
  (put-text-property (1- (match-beginning 0))
		     (match-end 0)
		     'display
		     (propertize 
		      (concat "\t-- "
			      (match-string-no-properties no))
		      'face 'lgit-diff-none)))

(defun lgit-decorate-hunk-header (no)
  (put-text-property (match-beginning no)
		     (match-end no)
		     'face
		     'lgit-diff-hunk-header)
  (put-text-property (match-end no)
		     (match-end 0)
		     'face
		     'lgit-diff-none))

(defsubst lgit-delimit-section (sect-type section beg end 
					  &optional mark-invisibility-p)
  (put-text-property beg end :sect-type sect-type)
  (put-text-property beg end sect-type section)
  (put-text-property beg end :navigation beg)
  (when mark-invisibility-p 
    (let ((current-inv (get-text-property beg 'invisibility)))
      (put-text-property beg (1- end) 'invisibility
			 (cons beg current-inv)))))

(defun lgit-decorate-diff-sequence (beg end regexp
					diff-re-no
					hunk-re-no
					index-re-no
					del-re-no
					add-re-no
					none-re-no)
  (save-match-data
    (save-excursion
      (let (sub-beg sub-end)
	(goto-char beg)
	(while (re-search-forward regexp end t)
	  (setq sub-beg (match-beginning 0))
	  (cond ((match-beginning del-re-no) ;; del
		 (put-text-property (match-beginning 0) (match-end 0)
				    'face 'lgit-diff-del))
		((match-beginning add-re-no) ;; add
		 (put-text-property (match-beginning 0) (match-end 0)
				    'face 'lgit-diff-add))
		((match-beginning none-re-no) ;; unchanged
		 (put-text-property (match-beginning 0) (match-end 0)
				    'face 'lgit-diff-none))
		((match-beginning hunk-re-no) ;; hunk
		 (setq sub-end (or (lgit-safe-search "^\\(:?diff\\|@@\\)"
						     end)
				   end))
		 (lgit-decorate-hunk-header hunk-re-no)
		 (lgit-delimit-section :hunk (cons sub-beg sub-end)
				       sub-beg sub-end t))
		((match-beginning diff-re-no) ;; diff
		 (setq sub-end (or (lgit-safe-search "^diff " end) end))
		 (lgit-decorate-diff-header diff-re-no)
		 (lgit-delimit-section :diff (cons sub-beg sub-end)
				       sub-beg sub-end t))
		((match-beginning index-re-no) ;; index
		 (lgit-decorate-diff-index-line index-re-no))
	      
		) ;; cond
	  )	  ;; while
	nil))))

(defun lgit-decorate-diff-section (sect-type section beg end 
					     &optional diff-src-prefix
					     diff-dst-prefix)
  (let ((a (or diff-src-prefix "a/"))
	(b (or diff-dst-prefix "b/"))
	re)
    (lgit-delimit-section sect-type section beg end)
    (setq re
	  (concat "^\\(?:"
		  "diff --git " a "\\(.+\\) " b ".+\\|" ;1 file
		  "\\(@@ .+@@\\).*\\|"			;2 hunk
		  "index \\(.+\\)\\|"			;3 index
		  "\\(-.*\\)\\|"			;4 del
		  "\\(\\+.*\\)\\|"			;5 add
		  "\\( .*\\)"				;6 none
		  "\\)$"))
    (lgit-decorate-diff-sequence beg end re 1 2 3 4 5 6)))

(defun lgit-sb-insert-repo-section ()
  (let ((head-info (lgit-head))
	(beg (point)))
    (insert (propertize (or (cdr head-info) 
			    (format "Detached HEAD: %s"
				    (lgit-name-rev (car head-info))))
			'face 'lgit-branch) 
		"\n"
		(propertize (car head-info) 'face 'font-lock-string-face)
		"\n"
		(propertize (lgit-git-dir) 'face 'font-lock-reference-face)
		"\n")
    (lgit-delimit-section :section 'repo beg (point))))

(defsubst lgit-prepend (str prefix &rest other-properties)
  (propertize str 'display 
	      (apply 'propertize (concat prefix str) other-properties)))

(defun lgit-sb-insert-unstaged-section ()
  (let ((beg (point)))
    (insert (lgit-prepend "Unstaged Changes:" "\n\n" 
			  'face 'lgit-section-title)
	    "\n")
    (call-process "git" nil t nil "diff" "--no-color"
		  "--src-prefix=INDEX/" "--dst-prefix=WORKDIR/")
    (lgit-decorate-diff-section :section 'unstaged beg (point)
				"INDEX/" "WORKDIR/")))

(defun lgit-sb-insert-staged-section ()
  (let ((beg (point)))
    (insert (lgit-prepend "Staged Changes:""\n\n"
			  'face 'lgit-section-title)
	    "\n")
    (call-process "git" nil t nil "diff" "--no-color" "--cached"
		  "--src-prefix=HEAD/" "--dst-prefix=INDEX/")
    (lgit-decorate-diff-section :section 'staged beg (point) 
				"HEAD/" "INDEX/")))

(defun lgit-update-status-buffer (&optional update-display-p)
  (with-current-buffer (lgit-get-status-buffer-create) 
      (let ((inhibit-read-only t))
	(erase-buffer)
	(lgit-sb-insert-repo-section)
	(lgit-sb-insert-unstaged-section)
	(lgit-sb-insert-staged-section)
	(when update-display-p
	  (force-window-update (current-buffer)))
	(current-buffer))))

(defun lgit-display-status-buffer (&optional update-p)
  (interactive "P")
  (let ((buf (if update-p
		 (lgit-update-status-buffer t)
	       (lgit-get-status-buffer-create))))
    (display-buffer buf t)))


(provide 'lgit)
