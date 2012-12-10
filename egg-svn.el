;;; egg-svn.el --- Emacs Got Git - Emacs interface to Git

;; Copyright (C) 2008  Linh Dang
;; Copyright (C) 2011-12 byplayer
;;
;; Author: Bogolisk <bogolisk@gmail.com>
;; Created: 19 Aug 2008
;; Version: 1.0.2
;; Keywords: git, version control, release management
;;
;; Special Thanks to
;;   Antoine Levitt, Bogolisk,
;;   Christian Köstlin
;;   Max Mikhanosha
;;   Aleksandar Simic
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
;;
;; Commentary:
;;
;; This is NOT for typical git-svn usage. This package is a tool
;; to help dealing with HUMONGOUS svn repo via git-svn.
;;
;;

(require 'egg-git)
(require 'egg-const)
(require 'egg)

(defcustom egg-svn-command "svn"
  "Name or full-path to the svn command.
Set this to the appropriate string in the case where `svn' is not the
desirable way to invoke GIT."
  :group 'egg
  :type 'string)


(defcustom egg-svn-profile-alist 
  '(("Standard" 
     :email "myself@y.svn.host"
     :namespace "svn"
     :url "http://my.svn.host/myrepo"
     :exclude "/(branches|tags)/"
     :trunk ("trunk" . "refs/remotes/svn/trunk")
     :one-to-one nil
     :branches (("branches/" . "refs/remotes/svn/"))
     :oldest nil
     )
    ("Large" 
     :email "myself@y.svn.host"
     :namespace "svn"
     :url "http://my.svn.host/large_repo"
     :exclude "/(branches|tags)/"
     :trunk ("main" . "refs/remotes/svn/main")
     :one-to-one nil
     :branches (("branches/release/5.x/" . "refs/remotes/svn/")
		("branches/privates/user1/" . "refs/remotes/user1/"))
     :oldest "branches/release/5.0"
     ))
  "Profiles to create new git-svn repository."
  :group 'egg
  :type '(repeat (list (string :tag "Profile Name")
		       (const :tag "----" :email)
		       (string :tag "Email Address")
		       (const :tag "----" :namespace)
		       (string :tag "Namespace (similar to origin)")
		       (const :tag "----" :url)
		       (string :tag "URL of repo")
		       (const :tag "----" :exclude)
		       (string :tag "Perl Regexp to ignore branches and tags")
		       (const :tag "----" :trunk)
		       (cons :tag "Trunk mapping"
			     (string :tag "SVN Trunk Name")
			     (string :tag "Git Ref Name"))
		       (const :tag "----" :one-to-one)
		       (repeat :tag "1-to-1 mappings"
			       (cons :tag "Fetch"
				     (string :tag "SVN Branch Path")
				     (string :tag "Git Full Ref")))
		       (const :tag "----" :branches)
		       (repeat :tag "Branch mappings"
			       (cons :tag "Branch"
				     (string :tag "SVN Branch Prefix (including final /)")
				     (string :tag "Git Ref Prefix (including final /)")))
		       
		       (const :tag "----" :oldest)
		       (choice :tag "Limit SVN History Fetching to"
			       (const :tag "Off (Fetch All Revisions)" nil)
			       (integer :tag "SVN Revision")
			       (string :tag "SVN Branch Path")))))

(defsubst egg--svn (buffer &rest args)
  "run SVN with ARGS and insert output into BUFFER at point.
return the t if the exit-code was 0. if BUFFER was t then
current-buffer would be used."
  (= (apply 'call-process egg-svn-command nil buffer nil args) 0))

(defsubst egg--svn-args (buffer args)
  "run SVN with ARGS and insert output into BUFFER at point.
return the t if the exit-code was 0. if BUFFER was t then
current-buffer would be used."
  (= (apply 'call-process egg-svn-command nil buffer nil args) 0))


(defun egg--do-svn (stdin cmd args)
  "Run svn command CMD with ARGS synchronously, using STDIN as starndard input.
ARGS should be a list of arguments for the git command CMD."
  (egg--do stdin egg-svn-command (cons cmd args)))


(defsubst egg-git-svn-remote-name ()
  (egg-pick-file-contents (concat (egg-git-dir) "/config") "^\\[svn-remote +\"\\(.+\\)\"\\]$" 1))

(defsubst egg-git-svn-url (&optional remote)
  (egg-git-to-string "config" "--get" 
		     (concat "svn-remote." (or remote (egg-git-svn-remote-name)) ".url")))

(defsubst egg-git-svn-max-rev (&optional remote)
  (egg-git-to-string "config" "--file" (concat (egg-git-dir) "/svn/.metadata")
		     "--get" (concat "svn-remote."
				     (or remote (egg-git-svn-remote-name))
				     ".branches-maxRev")))

(defsubst egg-svn-name-to-full-ref-name (&optional svn-name)
  (let ((default-directory (concat (egg-git-dir) "/svn/")))
    (car (file-expand-wildcards (concat "refs/remotes/*/" svn-name)))))

(defsubst egg-svn-name-to-short-ref-name (&optional svn-name)
  (let ((default-directory (concat (egg-git-dir) "/svn/refs/remotes/")))
    (car (file-expand-wildcards (concat "*/" svn-name)))))

(defsubst egg-svn-all-refs (&optional prefix)
  (let ((default-directory (concat (egg-git-dir) "/svn/refs/remotes/")))
    (file-expand-wildcards (concat (or prefix "*") "/*"))))

(defsubst egg-svn-all-full-refs (&optional prefix)
  (let ((default-directory (concat (egg-git-dir) "/svn/")))
    (file-expand-wildcards (concat "refs/remotes/" (or prefix "*") "/*"))))


(defun egg--do-svn-action (cmd buffer-to-update post-proc-func args)
  "Run svn command CMD with arguments list ARGS.
Show the output of CMD as feedback of the emacs command.
Update the buffer BUFFER-TO-UPDATE and use POST-PROC-FUNC as the
output processing function for `egg--do-handle-exit'."
  (egg--do-show-output (concat "SVN-" (upcase cmd))
		       (egg--do-handle-exit (egg--do-svn nil cmd args)
					    post-proc-func buffer-to-update)))

(defun egg--git-svn-init-repo (buffer-to-update svn-remote svn-trunk-ref svn-url ignore-paths-pcre)
  (egg--do-git-action
   "svn" buffer-to-update
   (lambda (ret-code)
     (cond ((= ret-code 0) (egg--git-pp-grab-line-matching "Initialized empty Git repository"
							   :success t :next-action 'status))
	   (t (egg--git-pp-fatal-result))))
   (list "init" "-R" svn-remote "-i" svn-trunk-ref svn-url 
	 (concat "--ignore-paths=" ignore-paths-pcre))
   'no-log))

(defun egg--git-svn-create-branch (buffer-to-update log-msg branch-name &optional svn-parent-url)
  (egg--do-git-action
   "svn" buffer-to-update
   (lambda (ret-code)
     (cond ((= ret-code 0)
	    (or (egg--git-pp-grab-line-matching (concat "^r[0-9]+ = .+ (refs/.+/" branch-name ")$")
						:success t :next-action 'log)
		(egg--git-pp-grab-line-matching "^r[0-9]+ = .+ (refs/.+)$"
						:success t :next-action 'log)
		(egg--git-pp-grab-line-matching "^Successfully.+" :success t :next-action 'log)
		(egg--git-pp-grab-line-matching "^Found branch.+" :success t :next-action 'log)
		(egg--git-pp-grab-line-matching "^Found possible branch.+" :success t :next-action 'log)
		(egg--git-pp-grab-line-matching (concat "/" branch-name) :success t :next-action 'log)))
	   (t (egg--git-pp-fatal-result))))
   (nconc (list "branch" "-m" log-msg)
	  (if svn-parent-url (list "-d" svn-parent-url))
	  (list branch-name))))

(defun egg--git-svn-reset-max-rev (buffer-to-update svn-rev)
  (egg--do-git-action
   "svn" buffer-to-update
   (lambda (ret-code)
     (cond ((= ret-code 0)
	    (or (egg--git-pp-grab-line-matching "^r[0-9]+ =" :success t)
		(egg--git-pp-grab-line-no -1 :success t)))
	   (t (egg--git-pp-fatal-result))))
   (list "reset" (concat "-r" (if (stringp svn-rev) svn-rev (number-to-string svn-rev))))))

(defun egg--git-svn-fetch-rev (buffer-to-update svn-rev)
  (egg--do-git-action
   "svn" buffer-to-update
   (lambda (ret-code)
     (cond ((= ret-code 0)
	    (or (egg--git-pp-grab-line-matching "^r[0-9]+ =" :success t)
		(egg--git-pp-grab-line-no -1 :success t)))
	   (t (egg--git-pp-fatal-result))))
   (list "fetch" (concat "-r" (if (stringp svn-rev) svn-rev (number-to-string svn-rev))))))

(defun egg--svn-delete (buffer-to-update log-msg svn-url)
  (egg--do-svn-action
   "delete" buffer-to-update
   (lambda (ret-code)
     (cond ((= ret-code 0)
	    (egg--git-pp-grab-line-matching "^Committed revision.+" :success t :next-action 'log))
	   (t (egg--git-pp-fatal-result))))
   (list "-m" log-msg svn-url)))

(defun egg--svn-get-parent-revision (svn-branch-path &optional remote-name)
  (with-egg-debug-buffer
    (let* ((url (egg-git-svn-url remote-name))
	  (path (and url (concat url "/" svn-branch-path))))
      (erase-buffer)
      (when (egg--svn-args t (list "log" "-v" "--stop-on-copy" "-r" "0:HEAD" "--limit" "1" path))
	(goto-char (point-min))
	(when (re-search-forward (concat svn-branch-path ".* (from /.+:\\([0-9]+\\))$") nil t)
	  (string-to-number (match-string-no-properties 1)))))))

(defun egg-svn-start-initial-fetch (process msg)
  (let ((profile (if (processp process) (process-plist process) process))
	(process (if (processp process) nil))
	(buffer (if (processp process) (process-buffer process) (current-buffer)))
	todo)

    (setq todo (plist-get profile :todo))
    (setq todo (prog1 (car todo)
		 (plist-put profile :todo (cdr todo))))

    (when (stringp msg)
      (goto-char (point-max))
      (insert "GIT-SVN:" msg "\n"))

    (if todo
	(with-current-buffer buffer
	  (setq process (funcall todo profile))
	  (if (processp process)
	      (progn
		(set-process-plist process profile)
		(set-process-sentinel process #'egg-svn-start-initial-fetch))
	    (message "cannot start svn to do initial fetching!")))
      (goto-char (point-max))
      (insert "EGG-SVN:done fetching initial SVN revisions.\n")
      (egg-log nil))))

(defun egg-svn-init-repo (profile)
  (interactive (let ((alist egg-svn-profile-alist)
		     name)
		 (setq name (completing-read "init an git-svn repo based on profile: " 
					     alist nil t))
		 (list (assoc name egg-svn-profile-alist))))
  (let* ((dir default-directory)
	 (buffer (get-buffer-create (concat "*git-svn-init@" dir "*")))
	 (profile-name (car profile))
	 (profile (copy-sequence (cdr profile)))
	 (remote (plist-get profile :namespace))
	 (trunk-full-ref (cdr (plist-get profile :trunk)))
	 (oldest (plist-get profile :oldest))
	 (ignore-re (plist-get profile :exclude))
	 (trunk (car (plist-get profile :trunk)))
	 (branch-mappings (plist-get profile :branches))
	 (direct-mappings (plist-get profile :one-to-one))
	 trunk-ref 
	 first-fetch-func last-fetch-func todo
	 tmp)
    (with-current-buffer buffer
      (erase-buffer)
      (pop-to-buffer buffer)

      (setq trunk-ref 
	    (mapconcat 'identity 
		       (nthcdr 2 (save-match-data (split-string trunk-full-ref "/" t))) "/"))
      
      (if (setq tmp (egg--git-svn-init-repo nil remote trunk-ref 
					    (plist-get profile :url) 
					    (plist-get profile :exclude)))
	  (insert (or (plist-get tmp :line) "init has not output") "\n")
	(error "Failed to init git-svn repo in %s" dir))

      (when (or (/= (call-process egg-git-command nil t t "config" "--replace-all"
				   "user.email" (plist-get profile :email)) 0)
		(/= (call-process egg-git-command nil t t "config" "--replace-all"
				  "svn-remote.svn.fetch"
				  (concat trunk ":" trunk-full-ref)) 0)
		(memq nil (mapcar (lambda (svn-ref)
				    (= (call-process egg-git-command nil t t "config" "--add"
						     "svn-remote.svn.branches"
						     (concat (car svn-ref) "*:" (cdr svn-ref) "*")) 0))
				  branch-mappings))
		(memq nil (mapcar (lambda (svn-ref)
				    (= (call-process egg-git-command nil t t "config" "--add"
						     "svn-remote.svn.branches"
						     (concat (car svn-ref) ":" (cdr svn-ref))) 0))
				  direct-mappings)))
	(error "Failed to configure git-svn repo in %s" dir))


      (setq oldest (cond ((numberp oldest) oldest)
			 ((stringp oldest)
			  (egg--svn-get-parent-revision oldest remote))
			 (t nil)))
      (when oldest 
	(setq oldest (number-to-string oldest))
	(plist-put profile :oldest oldest))
      
      (when (y-or-n-p (format "proceed to fetch %ssvn revisions%s (this might take %s)? "
			      (if oldest "" "all ")
			      (if oldest (format " from %s to HEAD" ""))
			      (propertize "weeks" 'face 'bold)))

	(setq last-fetch-func
		(lambda (profile)
		  (goto-char (point-max))
		  (insert "EGG-SVN: fetching initial revisions:\n")
		  (insert "GIV-SVN: fetch all\n")
		  (start-process "egg-svn-fetch" (current-buffer) egg-git-command "svn" "-q"
				 "fetch" "--ignore-paths" (plist-get profile :exclude))))
	(push last-fetch-func todo)

	(when oldest
	  (setq first-fetch-func
		(lambda (profile)
		  (goto-char (point-max))
		  (insert "EGG-SVN: fetching 1st revision:\n")
		  (insert "GIV-SVN: fetch r" (plist-get profile :oldest) "\n")
		  (start-process "egg-svn-fetch" (current-buffer) egg-git-command "svn" "-q"
				 "fetch" "--ignore-paths" (plist-get profile :exclude)
				 "-r" (plist-get profile :oldest))))
	  (push first-fetch-func todo))

	(plist-put profile :todo todo)

	(egg-svn-start-initial-fetch profile nil)))))

(defsubst egg-svn-full-to-remote (full-ref)
  (file-name-nondirectory (directory-file-name (file-name-directory full-ref))))

(defun egg-svn-handle-svn-remote (remote branch &rest names)
 (when (or (egg-git-to-string "config" "--get" (concat "svn-remote." remote ".url"))
	   (and (stringp branch)
		(member branch (egg-svn-all-full-refs))
		(setq remote (egg-svn-full-to-remote branch))))
   (setq remote (propertize remote
			    :svn-remote (egg-git-svn-remote-name)
			    :push #'egg-push-to-svn 
			    :fetch #'egg-fetch-from-svn))
   (mapc (lambda (name)
	   (add-text-properties 0 (length name) 
				(list :svn-remote (egg-git-svn-remote-name)
				      :push #'egg-push-to-svn 
				      :fetch #'egg-fetch-from-svn)
				name))
	 names)
   remote))

(add-hook 'egg-special-remote-handlers #'egg-svn-handle-svn-remote)



(provide 'egg-svn)
