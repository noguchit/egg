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


(defun egg-svn-lines-matching (re idx &rest args)
  "run SVN with ARGS.
Return the output lines as a list of strings."
  (with-temp-buffer
    (when (egg--svn-args t args)
      (let (lines)
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward re nil t)
            (setq lines (cons (match-string-no-properties idx) lines)))
          lines)))))


(defsubst egg-git-svn-remote-name ()
  (egg-pick-file-contents (concat (egg-git-dir) "/config") "^\\[svn-remote +\"\\(.+\\)\"\\]$" 1))

(defsubst egg-git-svn-config-name (name &optional remote)
  (concat "svn-remote." (or remote (egg-git-svn-remote-name)) "." name))

(defsubst egg-git-svn-url (&optional remote)
  (egg-git-to-string "config" "--get" (egg-git-svn-config-name "url")))

(defsubst egg-git-svn-ignore-paths (&optional remote)
  (egg-git-to-string "config" "--get" (egg-git-svn-config-name "ignore-paths")))

(defsubst egg-git-svn-max-rev (&optional remote)
  (egg-git-to-string "config" "--file" (concat (egg-git-dir) "/svn/.metadata")
		     "--get" (egg-git-svn-config-name "branches-maxRev")))

(defsubst egg-git-svn-epoch-rev (&optional remote)
  (car (egg-git-lines-matching "^r\\([0-9]+\\) " 1 "svn" "log" "--reverse" "--limit" "1")))


(defsubst egg-svn-name-to-full-ref-name (&optional svn-name)
  (let ((default-directory (concat (egg-git-dir) "/svn/")))
    (car (file-expand-wildcards (concat "refs/remotes/*/" svn-name)))))

(defsubst egg-svn-name-to-short-ref-name (&optional svn-name)
  (let ((default-directory (concat (egg-git-dir) "/svn/refs/remotes/")))
    (car (file-expand-wildcards (concat "*/" svn-name)))))

(defsubst egg-svn-is-known-ref (full-ref &optional svn-name)
  (file-directory-p (concat (egg-git-dir) "/svn/" full-ref)))

(defsubst egg-svn-all-refs (&optional prefix)
  (let ((default-directory (concat (egg-git-dir) "/svn/refs/remotes/")))
    (file-expand-wildcards (concat (or prefix "*") "/*"))))

(defsubst egg-svn-all-full-refs (&optional prefix)
  (let ((default-directory (concat (egg-git-dir) "/svn/")))
    (file-expand-wildcards (concat "refs/remotes/" (or prefix "*") "/*"))))

(defun egg-svn-get-all-prefixes ()
  (delete-dups (egg-git-lines-matching "^svn-remote.+:refs/remotes/\\([^/]+\\)/.+$" 1 
				       "config" "--get-regexp"
				       "svn-remote\\..+\\\.(branches|fetch)")))


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
	    (or (egg--git-pp-grab-line-matching "^r[0-9]+ =" :success t :next-action 'log)
		(egg--git-pp-grab-line-no -1 :success t :next-action 'log)))
	   (t (egg--git-pp-fatal-result))))
   (list "fetch" 
	 (concat "--ignore-paths=" (egg-git-svn-ignore-paths))
	 (concat "-r" (if (stringp svn-rev) svn-rev (number-to-string svn-rev))))))

(defun egg--git-svn-fetch (buffer-to-update)
  (egg--do-git-action
   "svn" buffer-to-update
   (lambda (ret-code)
     (cond ((= ret-code 0)
	    (or (egg--git-pp-grab-line-matching-backward "^r[0-9]+ =" :success t :next-action 'log)
		(egg--git-pp-grab-line-no -1 :success t :next-action 'log)))
	   (t (egg--git-pp-fatal-result))))
   (list "fetch" (concat "--ignore-paths=" (egg-git-svn-ignore-paths)))))

(defun egg--git-svn-dcommit (buffer-to-update branch)
  (egg--do-git-action
   "svn" buffer-to-update
   (lambda (ret-code)
     (cond ((= ret-code 0)
	    (or (egg--git-pp-grab-line-matching "^r[0-9]+ =" :success t :next-action 'log)
		(egg--git-pp-grab-line-matching "^dcommitted " :success t :next-action 'log)
		(egg--git-pp-grab-line-matching "^Committed " :success t :next-action 'log)
		(egg--git-pp-grab-line-no -1 :success t :next-action 'log)))
	   (t (egg--git-pp-fatal-result))))
   (list "dcommit" branch)))

(defun egg--svn-delete (buffer-to-update log-msg svn-url)
  (egg--do-svn-action
   "delete" buffer-to-update
   (lambda (ret-code)
     (cond ((= ret-code 0)
	    (egg--git-pp-grab-line-matching "^Committed revision.+" :success t :next-action 'log))
	   (t (egg--git-pp-fatal-result))))
   (list "-m" log-msg svn-url)))

(defun egg--svn-copy (buffer-to-update log-msg from-url to-url)
  (egg--do-svn-action
   "copy" buffer-to-update
   (lambda (ret-code)
     (cond ((= ret-code 0)
	    (egg--git-pp-grab-line-matching "^Committed revision.+" :success t :next-action 'log))
	   (t (egg--git-pp-fatal-result))))
   (list "-m" log-msg from-url to-url)))

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
				  (concat "svn-remote." remote ".fetch")
				  (concat trunk ":" trunk-full-ref)) 0)
		(memq nil (mapcar (lambda (svn-ref)
				    (= (call-process egg-git-command nil t t "config" "--add"
						     (concat "svn-remote." remote ".branches")
						     (concat (car svn-ref) "*:" (cdr svn-ref) "*")) 0))
				  branch-mappings))
		(memq nil (mapcar (lambda (svn-ref)
				    (= (call-process egg-git-command nil t t "config" "--add"
						     (concat "svn-remote." remote ".branches")
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

(defun egg-svn-map-name (name branch spec)
  (save-match-data
    (let ((mappings (mapcar (lambda (line)
			      (split-string line "[:* \t]+" t))
			    (egg-git-to-lines "config" "--get-regexp" 
					      "svn-remote\\..*\\.(branches|fetch)")))
	  match tmp remote type props)
      (setq mappings
	    (mapcar 
	     (lambda (map)
	       (setq tmp (split-string (car map) "\\." t))
	       (setq remote (nth 1 tmp))
	       (setq type (nth 2 tmp))
	       (list (nth 2 map) remote (intern (concat ":" type)) (nth 1 map) (nth 2 map)))
	     mappings))
      (setq match (or (and branch (assoc branch mappings))
		      (and spec (assoc spec mappings))))
      (when match
	(setq props (list :x-delete #'egg-delete-svn-path
			  :x-push #'egg-push-to-svn
			  :x-fetch #'egg-fetch-from-svn
			  :x-info (cdr match)))
	(if (stringp name)
	    (apply #'propertize name props)
	  props)))))

(defun egg-svn-add-remote-properies (name prefix)
  (egg-svn-map-name name nil (concat "refs/remotes/" prefix "/")))

(defun egg-svn-get-remote-properies (prefix branch)
  (or (and (not (equal prefix "."))
	   (egg-svn-map-name nil nil (concat "refs/remotes/" prefix "/")))
      (and (stringp branch) 
	   (egg-svn-map-name nil branch (file-name-directory branch)))))

(defun egg-svn-ref-to-path (r-ref x-info)
  (when (and (stringp r-ref) (consp x-info))
    (let ((r-ref (file-name-nondirectory r-ref))
	  (svn-name (car x-info))
	  (map-type (nth 1 x-info))
	  (dest (nth 2 x-info))
	  url)
      (setq url (and svn-name (egg-git-svn-url svn-name)))
      (when (and (stringp url) (memq map-type '(:fetch :branches)) (stringp dest))
	(concat url "/" (cond ((eq map-type :fetch) dest)
			      ((eq map-type :branches) (concat dest r-ref))))))))

(defun egg-svn-ref-to-git-svn-dir (r-ref x-info)
  (when (and (stringp r-ref) (consp x-info))
    (let ((r-ref (file-name-nondirectory r-ref))
	  (local-base (nth 3 x-info)))
      (when (stringp local-base)
	(concat (egg-git-dir) "/svn/" local-base r-ref)))))

(defun egg-delete-svn-path (buffer-to-update x-info r-ref)
  (let ((svn-path (egg-svn-ref-to-path r-ref x-info))
	(local-dir (egg-svn-ref-to-git-svn-dir r-ref x-info))
	(r-ref (file-name-nondirectory r-ref))
	res)
    (when (stringp svn-path)
      (setq res (if (save-match-data (string-match "@" r-ref)) 
		    ;; git-svn's metadata only, not on svn repo
		    (list :success t :next-action 'log)
		  (egg--svn-delete buffer-to-update (concat "delete " r-ref)
				   svn-path)))
      (when (and (plist-get res :success) (stringp local-dir))
	(delete-directory local-dir t)))
    res))

(defun egg-svn-find-last-mapped-rev (git-start)
  (let ((commit (egg-git-to-string "rev-parse" (concat git-start "^{/git-svn-id:}")))
	found svn-rev)
    (while (and (not found) commit)
      (setq svn-rev (egg-git-to-string "svn" "find-rev" commit))
      (unless (and svn-rev
		   (setq found (egg-git-to-string "svn" "find-rev" (concat "r" svn-rev) git-start))
		   (equal found commit))
	(setq found nil)
	(setq commit (egg-git-to-string "rev-parse" (concat commit "^^{/git-svn-id:}")))))
    found))

(defun egg-svn-path-exists-p (path-url)
  (egg--svn nil "info" path-url))

(defun egg-svn-path-last-rev (path-url)
  (car (egg-svn-lines-matching "^Last Changed Rev: \\([0-9]+\\)$" 1 "info" path-url)))

(defun egg-svn-path-first-rev (path-url)
  (car (egg-svn-lines-matching "^r\\([0-9]+\\) " 1 "log" "--stop-on-copy" 
			       "-r" "0:HEAD" "-l1" path-url)))

(defun egg-git-svn-map-svn-path (path)
  (save-match-data
    (let* ((path (directory-file-name path))
	   (path-prefix (file-name-directory path))
	   (name (file-name-nondirectory path))
	   (svn-name (egg-git-svn-remote-name))
	   (direct (mapcar (lambda (line)
			     (split-string line ":" t))
			   (egg-git-to-lines "config" "--get-all" 
					     (egg-git-svn-config-name "fetch" svn-name))))
	   (branches (mapcar (lambda (line)
			       (split-string line "[*:]+" t))
			     (egg-git-to-lines "config" "--get-all" 
					       (egg-git-svn-config-name "branches" svn-name))))
	   match)
      (cond ((setq match (assoc path direct))
	     (nth 1 match))
	    ((setq match (assoc path-prefix branches))
	     (concat (nth 1 match) name))
	    (t nil)))))

(defun egg-git-svn-map-full-ref (full-ref)
  (let* ((short-ref (file-name-nondirectory full-ref))
	 (local-base (file-name-directory full-ref))
	 (svn-name (egg-git-svn-remote-name))
	 (direct (mapcar (lambda (line)
			   (nreverse (split-string line ":" t)))
			 (egg-git-to-lines "config" "--get-all" 
					   (egg-git-svn-config-name "fetch" svn-name))))
	 (branches (mapcar (lambda (line)
			     (nreverse (split-string line "[*:]+" t)))
			   (egg-git-to-lines "config" "--get-all" 
					     (egg-git-svn-config-name "branches" svn-name))))
	 match)
    (cond ((setq match (assoc full-ref direct))
	   (nth 1 match))
	  ((setq match (assoc local-base branches))
	   (concat (nth 1 match) short-ref))
	  (t nil))))

(defun egg--git-svn-add-branch-mapping (svn-path-prefix git-svn-prefix)
  (let ((svn-path-prefix (file-name-as-directory svn-path-prefix))
	(git-svn-prefix 
	 (file-name-as-directory 
	  (if (equal (substring git-svn-prefix (length "refs/")) "refs/")
	      git-svn-prefix
	    (concat "refs/remotes/" git-svn-prefix)))))
    (egg--git nil "config" "--add" (egg-git-svn-config-name "branches")
	      (concat svn-path-prefix "*:" git-svn-prefix "*"))))

(defun egg--git-svn-add-custom-branch-mapping (mapping)
  (egg--git nil "config" "--add" (egg-git-svn-config-name "branches") mapping))

(defun egg--git-svn-add-direct-mapping (svn-path git-full-ref)
  (let ((svn-path (directory-file-name svn-path)))
    (egg--git nil "config" "--add" (egg-git-svn-config-name "fetch")
	      (concat svn-path ":" git-full-ref))))

(defun egg-svn-make-branch-from (buffer-to-update svn-repo-name new-url from-url)
  (let ((res (egg--svn-copy nil (concat "create branch " (file-name-nondirectory new-url))
			    from-url new-url))
	(pretty-new (propertize new-url 'face 'bold))
	(pretty-from (propertize from-url 'face 'bold))
	line ok new-rev fetched-rev)
    
    (setq line (plist-get res :line))
    (if (not (plist-get res :success))
	(error "Failed to create %s from %s: %s" pretty-new pretty-from line)
      (setq new-rev (save-match-data
		      (if (string-match "Committed revision \\([0-9]+\\)\\." line)
			  (match-string-no-properties 1 line)
			(error "Can't parse svn revision number in: \"%s\"" line))))
      (setq new-rev (string-to-number new-rev))
      (setq res (egg--git-svn-fetch buffer-to-update))
      (setq line (plist-get res :line))
      (if (not (plist-get res :success))
	  (error "Failed to do post-copy fetch: %s" line)
	(setq fetched-rev (string-to-number (egg-git-svn-max-rev svn-repo-name)))
	(if (>= fetched-rev new-rev)
	    (setq ok t)
	  (setq res (egg--git-svn-fetch-rev buffer-to-update new-rev))
	  (setq line (plist-get res :line))
	  (if (not (plist-get res :success))
	      (error "Failed to fetch svn revision %s: %s" new-rev line)
	    (setq fetched-rev (string-to-number (egg-git-svn-max-rev svn-repo-name)))
	    (if (>= fetched-rev new-rev)
		(setq ok t)
	      (error "Problems with git-svn: needs to fetch r%s but git-svn only fetch up to r%s" 
		     new-rev fetched-rev))))))
    ok))


(defun egg-push-to-svn (buffer-to-update svn-remote l-ref r-ref)
  (let* ((svn-name (car svn-remote))
	 (map-type (nth 1 svn-remote))
	 (dest (nth 2 svn-remote))
	 (local-base (nth 3 svn-remote))
	 (r-ref (and (stringp r-ref) (file-name-nondirectory r-ref)))
	 (r-full-name (and r-ref (concat local-base r-ref)))
	 (url (and svn-name (egg-git-svn-url svn-name)))
	 (svn-branch-url (and r-ref url 
			      (concat url "/" 
				      (cond ((eq map-type :fetch) dest)
					    ((eq map-type :branches) 
					     (concat dest r-ref))
					    (t (error "Unknown svn-to-git mapping type: %s"
						      map-type))))))
	 (l-is-ref (and (stringp l-ref) (egg-git-to-string "show-ref" l-ref)))
	 git-commit svn-rev res line base-commit)
    (cond ((null svn-branch-url)
	   (error "Failed to map branch %s on svn remote %s" r-ref svn-name))
	  ((not (stringp l-ref))
	   (error "Can't push local ref: %s" l-ref))
	  ((and (not (egg-svn-path-exists-p svn-branch-url))
		(progn 
		  (setq git-commit (egg-svn-find-last-mapped-rev l-ref))
		  (setq svn-rev (and git-commit
				     (egg-pick-from-commit-message git-commit
								   "^git-svn-id: \\(.+\\) .+$" 1)))
		  (y-or-n-p (format "create new svn branch %s at %s (%s)? " 
				    (propertize svn-branch-url 'face 'bold) 
				    (propertize svn-rev 'face 'bold)
				    git-commit))))
	   (when (egg-svn-make-branch-from buffer-to-update svn-name svn-branch-url svn-rev)
	     (message (if l-is-ref 
			  "created svn branch %s, please rebase %s on %s and push again"
			"new svn branch %s (from %s) -> %s") 
		      (propertize svn-branch-url 'face 'bold)
		      (propertize l-ref 'face 'bold)
		      (propertize r-full-name 'face 'bold)))
	   nil)
	  ((progn
	     (setq git-commit (egg-git-to-string "rev-parse" r-full-name))
	     (setq base-commit (egg-git-to-string "merge-base" l-ref r-full-name))
	     (not (equal git-commit base-commit)))
	   (message (if l-is-ref 
			"please rebase %s on %s before pushing on svn-remote %s"
		      "%s -> %s is not an fast-forward push!")
		    (propertize l-ref 'face 'bold)
		    (propertize r-full-name 'face 'bold) 
		    (propertize svn-name 'face 'bold))
	   nil)
	  ((not (equal (setq res (car (egg-git-lines-matching "Committing to \\(.+\\) \\.\\.\\." 1
							      "svn" "dcommit" "-n" l-ref)))
		       svn-branch-url))
	   (error "Fatal: git-svn would dcommit %s on %s instead of %s!"
		  l-ref res svn-branch-url))
	  ((y-or-n-p (format "push %s, %d revision(s), on svn-path %s? "
			     (propertize l-ref 'face 'bold) 
			     (length (egg-git-to-lines "rev-list" (concat r-full-name ".." l-ref)))
			     (propertize svn-branch-url 'face 'bold)))
	   (egg--git-svn-dcommit buffer-to-update l-ref)))))

(defun egg-fetch-unknown-svn-path (buffer-to-update svn-remote svn-path &optional local-base)
  (let* ((svn-path (directory-file-name svn-path))
	 (svn-name (car svn-remote))
	 (url (and svn-name (egg-git-svn-url svn-name)))
	 (svn-path-url (and url (concat url "/" svn-path)))
	 (svn-short-name (file-name-nondirectory (directory-file-name svn-path)))
	 (max-rev (and svn-name (string-to-number (egg-git-svn-max-rev svn-name))))
	 (epoch-rev (and svn-name (string-to-number (egg-git-svn-epoch-rev svn-name))))
	 use-direct-mapping full-ref mapping-config rev-to-fetch res line new-rev sha1
	 did-reset)
    (save-match-data
      (setq full-ref (read-string (format "map %s to: " (propertize svn-path 'face 'bold))
				  (cond ((not (string-match "/" svn-path))
					 (concat "refs/remotes/" svn-name "/" svn-short-name))
					((string-match "release" svn-path)
					 (concat "refs/remotes/" svn-name "/" svn-short-name))
					((stringp local-base) 
					 (concat local-base svn-short-name))
					(t nil))))
      (unless (and (stringp full-ref)
		   (string-match "\\`refs/remotes/" full-ref))
	(error "Cannot handle remote trackign branch name: %s" full-ref))
      
      (if (setq use-direct-mapping
		(y-or-n-p (format "use direct mapping (%s:%s)? " svn-path full-ref)))
	  (progn
	    (setq mapping-config (concat svn-path ":" full-ref))
	    (egg--git-svn-add-direct-mapping svn-path full-ref))
	(setq mapping-config (concat (file-name-directory svn-path) "*:"
				     (file-name-directory full-ref) "*"))
	(setq mapping-config
	      (read-string "add svn->git mapping rule: " mapping-config))
	(if (string-match (concat "\\*:" (file-name-directory full-ref) "\\*\\'")
			  mapping-config)
	    (egg--git-svn-add-custom-branch-mapping mapping-config)
	  (error "Cannot handle mapping svn->git mapping: %s" mapping-config)))

      (setq rev-to-fetch (egg--svn-get-parent-revision svn-path svn-name))

      (when (< rev-to-fetch epoch-rev)
	(error "Cannot fetch revision (r%d) older than epoch (r%d)!" rev-to-fetch epoch-rev))
      
      (when (< rev-to-fetch max-rev)
	(unless (y-or-n-p (format "unfetch and refetch %d revisions (might take a long time)? "
				  (- max-rev rev-to-fetch)))
	  (error "Cancelled before unfetch and refetch old revisions!"))
	(setq res (egg--git-svn-reset-max-rev buffer-to-update rev-to-fetch))
	(setq line (plist-get res :line))
	(unless (plist-get res :success)
	  (error "Failed to unfetch svn revisions back to r%d: %s" rev-to-fetch line))
	(setq did-reset nil))
      
      (setq max-rev (string-to-number (egg-git-svn-max-rev svn-name)))
      (setq rev-to-fetch (string-to-number (egg-svn-path-first-rev svn-path)))
      (when (< max-rev rev-to-fetch)
	(setq res (egg--git-svn-fetch buffer-to-update))
	(setq line (plist-get res :line))
	(unless (plist-get res :success)
	  (error "Failed to %sfetch svn revisions after r%d: %s" 
		 (if did-reset "re" "") max-rev line)))

      (setq max-rev (string-to-number (egg-git-svn-max-rev svn-name)))
      (when (< max-rev rev-to-fetch)
	(setq res (egg--git-svn-fetch-rev buffer-to-update rev-to-fetch))
	(setq line (plist-get res :line))
	(unless (plist-get res :success)
	  (error "Failed to fetch svn r%d: %s" rev-to-fetch line)))

      (setq max-rev (string-to-number (egg-git-svn-max-rev svn-name)))
      (when (< max-rev rev-to-fetch)
	(error "git-svn is acting up and refuses to fetch svn revisions up to r%d" rev-to-fetch))

      (setq sha1 (egg-git-to-string "rev-parse" full-ref))
      (setq new-rev (egg-git-to-string "svn" "find-rev" full-ref))
      (if (equal (and new-rev (egg-git-to-string "svn" "find-rev" new-rev full-ref))
		 sha1)
	  full-ref
	(error "Failed fetching %s to %s" svn-path full-ref)
	nil))))


(defun egg-fetch-from-svn (buffer-to-update svn-remote r-ref)
  (let* ((r-ref (if (equal r-ref "--all") nil r-ref))
	 (svn-name (car svn-remote))
	 (map-type (nth 1 svn-remote))
	 (url (and svn-name (egg-git-svn-url svn-name)))
	 (local-base (nth 3 svn-remote))
	 full-ref short-ref svn-path svn-path-url local-name
	 max-rev rev-to-fetch res line fetch-unknown)

    (setq full-ref 
	  (save-match-data
	    (cond ((null r-ref)
		   ;; --all
		   nil)
		  ((string-match "\\`refs/remotes/" r-ref)
		   ;; r-ref is fully-named ref
		   r-ref)
		  ((string-match "/" r-ref)
		   ;; svn-path
		   (setq svn-path r-ref)
		   nil)
		  ((eq map-type :fetch)
		   ;; e.g. cursor was on svn/release1 and r-ref is release2
		   (concat (file-name-nondirectory local-base) r-ref))
		  ((eq map-type :branches)
		   ;; e.g. cursor was on user1/my_stuffs_a and r-ref is my_stuffs_b
		   (concat local-base r-ref))
		  (t (error "Cannot determine full-ref from %s" r-ref)))))

    (if r-ref
	(progn
	  ;; not fetching --all
	  (if svn-path
	      ;; r-ref is svn-path, fetch from and svn and map
	      (setq full-ref (egg-fetch-unknown-svn-path buffer-to-update 
							 svn-remote svn-path local-base))
	    ;; r-ref might be a known ref which might need update
	    ;; or a new ref based on an existing mapping.
	    (setq svn-path (egg-git-svn-map-full-ref full-ref))
	    (unless (egg-svn-is-known-ref full-ref)
	      ;; new ref based on an existing mapping.
	      ;; how ever, the user might specify a new custom mapping
	      (setq full-ref (egg-fetch-unknown-svn-path buffer-to-update 
							 svn-remote svn-path local-base))))

	  (setq local-base (file-name-directory full-ref))
	  (setq svn-path-url (concat url "/" svn-path))
	  (setq local-name full-ref))
      ;; fetching --all
      (setq svn-path-url url)
      (setq local-name "everything"))

    (setq rev-to-fetch (string-to-number (egg-svn-path-last-rev svn-path-url)))

    (setq max-rev (string-to-number (egg-git-svn-max-rev svn-name)))
    (if (>= max-rev rev-to-fetch)
	(if fetch-unknown 
	    (message "%s is now up-to-date, no extra fetching required!" local-name)
	  (message "%s is already up-to-date, no fetching required!" local-name))
      (setq res (egg--git-svn-fetch buffer-to-update))
      (setq line (plist-get res :line))
      (unless (plist-get res :success)
	(error "Failed to fetch svn revisions from r%d to r%d: %s" 
	       max-rev rev-to-fetch line)))
      
    (setq max-rev (string-to-number (egg-git-svn-max-rev svn-name)))
    (when (< max-rev rev-to-fetch)
      (message "failed to fetch r%d, retry fetching with explicit -r%d" 
	       rev-to-fetch rev-to-fetch)
      (setq res (egg--git-svn-fetch-rev buffer-to-update rev-to-fetch))
      (setq line (plist-get res :line))
      (unless (plist-get res :success)
	(error "Failed to fetch svn r%d: %s" rev-to-fetch line)))))



(add-to-list 'egg-add-remote-properties #'egg-svn-add-remote-properies)
(add-to-list 'egg-get-remote-properties #'egg-svn-get-remote-properies)
(add-to-list 'egg-get-all-remotes #'egg-svn-get-all-prefixes)

(provide 'egg-svn)
