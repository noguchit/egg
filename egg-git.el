;;; egg-base.el --- Emacs Got Git - Emacs interface to Git

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

(require 'egg-custom)
(require 'egg-base)

(defsubst egg-git-ok (buffer &rest args)
  "run GIT with ARGS and insert output into BUFFER at point.
return the t if the exit-code was 0. if BUFFER was t then
current-buffer would be used."
  (= (apply 'call-process egg-git-command nil buffer nil args) 0))

(defsubst egg-git-ok-args (buffer args)
  "run GIT with ARGS and insert output into BUFFER at point.
return the t if the exit-code was 0. if BUFFER was t then
current-buffer would be used."
  (= (apply 'call-process egg-git-command nil buffer nil args) 0))



(defsubst egg-commit-contents (rev)
  "Retrieve the raw-contents of the commit REV."
  (with-temp-buffer
    (call-process egg-git-command nil t nil "cat-file" "commit" rev)
    (buffer-string)))

(defsubst egg-commit-message (rev)
  "Retrieve the commit message of REV."
  (save-match-data
    (with-temp-buffer
      (call-process egg-git-command nil t nil "cat-file" "commit" rev)
      (goto-char (point-min))
      (re-search-forward "^\n")
      (buffer-substring-no-properties (match-end 0) (point-max)))))

(defun egg-commit-subject (rev)
  "Retrieve the commit subject of REV."
  (save-match-data
    (with-temp-buffer
      (call-process egg-git-command nil t nil "cat-file" "commit" rev)
      (goto-char (point-min))
      (re-search-forward "^\n")
      (buffer-substring-no-properties (match-end 0)
				      (if (or (re-search-forward "\n\n" nil t)
					      (re-search-forward "\n" nil t))
					  (match-beginning 0)
					(point-max))))))

(defsubst egg-cmd-to-string-1 (program args)
  "Execute PROGRAM and return its output as a string.
ARGS is a list of arguments to pass to PROGRAM."
  (with-temp-buffer
    (if (= (apply 'call-process program nil t nil args) 0)
        (buffer-substring-no-properties
         (point-min) (if (> (point-max) (point-min))
                         (1- (point-max)) (point-max))))))

(defsubst egg-cmd-to-string (program &rest args)
  "Execute PROGRAM and return its output as a string.
ARGS is a list of arguments to pass to PROGRAM."
  (egg-cmd-to-string-1 program args))


(defsubst egg-git-to-string (&rest args)
  "run GIT wih ARGS and return the output as a string."
  (egg-cmd-to-string-1 egg-git-command args))

(defun egg-git-show-file-args (buffer file rev args)
  (let* ((mode (assoc-default file auto-mode-alist 'string-match))
	 (extras (and mode (assoc-default mode egg-git-diff-file-options-alist 'eq))))
    (egg-git-ok-args buffer (append (list "--no-pager" "show")
				    extras
				    args
				    (list rev "--" file)))))

(defsubst egg-git-show-file (buffer file rev &rest args)
  (egg-git-show-file-args buffer file rev args))

(defsubst egg-git-region-ok (start end &rest args)
  "run GIT with ARGS and insert output into current buffer at point.
return the t if the exit-code was 0. The text between START and END
is used as input to GIT."
  (= (apply 'call-process-region start end egg-git-command t t nil args) 0))

(defsubst egg-wdir-clean () (egg-git-ok nil "diff" "--quiet"))
(defsubst egg-file-updated (file)
  (egg-git-ok nil "diff" "--quiet" "--" file))
(defsubst egg-file-committed (file)
  (egg-git-ok nil "diff" "--quiet" "HEAD" "--" file))
(defsubst egg-file-index-empty (file)
  (egg-git-ok nil "diff" "--quiet" "--cached" "--" file))
(defsubst egg-index-empty () (egg-git-ok nil "diff" "--cached" "--quiet"))

(defsubst egg-has-stashed-wip ()
  (egg-git-ok nil "rev-parse" "--verify" "-q" "stash@{0}"))


(defsubst egg-git-to-lines (&rest args)
  "run GIT with ARGS.
Return the output lines as a list of strings."
  (save-match-data
    (split-string (or (egg-cmd-to-string-1 egg-git-command args) "")
                  "[\n]+" t)))

(defsubst egg-git-to-string-list (&rest args)
  "run GIT with ARGS.
Return the output lines as a list of strings."
  (save-match-data
    (split-string (or (egg-cmd-to-string-1 egg-git-command args) "")
                  "[\n\t ]+" t)))

(defun egg-git-lines-matching (re idx &rest args)
  "run GIT with ARGS.
Return the output lines as a list of strings."
  (with-temp-buffer
    (when (= (apply 'call-process egg-git-command nil t nil args) 0)
      (let (lines)
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward re nil t)
            (setq lines (cons (match-string-no-properties idx) lines)))
          lines)))))

(defun egg-git-lines-matching-stdin (stdin re idx &rest args)
  "run GIT with ARGS.
Return the output lines as a list of strings."
  (with-temp-buffer
    (let (lines pos)
      (insert stdin)
      (setq pos (point-max))
      (when (= (apply 'call-process-region (point-min) (point-max)
		      egg-git-command nil t nil args) 0)
        (save-match-data
          (goto-char pos)
          (while (re-search-forward re nil t)
            (setq lines (cons (match-string-no-properties idx) lines)))
          lines)))))

(defun egg-git-lines-matching-multi (re indices &rest args)
  "run GIT with ARGS.
Return the output lines as a list of strings."
  (with-temp-buffer
    (when (= (apply 'call-process egg-git-command nil t nil args) 0)
      (let (lines matches)
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward re nil t)
            (setq matches nil)
            (dolist (idx indices)
              (when (match-beginning idx)
                (setq matches
                      (cons (cons idx (match-string-no-properties idx))
                            matches))))
            (setq lines (cons matches lines)))
          lines)))))

(defsubst egg-file-git-name (file)
  "return the repo-relative name of FILE."
  (car (egg-git-to-lines "ls-files" "--full-name" "--" file)))

(defsubst egg-buf-git-name (&optional buf)
  "return the repo-relative name of the file visited by BUF.
if BUF was nil then use current-buffer"
  (egg-file-git-name (file-truename (buffer-file-name buf))))

(defsubst egg-files-git-name (files)
  "return the repo-relative name for each file in the list of files FILES."
  (delete-dups
   (apply 'egg-git-to-lines "ls-files" "--full-name" "--" files)))

(defsubst egg-unmerged-files ()
  "return a list of repo-relative names for each unmerged files."
  (save-match-data
    (delete-dups
     (mapcar 'car
             (mapcar 'last
                     (mapcar
                      'split-string
                      (egg-git-to-lines "ls-files" "--full-name" "-u")))))))

(defun egg--get-status-code ()
  (let ((lines (egg-git-to-lines "status" "--porcelain" "--untracked-files=no"))
	alist code index dir status)
    (dolist (line lines)
      (setq code (substring line 0 2))
      (setq index (aref code 0)
	    dir (aref code 1))
      (setq status nil)

      (cond ((and (= dir ? ) (memq index '(?M ?A ?R ?C))) 
	     (add-to-list 'status :wdir-index))
	    ((and (= dir ?M) (memq index '(?M ?A ?R ?C ? ))) 
	     (add-to-list 'status :wdir-modified))
	    ((= dir ?D)
	     (if (memq index '(?M ?A ?R ?C ? ))
		 (add-to-list 'status :wdir-deleted)
	       (add-to-list 'status :unmerged)
	       (cond ((= index ?D) (add-to-list 'status :both-deleted))
		     ((= index ?U) (add-to-list 'status :they-deleted)))))
	    ((= dir ?U)
	     (add-to-list 'status :unmerged)
	     (cond ((= index ?A) (add-to-list 'status :we-added))
		   ((= index ?D) (add-to-list 'status :we-deleted))
		   ((= index ?U) (add-to-list 'status :both-modified))))
	    ((= dir ?A)
	     (add-to-list 'status :unmerged)
	     (cond ((= index ?U) (add-to-list 'status :they-added))
		   ((= index ?A) (add-to-list 'status :both-added)))))

      (cond ((= index ? ) (add-to-list 'status :index-head))
	    ((and (= index ?M) (memq dir '(?M ?D ? ))) (add-to-list 'status :index-modified))
	    ((and (= index ?A) (memq dir '(?M ?D ? ))) (add-to-list 'status :index-added))
	    ((and (= index ?D) (memq dir '(?M ? ))) (add-to-list 'status :index-deleted))
	    ((and (= index ?R) (memq dir '(?M ?D ? ))) (add-to-list 'status :index-moved))
	    ((and (= index ?C) (memq dir '(?M ?D ? ))) (add-to-list 'status :index-copied)))
      
      (add-to-list 'alist (cons (substring line 3) status)))
    alist))

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

(defun egg-upstream (branch)
  (and (egg-git-ok nil "config" (concat "branch." branch ".merge"))
       (let ((upstream (egg-git-to-string "name-rev" "--name-only" 
					  (concat branch "@{upstream}"))))
	 (if (and (> (length upstream) 8)
		  (string-equal (substring upstream 0 8) "remotes/"))
	     (substring upstream 8)
	   upstream))))






(defsubst egg-is-in-git ()
  "is the default-directory in a git repo."
  (= (call-process egg-git-command nil nil nil "rev-parse" "--git-dir") 0))

(defsubst egg-is-dir-in-git (dir)
  "is DIR in a git repo."
  (let ((default-directory dir)) (egg-is-in-git)))

(defsubst egg-name-rev (rev)
  "get the symbolic name of REV."
  (egg-git-to-string "name-rev" "--always" "--name-only" rev))

(defun egg-pretty-short-rev (rev)
  (let ((rev (egg-name-rev rev))
	(short-sha (and rev (substring (egg-git-to-string "rev-parse" rev) 0 8))))
    (save-match-data
      (when (string-match "\\`\\(remotes\\|tags\\)/" rev)
	(setq rev (substring rev (match-end 0)))))
    (if (> (length rev) 24)
	short-sha
      rev)))

(defsubst egg-git-canon-name (rev file)
  (when (and rev file)
    (concat rev ":" (egg-file-git-name file))))

(defsubst egg-describe-rev (rev)
  "get the long symbolic name of REV."
  (egg-git-to-string "describe" "--always" "--tags" rev))

(defsubst egg-sha1 (rev)
  "get the SHA1 of REV."
  (egg-git-to-string "rev-parse" (concat rev "~0")))

(defun egg-completing-read-sha1 (from prompt &optional default max-back)
  (let* ((max-count (or max-back 100))
	 (sha1-list (egg-git-to-lines "rev-list" "--topo-order"
				      (format "--max-count=%d" max-count)
				      "--abbrev-commit" 
				     from))
	 (sha1-hist (copy-sequence sha1-list)))
    (egg-sha1 (completing-read prompt sha1-list nil nil default 'sha1-hist))))

(defun egg-read-git-dir ()
  "call GIT to read the git directory of default-directory."
  (let* ((dotgit-parent (locate-dominating-file default-directory ".git"))
	 (dotgit (and dotgit-parent (concat dotgit-parent "/.git")))
         (dir (or (and dotgit (file-directory-p dotgit) dotgit)
		  (egg-git-to-string "rev-parse" "--git-dir")))
	 (work-tree dotgit-parent))
    (when (stringp dir)
      (setq dir (expand-file-name dir))
      (when (stringp work-tree) 
	(setq work-tree (expand-file-name work-tree))
	(put-text-property 0 (length dir) :work-tree work-tree dir))
      dir)))

(defvar egg-git-dir nil)
(defun egg-git-dir (&optional error-if-not-git)
  "return the (pre-read) git-dir of default-directory"
  (if (and (local-variable-p 'egg-git-dir) egg-git-dir)
      egg-git-dir
    (set (make-local-variable 'egg-git-dir)
         (or (egg-read-git-dir)
             (and error-if-not-git
                  (or (kill-local-variable 'egg-git-dir) t)
                  (error "Not in a git repository: %s" default-directory))))
    ;; first time, no status yet.
    ;; this directory's specific var will be updated by
    ;; egg-set-mode-info
    (set (intern (concat "egg-" egg-git-dir "-HEAD")) " Egg")
    egg-git-dir))

(defsubst egg-work-tree-dir (&optional git-dir)
  (unless git-dir (setq git-dir (egg-git-dir)))
  (or (get-text-property 0 :work-tree git-dir)
      (file-name-directory git-dir)))

(defsubst egg-repo-name (&optional git-dir)
  (let* ((dir (or git-dir (egg-git-dir)))
	 (work-tree-dir (egg-work-tree-dir dir)))
    (when (stringp work-tree-dir)
      (file-name-nondirectory (directory-file-name work-tree-dir)))))

(defsubst egg-buf-git-dir (buffer)
  "return the (pre-read) git-dir of BUFFER."
  (with-current-buffer buffer
    (egg-git-dir)))

(defun egg-commit-parents (rev)
  (let ((default-directory (egg-work-tree-dir))
	parents)
    (with-temp-buffer
      (egg-git-ok t "--no-pager" "cat-file" "-p" rev)
      (goto-char (point-min))
      (while (re-search-forward (rx line-start 
				    "parent " (group (= 40 hex-digit)) 
				    (0+ space)
				    line-end) nil t)
	(add-to-list 'parents (match-string-no-properties 1)))
      (setq parents (mapcar (lambda (long)
			      (substring-no-properties long 0 8))
			    (nreverse parents))))
    parents))

(defun egg-HEAD ()
  "return HEAD. Either a symbolic ref or a sha1."
  (let* ((git-dir (egg-git-dir)))
    (if git-dir
        (egg-pick-file-contents (concat git-dir "/HEAD")
                                "^ref: refs/heads/\\(.+\\)\\|^\\([0-9a-f]+\\)" 1 2))))

(defsubst egg-get-symbolic-HEAD (&optional file)
  ;; get the symbolic name of HEAD
  (setq file (or file (concat (egg-git-dir) "/HEAD")))
  (egg-pick-file-contents file
                          "^ref: refs/heads/\\(.+\\)"
                          1))

(defsubst egg-get-full-symbolic-HEAD (&optional file)
  ;; get the symbolic full name of HEAD
  (setq file (or file (concat (egg-git-dir) "/HEAD")))
  (egg-pick-file-contents file
                          "^ref: \\(refs/heads/.+\\)"
                          1))

(defsubst egg-get-current-sha1 ()
  (or (egg-git-to-string "rev-parse" "--verify" "-q" "HEAD")
      "0000000000000000000000000000000000000000"))

(defun egg-get-all-refs (prefix)
  (egg-git-to-lines "for-each-ref" "--format=%(refname:short)" 
		    (format "refs/heads/%s*" prefix)
		    (format "refs/tags/%s*" prefix)
		    (format "refs/remotes/%s*/*" prefix)
		    (format "refs/remotes/%s*" prefix)))

(defun egg-get-local-refs (prefix)
  (egg-git-to-lines "for-each-ref" "--format=%(refname:short)" 
		    (format "refs/heads/%s*" prefix)
		    (format "refs/tags/%s*" prefix)))

(defsubst egg-git-rebase-dir (&optional git-dir)
  (concat (or git-dir (egg-git-dir)) "/" egg-git-rebase-subdir "/"))

(defsubst egg-rebase-author-info (rebase-dir)
  "Retrieve an alist of commit environment variables of the current
cherry in REBASE-DIR."
  (mapcar (lambda (lst)
            ;; chop the ' '
            (setcar (cdr lst) (substring (cadr lst) 1 -1))
            lst)
          (mapcar (lambda (line)
                    ;; name-value split
                    (save-match-data (split-string line "=" t)))
                  ;; grab the GIT_xxx=yyy
                  (egg-pick-file-records (concat rebase-dir "author-script")
                                         "^GIT_\\(.+\\)" "$"))))

(defsubst egg-interactive-rebase-in-progress ()
  "Is an interactive rebase in progress in the current repo?"
  (file-exists-p (concat (egg-git-dir) "/" egg-git-rebase-subdir
                         "/interactive") ))


(defsubst egg-get-rebase-apply-state (rebase-dir)
  "Build a plist of rebase info of REBASE-DIR.
this is for rebase -m variant."
  (let ((patch-files (directory-files rebase-dir nil "\\`[0-9]+\\'")))
    (list :rebase-dir rebase-dir
        :rebase-head (egg-pretty-short-rev 
		      (egg-file-as-string (concat rebase-dir "head-name")))
        :rebase-upstream
        (egg-pretty-short-rev (egg-file-as-string (concat rebase-dir "onto")))
        :rebase-step (string-to-number (car patch-files))
        :rebase-num (string-to-number (car (nreverse patch-files))))))

(defsubst egg-get-rebase-merge-state (rebase-dir)
  "Build a plist of rebase info of REBASE-DIR.
this is for rebase -m variant."
  (list :rebase-dir rebase-dir
        :rebase-head
        (egg-pretty-short-rev (egg-file-as-string (concat rebase-dir "head-name")))
        :rebase-upstream
        (egg-pretty-short-rev (egg-file-as-string (concat rebase-dir "onto_name")))
        :rebase-step			;; string-to-number?
        (egg-file-as-string (concat rebase-dir "msgnum"))
        :rebase-num			;; string-to-number?
        (egg-file-as-string (concat rebase-dir "end"))))

(defsubst egg-get-rebase-interactive-state (rebase-dir)
  "Build a plist of rebase info of REBASE-DIR.
this is for rebase -i variant."
  (list :rebase-dir rebase-dir
        :rebase-head
        (egg-pretty-short-rev (egg-file-as-string (concat rebase-dir "head-name")))
        :rebase-upstream
        (egg-pretty-short-rev (egg-file-as-string (concat rebase-dir "onto")))
        :rebase-num
        (length
         (egg-pick-file-records (concat rebase-dir "git-rebase-todo.backup")
                                "^[pesf]" "$"))
        :rebase-step
        (if (file-exists-p (concat rebase-dir "done"))
            (length (egg-pick-file-records (concat rebase-dir "done")
                                           "^[pesf]" "$"))
          0)
	:rebase-stopped
	(if (file-exists-p (concat rebase-dir "stopped-sha"))
	    (egg-pick-file-contents (concat rebase-dir "stopped-sha") "^[0-9a-f]+$"))
        :rebase-cherry
        (if (file-exists-p (concat rebase-dir "done"))
            (car (egg-pick-file-records
                  (concat rebase-dir "done")
                  "^[pesf]" "$")))))

(defsubst egg-set-mode-info (state)
  "Set the mode-line string for buffers visiting files in the current repo.
The string is built based on the current state STATE."
  (set (intern (concat "egg-" egg-git-dir "-HEAD"))
       (format " Git:%s" (cond ((plist-get state :rebase-dir)
                                "(rebasing)")
                               ((plist-get state :merge-heads)
                                "(merging)")
			       ((plist-get state :squash-head)
                                "(squashing)")
                               ((plist-get state :branch)
                                (plist-get state :branch))
                               (t "(detached)")))))

(defvar egg-internal-current-state nil)
(defun egg-get-repo-state (&optional extras)
  "Retrieve current repo's state as a plist.
The properties:
:gitdir :head :branch :sha1 :merge-heads :rebase-dir :rebase-head
:rebase-upstream :rebase-step :rebase-num :rebase-cherry

EXTRAS contains the extra properties to retrieve: :staged :unstaged

if EXTRAS contains :error-if-not-git then error-out if not a git repo.
"
  (let* ((git-dir (egg-git-dir (memq :error-if-not-git extras)))
         (head-file (concat git-dir "/HEAD"))
         (merge-file (concat git-dir "/MERGE_HEAD"))
	 (squash-file (concat git-dir "/SQUASH_MSG"))
         (branch (egg-get-symbolic-HEAD head-file))
         (branch-full-name (egg-get-full-symbolic-HEAD head-file))
         (sha1 (egg-get-current-sha1))
         (merge-heads
          (mapcar 'egg-pretty-short-rev
                  (if (file-readable-p merge-file)
                      (egg-pick-file-records merge-file "^" "$"))))
	 (squash-head (when (file-readable-p squash-file)
			(egg-pick-file-contents squash-file (rx line-start "commit " 
								(group (= 40 hex-digit))
								line-end)
						1)))
	 (rebase-apply (if (file-directory-p (concat git-dir "/rebase-apply"))
			   (concat git-dir "/rebase-apply/")))
         (rebase-dir
	  (or rebase-apply
	      (if (file-directory-p (concat git-dir "/" egg-git-rebase-subdir))
		  (concat git-dir "/" egg-git-rebase-subdir "/"))))
         (is-rebase-interactive 
	  (and (not rebase-apply)
	       (file-exists-p (concat rebase-dir "interactive"))))
         (rebase-state
          (if rebase-apply
	      (egg-get-rebase-apply-state rebase-dir)
	      (when rebase-dir
		(if is-rebase-interactive
		    (egg-get-rebase-interactive-state rebase-dir)
		  (egg-get-rebase-merge-state rebase-dir)))))
         (state (nconc (list :gitdir git-dir
                             :head branch-full-name
                             :branch branch
                             :sha1 sha1
			     :squash-head (and squash-head (egg-pretty-short-rev squash-head))
                             :merge-heads merge-heads)
                       rebase-state))
         files)
    (dolist (req extras)
      (cond ((eq req :unstaged)
             (setq files (egg-git-to-lines "diff" "--name-only"))
             (setq state (nconc (list :unstaged files) state))
             (when (and files (stringp (car files)))
               (setq state (nconc (list :unmerged (egg-unmerged-files))
                                  state))))
            ((eq req :staged)
             (setq state
                   (nconc (list :staged (egg-git-to-lines "diff" "--cached" "--name-only"))
                          state)))
	    
	    ((eq req :name)
             (setq state
                   (nconc (list :name (egg-git-to-string "config" "user.name")) state)))
	    ((eq req :email)
             (setq state
                   (nconc (list :email (egg-git-to-string "config" "user.email")) state)))))
    ;; update mode-line
    (egg-set-mode-info state)
    state))

(defun egg-repo-state (&rest args)
  "return the cached repo state or re-read it.
if ARGS contained :force then ignore the cached state."
  (if (or (null egg-internal-current-state) ;; not cached
	  (memq :force args)		    ;; forced
	  (memq nil ;; cached copy has no extra reqs
		(mapcar (lambda (req)
			  (memq req egg-internal-current-state))
			args)))
      (egg-get-repo-state args)
    egg-internal-current-state))

(defsubst egg-repo-clean (&optional state)
  "Whether the current repos is clean base on the current repo state.
use STATE as repo state if it was not nil. Otherwise re-read the repo state."
  (unless state
    (setq state (egg-repo-state :staged :unstaged)))
  (and
   (null (plist-get state :rebase-num))
   (null (plist-get state :merge-heads))
   (null (plist-get state :squash-head))
   (not (if (memq :unstaged state)
            (plist-get state :unstaged)
          (egg-wdir-clean)))
   (not (if (memq :staged state)
            (plist-get state :staged)
          (egg-index-empty)))))

(defsubst egg-is-merging (state)
  (or (plist-get state :merge-heads)
      (plist-get state :rebase-dir)
      (plist-get state :squash-head)))

(defun egg-wdir-dirty () (plist-get (egg-repo-state :unstaged) :unstaged))
(defun egg-staged-changes () (plist-get (egg-repo-state :staged) :staged))

(defsubst egg-current-branch (&optional state)
  "The current symbolic value of HEAD. i.e. name of a branch. if STATE
was not nil then use it as repo state instead of re-read from disc."
  (plist-get (or state (egg-repo-state)) :branch))

(defsubst egg-current-sha1 (&optional state)
  "The immutable sha1 of HEAD.  if STATE was not nil then use it
as repo state instead of re-read from disc."
  (plist-get (or state (egg-repo-state)) :sha1))

(defsubst egg-short-sha1 (&optional sha1)
  (egg-git-to-string "rev-parse" "--short" (or sha1 (egg-current-sha1))))

(defsubst egg-user-name (&optional state)
  "The configured user name."
  (plist-get (or state (egg-repo-state :name)) :name))

(defsubst egg-user-email (&optional state)
  "The configured email."
  (plist-get (or state (egg-repo-state :email)) :email))

(defsubst egg-head (&optional state)
  "a cons cell (branch . sha1) of HEAD.  if STATE was not nil then use it
as repo state instead of re-read from disc."
  (if (egg-git-dir)
      (let ((state (or state (egg-repo-state))))
        (cons (egg-current-sha1 state)
              (egg-current-branch state)))))

(defsubst egg-branch-or-HEAD () (or (egg-get-symbolic-HEAD) "HEAD"))


(defsubst egg-config-section-raw (type &optional name)
  (egg-pick-file-contents (concat (egg-git-dir) "/config")
                          (concat "^"
                                  (if name
                                      (format "\\[%s \"%s\"\\]" type name)
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

(defun egg-config-get-all (called-interactively file type)
  (interactive "p\nfFilename: \nsType: ")
  (let (res)
    (setq res
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
    (if called-interactively
	(message "%S" res))
    res))

(defsubst egg-config-get-all-branches ()
  (egg-config-get-all nil (concat (egg-git-dir) "/config") "branch"))

(defsubst egg-config-get-all-remotes ()
  (egg-config-get-all nil (concat (egg-git-dir) "/config") "remote"))

(defsubst egg-config-get-all-remote-names ()
  (mapcar 'car (egg-config-get-all-remotes)))

(defsubst egg-config-get (type attr &optional name)
  (and (egg-git-dir)
       (cadr (assoc attr (egg-config-section type name)))))

(defun egg-tracking-target (branch &optional mode)
  (let ((remote (egg-config-get "branch" "remote" branch))
        (rbranch (egg-config-get "branch" "merge" branch)))
    (when (stringp rbranch)
      (setq rbranch (egg-rbranch-name rbranch))
      (cond ((null mode) (concat remote "/" rbranch))
            ((eq :name-only mode) rbranch)
            (t (cons rbranch remote))))))

(defun egg-complete-get-all-refs (prefix &optional matches)
  (if matches
      (try-completion prefix matches)
    (egg-get-all-refs prefix)))

(defun egg-complete-get-local-refs (prefix &optional matches)
  (if matches
      (try-completion prefix matches)
    (egg-get-local-refs prefix)))

(defun egg-get-match-files-substring (sub &optional matches)
  (if matches
      (try-completion sub (mapcar #'file-name-nondirectory matches))
    (let ((default-directory (egg-work-tree-dir))
	  files name-matched-files full-match)
      (setq files (egg-git-to-lines "--no-pager" "ls-files" 
				    (concat sub "*")
				    (concat "*/" sub "*")))
      (dolist (file files)
	(if (string-equal file sub)
	    (setq full-match file))
	(if (string-equal (file-name-nondirectory file) sub)
	    (add-to-list 'name-matched-files file)))
      (or (and full-match (list full-match))
	  name-matched-files
	  files))))

(defun egg-do-completion (string &optional func all)
  "Do ref name completion"
  (let* ((matches (funcall func string))
	 (single (= (length matches) 1))
	 (perfect (and single (equal (car matches) string)))
	 prefix)

    (if all matches
      (when matches
	(setq prefix (funcall func string matches)))
      (cond ((null matches) nil)
	    (perfect t)
	    (single (car matches))
	    ((stringp prefix) prefix)
	    ((null prefix) nil)
	    (t string)))))

(defsubst egg-read-ref (prompt &optional default no-match-ok)
  (completing-read prompt #'egg-do-completion #'egg-complete-get-all-refs (not no-match-ok) default))

(defsubst egg-read-local-ref (prompt &optional default no-match-ok)
  (completing-read prompt #'egg-do-completion #'egg-complete-get-local-refs (not no-match-ok) default))



(provide 'egg-git)