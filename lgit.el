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

;;; simple routines

(defun lgit-cmd-to-string-1 (program args)
  "Execute PROGRAM and return its output as a string.
ARGS is a list of arguments to pass to PROGRAM."
  (with-output-to-string
    (with-current-buffer
	standard-output
      (apply 'call-process program nil t nil args))))

(defun lgit-cmd-to-string (program &rest args)
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

(defun lgit-git-to-lines (&rest args)
  (split-string (substring (lgit-cmd-to-string-1 "git" args) 0 -1)
		"\n"))

(defun lgit-local-branches ()
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

(defun lgit-current-branch ()
  (let ((ref (lgit-git-to-string "symbolic-ref" "-q" "HEAD")))
    (save-match-data
      (if (string-match "\\`refs/heads/\\(.+\\)" ref)
	  (match-string 1 ref)))))

(defun lgit-current-sha1 ()
  (lgit-git-to-string "rev-parse" "--verify" "-q" "HEAD"))

(defun lgit-HEAD ()
  (cons (lgit-current-sha1) (lgit-current-branch)))

(defun lgit-config-get (&rest keys)
  (lgit-git-to-string "config"
		      (mapconcat 'identity keys ".")))

(defun lgit-bool-config (&rest keys)
  (let* ((qual-name (mapconcat 'identity keys "."))
	 (flag (lgit-git-to-string "config" qual-name)))
    (cond ((equal flag "true")
	   t)
	  ((equal flag "false")
	   nil)
	  (t (error "Unexpected contents of boolean config %s"
		    qual-name)))))

(provide 'lgit)
