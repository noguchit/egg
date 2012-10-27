;;; egg-key.el --- Emacs Got Git - Emacs interface to Git

;; Copyright (C) 2012  Bogolisk
;;
;; Author: Bogolisk <bogolisk@gmail.com>
;; Created: 26 Oct 2012
;; Version: 1.0.2
;; Keywords: git, version control, release management
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

(require 'egg)

(defun egg-key-get-cmd-doc (cmd name regex &optional sub-no)
  (let (line-1 desc)
    (save-match-data
      (setq line-1 (car (split-string (documentation cmd) "\n")))
      (setq desc (if (and (stringp regex) (string-match regex line-1))
		     (replace-match name nil t line-1 sub-no)
		   line-1)))
    desc))

(defun egg-key-get-menu-heading (map-name name)
  (let ((fmt (assoc-default map-name '(("Egg:LogCommit" . "Operations on Commit %s")))))
    (when fmt
      (format fmt name))))

;;(egg-key-get-cmd-doc #'egg-log-buffer-push-to-local "testing" "commit at POS")

(defun egg-key-make-alist (name map regex &optional sub-no)
  (let (key cmd desc heading alist)
    (dolist (mapping map)
      (cond ((and (consp mapping) 
		  (characterp (setq key (car mapping)))
		  (commandp (setq cmd (cdr mapping))))
	     ;; just add support for C-c C-c
	     (setq desc (egg-key-get-cmd-doc cmd name regex sub-no))
	     (add-to-list 'alist (list key cmd desc)))
	    ((and (stringp mapping) (null heading))
	     (setq heading (egg-key-get-menu-heading mapping name)))))
    (cons heading alist)))

;;(where-is-internal #'egg-log-locate-commit egg-secondary-log-commit-map)
;;(lookup-key egg-secondary-log-commit-map [3 3])

;; (egg-key-make-alist "053a3f32" egg-log-commit-map
;; 		    "\\([Cc]ommit\\|rev\\) at POS\\|the current section")

(defmacro with-egg-key-buffer (&rest body)
  "Evaluate BODY there like `progn' in the egg-key buffer.
See also `with-temp-file' and `with-output-to-string'."
  (declare (indent 0) (debug t))
  (let ((egg-key-buffer (make-symbol "egg-key-buffer"))
	(egg-key-dir (make-symbol "egg-key-dir")))
    `(let ((,egg-key-dir (egg-work-tree-dir))
	   (,egg-key-buffer (get-buffer-create (concat "*egg-key:" (egg-git-dir) "*"))))
       (with-current-buffer ,egg-key-buffer
	 (setq default-directory ,egg-key-dir)
         (unwind-protect
	     (progn ,@body)
           )))))

;; (defun egg-key-dummy (prefix)
;;   (interactive "p")
;;   (message "%s" prefix))

;; (defun egg-key-cmd ()
;;   (interactive)
;;   (let* ((keys (this-command-keys))
;; 	 (len (length keys)))
;;     (message "%s" keys)
;;     (aset keys (1- len) ?y)
;;     (execute-kbd-macro keys)))

(defun egg-key-electric-throw-key (&optional prefix)
  (interactive "P")
  (when (boundp 'egg-key-electric-in-progress-p)
    (let ((keys (this-single-command-keys)))
      (throw 'egg-key-selection (cons keys prefix)))))

(defun egg-key-electric-throw-selection (&optional prefix)
  (interactive "P")
  (when (boundp 'egg-key-electric-in-progress-p)
    (let ((keys (get-text-property (point) :selected-key)))
      (throw 'egg-key-selection keys))))

(defun egg-key-electric-quit ()
  (interactive)
  (when (boundp 'egg-key-electric-in-progress-p)
    (throw 'egg-key-selection nil)))

(defun egg-key-make-electric-map (alist select-keys quit-keys)
  (let (map)
    (setq map (cons 'keymap
		    (mapcar (lambda (key)
			      (cons key 'egg-key-electric-throw-key))
			    (mapcar 'car alist))))
    (define-key map select-keys 'egg-key-electric-throw-selection)
    (define-key map quit-keys 'egg-key-electric-quit)
    map))

(defun egg-key-electric (heading alist)
  (let ((egg-key-electric-in-progress-p t)
        (old-buffer (current-buffer))
	key cmd desc selection
	beg pos map buf)
    (setq map (egg-key-make-electric-map alist "z" "Z"))
    (unwind-protect
        (setq selection
              (catch 'egg-key-selection
                (save-window-excursion
                  (with-egg-key-buffer
		   (setq buf (current-buffer))
		   (let ((inhibit-read-only t))
		     (erase-buffer)
		     (insert (egg-text heading 'egg-section-title))
		     (insert (egg-text "commands:" 'egg-text-1) "\n\n")
		     (put-text-property (point-min) (point) 'intangible t)
		     (setq beg (point))
		     (dolist (mapping alist)
		       (setq key (nth 0 mapping)
			     cmd (nth 1 mapping)
			     desc (nth 2 mapping))
		       (setq pos (point))
		       (insert (format " %3s - %s\n"
				       (edmacro-format-keys (string key))
				       desc))
		       (put-text-property pos (point) :selected-key (vector key)))
		     (goto-char beg)
		     (set-buffer-modified-p nil)
		     (setq buffer-read-only t))
		   (setq major-mode 'egg-key)
		   (setq mode-name "Egg-Key")
		   (use-local-map map))
                  (Electric-pop-up-window buf)
                  (goto-char beg)
                  (Electric-command-loop 'egg-key-selection
                                         "select command> "))))
      (when (bufferp buf)
	(bury-buffer buf)))
    selection))

(defun egg-key-prompt (pos)
  (interactive "d")
  (let ((map (get-text-property pos 'keymap))
	(ref (egg-ref-at-point pos))
	(commit (egg-commit-at-point))
	heading alist
	short name key-info selection)
    (setq short (substring commit 0 8))
    (setq key-info (cond (ref
			  (egg-key-make-alist
			   ref map 
			   "\\(ref\\|branch\\|remote\\) at POS\\|the current section"))
			 (commit
			  (egg-key-make-alist
			   short map
			   "\\([Cc]ommit\\|rev\\) at POS\\|the current section"))))
    (setq heading (car key-info)
	  alist (cdr key-info))
    (setq selection (egg-key-electric heading alist))
    (let (current-prefix-arg cmd key)
      (cond ((consp selection)
	     (setq current-prefix-arg (cdr selection))
	     (setq key (car selection))
	     (setq cmd (lookup-key map key))
	     (unless cmd
	       (error "can't find mapping for key: %c (%s)" 
		      (car selection) (car selection)))
	     (command-execute cmd nil nil t))
	    ((null selection)
	     (message "later!")
	     (ding))
	    (t (error "wtf! selection is: %s" selection))))))

(provide 'egg-key)