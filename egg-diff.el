;;; egg-diff.el --- Emacs Got Git - Emacs interface to Git

;; Copyright (C) 2008  Linh Dang
;;
;; Author: Bogolisk <bogolisk@gmail.com>
;; Created: 02 Nov 2012
;; Keywords: git, version control, release management
;;
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

(require 'egg)

(defvar egg-inline-diff-info nil)

(defun egg-compute-file-diff (file-name)
  (let (hunk beg ranges all text-beg block-lines prev-line-no
	     block-start line-no old-start old-line-no
	     current-prefix start-c no-diff) 
    (with-temp-buffer
      (setq no-diff
	    (egg-git-ok t "--no-pager" "diff" "--exit-code" 
			"--function-context" "--unified=5" "--" file-name))
      (unless no-diff
	(goto-char (point-max))
	(save-match-data
	  (while (re-search-backward "^@@@? .+ @@@?" nil t)
	    (setq beg (match-beginning 0))
	    (setq hunk (mapcar #'string-to-number
			       (split-string (match-string-no-properties 0) "[-+,@ ]+" t)))
	    (setq line-no (1- (nth 2 hunk)))
	    (setq old-line-no (1- (nth 0 hunk)))
	    (while (not (eobp))
	      (forward-line 1)
	      (setq start-c (char-after (point)))
	      (unless (eobp) (delete-char 1))

	      (setq prev-line-no line-no)
	      (when (memq start-c '(?- ? )) (setq old-line-no (1+ old-line-no)))
	      (when (memq start-c '(?+ ? )) (setq line-no (1+ line-no)))

	      (if (eq current-prefix start-c)
		  (setq block-lines (1+ block-lines))

		;; keep removed text because it's not in the file
		(when (eq start-c ?-) (setq text-beg (point)))

		(cond ((eq current-prefix ? ) nil)
		      ((eq current-prefix ?+)
		       (push (list :add block-start block-lines) ranges))
		      ((eq current-prefix ?-)
		       (push (list :del block-start block-lines old-start
				   (buffer-substring-no-properties text-beg (point)))
			     ranges)))
		;; start new block
		(setq block-lines 1)
		(setq current-prefix start-c)
		(setq old-start old-line-no)
		;; in the case of del block, line-no would have stayed the same
		;; but we must move the block start ahead
		(setq block-start (1+ prev-line-no))))
	    ;; do full block
	    (push (list :same (nth 2 hunk) (nth 3 hunk)) ranges)
	    ;; done current ranges
	    (setq all (nconc all ranges))
	    (setq ranges nil)
	    (goto-char beg)
	    (delete-region beg (point-max))))))
    all))

(defun egg-line-2-pos (line &optional num)
  (save-excursion
    (setq num (or num 0))
    (goto-char (point-min))
    (forward-line (1- (+ line num)))
    (point)))


(defun egg-do-file-inline-diff ()
  (let ((read-only-state buffer-read-only)
	(inhibit-read-only t)
	(ranges (egg-compute-file-diff (buffer-file-name)))
	beg end
	type line num text old-line
	old-text-list ov-list ov)
    (if (null ranges)
	(message "no differences in %s" (buffer-file-name))
      (widen)
      (setq buffer-invisibility-spec nil)
      (put-text-property (point-min) (point-max) :navigation 0)
      (put-text-property (point-min) (point-max) 'invisible 0)
      (dolist (range ranges)
	(setq type (nth 0 range))
	(setq line (nth 1 range))
	(setq num (nth 2 range))
	(setq old-line (nth 3 range))
	(setq text (nth 4 range))
	(setq beg (egg-line-2-pos line))
	(setq end (egg-line-2-pos line num))
	(cond ((eq type :same) 
	       (when (and (> beg (point-min)) (eq (char-before beg) ?\n))
		 (setq beg (1- beg)))
	       (when (and (< end (point-max)) (eq (char-after end) ?\n))
		 (setq end (1+ end)))
	       (put-text-property beg end :navigation line)
	       (put-text-property beg end 'invisible line))
	      ((eq type :add)
	       (put-text-property beg end :navigation line)
	       (put-text-property beg end 'invisible line)
	       (put-text-property beg end :num num)
	       (setq ov (make-overlay beg end nil t nil))
	       (overlay-put ov 'face 'egg-add-bg)
	       (overlay-put ov 'evaporate t)
	       (push ov ov-list))
	      ((eq type :del)
	       (goto-char beg)
	       (insert text)
	       (put-text-property beg (point) :navigation (- line))
	       (put-text-property beg (point) 'invisible (- line))
	       (put-text-property beg (point) :old-line old-line)
	       (put-text-property beg (point) :num num)
	       (setq ov (make-overlay beg (point) nil t nil))
	       (overlay-put ov 'face 'egg-del-bg)
	       (overlay-put ov 'evaporate t)
	       (push ov ov-list)
	       (setq beg (copy-marker beg t))
	       (setq end (point-marker))
	       (push (list (- line) beg end) old-text-list))))
      (setq buffer-invisibility-spec (list '(0 . t)))
      (set (make-local-variable 'egg-inline-diff-info)
	   (list :read-only read-only-state
		 :overlays ov-list :old-text old-text-list))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t))))

(defun egg-undo-buffer-inline-diff ()
  (let ((overlays (plist-get egg-inline-diff-info :overlays))
	(old-texts (plist-get egg-inline-diff-info :old-text))
	(inhibit-read-only t))
    (widen)
    (dolist (ov overlays) (delete-overlay ov))
    (remove-text-properties (point-min) (point-max)
			    '(:navigation nil invisible nil :num nil :old-line nil))
    (dolist (old old-texts)
      (delete-region (nth 1 old) (nth 2 old))
      (set-marker (nth 1 old) nil)
      (set-marker (nth 2 old) nil))
    (setq buffer-read-only (plist-get egg-inline-diff-info :read-only))
    (set-buffer-modified-p nil)
    (setq egg-inline-diff-info nil)))

(defun egg-buffer-toggle-inline-diff ()
  (interactive)
  (if egg-inline-diff-info
      (egg-undo-buffer-inline-diff)
    (egg-do-file-inline-diff)))

