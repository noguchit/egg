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

(defun egg--inline-diff-mk-patch (file rev line del-num add-num add-text)
  (let ((git-name (egg-file-git-name file))
	(dir (egg-work-tree-dir))
	(pre-lines 0)
	(post-lines 0)
	(del-num (or del-num 0))
	(add-num (or add-num 0))
	pre post beg end text)

    (with-temp-buffer
      (setq default-directory dir)
      (egg-git-ok t "--no-pager" "show" (concat rev ":" git-name))
      (goto-char (egg-line-2-pos line))
      (while (and (not (bobp)) (< pre-lines 3))
	(forward-line -1)
	(setq pre-lines (1+ pre-lines)))
      (setq beg (point))
      (goto-char (egg-line-2-pos line del-num))
      (when add-text (insert add-text))
      (while (and (not (eobp)) (< post-lines 3))
	(forward-line 1)
	(setq post-lines (1+ post-lines)))
      (setq end (point))
      (setq text (buffer-substring-no-properties beg end)))

    (with-temp-buffer
      (insert (format "diff a/%s b/%s\n" git-name git-name)
	      (format "--- a/%s\n+++ b/%s\n" git-name git-name)
	      (format "@@ -%d,%d +%d,%d @@\n"
		      (- line pre-lines)
		      (+ pre-lines del-num post-lines)
		      (- line pre-lines)
		      (+ pre-lines add-num post-lines)))
      (setq beg (point))
      (insert text)
      (setq end (point))
      (goto-char beg)
      (dotimes (i pre-lines)
	(insert " ")
	(forward-line 1))
      (dotimes (i del-num)
	(insert "-")
	(forward-line 1))
      (dotimes (i add-num)
	(insert "+")
	(forward-line 1))
      (dotimes (i post-lines)
	(insert " ")
	(forward-line 1))
      (buffer-string))))

(defun egg--inline-diff-block2patch (rev del-pos add-pos)
  (let* ((file (buffer-file-name))
	(del-nav (and del-pos (get-text-property del-pos :navigation)))
	(add-nav (and add-pos (get-text-property add-pos :navigation)))
	(del-line (and del-pos (get-text-property del-pos :old-line)))
	(add-line (and add-pos (get-text-property add-pos :orig-line)))
	(del-num (and del-pos (get-text-property del-pos :num)))
	(add-num (and add-pos (get-text-property add-pos :num)))
	add-text add-text-info patch)

    (unless (or del-line add-line)
      (error "No info found to build patch: positions %d/%d" del-pos add-pos))

    (unless (or (null del-line) (null add-line)
		(= (+ del-line del-num) add-line))
      (error "Cannot merge separate diff blocks: -%d and +%d" del-line add-line))
    
    (when add-pos
      (setq add-text-info (assq add-nav (plist-get egg-inline-diff-info :new-text)))
      (setq add-text (buffer-substring-no-properties (nth 1 add-text-info)
						     (nth 2 add-text-info))))
    (setq patch
	  (egg--inline-diff-mk-patch file rev (or del-line add-line)
				     del-num add-num add-text))
    (with-current-buffer "*foo*"
      (erase-buffer)
      (insert patch))
    patch))


(defvar egg-inline-diff-info nil)

(defun egg-inline-diff-compute-info (file-name)
  (let (hunk beg ranges all text-beg block-lines prev-line-no
	     block-start line-no old-start old-line-no
	     current-prefix start-c no-diff) 
    (with-temp-buffer
      (setq no-diff
	    (egg-git-ok t "--no-pager" "diff" "--exit-code" 
			;;"--function-context"
			;;"--unified=5"
			"--" file-name))
      (unless no-diff
	(goto-char (point-max))
	(save-match-data
	  (while (re-search-backward "^@@@? .+ @@@?" nil t)
	    (setq beg (match-beginning 0))
	    (setq hunk (mapcar #'string-to-number
			       (split-string (match-string-no-properties 0) "[-+,@ ]+" t)))
	    (setq line-no (1- (nth 2 hunk)))
	    (setq old-line-no (1- (nth 0 hunk)))
	    (setq old-start nil)
	    (while (not (eobp))
	      (forward-line 1)
	      (setq start-c (char-after (point)))
	      (unless (eobp) (delete-char 1))

	      ;; keep the prev-line-no because
	      ;; line-no is conditionally incremented
	      (setq prev-line-no line-no)
	      (when (memq start-c '(?- ? )) (setq old-line-no (1+ old-line-no)))
	      (when (memq start-c '(?+ ? )) (setq line-no (1+ line-no)))

	      (if (eq current-prefix start-c)
		  (setq block-lines (1+ block-lines))

		;; switching
		
		;; building info
		(cond ((eq current-prefix ? ) nil)
		      ((eq current-prefix ?+)
		       (push (list :add block-start block-lines old-start) ranges))
		      ((eq current-prefix ?-)
		       (push (list :del block-start block-lines old-start
				   (buffer-substring-no-properties text-beg (point)))
			     ranges)))

		(cond ((eq start-c ? )
		       ;; restart position in the original file
		       (setq old-start nil))
		      ((eq start-c ?-)
		       ;; keep removed text because it's not in the file
		       (setq text-beg (point))
		       ;; del-only or combined hunk, this is the starting point
		       ;; of the changes in the original file
		       (setq old-start old-line-no))
		      ((eq start-c ?+)
		       ;; the strting point in the original file is either:
		       ;;  - the beginning of the add-only lines
		       ;;  - or the end of the del-lines
		       ;; +1 because old-line-no doesn't advance this time
		       (setq old-start (1+ old-line-no))))

		;; start new block
		(setq block-lines 1)
		(setq current-prefix start-c)
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

(defun egg--inline-diff-mk-boundaries-visible (beg end)
  (when (and (> beg (point-min)) (eq (char-before beg) ?\n))
    (put-text-property (1- beg) beg 'invisible nil))
  (when (eq (char-before end) ?\n)
    (put-text-property (1- end) end 'invisible nil)))


(defun egg-do-buffer-inline-diff (ranges)
  (let ((read-only-state buffer-read-only)
	(inhibit-read-only t)
	beg end
	type line num text old-line
	text-list text-list ov-list ov)
    (widen)
    (setq buffer-invisibility-spec nil)
    (add-text-properties (point-min) (point-max)
			 (list :navigation 0
			       'invisible 0
			       'keymap egg-section-map))
    (dolist (range ranges)
      (setq type (nth 0 range))
      (setq line (nth 1 range))
      (setq num (nth 2 range))
      (setq old-line (nth 3 range))
      (setq text (nth 4 range))
      (setq beg (egg-line-2-pos line))
      (setq end (egg-line-2-pos line num))
      (cond ((eq type :same) 
	     (add-text-properties beg end
				  (list :navigation line
					'invisible nil
					'keymap egg-section-map))
	     (egg--inline-diff-mk-boundaries-visible beg end))
	    ((eq type :add)
	     (add-text-properties beg end
				  (list :navigation line
					'invisible nil
					:orig-line old-line
					:num num))
	     (egg--inline-diff-mk-boundaries-visible beg end)
	     (setq ov (make-overlay beg end nil t nil))
	     (overlay-put ov 'face 'egg-add-bg)
	     (overlay-put ov 'evaporate t)
	     (push ov ov-list)
	     (setq beg (copy-marker beg t))
	     (setq end (copy-marker end t))
	     (push (list line beg end) text-list))
	    ((eq type :del)
	     (goto-char beg)
	     (insert text)
	     (setq end (point))
	     (add-text-properties beg end 
				  (list :navigation (- line)
					'invisible nil
					:old-line old-line
					:num num))
	     (egg--inline-diff-mk-boundaries-visible beg end)
	     (setq ov (make-overlay beg end nil t nil))
	     (overlay-put ov 'face 'egg-del-bg)
	     (overlay-put ov 'evaporate t)
	     (push ov ov-list)
	     (setq beg (copy-marker beg t))
	     (setq end (copy-marker end t))
	     (push (list (- line) beg end) text-list))))
    (setq buffer-invisibility-spec (list '(0 . t)))
    (set (make-local-variable 'egg-inline-diff-info)
	 (list :read-only read-only-state
	       :overlays ov-list 
	       :text-positions text-list))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)))

(defun egg-undo-buffer-inline-diff ()
  (let ((overlays (plist-get egg-inline-diff-info :overlays))
	(text-positions (plist-get egg-inline-diff-info :text-positions))
	(inhibit-read-only t))
    (widen)
    (dolist (ov overlays) (delete-overlay ov))
    (remove-text-properties (point-min) (point-max)
			    '(:navigation nil invisible nil :num nil
					  :old-line nil :orig-line nil))
    (dolist (pos text-positions)
      (when (< (nth 0 pos) 0)
	(delete-region (nth 1 pos) (nth 2 pos)))
      (set-marker (nth 1 pos) nil)
      (set-marker (nth 2 pos) nil))
    (setq buffer-read-only (plist-get egg-inline-diff-info :read-only))
    (set-buffer-modified-p nil)
    (setq egg-inline-diff-info nil)))

(defun egg-buffer-toggle-inline-diff ()
  (interactive)
  (if egg-inline-diff-info
      (egg-undo-buffer-inline-diff)
    (let ((ranges (egg-inline-diff-compute-info (buffer-file-name))))
      (if ranges
	  (egg-do-buffer-inline-diff ranges)
	(message "no differences in %s" (buffer-file-name))))))

