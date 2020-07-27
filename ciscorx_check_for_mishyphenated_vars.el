
(defun ciscorx/check-for-mishyphenated-vars ()
  "An easy mistake to make is to mistakenly hyphenate variables instead of underscoring them, or vice versa.  This function tries to find those mistakes in your code."
  (interactive)
  (let (buffer1 buffer2 buffer3)
    (setq buffer1 (ciscorx/display-all-symbols-in-current-buffer))
    ;;(setq buffer1 (list-all-variables))
    (setq buffer2 (ciscorx/show-variables-with-_))
    (setq buffer3 (ciscorx/find-duplicate-lines))
    (kill-buffer buffer1)
    (kill-buffer buffer2)
    (set-buffer buffer3)
    (goto-char (point-min))
    (if (eobp)
	(progn
	  (kill-buffer buffer3)
	  (message "No ambiguous hyphens/underscores detected.")
	  )	       
					; else
      (message "There exists some ambiguous hyphens/underscores!")
      )
    )
  )

(defun ciscorx/uniquify-all-lines-region (start end)
    "Find duplicate lines in region START to END keeping first occurrence."
    (interactive "*r")
    (save-excursion
      (let ((lines) (end (copy-marker end)))
        (goto-char start)
        (while (and (< (point) (marker-position end))
                    (not (eobp)))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (if (member line lines)
                (delete-region (point) (progn (forward-line 1) (point)))
              (push line lines)
              (forward-line 1)))))))

  
(defun ciscorx/uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (ciscorx/uniquify-all-lines-region (point-min) (point-max))
  )

(defun ciscorx/display-all-symbols-in-current-buffer ()
  "Displays and sorts all symbols, displaying them in a newly created buffer, returned as a buffer.  This function is called from ciscorx-check-for-mishypenated-vars"
  (interactive)
  (let     ((display-all-symbols-in-current-buffer_buffername (buffer-name (current-buffer)))
	    (display-all-symbols-in-current-buffer_lastbuffer (current-buffer))
	    display-all-symbols-in-current-buffer_tmpbuffer
	    found-one
	    )
    (save-excursion
      (setq display-all-symbols-in-current-buffer_buffername (buffer-name (current-buffer)))
      (setq display-all-symbols-in-current-buffer_lastbuffer (current-buffer))
      (setq display-all-symbols-in-current-buffer_tmpbuffer (generate-new-buffer-name (concat "display-all-symbols-in-current-buffer_of_" display-all-symbols-in-current-buffer_buffername)))
      (get-buffer-create display-all-symbols-in-current-buffer_tmpbuffer)
      (set-buffer display-all-symbols-in-current-buffer_lastbuffer)
      (beginning-of-buffer)
      (while (re-search-forward "[a-zA-Z0-9\-_]+" nil t)
	(setq found-one (match-string-no-properties 0))
	(set-buffer display-all-symbols-in-current-buffer_tmpbuffer)
	(insert found-one)
	(newline)
	(set-buffer display-all-symbols-in-current-buffer_lastbuffer)
	)
      )
    (switch-to-buffer display-all-symbols-in-current-buffer_tmpbuffer)
    (ciscorx/uniquify-all-lines-buffer)
    (sort-lines nil (point-min) (point-max))
    display-all-symbols-in-current-buffer_tmpbuffer
    )
  )

(defun ciscorx/show-variables-with-_ ()
  "This function is called from check-for-mishypenated-vars, and only shows lines that end in an underscore or a hyphen, returing the buffer to the calling function.   Actually it assumes that underscores and hyphens are the same and returns them both as underscores."
  (let ((show-variables-with-__buffername (buffer-name (current-buffer)))
	(show-variables-with-__lastbuffer (current-buffer))
	show-variables-with-__tmpbuffer
	found-one
	)
    (save-excursion
      (setq show-variables-with-__tmpbuffer (generate-new-buffer-name (concat "show-variables-with-__of_" show-variables-with-__buffername)))
      (get-buffer-create show-variables-with-__tmpbuffer)
      (set-buffer show-variables-with-__lastbuffer)

      (beginning-of-buffer)
      (while (re-search-forward "[a-zA-Z0-9\-_]+" nil t)
	(setq found-one (match-string-no-properties 0))
	(if (or
	     (string-match "_" found-one)
	     (string-match "\-" found-one))
	    (progn
	      (set-buffer show-variables-with-__tmpbuffer)
	      (insert found-one)
	      (newline)
	      (set-buffer show-variables-with-__lastbuffer)
	      )
	  )
	)
      )  ; save-excursion ends
    (switch-to-buffer show-variables-with-__tmpbuffer)
    (uniquify-all-lines-buffer)
    (sort-lines nil (point-min) (point-max))
    (beginning-of-buffer)
    (replace-regexp "\-" "_")
    show-variables-with-__tmpbuffer
    )
  )


(defun ciscorx/find-duplicate-lines ()
"Copy all duplicate lines in the current buffer to a new buffer, returning the new buffer to the calling function..
  This function only works if the current buffer has first been sorted."
  (interactive)
  (let ((find-duplicate-lines_buffername (buffer-name (current-buffer)))
    (find-duplicate-lines_lastbuffer (current-buffer))
    find-duplicate-lines_tmpbuffer
    found-one
    )
    (save-excursion
      (setq find-duplicate-lines_tmpbuffer (generate-new-buffer-name (concat "find-duplicate-lines_of_" find-duplicate-lines_buffername)))
      (get-buffer-create find-duplicate-lines_tmpbuffer)
      (set-buffer find-duplicate-lines_lastbuffer)
      (beginning-of-buffer)
      (while (re-search-forward "^\\(.*\\)\n\\(\\1\\)$" nil t)
  	(setq found-one (match-string-no-properties 1))
	(set-buffer find-duplicate-lines_tmpbuffer)
	(insert found-one)
	(newline)
	(set-buffer find-duplicate-lines_lastbuffer)
	)
      )
    (switch-to-buffer find-duplicate-lines_tmpbuffer)
    
    )
  )


