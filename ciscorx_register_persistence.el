
(defun ciscorx/load-jmp-registers-from-file ( &optional incl-buffers-dirs )
  "A prefix argument passed interactively will cause this
function to recognize the current buffers directory, and all its
recursed subdirectories, in addition to all the directories
contained in load-path, as locations where point-to-register can
point.  All register types except frame, window, and bookmarks
are restored: point marker, number, rectangle, file-names and
text, but text properties are not preserved, and kmacros."
  (interactive "P")
  (let (reg  (i 0) func_name original_buffer tmp_buffer tmpbuffer jmp_buffer  saved_point saved_obj jump_pos filename numval load-path-alt dir str str_pos_start str_pos_end sexp_pos_start sexp_pos_end)
    
    (save-excursion
      (save-window-excursion
	(setq original_buffer (current-buffer))
	(if (and
	     (boundp 'incl-buffers-dirs)
	     incl-buffers-dirs)
	    (setq load-path-alt (append (ciscorx/directory-dirs-recursive "." 4) load-path))  ;; maxdepth 4
	  (setq load-path-alt load-path)
	  )
	
	(find-file "~/.emacs.d/my-jmp-registers.el")
	(setq jmp_buffer (current-buffer))

	;; load point markers
	(goto-char (point-min))
	(while (re-search-forward "(\\([[:digit:]]+\\) \\. #<marker at \\([[:digit:]]+\\) in \\([a-zA-Z0-9\-\.]+\\)<?\\([a-zA-Z0-9\-\.]*\\)>?>)" nil t)
	  (setq reg (string-to-number (match-string-no-properties 1))
		jump_pos (string-to-number (match-string-no-properties 2 ))
		filename (match-string-no-properties 3 )
		dir      (match-string-no-properties 4 ))
	  
	  (save-excursion
	    (save-window-excursion
	      (find-file (ciscorx/locate-file-among-load-path-dirs filename load-path-alt dir))
	      (goto-char jump_pos)
	      (point-to-register reg)
	      )
	    )
	  )

	;; load file-names
	(goto-char (point-min))
	(while (re-search-forward "(\\([[:digit:]]+\\) file \\. \"" nil t)
	  (setq reg (string-to-number (match-string-no-properties 1)))
	  (setq str_pos_start (match-end 0))
	  (save-excursion
	    (forward-char -1)
	    (forward-sexp)
	    (setq str_pos_end (1- (point)))
	    (setq str (buffer-substring-no-properties str_pos_start str_pos_end))
	    (set-register reg `(file . ,str)) 
	    )
	  )  ; while
	
	;; load numbers
	(goto-char (point-min))
	(while (re-search-forward "(\\([[:digit:]]+\\) \\. \\([[:digit:]]+\\))" nil t)
	  (setq reg (string-to-number (match-string-no-properties 1))
		numval (string-to-number (match-string-no-properties 2))
	  )
	  (number-to-register numval reg)
	  )  ; while

	;; load text
	(goto-char (point-min))
	(while (re-search-forward "(\\([[:digit:]]+\\) \\. #(\"" nil t)
	  (setq reg (string-to-number (match-string-no-properties 1)))
	  (setq str_pos_start (match-end 0))
	  (save-excursion
	    (forward-char -1)
	    (forward-sexp)
	    (setq str_pos_end (1- (point)))
	    (copy-to-register reg str_pos_start str_pos_end)
	    )
	  )

	
	;; load  kmacros
	(goto-char (point-min))
	(while (re-search-forward "(\\([[:digit:]]+\\) \\. \\[registerv " nil t)
	  (when (and
		 (not (looking-at-p "\\[\\["))    ;; but, skip frame configuration entries
		 (not (looking-at-p "\\[#<buffer"))  ;; and skip undo state data
		 )
	    (setq reg (string-to-number (match-string-no-properties 1)))
	    (save-excursion 
	      (backward-char 11)
	      (setq str_pos_start (point))
	      (forward-sexp)
	      (setq str_pos_end  (point))
	      (set-register reg (read (buffer-substring-no-properties str_pos_start str_pos_end)))
	      )
	    )
	)
	
	;; load rectangles
	(goto-char (point-min))
	(while (re-search-forward "(\\([[:digit:]]+\\) #(\"" nil t)
	  (setq reg (string-to-number (match-string-no-properties 1)))
	  (setq sexp_pos_start (match-beginning 0))
	  (goto-char sexp_pos_start)
	  (forward-sexp)
	  (setq sexp_pos_end (point))
	  (goto-char sexp_pos_start)
	  (save-match-data
	    
	    (setq tmpbuffer (generate-new-buffer-name (concat "newrectbuffer")))
	    (setq tmp_buffer (get-buffer-create tmpbuffer))
	    (set-buffer jmp_buffer)
	    (while (re-search-forward " #(\"" sexp_pos_end t)
	      (setq saved_point (point))
	      (backward-char)
	      (forward-sexp)
	      (setq str (buffer-substring-no-properties saved_point (1- (point))))
	      (set-buffer tmp_buffer)
	      (insert str)
	      (newline)
	      (set-buffer jmp_buffer)
	      )
	    (set-buffer tmp_buffer)
	    (set-buffer-modified-p nil)
	    (copy-rectangle-to-register reg (point-min) (1- (point-max)))
	    (set-buffer original_buffer)
	    
	    
	    (kill-buffer tmp_buffer)
	    )
	  
	  
	  (goto-char sexp_pos_end) 
	)
	      

	
	(kill-buffer jmp_buffer)
	)
      
      )
    ) ; let
  )


(defun ciscorx/my-jmp-registers-file ()
  "Opens the my-jmp-register.el file located in ~/.emacs.d/"
  (interactive)
  (find-file "~/.emacs.d/my-jmp-registers.el")
  )


(defun ciscorx/write-jmp-registers-to-file ()
  "In addition to writing the register file, an incremental diff is saved via 7z-revisions.el if its installed."
  (interactive)
  (with-temp-buffer
    (prin1 register-alist (current-buffer))
    (write-file "~/.emacs.d/my-jmp-registers.el"))
  
  (when (boundp '7z-revisions-mode)
    (find-file "~/.emacs.d/my-jmp-registers.el")
    (7z-revisions-mode 1)
    (set-buffer-modified-p t)
    (save-buffer)
    (kill-buffer)
    )
  )


(defun ciscorx/directory-dirs-recursive ( dir maxdepth )
  "This function returns a full-path list of all subdirectories,
including the directory that was passed as an argument, up to a
maximum depth.  Passing a maxdepth of -1 means infinite
subdirectory recursion."
  (let (retval dir_listing subdir_name)
    (when (string= dir ".")
      (setq dir (substring (pwd) 10 -1)))
    (when (string= (substring dir -1 nil) "/")
      (setq dir (substring dir 0 -1 )))
    (setq retval (list dir))
    (when (not (= maxdepth 0))
      (setq dir_listing (directory-files dir))
      (mapc #'(lambda (x)
		(setq subdir_name (concat dir "/" x))
		(when (and
		       (not (string= x "."))
		       (not (string= x ".."))
		       (file-directory-p subdir_name)
		       (file-accessible-directory-p subdir_name)
		       )
		  (setq retval (append retval (ciscorx/directory-dirs-recursive subdir_name (1- maxdepth))))
		  )
		)
	    (directory-files dir))
      )
    retval
    )
  )


(defun ciscorx/last-subdir-in-path (directory-path)
  "Return the leaf sub-directory name in dir"
  (let (len (subdirs (split-string directory-path "/")))
    (setq len (length subdirs))
    (if (string= (nth (1- len) subdirs) "")
	 (nth (- len 2) subdirs)
      (nth (1- len) subdirs)
      )
    )
  )


(defun ciscorx/locate-file-among-load-path-dirs ( filename &optional load-path-alt in-subdir)
  "Go through all the directories in load-path and see if any of them contain the file filename.  If the load-path-alt argument is given, then include the directory of load-path-alt along with load-path, for where to search.  If in-subdir argument is present, then the filename must be in the specific directory leaf specified by in-subdir.  in-subdir must not be a full-directory-path.  It must only be one leaf, without any parents." 
  (let ( (file_fullpath nil) tmpfilename load-path-to-use found)
    (if (and
	 (boundp 'load-path-alt)
	 load-path-alt)
	(setq load-path-to-use load-path-alt)
      (setq load-path-to-use load-path)
      )
    (if (and
	 in-subdir
	 (not (string= in-subdir "")))
	(mapc
	 #'(lambda (x)
	     (setq tmpfilename (concat x "/" filename))
	     (when (and
		    (file-exists-p tmpfilename)
		    (string= (ciscorx/last-subdir-in-path x) in-subdir)
		    (not found)
		    ) 
	       (setq file_fullpath tmpfilename)
	       (setq found t)
	       ) 
	     )
	 load-path-to-use)
					; else
      (mapc
       #'(lambda (x)
	   (setq tmpfilename (concat x "/" filename))
	   (when (and
		  (not found)
		  (file-exists-p tmpfilename))
	     (setq file_fullpath tmpfilename)
	     (setq found t)
	     )
	   )
       load-path-to-use)
    )
    file_fullpath
    )
  )


