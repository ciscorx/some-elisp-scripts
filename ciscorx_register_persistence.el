
(defun ciscorx/load-jmp-registers-from-file ( &optional incl-buffers-dirs )
  "A prefix argument passed interactively will cause this
function to recognize the current buffers directory, and all its
recursed subdirectories, in addition to all the directories
contained in load-path, as locations where point-to-register can
point.  All register types except frame, window, and bookmarks
are restored: point marker, number, rectangle, file-names and
text, but text properties are not preserved, and kmacros,
although the kmacro key binding are switched from C-x C-r
REGISTER to C-x C-k REGISTER.  In addition, the kmacros become
listed in the minibuffer and the *Messages* buffer when the
list-registers function is called."
  (interactive "P")
  (let (reg  (i 0) func_name original_buffer tmp_buffer tmpbuffer jmp_buffer  saved_point saved_obj jump_pos filename numval load-path-alt str str_pos_start str_pos_end sexp_pos_start sexp_pos_end)
    
    (save-excursion
      (save-window-excursion
	(setq original_buffer (current-buffer))
	(if (and
	     (boundp 'incl-buffers-dirs)
	     incl-buffers-dirs)
	    (setq load-path-alt (append (ciscorx/directory-dirs-recursive "." 3) load-path))  ;; maxdepth 3
	  (setq load-path-alt load-path)
	  )
	
	(find-file "~/.emacs.d/my-jmp-registers.el")
	(setq jmp_buffer (current-buffer))

	;; load point markers
	(goto-char (point-min))
	(while (re-search-forward "(\\([[:digit:]]+\\) \\. #<marker at \\([[:digit:]]+\\) in \\([a-zA-Z0-9\-\.]+\\)>)" nil t)
	  (setq reg (string-to-number (match-string-no-properties 1))
		jump_pos (string-to-number (match-string-no-properties 2 ))
		filename (match-string-no-properties 3 ))
	  
	  (save-excursion
	    (save-window-excursion
	      (find-file (ciscorx/locate-file-among-load-path-dirs filename load-path-alt))
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

	
	;; load single text line stored as kmacro register
	(goto-char (point-min))
	(while (re-search-forward "(\\([[:digit:]]+\\) \\. \\[registerv \"" nil t)
	  (setq reg (string-to-number (match-string-no-properties 1)))
	  (setq str_pos_start (match-end 0))
	  (save-excursion 
	    (backward-char)
	    (forward-sexp)
	    (setq str_pos_end (1- (point)))
	    (copy-to-register reg str_pos_start str_pos_end)
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
	      
	;; load kbd-macros: Unfortunately, I couldnt figure out an easy way to load them into registers, so instead resorted to creating functions for them and enabling them through global set key C-x C-k reg, instead of C-x C-r reg.  And, as such, unfortunately, they dont show up on list-registers
	(goto-char (point-min))
	(while (re-search-forward "(\\([[:digit:]]+\\) \\. \\[registerv \\[" nil t)
	  (setq reg (string-to-number (match-string-no-properties 1)))
	  (setq str_pos_start (match-end 0))
	  (when (not (looking-at-p "\\["))  ;; skip frameset configuration entries
	    (save-excursion
	      (backward-char)
	      (setq saved-point (point))
	      (forward-sexp)
	      (setq str_pos_end  (point))
	      (setq str (buffer-substring-no-properties saved-point str_pos_end))
	      (setq func_name (format "setq-from-jmp-registers%s" (number-to-string reg)))
	      (setq str (format "(fset '%s %s )" func_name str))
	      (setq saved_obj (read str))
	      (eval saved_obj)
	      (setq str (format "(global-set-key (kbd \"C-x C-k %c\") '%s)" reg func_name))
	      (setq saved_obj (read str))
	      (eval saved_obj)
	      
	      
	      (setq i (1+ i))
	      )
	    )
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

(defun ciscorx/list-jmp-register-kmacros ()
  "In addition, this function is automatically called after the
list-registers function is called, to list the kmacros, that were
loaded from ciscorx/load-jmp-registers-from-file, in the
minibuffer and the *Messages* buffer"
  (interactive)
  
  (let* (testf testfn lines (i 0) (func_name_prefix "setq-from-jmp-registers") (func_name_width (string-width func_name_prefix))) 
    (setq testf (loop for x being the symbols
      if (and (symbol-function x) (> (string-width (symbol-name x)) func_name_width) (string= (substring (symbol-name x) 0 func_name_width) func_name_prefix))
      collect (symbol-name x)))
    
    (setq testfn (loop for x being the symbols
      if (and (symbol-function x) (> (string-width (symbol-name x)) func_name_width) (string= (substring (symbol-name x) 0 func_name_width) func_name_prefix))
      collect (symbol-function x)))
    
    (while (< i (length testf))
      (setq lines  (concat lines  (format "C-x C-k %c = %s" (string-to-number (substring (nth i testf) func_name_width)) (key-description (read (prin1-to-string (nth i testfn))) )) "\n"))
      
      (setq i (1+ i))
      )
    (message lines)
    )
  )
(add-function :after (symbol-function 'list-registers) #'ciscorx/list-jmp-register-kmacros)


(defun ciscorx/write-jmp-registers-to-file ()
  "In addition to writing the register file, an incremental diff is saved via 7z-revisions.el if its installed."
  (interactive)
  (with-temp-buffer
    (prin1 register-alist (current-buffer))
    (ciscorx/write-jmp-registers-to-file_append_kmacros)
    (write-file "~/.emacs.d/my-jmp-registers.el"))
  
  (when (boundp '7z-revisions-mode)
    (find-file "~/.emacs.d/my-jmp-registers.el")
    (7z-revisions-mode 1)
    (set-buffer-modified-p t)
    (save-buffer)
    (kill-buffer)
    )
  )

(defun ciscorx/write-jmp-registers-to-file_append_kmacros ()
  "Inserts any kmacros that were loaded via ciscorx/load-jmp-registers back into the ~/.emacs.d/my-jmp-registers.el file.  This function must only be called from ciscorx/write-jmp-registers-to-file."
  
  (when (looking-back ")")
    (backward-delete-char-untabify 1))
    
  (let* (testf testfn (i 0) (func_name_prefix "setq-from-jmp-registers") (func_name_width (string-width func_name_prefix))) 
    (setq testf (loop for x being the symbols
      if (and (symbol-function x) (> (string-width (symbol-name x)) func_name_width) (string= (substring (symbol-name x) 0 func_name_width) func_name_prefix))
      collect (symbol-name x)))
    
    (setq testfn (loop for x being the symbols
      if (and (symbol-function x) (> (string-width (symbol-name x)) func_name_width) (string= (substring (symbol-name x) 0 func_name_width) func_name_prefix))
      collect (symbol-function x)))
    
    (while (< i (length testf))
      (insert (concat "( " (substring (nth i testf) func_name_width )) " . [registerv " (prin1-to-string (nth i testfn)) " ])" )
      (newline)
      (setq i (1+ i))
      )
    )
  (insert ")")
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

(defun ciscorx/locate-file-among-load-path-dirs ( filename &optional load-path-alt)
  (let ( (file_fullpath nil) tmpfilename load-path-to-use)
    (if (and
	 (boundp 'load-path-alt)
	 load-path-alt)
	(setq load-path-to-use load-path-alt)
      (setq load-path-to-use load-path)
      )
	
    (mapc
     #'(lambda (x)
	 (setq tmpfilename (concat x "/" filename))
	 (when (file-exists-p tmpfilename)
	   (setq file_fullpath tmpfilename))
	 )
     load-path-to-use)
    file_fullpath
    )
  )

