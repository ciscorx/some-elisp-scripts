;; tweets-chain.el            -*- lexical-binding: t; -*-          
(defun ciscorx/split-string-on-word-boundaries-every (string chars)
  "Split STRING into substrings of length CHARS characters, while trying not to split up any words.
But, if it reaches a line that cannot be split up, without breaking up a word, it just accepts the line of CHARS characters as a whole.
This returns a list of strings."
  (cond ((string-empty-p string)
         nil)
        ((< (length string)
            chars)
         (list string))
	((string= (substring string chars chars) " ")
	 (cons (substring string 0 chars)
               (ciscorx/split-string-on-word-boundaries-every (substring string chars)
                                   chars)))
	(t (let ((lastspace_pos chars))
	     (while (and
		     (> lastspace_pos 0)
		     (not (string= " " (substring string (1- lastspace_pos) lastspace_pos))))
	       (setq lastspace_pos (1- lastspace_pos))
	       )
	     (if (> lastspace_pos 0)
		 (cons (substring string 0  lastspace_pos)
		       (ciscorx/split-string-on-word-boundaries-every (substring string lastspace_pos)
							     chars))
					; else
		 (cons (substring string 0  chars)
		       (ciscorx/split-string-on-word-boundaries-every (substring string chars)
							     chars))
		 )
	     ))
	)
  )
  


(defun ciscorx/split-string-on-word-boundaries-every_w_trailing_idx (string chars &optional recurse_level recurse_max)
  "Split STRING into substrings of length CHARS characters, while trying not to split up any words.
But, if it reaches a line that cannot be split up, without breaking up a word, it just accepts the line of CHARS characters as a whole.
This returns a list of strings.  Dont pass the RECURSE_LEVEL nor RECURSE_MAX parameters as those are only used internally and will cause a malfunction if supplied.  Required: ciscorx/split-string-on-word-boundaries-every "
  (if (not (and recurse_level recurse_max))
      (progn
	(setq recurse_max (length (ciscorx/split-string-on-word-boundaries-every string (- chars 6))))
	(if (> recurse_max 9)
	    (progn
	      (setq recurse_max (length (ciscorx/split-string-on-word-boundaries-every string (- chars 8))))
	      (ciscorx/split-string-on-word-boundaries-every_w_trailing_idx string (- chars 8) 1 recurse_max)
	      )
					; else
	  (ciscorx/split-string-on-word-boundaries-every_w_trailing_idx string (- chars 6) 1 recurse_max) 
	  )
	)
					; else
      
    (cond ((string-empty-p string)
           nil)
          ((< (length string)
              chars)
           (list (concat string " [" (number-to-string recurse_level) "/" (number-to-string recurse_max) "]")))
	  ((string= (substring string chars chars) " ")
	   (cons (concat (substring string 0 chars) " [" (number-to-string recurse_level) "/" (number-to-string recurse_max) "]")
		 (ciscorx/split-string-on-word-boundaries-every_w_trailing_idx (substring string chars)
									      chars (1+ recurse_level) recurse_max)))
	  (t (let ((lastspace_pos chars))
	       (while (and
		       (> lastspace_pos 0)
		       (not (string= " " (substring string (1- lastspace_pos) lastspace_pos))))
		 (setq lastspace_pos (1- lastspace_pos))
		 )
	       (if (> lastspace_pos 0)
		   (cons (concat (substring string 0  lastspace_pos) " [" (number-to-string recurse_level) "/" (number-to-string recurse_max) "]")
			 (ciscorx/split-string-on-word-boundaries-every_w_trailing_idx (substring string lastspace_pos) chars (1+ recurse_level) recurse_max
										      ))
					; else
		 (cons (concat (substring string 0  chars) " [" (number-to-string recurse_level) "/" (number-to-string recurse_max) "]")
		       (ciscorx/split-string-on-word-boundaries-every_w_trailing_idx (substring string chars) chars (1+ recurse_level) recurse_max
										    ))
		 )
	       ))
	  )
    )
  )
  



(defun ciscorx/tweets-chain ( begin end )
  "Split region up into 280 character individual tweets, without splitting a line in the middle of any words, outputting the chain of tweets after the active region, along with a trailing index for each tweet, indicating the order of tweets.  After outputting its report, this function can be executed again following any additional edits made to the original region, overwriting the previously outputted report.  Required functions: ciscorx/split-string-on-word-boundaries-every_w_trailing_idx ciscorx/split-string-on-word-boundaries-every"
  (interactive "r")
  (let* ((text (buffer-substring-no-properties begin end))
	 output)
    (setq output (ciscorx/split-string-on-word-boundaries-every_w_trailing_idx text 280))
    
    (when (re-search-forward "[ \t\r\n\v\f]*." nil t)
      (forward-char -1)
      )
    (if (looking-at "_TWEETS CHAIN_ \\*BEGIN\\*")
      (let (display_begin
	    display_end)
	(next-line 1)
	(setq display_begin (point))
	(if (re-search-forward "_TWEETS CHAIN_ \\*END\\*" nil t)
	    (progn
	      (previous-line 1)
	      (setq display_end (point))
	      (delete-region display_begin display_end)
	      (mapc '(lambda (x) (insert x) (insert "\n\n")) output)
	      )
	  )
	)
					; else
      (goto-char end)
      (insert "\n\n")
      (insert "_TWEETS CHAIN_ *BEGIN*") (insert "\n\n")
	
      (mapc '(lambda (x) (insert x) (insert "\n\n")) output)
	
	
      (insert "_TWEETS CHAIN_ *END*") (insert "\n\n")
      )
    )
  )

