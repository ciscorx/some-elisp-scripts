
(defun ciscorx/cut-rect ( start end )
  "This is an easy way to kill-rectangle when there are no
trailing spaces.  First the maximum column number is determined
for the region, then that number of spaces is added to the last
line in the region before the rectangle is cut."
  (interactive "*r")
  (when (< end start)
    (setq tmp start)
    (setq start end)
    (setq end tmp))
  (let (max-col start_char col)
    (goto-char start)
    (beginning-of-line)
    (setq start_char (point))
    (setq max-col (ciscorx/max-col-region-quiet start end))
    (goto-char end)
    (end-of-line)
    (setq col (ciscorx/what-col-position))
    (if (<= col max-col)
	(ciscorx/insert-n-number-of-characters (- max-col col) " ")
      (backward-char (- col max-col))
      )
    (kill-rectangle start_char (point))
    )
  )
         


(defun ciscorx/paste-rect (start end)
  "This is an easy way to yank-rectangle.  First the maximum
column number is determined for the region, then that number of
spaces is added to the first line of the region before the
rectangle is yanked into place.  This function's useful in
tacking a list of foreign words next to their english translated
equivalents, after copying from google translate."
  (interactive "*r")
  (when (< end start)
    (setq tmp start)
    (setq start end)
    (setq end tmp))
  (let (max-col  col)
    (setq max-col (ciscorx/max-col-region-quiet start end))
    (goto-char start)
    (end-of-line)
    
    (setq col (ciscorx/what-col-position))
    (if (<= col max-col)
	(ciscorx/insert-n-number-of-characters (- max-col col) " ")
      )
    (insert " ")
    (yank-rectangle)
    )
  )


(defun ciscorx/max-col-region-quiet ( start end )
  "same as ciscorx/max-col-region but without messaging the results"
  (save-excursion
    (let ((max_col 0) col line_start line_end current_line tmp)
      (when (< end start)
	(setq tmp start)
	(setq start end)
	(setq end tmp))
      
      (goto-char start)
      (setq line_start (line-number-at-pos))
      (goto-char end)
      (setq line_end (line-number-at-pos))
      (setq tmp line_start)
      (goto-char start)
      (while (<= tmp line_end)
	(end-of-line)
	(setq col (ciscorx/what-col-position))
	(when (> col max_col)
	  (setq max_col col)
	  )
	(forward-line) 
	(setq tmp (1+ tmp))
	)
      max_col
      )
    )
  )


(defun ciscorx/insert-n-number-of-characters ( n c )
  "There must already exist an emacs function that does this.
Where is it?"
  (let ((i 0))
    (while (< i n)
      (insert c)
      (setq i (1+ i))
      )
    )
  )
