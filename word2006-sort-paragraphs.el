(defun word2006-sort-paragraphs()
"Sorts paragraphs of the opened document.xml file of an unzipped or explored .docx MS Word 2006 document.  \
A paragraph is defined as a group of adjacent lines delimited by a blank line. Requires nxml-mode"
(interactive
 (let 
     ((buff2 (generate-new-buffer-name "temporary"))
      (buff4 (current-buffer))
      (buff4_min (point-min))
      (buff4_max (point-max))
      buff1
      buff3
      plast
      pstart
      pend
      rstart
      rend
      fstart
      fend
      footer_start
      footer_end
      header_start
      header_end
      )
 (require 'nxml-mode)
 (goto-char (point-min))
 (if (search-forward "wne=\"http://schemas.microsoft.com/office/word/2006/wordml\"") () ())  ; error if wrong name space
 (if (string= (buffer-name) "document.copy.xml")               ; work with copy of xml file so we can bf-pretty-print
     (setq buff1 (buffer-name))
   (progn
     (setq buff1 (generate-new-buffer-name "document.copy.xml"))
     (get-buffer-create buff1)
     (set-buffer buff1)
     (insert-buffer-substring buff4 buff4_min buff4_max)
     (kill-buffer buff4)
     (set-buffer buff1)
     (goto-char (point-min))
     (nxml-mode)
     (while (search-forward-regexp "\>[ \\t]*\<" nil t)                 ; bf-pretty-print-xml-region 
       (backward-char) (insert "\n"))                                   ;
     (indent-region (point-min) (point-max)))                           ;   
   (setq buff3 (generate-new-buffer-name "document.xml"))
   (get-buffer-create buff2)
   (get-buffer-create buff3)
   (set-buffer buff1)
   (goto-char (point-min))
   (search-forward "<w:p ")
   (backward-char 5)
   (setq rstart (point))
   (setq pstart (point))
   (setq plast (point))
   (setq header_end (point))
   (goto-char (point-max))
   (setq footer_end (point))
   (setq buff1_max (point))
   (search-backward "</w:p>")
   (forward-char 6)
   (setq rend (point))
   (setq footer_start (point))
   (goto-char (point-min))
   (setq header_start (point))
   (while (and (set-buffer buff1)
	       (< (point) (point-max))
	       (search-forward "<w:p " (point-max) t))
     (progn
       (backward-char 5)
       (setq pstart (point))
       (if (search-forward "</w:p>" buff1_max t) () (search-forward "/>" buff1_max t))           ; case one line <w:p .../>
       (setq pend (point))
       (goto-char pstart)                                         ; find all the texts between <w:p ...></w:p> and group w:p with delimiters that dont contain text
       (re-search-forward "<w:t>\\(.*\\)</w:t>" pend t)
       (if (string= (match-string 1) nil) 
	   (progn   (set-buffer buff2)
		    (insert ":plast:")
		    (insert (number-to-string plast))
		    (insert ":pend:")
		    (insert (number-to-string pend))
		    (setq plast pend)
		    (newline)
		    (set-buffer buff1))
	 (progn
	   (goto-char pstart)
	   (while (re-search-forward "<w:t>\\(.*\\)</w:t>" pend t)
	     (progn
	       (setq texttocopy (match-string 1))
	       (set-buffer buff2)
	       (insert texttocopy)
	       (insert " ")
	       (set-buffer buff1)))))
       (goto-char pend)
       (backward-char 1))
     
     )
   (progn 
     (set-buffer buff2)                   ; last entry
     (insert ":plast:")
     (insert (number-to-string plast))
     (insert ":pend:")
     (insert (number-to-string rend))
     (setq plast pend)
     (newline)
     (sort-lines nil (point-min) (point-max))
     (goto-char (point-min))
     (set-buffer buff3)
     (insert-buffer-substring buff1 header_start header_end)
     (set-buffer buff2)
     (while (and (set-buffer buff2)
		 (re-search-forward ":plast:\\([[:digit:]]*\\):pend:\\([[:digit:]]*\\)" (point-max) t))
       (progn
	 (set-buffer buff2)
	 (setq fstart (string-to-number (match-string 1)))
	 (setq fend (string-to-number (match-string 2)))
	 (set-buffer buff3)
	 (insert-buffer-substring buff1 fstart fend)))
     (set-buffer buff3)
     (insert-buffer-substring buff1 footer_start footer_end)
     (goto-char (point-min))
     (while (search-forward-regexp "\n[[:blank:]]*" nil t)                ; un-bf-pretty-print region
       (replace-match ""))                                                ;
     (set-buffer buff2)
     (setq set-buffer-modified-p nil)
     (kill-buffer)
     (set-buffer buff1)
     (setq set-buffer-modified-p nil)
     (kill-buffer)
     (set-buffer buff3)
     (setq set-buffer-modified-p nil)
     (switch-to-buffer buff3))))
))