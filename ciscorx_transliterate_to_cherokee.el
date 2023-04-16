; ciscorx_transliterate_to_cherokee.el            -*- lexical-binding: t; -*-          
(defun ciscorx/transliterate_to_cherokee (begin end)
  "encode region to cherokee, outputing cherokee text after region"
  (interactive "r")
  (let* ((text (buffer-substring-no-properties begin end))
	 output to_syllabary words words_quantity word_idx
	 word_out char_idx wordparts_idx wordparts wordparts_qty
	 wordpart_out done_with_word ch ch2 ch3 syl)

    (setq to_syllabary '(("a" . "Ꭰ") ("e" . "Ꭱ") ("i" . "Ꭲ") ("o" . "Ꭳ") ("u" . "Ꭴ") ("v" . "Ꭵ")
			 ("g" .  (("a" . "Ꭶ" ) ("e" . "Ꭸ") ("i" . "Ꭹ") ("o" . "Ꭺ") ("u" . "Ꭻ") ("v" . "Ꭼ")))
			 ("k" .  (("a" . "Ꭷ")))
			 ("h" .  (("a" . "Ꭽ") ("e" . "Ꭾ") ("i" . "Ꭿ") ("o" . "Ꮀ") ("u" . "Ꮁ") ("v" . "Ꮂ") ("n" . (("a" . "Ꮏ")))))
			 
			 ("l" .  (("a" . "Ꮃ") ("e" . "Ꮄ") ("i" . "Ꮅ") ("o" . "Ꮆ") ("u" . "Ꮇ") ("v" . "Ꮈ")))
			 ("m" .  (("a" . "Ꮉ") ("e" . "Ꮊ") ("i" . "Ꮋ") ("o" . "Ꮌ") ("u" . "Ꮍ") ("v" . "Ᏽ")))
			 ("n" .  (("a" . (( "■" . "Ꮎ") ("h" . "Ꮐ"))) ("e" . "Ꮑ") ("i" . "Ꮒ") ("o" . "Ꮓ") ("u" . "Ꮔ") ("v" . "Ꮕ")))
			 ("q" .  (("u" . (( "a" . "Ꮖ") ("e" . "Ꮗ") ("i" . "Ꮘ") ("o" . "Ꮙ") ("u" . "Ꮚ") ("v" . "Ꮛ")))))
			 ("s" .  (("■" . "Ꮝ") ("a" . "Ꮜ") ("e" . "Ꮞ") ("i" . "Ꮟ") ("o" . "Ꮠ") ("u" . "Ꮡ") ("v" . "Ꮢ")))
			 ("d" .  (("a" . "Ꮣ") ("e" . "Ꮥ") ("i" . "Ꮧ") ("o" . "Ꮩ") ("u" . "Ꮪ") ("v" . "Ꮫ") ("l" . (("a" . "Ꮬ"))) ))
			 ("t" .  (("a" . "Ꮤ") ("e" . "Ꮦ") ("i" . "Ꮨ") ("o" . "") ("u" . "") ("v" . "") ("l" . (("a" . "Ꮭ"  ) ("e" . "Ꮮ") ("i" . "Ꮯ") ("o" . "Ꮰ") ("u" . "Ꮱ") ("v" . "Ꮲ"))) ("s" . (("a" . "Ꮳ") ("e" . "Ꮴ") ("i" . "Ꮵ") ("o" . "Ꮶ") ("u" . "Ꮷ") ("v" . "Ꮸ"))) ))
			 ("w" .  (("a" . "Ꮹ") ("e" . "Ꮺ") ("i" . "Ꮻ") ("o" . "Ꮼ") ("u" . "Ꮽ") ("v" . "Ꮾ")))
			 (" " . " ")
			 ("y" .  (("a" . "Ꮿ") ("e" . "Ᏸ") ("i" . "Ᏹ") ("o" . "Ᏺ") ("u" . "Ᏻ") ("v" . "Ᏼ")))
			 ))
    (setq words (split-string text))
    (setq words_quantity (length words))
    (setq word_idx 0)
    (setq word_out " ")
    (while (< word_idx words_quantity)
      (setq char_idx 0)
      (setq done_with_word nil)
      (setq word (nth word_idx words))
      (setq wordparts_idx 0)
      (setq wordparts (split-string word "-")) 
      (setq wordparts_qty (length wordparts))
      (setq wordpart_out "")
      (while (not done_with_word)
	(setq wordpart (nth wordparts_idx wordparts))
	(setq wordpart_length (length wordpart))
	(setq done_with_wordpart nil)
	(setq idx 0)    ;; character idx
	(while (not done_with_wordpart)
	  (setq wordpart (nth wordparts_idx wordparts))
	  (message wordpart) 
	  (message wordpart_out)
	  (setq ch (if (< idx wordpart_length) (substring wordpart idx (1+ idx)) ""))
	  (setq syl (cdr (assoc ch to_syllabary)))
	  (if (stringp syl)
	      (progn
		(setq wordpart_out (concat wordpart_out syl))
		(setq idx (1+ idx))
		(if (equal idx wordpart_length)
		    (setq done_with_wordpart t)
		  )
		)
					; else
	    (setq ch2 (if (< (1+ idx) (length wordpart))
			  (substring wordpart (1+ idx) (+ 2 idx))
			""))
	    (setq syl (cdr (assoc ch2 syl)))
	    (if (stringp syl)
		(progn
		  (setq wordpart_out (concat wordpart_out syl))
		  (setq idx (+ 2 idx))
		  (if (equal idx wordpart_length)
		      (setq done_with_wordpart t)
		    )
		  )
					; else
	      (if (null syl)  ; which will be the nil symbol
		  (progn
		    (setq wordpart_out (concat wordpart_out (cdr (assoc "■" (cdr (assoc ch to_syllabary))))))  ; "■" means default
		    (setq idx (+ 1 idx))
		    (if (equal idx wordpart_length)
			(setq done_with_wordpart t))
		    )
					; else
		(setq ch3 (if (and (< (+ 2 idx) (length wordpart)) (< (+ 3 idx) (length wordpart)))
			      (substring wordpart (+ 2 idx) (+ 3 idx))
			    ""))
		(setq syl (cdr (assoc ch3 syl)))
		(if (stringp syl)
		    (progn
		      (setq wordpart_out (concat wordpart_out syl))
		      (setq idx (+ 3 idx))
		      (if (equal idx wordpart_length)
			  (setq done_with_wordpart t)
			)
		      )
					; else
		  (if (null syl)  ; which will be the nil symbol
		      (progn
			(setq wordpart_out (concat wordpart_out (cdr (assoc "■" (cdr (assoc ch2 (cdr (assoc ch to_syllabary))))))))  ; "■" means default
			(setq idx (+ 2 idx))
			)
		    )
		  )
		)
	      ) 
	    )
	  ) ; ends while not done_with_wordpart
	(setq wordparts_idx (1+ wordparts_idx))
	(setq done_with_word (equal wordparts_idx wordparts_qty))
	(setq word_out (concat word_out wordpart_out))
	(setq wordpart_out "")
	) ; ends while not done_with_word  
      (setq word_out (concat word_out " "))
      (setq done_with_word nil)
      (setq word_idx (1+ word_idx))
      (when (< word_idx words_quantity)
	(setq word (nth word_idx words)))
      ) ; end while not done with word list
    (message word_out)     
    (insert word_out) 
    ) ; end let*
  )


 

