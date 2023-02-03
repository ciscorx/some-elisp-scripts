;; ciscorx_transliterate_to_cherokee.el            -*- lexical-binding: t; -*-          
(defun ciscorx/transliterate_to_cherokee ( begin end )
  "encode region to cherokee, outputing cherokee text after region"
  (interactive "r")
  (let* ((text (buffer-substring-no-properties begin end))
	 output done to_syllabary words words_length word_idx word_out char_idx done_with_word word_length_minus_1 word_length_minus_2 ch ch2 ch3 syl)


  
;    (setq text "dideloquasgi tsa")
    (setq text (replace-regexp-in-string "-" "" text))

    (setq done nil)
    
    (setq to_syllabary '(("a" . "Ꭰ") ("e" . "Ꭱ") ("i" . "Ꭲ") ("o" . "Ꭳ") ("u" . "Ꭴ") ("v" . "Ꭵ")
			 ("g" .  (("a" . "Ꭶ" ) ("e" . "Ꭸ") ("i" . "Ꭹ") ("o" . "Ꭺ") ("u" . "Ꭻ") ("v" . "Ꭼ")))
			 ("k" .  (("a" . "Ꭷ")))
			 ("h" .  (("a" . "Ꭽ") ("e" . "Ꭾ") ("i" . "Ꭿ") ("o" . "Ꮀ") ("u" . "Ꮁ") ("v" . "Ꮂ") ("n" . (("a" . "Ꮏ")))))
			 
			 ("l" .  (("a" . "Ꮃ") ("e" . "Ꮄ") ("i" . "Ꮅ") ("o" . "Ꮆ") ("u" . "Ꮇ") ("v" . "Ꮈ")))
			 ("m" .  (("a" . "Ꮉ") ("e" . "Ꮊ") ("i" . "Ꮋ") ("o" . "Ꮌ") ("u" . "Ꮍ") ("v" . "Ᏽ")))
			 ("n" .  (("a" . (( "^" . "Ꮎ") ("h" . "Ꮐ"))) ("e" . "Ꮑ") ("i" . "Ꮒ") ("o" . "Ꮓ") ("u" . "Ꮔ") ("v" . "Ꮕ")))
			 ("q" .  (("u" . (( "a" . "Ꮖ") ("e" . "Ꮗ") ("i" . "Ꮘ") ("o" . "Ꮙ") ("u" . "Ꮚ") ("v" . "Ꮛ")))))
			 ("s" .  (("^" . "Ꮝ") ("a" . "Ꮜ") ("e" . "Ꮞ") ("i" . "Ꮟ") ("o" . "Ꮠ") ("u" . "Ꮡ") ("v" . "Ꮢ")))
			 ("d" .  (("a" . "Ꮣ") ("e" . "Ꮥ") ("i" . "Ꮧ") ("o" . "Ꮩ") ("u" . "Ꮪ") ("v" . "Ꮫ") ("l" . (("a" . "Ꮬ"))) ))
			 ("t" .  (("a" . "Ꮤ") ("e" . "Ꮦ") ("i" . "Ꮨ") ("o" . "") ("u" . "") ("v" . "") ("l" . (("a" . "Ꮭ"  ) ("e" . "Ꮮ") ("i" . "Ꮯ") ("o" . "Ꮰ") ("u" . "Ꮱ") ("v" . "Ꮲ"))) ("s" . (("a" . "Ꮳ") ("e" . "Ꮴ") ("i" . "Ꮵ") ("o" . "Ꮶ") ("u" . "Ꮷ") ("v" . "Ꮸ"))) ))
			 ("w" .  (("a" . "Ꮹ") ("e" . "Ꮺ") ("i" . "Ꮻ") ("o" . "Ꮼ") ("u" . "Ꮽ") ("v" . "Ꮾ")))
			 ("y" .  (("a" . "Ꮿ") ("e" . "Ᏸ") ("i" . "Ᏹ") ("o" . "Ᏺ") ("u" . "Ᏻ") ("v" . "Ᏼ")))
			 ))
    
    (setq words (split-string text))
    (setq words_length (length words))
    (setq word_idx 0)
    (setq word_out " ")
    (while (< word_idx words_length)
      (setq char_idx 0)
      (setq done_with_word nil)
      (setq word (nth word_idx words))
      (setq word_length (length word))
      (setq word_length_minus_1 (1- word_length))
      (setq word_length_minus_2 (1- word_length_minus_1))
      
      (setq idx 0)    ;; character idx
      ;; loop for each syllable
      (while (not done_with_word)
	(setq ch (substring word idx (1+ idx)))
	(setq syl (cdr (assoc ch to_syllabary)))
	(if (equal (type-of syl) 'string)
	    (progn
	      (setq word_out (concat word_out syl))
	      (setq idx (+1 idx))
	      (if (equal idx word_length)
		  (setq done_with_word t)
		)
	      )
					; else
	  
	  (setq ch2 (substring word (1+ idx) (+ 2 idx)))
	  (setq syl (cdr (assoc ch2 syl)))
	  (if (equal (type-of syl) 'string)
	      (progn
		(setq word_out (concat word_out syl))
		(setq idx (+ 2 idx))
		(if (equal idx word_length)
		    (setq done_with_word t)
		  )
		)
					; else
	    (if (equal (type-of syl) 'symbol)  ; which will be the nil symbol
		(progn
		  (setq word_out (concat word_out (cdr (assoc "^" (cdr (assoc ch to_syllabary))))))  ; "^" means default
		  (setq idx (+ 1 idx))
		  )
					; else
	      (setq ch3 (substring word (+ 2 idx) (+ 3 idx)))
	      (setq syl (cdr (assoc ch3 syl)))
	      (if (equal (type-of syl) 'string)
		  (progn
		    (setq word_out (concat word_out syl))
		    (setq idx (+ 3 idx))
		    (if (equal idx word_length)
			(setq done_with_word t)
		      )
		    )
					; else
		(if (equal (type-of syl) 'symbol)  ; which will be the nil symbol
		    (progn
		      (setq word_out (concat word_out (cdr (assoc "^" (cdr (assoc ch2 (cdr (assoc ch to_syllabary))))))))  ; "^" means default
		      (setq idx (+ 2 idx))
		      )
		  )
		)
	      )
	    )
	  )  
	) ; end while not done with a particular word
      
      (setq word_out (concat word_out " "))
      (setq done_with_word nil)
      (setq word_idx (1+ word_idx))
      (setq word (nth word_idx words))
      )  ; end while not done with word list
      
      (insert word_out) 
      ) ; end let*
    )
  

 

