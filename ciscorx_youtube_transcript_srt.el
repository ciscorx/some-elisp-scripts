
(defun ciscorx/youtube_transcript->srt ()
  "Converts current buffer from youtube transcript to srt format, a text format for subtitles.  In order to obtain a youtube transcript it is necessary to first navagate with a browser to a youtube video and then click the 3 dots '...' to the right of the Dislike, Share and Download options, which will pop up a menu containing a button labeled 'Show transcription'.  It is necessary to click that button and then cut and paste the entire page, using control-a control-c, into an emacs buffer before running the ciscorx/youtube_transcript->srt function from within that buffer."
  (interactive)
  (let (tmppos yttimestamp cntr nxt_hour nxt_min nxt_sec cur_hour cur_min cur_sec)
    (beginning-of-buffer)

    ;; delete all text in buffer except the actual transcript part
    (if (re-search-forward "^Search in video" nil t)
	(progn
	  (setq tmppos (line-end-position))
	  (delete-region 1 tmppos)
	  (forward-line)
	  (backward-delete-char-untabify 1)
	  (forward-paragraph)
	  (delete-region (point) (point-max))
	  (goto-char (point-min))
	  )
      (re-search-forward "^Transcript")
      (forward-line)
      (setq tmppos (line-end-position))
      (delete-region 1 tmppos)
      (forward-line)
      (backward-delete-char-untabify 1)
      (forward-paragraph)
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (kill-line)
      )
    
  ;; traverse each line of transcript, updating the timestamps to srt format
    (setq cntr 1)
    (setq nxt_hour 0)
    (setq nxt_min 0)
    (setq nxt_sec 0)
    (setq yttimestamp "\\([[:digit:]]?[[:digit:]]?\\):?\\([[:digit:]]?[[:digit:]]\\):\\([[:digit:]][[:digit:]]\\)")
    (catch 'transcript_format
      (while (not (eobp))
	(insert (number-to-string cntr))
	(newline)
	(if (looking-at yttimestamp)
	    (progn 
	      (setq cur_hour (string-to-number (match-string-no-properties 1)))
	      (setq cur_min (string-to-number (match-string-no-properties 2)))
	      (setq cur_sec (string-to-number (match-string-no-properties 3)))
	      (save-excursion
		
		(forward-line 2)
		(beginning-of-line)
		(save-match-data
		  (if (looking-at yttimestamp)
	              (progn 
			(setq nxt_hour (string-to-number (match-string-no-properties 1)))
			(setq nxt_min (string-to-number (match-string-no-properties 2)))
			(setq nxt_sec (string-to-number (match-string-no-properties 3)))
			)
		 ;  (throw 'transcript_format "error")
		    (setq nxt_min (1+ cur_min))
		    )
		  )
		)
	      (kill-line)
	      (insert (concat (format "%02d" cur_hour) ":" (format "%02d" cur_min) ":" (format "%02d" cur_sec) ",400  -->  " (format "%02d" nxt_hour) ":" (format "%02d" nxt_min) ":" (format "%02d" nxt_sec) ",300"))
	      
	      (forward-line 2)
	      (newline)
	      (setq cntr (1+ cntr))
	      )
	  (throw 'transcript_format "error")
	  )
	)
      )
    )
  )
     
