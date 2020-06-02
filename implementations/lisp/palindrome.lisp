#!/usr/bin/sbcl --script

; Apparently have to set this to next char after end or we lose the last char.
(defconstant fnboundary 25)
(defconstant lnstart 25)
(defconstant yep "YES")
(defconstant nope " NO")
(defvar infilename)
(defvar maxrecords)

(defun palindrome-check (str)
  (let ((strup (string-upcase str)))
    (string= strup (reverse strup))
  )
)

(defun process-line (line)
  "Processes one line of input with first/last names."
  (let ((firstname) (lastname) (fnispalindrome nope) (lnispalindrome nope))
    (setq firstname (subseq line 0 fnboundary))
    (setq lastname (subseq line lnstart))
    (if (palindrome-check (string-right-trim " " firstname))
	(setq fnispalindrome yep))
    (if (palindrome-check (string-right-trim " " lastname))
	(setq lnispalindrome yep))
    (format t "~a ~a ~a ~a~%" fnispalindrome firstname lnispalindrome lastname)
  )
)

; Convert to integer. Return nil if parse fails.
(defun coerce-to-integer (string)
  (ignore-errors (parse-integer string)))

;(format t "~&~S~&" sb-ext:*posix-argv*)
(setq infilename (nth 1 sb-ext:*posix-argv*))
(setq maxrecords (nth 2 sb-ext:*posix-argv*))

(if (eq infilename nil)
    (setq infilename "*standard-input*")
)

(if (not maxrecords)
    (setq maxrecords "0")) ; Make it a string version of 0.
			   ; It will be converted in the following let.

;(format t "Max Records: ~a~%" maxrecords)

(let ((tempvar (coerce-to-integer maxrecords)))
  (if (not tempvar)
      (progn
	(format t "ERROR: maxrecords is not a number.~%")
	(cl-user::quit)
      )
    )

  (setq maxrecords (coerce-to-integer maxrecords))
  (if (< maxrecords 0)
      (progn
	(format t "ERROR: maxrecords cannot be < 0.~%")
	(cl-user::quit)
      )
    )
  )
    
;(format t "Input File: ~S~&" infilename)
;(format t "Max Records: ~a~%" maxrecords)

(handler-case
    (let ((loopcount 0)
	  (in
	   (if (string= infilename "*standard-input*")
	       *standard-input*
	     (open infilename :if-does-not-exist nil)))
	  )
      (when in
	(loop for line = (read-line in nil)
	      while line
	      do (progn
;		   (if (and (> maxrecords 0)
		   (cond ((and (> maxrecords 0)
			    (>= loopcount maxrecords))
		       (return nil)))
		   (process-line line)
		   (incf loopcount)
		   )
	      )
	(close in)
	)
      (unless in
	(format t "ERROR: '~a' does not exist.~%" infilename)
	)

;      (format t "Records processed: ~a~%" loopcount)
   )
(sb-sys:interactive-interrupt
 ()
 (progn
   (format *error-output* "~%Abort.~&")
   (cl-user::quit)
   )
)
(sb-kernel:bounding-indices-bad-error
 ()
 (progn
   (format *error-output* "~%Bad input. Aborting.~&")
   (cl-user::quit)
   )
 )
)
