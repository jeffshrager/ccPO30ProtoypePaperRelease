;;;***************************************************************************
;;;* Copyright 2017 by Cancer Commons                                        *
;;;*                                                                         *
;;;* Permission is hereby granted, free of charge, to any person obtaining   *
;;;* a copy of this software and associated documentation files (the         *
;;;* "Software"), to deal in the Software without restriction, including     *
;;;* without limitation the rights to use, copy, modify, merge, publish,     *
;;;* distribute, sublicense, and/or sell copies of the Software, and to      *
;;;* permit persons to whom the Software is furnished to do so, subject to   *
;;;* the following conditions:                                               *
;;;*                                                                         *
;;;* The above copyright notice and this permission notice shall be          *
;;;* included in all copies or substantial portions of the Software.         *
;;;*                                                                         *
;;;* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         *
;;;* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      *
;;;* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND                   *
;;;* NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE  *
;;;* LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION  *
;;;* OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION   *
;;;* WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.         *
;;;***************************************************************************


(defpackage :utils (:use :common-lisp :common-lisp-user)
  (:export flatten remdups dht all-ordered-sublists pprint+ *pprint+* string-split first-n log! *log* pretty-ts
	   lenient-string-equal host docs header footer *kp-version-utime* human-readable-timestamp 
	   clean-xml dig-out-first dig-out-all xlate-unicode-file-in-place funmat))

(in-package :utils)

(defvar *kp-version-utime* (get-universal-time))

(defvar *localhost?* "Should be set by kp.asd initalization!")

(defun host ()
  (if *localhost?* "http://localhost:4240" "http://platformtest.cancercommons.org:4240"))
(defun docs ()
  (if *localhost?* "http://localhost:4241" "http://platformtest.cancercommons.org:4241"))

;;; =============================================
;;; (funmat out-stream "Foo ~a" exp1 " bar ~s" exp2 ...)
;;; -> (format out-stream "Foo ~a bar ~s" exp1 exp2 ...)

(defmacro funmat (stream &rest args)
  `(format ,stream 
	   ,(apply #'concatenate 'string 
		   (loop for arg in args by #'cddr
			 collect arg))
	   ,@(loop for arg in (cdr args) by #'cddr
		   collect arg)))

(defun header (stream title)
  (funmat stream "
<html>
<head>
<style>
h1, h2, h3, h4, h5, h6 {
    font-family: Georgia, Times, serif;
    text-shadow: 1px 1px 1px #ccc;
}

textarea  
{  
   font-family: Ariel, Times, serif;  
   font-size: 18px;   
}

p, div {
    font-family: Georgia, Times, serif;
}

p {
    line-height: 125%;
    max-width: 10in;
    margin-top: 20px;
    margin-bottom: 10px;
    margin-right: 15px;
    margin-left: 18px;
}

body {
    background-color: #feffff;
}

table {
    border-collapse: collapse;
    width: 100%;
}

th, td {
    padding: 8px;
    text-align: left;
    border-bottom: 1px solid #ddd;
}

input[type=text] {
    padding:5px; 
    border:2px solid #ccc; 
    -webkit-border-radius: 5px;
    border-radius: 5px;
}

input[type=text]:focus {
    border-color:#333;
}

input[type=submit] {
    padding:5px 15px; 
    background:#80aaff;
    font-weight: bold;
    border:1px solid #ccc;
    cursor:pointer;
    -webkit-border-radius: 5px;
    border-radius: 5px; 
}

tr:hover{background-color:#f5f5f5}

<meta http-equiv=\"Content-type\" name=\"viewport\" content=\"initial-scale=1.0, maximum-scale=1.0, user-scalable=no, width=device-width\">

</style>
</head>
<body>
<table><tr><td><h1>~a" title "</h1></td><td><a href=http://cancercommons.org><img src=https://www.cancercommons.org/wordpress/wp-content/themes/cancercommons/assets/images/logo@2x.png></a></td></tr></table>
<center><em><font color=red>FOR DEMONSTRATION AND EDUCATIONAL PURPOSES ONLY; NOT TO BE USED IN CLINICAL SETTINGS; DO NOT ENTER PHI</font></em></center>
")
  (navbar stream)
  )

(defun navbar (stream)
  (funmat stream "
<hr><center><a target=_blank href=~a" (host) "/trexui>[TrEx]</a>
&nbsp;&nbsp;<a href=~a" (host) "/quark>[Quark]</a>&nbsp;&nbsp;
<a href=~a" (host) "/vetui>[Peer Review]</a>&nbsp;&nbsp; 
<a href=~a" (host) "/iio>['<em>Insights in Oncology</em>' Nano-Journal]</a>&nbsp;&nbsp; 
<a href=~a" (host) "/norman>[Trials Dashboard]</a>&nbsp;&nbsp; 
<a href=~a" (docs) "/docs/SelfDrivingTour.html target=_blank>[Take a Tour]</a></b>&nbsp;&nbsp;
<a href=~a" (docs) "/docs/index.html target=_blank>[Docs]</a></b> 
</center><hr>
"))

(defun footer (stream)
  (navbar stream)
       (format stream "Copyright 2017 <a
href=https://www.cancercommons.org/>Cancer Commons</a>. These
tools were built by a team headed by Jeff Shrager, Connor Sweetnam,
and Robert Baertsch. Thanks, as well, to advice and guidance from
Simone Mocellin. For support or futher information contact <a
href=mailto:jeff@cancercommons.org>Jeff
Shrager (jeff@cancercommons.org)</a></body> </html>
"))

(defun flatten (l)
  (cond ((null l) nil)
	((atom l) (list l))
	(t (append (flatten (car l))
		   (flatten (cdr l))))))

(defun remdups (l &key (test #'equal))
  (loop for e+ on l
	unless (member (car e+) (cdr e+) :test test)
	collect (car e+)))

(defun dht (table &optional (n 10))
  (maphash #'(lambda (key value)
	       (when (zerop (decf n)) (return-from dht))
	       (format t "~s: ~s~%" key value)	       
	       )
	   table))

;; This version of string-equal ignores white space and non-printing
;; characters, as well as case folding. It's slightly inefficient bcs
;; it copies both strings, but as this is only used in the testing. It
;; also tells you where the problem starts, if it fails.

(defun lenient-string-equal (a b)
  (string-equal (copy-ascii-only-string a) (copy-ascii-only-string b)))

(defparameter *allowed-chars*
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ:\",{}[]0123456789-\'/*+=%$#@!)(&^><;|._")

(defun copy-ascii-only-string (s)
  (loop for c across s
	with r = ""
	when (position c *allowed-chars* :test #'char-equal)
	do (setf r (format nil "~a~c" r c))
	finally (return r)))

;;; ===================================================================
;;; --- Time/date functions.

(defun human-readable-timestamp ()
  (multiple-value-bind 
   (sec min hr day mo year)
   (decode-universal-time (get-universal-time))
   (format nil "~a~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d" year mo day hr min sec)))

(defun get-time (&optional (utime (get-universal-time)))
  (multiple-value-bind
    (second minute hour date month year dow dstp tz)
    (decode-universal-time (or utime ))
    (list :second second
	  :minute minute
	  :hour hour
	  :date date
	  :month month
	  :year year
	  :dow dow
	  :dstp dstp
	  :tz tz)))

(defun pretty-ts (&optional (utime (get-universal-time)))
  (format nil "~a@~a" (pretty-date utime) (pretty-time utime)))

(defun time-as-int (&optional (utime (get-universal-time)))
  (let ((time (get-time utime)))
  (+ (* (getf time :hour) 3600)
     (getf time :second)
     (* 60 (getf time :minute)))))

(defun pretty-time (&optional (utime (get-universal-time)))
  (let* ((time-as-int (time-as-int utime))
	 (r (mod time-as-int 3600))
	 (h (truncate (/ time-as-int 3600)))
	 (s (mod r 60))
	 (m (truncate (/ r 60))))
    (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)))

(defvar *months* '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun pretty-date (&optional (utime (get-universal-time)))
  (let* ((time (get-time utime))
	 (date (getf time :date))
	 (month (getf time :month))
	 (year (getf time :year))
	 )
    (format nil "~a-~2,'0d-~2,'0d" year month date)))
    
;;; ===================================================================

(defun all-ordered-sublists (l)
  "From '(a s d f) produce: ((A) (S) (D) (F) (A S) (S D) (D F) (A S D) (S D F) (A S D F))"
  (let ((len (length l)))
    (loop for curlen from 1 to len
	  append (loop for w+ on l
		       as k from 1 to (1+ (- len curlen))
		       collect (loop for i from 1 to curlen
				     as w in w+
				     collect w)))))


(defvar *pprint+?* nil)
(defun pprint+ (l &key label) 
  (when *pprint+?* 
    (when label (format t "~%vvvvvvvvvvv ~a vvvvvvvvvvv~%" label))
    (pprint l) (terpri)
    (when label (format t "~%^^^^^^^^^^^ ~a ^^^^^^^^^^^~%" label))
    )
  l)

(defun first-n (l n) (loop for i below n as elt in l collect elt))

(defun string-split (string &key (delimiter #\space) (convert-num-values? nil))
  "Split string into substrings delimited by delimiter"
  (let ((substrings '())
        (length (length string))
        (last 0))
    (flet ((add-substring 
	    (i)
	    (push (subseq string last i)
		  substrings)))
	  (dotimes (i length)
	    (when (eq (char string i) delimiter)
	      (add-substring i)
	      (setq last (1+ i))))
	  (add-substring length)
	  (let ((substrings (nreverse substrings)))
	    (if convert-num-values?
		(loop for string in substrings
		      as v = (ignore-errors (read-from-string string))
		      if (numberp v)
		      collect v
		      else 
		      collect string)
	      substrings)))))


(defvar *log* "INIT-LOG makes this a filename.")

(defun init-log ()
  (setq *kp-version-utime* (get-universal-time))
  (setq *log* (format nil "logs/~a-trex-log.lisp" *kp-version-utime*))
  (log! :init-log)
  )

(defun log! (key &optional format-template &rest format-args)
  (with-open-file
   (log *log* :direction :output :if-does-not-exist :create :if-exists :append)
   (format log "(~a ~a " key (get-universal-time))
   (when format-template (apply #'format log format-template format-args))
   (format log ")~%")
   ))

;;; ============= From norman:

(defun xml-from-file->lisp (file)
  (clean-xml (cxml:parse-file file (cxml:make-whitespace-normalizer (cxml-xmls:make-xmls-builder)))))

(defun clean-xml (xml)
  (loop for entry in (destringify-tree xml)
	when (not (null entry))
	collect entry))

(defun destringify-tree (tree)
  "Walk a tree and turn all the strings into symbols (or numbers)."
  (cond ((null tree) tree)
	((stringp tree) 
	 (setq tree (string-trim " 	
" tree))
	 (if (not (zerop (length tree)))
	     (cond ((char-equal #\~ (aref tree 0))
		    `(not ,(read-from-string (subseq tree 1))))
		   (t tree))))
	((atom tree) tree)
	((and (listp tree)
	      (equal "value" (car tree))
	      (equal '(("dataType" "string")) (second tree))
	      )
	 (third tree))
	(t (cons (destringify-tree (car tree))
		 (destringify-tree (cdr tree))))))

(defun remstring (target from)
  (loop with len = (length target)
	as start = (search target from :test #'char-equal)
        until (null start)
	finally (return from)
	do (setq from (format nil "~a~a" (subseq from 0 start) (subseq from (+ start len))))))

(defun replace-string (string target replacement)
  (loop with len = (length target)
	with start-at = 0
	as pos = (search target string :test #'char-equal :start2 start-at)
        until (null pos)
	finally (return string)
	do (setq string (format nil "~a~a~a" (subseq string 0 pos) replacement (subseq string (+ pos len))))
	(setq start-at (+ pos len))))

(defun downcase-tree (tree)
  (cond ((null tree) nil)
	((stringp tree) (string-downcase tree))
	((listp tree)
	 (cons (downcase-tree (car tree))
	       (downcase-tree (cdr tree))))
	(t tree)))
 
(defun shorten-string (s &optional (len 80))
  (if (> (length s) len)
      (format nil "~a..." (subseq s 0 (- len 3)))))

(defun all-contiguous-sublists (set &key (length-lower-limit 1) (length-upper-limit 4))
  (loop for n from length-lower-limit to (min (length set) length-upper-limit)
	append (contiguous-sublists-of-length n set)))

(defun contiguous-sublists-of-length (n set)
  (loop for set+ on set
	when (>= (length set+) n)
	collect (first-n n set+)))

;;; Ugly XML tools

;;; Dig-out is used to extract stuff from arbitrarily deep inside
;;; trees. You provide a tree, and one or a list of search forms. Each
;;; search form can be either a string, in which case it is assumed to
;;; head the target form (by #'string-equal), or a one argument
;;; function that is applied to the tree each step of the
;;; search. (I.e., if the form is a string, that's equivalent to
;;; saying #'(lambda (tree) (string-equal (car tree) "string")) For
;;; convenience we wrap this whole operating at each level in an
;;; ingnore-errors. The multiple forms are evaluated in sequence,
;;; allowing you to pull out sub-elements.  Note the order of
;;; extraction is outside in, so you would access the outer levels
;;; first, in the natural way (but not the lisp functional semantics
;;; way!) This can be a little confusing bcs if you find an outer key,
;;; it can shadow an inner one.

#| Usage:

(defparameter x '(("f" 0) (("a" 1 ((("b" 2) ((("c" 3) ("d" 4 (((("e" 5)) ((((("f" 6)))))))) 
			      ((("g" 7 (("h" 8)))))((("e" 25)) ((((("f" 26))))))))) 
		   ("i" 9))) ((("j" (("c" 13) 
				     ("d" 14 (((("e" 15)) ((((("f" 16)))))))) ((("g" 17 (("h" 18)))))) 10))))))

(defun test ()
  (pprint (dig-out x '("d" "f")))
  (pprint (dig-out x (list "d" #'(lambda (form) (string-equal (car form) "f")))))
  (pprint (dig-out x (list #'(lambda (form) (string-equal (car form) "d")) "f")))
  (pprint (dig-out x (list #'(lambda (form) (string-equal (car form) "d")) #'(lambda (form) (string-equal (car form) "f")))))
  )

> (test)
Results:
(("f" 6) ("f" 16))
(("f" 6) ("f" 16))
(("f" 6) ("f" 16))
(("f" 6) ("f" 16))

|#

(defun dig-out (tree forms)
  (cond ((null forms) (list tree))
	((atom tree) nil)
	((and (stringp (car forms))
	      (ignore-errors (string-equal (car forms) (car tree))))
	 (dig-out tree (cdr forms)))
	((and (listp (car forms))
	      (ignore-errors (equal (car forms) (car tree))))
	 (dig-out tree (cdr forms)))
	((and (functionp (car forms))
	      (ignore-errors (funcall (car forms) tree)))
	 (dig-out tree (cdr forms)))
	(t (append (dig-out (car tree) forms)
		   (dig-out (cdr tree) forms)))))

(defun dig-out-all (header tree)
  (dig-out tree (list header)))
(defun dig-out-first (header tree)
  (first (dig-out-all header tree)))
(defun dig* (header tree)
  (dig-out tree (list #'(lambda (form) (string-equal header (caar form))))))

;;; Cleann up trial XML files, xlating fancy unicode chars into ascii
;;; equivs characters. Then re-compute their Lisp forms.

(defparameter *unicode-to-ascii* ;; Following #\latin_small_letter_a_with_circumflex
  '(
    ((#\U+0080 #\Cent_Sign) "*")
    ((#\U+0080 #\Broken_Bar) "...")
    ((#\U+0080 #\U+0098) "'")
    ((#\U+0080 #\U+0099) "'")
    ((#\U+0080 #\U+0093) "-")
    ((#\U+0080 #\U+009C) "\"")
    ((#\U+0080 #\U+009D) "\"")
    ((#\U+0080 #\U+0094) "-")
    ((#\U+0080 #\U+0092) "degC")
    ((#\U+0080 #\U+009F) "\"")
    ((#\U+0080 #\U+008B) "v[?]")
    ((#\U+0080 #\No-Break_Space) "[sword]")
    ((#\U+0080 #\U+0090) "-")

    ((#\U+0081 #\Superscript_One) "&gt;=")

    ((#\U+0082 #\Not_Sign) "(R)")

    ((#\U+0084 #\U+0083) "degC")
    ((#\U+0084 #\Cent_Sign) "(TM)")

    ((#\U+0085 #\Superscript_One) "x")

    ((#\U+0085 #\No-Break_Space) "1[I]")
    ((#\U+0085 #\Inverted_Exclamation_Mark) "2[II]")
    ((#\U+0085 #\Cent_Sign) "3[III]")
    ((#\U+0085 #\Pound_Sign) "4[IV]")
    ((#\U+0085 #\Currency_Sign) "5[V]")

    ((#\U+0086 #\U+0093) "[down-arrow]")
    ((#\U+0086 #\U+0092) "--&gt;")

    ((#\U+0088 #\U+0092) "-") 
    ((#\U+0088 #\U+009E) "[Infinity]")

    ((#\U+0089 #\U+0088) "~")
    ((#\U+0089 #\Broken_Bar) "&lt;=")
    ((#\U+0089 #\Section_Sign) "&gt;=")
    ((#\U+0089 #\Yen_Sign) "&gt;=")
    ((#\U+0089 #\Currency_Sign) "&lt;=")

    ((#\U+0091 #\No-Break_Space) "(1)")
    ((#\U+0091 #\Inverted_Exclamation_Mark) "(2)")
    ((#\U+0091 #\Cent_Sign) "(3)")
    ((#\U+0091 #\Pound_Sign) "(4)")
    ((#\U+0091 #\Yen_Sign) "(6)")
    ((#\U+0091 #\Broken_Bar) "(7)")
    ((#\U+0091 #\Section_Sign) "(8)")

    ))

(defun xlate-unicode-file-in-place (infile)
  (format t "xlating unicode for ~a~%" infile)
  (with-open-file 
   (i infile)
   (with-open-file
    (o "/tmp/xl.tmp" :direction :output :if-exists :supersede)
    (loop for line = (read-line i nil nil)
	  until (null line)
	  do 
	  (loop with new = ""
		   with skip = 0
		   as pos from 0 by 1
		   as char across line
		   do
		   (cond ((not (zerop skip))
			  (decf skip))
			 ((equal char #\latin_small_letter_a_with_circumflex)
			  (let ((pair (list (aref line (+ pos 1)) (aref line (+ pos 2)))))
			    (setq new (format nil "~a~a" new (second (assoc pair *unicode-to-ascii* :test #'equal))))
			    (setq skip 2)))
			 (t (setq new (format nil "~a~c" new char)))
			 )
		   finally (format o "~a~%" new)
		   ))))
  (let ((holdfile (format nil "~a.xlhold" infile)))
    (when (probe-file holdfile)
      (delete-file holdfile))
    (rename-file infile holdfile)
    (rename-file "/tmp/xl.tmp" infile)
    ))

