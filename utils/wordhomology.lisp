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


(defpackage :wh (:use :common-lisp))
(in-package :wh)

(defun whtest ()
  (let* ((test-in-words
	 '(("cat" . "dog")
	   "elephant"
	   ("dinosaur" . "trex")
	   "horse" 
	   ("snake" . "apple")
	   "robin"
	   ))
	 (whtbl (init-wh-target-list test-in-words))
	 (test-out-words
	  '("car" "house" "scan" "ribbon"))
	 )
    (loop for word in test-out-words
	  do (print (list word (sort (word-homology word whtbl)
				     #'(lambda (a b) (> (car a) (car b))))))
	  )))

;;; This compiles a list of words with compile-word. Each word can be
;;; a single string, or a list, in which case the whole list is stored
;;; (and returned) upon matching. 

(defun init-wh-target-list (names-or-info-lists)
  (loop for name-or-info-list in names-or-info-lists
	collect (cons (compile-word (if (listp name-or-info-list) 
					(car name-or-info-list) 
				      name-or-info-list))
		      name-or-info-list)))

;;; This converts a word to a series of numbers where each unique pair of letters 
;;; is a new number and adds it to the existing word-homology database.  The resulting
;;; list of numbers is sorted so that fast-n-member can cut off search early.  Note
;;; that the pairs are order-independent, so that AB is the same as BA although they
;;; are only stored in one order in the table (which is the first order that they
;;; happen to be seen in).

(defun compile-word (word)
  (setq word (string-downcase word))
  (sort 
   (loop for p from 0 to (- (length word) 2)
	 as l1 = (aref word p)
	 as l2 = (aref word (1+ p))
	 collect (+ (* (char-code l1) 1000) (char-code l2)))
   #'<))

;;; This function determines how similar the two input lists are, where the lists are the numbers generated
;;; by compile-word

(defun score-homology (w1 w2)
  (let* ((o1 (loop for l1 in w1 if (fast-n-member l1 w2) sum 1))
	 (o2 (loop for l2 in w2 if (fast-n-member l2 w1) sum 1))
	 (l1 (length w1))
	 (l2 (length w2))
	 )
    (if (and (not (zerop o1)) (not (zerop o2)))
	(/ (+ (/ o1 l1) (/ o2 l2)) 2.0)
      0.0)))

;;; This does member for numbers, but assumes that the target list is sorted
;;; so that if it hits a number greater than the one we're at, it can cut the
;;; search short.

(defun fast-n-member (n n*)
  (loop for n2 in n*
	do (cond ((= n n2) (return t))
		 ((> n2 n) (return nil)))))

;;; Get the top N scoring of a set of words w/o having to sort, which is too slow.

(defun word-homology (w whtbl &optional (n 3))
  (let* ((wc (compile-word w))
	 (topn (loop for i from 1 to n collect (list 0 'foo)))
	 )
    (when wc ; Compile-word gives nil if there's no entries that match any letter combination
      ;; If the new word is more than the lowest of the ones in the set, replace that one with the new one.
      (loop for (tc . tw) in whtbl
	    as score = (score-homology wc tc)
	    as lowest = (loop with least-entry = (car topn)
			      with least-score = (car least-entry)
			      for test-entry in (cdr topn)
			      as ts = (car test-entry)
			      if (< ts least-score)
			      do 
			      (setq least-score ts least-entry test-entry)
			      finally (return least-entry))
	    if (> score (car lowest))
	    do 
	    (setf (first lowest) score)
	    (setf (second lowest) tw)
	    finally (return (sort (remove (list 0 'foo 'bar) topn :test #'equal)
				  #'> :key #'car))
	    ))))
