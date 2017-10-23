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


(in-package :trex)

#|

so upon boot-up:
	we check if there is a value associated with keyname
		if there is we populate with the user's data (so they don't have to fill it in again)
		if not we create a new gensym and store it in an alist (along with eventually the user's responses)
	if there is a value but it is not in the alist of gensyms, we raise and error and ask the user to reload the page or something -DONE
|#

(setf hunchentoot:*show-lisp-errors-p* t
      hunchentoot:*show-lisp-backtraces-p* t)

(defvar *trexrecs* nil)
(defvar *reckeys* nil)
(defvar *tests* nil)
(defvar *drugs* nil)

;;; Test keys are just numbers that refer to the tests to simplify
;;; JSON I/O. 

(defvar *test-keys-alist* "Set by initialization")
(defvar *drug-keys-alist* "Set by initialization")
(defvar *user-ids* nil) ; a list to contain the user ids 

(defvar *name->drug* (make-hash-table :test #'equal))
(defstruct drug name hypotheses all-factoids ;; (is all-factoids used???)
  (relation->factoids (make-hash-table :test #'equal)))

(defparameter *relations* '(:efficacy :toxicity :synergism :progression))

(defvar *r/c/d->hyp* (make-hash-table :test #'equal))
(defstruct (hypothesis (:print-function 
			(lambda (o s k)
			  (declare (ignorable k))
			  (format s "[Hyp:~s(~s;~s) ts=~a(~a)~%[CalcSets(prev=~a:pp=~a: ~s): ~s]~%[Facts:~s]"
				  (drug-name (hypothesis-drug o)) (hypothesis-condition o) (hypothesis-relation o)
				  (hypothesis-total-score o) (hypothesis-corrected-total-score o)
				  (hypothesis-prevalent-calc-set-key-is-1/0/1 o)
				  (hypothesis-prevalent-percentage o) 
				  (hypothesis-prevalent-calc-set o) 
				  (hypothesis-calc-sets o)
				  (hypothesis-factoids o)))))
  drug factoids relation condition 
  ;; Each hypothesis has three calc sets, one for each of -1, 0, and
  ;; +1 Unfortunately, for various computations we need the key of the
  ;; prevelant calc set (-1, 0, or +1) and for others we need the
  ;; actual calc set, so we keep them both, which is ughly...Oh well.
  (calc-sets (loop for i from -1 to 1 by 1 collect (cons i (make-calc-set :sign i))))
  prevalent-calc-set-key-is-1/0/1 prevalent-calc-set prevalent-percentage
  (total-score 0.0) corrected-total-score 
  )

(defvar *factoids* nil)
(defstruct (factoid (:print-function 
		     (lambda (o s k)
		       (declare (ignorable k))
		       (format s "[Fact:~a ~s ~s ~s ~s ~a ~a ~a@~a(~a)~%~s"
			       (factoid-fact-id o)
			       (drug-name (factoid-drug o))
			       (factoid-molecule o)
			       (factoid-molecule-state o)
			       (factoid-relation o)
			       (factoid-relationship o)
			       (factoid-condition o)
			       (factoid-model o)
			       (factoid-cases o)
			       (factoid-h o)
			       (factoid-hypothesis o)
			       ))))
  fact-id drug source molecule molecule-state relationship 
  condition ;; NNN condition is the index FFF
  drug-name model h cases reference notes relation hypothesis)

(defstruct (calc-set (:print-function (lambda (o s k)
					(declare (ignorable k))
					(format s "[CS:~a es=~a(~a) sp=~a(~a) w=~a w*sp=~a]"
						(calc-set-sign o) (calc-set-evidence-score o)
						(calc-set-corrected-evidence-score o)
						(calc-set-score-percentage o) (calc-set-corrected-score-percentage o)
						(calc-set-weight o) (calc-set-weight-times-sp o)))))
  sign (evidence-score 0.0) corrected-evidence-score score-percentage corrected-score-percentage variance standard-error
  confidence-interval confidence-interval-upper confidence-interval-lower weight weight-times-sp)

;;; Here "state" is something like ("mut" "v600e") and molecule is a
;;; list, something like ("braf" "v600e") or ("braf" "mut"
;;; "v600e"). 

(defparameter *equivalent-indications*
  '(("expression" "overexpression" "expressed" "overexpressed" "active (high activity)" "active")
    ("phosphorylated (activated)" "phosphorylated" "activated")
    ("methylated" "methylation")
    ("amplified" "amplification")
    ))

(defun state-match (state molecule)
  (intersection state molecule 
		:test #'(lambda (a b)
			  (setq a (string-trim " ()" a) b (string-trim " ()" b))
			  (or 
			   (string-equal a b)
			   (loop for entry in *equivalent-indications*
				 if (and (member a entry :test #'string-equal) (member b entry :test #'string-equal))
				 do (return t))))))

(defun output-result (molecule)
  (let ((matches (recs-matching-molecule molecule)))
    (if matches
	(progn
	  (format t "<p><hr><p>BIOMARKER CONTEXT: ~a:<br><p>" molecule)
	  (format t "<table border=1>~%<tr>")
	  (loop for key in *reckeys*
		do (format t "<td>~a</td>" key))
	  (format t "</tr>~%")
	  (mapcar #'(lambda (rec) 
		      (format t "<tr>")
		      (loop for key in *reckeys*
			    do (format t "<td>~a</td>" (cdr (assoc key rec))))
		      (format t "<tr>")
		      )
		  matches
		  )
	  (format t "</table>"))
      (output-all-possible-molecular-states)
      )))
  
(defun recs-matching-molecule (molecule)
  (loop for rec in *trexrecs* 
	when (molecule-matches-state? molecule (assoc :molecule rec))
	collect rec))

(defun molecule-matches-state? (molecule state)
  (pop state)
  (and 
   ;; Check that the gene is there (this is merely name match).
   (member (string-trim " " (string-downcase (cdr (assoc :name state)))) molecule :test #'string-equal)
   ;; State match is more complex! 
   (or (null (cdr molecule)) ;; if the only gene name given, accept any mutation
       (state-match (split (cdr (assoc :state state)) #\space) molecule)
       ))
  )

(defun get-test-state (test)
  (hunchentoot::compute-parameter test 'string :both))

(defun convert-test-state-to-input (test)
  (cond 
   ((string-equal (get-test-state test) "Positive") (cons test 1))
   ((string-equal (get-test-state test) "Negative") (cons test -1))
   ((string-equal (get-test-state test) "Untested") (cons test 0))))
		
(defun radio-states-to-stats-input (&key (display-link? nil))
  (let ((r
	 (loop for test in *tests*
	       collect (convert-test-state-to-input test))))
    (when display-link?
      (let ((api-arg 
	     (with-output-to-string
	       (s) 
	       (json::encode-json 
		(loop for item in r
		      collect (list 
			       (car (find (car item) *test-keys-alist* :key #'cdr))
			       (cdr item)))
		s))))
	;; (format t "(<a target=_blank href=trexrank?observations=~a>Click here to call the API with: observations=~a</a>)" api-arg api-arg)
	))
    r))

;;; When there are no matches we provide guidance.

(defvar *molname->stati* (make-hash-table :test #'equal))

(defun output-all-possible-molecular-states ()
  (or (not (zerop (hash-table-count *molname->stati*)))
      (loop for rec in *trexrecs*
	    as mol = (cdr (assoc :molecule rec))
	    as name = (cdr (assoc :name mol))
	    as state = (cdr (assoc :state mol))
	    do (pushnew state (gethash name *molname->stati*) :test #'string-equal)))
  (loop for (mol . states) in 
	(sort (loop for name being the hash-keys of *molname->stati*
		    using (hash-value muts)
		    collect (cons (string-downcase name) muts))
	      #'string< :key #'car)
	do (format t "<p><b>~a:</b> " mol)
	(loop for mut in states
	      do (format t "~a, " (string-downcase mut)))))

(defun init-trex-server ()
  (initialize)
  (setq *reckeys* (mapcar #'car (car *trexrecs*)))
  (hunchentoot:define-easy-handler 
   (trexui :uri "/trexui")
   nil
   (let ((user-id (hunchentoot::compute-parameter "user-id" 'string :both)))
     (if user-id (current-user user-id) (new-user))))
  (hunchentoot:define-easy-handler 
   (trexev :uri "/trexev")
   (key)
   (with-output-to-string
     (s) (json::encode-json 
	  (generalized-evidence-lookup key) s)))
  (hunchentoot:define-easy-handler 
   (trexrank :uri "/trexrank")
   (observations)
   (with-output-to-string
     (*standard-output*) 
     (json::encode-json 
      (with-input-from-string 
       (i observations)
       (trexranker (cl-json::decode-json i))))))
  (hunchentoot:define-easy-handler 
   (trexkeys :uri "/trexkeys")
   ()
   (with-output-to-string
     (*standard-output*) 
     (format t "{~%\"tests\":~%")
     (json::encode-json *test-keys-alist*)
     (format t ",~%\"drugs\":~%")
     (json::encode-json *drug-keys-alist*)
     (format t "~%}~%")
     ))
  (hunchentoot:define-easy-handler 
   (trexseev :uri "/trexseev") ;; Pretty evidence viewer
   (type key)
   (seev type key))
  (hunchentoot:define-easy-handler 
   (trexresetdb :uri "/trexresetdb") 
   ()
   (setq *trexrecs* nil quark::*unvetted-insights* nil)
   (initialize)
   "<meta http-equiv=\"refresh\" content=\"1; url=trexui\" />"
   )
  (hunchentoot:define-easy-handler 
   (trexrec :uri "/trexrec") 
   (id format)
   (show-trexrec id format)
   )
  )

(defun show-trexrec (id format)
  (cl-who:with-html-output-to-string
   (*standard-output* nil :prologue t)
   (let ((rec (find id *trexrecs* :test #'(lambda (id rec) (string-equal (cdr (assoc :--id rec)) id)))))
     (if rec
	 (cond ((string-equal format "lisp")
		(pprint rec))
	       ((string-equal format "json")
		(cl-json:encode-json rec *standard-output*) 
		(terpri))
	       (t (format t "\"**INVALID_FORMAT**\"")))
       (format t "\"**NO_SUCH_RECORD**\"")))))

(defun decode-drug-from-parse (parse)
  (let* ((d (second (assoc :drug parse)))
	 (o (car (ncit::term->objs d)))) ;; FFF Takes the first heurietically FFF WWW
    (if o (ncit::obj-common-name o) d)))

(defun convert-parse-to-trex-alist (parse &key (source-sentence "n/a") (context nil))
  (let* ((trex-id (generate-trex-id))
	 (drug (decode-drug-from-parse parse)))
    `(
      (:--ID . ,trex-id) 
      (:CANCER . ,(find-cancertype (cdr (assoc :cancer parse)))) 
      (:|*ALIAS (MODIFIER)| . "") 
      (:RELATION . "efficacy") 
      (:context . ,context)
      (:REFERENCE . ,(cdr (assoc :reference parse)))
      (:NOTE . ,source-sentence)
      (:MOLECULE 
       (:STATE . ,(subseq (cdr (assoc :state (second (assoc :molecule parse)))) 11)) 
       (:NAME . ,(subseq (cdr (assoc :name (second (assoc :molecule parse)))) 2)) 
       (:CLASS . "DNA")) 
      (:HYPOTHESIS 
       (:CONDITION . ,(concatenate 'string (subseq (cdr (assoc :name (second (assoc :molecule parse)))) 2) 
				   " " (subseq (cdr (assoc :state (second (assoc :molecule parse)))) 11))) 
       (:RELATION . "efficacy") 
       (:CANCER . ,(find-cancertype (cdr (assoc :cancer parse)))) 
       (:DRUG . ,drug))
      (:CASES . ,(force-to-be-a-number (cdr (assoc :cases parse)) 1))
      (:*MODIFIER . "") 
      (:MODEL . ,(generate-model-score (cdr (assoc :model parse))))
      (:CONDITION . ,(concatenate 'string (subseq (cdr (assoc :name (second (assoc :molecule parse)))) 2) " " (subseq (cdr (assoc :state (second (assoc :molecule parse)))) 11))) 
      (:COLLABORATIONS "TTD") 
      (:H . ,(generate-hyp-score (cdr (assoc :relation parse)))) 
      (:DRUG . ,drug) 
      (:INSIGHT-LABEL . ,trex-id) 
      (:RELATIONSHIP-TO . ,(translate-relation (cdr (assoc :relation parse)))) 
      (:REF--URL . ,(cdr (assoc :reference parse)))
      (:entry-utime . ,(get-universal-time))
      ,(list :vetting-history)
      )))

(defun failed-parse ()
  "<html>
      <head>
        <meta http-equiv=\"refresh\" content=\"3; url=trexui\" />
      </head>
      <body>
        <h1>Parse failed. Redirecting back to TrEx in 3 seconds...</h1>
      </body>
    </html>"
  )

;;; If N isn't any sort of number, then we force it to be one.

(defun force-to-be-a-number (n default)
  (cond ((numberp n) n)
	((stringp n) (force-to-be-a-number (ignore-errors (read-from-string n)) default))
	(t default)))

(defun generate-trex-id ()
  (format nil "TTD-~a-~a" (get-universal-time) (random 1000))) ;; FFF

(defun find-cancertype (cancer-info)
  (loop for item in cancer-info 
	when (and (stringp item) (search "CANCERTYPE." item) (= 0 (search "CANCERTYPE." item))) 
	do (return-from find-cancertype (subseq item 11))
	)
  )

(defun translate-relation (relation)
  (cond ((null relation) nil)
	((string-equal relation "sensitivity_to") "sensitivity to")
	((string-equal relation "resistance_to") "resistance to")
	((string-equal relation "no_sensitivity") "no relationship with")
	(t nil)
	))

;; parser will need a build to include animal/human/xenograft/meta-analysis 
(defun generate-model-score (model)
  (cond ((null model) 5) ;; WWW FFF Default is a bad idea here! FFF NEED A "RANDOM FACTOID" Model number!
	((string-equal model "in_vitro") 1)
	((string-equal model "in_vivo") 2)
	((string-equal model "case_study") 5)
	((string-equal model "trial") 5)
	((string-equal model "rct") 6)
	(t 5) ;; WWW FFF Default is a bad idea here! FFF NEED A "RANDOM FACTOID" Model number!
	))

(defun generate-hyp-score (relation)
  (cond ((null relation) 0)
	((string-equal relation "sensitivity_to") 1)
	((string-equal relation "resistance_to") -1)
	((string-equal relation "no_sensitivity") 0)
	(t (error "In generate-hyp-score got relation = ~a" relation))
	))

(defun seev (type key)
  (cl-who:with-html-output-to-string
   (*standard-output* nil :prologue t)
   (:html
    (header t "TrEx Evidience")
    (format t "<table border=1>~%")
    (cond ((string-equal type "test")
	   (emit-evidence-header)
	   (let ((test (cdr (assoc (ignore-errors (parse-integer key)) *test-keys-alist*))))
	     (loop for rec in *trexrecs*
		   when (string-equal test (cdr (assoc :condition rec)))
		   do (emit-table-row-from-trexrec rec))))
	  ((string-equal type "drug")
	   (emit-evidence-header)
	   (let ((drug (cdr (assoc (ignore-errors (parse-integer key)) *drug-keys-alist*))))
	     (loop for rec in *trexrecs*
		   when (string-equal drug (cdr (assoc :drug rec)))
		   do (emit-table-row-from-trexrec rec))))
	  (t "Type must be 'test' or 'drug'")
	  )
    (format t "</table>~%")
    (footer t))))

(defparameter *trexrec-cols-for-display*
  '((:--id . "ID") (:CANCER . "Diagnosis") (:CONDITION . "Condition") (:RELATION . "Relation") (:RELATIONSHIP-TO . "Relation To")
    (:DRUG . "Drug") (:REFERENCE . "Source") (:MODEL . "Model") (:CASES . "cases") (:NOTE . "Note")))

(defun emit-evidence-header ()
  (loop with r = "<tr>"
	as (nil . label) in *trexrec-cols-for-display*
	do (setf r (format nil "~a<th bgcolor=#f0f0f0>~a</th>" r label))
	finally (format t "~a</tr>" r)))

(defparameter *model-text*
  "
1. animal, in vitro (e.g., murine melanoma cell line)
2. animal, in vivo (e.g., syngeneic murine melanoma model)
3. human in vitro (e.g., human melanoma cell line)
4. human xenograft (e.g., human melanoma xenogeneic model)
5. clinical study/non-randomized clinical trial
6. randomized controlled trial
7. meta-analysis of clinical trials/studies
")

(defparameter *model-code->model-alist*
  '((1 . "animal in vitro") 
    (2 . "animal in vivo") 
    (3 . "human in vitro")
    (4 . "human xenograft")
    (5 . "clinical series") 
    (6 . "controlled trial") 
    (7 . "meta analysis")))

(defun emit-table-row-from-trexrec (rec)
  (loop with r = "<tr>"
	as (key . nil) in  *trexrec-cols-for-display*
	as val = (cdr (assoc key rec))
	;; Confusingly this is sometimes actually sa URL and sometimes
	;; just the name of a paper. For correct output we need to
	;; distinguish these cases.
	as url = (cdr (assoc :ref--url rec))
	as id = (cdr (assoc :--id rec))
	as http-url? = (when url (equal 0 (search "http" url :test #'char-equal))) ;; looks for either http or https!
	do 
	(setf r 
	      (format nil "~a~%<td>~a</td>~%" 
		      r 
		      (if val
			  (case key
				(:reference 
				 (if http-url?
				     (format nil "<a target=_blank href=~a>~a</a>~%" url val)
				   (format nil "<a target=_blank href=\"https://scholar.google.com/scholar?hl=en&q=~a\">~a</a>~%"
					   val val)))
				(:--id
				 (format nil "<a href=~a/trexrec?format=json&id=~a>~a</a>"
					 (host) id val))
				(:note 				 
				 (if http-url?
				     (format nil "<a target=_blank href=~a>~a</a>~%" url val)
				   (format nil "<a href=~a/seecase?dbfile=~a target=_blank>~a</a>&nbsp;&nbsp;&nbsp;
                            <a id=\"scholarLink\" href=\"https://scholar.google.com/scholar?hl=en&q=~a\" target=\"_blank\">[Search it on Google Scholar]</a>~%" 
					   (host) 
					   (cdr (assoc :context rec)) 
					   val val)))
				(:model (cdr (assoc val *model-code->model-alist*)))
				(t val))
			"--")))
	finally (format t "~a~%</tr>~%" r)))

(defun trexranker (observations) 
  (loop for rank in 
	(rank (recode-observations observations) *drugs*)
	collect `(("drug" . ,(drug-name (drug-info-drug rank)))
		  ("overallscore" . ,(drug-info-overall-score rank))
		  ("standard-error" . ,(drug-info-os-standard-error rank))
		  ("variance" . ,(drug-info-os-variance rank))
		  ("ci-upper" . ,(drug-info-os-ci-upper rank))
		  ("ci-lower" . ,(drug-info-os-ci-lower rank))
		  ("p" . ,(drug-info-p rank))
		  ("z" . ,(drug-info-z rank)))))

(defun recode-observations (observations) 
  (loop for (key val) in observations
	collect (cons (cdr (assoc key *test-keys-alist*)) val)))

(defun generalized-evidence-lookup (key)
  (list (recs-matching-molecule (string-split key :delimiter #\space))
	(gethash key *name->drug*)))
   
(defun current-user (user-id)
  (if (assoc user-id *user-ids* :test #'string-equal) ;; (string-equal avoids package gubbish)
      (cl-who:with-html-output-to-string ;; user-id exists
       (*standard-output* nil :prologue t)
       (:html
	(header t "TrEx -- The Treatment Explorer")
	(:body
	 (display-test-ranking (radio-states-to-stats-input :display-link? nil))
	 (display-user-tests)
	 (format t "<p><hr><form action=trexresetdb><p><input type=\"submit\" value=\"Reset db\" background=\"#ee2906\"></form>")
	 (:p)
	 (footer t)
	 )))
    (cl-who:with-html-output-to-string ;; user-id exists
     (*standard-output* nil :prologue t)
     (:html
      (header t "TrEx -- THe Treatment Explorer")
      (:body 
       :style "margin: 20px"
       (:p)
       (:p (format t "Your user-id is no longer in the system. Please reload the page and start fresh."))
       (footer t)
       )))))

(defun new-user ()
  (let ((temp-id (intern (string (gensym "USERID")))))
    (cl-who:with-html-output-to-string
     (*standard-output* nil :prologue t)
     (:html
      (header t "TrEx -- The Treatment Explorer")
      (:body
       (display-new-tests temp-id)
       (trex-instructions)
       (format t "<p><hr><form action=trexresetdb><p><input type=\"submit\" value=\"Reset db\" background=\"#ee2906\"></form>")
       (footer t)
       )))))

(defun trex-instructions ()
  (format t "
<p>Select:<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Positive/Aberrant/Active/Expressed/Mutated/etc. indicates that the biomarker/mutation is present and active in the patient<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Negative/Normal/Inactive/Unexpressed/Unmutated/etc. indicates that the biomarker/mutation is not present and is inactive in the patient (i.e. normal)<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Untested/Unknown/Inconclusive/etc. indicates that the result of the biomarker/mutation status is not known (i.e. inconclusive test or untested) in the patient<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;All tests default to Untested/Unknown/Inconclusive/etc. This should not be changed unless the specific biomarker/mutation status is explicitly known to be activated or inactivated.</p>
<p>
<p>Tests whose result is <em>normal</em> should be marked as Negative/Normal/Inactive/Unexpressed/etc.
<p>
<p>
For example, if your test for the \"BRAF D595G\" mutation came back showing it was present (active),
your test for \"ERK phosphorylation\" came back showing it was not present (inactive), and your
test for \"CRAF expression\" came back inconclusive, you would select:
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;BRAF mut D595G: Positive/Active/etc.,
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ERK phosphorylation: Negative/Inactive/etc.,
<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;and leave all other biomarkers (including CRAF expression): Untested/Unknown/etc.
<p>
"))

(defun split (s d)
  (mapcar #'(lambda (s) (string-trim " ,	" s)) 
	  (string-split s :delimiter d)))

;;; ====================================================================
;;; Ranking

#| Notes from the ruby code, with additional, more recent comments in "lisp": ;;;...

Prevalent hypothesis computation:
-------------------

Relation type = efficacy / toxicity / synergism

Hypothesis [hyp] = 1 if associated with increase in [relation type], -1 if associated with decrease, 0 if no effect
(This is already computed in the :H column: (cdr (assoc :h rec)))

"Condition" = drug/molecule/state(relation type)

These are computed from a single row of the input spreadsheet, so could be precomputed:
 - Model score [MS] = case(model) of 6,12,24,48,96,192,384 (i.e., 6*2^model)
 - Size score [SS] = cases/10
 - Adjusted size score [SSa] = 1 if size score=0, else size score
 - Evidence score [ES] = MS * SSa

For each Condition matching item:
 - Sensitivity score [SeS] = ES if hyp=1, else 0
 - Resistance score [ReS] = ES if hyp=-1, else 0
 - Null score [NuS] = ES if hyp=0, else 0

 - For all following, i is minus, null, plus [m, n, p] (-1, 0, 1)

 - Evidence score(plus) [ESp] = sum of plus scores
 - Evidence score(minus) [ESm] = sum of minus scores
 - Evidence score(null) [ESn] = sum of null scores

 - Total score [TS] = abs(ESp) + abs(ESm) + abs(ESn)

 - Score percentage [SPi] = ESi / TS (i in p, m, n (i.e., calc for each evidence score))

 - Agresti-Coull method corrected scores:
   - Corrected evidence score [ESic]  = evidence score + (1.96^2 / 2)
   - Corrected total score [TSc] = total score + (1.96^2)
   - Corrected score percentage [SPic] = ESic / TSc

 - Variance [Vi] = (SPic * (1 - abs(SPic))) / abs(TSc)
 - Standard error [SEi] = sqrt(Vi)
 - Score percentage 95% confidence interval limits:
   - upper [CLui] = SPic + (1.96 * SEi)
   - lower [CLli] = SPic - (1.96 * SEi)

 - Z score:
   - Zi = (SPi - criterion) / SEi [note: NOT corrected SP, but SE uses corrected version!]

"Prevalent" flag = 1 if Z score > criterion (default is 0.5) for either plus or minus effect, 0 if
 none of the three above 0.5, or null > 0.5 (Z score = (score percentage - 0.5) / standard error)
 - PH = (Zs > criterion) || (Zr > criterion) || (Zn !> 0.5)

-------------------
Following the process described above, we have a set of hypotheses, each corresponding to a unique
set of [Condition, drug].
Profile matching (for one drug):
-------------------

Patient results are a set of [Condition, concordance] pairs, where concordance is 1, 0, or -1
depending on whether each Condition test result was positive, inconclusive (or missing), or negative.
Results with a concordance of 0 can be discarded.
Relevant hypotheses are first grouped by drug. For each hypothesis being considered:
Weight = 1 / abs(Vj) [where j indexes a [molecule, state] pairing that matches one of the results supplied]
Score percentage for profile matching is multiplied by concordance (this is where the zero results disappear), so:
Wj * SPj = Weight * profile matching score percentage, with sign determined by concordance or discordance with
the prevalent hypothesis (multiply by -1 for discordance).
Overall score [OS] =  sum of W*SP values across molecules / sum of weights across molecules
Overall score variance [Vo] = 1 / sum of Wi's
Overall score standard error [SEo] = sqrt(Vo)
OS 95% confidence interval upper limit = OS + (1.96 * SEo)
OS 95% confidence interval lower limit = OS - (1.96 * SEo)
If overall score > threshold and 95% confidence interval does not cross threshold (CIlower > threshold),
then profile is associated with sensitivity / resistance
Z score = (OS - criterion) / SEo
P value = 2 * (1 - (cumulative distribution function(abs(Z))))

-------------------
Drug ranking:
-------------------
For each pair of drugs A & B:
Standard error(A - B) = sqrt(OS variance(A) + OS variance(B))
Z = (Overall(A) - Overall(B)) / Standard error(A - B)

-------------------
- Match molecule & state (i.e., Condition)
- For matching facts, run prevalent hypothesis computation
- Throw out molecules that don't meet criterion for prevalent hypothesis
- For remaining molecules, run profile matching Z score, discard that don't meet criterion
- For remaining drugs, compute & sort by P value
=end

|#

;;; Precalculation of hypotheses

(defun recval (key rec) (cdr (assoc key rec)))

#|

The ruby code (https://github.com/jeffshrager/tta/blame/master/rmmmp/app/models/hypothesis.rb)
contains this extremely obscure helping fn:

def self.unpack-results(results)
   rhash = {}
   results.split(',').collect 
      {|r| rhash[r.slice(0..-2)] = case r.slice(-1..-1); when 'p' then 1; when 'm' then -1; else 0; end}
   return rhash
end

WTF?! Okay, so here's the story: In the rails app, the results is a
giant list of all of the settings for all of the possible tests that
are relevant to the selected drugs. In the rails code, these are given
special keys, but we just use their names strings for the moment. (FFF
We should probably assign these keys as well.) Anyway, so each test
can be "concordant" (positive: p), "discordant" (negative: minus: m),
or unspecified (else: 0), so what you end up with is a table of -1, 0,
or 1 for each test relevant to the drugs in the drug set. (In the
rails app all tests had one of these three settings.)

Here this function isn't used, we input the conditions as a list of
strings that are straight from the factiods, along with a -1, 0, or 1, as:

  '(("condition a" . 1) ("condition b" . -1) ...)

|#

(defstruct (drug-info (:print-function 
		       (lambda (o s k)
			 (declare (ignorable k))
			 (format s "[DI:~a(~a) os=~a sw=~a p=~a z=~a Hyps=~s]"
				 (drug-name (drug-info-drug o)) (drug-info-condition o)
				 (drug-info-overall-score o) (drug-info-sum-weights o)
				 (drug-info-p o) (drug-info-z o)
				 (drug-info-hypotheses o)))))
  condition drug hypotheses
  (overall-score 0.0) (sum-weights 0.0) (sum-weight-times-sp 0.0)
  (os-standard-error 0.0) (os-variance 0.0) (os-ci-upper 0.0) 
  (os-ci-lower 0.0) (p 0.0) (z 0.0))

;(defvar *dbg* nil) ;; for debugging
(defparameter *debugging-target-drug-names* '("sorafenib" "dexamethasone"))

(defun drugs->r/c/d+hyps (&optional (drugs *debugging-target-drug-names*) &key (relation :efficacy))
  (loop for r/c/d being the hash-keys of *r/c/d->hyp*
	using (hash-value hyp)
	when (and (eq relation (first r/c/d)) 
		  (member (drug-name (third r/c/d)) drugs :test #'string-equal))
	collect (list r/c/d hyp)))

(defun rank (reported-conditions drugs &key (relation :efficacy) (threshold 0.5) &aux results)
; (setf *dbg* nil)
  ;; The drug info blocks are precomputed and get pulled from the *r/c/d->hyp* table.
  ;; For each drug we create a drug info block for each drug x reported condition (e.g.: (:efficacy "plx4032" "braf mut v600e")
  (let ((dis (loop for drug in drugs
		   as di = (make-drug-info :drug drug 
					   :hypotheses (remove nil ;; ??? Why would there be a nil ??
							       (mapcar #'(lambda (condition) 
									   (gethash (list relation condition drug) *r/c/d->hyp*))
								       (mapcar #'car reported-conditions))))
		   collect di)))
    (loop for di in dis
	  do
	  (loop for h in (drug-info-hypotheses di)
		;; (These are all taken apart for debugging.)  
		as hc = (hypothesis-condition h) 
		;; As "braf mut v600e" The reported-conditions here
		;; are, e.g.: '(("KIT mut V560A (exon 11)" . +1)
		;; ("MGMT active (high activity)" . -1)) [Obviously
		;; the +1 is just 1 -- I sometimes put +'s in for
		;; emphasis.] So CS will end up with -1, 0, or +1, and 0
		;; if there's no setting.
		as cs = (or (cdr (assoc hc reported-conditions :test #'string-equal)) 0)
		;; This is the KEY to the hypothesis, which is also
		;; -1, 0, or +1
		as hpcs = (hypothesis-prevalent-calc-set-key-is-1/0/1 h)
		;; RC is the concordance. It's -1,0, or 1, which will
		;; below control what gets summed. The left of the *
		;; (cs) is -1, 0, or 1 for the selected hypothesis
		;; condition, and right of the * (hpcs) is also -1 0 1
		;; based on which was the prevalent hypothesis. When
		;; these match, you'll get either a -1 or +1, and when
		;; they don't, you'll get a 0.
		as rc = (* cs hpcs) ;; ???? Shouldn't this be a test, or something?
		as prevalent-calc-set = (hypothesis-prevalent-calc-set h)
		do 
		(incf (drug-info-sum-weight-times-sp di)
		      (* rc (calc-set-weight-times-sp prevalent-calc-set)))
		(incf (drug-info-sum-weights di)
		      (calc-set-weight prevalent-calc-set))
		;; DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
;		(when (member (drug-name (drug-info-drug di)) *debugging-target-drug-names* :test #'string-equal) 
;		  (push `(:key AAAAAAAAAA :di ,di :hc ,hc :cs ,cs :hpcs ,hpcs :rc ,rc :pcs ,prevalent-calc-set 
;			  :drug-info-sum-weight-times-sp ,(drug-info-sum-weight-times-sp di))
;			*dbg*))
		)
	  ;; DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
;	  (when (member (drug-name (drug-info-drug di)) *debugging-target-drug-names* :test #'string-equal) 
;	    (push `(:key BBBBBBBBB :di ,di :di-sw*sp ,(drug-info-sum-weight-times-sp di)) *dbg*))
	  ;; Concordance statistics (with respect to current context)
	  ;;(when (not (zerop (drug-info-sum-weights di))) ;; Trying this a different way, to drop drugs with no observartions
	  (when (not (zerop (drug-info-sum-weight-times-sp di))) ;; using ...times-sp instead of the sum weights bcs the former includes the observations that are non-zero
	    (setf (drug-info-overall-score di)
		  (/ (drug-info-sum-weight-times-sp di) 
		     (drug-info-sum-weights di)))
	    (setf (drug-info-os-variance di)
		  (/ 1.0 (drug-info-sum-weights di)))
	    (setf (drug-info-os-standard-error di)
		  (sqrt (drug-info-os-variance di)))
	    (let ((os-confidence-interval (* 1.96 (drug-info-os-standard-error di))))
	      (setf (drug-info-os-ci-upper di)
		    (+ (drug-info-overall-score di) os-confidence-interval))
	      (setf (drug-info-os-ci-lower di)
		    (- (drug-info-overall-score di) os-confidence-interval))
	      )
	    (setf (drug-info-z di)
		  (/ (- (abs (drug-info-overall-score di)) threshold)
		     (drug-info-os-standard-error di)))
	    (setf (drug-info-p di)
		  (* 2.0 (- 1.0 (statistics::phi (abs (drug-info-z di))))))
	    (push di results)
	    )))
  (sort results #'> :key #'drug-info-overall-score)
  )

;;; *** WWW FFF This should convert z to p, but since we just need
;;; relative values, we just leave it as z scores.

(defun GSL-Cdf-gaussian-P (z) z)

;;   def z(threshold)
;;     (self.score-percentage - threshold) / self.standard-error
;;   end

;;   def p(z)
;;     2.0 * (1.0 - GSL::Cdf::gaussian-P(z.abs))
;;   end

;;; ====================================================================
;;; Load database and pre-computate ranking statistics

(defparameter *string-to-relation-alist* 
  '(("sensitivity to" . :EFFICACY)
    ("no relationship with" . :EFFICACY)
    ("resistance to" . :EFFICACY)
    ("toxicity increased for" . :TOXICITY)
    ("toxicity unchanged for" . :TOXICITY)
    ("toxicity decreased for" . :TOXICITY)
    ("synergism with" . :SYNERGISM)
    ("no synergism with" . :SYNERGISM)
    ("antagonism with" . :SYNERGISM)
    ("progression favored by" . :PROGRESSION)
    ("progression" . :PROGRESSION)
    ("efficacy" . :EFFICACY)
    ("toxicity" . :TOXICITY)
    ("synergism" . :SYNERGISM)
    ))

(defun initialize ()
  (format t "Initializing...~%")
  (import-trex-recs)
  (clrhash *name->drug*)
  (clrhash *r/c/d->hyp*)
  (loop for rec in *trexrecs*
	as counter from 0 by 1
	as fact-id = (recval :--ID rec)
	as source = (recval :source rec)
	as h = (recval :h rec)
	as evidence-type = (recval :model rec)
	as reference = (recval :reference rec)
	as drug-name = (string-downcase (recval :drug rec)) ;; FFF Ontologize!
	as relationship = (recval :relationship-to rec)
	as relation = (string-to-relation (recval :relation rec))
	as cases = (recval :cases rec)
	as notes = (recval :note rec)
	as moldata = (recval :molecule rec)
	as molecule = (cdr (assoc :name moldata))
	as molecule-state = (cdr (assoc :state moldata))
	as condition = (format nil "~a ~a" molecule molecule-state)
        as drug = (or (gethash drug-name *name->drug*)
		      (setf (gethash drug-name *name->drug*) 
			    (make-drug :name drug-name)))
	as hypkey = (list relation condition drug)
        as hypothesis = (or (gethash hypkey *r/c/d->hyp*)
			    (setf (gethash hypkey *r/c/d->hyp*)
				  (make-hypothesis :relation relation :condition condition :drug drug)))
        as factoid = (make-factoid :fact-id fact-id 
				   :drug drug
				   :source source 
				   :molecule molecule
				   :molecule-state molecule-state
				   :relationship relationship
				   :condition condition
				   :drug-name drug-name 
				   :model evidence-type 
				   :h h
				   :cases cases 
				   :reference reference 
				   :notes notes
				   :relation relation 
				   :hypothesis hypothesis
				   )
	do 
	(push factoid (hypothesis-factoids hypothesis))
	(push factoid (gethash relation (drug-relation->factoids drug)))
	(pushnew hypothesis (drug-hypotheses drug))
	)
  ;; Gather drug list, removing any drug that doesn't have any
  ;; clinical (>= model 5) evidence at all. FFF Combine these steps.
  (setf *drugs* 
	(loop for key being the hash-keys of *name->drug*
	      collect (gethash key *name->drug*)))
  (setf *drugs* 
	(loop for drug in *drugs*
	      as name = (drug-name drug)
	      if (> (loop for rec in *trexrecs*
			  when (and (string-equal name (cdr (assoc :drug rec)))
				    (>= (cdr (assoc :model rec)) 5))
			  sum 1) 0)
	      collect drug))
  (setf *tests* (remdups (collect-conditions)))
  ;; FFF UUU These lists are hacks for REST communications with the
  ;; outside world. They are a fairly ugly hack, and should be
  ;; pointers to the objects, not to the names of the objects UUU
  (setf *test-keys-alist* 
	(loop for test in *tests*
	      as key from 1 by 1
	      collect (cons key test))) 
  (setf *drug-keys-alist* 
	(loop for drug-name being the hash-keys of *name->drug*
	      as key from 1 by 1
	      collect (cons key drug-name)))
  (loop for relation in *relations* 
	do (summarize-hypothesis-evidence relation))
  (format t "Trex initialization complete!~%")
  (log! :trex-reinitialized "(:drug-names ~a) (:r/c/d-entries ~a)" (hash-table-count *name->drug*) (hash-table-count *r/c/d->hyp*))
  )

(defvar *insights.json-path* "insights.json")

(defun import-trex-recs ()
  (unless *trexrecs*
    (format t "Imported ~a records from insights.json~%"
	    (length
	     (setq *trexrecs*
		   (with-open-file 
		    (i *insights.json-path*)
		    (loop for rec = (ignore-errors (cl-json::decode-json-from-source i))
			  until (null rec)
			  collect rec)))))))

;;; Gather tests that make sense, excluding "uncharacterized", and any
;;; that ONLY have "no relationship to" as their relationship(s), and
;;; which have clinical support (model >= 5). (There's sort of a
;;; left-hand right-hand problem here bcs you could have to refilter
;;; drugs based on tests, etc. ??? FFF WWW)

(defvar *condition->recs* (make-hash-table :test #'equal))

(defun collect-conditions ()
  (clrhash *condition->recs*)
  (loop for rec in *trexrecs*
	when (member (cdr (assoc :drug rec)) *drugs* :test #'(lambda (a b) (string-equal a (drug-name b))))
	do (push rec (gethash (cdr (assoc :condition rec)) *condition->recs*)))
  (remhash "uncharacterized" *condition->recs*)
  (sort 
    (loop for condition being the hash-keys of *condition->recs*
	  using (hash-value recs)
	  when (loop for rec in recs
		     when (and (not (string-equal "no relationship with" (cdr (assoc :RELATIONSHIP-TO rec))))
			       (>= (cdr (assoc :model rec)) 5))
		     do (return t))
	  collect condition)
    #'(lambda (a b) (string< (string-downcase a) (string-downcase b)))))

(defun output-individual-tests ()
  (loop for test in *tests*
	do 
	(format t 
		"<tr>
             <td><a target=_blank href=trexseev?type=test&key=~a>~a</a></td>
      <td>Positive: <input type=\"radio\" name=\"~a\" value=\"Positive\"></td>
      <td>Negative: <input type=\"radio\" name=\"~a\" value=\"Negative\"></td>
      <td>Unknown: <input type=\"radio\" name=\"~a\" value=\"Untested\" checked></td></tr>
      " 
		(car (find test *test-keys-alist* :key #'cdr :test #'string-equal))
		test test test test
		)))

(defun output-prefilled-tests ()
  (loop for test in (cdr (assoc (intern (hunchentoot::compute-parameter "user-id" 'string :both)) *user-ids*))
	as name = (car test)
	as val = (cdr test)
	as v1 = (if (= 1 val) "checked" "")
	as v2 = (if (= -1 val) "checked" "")
	as v3 = (if (= 0 val) "checked" "")
	do 
	(format t 
             "
             <tr>
             <td><a target=_blank href=trexseev?type=test&key=~a>~a</a></td>
             <td>Positive: <input type=\"radio\" name=\"~a\" value=\"Positive\" ~a></td>
             <td>Negative: <input type=\"radio\" name=\"~a\" value=\"Negative\" ~a></td>
             <td>Unknown: <input type=\"radio\" name=\"~a\" value=\"Untested\" ~a></td>
             </tr>
             "
	     (car (find name *test-keys-alist* :key #'cdr :test #'string-equal))
	     name name v1 name v2 name v3
	     )
	))

;;; BBB!!! Somehow there's a nil getting into the test set, so we
;;; remove it...but I don't know why it's in there to begin with!
;;; BBBBBBBBBBBBBBBBBBBB!!!!!!!!!!!!!!!!FFFFFFFFFFFFFFFFFFFFFF

(defun remove-replace (id states)
  ;(setq states (remove nil states)) ;; !!!!!!!!!!!!!!!!!!!!
  (let ((entry (assoc id *user-ids*)))
  	  (if entry
  	  	(setf (cdr entry) states)
  	  	(push (cons id states) *user-ids*))))


(defun display-new-tests (temp-id)
  (format t "<hr><h3>Test results (see instructions below):</h3><br><form action=/trexui>
<input type=\"submit\" value=\"Rank drugs\" background=\"#B9DFFF\">")
  (format t "
<input type=\"hidden\" name=\"user-id\" value=\"~a\"> 
<table border=1>
<tr>
<th bgcolor=#f0f0f0>Test</th>
<th bgcolor=#f0f0f0>Positive (Aberrant/Active/Expressed/Mutated)</th>
<th bgcolor=#f0f0f0>Negative (Inactive/Unexpressed/Unmutated)</th>
<th bgcolor=#f0f0f0>Untested (Unknown/Inconclusive)</th>
</tr>
    <p>
    "
	  temp-id)
  (update-user-info temp-id (radio-states-to-stats-input :display-link? nil))
  (output-individual-tests)
  (format t "</table>
<input type=\"submit\" value=\"Rank drugs\" background=\"#B9DFFF\">
</form>"
	  ))

(defun display-user-tests ()
  (let* ((temp-id (intern (hunchentoot::compute-parameter "user-id" 'string :both))))
    (format t "<hr><h3>Test results:</h3>
<form action=/trexui>
<input type=\"submit\" value=\"Rank drugs\" background=\"#B9DFFF\">")
    (format t "
<input type=\"hidden\" name=\"user-id\" value=\"~a\"> 
<table border=1>
<tr>
<th bgcolor=#f0f0f0>Test</th>
<th bgcolor=#f0f0f0>Positive (Aberrant/Active/Expressed/Mutated)</th>
<th bgcolor=#f0f0f0>Negative (Inactive/Unexpressed/Unmutated)</th>
<th bgcolor=#f0f0f0>Untested (Unknown/Inconclusive)</th>
</tr>
    <p>
    "
	    temp-id)
    (update-user-info temp-id (radio-states-to-stats-input :display-link? t))
    (output-prefilled-tests)
    (format t "</table>
<input type=\"submit\" value=\"Rank drugs\" background=\"#B9DFFF\">
</form>"
	    )))

(defun update-user-info (user-id test-states)
  (remove-replace user-id test-states))

(defun string-to-relation (s)
  (or (cdr (assoc s *string-to-relation-alist* :test #'string-equal))
      (error "In STRING-TO-RELATION, can't find relation: ~s" s))  )

(defun summarize-hypothesis-evidence (relation &aux hypotheses)
  (format t "Summarizing facts for ~a..." relation)
  (loop for drug being the hash-values of *name->drug*
	;; using (hash-key drug-name) -- not used, I think?
	as relfacts = (gethash relation (drug-relation->factoids drug))
	do 
	(loop for f in relfacts
	      as h = (factoid-hypothesis f)
	      as model-score = (* 6.0 (expt 2.0 (- (factoid-model f) 1.0)))
	      ;; If no cases given, default 1... if this right??? 
	      as size-score = (max 1.0 (/ (or (factoid-cases f) 1) 10.0))
	      as evidence-score = (* model-score size-score)
	      as calc-set = (cdr (assoc (factoid-h f) (hypothesis-calc-sets h)))
	      do 
	      (push h hypotheses)
	      (incf (calc-set-evidence-score calc-set) evidence-score)
	      (incf (hypothesis-total-score h) (abs evidence-score))
	      ))
  (mapcar #'initialize-hypotheses hypotheses)
  (format t "~a hypotheses initialized.~%" (length hypotheses)))

(defparameter i96^2 (expt 1.96 2))

(defun initialize-hypotheses (h)
  (let* ((h-total-score (hypothesis-total-score h)))
    (setf (hypothesis-corrected-total-score h) (+ h-total-score i96^2))
    (mapcar #'(lambda (calc-set)
		(set-calc-set-static-stats (cdr calc-set) h-total-score (hypothesis-corrected-total-score h)))
	    (hypothesis-calc-sets h))
    ;; The "prevelant hypothesis" is mis-named; it's actually a
    ;; calc-set within a hypothesis. (??? What happens if there's
    ;; more than one > 0.5 ??? Maybe this can't happen?  FFF Should
    ;; put in an error test for this condtion. At the moment we just
    ;; take the first one it comes to that's > 0.5)
    (let ((prevelant-calc-set
	   (loop for (key . c) in (hypothesis-calc-sets h)
		when (>= (calc-set-corrected-score-percentage c) 0.5) ;; >= bcs sometimes it's exact 0.5 and f's up!
		do 
		(setf (hypothesis-prevalent-calc-set-key-is-1/0/1 h) key)
		(return c))))
      ;; There often won't be a prevalant calc set bcs there are no
      ;; facts at all, so don't flag, just ignore!
      (when prevelant-calc-set 
	(setf (hypothesis-prevalent-calc-set h) prevelant-calc-set)
	(setf (hypothesis-prevalent-percentage h)
	      (calc-set-corrected-score-percentage prevelant-calc-set))))))

;;; Work out the detailed precomputation statistics for each calc
;;; set. These get done only once, at intit time.

(defun set-calc-set-static-stats (calc-set total-score corrected-total-score)
    (setf (calc-set-score-percentage calc-set)
	  (/ (calc-set-evidence-score calc-set) total-score))
    (setf (calc-set-corrected-evidence-score calc-set) 
	  (+ (calc-set-evidence-score calc-set) (/ (expt 1.96 2) 2.0)))
    (setf (calc-set-corrected-score-percentage calc-set)
	  (/ (calc-set-corrected-evidence-score calc-set) corrected-total-score))
    (setf (calc-set-variance calc-set)
	  (/ 
	   (* (abs (calc-set-corrected-score-percentage calc-set))
	      (- 1.0 (abs (calc-set-corrected-score-percentage calc-set))))
	   corrected-total-score))
    (setf (calc-set-standard-error calc-set) 
	  (sqrt (abs (calc-set-variance calc-set))))
    (setf (calc-set-confidence-interval calc-set) 
	  (* 1.96 (calc-set-standard-error calc-set)))
    (setf (calc-set-confidence-interval-upper calc-set)
	  (+ (calc-set-corrected-score-percentage calc-set) (calc-set-confidence-interval calc-set)))
    (setf (calc-set-confidence-interval-lower calc-set)
	  (- (calc-set-corrected-score-percentage calc-set) (calc-set-confidence-interval calc-set)))
    (setf (calc-set-weight calc-set) 
	  (/ 1.0 (abs (calc-set-variance calc-set))))
    (setf (calc-set-weight-times-sp calc-set)
	  (* (calc-set-weight calc-set) (calc-set-score-percentage calc-set)))
    )

;;; ====================================================================
;;; Testing and Execution

(defun display-test-ranking (conds)
  (let ((ranks (rank conds *drugs*)))
    (when ranks
      (format t "<hr><h3>Rankings (by overall score):</h3><br>~%
      <em>WARNING: THIS TOOL IS FOR DEMOSTRATION, RESEARCH, AND EDUCATIONAL PURPOSES ONLY. IT IS NOT TO BE USED IN ANY CLINICAL SETTING</em>~%")
      (format t "<table border=1><tr><th bgcolor=#f0f0f0>Treatment</th><th bgcolor=#f0f0f0>[lower CI, score, upper CI]</th><th bgcolor=#f0f0f0>z score</th><th bgcolor=#f0f0f0>serr</th><th bgcolor=#f0f0f0>p</th></tr>")
      (mapcar #'(lambda (di) 
		  (format t "<tr>
             <td><a target=_blank href=trexseev?type=drug&key=~a>~a</a></td>
<td><font color=~a>[~$, ~$, ~$]</font></td><td>~a</td><td>~a</td><td>~a</td></tr>"
			  (car (find (drug-name (drug-info-drug di))
				     *drug-keys-alist* 
				     :key #'cdr :test #'string-equal))
			  (drug-name (drug-info-drug di))
			  (if (> (drug-info-overall-score di) 0.0) 
			      "green" 
			    "red")
			  (drug-info-os-ci-lower di)
			  (drug-info-overall-score di)
			  (drug-info-os-ci-upper di)
			  (drug-info-z di)
			  (drug-info-os-standard-error di)
			  (drug-info-p di)
			  ))
	      ranks)
      (format t "</table>")
      )))


;; ========================================================================
;; Experimentation
(defvar *experiment-entry* nil)
(defvar *expt-api-arg* nil)
(defvar *experiment-file-name* "plx4032-braf-")
(defvar *number-entries* 5)
(defvar *insights-expts.json-path* "resources/insights/insights-expts.json")

(setf *experiment-entry* 
  '((:--ID . "TTD-0000") 
    (:CANCER . "Melanoma") 
    (:|*ALIAS (MODIFIER)| . "") 
    (:RELATION . "efficacy") 
    (:REFERENCE . "Flaherty KT N Engl J Med 2010 363:809-19") ; note commas removed for csv printing
    (:NOTE . "Treatment of metastatic melanoma with PLX4032 in patients with tumors that carry the V600E BRAF mutation resulted in complete or partial tumor regression in the majority of patients") 
    (:MOLECULE (:STATE . "mut V600E") (:NAME . "BRAF") (:CLASS . "DNA")) 
    (:HYPOTHESIS (:CONDITION . "BRAF mut V600E") (:RELATION . "efficacy") (:CANCER . "Melanoma") (:DRUG . "PLX4032")) 
    (:CASES . 1) 
    (:*MODIFIER . "") 
    (:MODEL . 5) 
    (:CONDITION . "BRAF mut V600E") 
    (:COLLABORATIONS "TTD") 
    (:H . 1) 
    (:DRUG . "PLX4032") 
    (:INSIGHT-LABEL . "TTD-1385") 
    (:RELATIONSHIP-TO . "sensitivity to") 
    (:REF--URL . "https://www.ncbi.nlm.nih.gov/pubmed/20818844"))
  )

(defun expt-initialize ()
  (format t "Initializing...~%")
  (expt-import-trex-recs)
  (clrhash *name->drug*)
  (clrhash *r/c/d->hyp*)
  (loop for rec in *trexrecs*
  as counter from 0 by 1
  as fact-id = (recval :--ID rec)
  as source = (recval :source rec)
  as h = (recval :h rec)
  as evidence-type = (recval :model rec)
  as reference = (recval :reference rec)
  as drug-name = (string-downcase (recval :drug rec)) ;; FFF Ontologize!
  as relationship = (recval :relationship-to rec)
  as relation = (string-to-relation (recval :relation rec))
  as cases = (recval :cases rec)
  as notes = (recval :note rec)
  as moldata = (recval :molecule rec)
  as molecule = (cdr (assoc :name moldata))
  as molecule-state = (cdr (assoc :state moldata))
  as condition = (format nil "~a ~a" molecule molecule-state)
        as drug = (or (gethash drug-name *name->drug*)
          (setf (gethash drug-name *name->drug*) 
          (make-drug :name drug-name)))
  as hypkey = (list relation condition drug)
        as hypothesis = (or (gethash hypkey *r/c/d->hyp*)
          (setf (gethash hypkey *r/c/d->hyp*)
          (make-hypothesis :relation relation :condition condition :drug drug)))
        as factoid = (make-factoid :fact-id fact-id 
           :drug drug
           :source source 
           :molecule molecule
           :molecule-state molecule-state
           :relationship relationship
           :condition condition
           :drug-name drug-name 
           :model evidence-type 
           :h h
           :cases cases 
           :reference reference 
           :notes notes
           :relation relation 
           :hypothesis hypothesis
           )
  do 
  (push factoid (hypothesis-factoids hypothesis))
  (push factoid (gethash relation (drug-relation->factoids drug)))
  (pushnew hypothesis (drug-hypotheses drug))
  )
  ;; Gather drug list, removing any drug that doesn't have any
  ;; clinical (>= model 5) evidence at all. FFF Combine these steps.
  (setf *drugs* 
  (loop for key being the hash-keys of *name->drug*
        collect (gethash key *name->drug*)))
  (setf *drugs* 
  (loop for drug in *drugs*
        as name = (drug-name drug)
        if (> (loop for rec in *trexrecs*
        when (and (string-equal name (cdr (assoc :drug rec)))
            (>= (cdr (assoc :model rec)) 5))
        sum 1) 0)
        collect drug))
  (setf *tests* (remdups (collect-conditions)))
  ;; FFF UUU These lists are hacks for REST communications with the
  ;; outside world. They are a fairly ugly hack, and should be
  ;; pointers to the objects, not to the names of the objects UUU
  (setf *test-keys-alist* 
  (loop for test in *tests*
        as key from 1 by 1
        collect (cons key test))) 
  (setf *drug-keys-alist* 
  (loop for drug-name being the hash-keys of *name->drug*
        as key from 1 by 1
        collect (cons key drug-name)))
  (loop for relation in *relations* 
  do (summarize-hypothesis-evidence relation))
  (format t "Trex initialization complete!~%")
  (log! :trex-reinitialized "(:drug-names ~a) (:r/c/d-entries ~a)" (hash-table-count *name->drug*) (hash-table-count *r/c/d->hyp*))
  )

(defun expt-trexranker (observations) 
  (loop for rank in 
  (rank (recode-observations observations) *drugs*)
  collect `((:drug . ,(drug-name (drug-info-drug rank)))
      (:overallscore . ,(drug-info-overall-score rank))
      (:standard-error . ,(drug-info-os-standard-error rank))
      (:variance . ,(drug-info-os-variance rank))
      (:ci-upper . ,(drug-info-os-ci-upper rank))
      (:ci-lower . ,(drug-info-os-ci-lower rank))
      (:p . ,(drug-info-p rank))
      (:z . ,(drug-info-z rank)))))

(defun expt-import-trex-recs ()
    (format t "Imported ~a records from insights-expts.json~%"
      (length
       (setq *trexrecs*
       (with-open-file 
        (i *insights-expts.json-path*)
        (loop for rec = (ignore-errors (cl-json::decode-json-from-source i))
        until (null rec)
        collect rec))))))

(defun experiment-runner ()
  (let* ((filename (concatenate 'string "experiments/" *experiment-file-name* (write-to-string (get-universal-time)) ".csv")))
    (setf *expt-api-arg*
      (loop 
        for i from 1 to (length *tests*)
        for test in *tests*
        collect (list i (if (string-equal test (cdr (assoc :condition *experiment-entry*))) 1 0)))) 
    (loop for x from 0 to *number-entries*
      do
      (if (> x 0) (add-entry-to-insights *experiment-entry*))
      (expt-initialize)
      (let* ((results (expt-trexranker *expt-api-arg*)))
        (record-experiment-results results filename x))
      ))
  (clean-up *insights-expts.json-path* (- (length *trexrecs*) (- *number-entries* 1)) *number-entries*)
  )

(defun add-entry-to-insights (entry)
  (with-open-file (o *insights-expts.json-path* :direction :output :if-exists :append)
    (cl-json:encode-json entry o)
    (terpri o)))

(defun record-experiment-results (results filename entry-number)
  (loop for drug in results
    when (string-equal (cdr (first drug)) "plx4032")
    do
    (with-open-file (o filename :direction :output :if-does-not-exist :create :if-exists :append)
      (if (= entry-number 0)
        (format o "number of entries, z, variance, standard-error, ci-upper, overall-score, ci-lower, p, entry-added:, ~a, total number of times added:, ~a~%" 
          *experiment-entry* *number-entries*)) ; need to change entry-number
      (format o "~a, ~a, ~a, ~a, ~a, ~a, ~a, ~a~%"
        entry-number
        (cdr (assoc :z drug))
        (cdr (assoc :variance drug))
        (cdr (assoc :standard-error drug))
        (cdr (assoc :ci-upper drug))
        (cdr (assoc :overallscore drug))
        (cdr (assoc :ci-lower drug))
        (coerce (cdr (assoc :p drug)) 'single-float))
      )))

(defun clean-up (filename start num)
  (let* ((tmp-filename  (concatenate 'string filename ".tmp"))
  (lines-omitted 0))
    ;; Open a temp file to write the result to
    (with-open-file (out tmp-filename
       :direction :output
       :if-exists :supersede
       :if-does-not-exist :create)
      ;; Open the original file for reading
      (with-open-file (in filename)
        (loop
           for line = (read-line in nil 'eof)
           for i from 1
           until (eql line 'eof)
           ;; Write the line to temp file if it is not in the omitted range
           do (if (or (< i start)
                (>= i (+ start num)))
          (write-line line out)
          (setf lines-omitted (1+ lines-omitted))))))
          ;; Swap in the temp file for the original
          (delete-file filename)
          (rename-file tmp-filename "insights-expts.json") ; currently failing...
          ;; Warn if line removal went past the end of the file
          (when (< lines-omitted num)
            (warn "End of file reached with only ~d lines removed." lines-omitted))
  ))



