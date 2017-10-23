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


(in-package :quark)

;; to dos:
;;  update the previous kickout for the mutation source to go to either the url or just simple scholar search.... 

(defstruct (quark-xscript (:type list) :named (:conc-name qxs-))
  file mtgid mtgdesc scribeemail xscript parse
  (utime (get-universal-time))
  (version *kp-version-utime*))

(defun init-quark-server ()
  (hunchentoot:define-easy-handler 
   (quark :uri "/quark")
   nil
   (qscribeui))
  (hunchentoot:define-easy-handler 
   (xsave :uri "/xsave")
   (mtgid mtgdesc scribeemail xscript vet)
   (save-xscript (new-db-file) mtgid mtgdesc scribeemail xscript vet)
   (format nil "<meta http-equiv=\"refresh\" content=\"1; url=~a/vetui\" />"
	   (host)))
  (hunchentoot:define-easy-handler
   (seecase :uri "/seecase")
   (dbfile)
   (see-case dbfile))
  (hunchentoot:define-easy-handler 
   (vetui :uri "/vetui") 
   nil
   (vetuir))
  (hunchentoot:define-easy-handler 
   (vetvote :uri "/vetvote") 
   (uid)
   (vetvoter uid))
  )

(defun see-case (file)
  (cl-who:with-html-output-to-string
   (*standard-output* nil :prologue t)
   (with-open-file 
    (i file)
    (let ((r (read i)))
      (format t "<h2>Meeting ~a (~a) ~s~%<br>~%Contact:~a~%</h2><hr><hr>"
	      (qxs-mtgid r) (pretty-ts (qxs-utime r))
	      (qxs-mtgdesc r) (craiglistify (qxs-scribeemail r))
	      )
      (loop for char across (qxs-xscript r)
	    do (if (char-equal #\^M char)
		   (princ "<br>")
		 (princ char)))))))

(defvar *craigslistified-email->real-email* (make-hash-table :test #'equal))

(defun craiglistify (real-email)
  (let ((new-email (format nil "~a~5,'0d@cancercommons.org" (get-universal-time) (random 10000))))
    (setf (gethash new-email *craigslistified-email->real-email*) real-email)
    new-email))

(defvar *unvetted-insights* nil)

(defvar *uid->cids* (make-hash-table :test #'equal))

(defparameter *default-comment* "Anonymous comment if desired (NO PHI, PLEASE!)")

(defun vetvoter (uid)
  (loop for (kid . cid) in (gethash uid *uid->cids*)
	as new-vh = (cons (cons :vetime (get-universal-time))
		       (loop for (key . sfx) in '((:agree . "_a") (:general . "_g")
						  (:evidence . "_e") (:import . "_i")
						  (:comment . "_c"))
			     collect (cons key 
					   (let ((result (hunchentoot::compute-parameter (format nil "~a~a" cid sfx) 'string :both)))
					     (if (string-equal result *default-comment*) nil result)))))
	as insight = (find kid *unvetted-insights* :test #'(lambda (a b) (string-equal a (cdr (assoc :--id b)))))
	do (push new-vh (cdr (assoc :vetting-history insight))))
  (remhash uid *uid->cids*) ;; Things are totally confused if you don't do this!
  (move-validated-insights-if-any-into-trexrecs-and-reinit-trex)
  (cl-who:with-html-output-to-string
   (*standard-output* nil :prologue t)
   (format t "<meta http-equiv=\"refresh\" content=\"1; url=~a/trexui\" />" (host)))
  )

;;; Decide whether a ke needs to be vetted.

(defparameter *n-agreements-to-pass* 2)
(defparameter *n-disagreements-to-reject* 2)

(defun accepted? (rec)
  (>= (loop for vhelt in (cdr (assoc :vetting-history rec))
	    when (string-equal "agree" (cdr (assoc :agree vhelt)))
	    sum 1)
      *n-agreements-to-pass*))
  
(defun rejected? (rec)
  (>= (loop for vhelt in (cdr (assoc :vetting-history rec))
	    when (string-equal "disagree" (cdr (assoc :agree vhelt)))
	    sum 1)
      *n-disagreements-to-reject*))
  
;;; Vetting UI

(defun vetuir ()
  (cl-who:with-html-output-to-string
   (*standard-output* nil :prologue t)
   (header t "Peer Review DashBoard")
(format t "
All vetting input is anonymous. If you want to be contacted, you'll have to put your email, phone, etc. in a comment.<p>
(~a 'Agrees' to accpet; ~a 'Disgrees' to reject.)
<em></em>
" *n-agreements-to-pass* *n-disagreements-to-reject*)
   (emit-vetting-uis)
   (footer t)
   ))

(defun emit-vetting-uis ()
  (let* ((uid (string (gensym "UID"))))
    (format t "<form action=vetvote><table>")
    (loop for rec in *unvetted-insights*
	  as kid = (cdr (assoc :--id rec))
	  as file = (second (assoc :registry-file rec))
	  do (format t "<tr><td><table>~%")
	  (if file
	      (format t "<em><font size=5><a target=_blank href=/seecase?dbfile=~a>~a</a></font></em>&nbsp;&nbsp;&nbsp;<a id=\"scholarLink\" href=\"https://scholar.google.com/scholar?hl=en&q=~a\" target=\"_blank\">Search on Google Scholar</a>" 
		      file (second (assoc :sentence rec)) (second (assoc :sentence rec)))
	    (format t "<em><font size=5>~a</font></em>&nbsp;&nbsp;&nbsp;<a id=\"scholarLink\" href=\"https://scholar.google.com/scholar?hl=en&q=~a\" target=\"_blank\">[Search it on Google Scholar]</a>"
       (second (assoc :sentence rec)) (second (assoc :sentence rec)))
	    )
	  (emit-vetting-prompts uid kid)
	  (format t "</table></td></tr>~%")
	  )
    (format t "
</table>
<p><input type=hidden name=uid value=\"~a\">
<input type=submit></form>
" uid)))

(Defun emit-vetting-prompts (uid kid)
  (let* ((cid (format nil "~a-~a" uid kid))) ;; combined id
    (push (cons kid cid) (gethash uid *uid->cids*))
    (format t "<tr>
<th>Agreement</th><th>Generality</th><th>Evidence</th><th>Importance</th></tr>
<tr>
<td>
<input type=\"radio\" name=\"~a_a\" value=\"disagree\">Disagree
<input type=\"radio\" name=\"~a_a\" value=\"dontknow\">Neutral
<input type=\"radio\" name=\"~a_a\" value=\"agree\">Agree
</td>
<td>
<input type=\"radio\" name=\"~a_g\" value=\"specific\">Specific
<input type=\"radio\" name=\"~a_g\" value=\"neutral\">Neutral
<input type=\"radio\" name=\"~a_g\" value=\"general\">General
</td>
<td>
<input type=\"radio\" name=\"~a_e\" value=\"weak\">Weak
<input type=\"radio\" name=\"~a_e\" value=\"medium\">Medium
<input type=\"radio\" name=\"~a_e\" value=\"strong\">Strong
</td>
<td>
<input type=\"radio\" name=\"~a_i\" value=\"less\">Less
<input type=\"radio\" name=\"~a_i\" value=\"medium\">Medium
<input type=\"radio\" name=\"~a_i\" value=\"very\">Very
</td>
</tr>
<tr><td colspan=4><input type=text size=150 value=~s name=~a_c></td></tr>
" cid cid cid cid cid cid cid cid cid cid cid cid *default-comment* cid
)))

;;; Analyst UI

(defun qscribeui ()
  (cl-who:with-html-output-to-string
   (*standard-output* nil :prologue t)
   (header t "Quark -- Case and Insight Capture")
   (tooltip-style t)
   (format t "
<form action=xsave onsubmit=\"return confirm('Do you really want to submit the form?');\">
Header info (<em>DO NOT INCLUDE PHI!</em>):<br>
<table border=1>
<tr>
<td>Meeting ID: <input type=text name=mtgid id=mtgid size=20><br></td>
<td>Analyst's ID (or email): <input type=text size=80 name=scribeemail id=scribeemail><br></td></tr><tr>
<td colspan=2>Meeting description: <input type=text size=120 name=mtgdesc id=mtgdesc><br></div></td>
</tr>
</table>
<p>
<hr>
<p>
<em>DO NOT INCLUDE PHI</em>")
   (cheatsheet t)
   (format t "
<button type=\"button\" id=\"newpt\">New Case</button>
<button type=\"button\" id=\"addtr\">Add TR</button>
<br>
<br>
<textarea rows=40 cols=150 name=xscript id=xscript>
*EIF2AK3-alk fusion in NSCLC confers sensitivity to ceritinib (Won, Mambetsariev, and Salgia, BMC Cancer. 2016 Aug 2;16:568)

*Combination BRAF plus MEK inhibitor therapy confers increased efficacy as compared to BRAF inhibitor monotherapy in mice with BRAFV600E-positive glioma (https://www.ncbi.nlm.nih.gov/pubmed/27848137)

*Ceritinib is efficacious in a patient with EIF2AK3-ALK fusion-positive NSCLC with acquired resistance to crizotinib (https://www.ncbi.nlm.nih.gov/pubmed/27480287)

*Loss of function mutations in JAK1 and JAK2 are associated with acquired resistance to PD-1 immunotherapy in patients with mismatch repair deficient, high mutational load colorectal cancer (PMID:27903500)

*In a study of patients receiving autologous stem cell transplant for mantle cell non hodgkin's lymphoma rituximab 
maintenance resulted in prolonged overall survival S Le Gouill , C Thieblemont, L Oberic et al. N Engl J Med 377;13:1250-1260; 2017)


*In a randomized controlled trial with 200 patients with stage 2 metastatic melanoma (Shrager & Tenenbaum, 2014), EGFR expression leads to resistance to cisplatin.
*EGFR expression leads to sensitivity to dexamethasone in a randomized controlled trial with 100 patients with stage 2 metastatic melanoma.
*EGFR expression confers sensitivity to sorafenib (Mocellin, Shrager, et al. 2010).
*EGFR amplification confers resistance to dexamethasone. (Tenenbaum and Shrager, 2011)
</textarea>
<p>
<style>
button.save-continue{color: green;}
button.save-close{color: red;}
input[type=submit] {
    background:#ff66a3;
}
</style>
<input type=hidden name=vet id=vet value=\"t\">
<table>
<tr>
<td><input type=submit class=\"save-close\" value=\"Save and Close\"></td>
<td><button type=\"button\" class=\"save-continue\" id=\"save-continue\">Save And Continue</button></td>
<td><div id=\"save-time\"><em>UNSAVED</em></div></td>
</table>
</form>
<p>
<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js\"></script>
<script>
var myInterval;
$(document).ready(function(){
       $('#save-continue').click(function(){
         clearInterval(myInterval);
         var mtgid = document.getElementById(\"mtgid\").value;
         var mtgdesc = document.getElementById(\"mtgdesc\").value;
         var scribeemail = document.getElementById(\"scribeemail\").value;
         var xscript = document.getElementById(\"xscript\").value;
         var vet = \"nil\";
         var now = new Date();
         document.getElementById(\"save-time\").innerHTML = \"Last saved at: \".concat(now);
         $.post('/xsave', {mtgid: mtgid, mtgdesc: mtgdesc, scribeemail: scribeemail, xscript: xscript, vet: vet});
         myInterval = setTimeout(arguments.callee, 30000);
         });
       });

var myTextArea = $('#xscript');
$(document).ready(function(){
       $('#newpt').click(function(){
            myTextArea.val(myTextArea.val() + '\\r=============================\\rxx yo m/f; dx:xx in 20xx; hist: XX in 20XX, ...; currently: tumor load/location/stage/etc; \\rHypotheses: ..., \\rQuestions: ...\\r\\rDiscussion: (use the ADD TR button to frame treatment rationales)\\r\\r');
         });
       });

$(document).ready(function(){
       $('#addtr').click(function(){
            myTextArea.val(myTextArea.val() + '\\r\\r*braf v6003e mutation confers resistance to/sensitivity to vemurafenib (Smith, 2016) \\r\\r');
         });
       });
</script>
")
(footer t)
))

(defun tooltip-style (o)
  (format o "
<style>
/* Tooltip container */
.tooltip {
    position: relative;
    display: inline-block;
    border-bottom: 1px dotted black; /* If you want dots under the hoverable text */
}

/* Tooltip text */
.tooltip .tooltiptext {
    visibility: hidden;
    width: 1000px;
    background-color: #ccddff;
    color: black;
    text-align: left;
    padding: 5px 0;
    border-radius: 6px;
 
    /* Position the tooltip text - see examples below! */
    position: absolute;
    z-index: 1;
}

/* Show the tooltip text when you mouse over the tooltip container */
.tooltip:hover .tooltiptext {
    visibility: visible;
}</style>"))

(defun cheatsheet (o)
  (format o "
<div class=\"tooltip\">[Cheatsheet]
<span class=\"tooltiptext\">
<em>DO NOT INCLUDE PHI</em>
<p>
Annotation Key:<br>
<ul>
@speaker (can appear anywhere)<br>
<p>
[insight] <br>
*insight (if the * is in column 1 and the insight is the whole line, you don't need the [brackets] from above)<br>
<p>
/note (the whole line is considered a note)<br>
<p>
A multi-line insight or note should either be bracketed, or have * or / at the head of each line.
<p>
======================= (a line beginning with any number of only equal signs) <br>
indicates a break in topic, usually moving on to a new case. (The next line probably should
describe the new topic, for example, giving a brief description of the case.<br>
(Use the [New Case] button, Luke!)
</ul>
</span>
</div>
"))

(defun save-xscript (file mtgid mtgdesc scribeemail xscript vet)
  (with-open-file
   (o (print file) :direction :output)
   (pprint (make-quark-xscript :file file
	    :mtgid mtgid :mtgdesc mtgdesc 
			 :scribeemail scribeemail :xscript xscript
						  :parse (parse-xscript xscript file vet))

	   o)))

(defvar *db-base* "db/")

(defun new-db-file ()
  ;; Every time the thread starts ccl need a new random state!
  (setf *random-state* (make-random-state t))
  (let ((dir (db-dir-for-today)))
    (ensure-directories-exist dir)
    (loop as file = (print (format nil "~a/~a~5,'0d" dir (get-universal-time) (print (random 10000))))
	  until (null (probe-file file))
	  finally (return file))))

(defun db-dir-for-today ()
  (format nil "~a~a" *db-base* (yyyymmdd)))

(defun yyyymmdd ()
  (multiple-value-bind
   (second minute hour date month year day daylight-p zone)
   (decode-universal-time (get-universal-time))
   (declare (ignorable second minute hour date month year day daylight-p zone))
   (format nil "~a~2,'0d~2,'0d" year month date)))

(defun parse-xscript (xs file vet)
  (let ((*print-circle* nil))
    (with-input-from-string 
     (i xs)
     (loop for line = (read-line i nil nil)
	   until (null line)
	   collect (let* ((line (string-trim " 	
" line)))
		     (when (not (zerop (length line)))
		       (case (aref line 0)
			     (#\* (progn (if (string-equal "t" vet) (submit-insight-for-vetting (subseq line 1) file))
					 (list line (nlp::drive-parser (subseq line 1) :format :lisp))))
			     (t line))))))))

(defvar *insights-that-dont-parse* nil) ;; FFF !!! We don't do anything with these at the moment!

(defun submit-insight-for-vetting (sentence file)
  ;; save and continue works alone (clicking mutliple times does not grow unvetted-insights)
  ;; save and close works alone
  ;; when one is clicked and then the other... it fails (duplications occur)
  (let ((parse (nlp::drive-parser sentence)))
    (if (search "[\"_PARSEFAILED" parse)
	(push sentence *insights-that-dont-parse*)
      (let ((combined-insight 
	     (append `((:sentence ,sentence) (:registry-file ,file))
		     (trex::convert-parse-to-trex-alist (cl-json:decode-json-from-source parse) :source-sentence sentence :context file))))
	(if (not (member sentence (loop for item in *unvetted-insights* collect (car (cdr (assoc :sentence item)))) 
			 :test #'lenient-string-equal))
	    (push combined-insight *unvetted-insights*))
	(log! :submit-insight-for-vetting "~s" combined-insight) ;; FFF log the insight even if duplicate for now, this the right way to go about it?
	))))

(defun move-validated-insights-if-any-into-trexrecs-and-reinit-trex ()
  (let* ((accepted-insights
	  (loop for insight in *unvetted-insights*
		when (accepted? insight)
		collect insight
		when (rejected? insight)
		do (reject-insight insight)
		)))
    (when accepted-insights
      (mapcar #'accept-insight accepted-insights)
      (trex::initialize))))

(defun reject-insight (insight)
  (setf *unvetted-insights* (remove insight *unvetted-insights*))
  (log! :reject-insight "~s" insight))

(defun accept-insight (insight) ;; for novel drugs, this will not work for the trex ranker?
  (push insight trex::*trexrecs*)
  (setf *unvetted-insights* (remove insight *unvetted-insights*))
  (log! :accept-insight "~s" insight))
