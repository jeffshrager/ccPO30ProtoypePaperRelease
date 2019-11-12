(in-package :ncit)

;;; Loads from the .txt version of the NCI Thesaurus which comes from Thesaurus_<version>.FLAT.zip

(defstruct (obj) cid cname priority types isas names common-name raw) ;; :name is the car of :names

;;; I assumed that the first was the most common name, although this is obviously NOT always true!

(defun objs->json (objs o)
  (format o "[~%")
  (loop for obj in objs
	as i from (length objs) by -1
	when obj
	do (format o "{\"cid\":\"~a\",\"cname\":\"~a\",\"types\":\"~a\",\"isas\":~a,\"names\":~a}"
		   (obj-cid obj) 
		   (obj-cname obj) 
		   (list-to-json-array (obj-types obj))
		   (list-to-json-array (obj-isas obj))
		   (list-to-json-array (obj-names obj))
		   )
	(unless (= i 1) (format o ",~%")))
  (format o "~%]~%")
  )

(defun list-to-json-array (l)
  (with-output-to-string (s) (cl-json::encode-json l s) s))

(defvar *allobjs* nil)

;;; Key Primary_Name Is_A All_Names Description Seems_To_Be_Empty MetaTypes Types

(defvar *cid->obj* (make-hash-table :test #'equal))
(defvar *type->objs* (make-hash-table :test #'equal))
(defvar *name->objs* (make-hash-table :test #'equal))
(defvar *isa->objs* (make-hash-table :test #'equal))

(defvar *ncitTXT-path* "resources/ncithesaurus/Thesaurus.txt")

;;; These are actually is-a's, that is, types, but bcs it's a
;;; hierarchy, each is just another id. These should be listed in the
;;; order in which you want them to be selected by the tagger, higher
;;; priority first.

(defparameter *known-ids*
  '(
    "Neoplastic Process" "Gene or Genome" "Amino Acid, Peptide, or Protein" "Disease or Syndrome" 
    "Pharmacologic Substance" "Finding" "Intellectual Product" 
    "Therapeutic or Preventive Procedure" 
    "Geographic Area" "Quantitative Concept" "Organic Chemical"
    "Laboratory Procedure" "Cell" "Anatomical Structure" "Qualitative Concept" 
    "Body Part, Organ, or Organ Component" "Functional Concept" "Cell or Molecular Dysfunction"
    "Spatial Concept" "Conceptual Entity" "Immunologic Factor" "Enzyme" "Bacterium"
    "Mammal" "Population Group" "Manufactured Object" "Plant" "Classification"
    "Temporal Concept" "Diagnostic Procedure" "Tissue" "Activity" "Indicator, Reagent, or Diagnostic Aid"
    "Event" "Research Activity" "Biomedical Occupation or Discipline" "Nucleic Acid, Nucleoside, or Nucleotide" 
    "Laboratory or Test Result" "Chemical Viewed Functionally" "Receptor" "Biomedical or Dental Material"
    "Hazardous or Poisonous Substance" "Health Care Related Organization" "Phenomenon or Process"
    "Biologically Active Substance" "Sign or Symptom" "Occupation or Discipline" "Obsolete_Concept"
    "Idea or Concept" "Professional or Occupational Group" "Antibiotic" "Pathologic Function" "Embryonic Structure"
    "Virus" "Substance" "Regulation or Law" "Health Care Activity" "Clinical Attribute"
    "Food" "Molecular Function" "Medical Device" "Fungus" "Mental or Behavioral Dysfunction"
    "Body Location or Region" "Header_Concept" "Inorganic Chemical" "Cell Component" "Human"
    "Organism Attribute" "Physiologic Function" "Organization" "Element, Ion, or Isotope" 
    "Governmental or Regulatory Activity" "Body Substance" "Amino Acid Sequence" "Congenital Abnormality" 
    "Molecular Biology Research Technique" "Cell Function" "Genetic Function" "Mental Process" 
    "Chemical Viewed Structurally" "Retired_Concept"))

(defun load-thesaurus (&key (force? nil))
  (when (or force? (null *allobjs*))
    (setq *allobjs* nil)
    (clrhash *cid->obj*)
    (clrhash *type->objs*)
    (clrhash *name->objs*)
    (clrhash *isa->objs*)
    (with-open-file 
     (i *ncitTXT-path*)
     (loop for line = (read-line i nil nil)
	   as k from 1 by 1
	   until (null line)
	   do 
	   ;;(when (zerop (mod k 10000)) (print line))
	   (let ((obj (line->obj line)))
	     (push obj *allobjs*)
	     (setf (gethash (obj-cid obj) *cid->obj*) obj)
	     (loop for type in (obj-types obj)
		   do (push obj (gethash type *type->objs*)))
	     (loop for isa in (obj-isas obj)
		   do (push obj (gethash isa *isa->objs*)))
	     (loop for name in (obj-names obj)
		   do 
		   (push obj (gethash name *name->objs*))
		   ;; Enable default match for various stupid cap'ings.
		   (push obj (gethash (string-downcase name) *name->objs*)))
	     )))
    )
  ;; Assign the highest priority to each object in accord with the *known-ids* order.
  (let ((known-id->priorities (make-hash-table :test #'equal)))
    (loop for id in *known-ids*
	  as i from 1 by 1
	  do (setf (gethash (keywordize id) known-id->priorities) i))
    (loop for cid being the hash-keys of *cid->obj*
	  using (hash-value obj)
	  ;; 1 is the highest priority, and larger numbers are lower
	  ;; priority.  (This makes the min'ing and max'ing a little
	  ;; confusing!)
	  as prilist = (remove nil (mapcar #'(lambda (type) (gethash type known-id->priorities)) (obj-types obj)))
	  when prilist 
	  do (setf (obj-priority obj) (reduce #'min prilist))
	  ))
  ;; Reporting
  (log! :nci-thesaurus-loaded
	:nobjs (length *allobjs*)
	:ntypes (hash-table-count *type->objs*)
	:nnames (hash-table-count *name->objs*)
	:nisas (hash-table-count *isa->objs*))
  )

(defun keywordize (string)
  (CHUNGA::MAKE-KEYWORD
   (string-upcase 
    (substitute #\_ #\space (substitute #\_ #\, string))) t))

(defvar *ncitxt-keys* '(:cid :concept-name :isa-cids :alt-names :desc :ignore :ignore :types))
(defun line->obj (line)
  (let ((raw (mapcar #'subsplit (string-split line :delimiter #\tab))))
    (labels 
     ((k (key) (nth (position key *ncitxt-keys*) raw)))
     (let ((cname (car (k :concept-name))))
     (make-obj :raw raw 
	       :cid (car (k :cid))
	       :cname cname
	       :common-name (or (cdr (assoc cname *cnames->common-names* :test #'string-equal)) cname)
	       :isas (k :isa-cids)
	       :names (k :alt-names)
	       :types (k :types))))))

(defun subsplit (l)
  (loop for entry in (string-split l :delimiter #\|)
	;; (We don't want to keywordize every last word or we'll flood the symbol table.)
	collect (or
		 (if-cid-keywordize-or-nil entry)
		 (if (member entry *known-ids* :test #'string-equal)
		     (keywordize entry))
		 entry)))

(defun if-cid-keywordize-or-nil (term)
  (if (symbolp term) (setf term (string term)))
  (and (> (length term) 3)
       (char-equal (aref term 0) #\c)
       (ignore-errors (parse-integer (subseq term 1)))
       (keywordize term)))

(defun exploretbl (tbl)
  (format t "Most common elements of ~a: ~%" tbl)
  (loop for i below 10 
	as (n . key) in 
	(sort (loop for key being the hash-keys of tbl
		    using (hash-value values)
		    collect (cons (length values) key))
	      #'> :key #'car)
	do (print (list n key))))

(defun init-ncit-server ()
  (load-thesaurus)
  (hunchentoot:define-easy-handler
   (ncit :uri "/ncit")
   (name)
   (setf (hunchentoot:content-type*) "text/plain")
   (with-output-to-string
     (s)
     (objs->json 
      (if (char-equal #\^ (aref name 0))
	  (children-of (subseq name 1))
	(term->objs name))
      s)))
  )

(defun children-of (name)
  (loop for obj in *allobjs*
	with target-cid = (obj-cid (car (term->objs name)))
	when (member target-cid (obj-isas obj) :test #'string-equal)
	collect obj))

(defun term->objs (term)
  (if (symbolp term) (setf term (string-downcase (string term))))
  (remdups
   (cond ((let ((kw (if-cid-keywordize-or-nil term)))
	    (when kw (list (gethash kw *cid->obj*)))))
	 ((or (search "pmid" term :test #'string-equal) (search "pubmed" term :test #'string-equal)) 
	  (list 
	   (let* ((n (extract-integer term))
		  (url (format nil "https://www.ncbi.nlm.nih.gov/pubmed/~a" n)))
	     (make-obj :cid url
		       :types '("NCBI")
		       ;; !!! Supressing actual data capture from PUBMED
		       ;; :names (list (drakma::http-request url))
		       ))))
	 (t 
	  (append (gethash term *name->objs*)
		  (gethash (string-downcase term) *name->objs*)
		  (gethash (string-upcase term) *name->objs*)
		  ))
	 )))

(defparameter *cnames->common-names*
  '(("BAY_43-9006" . "Sorafenib")
    ("plx4032" . "Vemurafenib")))

;; UUU Collect all the numerical chars together -- this is a
;; really horrible way of getting the pmid out without
;; having to know what the exact syntax is.

(defun extract-integer (s)
  (or 
   (ignore-errors 
    (parse-integer 
     (coerce 
      (loop for c across s
	    when (and (char>= c #\0) (char<= c #\9))
	    collect c)
      'string)))
   "INVALID_NUMBER"))

;;; Check the type of something anywhere up the hierarchy:

(defun is-ncit-type? (obj type)
  (cond
   ((null obj) nil)
   ((stringp obj) (is-ncit-type? (gethash obj *name->objs*) type))
   ((listp obj)
    (or (is-ncit-type? (car obj) type)
	(is-ncit-type? (cdr obj) type)))
   ((symbolp obj)
    (if (equal :ROOT_NODE obj) 
	nil
      (is-ncit-type? (term->objs obj) type)))
   ((typep obj 'obj) 
    (or (member type (obj-types obj))
	(is-ncit-type? (obj-isas obj) type)))
   ))
