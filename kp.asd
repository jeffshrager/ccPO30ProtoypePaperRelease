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

;;; Precreate packages to avoid messing with cross-module dependencies
(defpackage :utils (:use :common-lisp))
(defpackage :nlp (:use :common-lisp :utils))
(defpackage :ncit (:use :common-lisp :utils))
(defpackage :norman (:use :common-lisp :utils))
(defpackage :trex (:use :common-lisp :nlp :utils))
(defpackage :quark (:use :common-lisp :nlp :utils))
(defpackage :iic (:use :common-lisp :nlp :utils))
(defvar QUARK::*UNVETTED-INSIGHTS* nil)

(in-package :asdf)
(defsystem kp
  :depends-on (:cl-json :hunchentoot :cl-who :drakma :cxml)
  :components
  ((:module "utils"
    :components
    ((:file "utils")
     (:file "wordhomology")
     (:file "statslh")))
   (:module "ncit"
    :components
    ((:file "NCITXTThesaurusServer")))
   (:module "nlp" 
    :components
    ((:file "myparser")))
   (:module "norman" 
    :components
    ((:file "norman")))
   (:module "trex"
    :components
    ((:file "trex")))
   (:module "quark"
    :components
    ((:file "quark")))
   (:module "iic"
    :components
    ((:file "iic")))
   ))
(load-system :kp)

;;; =============== SERVER STARTUP =============== 

;;; Some day figure out how to put this into huncentoot's thread
;;; initialization:
;(bt:make-thread 
; (function your-thread-main) 
; :initial-bindings (acons '*random-state* (make-random-state t) 
;			  bt:*default-special-bindings*)) 

;;; Global settings
(setf *print-circle* t)
(setf *random-state* (make-random-state t))

(if (string-equal "/home/ubuntu" (CCL:GETENV "HOME"))
    (setf utils::*localhost?* nil)
    t)

;;; Setup logging
(setq utils::*kp-version-utime* (get-universal-time))
(utils::init-log)

;;; Since the Thesaurus provide the ontology, it has to be inialized first
(setf ncit::*ncitTXT-path* "resources/ncithesaurus/Thesaurus.txt")
(ncit::init-ncit-server)

;;; Start servers

(setf trex::*insights.json-path* "resources/insights/insights.json")

(norman::init-norman-server)
(trex::init-trex-server)
(nlp::init-parse-server :test? nil)
(quark::init-quark-server)
(iic::init-iic-server)

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4240))
(hunchentoot:start (make-instance 'hunchentoot:acceptor :port 4241 :document-root #p"docs"))

;;; Hang for everything to run
(loop do (sleep 100))
