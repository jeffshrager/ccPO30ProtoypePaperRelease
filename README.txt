;;;***************************************************************************
;;;* Copyright 2017-2019 by Cancer Commons and xCures                        *
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

Usage:

1. Start any standard-compliant Common Lisp (I mostly test in clozure cl)

   Notes for ccl:
      You'll want to add:
          alias lisp='/usr/local/src/ccl/scripts/ccl'
      to your .bashrc, but you may need to hack that script to
      start lx86cl64 instead of lx86cl in this line:
          *86*) OPENMCL_KERNEL=lx86cl64 ;;

2. Install quicklisp (https://www.quicklisp.org/beta/)
   You'll also want to create a .ccl-init.lisp that loads up ql.
   Mine says this:

	#-quicklisp
	(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
	(when (probe-file quicklisp-init)
	(load quicklisp-init)))
	(princ "(ql:quickload \"...\")")(terpri)
	(princ "(ql:system-apropos \"...\")")(terpri)
	(princ "--- Loaded quicklisp ---")(terpri)
	(ql:quickload "cl-who")
	(ql:quickload "cl-json")
	(ql:quickload "cxml")
	(ql:quickload "hunchentoot")
	(ql:quickload "drakma")

3. In kp.asd, make sure the ports are as you like (default 4240 and 4241)
   Also, you want to replace the string constant here:

(if (string-equal "REPLACE WITH WHATEVER YOUR CCL::GETENV RETURNS" (CCL:GETENV "HOME"))
    (setf utils::*localhost?* nil)
    t)

4. In utils/utils.lisp, you'll want to change these lines:

(defun host ()
  (if *localhost?* "http://localhost:4240" "http://trex.MYDOMAIN.COM:4240"))
(defun docs ()
  (if *localhost?* "http://localhost:4241" "http://trex.MYDOMAIN.COM:4241"))

to match your DNS record.

5. You'll need to pull down a current tab-deliminted flat file version of ther NCIT Thesaurus
   from here:

       https://evs.nci.nih.gov/evs-download/thesaurus-downloads
   or: https://evs.nci.nih.gov/ftp1/NCI_Thesaurus/

   into ../resources/ncithesaurus/

   and call it "Thesaurus.txt"

6. In your lisp:

   (load "kp.asd")

Once all the above has been tested, one can generally just start the
server by a command line, for example:

     ccl --load kp.asd

See your lisp's documentation for the specifics of this command.

7. Once it's all running, you should be able to go to:

     http://trex.MYDOMAIN.COM:4240/trexui

   and it'll just work :-)

8. If you want to leave a running server, you can do something like: 

        lisp --load kp.asd >> /dev/null 2>> /dev/null &

   or replace the /dev/null(s) with maybe a timestamped log file in a logs directory,
   or somesuch thing.
   
