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

(in-package :iic)

(defun init-iic-server ()
  (hunchentoot:define-easy-handler 
   (scribeui :uri "/iio")
   (page)
   (iic-home page))
  )

(defvar *page-length* 10)

(defun iic-home (page)
  (cl-who:with-html-output-to-string
   (*standard-output* nil :prologue t)
   (header t "<em>Insights in Oncology</em> -- A Nano-Journal")
   (format t "<form action=/iio>
<input type=\"hidden\" name=\"page\" value=\"0\">
<input type=\"submit\" value=\"&lt;&lt; First page\">
</form>
<table>
")
   (let* ((page (if page (parse-integer page) 0))
	  (skip (* *page-length* page))
	  )
     (loop for rec in (nthcdr skip  trex::*trexrecs*)
	   as i below *page-length*
	   do (trex::emit-table-row-from-trexrec rec)
	   )
     (format t "
</table>
<table>
<tr><td>
<form action=/iio>
<input type=\"hidden\" name=\"page\" value=\"0\">
<input type=\"submit\" value=\"&lt;&lt; First page\">
</form>
</td>
<td>
<form action=/iio>
<input type=\"hidden\" name=\"page\" value=\"~a\">
<input type=\"submit\" value=\"Next page &gt;\">
</form>
</td></tr>
</table>
" (1+ page))
     )
   (footer t)))
