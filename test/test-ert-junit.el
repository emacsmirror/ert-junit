;;; test-ert-junit.el --- ERT tests for ert-junit    -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ola Nilsson

;; Author: Ola Nilsson <ola.nilsson@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert-junit)
(require 'xml)

;; Introduced in Emacs 25.1
(unless (require 'dom nil t)
  ;; these defuns are not as robust as those in dom.el, but should
  ;; work well enough.
  (defun dom-tag (node)
	"Return the NODE tag."
	(car node))
  (defun dom-attributes (node)
	"Return the NODE attributes."
	(cadr node))
  (defun dom-children (node)
	"Return the NODE children."
	(cddr node))
  )

(defun test-ert-junit-xml2dom (xmlstring)
  "Parse XMLSTRING and return a dom object."
  (let ((dom (with-temp-buffer
			   (insert xmlstring)
			   ;; TODO: This test is broken
			   (condition-case nil
				   (libxml-parse-xml-region 1 (point-max))
				 (void-function (xml-parse-region))))))
	;; Emacs 24.5 and earlier wraps the result in another list for
	;; some reason
	(when (listp (car dom))
	  (setq dom (car dom)))
	dom))

(defun test-ert-junit-normalize-dom (dom)
  "Normalize a DOM.
Sort the attribute list by attribute name and remove any child
nodes that are pure whitespace strings."
  (let ((node (list (dom-tag dom)
					(cl-sort (copy-sequence (dom-attributes dom))
							 'string-lessp :key 'car)))
		(children
		 (cl-loop for child in (dom-children dom)
				  if (stringp child)
				    if (not (string-match "^\\s-*$" child))
				      collect child
				    end
				  else
				  collect (test-ert-junit-normalize-dom child))))
	(when children
	  (setcdr (cdr node) children))
	node))

(defmacro should-equal-normalized (d1 d2)
  "Check that normalized doms D1 and D2 are `equal'.
Normalization is done by `test-ert-junit-normalize-dom'."
  `(should (equal (test-ert-junit-normalize-dom ,d1) (test-ert-junit-normalize-dom ,d2))))

(ert-deftest test-ert-junit--infos-string-1 ()
  :tags '(ert-junit--infos-string)
  (ert-info ("Calling ert-junit--infos-string with bad argument")
    (should-error (ert-junit--infos-string nil) :type 'wrong-type-argument))
  (let* ((result-with-condition (make-ert-test-result-with-condition :condition '(= 1 2)
																	 :backtrace "backtrace"
																	 :infos '()))
		 (infostring (ert-junit--infos-string result-with-condition)))
	(should (string= "" infostring)))
  (let* ((result-with-condition (make-ert-test-result-with-condition :condition '(= 1 2)
																	 :backtrace "backtrace"
																	 :infos '(("Info: " . "message"))))
		 (infostring (ert-junit--infos-string result-with-condition)))
	(should (string= "Info: message\n" infostring)))
  )

(ert-deftest test-ert-junit--condition-string-1 ()
  :tags '(ert-junit--condition-string)
  (ert-info ("Calling ert-junit--condition-string with bad argument")
    (should-error (ert-junit--condition-string nil) :type 'wrong-type-argument))
  (let ((result-with-condition (make-ert-test-result-with-condition :condition '(= 1 2)
																	:backtrace "backtrace"
																	:infos '())))
	(should (string= "(= 1 2)\n" (ert-junit--condition-string result-with-condition))))
  )

(ert-deftest test-ert-junit-testcase-1 ()
  :tags '(ert-junit-testcase)
  (should-error (ert-junit-testcase nil nil nil))
  (let* ((t1 (make-ert-test :name 't1 :body (lambda () nil)))
		 (stats (ert--make-stats (list t1) 't)))
	(should-error (ert-junit-testcase stats nil nil))

	(ert--stats-set-test-and-result stats 0 t1 (make-ert-test-passed))
	(setf (aref (ert--stats-test-start-times stats) 0) '(0 0 0 0))
	(setf (aref (ert--stats-test-end-times stats) 0) '(0 0 0 0))


	(should-equal-normalized
	 '(testcase ((name . "t1")
				 (classname . "ert")
				 (time . "0.000000")))
	 (test-ert-junit-xml2dom (ert-junit-testcase stats "t1" 0)))))

(ert-deftest test-ert-junit-testcase-2 ()
  :tags '(ert-junit-testcase)
  (let* ((t1 (make-ert-test :name 't1 :body (lambda () nil)))
		 (t2 (make-ert-test :name 't2 :body (lambda () nil)))
		 (stats (ert--make-stats (list t1 t2) 't)))
	(ert--stats-set-test-and-result stats 0 t1 (make-ert-test-failed :condition '(= 1 2)
																	 :backtrace ""
																	 :infos '()))
	(setf (aref (ert--stats-test-start-times stats) 0) '(0 0 0 0))
	(setf (aref (ert--stats-test-end-times stats) 0) '(0 0 0 0))

	(ert--stats-set-test-and-result stats 1 t2 (make-ert-test-quit :condition '(error "Foo")
																   :backtrace ""
																   :infos '()))
	(setf (aref (ert--stats-test-start-times stats) 1) '(0 0 0 0))
	(setf (aref (ert--stats-test-end-times stats) 1) '(0 0 0 0))

	(let* ((output (ert-junit-testcase stats "t1" 0))
		   (testcase (test-ert-junit-xml2dom output))
		   (expected `(testcase ((name . "t1")
								 (classname . "ert")
								 (time . "0.000000"))
								(failure
								 ((message . "test")
								  (type . "type"))))))
	  (should-equal-normalized expected testcase))

	(should-equal-normalized '(testcase ((name . "t2")
										 (classname . "ert")
										 (time . "0.000000"))
										(failure () "quit"))
				   			 (test-ert-junit-xml2dom (ert-junit-testcase stats "t2" 1)))))

(provide 'test-ert-junit)
;;; test-ert-junit.el ends here
