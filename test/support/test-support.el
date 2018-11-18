;;; test-support.el --- JUnit test support  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ola Nilsson

;; Author: Ola Nilsson <ola.nilsson@gmail.com>
;; Maintainer: Ola Nilsson <ola.nilsson@gmail.com>
;; Created: Nov 16, 2018
;; Keywords: test

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

;; This file contains support functions and macros that are useful
;; when testing other code that generate JUnit XML data.  It was
;; created to support testing of
;; https://bitbucket.org/olanilsson/ert-junit and
;; https://bitbucket.org/olanilsson/buttercup-junit .

;; esxml is the form that is returned by such functions as
;; libxml-parse-xml-region and is used internally by Emacs in many xml
;; related libraries.

;;; Code:

(eval-and-compile (require 'cl))

;;; Functions to create expected esxml/dom structures

(defvar junit-timestamp-re
  "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [012][0-9]:[0-5][0-9]:[0-5][0-9]\\+[012][0-9][0-5][0-9]"
  "Regex that matches JUnit timestamps; YYY-MM-DD hh:mm::ss+hhmm.")

(defvar junit-default-class "buttercup"
 "Default value for the `class' attribute used by `testcase'.")

(defun test-support--remove-keys (rest-list &rest keys)
  "Remove all key-value pairs from REST-LIST for all KEYS.
Useful to remove &key arguments from a &rest argument in
`defun*'s and `defmacro*'s.

Example:
 (test-support--remove-keys '(1 :foo 2 3) :foo :bar)
 -> '(1 3)"
  (let (filtered elt)
    (while rest-list
      (setq elt (pop rest-list))
      (if (memq elt keys)
          (pop rest-list)
        (push elt filtered)))
    (nreverse filtered)))

(defun testsuites (&rest contains)
  "Return an esxml list for a `testsuites' tag.
CONTAINS should be the contained esxml lists."
  (declare (indent defun))
  (if contains
      `(testsuites nil ,@contains)
    '(testsuites nil)))

(defun* testsuite (name
                     &rest contains
                     &key (fail 0) (err 0) (skip 0) (tests (+ fail skip err))
                     (stamp junit-timestamp-re)
                     (host ".+") (time "[0-9]+\\.[0-9]+")
                     &allow-other-keys)
  "Return an esxml list for a testsuite tag.
NAME is the suite description.
FAIL is the number of failed testcases, default 0.
ERR is the number of testcases that threw an error, default 0.
SKIP is the number of skipped (pending) testcases, default 0.
TESTS is the total number of testcases, defaults to FAIL + ERR + SKIP.
STAMP should be a JUnit timestamp string or a time value as
      returned by `current-time'.  STAMP defaults to
      `junit-timestamp-re'.
HOST is a hostname, default `.+'.
TIME is the elapsed time in seconds, default `[0-9]+\\.[0-9]+'."
  (declare (indent defun))
  `(testsuite
    ((name . ,name)
     (timestamp . ,(cond ((stringp stamp) stamp)
                         ((listp stamp)
                          (regexp-quote
                           (format-time-string "%Y-%m-%d %T%z" stamp)))
                         (t (error "Unexpected stamp argument type %s"
                                   (type-of stamp)))))
     (hostname . ,host)
     (tests . ,(number-to-string tests))
     (failures . ,(number-to-string fail))
     (errors . ,(number-to-string err)) (time . ,time)
     (skipped . ,(number-to-string skip)))
    ,@(test-support--remove-keys contains :fail :err :skip
                                         :tests :stamp :host :time)))

(defun* testcase (name &rest contains &key (class junit-default-class)
                         (time "[0-9]+\\.[0-9]+") skip &allow-other-keys)
  "Return an esxml list for a testcase tag.
NAME is the spec description.
CONTAINS is any inner data for the tag.
CLASS is the value if the `class' attribute, default is the value
      of `junit-default-class'.
TIME is the elapsed time, default `[0-9]+\\.[0-9]+'.
If SKIP is non-nil, include the `skip' attribute."
  (declare (indent defun))
  (let ((attrs `((name . ,name) (classname . ,class) (time . ,time))))
    (setq contains (test-support--remove-keys contains :class :time))
    (cond (skip `(testcase ,attrs (skipped nil)))
          (contains `(testcase ,attrs ,@contains))
          (t `(testcase ,attrs)))))

;;; Functions to compare esxml/dom structures

(require 'dom nil t)

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

(defun test-support-xml2dom (xmlstring)
  "Parse XMLSTRING and return a dom object."
  (let ((dom (with-temp-buffer
               (insert xmlstring)
               ;; TODO: This test is broken
               (condition-case nil
                   (libxml-parse-xml-region 1 (point-max))
                 (void-function (xml-parse-region 1 (point-max)))))))
    ;; Emacs 24.5 and earlier wraps the result in another list for
    ;; some reason
    (when (listp (car dom))
      (setq dom (car dom)))
    dom))

(defun test-support-normalize-dom (dom)
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
                  collect (test-support-normalize-dom child))))
    (when children
      (setcdr (cdr node) children))
    node))

(provide 'test-support)
;;; test-support.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
