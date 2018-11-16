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
(eval-and-compile (require 'cl))
(unless (require 'ert-x nil t)
  (defmacro* ert-with-test-buffer ((&key ((:name name-form)))
                                     &body body)
    "Simple replacement for `ert-with-test-buffer' for Emacs 23.4."
    (declare (debug ((":name" form) body))
             (indent 1))
    `(with-temp-buffer ,@body)
    ))
(require 'xml)
(require 'test-support)

;; For Emacs 23.4
(unless (fboundp 'cl-incf) (defalias 'cl-incf 'incf))
(unless (fboundp 'cl-loop) (defalias 'cl-loop 'loop))
(unless (fboundp 'cl-sort) (defalias 'cl-sort 'sort*))
(unless (fboundp 'user-error)
  (defun user-error (format &rest args)
    (signal 'user-error (list (apply #'format format args))))
  (put 'user-error 'error-conditions '(error user-error))
  (put 'user-error 'error-message "")
  )

(when (version< emacs-version "24.1")
  ;; ert for Emacs 23.4 does not handle ert-test-quit objects in
  ;; ert--stats-set-test-and-result.  The function below is a copy of
  ;; ert--stats-set-test-and-result from the last ert version outside
  ;; GNU Emacs with the ert-test-quit cases added.
  (defun ert--stats-set-test-and-result (stats pos test result)
    "Change STATS by replacing the test at position POS with TEST and RESULT.

Also changes the counters in STATS to match."
    (let* ((tests (ert--stats-tests stats))
           (results (ert--stats-test-results stats))
           (old-test (aref tests pos))
           (map (ert--stats-test-map stats)))
      (flet ((update (d)
               (if (ert-test-result-expected-p (aref tests pos)
                                               (aref results pos))
                   (etypecase (aref results pos)
                     (ert-test-passed (incf (ert--stats-passed-expected stats) d))
                     (ert-test-failed (incf (ert--stats-failed-expected stats) d))
                     (null)
                     (ert-test-aborted-with-non-local-exit)
                     (ert-test-quit))
                 (etypecase (aref results pos)
                   (ert-test-passed (incf (ert--stats-passed-unexpected stats) d))
                   (ert-test-failed (incf (ert--stats-failed-unexpected stats) d))
                   (null)
                   (ert-test-aborted-with-non-local-exit)
                   (ert-test-quit)))))
        ;; Adjust counters to remove the result that is currently in stats.
        (update -1)
        ;; Put new test and result into stats.
        (setf (aref tests pos) test
              (aref results pos) result)
        (remhash (ert--stats-test-key old-test) map)
        (setf (gethash (ert--stats-test-key test) map) pos)
        ;; Adjust counters to match new result.
        (update +1)
        nil))))

(defvar test-ert-junit-has-skipped (version<= "24.4" emacs-version))

(defmacro should-equal-normalized (d1 d2)
  "Check that normalized doms D1 and D2 are `equal'.
Normalization is done by `test-ert-junit-normalize-dom'."
  (declare (debug t))
  `(let ((n1 (test-ert-junit-normalize-dom ,d1))
		 (n2 (test-ert-junit-normalize-dom ,d2)))
	 (ert-info ((pp-to-string n1) :prefix "D1: ")
	   (ert-info ((pp-to-string n2) :prefix "D2: ")
		 (should (equal n1 n2))))))

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

(ert-deftest test-ert-junit--condition-string-1-wrong-type ()
  :tags '(ert-junit--condition-string)
  (ert-info ("Calling ert-junit--condition-string with bad argument")
    (should-error (ert-junit--condition-string nil)
                  ;; CL in Emacs 23.4 does not have a specific error
                  ;; type for wrong struct type arguments.
                  :type (if (version< emacs-version "24")
                            'error
                          'wrong-type-argument))))

(ert-deftest test-ert-junit--condition-string-2-expected-string ()
  :tags '(ert-junit--condition-string)
  (let ((result-with-condition (make-ert-test-result-with-condition
                                :condition '(= 1 2)
                                :backtrace "backtrace"
                                :infos '())))
    (should (string= "(= 1 2)\n"
                     (ert-junit--condition-string result-with-condition)))))

(defun test-ert-junit--set-test-status (stats pos test result
											  &optional duration start-time)
  "Change STATS by replacing the test at position POS with TEST and RESULT.
Also set start and end time for POS according to DURATION and
START-TIME.  START-TIME should be a time value as accepted by
`float-time'.  DURATION should be a float number of seconds as
returned by `float-time'.  Default value for START-TIME is `'(0 0
0 0)' and 0 for DURATION."
  (ert--stats-set-test-and-result stats pos test result)
  (setq start-time (or start-time '(0 0 0 0)))
  (setf (aref (ert--stats-test-start-times stats) pos) start-time)
  (setf (aref (ert--stats-test-end-times stats) pos)
		(if duration
			(time-add start-time (seconds-to-time duration))
			'(0 0 0 0))))

(defun test-ert-junit-run-tests (test-list)
  "Run each test in TEST-LIST and return a stats object for them."
  (cl-loop with stats = (ert--make-stats test-list 't)
           for test in test-list
           for idx from 0
           do (test-ert-junit--set-test-status stats idx test
                                               (ert-run-test test))
           finally return stats))

(ert-deftest test-ert-junit-testcase-1 ()
  "Check a single passing test."
  :tags '(ert-junit-testcase)
  (let* ((passing-test (make-ert-test :body (lambda () (should t))))
         (stats (test-ert-junit-run-tests (list passing-test))))
	(should-equal-normalized
     '(testcase ((name . "passing-test")
				 (classname . "ert")
				 (time . "0.000000")))
     (test-ert-junit-xml2dom (ert-junit-testcase stats "passing-test" 0)))))

(ert-deftest test-ert-junit-testcase-2-failed ()
  "Test failed."
  :tags '(ert-junit-testcase)
  (let* ((failed-test (make-ert-test :body (lambda () (should (= 1 2)))))
         (stats (test-ert-junit-run-tests (list failed-test)))
         (output (ert-junit-testcase stats "failed-test" 0))
         (testcase (test-ert-junit-xml2dom output))
         (expected `(testcase ((name . "failed-test")
                               (classname . "ert")
                               (time . "0.000000"))
                              (failure
                               ((message . "test")
                                (type . "type"))
                               "(ert-test-failed\n ((should\n   (= 1 2))\n  :form\n  (= 1 2)\n  :value nil))"))))
    (should-equal-normalized expected testcase)))


(ert-deftest test-ert-junit-testcase-3 ()
  "Check unexpected ok."
  :tags '(ert-junit-testcase)
  (let* ((unexpected-ok (make-ert-test :expected-result-type :failed
                                       :body (lambda () (should t))))
         (stats (test-ert-junit-run-tests (list unexpected-ok))))
	(should-equal-normalized
	 '(testcase ((name . "unexpected-ok")
				 (classname . "ert")
				 (time . "0.000000"))
				(failure
				 ((message . "passed unexpectedly")
				  (type . "type"))))
	 (test-ert-junit-xml2dom (ert-junit-testcase stats "unexpected-ok" 0)))))

(ert-deftest test-ert-junit-testcase-4 ()
  "Check expected fail."
  :tags '(ert-junit-testcase)
  (let* ((expected-fail (make-ert-test :expected-result-type :failed
                                       :body (lambda () (should nil))))
         (stats (test-ert-junit-run-tests (list expected-fail))))
	(should-equal-normalized
     '(testcase ((name . "expected-fail")
				 (classname . "ert")
				 (time . "0.000000"))
				(failure
                 ((message . "test")
                  (type . "type"))
                 "(ert-test-failed\n ((should nil)\n  :form nil :value nil))"
                 ))
     (test-ert-junit-xml2dom (ert-junit-testcase stats "expected-fail" 0)))))

(ert-deftest test-ert-junit-testcase-5 ()
  "Check skipped tests."
  :tags '(ert-junit-testcase)
  ;; Can't test skip if skip is not supported
  :expected-result (if (fboundp 'ert-skip) :passed :failed)
  (should (fboundp 'ert-skip))
  (let* ((skipped-unless (make-ert-test :body (lambda () (skip-unless (= 1 2)))))
         (skipped-string (make-ert-test :body (lambda () (ert-skip "skip"))))
         (skipped-data (make-ert-test :body (lambda () (ert-skip '(= 1 2)))))
         (stats (test-ert-junit-run-tests (list skipped-unless skipped-string skipped-data))))
    (should-equal-normalized
     '(testcase ((name . "skipped-unless")
                 (classname . "ert")
                 (time . "0.000000"))
                (skipped
                 ((message . "(= 1 2)")
                  (type . "type"))
                 "(ert-test-skipped\n ((skip-unless\n   (= 1 2))\n  :form\n  (= 1 2)\n  :value nil))"
                 ))
     (test-ert-junit-xml2dom (ert-junit-testcase stats "skipped-unless" 0)))
    (should-equal-normalized
     '(testcase ((name . "skipped-string")
                 (classname . "ert")
                 (time . "0.000000"))
                (skipped
                 ((message . "skip")
                  (type . "type"))
                 "(ert-test-skipped \"skip\")"))
     (test-ert-junit-xml2dom (ert-junit-testcase stats "skipped-string" 1)))
    (should-equal-normalized
     '(testcase ((name . "skipped-data")
                 (classname . "ert")
                 (time . "0.000000"))
                (skipped
                 ((message . "(= 1 2)")
                  (type . "type"))
                 "(ert-test-skipped\n (= 1 2))"))
     (test-ert-junit-xml2dom (ert-junit-testcase stats "skipped-data" 2)))
    ))

(ert-deftest test-ert-junit-testcase-6 ()
  "Test that `ert-junit-testcase' errors on bad input."
  :tags '(ert-junit-testcase)
  (should-error (ert-junit-testcase nil nil nil))
  (let* ((passing-test (make-ert-test :body (lambda () (should t))))
         (stats (ert--make-stats (list passing-test) 't)))
    (should-error (ert-junit-testcase stats nil nil))))

(ert-deftest test-ert-junit-testcase-7 ()
  "Test that ert tests that signals unexpected errors are reported as errored."
  :tags '(ert-junit-testcase)
  (let* ((error-test (make-ert-test :body (lambda () (error "Unexpected"))))
         (signal-test (make-ert-test
                       :body (lambda () (user-error "Unexpected user-error"))))
         (expected-error (make-ert-test
                          :body (lambda () (should-error (error "Expected")))))
         (wrong-error (make-ert-test
                       :body (lambda () (should-error (error "Wrong type")
                                                      :type 'user-error))))
         (stats (test-ert-junit-run-tests
                 (list error-test signal-test expected-error wrong-error))))
    (should-equal-normalized
     '(testcase ((name . "error-test")
                 (classname . "ert")
                 (time . "0.000000"))
                (error ((type . "type")
                        (message . "Unexpected"))))
     (test-ert-junit-xml2dom (ert-junit-testcase stats "error-test" 0)))
    (should-equal-normalized
     '(testcase ((name . "signal-test")
                 (classname . "ert")
                 (time . "0.000000"))
                (error ((type . "type")
                        (message . "Unexpected user-error"))))
     (test-ert-junit-xml2dom (ert-junit-testcase stats "signal-test" 1)))
    (should-equal-normalized
     '(testcase ((name . "expected-error")
                 (classname . "ert")
                 (time . "0.000000")))
     (test-ert-junit-xml2dom (ert-junit-testcase stats "expected-error" 2)))
    (should-equal-normalized
     `(testcase ((name . "wrong-error")
                 (classname . "ert")
                 (time . "0.000000"))
                (failure ((type . "type")
                          (message . "test"))
                         ,(concat
                           "(ert-test-failed\n ((should-error\n"
                           "   (error \"Wrong type\")\n"
                           "   :type 'user-error)\n  :form\n"
                           "  (error \"Wrong type\")\n"
                           "  :condition\n  (error \"Wrong type\")\n  "
                           ":fail-reason \"the error "
                           ;; Spelling of signalled changed in Emacs 24
                           (if (version< emacs-version "24")
                               "signalled" "signaled")
                           " did not have the expected type\"))")))
     (test-ert-junit-xml2dom (ert-junit-testcase stats "wrong-error" 3)))
    ))

(ert-deftest test-ert-junit-testcase-8-quit ()
  "Test quit."
  :tags '(ert-junit-testcase)
  (let* ((quit-test (make-ert-test :body (lambda () (signal 'quit nil))))
         (stats (test-ert-junit-run-tests (list quit-test)))
         (quit-xml (ert-junit-testcase stats "quit-test" 0)))
    (should-equal-normalized
     '(testcase ((name . "quit-test")
                 (classname . "ert")
                 (time . "0.000000"))
                (failure () "quit"))
     (test-ert-junit-xml2dom quit-xml))))

(defun mock-ert-junit-testcase (stats test-name test-index)
  "STATS TEST-NAME TEST-INDEX."
  (ignore stats test-name test-index)
  "")

(defmacro test-ert-junit--report-env (&rest body)
  "Execute BODY with mocked functions and UTC0 timezone.
Function `ert-junit-testcase' and function `system-name' are mocked."
  (declare (debug t) (indent defun))
  ;; cl-letf was introduced in Emacs 24, while flet was declared obsolete
  (if (version< emacs-version "24")
      `(flet ((ert-junit-testcase (stats test-name test-index)
                                  (mock-ert-junit-testcase stats
                                                           test-name
                                                           test-index))
              (system-name () (progn "mock")))
         (let ((process-environment process-environment))
           (setenv "TZ" "UTC0")
           ,@body))
    `(cl-letf (((symbol-function 'ert-junit-testcase) #'mock-ert-junit-testcase)
               ((symbol-function 'system-name) (lambda () "mock")))
       (let ((process-environment process-environment))
         (setenv "TZ" "UTC0")
         ,@body))))

(ert-deftest test-ert-junit-generate-report-1-notests ()
  "Generate a report with no testcases."
  (test-ert-junit--report-env
    (let ((stats (ert--make-stats '() 't))
          xml)
      (setf (ert--stats-start-time stats) (date-to-time "2018-05-09 00:25+0000")
            (ert--stats-end-time stats) (date-to-time "2018-05-09 00:27+0000"))
      (ert-with-test-buffer
          (:name (progn "xml"))
          (ert-junit-generate-report stats (current-buffer))
          (goto-char 1)
          (should (string= (buffer-substring 1 (line-end-position))
                           "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"))
          (should-equal-normalized
           (testsuites
             (testsuite "ERT"
               :stamp "2018-05-09 00:25:00+0000"
               :host "mock"
               :time "120.000000"))
           (test-ert-junit-xml2dom (buffer-substring (line-end-position) (point-max))))
          ))))

(ert-deftest test-ert-junit-generate-report-2 ()
  "Generate a report with a mix of testcase results."
  (test-ert-junit--report-env
    (let* ((passed-expected (make-ert-test :body (lambda () (should t))))
           (passed-unexpected (make-ert-test :body (lambda () (should t))
                                             :expected-result-type :failed))
           (failed-expected (make-ert-test :body (lambda () (should nil))
                                           :expected-result-type :failed))
           (failed-unexpected (make-ert-test :body (lambda () (should nil))))
           (skipped (make-ert-test :body (lambda () (ert-skip "skip"))))
           (erroring (make-ert-test :body (lambda () (error "Error"))))
           (ok-error (make-ert-test :body (lambda () (should-error (error "Ok") :type 'error))))
           (stats (test-ert-junit-run-tests (list passed-expected passed-unexpected
                                                  failed-expected failed-unexpected
                                                  skipped erroring ok-error)))
           xml)
      (setf (ert--stats-start-time stats) (date-to-time "2018-05-09 00:25+0000")
            (ert--stats-end-time stats) (date-to-time "2018-05-09 00:27+0000"))
      (ert-with-test-buffer
          (:name (progn "xml"))
          (ert-junit-generate-report stats (current-buffer))
          (goto-char 1)
          (should-equal-normalized
           (testsuites
             (testsuite "ERT"
               :stamp "2018-05-09 00:25:00+0000"
               :host "mock"
               :tests 7
               :fail 3
               :err (if test-ert-junit-has-skipped 1 2)
               :skip (if test-ert-junit-has-skipped 1 0)
               :time  "120.000000"))
           (test-ert-junit-xml2dom (buffer-substring (line-end-position) (point-max))))
          ))))

(provide 'test-ert-junit)
;;; test-ert-junit.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 4
;; End:
