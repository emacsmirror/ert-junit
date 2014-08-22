;;; ert-junit.el --- ERT extensions used by bitbake tests

;; Copyright (C) 2014  Ola Nilsson

;; Author: Ola Nilsson <ola.nilsson@gmail.com>
;; Maintainer: Ola Nilsson <ola.nilsson@gmail.com>
;; Created; Jul 24 2014
;; Keywords: lisp, tools
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(eval-when-compile (require 'cl))

(defun ert-junit--infos-string (result)
  "Return `ert-info' infos from RESULT as a string.
RESULT must be an `ert-test-result-with-condition'."
  (check-type result ert-test-result-with-condition)
  (mapconcat
   (lambda (info)
	 ;; for each info read out prefix and message
	 (destructuring-bind (prefix . message) info
	   ;; rearrange the string so that each line except the first is
	   ;; prefixed with spaces matching prefix in length. The first
	   ;; line is prefixed with the prefix.
	   (concat prefix
			   (mapconcat #'identity
						  (split-string message "\n" t)
						  (concat "\n" (make-string (length prefix) ?\s))))))
   (ert-test-result-with-condition-infos result) "\n"))


(defun ert-junit-testcase (stats test-name test-index)
  "Insert a testcase XML element at point in the current buffer.
STATS is the test run state.  The name of the testcase is
TEST-NAME and TEST-INDEX its index into STATS."
  (concat " "
		  (format "<testcase name=\"%s\" classname=\"ert\" time=\"%f\">"
				  test-name ;name
				  ;; time
				  (float-time (time-subtract (aref (ert--stats-test-end-times stats) test-index)
											 (aref (ert--stats-test-start-times stats) test-index))))
  ;; success, failure or error
   (let ((test-status (aref (ert--stats-test-results stats) test-index))
		 (text ""))
	 (unless (ert-test-result-expected-p (aref (ert--stats-tests stats) test-index) test-status)
	   (etypecase test-status
		 (ert-test-passed "")
		 (ert-test-failed
		  (setq text (concat "<failure message=\"test\" type=\"type\">"
							 (ert-junit--infos-string test-status)
							 ;;(ert--print-backtrace (ert-test-result-with-condition-backtrace test-status))
							 "</failure>")))
		 (ert-test-quit (setq text " <failure>quit</failure>"))))
	 text)
   "</testcase>" "\n"))

(defvar ert--results-stats)

(defun ert-junit-report ()
  "Show a JUnit report for the current `ert' buffer."
  (interactive)
  (when (derived-mode-p 'ert-results-mode)
	(let ((buf (get-buffer-create "*junit*")))
	  (with-current-buffer buf
		  (erase-buffer))
	  (ert-junit-generate-report ert--results-stats buf)
	  (set-window-buffer nil buf))))

(defun ert-junit-generate-report (stats buf)
  "Generate a JUnit XML report for STATS at point in BUF."
  (with-current-buffer buf
	(insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
	(insert (format "<testsuite name=\"ERT\" timestamp=\"%s\" hostname=\"%s\" tests=\"%d\" failures=\"%d\" errors=\"%d\" time=\"%f\" skipped=\"%d\" >"
					(ert--format-time-iso8601 (ert--stats-start-time stats)) ; timestamp
					(system-name) ;hostname
					(ert-stats-total stats) ;tests
					(ert-stats-completed-unexpected stats) ;failures
					0; errors
					;;time
					(float-time (time-subtract (ert--stats-end-time stats)
											   (ert--stats-start-time stats)))
					(- (ert-stats-total stats) (ert-stats-completed stats)) ;skipped
					)
			"\n")
	(maphash (lambda (key value)
			   (insert (ert-junit-testcase stats key value)))
			 (ert--stats-test-map stats))
	(insert "</testsuite>" "\n")))

(defun ert-run-tests-junit-batch (result-file &optional selector)
  "Run `ert-tests-batch-selector' and generate JUnit report.
The report is written to RESULT-FILE and pertains to tests
selected by SELECTOR."
  (let ((stats (ert-run-tests-batch selector))
		(buf (find-file-noselect result-file)))
	(with-current-buffer buf
	  (erase-buffer)
	  (ert-junit-generate-report stats buf)
	  (save-buffer))))

(defun ert-run-xtests-batch-and-exit (&optional selector)
  "Like `ert-run-tests-batch', but exits Emacs when done.

The exit status will be 0 if all test results were as expected, 1
on unexpected results, or 2 if the tool detected an error outside
of the tests (e.g. invalid SELECTOR or bug in the code that runs
the tests)."
  (unwind-protect
      (let ((stats (ert-run-tests-batch selector))
			(result-file (and command-line-args-left
							  (= (length command-line-args-left) 1)
							  (car command-line-args-left))))
		(ert-run-tests-junit-batch result-file selector)
        (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))
    (unwind-protect
        (progn
          (message "Error running tests")
          (backtrace))
      (kill-emacs 2))))


;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; tab-width: 4
;; End:
(provide 'ert-junit)
;;; ert-junit.el ends here
