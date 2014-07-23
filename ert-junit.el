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

(defun ert-run-xtests-batch-and-exit (&optional selector)
  "Like `ert-run-tests-batch', but exits Emacs when done.

The exit status will be 0 if all test results were as expected, 1
on unexpected results, or 2 if the tool detected an error outside
of the tests (e.g. invalid SELECTOR or bug in the code that runs
the tests)."
  (progn; unwind-protect
      (let ((stats (ert-run-tests-batch selector))
			(result-file (and command-line-args-left
							  (= (length command-line-args-left) 1)
							  (car command-line-args-left))))
		(when result-file
		  (find-file result-file)
		  (delete-region (point-min) (point-max))
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
					 (insert " "
							 (format "<testcase name=\"%s\" classname=\"ert\" time=\"%f\""
									 key ;name
									 ;; time
									 (float-time (time-subtract (aref (ert--stats-test-end-times stats) value)
																(aref (ert--stats-test-start-times stats) value)))))
					 ;; success, failure or error
					 (let ((test-status (aref (ert--stats-test-results stats) value)))
					   (etypecase test-status
						 (ert-test-passed "")
						 (ert-test-failed (insert " failure=\"")
										  (ert--insert-infos test-status)
										  ;(ert--print-backtrace (ert-test-result-with-condition-backtrace test-status))
										  (insert "\""))
						 (ert-test-quit (insert " failure=\"quit\"")
										)))
					 (insert ">"
							 "</testcase>" "\n"
							 ))
				   (ert--stats-test-map stats))
		  (insert "</testsuite>" "\n")
		  (save-buffer))
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

