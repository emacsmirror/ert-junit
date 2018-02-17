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



(provide 'test-ert-junit)
;;; test-ert-junit.el ends here
