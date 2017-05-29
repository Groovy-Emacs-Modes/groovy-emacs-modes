;;; test-helper.el --- Helper for tests              -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'f)

(let ((peval-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path peval-dir))

(require 'undercover)
(undercover "groovy-mode.el"
	    (:exclude "*-test.el")
	    (:report-file "/tmp/undercover-report.json"))
