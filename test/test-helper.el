;;; test-helper.el --- Helper for tests              -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'f)

(let ((groovy-mode-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path groovy-mode-dir))

(require 'undercover)
(undercover "groovy-mode.el"
	    (:exclude "*-test.el")
	    (:report-file "/tmp/undercover-report.json"))
