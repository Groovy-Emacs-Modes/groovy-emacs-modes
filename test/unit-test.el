(require 'ert)

(load-file "groovy-mode.el")

(ert-deftest groovy-smoke-test ()
  "Ensure that we can activate the Groovy major mode."
  (with-temp-buffer
    (groovy-mode)))
