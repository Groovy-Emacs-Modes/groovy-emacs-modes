(require 'ert)
(require 'shut-up)
(require 'groovy-mode)

(ert-deftest groovy-smoke-test ()
  "Ensure that we can activate the Groovy major mode."
  (with-temp-buffer
    (groovy-mode)))


(defmacro should-indent (source result)
  "Assert that SOURCE is indented to produce RESULT."
  `(with-temp-buffer
     (insert ,source)
     (groovy-mode)
     (shut-up
       (indent-region (point-min) (point-max)))
     (should (equal (buffer-string) ,result))))

(ert-deftest groovy-indent-function ()
  "We should indent according to the number of parens."
  (should-indent
   "def foo() {
bar()
}"
   "def foo() {
    bar()
}"))

(ert-deftest groovy-indent-infix-operator ()
  "We should increase indent after infix operators."
  (should-indent
   "def a = b +
1"
   "def a = b +
    1"))
