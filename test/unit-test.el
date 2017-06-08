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
     (setq indent-tabs-mode nil)
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

(ert-deftest groovy-indent-method-call ()
  "We should increase indent for method calls"
  (should-indent
   "foo
.bar()"
   "foo
    .bar()"))

(ert-deftest groovy-indent-switch ()
  "We should indent case statements less than their bodies."
  (should-indent
   "switch (foo) {
case Class1:
bar()
break
default:
baz()
}"
   "switch (foo) {
    case Class1:
        bar()
        break
    default:
        baz()
}")
  (should-indent
   "switch (foo) {
    case Class1:
        if (bar) {
            bar()
    }
        break
    default:
        baz()
}"
   "switch (foo) {
    case Class1:
        if (bar) {
            bar()
        }
        break
    default:
        baz()
}"))
