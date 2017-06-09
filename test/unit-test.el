(require 'ert)
(require 'shut-up)
(require 'groovy-mode)

(ert-deftest groovy-smoke-test ()
  "Ensure that we can activate the Groovy major mode."
  (with-temp-buffer
    (groovy-mode)))


(defmacro should-indent-to (source result)
  "Assert that SOURCE is indented to produce RESULT."
  `(with-temp-buffer
     (insert ,source)
     (groovy-mode)
     (setq indent-tabs-mode nil)
     (shut-up
       (indent-region (point-min) (point-max)))
     (should (equal (buffer-string) ,result))))

(defmacro should-preserve-indent (source)
  "Assert that SOURCE does not change when indented."
  (let ((src-sym (make-symbol "src")))
    `(let ((,src-sym ,source))
       (should-indent-to ,src-sym ,src-sym))))

(ert-deftest groovy-indent-function ()
  "We should indent according to the number of parens."
  (should-indent-to
   "def foo() {
bar()
}"
   "def foo() {
    bar()
}"))

(ert-deftest groovy-indent-infix-operator ()
  "We should increase indent after infix operators."
  (should-preserve-indent
   "def a = b +
    1"))

(ert-deftest groovy-indent-method-call ()
  "We should increase indent for method calls"
  (should-preserve-indent
   "foo
    .bar()"))

(ert-deftest groovy-indent-switch ()
  "We should indent case statements less than their bodies."
  ;; Simple switch statement
  (should-preserve-indent
   "switch (foo) {
    case Class1:
        bar()
        break
    default:
        baz()
}")
  ;; Braces within switch statements.
  (should-preserve-indent
   "switch (foo) {
    case Class1:
        if (bar) {
            bar()
        }
        break
    default:
        baz()
}")
  ;; Ensure we handle colons correctly.
  (should-preserve-indent
   "switch (foo) {
    case Class1 :
        bar()
}")
  (should-preserve-indent
   "switch (foo) {
    case Class1:
        x? y: z
}")
  )
