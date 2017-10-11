(require 'ert)
(require 'shut-up)
(require 'faces)
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
    1")
  (should-preserve-indent
   "def a = b+
    1")
  ;; Don't get confused by commented-out lines.
  (should-preserve-indent
   "// def a = b+
1")
  ;; Don't get confused by lines that end by / when it isn't division.
  (should-preserve-indent
   "def x = /foo/
1"))

(ert-deftest groovy-indent-infix-closure ()
  "We should only indent by one level inside closures."
  (should-preserve-indent
   "def foo() {
    def f = { ->
        \"foo\"
    }
}")
  (should-preserve-indent
   "def foo() {
    def f = { def bar ->
        \"foo\"
    }
}"))

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

(ert-deftest groovy-indent-list ()
  "Ensure we handle indents inside lists correctly."
  ;; If we have a single empty [ on a line, we should increase by one
  ;; tab stop.
  (should-preserve-indent "def x = [
    1,
    2,
]")
  ;; But if we have values after the [, we should line up subsequent
  ;; lines.
  (should-preserve-indent "def x = [1,
         2,
         3,
]"))

(defmacro with-highlighted-groovy (src &rest body)
  "Insert SRC in a temporary groovy-mode buffer, apply syntax highlighting,
then run BODY."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,src)
     (goto-char (point-min))
     ;; Activate groovy-mode, but don't run any hooks. This doesn't
     ;; matter on Travis, but is defensive when running tests in the
     ;; current Emacs instance.
     (delay-mode-hooks (groovy-mode))
     ;; Ensure we've syntax-highlighted the whole buffer.
     (if (fboundp 'font-lock-ensure)
         (font-lock-ensure)
       (with-no-warnings
         (font-lock-fontify-buffer)))
     ,@body))

(ert-deftest groovy-highlight-triple-double-quote ()
  "Ensure we handle single \" correctly inside a triple-double-quoted string."
  (with-highlighted-groovy "x = \"\"\"foo \" bar \"\"\""
    (search-forward "bar")
    (should (eq (face-at-point) 'font-lock-string-face))))

(ert-deftest groovy-highlight-triple-single-quote ()
  "Ensure we handle single \" correctly inside a triple-double-quoted string."
  (with-highlighted-groovy "x = '''foo ' bar '''"
    (search-forward "bar")
    (should (eq (face-at-point) 'font-lock-string-face))))

(ert-deftest groovy-highlight-annotation ()
  "Highlight annotations correctly."
  (with-highlighted-groovy
   "@Test() private foo = 1"
   (search-forward "Test")
   (backward-char 1)
   (should (eq (face-at-point) 'groovy-annotation-face))))

(ert-deftest groovy-highlight-interface-keyword ()
  "Highlight interface as annotation or keyword depending on state."
  (with-highlighted-groovy
   "public @interface Anno {}"
   (search-forward "interface")
   (backward-char 1)
   (should (eq (face-at-point) 'groovy-annotation-face)))
  (with-highlighted-groovy
   "public interface Interface1 {}"
   (search-forward "interface")
   (backward-char 1)
   (should (eq (face-at-point) 'font-lock-keyword-face))))

(defun faces-at-point ()
  (let* ((props (text-properties-at (point)))
         (faces (plist-get props 'face)))
    (if (listp faces) faces (list faces))))

(ert-deftest groovy-highlight-interpolation ()
  "Ensure we highlight interpolation in double-quoted strings."
  (with-highlighted-groovy "x = \"$foo\""
    (search-forward "$")
    (should (memq 'font-lock-variable-name-face (faces-at-point))))
  ;; Triple-double-quoted strings have interpolation.
  (with-highlighted-groovy "x = \"\"\"$foo\"\"a\""
    (search-forward "$")
    (should (memq 'font-lock-variable-name-face (faces-at-point))))
  ;; Escaped $ are not interpolated.
  (with-highlighted-groovy "x = \"\\$foo\""
    (search-forward "$")
    (should (not (memq 'font-lock-variable-name-face (faces-at-point)))))
  ;; Sequences of interpolations do not need to be separated.
  (with-highlighted-groovy "x = \"$foo$bar\""
    (search-forward "b")
    (should (memq 'font-lock-variable-name-face (faces-at-point))))
  ;; Interpolation after an escaped dollar, i.e. \$$foo
  (with-highlighted-groovy "x = \"\\$$foo\""
    (search-forward "f")
    (should (memq 'font-lock-variable-name-face (faces-at-point)))) )

(ert-deftest groovy-highlight-interpolation-single-quotes ()
  "Ensure we do not highlight interpolation in single-quoted strings."
  (with-highlighted-groovy "x = '$foo'"
    (search-forward "$")
    ;; This should be highlighted as a string, nothing else.
    (should (equal '(font-lock-string-face) (faces-at-point))))
  (with-highlighted-groovy "x = '${foo}'"
    (search-forward "f")
    (should (equal '(font-lock-string-face) (faces-at-point))))
  (with-highlighted-groovy "x = '''$foo'''"
    (search-forward "$")
    (should (equal '(font-lock-string-face) (faces-at-point))))
  (with-highlighted-groovy "x = '''${foo}'''"
    (search-forward "f")
    (should (equal '(font-lock-string-face) (faces-at-point)))))

(ert-deftest groovy-highlight-comments ()
  "Ensure we do not confuse comments with slashy strings."
  (with-highlighted-groovy "// foo"
    (search-forward " ")
    (should (memq 'font-lock-comment-face (faces-at-point))))
  ;; // on a single line is a comment, not an empty slashy-string.
  (with-highlighted-groovy "// foo\n//\n"
    (search-forward "\n")
    (backward-char)
    (should (memq 'font-lock-comment-face (faces-at-point)))))

(ert-deftest groovy-highlight-slashy-string ()
  "Highlight /foo/ as a string."
  ;; simple case
  (with-highlighted-groovy "x = /foo/"
    (search-forward "foo")
    (should (memq 'font-lock-string-face (faces-at-point))))
  ;; multiline
  (with-highlighted-groovy "x = \"bar\" + /foo\nbaz/"
    (search-forward "foo")
    (should (memq 'font-lock-string-face (faces-at-point))))
  ;; division is not a multiline string
  (with-highlighted-groovy "x = y / z"
    (search-forward "z")
    (forward-char -1)
    (should (not (memq 'font-lock-string-face (faces-at-point)))))
  ;; Don't get confused by ending with $/.
  (with-highlighted-groovy "x = /foo$/ + bar"
    (search-forward "bar")
    (forward-char -1)
    (should (not (memq 'font-lock-string-face (faces-at-point)))))
  ;; Don't get confused by $ inside a slashy string.
  (with-highlighted-groovy "x = /$ foo $/"
    (search-forward "foo")
    (should (memq 'font-lock-string-face (faces-at-point))))
  ;; Don't get confused by comments.
  (with-highlighted-groovy
      "def bar /* foo */"
    (search-forward "foo")
    (should (not (memq 'font-lock-string-face (faces-at-point))))))

(ert-deftest groovy-highlight-variable-assignment ()
  "Highlight 'x = 1' as variable."
  (let ((groovy-highlight-assignments t))
    (with-highlighted-groovy "x = 1"
      (search-forward "x")
      (backward-char 1)
      (should (memq 'font-lock-variable-name-face (faces-at-point))))
    (with-highlighted-groovy "if (x = \"foo\") {"
      (search-forward "x")
      (backward-char 1)
      (should (memq 'font-lock-variable-name-face (faces-at-point))))
    (with-highlighted-groovy "Foo y; x = 1"
      (search-forward "x")
      (backward-char 1)
      (should (memq 'font-lock-variable-name-face (faces-at-point))))
    (with-highlighted-groovy "(x =~ /bar/)"
      (search-forward "x")
      (backward-char 1)
      (should (not (memq 'font-lock-variable-name-face (faces-at-point)))))
    (with-highlighted-groovy "x == bar"
      (search-forward "x")
      (backward-char 1)
      (should (not (memq 'font-lock-variable-name-face (faces-at-point)))))
    (with-highlighted-groovy "@Foo(x=false)"
      (search-forward "x")
      (backward-char 1)
      (should (not (memq 'font-lock-variable-name-face (faces-at-point)))))))

(ert-deftest groovy-highlight-variable-declaration ()
  "Highlight 'def x' as variable."
  (with-highlighted-groovy "def x"
    (search-forward "x")
    (backward-char 1)
    (should (memq 'font-lock-variable-name-face (faces-at-point))))
  (with-highlighted-groovy "private String x = 1"
    (search-forward "x")
    (backward-char 1)
    (should (memq 'font-lock-variable-name-face (faces-at-point))))
  (with-highlighted-groovy "Foo x; def y = 1"
    (search-forward "x")
    (backward-char 1)
    (should (memq 'font-lock-variable-name-face (faces-at-point))))
  (with-highlighted-groovy "List<Map<String, Object>> x"
    (search-forward "x")
    (backward-char 1)
    (should (memq 'font-lock-variable-name-face (faces-at-point))))
  (with-highlighted-groovy "String a, b, c, d"
    (search-forward "b")
    (backward-char 1)
    (should (memq 'font-lock-variable-name-face (faces-at-point))))
  (with-highlighted-groovy "def (a, b, c) = [1, 2, 3]"
    (search-forward "b")
    (backward-char 1)
    (should (memq 'font-lock-variable-name-face (faces-at-point))))
  (with-highlighted-groovy "[:].each { String x, def y ->"
    (search-forward "y")
    (backward-char 1)
    (should (memq 'font-lock-variable-name-face (faces-at-point))))
  (with-highlighted-groovy "private void fooBar(Foo x) {"
    (search-forward "x")
    (backward-char 1)
    (should (memq 'font-lock-variable-name-face (faces-at-point)))))

(ert-deftest groovy-highlight-variables ()
  "Make sure symbols aren't being highlighted that shouldn't be."
  (with-highlighted-groovy "def (a, b, c) = [1, x, 3]"
    (search-forward "x")
    (backward-char 1)
    (should (not (memq 'font-lock-variable-name-face (faces-at-point)))))
  (with-highlighted-groovy "def (a, b, c) = foo(1, x, 3)"
    (search-forward "x")
    (backward-char 1)
    (should (not (memq 'font-lock-variable-name-face (faces-at-point)))))
  (with-highlighted-groovy "x"
    (should (not (memq 'font-lock-variable-name-face (faces-at-point))))))

(ert-deftest groovy-highlight-functions ()
  (with-highlighted-groovy "private void fooBar(Foo x) {"
    (search-forward "foo")
    (should (memq 'font-lock-function-name-face (faces-at-point))))
  (with-highlighted-groovy "private List<String> fooBar() {"
    (search-forward "foo")
    (should (memq 'font-lock-function-name-face (faces-at-point)))))
