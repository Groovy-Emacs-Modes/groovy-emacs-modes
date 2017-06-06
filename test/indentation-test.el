(require 'ert)

(load-file "groovy-mode.el")

(ert-deftest test-function-indentation-using-groovy-specific-function ()
  (with-temp-buffer
    (let ((text "def f() {
def a = 1
}
"))
      (insert text)
      (goto-char (point-min))
      (groovy-indent-line)
      (next-line)
      (groovy-indent-line)
      (next-line)
      (groovy-indent-line)
      (should (equal (buffer-string) "def f() {
    def a = 1
}
")))))

(ert-deftest test-function-indentation-using-generic-function ()
  (with-temp-buffer
    (let ((text "def f() {
def a = 1
}
"))
      (groovy-mode)
      (insert text)
      (goto-char (point-min))
      (indent-line-function)
      (next-line)
      (indent-line-function)
      (next-line)
      (indent-line-function)
      (should (equal (buffer-string) "def f() {
    def a = 1
}
")))))

(ert-deftest test-for-issue-56 ()
  (with-temp-buffer
    (let ((text "class foo {
def bar() {
def a = 1
def b = a +
1
def c =
1
def d = a
.baz()
}
}
"))
      (groovy-mode)
      (insert text)
      (should (equal (buffer-string) text))
      (indent-region (point-min) (point-max))
       (should (equal (buffer-string) "class foo {
    def bar() {
        def a = 1
        def b = a +
            1
        def c =
            1
        def d = a
            .baz()
    }
}
")))))
