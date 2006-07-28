;;; groovy-mode.el --- Groovy mode derived mode

;;  Author: Russel Winder <russel@russel.org.uk>
;;  Created: 2006-07-22

;;  Copyright (C) 2006 Russel Winder

;;  This program is free software; you can redistribute it and/or modify it under the terms of
;;  the GNU General Public License as published by the Free Software Foundation; either version
;;  2 of the License, or (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;;  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;  See the GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License along with this program;
;;  if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;  Boston, MA 02110-1301 USA.

;;; Commentary:
;;
;;  This mode was developed using the Java and Awk modes that are part of CC Mode (the 5.31
;;  source was used) and C# Mode from Dylan R. E. Moonfire <contact@mfgames.com> (the 0.5.0
;;  source was used).  This code may contain some code fragments from those sources that was
;;  cut-and-pasted then edited.  All other code is newly entered by the author.

;;; Bugs:
;;

;;; Versions:
;;
;;    0.1.0 - will be the initial release when it is ready :-)
;;

;;; Notes:

;;  Sort out how to get EOL to be a (context sensitive) statement terminator.  Awk mode has
;;  infrastructure for this.

;;  Need to think about the `.*', `.?' and `.@' operators.  THis probably means changing
;;  `c-after-id-concat-ops' but also `c-operators'.

;;  Need to deal with operator overloading (groovy has this but Java does not) so
;;  `c-overloadable-operators' needs investigating.

;;  Need to investigate how to support the triple string delimiters for multi-line strings. 

;;  It is not clear whether Java mode supports Java 5.0 yet.  Annotations, generics, enums,
;;  etc. are not properly handled.  Does Groovy need to worry about this?

;;  Should we support GString / template markup ( e.g. `<%' and `%>') specially?

;;  Need to support sqaure bracket indenting for list literals.

;;  Need to think whether Groovy needs a different c-decl-prefix-re compared to Java.
;;  Certainly, Java will have to change to handle the generics.

;;  Probably need to change `c-block-prefix-disallowed-chars' as Groovy is not the same as Java.

;;  Probably need to change `c-type-decl-suffix-key' as Groovy is not the same as Java.

;;  Need to add the spaceship operator to the comparison operators.


;;; Code:
(require 'cc-mode)

;; CSharp mode comment says: These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock related constants could
;; additionally be put inside an (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-mode" nil t) ; C# mode has this
    (load "cc-fonts" nil t) ; C# mode has this
    (load "cc-langs" nil t) ; C# mode has this
;    (load "cc-bytecomp" nil t) ; Awk mode has this
))

(eval-and-compile
  (c-add-language 'groovy-mode 'java-mode))

;; Groovy allows operators such as `.*', `.?' and `.@'.  Java mode puts `*' here to deal with
;; import statement usage whcih we need for Groovy.
;(c-lang-defconst c-after-id-concat-ops
;  groovy '( "*" "?" "@" ))

;; Because of the above we have to redefine `c_operatos' because no other language has `.?' and
;; `.@' operators.
;(c-lang-defconst c-operators

;  "List describing all operators, along with their precedence and
;associativity.  The order in the list corresponds to the precedence of
;the operators: The operators in each element is a group with the same
;precedence, and the group has higher precedence than the groups in all
;following elements.  The car of each element describes the type of of
;the operator group, and the cdr is a list of the operator tokens in
;it.  The operator group types are:

;'prefix         Unary prefix operators.
;'postfix        Unary postfix operators.
;'postfix-if-paren
;		Unary postfix operators if and only if the chars have
;		parenthesis syntax.
;'left-assoc     Binary left associative operators (i.e. a+b+c means (a+b)+c).
;'right-assoc    Binary right associative operators (i.e. a=b=c means a=(b=c)).
;'right-assoc-sequence
;                Right associative operator that constitutes of a
;                sequence of tokens that separate expressions.  All the
;                tokens in the group are in this case taken as
;                describing the sequence in one such operator, and the
;                order between them is therefore significant.

;Operators containing a character with paren syntax are taken to match
;with a corresponding open/close paren somewhere else.  A postfix
;operator with close paren syntax is taken to end a postfix expression
;started somewhere earlier, rather than start a new one at point.  Vice
;versa for prefix operators with open paren syntax.

;Note that operators like \".\" and \"->\" which in language references
;often are described as postfix operators are considered binary here,
;since CC Mode treats every identifier as an expression."

;  groovy `(
;      ;; Primary.
;      ;,@(c-lang-const c-identifier-ops)
;      ;,@((postfix-if-paren "<" ">") ; Templates.
;      ;   (prefix "super"))

;      ;; Postfix.
;      (left-assoc "." ".*" ".?" ".@")

;      (postfix "++" "--" "[" "]" "(" ")" "<:" ":>")

;      ;; Unary.
;      (prefix "++" "--" "+" "-" "!" "~" "new")

;      ;; Multiplicative.
;      (left-assoc "*" "/" "%")

;      ;; Additive.
;      (left-assoc "+" "-")

;      ;; Shift.
;      (left-assoc "<<" ">>" ">>>")

;      ;; Relational.
;      (left-assoc "<" ">" "<=" ">=" "instanceof")

;      ;; Equality.
;      (left-assoc "==" "!=")

;      ;; Bitwise and.
;      (left-assoc "&")

;      ;; Bitwise exclusive or.
;      (left-assoc "^")

;      ;; Bitwise or.
;      (left-assoc "|")

;      ;; Logical and.
;      (left-assoc "&&")

;      ;; Logical or.
;      (left-assoc "||")

;      ;; Conditional.
;      (right-assoc-sequence "?" ":")

;      ;; Assignment.
;      (right-assoc ,@(c-lang-const c-assignment-operators))

;      ;; Exception.
;      (prefix "throw") ; Java mode didn't have this but c++ mode does.  Humm...

;      ;; Sequence.
;      (left-assoc ",")))

;;; Groovy allows operators to be overloaded like C++ and unlike Java.
;(c-lang-defconst c-overloadable-operators
;  groovy  '(	 "+" "-" "*" "/" "%"
;                 "^" "&" "|" "~"
;                 "!" "=" "<" ">" "+=" "-=" "*=" "/=" "%=" "^="
;                 "&=" "|="
;                 "<<" ">>" ">>=" "<<=" "==" "!=" "<=" ">="
;                 ">>>="
;                 "&&" "||" "++" "--"))

;; Groovy allows newline to terminate a statement unlike Java and like Awk.  We draw on the Awk
;; Mode `Virtual semicolon material.  The idea is to say when an EOL is a `virtual semicolon,
;; i.e. a statement terminator.

(c-lang-defconst c-stmt-delim-chars
                 groovy ";{}\n\r")

(c-lang-defconst c-stmt-delim-chars-with-comma
                 groovy ";,{}\n\r")

(defun c-groovy-at-vsemi-p () nil)

(c-lang-defconst c-at-vsemi-p-fn
                 groovy 'c-groovy-at-vsemi-p)

(defun c-groovy-vsemi-status-inknown-p () nil)

(c-lang-defconst c-vsemi-status-unknown-p-fn
                 groovy 'c-groovy-vsemi-status-unknown-p)

;; Groovy allows use of `<%' and `%>' in template expressions.
;(c-lang-defconst c-other-op-syntax-tokens
;  groovy '( "<%" "%>" ))

;; Groovy does not allow the full set of Java words in category and, of course, there is the
;; `def' modifier which Groovy introduces to support dynamic typing.  Should `const' be treated
;; as reserved here as it is in Java.
(c-lang-defconst c-modifier-kwds
                 groovy '( "abstract" "def" "final" "private" "protected" "public" "static" "synchronized" ))

;;  Java does not define these pseudo-kewords as keywords, why not?

;; Constant keywords
;(c-lang-defconst c-constant-kwds
;  groovy '( "true" "false" "null" ))

;;  Why does Java mode not put `super' into the `c-primary-expr-kwds?

;; Keywords that start "primary expressions."
;(c-lang-defconst c-primary-expr-kwds
;  groovy '( "this" "super" ))

;;  Groovy does not allow anonymous classes as Java does.
(c-lang-defconst c-inexpr-class-kwds
                 groovy nil)

(c-lang-defconst c-inexpr-brace-list-kwds
                 groovy nil)

;; We need to include the "as" for the cast and "in" for for.
(c-lang-defconst c-other-kwds
                 groovy '( "in" "as" ))

;;  Groovy can overload operators where Java cannot.
(c-lang-defconst c-overloadable-operators
                 groovy '("+" "-" "*" "/" "%" "&" "|" "^"
                          "<<" ">>" ">>>" "==" "!=" ">" "<" ">=" "<="
                          "+=" "-=" "*=" "/=" "&=" "|=" "^="
                          "<<=" ">>=" ">>>="))

(defconst groovy-font-lock-keywords-1 (c-lang-const c-matchers-1 groovy)
  "Minimal highlighting for Groovy mode.
Fontifies nothing except the syntactic fontification of strings and
comments.")

(defconst groovy-font-lock-keywords-2 (c-lang-const c-matchers-2 groovy)
  "Fast normal highlighting for Groovy mode.
In addition to `java-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, the
user defined types on `java-font-lock-extra-types', and the doc
comment styles specified by `c-doc-comment-style'.")

(defconst groovy-font-lock-keywords-3 (c-lang-const c-matchers-3 groovy)
  "Accurate normal highlighting for Groovy mode.
Like `java-font-lock-keywords-2' but detects declarations in a more
accurate way that works in most cases for arbitrary types without the
need for `java-font-lock-extra-types'.")

(defvar groovy-font-lock-keywords groovy-font-lock-keywords-3
  "Default expressions to highlight in Groovy mode.")

(defun groovy-font-lock-keywords-2 ()
  (c-compose-keywords-list groovy-font-lock-keywords-2))
(defun groovy-font-lock-keywords-3 ()
  (c-compose-keywords-list groovy-font-lock-keywords-3))
(defun groovy-font-lock-keywords ()
  (c-compose-keywords-list groovy-font-lock-keywords))

(defvar groovy-mode-syntax-table nil
  "Syntax table used in Groovy mode buffers.")
(or groovy-mode-syntax-table
    (setq groovy-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table groovy))))

(defvar groovy-mode-abbrev-table nil
  "Abbreviation table used in groovy-mode buffers.")
(c-define-abbrev-table 'groovy-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the syntactic context, and which
  ;; therefore should trigger reindentation when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))

(defvar groovy-mode-map ()
  "Keymap used in groovy-mode buffers.")
(if groovy-mode-map
    nil
  (setq groovy-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for Java
  )

;(easy-menu-define c-groovy-menu groovy-mode-map "Groovy Mode Commands"
;		  (cons "Groovy" (c-lang-const c-mode-menu groovy)))

;;; Autoload mode trigger
;(add-to-list 'auto-mode-alist '("\\.groovy" . groovy-mode))

;; Custom variables
(defcustom groovy-mode-hook nil
  "*Hook called by `groovy-mode'."
  :type 'hook
  :group 'c)

;;; The entry point into the mode
(defun groovy-mode ()
  "Major mode for editing Groovy code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `groovy-mode-hook'.

Key bindings:
\\{groovy-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table groovy-mode-syntax-table)
  (setq major-mode 'groovy-mode
	mode-name "Groovy"
	local-abbrev-table groovy-mode-abbrev-table
	abbrev-mode t)
  (use-local-map groovy-mode-map)
  (c-init-language-vars groovy-mode)
  (c-common-init 'groovy-mode)
  ;;(easy-menu-add groovy-menu)
  ;;(cc-imenu-init cc-imenu-groovy-generic-expression)
  (c-run-mode-hooks 'c-mode-common-hook 'groovy-mode-hook)
  (c-update-modeline))

(provide 'groovy-mode)

;;; groovy-mode.el ends here
