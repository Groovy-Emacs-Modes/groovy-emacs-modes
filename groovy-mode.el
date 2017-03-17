;;; groovy-mode.el --- Major mode for Groovy source files

;;  Copyright © 2006, 2009–2010, 2012–2016  Russel Winder

;;  Author: Russel Winder <russel@winder.org.uk>, 2006–
;;	Jim Morris <morris@wolfman.com>, 2009–
;;  Maintainer:  Russel Winder <russel@winder.org.uk>
;;  Created: 2006-08-01
;;  Keywords: languages

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Usage:
;; If you install using the packaging system no further set up should be needed. If you install this mode
;; manually then you will likely need to put these lines in your init file:
;;
;;   (autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
;;   (add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))

;;; Commentary:
;;  This mode was initially developed using the Java and Awk modes that are part of CC Mode (the 5.31 source
;;  was used) and C# Mode from Dylan R. E. Moonfire <contact@mfgames.com> (the 0.5.0 source was used).  This
;;  code may contain some code fragments from those sources that was cut-and-pasted then edited.  All other
;;  code was newly entered by the author.  Obviously changes have been made since then.

;;; Bugs:
;;  Bug tracking is currently handled using the GitHub issue tracker at
;;  https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes/issues

;;; Versions:
;;  This mode is available on MELPA which tracks the mainline Git repository on GitHub, so there is a rolling release
;;  system based on commits to the mainline.

;;; Notes:
;;  Should we support GString / template markup ( e.g. `<%' and `%>') specially?

;;;  TODO:
;;   Issues with this code are managed via the project issue management
;;   on GitHub: https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes/issues?state=open

;; History:
;;   History is tracked in the Git repository rather than in this file.
;;   See https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes/commits/master

;;----------------------------------------------------------------------------
;;; Code:

(defvar groovy-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Operators
    ;; http://docs.groovy-lang.org/latest/html/documentation/#groovy-operators
    (dolist (i '(?+ ?- ?* ?/ ?% ?& ?| ?^ ?! ?< ?> ?~ ?@ ?=))
      (modify-syntax-entry i "." table))
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?' "\"" table)
    ;; Comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)
    table)
  "Syntax table used in Groovy mode buffers.")

;;;###autoload (add-to-list 'auto-mode-alist '("\\.g\\(?:ant\\|roovy\\|radle\\)\\'\\|Jenkinsfile\\'" . groovy-mode))
;;;###autoload (add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

(defconst groovy-type-regexp
  (rx symbol-start
      (group
       (or
        ;; Treat Foo, FooBar and FFoo as type names, but not FOO.
        (seq (+ upper) lower (0+ (or (syntax symbol) (syntax word))))
        "byte"
        "short"
        "int"
        "long"
        "float"
        "double"
        "boolean"
        "char"
        "void"))
      symbol-end
      (? "[]"))
  "Matches types, where the name is first group.")

(defvar groovy-symbol-regexp
  (rx
   symbol-start
   (group (+ (or (syntax word) (syntax symbol))))
   symbol-end)
  "A variable name or a type name.")

(defvar groovy-function-regexp
  (rx-to-string
   `(seq
     line-start (0+ space)
     ;; A function may start with 'public static final'
     (1+
      (or
       (seq
        (or "public" "private" "protected"
            "abstract" "final" "static"
            "synchronized" "native" "def")
        (+ space))
       ;; or it may start with a type name.
       (seq (regexp ,groovy-type-regexp) (+ space))))
     
     ;; The actual function name.
     (group (regexp ,groovy-symbol-regexp))

     ;; Require an open paren to avoid confusing with "def foo ="
     (0+ space)
     "("))
  "Matches functions and methods in groovy code.
The function name is the second group in the regexp.")

(defvar groovy-class-regexp
  "^[ \t\n\r]*\\(final\\|abstract\\|public\\|[ \t\n\r]\\)*class[ \t\n\r]+\\([a-zA-Z0-9_$]+\\)[^;{]*{"
  "Matches class names in groovy code, select match 2")

(defvar groovy-interface-regexp
  (rx-to-string
   `(seq
     line-start (0+ space)
     (0+ (or "abstract" "public") (+ space))
     "interface" (+ space)
     (group (regexp ,groovy-symbol-regexp))))
  "Matches interface names in groovy code.")

(defvar groovy-imenu-regexp
  (list (list "Functions" groovy-function-regexp 2)
        (list "Classes" groovy-class-regexp 2)
        (list "Interfaces" groovy-interface-regexp 1)
        (list "Closures" "def[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*=[ \t]*{" 1))
  "Imenu expression for Groovy")


;; For compatibility with Emacs < 24
(defalias 'groovy-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

(defvar groovy-declaration-regexp
  (rx-to-string
   `(seq
     line-start (0+ space)
     (+
      (or "def" "public" "private" "protected" "final"
          (regexp ,groovy-type-regexp))
      (+ space))
     (group (regexp ,groovy-symbol-regexp))))
  "Match 'def foo' or 'private Type foo'. The name is the second group.")

(defvar groovy-font-lock-keywords
  `((,(regexp-opt
       ;; http://docs.groovy-lang.org/latest/html/documentation/#_keywords
       '("as"
         "assert"
         "break"
         "case"
         "catch"
         "class"
         "const"
         "continue"
         "def"
         "default"
         "do"
         "else"
         "enum"
         "extends"
         "finally"
         "for"
         "goto"
         "if"
         "implements"
         "import"
         "in"
         "instanceof"
         "interface"
         "new"
         "package"
         "return"
         "super"
         "switch"
         "this"
         "throw"
         "throws"
         "trait"
         "try"
         "while"
         ;; Other strings that we want to highlight as keywords.
         "abstract"
         "final"
         "native"
         "private"
         "protected"
         "public"
         "static"
         "synchronized"
         )
       'symbols)
     . font-lock-keyword-face)
    ;; Highlight println as a keyword, but don't highlight foo.println.
    (,(rx (or line-start space)
          symbol-start (or "print" "println") symbol-end)
     . font-lock-keyword-face)
    ;; Constants
    (,(regexp-opt '("false" "null" "true") 'symbols)
     . font-lock-constant-face)
    (,(rx symbol-start "it" symbol-end)
     . font-lock-variable-name-face)
    ;; Annotations
    (,(rx "@" symbol-start (+ (or (syntax word) (syntax symbol))) symbol-end)
     . c-annotation-face)
    (,groovy-type-regexp
     1 font-lock-type-face)
    ;; Highlight function names.
    (,groovy-function-regexp 2 font-lock-function-name-face)
    ;; Highlight declarations of the form 'def foo'.
    (,groovy-declaration-regexp
     2 font-lock-variable-name-face)
    ;; Highlight variables of the form 'foo = '
    (,(rx 
       line-start (0+ space)
       (group (+ (or (syntax word) (syntax symbol))))
       (0+ space) "=")
     1 font-lock-variable-name-face)))

(defconst groovy-triple-quoted-string-regex
  (rx "\"\"\""))

(defun groovy-stringify-triple-quote ()
  "Put `syntax-table' property on triple-quoted strings."
  (let* ((string-end-pos (point))
         (string-start-pos (- string-end-pos 3))
         (ppss (prog2
                   (backward-char 3)
                   (syntax-ppss)
                 (forward-char 3))))
    (unless (nth 4 (syntax-ppss)) ;; not inside comment
      (if (nth 8 (syntax-ppss))
          ;; We're in a string, so this must be the closing triple-quote.
          ;; Put | on the last " character.
          (put-text-property (1- string-end-pos) string-end-pos
                             'syntax-table (string-to-syntax "|"))
        ;; We're not in a string, so this is the opening triple-quote.
        ;; Put | on the first " character.
        (put-text-property string-start-pos (1+ string-start-pos)
                           'syntax-table (string-to-syntax "|"))))))

(defconst groovy-syntax-propertize-function
  (syntax-propertize-rules
   (groovy-triple-quoted-string-regex
    (0 (ignore (groovy-stringify-triple-quote))))))

(defcustom groovy-indent-offset 4
  "Indentation amount for Groovy.")

(defun groovy-indent-line ()
  "Indent the current line according to the number of parentheses."
  (interactive)
  (let* ((point-offset (- (current-column) (current-indentation)))
         (syntax-bol (syntax-ppss (line-beginning-position)))
         (syntax-eol (syntax-ppss (line-end-position)))
         (multiline-string-p (nth 3 syntax-bol))
         (multiline-comment-p (nth 4 syntax-bol))
         ;; Indent according to the number of parens. If this line
         ;; ends with open parens, e.g.
         ;;     def foo() {
         ;; then we want the outer parens, hence `min'.
         ;; 
         ;; This should never be negative, unless the code contains
         ;; unbalance parens. Ensure we handle that robustly.
         (target-paren-depth (max 0 (min (car syntax-bol) (car syntax-eol)))))
    (cond
     ;; Don't try to indent the line if we're in a multiline string.
     (multiline-string-p 'noindent)
     ;; Ensure we indent
     ;; /*
     ;;  * foo
     ;;  */
     ;;  correctly.
     (multiline-comment-p
      (indent-line-to (1+ (* groovy-indent-offset target-paren-depth))))
     (t
      (indent-line-to (* groovy-indent-offset target-paren-depth))))
    ;; Point is now at the beginning of indentation, restore it
    ;; to its original position (relative to indentation).
    (when (>= point-offset 0)
      (move-to-column (+ (current-indentation) point-offset)))))

(define-derived-mode groovy-mode groovy-parent-mode "Groovy"
  "Major mode for editing Groovy code.

The hook `groovy-mode-hook' is run with no args at mode
initialization.

Key bindings:
\\{groovy-mode-map}"
  (set (make-local-variable 'font-lock-defaults)
       '(groovy-font-lock-keywords))
  (set (make-local-variable 'syntax-propertize-function)
       groovy-syntax-propertize-function)
  (setq imenu-generic-expression groovy-imenu-regexp)
  (set (make-local-variable 'indent-line-function) #'groovy-indent-line))

(provide 'groovy-mode)

;;; groovy-mode.el ends here
