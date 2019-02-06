;; a simple major mode, cml-mode

(defvar cml-mode-syntax-table nil "Syntax table for `cml-mode'.")

(setq cml-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        (modify-syntax-entry ?\( ". 1" synTable)
        (modify-syntax-entry ?\) ". 4" synTable)
        (modify-syntax-entry ?* ". 23" synTable)
        synTable))

(defun cml-regexp-opt (l)
  (regexp-opt l 'symbols))

;;(setq cml-ext-regexp "\\[%*[a-z A-z][a-z A-z _]+\\]")
(setq cml-ext-regexp "\\[%[^]]*\\]")
(setq cml-var-regexp "<%[a-z A-z][a-z A-z _]+>")
(setq cml-address-regexp "@[0-9 a-z A-z _]+")


;; Faces for Font Lock
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html
(defconst cml-font-lock-keywords-1
  (list
   `(,cml-ext-regexp . font-lock-preprocessor-face)
   `(,cml-var-regexp . font-lock-variable-name-face)
   `(,cml-address-regexp . font-lock-builtin-face)
   `(,(cml-regexp-opt '("state" "now" "transferred")) . font-lock-constant-face)
   `(,(cml-regexp-opt '("use" "model" "extension" "constant" "variable" "role" "asset" "states" "enum" "transition" "transaction" "function" "object" "key" "namespace" "contract")) . font-lock-type-face)
   `(,(cml-regexp-opt '("identified" "sorted" "by" "as" "from" "to" "with" "ref" "fun" "=>" "initialized" "collection" "queue" "stack" "set" "partition" "asset" "object" "initial" "args" "called" "condition" "action" "specification" "let" "if" "then" "else" "for" "in" "break" "of" "transfer" "back")) . font-lock-keyword-face)
   `(,(cml-regexp-opt '("and" "or" "not" "->" "<->" "forall" "exists" "assert" "invariant" "true" "false")) . font-lock-doc-face)

   )
  "Minimal highlighting for Cml mode")

(defvar cml-font-lock-keywords cml-font-lock-keywords-1
  "Default highlighting for Cml mode")

(define-derived-mode cml-mode fundamental-mode "cml"
  "major mode for editing cml language code."
  (set (make-local-variable 'font-lock-defaults) '(cml-font-lock-keywords))
)

(provide 'cml)
