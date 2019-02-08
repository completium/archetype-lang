;; a simple major mode, cml-mode

(defvar cml-mode-syntax-table nil "Syntax table for `cml-mode'.")

(setq cml-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        (modify-syntax-entry ?\( ". 1" synTable)
        (modify-syntax-entry ?\) ". 4" synTable)
        (modify-syntax-entry ?* ". 23" synTable)
        synTable))

(defvar cml-mode-map nil
  "Keymap for Cml major mode")

(defun cml-regexp-opt (l)
  (regexp-opt l 'symbols))

;;(setq cml-ext-regexp "\\[%*[a-z A-z][a-z A-z _]+\\]")
(setq cml-ext-regexp "\\[%[^]]*\\]")
(setq cml-var-regexp "<%[a-z A-z][a-z A-z _]+>")
(setq cml-address-regexp "@[0-9 a-z A-z _]+")
(setq cml-number-regexp "[0-9]+")
(setq cml-float-regexp "[0-9]+\\.[0-9]+")
;;(setq cml-date-regexp "(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[01]|0[1-9]|[12][0-9])T(2[0-3]|[01][0-9]):([0-5][0-9]):([0-5][0-9])(.[0-9]+)?(Z)?")
;;(setq cml-duration-regexp "[0-9]+Y")
;;(setq cml-date-regexp
;;(concat
;; (format-time-string "%Y-%m-%dT%T")
;; ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
;;  (format-time-string "%z")))
;;)

;; Faces for Font Lock
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html
(defconst cml-font-lock-keywords-1
  (list
   `(,cml-ext-regexp . font-lock-preprocessor-face)
   `(,cml-var-regexp . font-lock-variable-name-face)
;;   `(,cml-date-regexp . font-lock-builtin-face)
;;   `(,cml-duration-regexp . font-lock-builtin-face)
   `(,cml-address-regexp . font-lock-builtin-face)
;;   `(,cml-number-regexp . font-lock-builtin-face)
;;   `(,cml-float-regexp . font-lock-builtin-face)
   ;; logical function : "mem" "idem" "before" "after" "fixed" "subset"
   `(,(cml-regexp-opt '("state" "now" "transferred" "caller" "fail" "balance" "conditions" "actions" "none")) . font-lock-constant-face)
   `(,(cml-regexp-opt '("use" "model" "extension" "constant" "variable" "role" "asset" "states" "enum" "transaction" "function" "object" "key" "namespace" "contract")) . font-lock-type-face)
   `(,(cml-regexp-opt '("identified" "sorted" "by" "as" "from" "to" "with" "ref" "fun" "=>" "initialized" "collection" "queue" "stack" "set" "partition" "asset" "object" "initial" "args" "called" "transition" "condition" "action" "specification" "invariant" "ensure" "let" "if" "then" "else" "for" "in" "break" "of" "transfer" "back")) . font-lock-keyword-face)
   `(,(cml-regexp-opt '("and" "or" "not" "->" "<->" "forall" "exists" "assert" "true" "false")) . font-lock-doc-face)

   )
  "Minimal highlighting for cml mode")

(defvar cml-font-lock-keywords cml-font-lock-keywords-1
  "Default highlighting for cml mode")

(defvar cml-indent 2
  "How many spaces to indent in cml mode.")
(make-variable-buffer-local 'cml-indent)


(defun cml-indent-line ()
  "Indent current line as cml logic"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "model")  ; Check for rule 1
        (indent-line-to 0)
      )))

(define-derived-mode cml-mode fundamental-mode "cml"
  "major mode for editing cml language code."
  (set (make-local-variable 'font-lock-defaults) '(cml-font-lock-keywords))
  ; indentation
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'cml-indent-line)
  ; providing the mode
  (setq major-mode 'cml-mode)
  (setq mode-name "Cml")
  (use-local-map cml-mode-map)
  (font-lock-mode 1)
)

(provide 'cml)
