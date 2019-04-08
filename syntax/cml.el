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

(defvar cml-keywords
  '("use" "model" "constant" "variable" "identified" "sorted" "by" "as" "from" "to" "on" "ref" "fun" "initialized" "collection" "queue" "stack" "set" "partition" "asset" "match" "with" "end" "assert" "object" "key" "of" "enum" "states" "initial" "invariant" "transition" "action" "when" "called" "condition" "verification" "predicate" "definition" "axiom" "theorem" "specification" "effect" "function" "ensure" "let" "otherwise" "if" "then" "else" "for" "in" "break" "transfer" "back" "extension" "namespace" "contract" "and" "or" "not" "forall" "exists" "true" "false"))

(defvar cml-constants
  '("state" "now" "transferred" "caller" "fail" "balance" "conditions" "effects" "none" "any" "transfer" "mem" "idem" "before" "after" "fixed" "subset" "default"))

(defun cml-regexp-opt (l)
  (regexp-opt l 'symbols))

;;(setq cml-ext-regexp "\\[%*[a-z A-z][a-z A-z _]+\\]")
(setq cml-ext-regexp "\\[%[^]]*\\]")
(setq cml-var-regexp "<%[a-z A-z][a-z A-z _]+>")
(setq cml-address-regexp "@[0-9 a-z A-z _]+")
(setq cml-number-regexp "[0-9]+")
(setq cml-float-regexp "[0-9]+\\.[0-9]+")
(setq cml-label-regexp "^[ ]*[a-zA-z][a-zA-z0-9_]+[ ]*\\:[ ]+")
(setq cml-label2-regexp "\\:[a-zA-z][a-zA-z0-9_]+[ ]*\\:")
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
   `(,cml-label-regexp . font-lock-preprocessor-face)
   `(,cml-label2-regexp . font-lock-preprocessor-face)

   `( ,(cml-regexp-opt cml-keywords) . font-lock-keyword-face)
   `( ,(cml-regexp-opt cml-constants) . font-lock-constant-face)


;;   `(,(cml-regexp-opt '("state" "now" "transferred" "caller" "fail" "balance" "conditions" "effects" "none" "any" "transfer")) . font-lock-constant-face)
;;   `(,(cml-regexp-opt '("use" "model" "extension" "constant" "variable" "role" "asset" "states" "enum" "action" "function" "object" "key" "namespace" "contract")) . font-lock-type-face)
;;   `(,(cml-regexp-opt '("identified" "sorted" "by" "as" "from" "to" "match" "with" "end" "ref" "fun" "=>" "initialized" "collection" "queue" "stack" "set" "partition" "asset" "object" "initial" "args" "called" "transition" "condition" "effect" "specification" "invariant" "ensure" "let" "if" "then" "else" "for" "in" "break" "of" "back")) . font-lock-keyword-face)
;;   `(,(cml-regexp-opt '("and" "or" "not" "->" "<->" "forall" "exists" "assert" "true" "false")) . font-lock-doc-face)

   )
  "Minimal highlighting for cml mode")

(defvar cml-font-lock-keywords cml-font-lock-keywords-1
  "Default highlighting for cml mode")

(defvar cml-indent 2
  "How many spaces to indent in cml mode.")
(make-variable-buffer-local 'cml-indent)

(defun cml-indent-line ()
  (interactive)
  (setq cur-col (current-column))
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((idt nil) cur-indent (cur-line (+ (count-lines 1 (point)) 1)) (cur-is nil))
      (save-excursion
        (progn
          (if (looking-at "^}.*$")
              (progn
                (re-search-backward "{")
                (forward-line -1)
                (setq cur-indent (current-column ))
                (setq idt 1)
                )
            )

          (forward-line -1)
          (while (looking-at "^$")
            (progn
              (forward-line -1)
              )
            )
          (if (or (looking-at ".*{$")
                  (looking-at ".*specification.*$")
                  (looking-at ".*effect.*$"))
              (progn
                (setq cur-indent (+ (current-indentation) default-tab-width))
                (setq idt 1)
                )
            )
          (if (not idt)
              (progn
                (setq cur-indent (current-indentation))
                (setq idt 1)
                )
            )
          )
        )
      (let ((d))
        (progn
          (setq d (- cur-col (current-indentation)))
          (if idt
              (indent-line-to cur-indent)
            (indent-line-to 0)
            )
          (if (> d 0)
              (forward-char d)
            )
          (if (< (current-column) cur-indent)
              (move-to-column cur-indent)
            )
          )
        )
      )
    )
  )

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
