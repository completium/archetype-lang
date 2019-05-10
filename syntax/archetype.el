;; a simple major mode, archetype-mode

(defvar archetype-mode-syntax-table nil "Syntax table for `archetype-mode'.")

(setq archetype-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        (modify-syntax-entry ?\( ". 1" synTable)
        (modify-syntax-entry ?\) ". 4" synTable)
        (modify-syntax-entry ?* ". 23" synTable)
        synTable))

(defvar archetype-mode-map nil
  "Keymap for Archetype major mode")

(defvar archetype-keywords
  '("archetype" "constant" "variable" "identified" "sorted" "by" "as" "from" "to" "on" "ref" "fun" "initialized" "collection" "queue" "stack" "set" "partition" "asset" "match" "with" "end" "assert" "enum" "states" "initial" "invariant" "transition" "action" "when" "called" "condition" "verification" "predicate" "definition" "axiom" "theorem" "specification" "effect" "function" "ensure" "let" "otherwise" "if" "then" "else" "for" "in" "break" "transfer" "back" "extension" "namespace" "contract" "and" "or" "not" "forall" "exists" "true" "false" "failif" "require"))

(defvar archetype-constants
  '("state" "now" "transferred" "caller" "fail" "balance" "conditions" "effects" "none" "any" "transfer" "mem" "idem" "before" "after" "fixed" "subset" "default" "type"))

(defun archetype-regexp-opt (l)
  (regexp-opt l 'symbols))

;;(setq archetype-ext-regexp "\\[%*[a-z A-z][a-z A-z _]+\\]")
(setq archetype-ext-regexp "\\[%[^]]*\\]")
(setq archetype-var-regexp "<%[a-z A-z][a-z A-z _]+>")
(setq archetype-address-regexp "@[0-9 a-z A-z _]+")
(setq archetype-number-regexp "[0-9]+")
(setq archetype-float-regexp "[0-9]+\\.[0-9]+")
(setq archetype-label-regexp "^[ ]*[a-zA-z][a-zA-z0-9_]+[ ]*\\:[ ]+")
(setq archetype-label2-regexp "\\:[a-zA-z][a-zA-z0-9_]+[ ]*\\:")
;;(setq archetype-date-regexp "(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0[1-9])-(3[01]|0[1-9]|[12][0-9])T(2[0-3]|[01][0-9]):([0-5][0-9]):([0-5][0-9])(.[0-9]+)?(Z)?")
;;(setq archetype-duration-regexp "[0-9]+Y")
;;(setq archetype-date-regexp
;;(concat
;; (format-time-string "%Y-%m-%dT%T")
;; ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
;;  (format-time-string "%z")))
;;)

;; Faces for Font Lock
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html
(defconst archetype-font-lock-keywords-1
  (list
   `(,archetype-ext-regexp . font-lock-preprocessor-face)
   `(,archetype-var-regexp . font-lock-variable-name-face)
;;   `(,archetype-date-regexp . font-lock-builtin-face)
;;   `(,archetype-duration-regexp . font-lock-builtin-face)
   `(,archetype-address-regexp . font-lock-builtin-face)
;;   `(,archetype-number-regexp . font-lock-builtin-face)
;;   `(,archetype-float-regexp . font-lock-builtin-face)
   ;; logical function : "mem" "idem" "before" "after" "fixed" "subset"
   `(,archetype-label-regexp . font-lock-preprocessor-face)
   `(,archetype-label2-regexp . font-lock-preprocessor-face)

   `( ,(archetype-regexp-opt archetype-keywords) . font-lock-keyword-face)
   `( ,(archetype-regexp-opt archetype-constants) . font-lock-constant-face)


;;   `(,(archetype-regexp-opt '("state" "now" "transferred" "caller" "fail" "balance" "conditions" "effects" "none" "any" "transfer")) . font-lock-constant-face)
;;   `(,(archetype-regexp-opt '("use" "model" "extension" "constant" "variable" "role" "asset" "states" "enum" "action" "function" "namespace" "contract")) . font-lock-type-face)
;;   `(,(archetype-regexp-opt '("identified" "sorted" "by" "as" "from" "to" "match" "with" "end" "ref" "fun" "=>" "initialized" "collection" "queue" "stack" "set" "partition" "asset" "initial" "args" "called" "transition" "condition" "effect" "specification" "invariant" "ensure" "let" "if" "then" "else" "for" "in" "break" "of" "back")) . font-lock-keyword-face)
;;   `(,(archetype-regexp-opt '("and" "or" "not" "->" "<->" "forall" "exists" "assert" "true" "false")) . font-lock-doc-face)

   )
  "Minimal highlighting for archetype mode")

(defvar archetype-font-lock-keywords archetype-font-lock-keywords-1
  "Default highlighting for archetype mode")

(defvar archetype-indent 2
  "How many spaces to indent in archetype mode.")
(make-variable-buffer-local 'archetype-indent)

(defun archetype-indent-line ()
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

(define-derived-mode archetype-mode fundamental-mode "archetype"
  "major mode for editing archetype language code."
  (set (make-local-variable 'font-lock-defaults) '(archetype-font-lock-keywords))
  ; indentation
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'archetype-indent-line)
  ; providing the mode
  (setq major-mode 'archetype-mode)
  (setq mode-name "Archetype")
  (use-local-map archetype-mode-map)
  (font-lock-mode 1)
)

(provide 'archetype)
