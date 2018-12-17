;; a simple major mode, cml-mode

(defvar cml-mode-syntax-table nil "Syntax table for `cml-mode'.")

(setq cml-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        (modify-syntax-entry ?\( ". 1" synTable)
        (modify-syntax-entry ?\) ". 4" synTable)
        (modify-syntax-entry ?* ". 23" synTable)
        synTable))


(setq cml-highlights
      '(
        ("model\\|constant\\|role\\|asset\\|value\\|states\\|enum\\|transaction"   . font-lock-constant-face)
        ("assert\\|called by\\|args\\|condition\\|identified by\\|action\\|transition\\|from\\|to" . font-lock-function-name-face)
        )
      )




(define-derived-mode cml-mode fundamental-mode "cml"
  "major mode for editing cml language code."
  (setq font-lock-defaults (list nil))
  (setq font-lock-defaults '(cml-highlights))
)

(provide 'cml)
