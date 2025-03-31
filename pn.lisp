;;;; Implement a Pattern Number struct

(defstruct pn
  (value 1)
)

; Functions automatically created by defstruct:
;
; Most used:
;   (pn-<field name> <instance>) returns struct field.
;   (pn-p <instance>) -> t
;
; Least used:
;   (type-of <instance>) -> pn
;   (typep <instance> 'pn) -> t
;
; Don't use:
;   (make-pn [:<field-name> <field-value>]*).
;   (copy-pn <instance>) copies a pn instance.

; Use eq and neq (macro) for simple comparisons of pn values.

(defvar *pn-one*  (make-pn :value 1))
(defvar *pn-two*  (make-pn :value 2))
(defvar *pn-none* (make-pn :value 3))

(defun pn-str (pnx)
  (assert (pn-p pnx))

  (if (eq pnx *pn-one*) "One "
      (if (eq pnx *pn-two*) "Two "
 	 (if (eq pnx *pn-none*) "None")))
)

(defun pn-gt (pnx pny)
  (assert (pn-p pnx))
  (assert (pn-p pny))

  (> (pn-value pnx) (pn-value pny))
)

(defun pn-lt (pnx pny)
  (assert (pn-p pnx))
  (assert (pn-p pny))

  (< (pn-value pnx) (pn-value pny))
)

(defun pn-eq (pnx pny)
  ;(format t "~&pn-eq: pnx ~A pny ~A" (type-of pnx) (type-of pny))
  (assert (pn-p pnx))
  (assert (pn-p pny))

  (= (pn-value pnx) (pn-value pny))
)

(defun pn-ne (pnx pny)
  (assert (pn-p pnx))
  (assert (pn-p pny))

  (/= (pn-value pnx) (pn-value pny))
)


