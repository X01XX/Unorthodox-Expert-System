; Implement an error struct.
(defstruct err
  str  ; Text message.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (err-<field name> <instance>) -> struct field.
;   (err-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> err
;   (typep <instance> 'err) -> bool
;
; Probably shouldn't use:
;   (make-err [:<field-name> <field-err>]*), use err-new instead.
;   (copy-err <instance>) copies a err instance.
(defun err-new (msg) ; -> err.
  (assert (stringp msg))
  (assert (> (length msg) 0))

  (make-err :str msg)
)

