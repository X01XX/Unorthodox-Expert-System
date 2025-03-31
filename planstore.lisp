; Implement a store of plans.




; Implement a store of plans.
(defstruct planstore
  plans  ; A list of zero, or more, plans.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (planstore-<field name> <instance>) -> struct field.
;   (planstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> planstore
;   (typep <instance> 'planstore) -> bool
;
; Probably shouldn't use:
;   (make-planstore [:<field-name> <field-planstore>]*), use planstore-new instead.
;   (copy-planstore <instance>) copies a planstore instance.

;;; Return a new planstore instance, from a list of plans.
(defun planstore-new (plans) ; -> planstore.
  ;(format t "~&plans ~A" plans)
  (assert (plan-list-p plans))

  (make-planstore :plans plans)
)

;;; Add plan to the end of a planstore.
(defun planstore-add-end (storex plnx) ; -> nothing, side-effect planstore changed.
  (assert (planstore-p storex))
  (assert (plan-p plnx))

  (setf (planstore-plans storex) (append (planstore-plans storex) (list plnx)))
)

;;; Return the number of plans in a planstore.
(defun planstore-length (storex) ; -> number.
  (assert (planstore-p storex))

  (length (planstore-plans storex))
)

;;; Return true if a planstore is empty.
(defun planstore-is-empty (storex) ; -> bool
  (assert (planstore-p storex))

  (zerop (planstore-length storex))
)

;;; Return true if a planstore is not empty.
(defun planstore-is-not-empty (storex) ; -> bool
  (assert (planstore-p storex))

  (plusp (planstore-length storex))
)

;;; Return a string representing a planstore.
(defun planstore-str (storex) ; -> string.
  (assert (planstore-p storex))

  (let ((ret "#S(PLST ") (start t))

    (loop for plnx in (planstore-plans storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))

      (setf ret (concatenate 'string ret (plan-str plnx)))
    )
    (if (zerop (planstore-length storex))
      (setf ret (concatenate 'string ret "NIL)"))
      (setf ret (concatenate 'string ret ")"))
    )
    ret
  )
)

