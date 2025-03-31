;;;; Implement a change struct and functions.




;;; The change struct.
(defstruct change
  m01  ; 0->1 mask.
  m10  ; 1->0 mask.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (change-<field name> <instance>) -> struct field.
;   (change-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> change
;   (typep <instance> 'change) -> bool
;
; Probably shouldn't use:
;   (make-change [:<field-name> <field-change>]*), use change-new instead.
;   (copy-change <instance>) copies a change instance.

;;; Return a new change.
(defun change-new (&key m01 m10) ; -> change.
  (assert (mask-p m01))
  (assert (mask-p m10))
  (assert (= (mask-num-bits m01) (mask-num-bits m10)))

  (make-change :m01 m01 :m10 m10)
)

;;; Return a string for a change.
(defun change-str (cngx) ; -> string
  (change-p cngx)

  (let ((str "(0->1 "))
    (setf str (concatenate 'string str (value-str (mask-value (change-m01 cngx)))))
    (setf str (concatenate 'string str ", 1->0 "))
    (setf str (concatenate 'string str (value-str (mask-value (change-m10 cngx)))))
    (setf str (concatenate 'string str ")"))
    str
  )
)

;;; Return the number of bit positions set to one.
(defun change-num-changes (cngx) ; -> integer
  (change-p cngx)

  (+ (mask-num-ones (change-m01 cngx))
     (mask-num-ones (change-m10 cngx)))
)

;;; Return true if there is at least one bit set to one in a change.
(defun change-is-not-low (cngx) ; -> bool.
  (change-p cngx)

  (if (or (mask-is-not-low (change-m01 cngx))
          (mask-is-not-low (change-m10 cngx)))
    true
    false)
)

;;; Return true if there is no bit set to one in a change.
(defun change-is-low (cngx) ; -> bool.
  (change-p cngx)

  (if (and (mask-is-low (change-m01 cngx))
           (mask-is-low (change-m10 cngx)))
    true
    false)
)

;;; Return the number of bits used in change masks.
(defun change-num-bits (cngx) ; -> integer.
  (change-p cngx)

  (mask-num-bits (change-m01 cngx))
)

;;; Return a list of changes containing only one bit from a change.
(defun change-split (cngx) ; -> list of changes.
  (change-p cngx)

  (let (ret-lst m01 m10 (num-bits (change-num-bits cngx)))
    (setf m01 (mask-split (change-m01 cngx)))
    (loop for bitx in m01 do
      (push (change-new :m01 bitx :m10 (mask-new (value-new :num-bits num-bits :bits 0))) ret-lst)
    )

    (setf m10 (mask-split (change-m10 cngx)))
    (loop for bitx in m10 do
      (push (change-new :m10 bitx :m01 (mask-new (value-new :num-bits num-bits :bits 0))) ret-lst)
    )
    ret-lst
  )
)

;;; Return true if two changes ar equal.
(defun change-eq (cng1 cng2) ; -> bool.
  (change-p cng1)
  (change-p cng2)
  (assert (= (change-num-bits cng1) (change-num-bits cng2)))

  (and (mask-eq (change-m01 cng1) (change-m01 cng2))
       (mask-eq (change-m10 cng1) (change-m10 cng2)))
)
