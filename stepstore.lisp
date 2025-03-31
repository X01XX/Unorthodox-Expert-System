; Implement a store of steps.




; Implement a store of steps.
(defstruct stepstore
  steps  ; A list of zero, or more, non-duplicate, same number bits, steps.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (stepstore-<field name> <instance>) -> struct field.
;   (stepstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> stepstore
;   (typep <instance> 'stepstore) -> bool
;
; Probably shouldn't use:
;   (make-stepstore [:<field-name> <field-stepstore>]*), use stepstore-new instead.
;   (copy-stepstore <instance>) copies a stepstore instance.
(defun stepstore-new (steps) ; -> stepstore.
  ;(format t "~&steps ~A" steps)
  (assert (step-list-p steps))

  (make-stepstore :steps  steps)
)

; Push a new step into a stepstore, suppress dups, subsets.
; Return true if the step has been added.
(defun stepstore-push (storex stpx) ; -> nothing, side-effect stepstore is changed.
  ;(format t "~&stepstore-push store ~A step ~A" storex stpx)
  (assert (stepstore-p storex))
  (assert (step-p stpx))

  (push stpx (stepstore-steps storex))
)

; Return the number of steps in a stepstore.
(defun stepstore-length (storex) ; -> number.
  (assert (stepstore-p storex))

  (length (stepstore-steps storex))
)

; Return true if a stepstore is empty.
(defun stepstore-is-empty (storex) ; -> bool
  (assert (stepstore-p storex))

  (zerop (stepstore-length storex))
)

; Return true if a stepstore is not empty.
(defun stepstore-is-not-empty (storex) ; -> bool
  (assert (stepstore-p storex))

  (plusp (stepstore-length storex))
)

; Return a string representing a stepstore.
(defun stepstore-str (storex) ; -> string.
  (assert (stepstore-p storex))

  (when (stepstore-is-empty storex)
    (return-from stepstore-str "(steps: NIL)")
  ) 
  (let ((ret "(steps: ") (start t))

    (loop for stpx in (stepstore-steps storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))    

      (setf ret (concatenate 'string ret (format nil " ~&  ~A" (step-str stpx))))
    )
    (setf ret (concatenate 'string ret ")"))

    ret
  )
)

; Return true if a stepstore contains a given step.
(defun stepstore-member (storex stpx) ; -> bool
  (assert (stepstore-p storex))
  (assert (step-p stpx))

  (if (member stpx (stepstore-steps storex) :test #'step-eq) true false)
)

;;; Return the first step of a non-empty stepstore.
(defun stepstore-first-step (storex) ; -> step
  (assert (stepstore-p storex))
  (assert (stepstore-is-not-empty storex))

  (car (stepstore-steps storex))
)

;;; Return the last step of a non-empty stepstore.
(defun stepstore-last-step (storex) ; -> step
  (assert (stepstore-p storex))
  (assert (stepstore-is-not-empty storex))

  (car (last (stepstore-steps storex)))
)

;;; Return the number of bits used is elements of a non-empty stepstore.
;;; Return the number of bits used is elements of a non-empty stepstore.
(defun stepstore-num-bits (stpstrx) ; -> integer ge 1.
  (assert (stepstore-p stpstrx))
  (assert (stepstore-is-not-empty stpstrx))

  (step-num-bits (stepstore-first-step stpstrx))
)
