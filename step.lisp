



;;;; Implement the Step type.
;;;;
(defstruct step
  act-id	; An action ID, GE zero.
  rule		; A rule.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (step-<field name> <instance>) returns struct field.
;   (step-p <instance>) -> t
;
; Least used:
;   (type-of <instance>) -> step
;   (typep <instance> 'step) -> t
;
; Don't use:
;   (make-step [:<field-name> <field-value>]*), use step-new instead.
;   (copy-step <instance>) copies a step instance.

;;; Return a new step.
;;; A nil act-id indicates it will be assigned later.
(defun step-new (&key act-id rule)
  (assert (rule-p rule))
  (assert (and (integerp act-id) (>= act-id 0)))

  (make-step :act-id act-id :rule rule)
)

;;; Return a string representing a step
(defun step-str (stpx)
    (assert (step-p stpx))

    (let ((str "#S(STEP "))
        (setf str (concatenate 'string str (format nil "act-id ~D" (step-act-id stpx))))
        (setf str (concatenate 'string str (format nil " rule ~A" (rule-str (step-rule stpx)))))
        (setf str (concatenate 'string str ")"))
        str
    )
)

;;; Return true if the argument is a list of steps, or nil.
(defun step-list-p (steps) ; -> bool
  ;(format t "~&step-list-p: ~A ~A" (type-of steps) steps)
  (if (not (listp steps))
    (return-from step-list-p false))

  ;; Check for a non-step.
  (loop for stpx in steps do
    (if (not (step-p stpx))
      (return-from step-list-p false))
  )
  true
)

;;; Return true if two steps ar equal.
(defun step-eq (stp1 stp2) ; -> bool
  (assert (step-p stp1))
  (assert (step-p stp2))

  (and (= (step-act-id stp1) (step-act-id stp2))
       (rule-eq (step-rule stp1) (step-rule stp2)))
)

;;; Return the number of bits used by elements of a step.
(defun step-num-bits (stpx) ; -> integer, ge 1.
  (assert (step-p stpx))

  (rule-num-bits (step-rule stpx))
)

;;; Return the initial region of a step.
(defun step-initial-region (stepx) ; -> region.
  (assert (step-p stepx))

  (rule-initial-region (step-rule stepx))
)

;;; Return the result region of a step.
(defun step-result-region (stepx) ; -> region.
  (assert (step-p stepx))

  (rule-result-region (step-rule stepx))
)

;;; Return a step with its rule initial region restricted bf a given region.
(defun step-restrict-initial-region (stepx regx) ; -> step
  (let ((new-rule (rule-restrict-initial-region (step-rule stepx) regx)))

    (make-step :act-id (step-act-id stepx) 
  	       :rule new-rule 
    )
  )
)

;;; Return a step with its rule result region restricted bf a given region.
(defun step-restrict-result-region (stepx regx) ; -> step
  (let ((new-rule (rule-restrict-result-region (step-rule stepx) regx)))

    (make-step :act-id (step-act-id stepx) 
  	       :rule new-rule 
    )
  )
)

