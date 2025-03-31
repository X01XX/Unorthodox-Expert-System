;;;; Implement the plan struct and functions.




;;; The plan struct.
(defstruct plan
  stepstore        ; A store of steps to go from one state to another.  May be empty.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (plan-<field name> <instance>) -> struct field.
;   (plan-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> plan
;   (typep <instance> 'plan) -> bool
;
; Probably shouldn't use:
;   (make-plan [:<field-name> <field-plan>]*), use plan-new instead.
;   (copy-plan <instance>) copies a plan instance.

;;; Return a new plan, made up of zero, or more, steps.
(defun plan-new (steps) ; -> plan.
  (assert (step-list-p steps))

  (let ((planx (make-plan :stepstore (stepstore-new steps))))
    ;(format t "~& plan-new: planx ~A" planx)
    (assert (plan-is-valid planx))
    planx
  )
)

;;; Return a list of steps representing a plan.
(defun plan-step-list (planx) ; -> a list of steps.
  (assert (plan-p planx))

  (stepstore-steps (plan-stepstore planx))
)

;;; Return a string representing a plan.
(defun plan-str (planx) ; -> A string.
  (assert (plan-p planx))

  (let ((strs "#S(PLAN ") (at-start true))

    (loop for stepx in (plan-step-list planx) do
      (when at-start
        (setf at-start nil)
        (setf strs (concatenate 'string strs (format nil "~A" (region-str-bits (step-initial-region stepx))))))
   
      (setf strs (concatenate 'string strs (format nil "-~D->~A" (step-act-id stepx) (region-str-bits (step-result-region stepx)))))
    )
    (setf strs (concatenate 'string strs ")"))
    strs
  )
)

;;; Return true if a plan is empty.
(defun plan-is-empty (planx) ; -> bool
  (assert (plan-p planx))

  (stepstore-is-empty (plan-stepstore planx))
)

;;; Return true if a plan is not empty.
(defun plan-is-not-empty (aplan) ; -> bool
  (assert (plan-p aplan))

  (stepstore-is-not-empty (plan-stepstore aplan))
)

;;; Return the number of steps in a plan.
(defun plan-length (planx) ; -> an integer.
  (assert (plan-p planx))

  (stepstore-length (plan-stepstore planx))
)

;;; Return the first step of a non-empty plan.
(defun plan-first-step (planx) ; -> step.
  (assert (plan-p planx))

  (stepstore-first-step (plan-stepstore planx))
)

;;; Return the last step of a non-empty plan.
(defun plan-last-step (planx) ; -> step.
  (assert (plan-p planx))

  (stepstore-last-step (plan-stepstore planx))
)

;;; Return true if a plan is valid.
;;; That is, all steps link together.
(defun plan-is-valid (planx) ; -> bool
  ;(format t "~&plan-is-valid: ~A" planx)
  (assert (plan-p planx))

  (if (< (plan-length planx) 2)
    (return-from plan-is-valid true))

  ;; Check that all steps are linked together.
  (let ((last-step))
    (loop for stepx in (plan-step-list planx) do
        (when last-step
	  (when (region-ne (step-result-region last-step) (step-initial-region stepx))
	    (format t "~&~A -> ~A ?" (step-result-region last-step) (step-initial-region stepx))
            (return-from plan-is-valid false))
	)
	(setf last-step stepx)
    )
  )
  ;; Check that plan goes somewhere.
  (if (region-eq (step-initial-region (plan-first-step planx)) (step-result-region (plan-last-step planx)))
    (return-from plan-is-valid false))

  true
)

;; Return true if a list is a list of plans.
;;; An empty list will return true.
(defun plan-list-p (plnlst) ; -> bool
  ;(format t "~&plan-list-p: ~A" plnlst)
  (if (not (listp plnlst))
    (return-from plan-list-p false))

  (loop for plnx in plnlst do
    (if (not (plan-p plnx))
      (return-from plan-list-p false))
  )
  true
)

;;; Return the initial region of a plan.
(defun plan-initial-region (aplan) ; -> region, or nil.
  (assert (plan-p aplan))

  (if (plan-is-empty aplan)
    (return-from plan-initial-region nil))

  (step-initial-region (plan-first-step aplan))
)

;;; Return the result region of a plan.
(defun plan-result-region (aplan) ; -> region, or nil.
  (assert (plan-p aplan))

  (if (plan-is-empty aplan)
    (return-from plan-result-region nil))

  (step-result-region (plan-last-step aplan))
)

;;; Return plan with the initial region restricted.
;;; The restriction region should intersect the initial region of the plan,
;;; but does not have to be a subset.
(defun plan-restrict-initial-region (aplan regx) ; -> plan, or nil.
  (assert (plan-p aplan))
  (assert (region-p regx))
  (assert (plan-is-valid aplan))

  (if (or (plan-is-empty aplan) (not (region-intersects regx (plan-initial-region aplan))))
    (return-from plan-restrict-initial-region nil))

  (let ((cur-reg regx) steps temp-step)

    (loop for stepx in (plan-step-list aplan) do

      (if (not (region-intersects cur-reg (step-initial-region stepx)))
        (return-from plan-restrict-initial-region nil))

      (setf temp-step (step-restrict-initial-region stepx cur-reg))

      (setf cur-reg (step-result-region temp-step))

      (push temp-step steps)
    )
    (plan-new (reverse steps))
  )
)

;;; Return plan with the result region restricted.
;;; The restriction region should intersect the result region of the plan,
;;; but does not have to be a subset.
(defun plan-restrict-result-region (aplan regx) ; -> plan, or nil.
  (assert (plan-p aplan))
  (assert (region-p regx))
  (assert (plan-is-valid aplan))

  (if (or (plan-is-empty aplan) (not (region-intersects regx (plan-result-region aplan))))
    (return-from plan-restrict-result-region nil))

  (let ((cur-reg regx) steps temp-step)

    (loop for stepx in (reverse (plan-step-list aplan)) do

      (if (not (region-intersects cur-reg (step-result-region stepx)))
        (return-from plan-restrict-result-region nil))

      (setf temp-step (step-restrict-result-region stepx cur-reg))

      (setf cur-reg (step-initial-region temp-step))

      (push temp-step steps)
    )
    (plan-new steps)
  )
)

;;; Return two plans linked together, in the order given.
(defun plan-link (planx plany) ; -> plan, or nil.
  (assert (plan-p planx))
  (assert (plan-p plany))
  (assert (plan-is-valid planx))
  (assert (plan-is-valid plany))
  (assert (= (plan-num-bits planx) (plan-num-bits plany)))

  (let ((result-reg (step-result-region (plan-last-step planx)))
        (initial-reg (step-initial-region (plan-first-step plany))))

    (if (not (region-intersects result-reg initial-reg))
      (return-from plan-link nil))

    (if (region-eq result-reg initial-reg)
      (return-from plan-link (plan-new (append (plan-step-list planx) (plan-step-list plany)))))

    (let ((int-reg (region-intersection result-reg initial-reg)))

      (cond ((region-superset-of :sup result-reg :sub initial-reg)
               (plan-new (append (plan-step-list (plan-restrict-result-region planx int-reg)) (plan-step-list plany)))
	     )
            ((region-superset-of :sup initial-reg :sub result-reg)
               (plan-new (append (plan-step-list planx) (plan-step-list (plan-restrict-initial-region plany int-reg))))
	     )
	    (t
               (plan-new (append (plan-step-list (plan-restrict-result-region planx int-reg))
                                 (plan-step-list (plan-restrict-initial-region plany int-reg))))
	    )
      )
    )
  )
)

;;; Return the number of bits used to define regions in a non-empty plan.
(defun plan-num-bits (plnx) ; -> integer GT zero.
  (assert (plan-p plnx))
  (assert (plan-is-not-empty plnx))

  (region-num-bits (plan-initial-region plnx))
)

;;; Return a plan, given a string of the form region-actnum->region-actnum->region ...
(defun plan-from (plan-str) ; -> plan
  ;(format t "~&plan-from: ~A" plan-str)
  (let (token token-list token-list2)
    ; Split string into <region>-<action number> tokens, plus region at end.
    (loop for chr across plan-str do
	(if (char= chr #\>)
	  (progn
	    ;(format t "~&token: ~A" token)
	    (setf token-list (append token-list (list token)))
	    (setf token nil)
	  )
          (setf token (concatenate 'string token (string chr)))
	)
    ) ; next chr
    ;(format t "~&last token: ~A" token)
    (setf token-list (append token-list (list token)))
    
    ; sanity checks.

    ; Token list should be non-nil.
    (if (null token-list)
      (return-from plan-from nil))

    ;(format t "~&token-list: ~A" token-list)

    ; Split tokens between region and action, plus region at end.
    (let (token)
      (loop for tokenx in token-list do
         (loop for chr across tokenx do
	   (if (char= chr #\-)
	     (progn
	       ;(format t "~&token: ~A" token)
	       (setf token-list2 (append token-list2 (list token)))
	       (setf token nil)
	     )
             (setf token (concatenate 'string token (string chr)))
	   )
	 ) ;  next chr.
	 (setf token-list2 (append token-list2 (list token)))
	 (setf token nil)
      ) ; next tokenx.
      ;(format t "~&token-list2: ~A" token-list2)
    )

    ;; Tally up tokens and actions.
    (let (regions actions reg1 reg2 actx rulx stepx steps planx)
      (loop for itemx in token-list2
	    for inx from 0 do

	(if (evenp inx)
	  (progn 
	    ;(format t "~&  a region ~A" itemx)
	    (push itemx regions)
	  )
	  (progn 
	    ;(format t "~&  an action ~A" itemx)
	    (push itemx actions)
	  )
	) ; end if
	(when (and (= 2 (length regions)) (= 1 (length actions)))
	  ;(format t "~&  figure step ~A -~D-> ~A" (second regions) (car actions) (car regions))

	  (setf reg1 (region-from-str (second regions)))
	  (setf actx (parse-integer (car actions)))
	  (setf reg2 (region-from-str (car regions)))
	  (assert (= (region-num-bits reg1) (region-num-bits reg2)))
	  (setf rulx (rule-new-region-to-region reg1 reg2))
	  ;(format t "~&rule is: ~A" rulx)
	  (setf stepx (step-new :act-id actx :rule rulx))
	  ;(format t "~&step ~A" (step-str stepx))
	  (push stepx steps)

	  (setf regions (list (car regions)))
	  (setf actions nil)
	) ; end-when 
      ) ; next itemx, inx.
      (setf planx (plan-new (reverse steps)))
      ;(format t "~&plan: ~A" planx)
      planx
    )
  )
)

;;; Check use of act 0 steps.
(defun plan-act0-steps-valid (plnx) ; -> bool.
  (assert (plan-p plnx))
  
  (if (< (length (plan-step-list plnx)) 2)
    (return-from plan-act0-steps-valid true))

  (loop for stepx in (plan-step-list plnx) do
    (if (zerop (step-act-id stepx))
      (return-from plan-act0-steps-valid false))
  )
  true
)

