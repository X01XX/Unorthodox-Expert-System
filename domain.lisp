



;;;; Implement the Action type.
;;;;
(defstruct domain
  id		; A number id, GE zero.
  actions	; A actionstore.
  current-state	; The current state of the domain. Actions change this.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (domain-<field name> <instance>) returns struct field.
;   (domain-p <instance>) -> t
;
; Least used:
;   (type-of <instance>) -> domain
;   (typep <instance> 'domain) -> t
;
; Don't use:
;   (make-domain [:<field-name> <field-value>]*), use domain-new instead.
;   (copy-domain <instance>) copies a domain instance.

;;; Return a new domain.
(defun domain-new (&key id initial-state)
  (assert (state-p initial-state))
  (assert (and (integerp id) (>= id 0)))

  (let (act0 high-state low-state sample1 sample2)
    ;; Create a no-op action as action 0.
    (setf high-state (state-new-high initial-state))
    (setf low-state (state-new-low initial-state))
    (setf sample1 (sample-new :initial low-state :result low-state))
    (setf sample2 (sample-new :initial high-state :result high-state))

    (setf act0 (action-new :id 0 :rules (list (rulestore-new (list (rule-union  (rule-new sample1) (rule-new sample2)))))))
    (action-take-sample-arbitrary act0 high-state)
    (action-take-sample-arbitrary act0 low-state)
    (action-take-sample-arbitrary act0 high-state)
    (action-take-sample-arbitrary act0 low-state)
    (action-take-sample-arbitrary act0 high-state)
    (action-take-sample-arbitrary act0 low-state)

    (make-domain :id id :actions (actionstore-new (list act0)) :current-state initial-state)
  )
)

;;; Set a domain id.
(defun domain-set-id (domx id) ; -> nothing.  Side effect, domain id is changed.
  (assert (domain-p domx))
  (assert (and (integerp id) (>= id 0)))

  (setf (domain-id domx) id)
)

;;; Set the domain state.
(defun domain-set-state (domx stax) ; -> nothing.  Side effect, domain id is changed.
  (assert (domain-p domx))
  (assert (state-p stax))

  (setf (domain-current-state domx) stax)
)

;;; Return a string representing a domain
(defun domain-str (domx)
    (assert (domain-p domx))

    (let ((str "#S(DOMAIN "))
        (setf str (concatenate 'string str (format nil "id ~D current-state ~A" (domain-id domx) (state-str (domain-current-state domx)))))
        (setf str (concatenate 'string str (format nil " actions ~A" (actionstore-str (domain-actions domx)))))
        (setf str (concatenate 'string str ")"))
        str
    )
)

;;; Print a domain.
(defun domain-print (domx)
    (assert (domain-p domx))

    (format t "~&Domain ~D current-state ~A change-surface: ~A" (domain-id domx) (state-str (domain-current-state domx))
      (regionstore-str (domain-change-surface domx)))

    (actionstore-print (domain-actions domx))
)

;;; Return possible steps, given a rule.
(defun domain-get-steps (domx rule-to-goal within) ; -> stepstore.
  (assert (domain-p domx))
  (assert (rule-p rule-to-goal))
  (assert (region-p within))
  (assert (= (domain-num-bits domx) (rule-num-bits rule-to-goal)))
  (assert (= (domain-num-bits domx) (region-num-bits within)))

  (actionstore-get-steps (domain-actions domx) rule-to-goal within)
)

;;; Return the number of bits used by a domain.
(defun domain-num-bits (domx) ; -> integer, gt zero.
  (assert (domain-p domx))

  (state-num-bits (domain-current-state domx))
)

;;; Return true if a list is a list of domains.
;;; An empty list will return true.
(defun domain-list-p (domlst) ; -> bool
  ;(format t "~&domain-list-p: ~A" domlst)
  (if (not (listp domlst))
    (return-from domain-list-p false))

  (loop for domx in domlst do
    (if (not (domain-p domx))
      (return-from domain-list-p false))
  )
  true
)

;;; Return the maximum region for a domain.
(defun domain-max-region (domx) ; -> region.
  (region-new (list (domain-current-state domx) (state-new (state-not (domain-current-state domx)))))
)

;;; Return a plan to change a current region to a goal region.
(defun domain-get-plan (domx from-reg to-reg with-reg depth) ; -> plan, or nil.
  ;(format t "~&domain-get-plan: domx ~D from ~A to ~A within ~A depth ~D" (domain-id domx) (region-str from-reg) (region-str to-reg) (region-str with-reg) depth)
  (assert (domain-p domx))
  (assert (region-p from-reg))
  (assert (region-p to-reg))
  (assert (region-p with-reg))
  (assert (= (domain-num-bits domx) (region-num-bits from-reg)))
  (assert (= (domain-num-bits domx) (region-num-bits to-reg)))
  (assert (= (domain-num-bits domx) (region-num-bits with-reg)))
  (assert (region-superset-of :sup with-reg :sub from-reg))
  (assert (region-superset-of :sup with-reg :sub to-reg))

  (if (region-intersects to-reg from-reg)
    (let ((int-reg (region-intersection to-reg from-reg)))
      ;(format t "~&domain-get-plan: returning 1 act 0 plan")
      (return-from domain-get-plan (plan-new (list (step-new :act-id 0 :rule (rule-new-region-to-region int-reg int-reg)))))))

  (when (zerop depth)
    ;(format t "~&domain-get-plan: returning 2 nil")
    (return-from domain-get-plan nil))

  (let ((steps (domain-get-steps domx (rule-new-region-to-region from-reg to-reg) with-reg)) stepy)
    ;(format t "~&steps found ~A" (stepstore-str steps))
    ;; Check for one step that spans the gap.
    (let (span-steps)
      (loop for stepx in (stepstore-steps steps) do
        (when (and (region-superset-of :sup (rule-initial-region (step-rule stepx)) :sub from-reg)
                   (region-intersects (rule-result-region (step-rule stepx)) to-reg))
          (push stepx span-steps)) 
      )
      (when span-steps
	    (setf stepy (nth (random (length span-steps)) span-steps))
        ;(format t "~&domain-get-plan: returning 3 plan")
	    (return-from domain-get-plan (plan-new (list stepy)))
      )
    )

    ;; Gather steps that intersect the from-reg or two-reg.
    (let (step-list stepy planx)
      (loop for stepx in (stepstore-steps steps) do
	    ;(format t "~& rule ~A initial ~A result ~A" (step-rule stepx) (rule-initial-region (step-rule stepx))
	    ;                                                              (rule-result-region (step-rule stepx)))
	    ;(format t "~&rule initial ~A intersects ~A = ~A" (region-str (rule-initial-region (step-rule stepx))) (region-str from-reg)
	    ;    	                                         (region-intersects (rule-initial-region (step-rule stepx)) from-reg))
	    ;(format t "~&rule result ~A intersects ~A = ~A" (region-str (rule-result-region (step-rule stepx))) (region-str to-reg)
		;                                                (region-intersects (rule-result-region (step-rule stepx)) to-reg))
	    (when (or (region-intersects (rule-initial-region (step-rule stepx)) from-reg)
	              (region-intersects (rule-result-region (step-rule stepx)) to-reg))
	      (push stepx step-list))
      )
	  (when step-list
	    ;; Choose a random step.
	    (setf stepy (nth (random (length step-list)) step-list))

	    ;; Recurse to build the rest of the plan.
	    ;(format t "~&rule initial ~A intersects ~A = ~A" (region-str (rule-initial-region (step-rule stepy))) (region-str from-reg)
		;                                                 (region-intersects (rule-initial-region (step-rule stepy)) from-reg))
	    (when (region-intersects (rule-initial-region (step-rule stepy)) from-reg)
	      (setf stepy (step-restrict-initial-region stepy from-reg))
	      (if stepy
	        (progn
	          (setf planx (domain-get-plan domx (step-result-region stepy) to-reg with-reg (1- depth)))
              ;(format t "~&domain-get-plan: returning 4 plan/nil")
	          (if planx
                (return-from domain-get-plan (plan-link (plan-new (list stepy)) planx))
                (return-from domain-get-plan nil)
	          )
	        )
            (progn
              ;(format t "~&domain-get-plan: returning 5 nil")
              (return-from domain-get-plan nil)
            )
	      )
        )
	    ;(format t "~&rule result ~A intersects ~A = ~A" (region-str (rule-result-region (step-rule stepy))) (region-str to-reg)
		;                                                (region-intersects (rule-result-region (step-rule stepy)) to-reg))
	    (when (region-intersects (rule-result-region (step-rule stepy)) to-reg)
	      (setf stepy (step-restrict-result-region stepy to-reg))
          (setf planx (domain-get-plan domx from-reg (step-initial-region stepy) with-reg (1- depth)))
          ;(format t "~&domain-get-plan: returning 6 plan/nil")
	      (if planx
            (return-from domain-get-plan (plan-link planx (plan-new (list stepy)))) 
            (return-from domain-get-plan nil))
        )
	  )
      ;(format t "~&domain-get-plan: returning 7 nil")
	  (return-from domain-get-plan nil)
    ) ; end-let
  ) ; end-let
)

(defun domain-get-needs (domx) ; ->  needstore.
  ;(format t "~&domain-get-needs: ~A" (type-of domx))
  (assert (domain-p domx))

  (let ((needs (actionstore-get-needs (domain-actions domx) (domain-current-state domx) (domain-change-surface domx))))

    (needstore-set-dom-id needs (domain-id domx)) ; set needs domain-id

    ;; Find plan for each need.
    (loop for needx in (needstore-needs needs) do
        (cond ((state-p (need-target needx))
                (if (state-eq (need-target needx) (domain-current-state domx))
                  (setf (need-plan needx) (plan-new nil))
                  (setf (need-plan needx) (domain-get-plan domx (region-new (domain-current-state domx))
                                                               (region-new (need-target needx))
                                                               (domain-max-region domx) 10)))
              )
              ((region-p (need-target needx))
                (if (region-superset-of-state (need-target needx) (domain-current-state domx))
                  (setf (need-plan needx) (plan-new nil))
                  (setf (need-plan needx) (domain-get-plan domx (region-new (domain-current-state domx))
                                                               (need-target needx)
                                                               (domain-max-region domx) 10)))
              )
              (t (error "Unrecognized target type"))
        )
    )
    needs
  )
)

(defun domain-add-action (domx actx)
  (assert (domain-p domx))
  (assert (action-p actx))
  (assert (= (domain-num-bits domx) (action-num-bits actx)))

  (action-set-id actx (actionstore-length (domain-actions domx)))
  (actionstore-push (domain-actions domx) actx)    
)

;;; Return a domain instance, given a list of symbols.
(defun domain-from (symbols) ; -> domain instance.
    ;(format t "~&domain-from: ~A" (type-of symbols))
    (assert (listp symbols))
    (assert (not (null symbols)))
    (assert (symbolp (car symbols)))
    (assert (eq (car symbols) 'DOM))

    (setf symbols (cdr symbols))

    (let (actions domx actx)
        (loop for tokx in symbols do
            ;(format t "~&domain-from ~A ~A" (type-of tokx) tokx)
            (setf actx (action-from tokx))
            (action-set-id actx (length actions))
            (push actx actions)
        )
        (assert (not (null actions)))

        (setf domx (domain-new :id 0 :initial-state (state-random (action-num-bits (car actions)))))
        (loop for actx in (reverse actions) do
             (domain-add-action domx actx)
        )
        domx
    )
)

;;; Run a plan.
;;; Return false as soon as an unexpected result happens.
;;; Otherwise return true.
(defun domain-run-plan (domx planx) ; -> bool. side effect, domain may be changed.
  (assert (domain-p domx))
  (assert (plan-p planx))
  (assert (= (domain-num-bits domx) (plan-num-bits planx)))

  (if (zerop (step-act-id (plan-first-step planx)))
     (return-from domain-run-plan true))

  (let (smpl)
    (format t "~&Domain: ~D, running plan: ~A" (domain-id domx) (plan-str planx))
    (loop for stepx in (plan-step-list planx) do

      (if (region-superset-of-state (step-initial-region stepx) (domain-current-state domx))
        (progn
          (setf smpl (action-take-sample-for-step (actionstore-nth (domain-actions domx) (step-act-id stepx)) (domain-current-state domx)))
          (setf (domain-current-state domx) (sample-result smpl))
          (when (not (region-superset-of-state (step-result-region stepx) (sample-result smpl)))
            (format t "~&step result region unexpected.")
            (return-from domain-run-plan false)
          )
        )
        (progn
          (format t "~&step initial region is not a superset of the current state")
          (return-from domain-run-plan false)
        )
      ) 
    ) ; next stepx
    true
  )
)

;;; Process a need.
(defun domain-process-need (domx needx) ; -> sample instance.
   ;(format t "~&domain-process-need: ~A ~A" (type-of domx) (type-of needx)) 
   (assert (domain-p domx))
   (assert (need-p needx))
   (assert (= (domain-id domx) (need-dom-id needx)))

   (let ((act-id (need-act-id needx)) smpl)
      (if (plan-is-not-empty (need-plan needx))
        (domain-run-plan domx (need-plan needx))
      )
      (if (or
           (and (state-p (need-target needx)) (state-eq (domain-current-state domx) (need-target needx)))
           (and (region-p (need-target needx)) (region-superset-of-state (need-target needx) (domain-current-state domx)))
          )
        (progn
          (setf smpl (action-take-sample-for-need (actionstore-nth (domain-actions domx) act-id) (domain-current-state domx) needx))
          (setf (domain-current-state domx) (sample-result smpl))
        )
        (format t "~&need action not taken")
      )
   )
)

;;; Return the domain change surface.
;;; The aggregation of all action group rule initial-regions that allow a predictable change to be made.
(defun domain-change-surface (domx) ; -> regionstore.
  (assert (domain-p domx))

  (let (ret
        (max-region (region-new (list (state-new-high (domain-current-state domx)) (state-new-low (domain-current-state domx)))))
       )
    ;; Get change surface.
    (setf ret (actionstore-change-surface (domain-actions domx)))

    ;; Combine regions, like (1xxx, 0xxx) or (01x1, 11x1, x101, x111).
    (setf ret (regionstore-subtract :min-store (regionstore-new (list max-region)) :sub-store ret))
    (setf ret (regionstore-subtract :min-store (regionstore-new (list max-region)) :sub-store ret))

    ret
  )
)

