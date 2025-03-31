;;;; Implement a need struct

;;; Define need kinds
(defvar *first-sample-of-state* 1009) ; There should be no square stored with this state.
(defvar *resample-state*        1013) ; There should be a non-pnc square stored with this state.
(defvar *sample-in-region*      1019) ; There should be no square with a state in the region.
(defvar *change-position*       1023) ; hange the current position.
(defvar *kinds* (list *first-sample-of-state* *resample-state* *sample-in-region* *change-position*))

;;; Define need reasons
(defvar *state-not-in-group* 2003)
(defvar *confirm-group* 2007)
(defvar *contradictory-intersection* 2011)
(defvar *between-ip* 2017)
(defvar *expand-group* 2019)
(defvar *avoid-negative-selectregions* 2023)
(defvar *seek-positive-selectregions* 2027)
(defvar *reasons* (list *state-not-in-group*
                        *confirm-group*
                        *contradictory-intersection*
                        *expand-group*
                        *between-ip*
                        *avoid-negative-selectregions*
                        *seek-positive-selectregions*)
   )

(defstruct need
    (dom-id 0)      ; Domain ID, integer GE 0.
    (act-id 0)      ; Action ID, integer GE 0.
    (kind 0)        ; Sample state, resample state, or sample anywhere within a region.
    (reason 0)      ; Reason for the need.
    target          ; A state or region.
    extra-info      ; Added text to explain the need.
    (plan nil)      ; Plan to position the current state to the target.
                    ; Nil means no plan to target.
                    ; An empty plan means the position is already at the target.
                    ; Planscorrstore indicates actions in multiple domains to avoid negative selectregions.
)

; Functions automatically created by defstruct:
;
; Most used:
; (need-<field name> <instance>) returns struct field.
; (need-p <instance>) -> t
;
; Least used:
; (make-need [:<field-name> <field-value>]*), use need-new instead.
; (copy-need <instance>) copies a need instance, don't use.
; (type-of <instance>) -> need
; (typep <instance> 'need) -> t

; Return a new need instance.
(defun need-new (&key (dom-id 0) (act-id 0) kind reason target (extra-info ""))
    (assert (integerp dom-id))
    (assert (integerp act-id))
    (assert (numberp kind))
    (assert (member kind *kinds*))
    (assert (numberp reason))
    (assert (member reason *reasons*))
    (assert (stringp extra-info))

    (make-need :dom-id dom-id :act-id act-id :kind kind :reason reason :target target :extra-info extra-info)
)

;;; Return a string representing a need.
(defun need-str (needx)
    (assert (need-p needx))

    (let ((str "#S[NEED"))
        (when (not (regionscorr-p (need-target needx)))
          (setf str (concatenate 'string str (format nil " :dom ~D" (need-dom-id needx))))
          (setf str (concatenate 'string str (format nil " :act ~D" (need-act-id needx))))
        )

        (cond ((= (need-kind needx) *first-sample-of-state*)
                (setf str (concatenate 'string str " :kind Get first sample of state")))
              ((= (need-kind needx) *resample-state*)
                (setf str (concatenate 'string str " :kind Resample state")))
              ((= (need-kind needx) *sample-in-region*)
                (setf str (concatenate 'string str " :kind Get sample in region")))
              ((= (need-kind needx) *change-position*)
                (setf str (concatenate 'string str " :kind Change current position")))
        )

        (cond ((= (need-reason needx) *state-not-in-group*)
                (setf str (concatenate 'string str " :reason State not in a group")))
              ((= (need-reason needx) *confirm-group*)
                (setf str (concatenate 'string str (format nil " :reason Confirm Group"))))
              ((= (need-reason needx) *contradictory-intersection*)
                (setf str (concatenate 'string str (format nil " :reason Contradictory intersection"))))
              ((= (need-reason needx) *between-ip*)
                (setf str (concatenate 'string str (format nil " :reason Between Incompatible Pair "))))
              ((= (need-reason needx) *expand-group*)
                (setf str (concatenate 'string str (format nil " :reason To expand group"))))
              ((= (need-reason needx) *avoid-negative-selectregions*)
                (setf str (concatenate 'string str (format nil " :reason Avoid staying in a negative selectregion"))))
              ((= (need-reason needx) *seek-positive-selectregions*)
                (setf str (concatenate 'string str (format nil " :reason Seek a positive selectregion"))))
        )

        (cond ((state-p (need-target needx))
               (setf str (concatenate 'string str (format nil " :target ~A" (state-str (need-target needx))))))
              ((region-p (need-target needx))
               (setf str (concatenate 'string str (format nil " :target ~A" (region-str (need-target needx))))))
              ((regionscorr-p (need-target needx))
               (setf str (concatenate 'string str (format nil " :target ~A" (regionscorr-str (need-target needx))))))
        )

        (if (string/= (need-extra-info needx) "")
            (setf str (concatenate 'string str (format nil " :info ~A" (need-extra-info needx)))))

        (cond ((plan-p (need-plan needx))
               (if (plan-is-empty (need-plan needx))
                 (setf str (concatenate 'string str " At target"))
                 (setf str (concatenate 'string str (format nil " :plan ~A" (plan-str (need-plan needx)))))))
              ((planscorrstore-p (need-plan needx))
                 (setf str (concatenate 'string str (format nil " :plan ~A" (planscorrstore-str (need-plan needx))))))
        )
  
        (setf str (concatenate 'string str "]"))
        str
    )
)

;;; Return true if a list is a list of needs.
;;; An empty list will return true.
(defun need-list-p (nedlst) ; -> bool
  (if (not (listp nedlst))
    (return-from need-list-p false))

  (loop for nedx in nedlst do
    (if (not (need-p nedx))
      (return-from need-list-p false))
  )
  true
)

