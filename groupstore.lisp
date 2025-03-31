; Implement a store of groups.




; Implement a store of groups.
(defstruct groupstore
  groups  ; A list of zero, or more, non-duplicate, same number bits, groups.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (groupstore-<field name> <instance>) -> struct field.
;   (groupstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> groupstore
;   (typep <instance> 'groupstore) -> bool
;
; Probably shouldn't use:
;   (make-groupstore [:<field-name> <field-groupstore>]*), use groupstore-new instead.
;   (copy-groupstore <instance>) copies a groupstore instance.
(defun groupstore-new (groups) ; -> groupstore.
  ;(format t "~&groups ~A" groups)
  (let ((ret (make-groupstore :groups nil)))
    (loop for grpx in groups do 
      (groupstore-push ret grpx)
    )
    ret
  )
)

; Push a new group into a groupstore, suppress dups, subsets.
; Return true if the group has been added.
(defun groupstore-push (storex groupx) ; -> bool, true if added.
  (assert (groupstore-p storex))
  (assert (group-p groupx))

  (if (groupstore-is-not-empty storex)
    (assert (= (group-num-bits groupx) (group-num-bits (groupstore-first storex)))))

  (if (groupstore-member storex groupx)
    (return-from groupstore-push false))

  (let (del-grps)

    ; Check for equal, superset and subset groups.
    (loop for grpx in (groupstore-groups storex) do
      (if (region-superset-of :sub (group-region groupx) :sup (group-region grpx))
        (return-from groupstore-push false))

      (if (region-superset-of :sup (group-region groupx) :sub (group-region grpx))
        (push grpx del-grps))
    )

    ; Delete subset groups, if any.
    (loop for grpx in del-grps do
        (remove grpx (groupstore-groups storex) :test #'group-eq)
    )

    ; Add the new group to the end of the groups list, old survivors migrate to the beginning of the list.
    (if (null (groupstore-groups storex))
      (push groupx (groupstore-groups storex))
      (push groupx (cdr (last (groupstore-groups storex))))) 
  )
  true
)

; Return the number of groups in a groupstore.
(defun groupstore-length (storex) ; -> number.
  (assert (groupstore-p storex))

  (length (groupstore-groups storex))
)

; Return true if a groupstore is empty.
(defun groupstore-is-empty (storex) ; -> bool
  (zerop (groupstore-length storex))
)

; Return true if a groupstore is not empty.
(defun groupstore-is-not-empty (storex) ; -> bool
  (plusp (groupstore-length storex))
)

; Return a string representing a groupstore.
(defun groupstore-str (storex) ; -> string.
  (assert (groupstore-p storex))

  (let ((ret "#S(GROUPSTORE ") (start t))

    (loop for grpx in (groupstore-groups storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))    

      (setf ret (concatenate 'string ret (format nil " ~&    ~A" (group-str grpx))))
    )
    (setf ret (concatenate 'string ret ")"))

    ret
  )
)

; Return true if a groupstore contains a given group.
(defun groupstore-member (storex grpx) ; -> bool
  (assert (groupstore-p storex))
  (assert (group-p grpx))

  (if (member grpx (groupstore-groups storex) :test #'group-eq) true false)
)

(defun groupstore-first (storex) ; -> group
  (assert (groupstore-p storex))
  (assert (groupstore-is-not-empty storex))

  (car (groupstore-groups storex))
)

; Return possible steps to satisfy a rule.
(defun groupstore-get-steps (storex rule-to-goal within) ; -> stepstore.
  ;(format t "~&groupstore-get-steps")
  (assert (groupstore-p storex))
  (assert (rule-p rule-to-goal))
  (assert (region-p within))

  (let ((ret-steps (stepstore-new nil)) steps)
    (loop for grpx in (groupstore-groups storex) do
        (setf steps (group-get-steps grpx rule-to-goal within))
	(loop for stpx in (stepstore-steps steps) do
	  (if (not (stepstore-member ret-steps stpx))
	    (stepstore-push ret-steps stpx)
	  )	
	)
    )
    ret-steps
  )
)

;;; Return true if a state is in a group.
(defun groupstore-state-in-group (groups stax) ; -> bool.
  ;(format t "~&groupstore-state-in-group: ~A ~A" (type-of groups) (type-of stax))
  (assert (groupstore-p groups))
  (assert (state-p stax))

  (loop for grpx in (groupstore-groups groups) do 
    (if (region-superset-of-state (group-region grpx) stax)
        (return-from groupstore-state-in-group true))
  )
  false
)

;;; Return true if a state is in a group made with more than one state.
(defun groupstore-multistate-groups-state-in (groups stax) ; -> bool.
  ;(format t "~&groupstore-state-in-group: ~A ~A" (type-of groups) (type-of stax))
  (assert (groupstore-p groups))
  (assert (state-p stax))

  (loop for grpx in (groupstore-groups groups) do 
    (if (and (> (region-number-states (group-region grpx)) 1) (region-superset-of-state (group-region grpx) stax))
        (return-from groupstore-multistate-groups-state-in true))
  )
  false
)

;;; Return a list of groups a state is in.
(defun groupstore-groups-state-in (groups stax) ; -> GroupStore instance.
  ;(format t "~&groupstore-groups-state-in: ~A ~A" (type-of groups) (type-of stax))
  (assert (groupstore-p groups))
  (assert (state-p stax))

  (let ((ret (groupstore-new nil)))
    (loop for grpx in (groupstore-groups groups) do 
      (if (region-superset-of-state (group-region grpx) stax)
         (groupstore-push ret grpx))
    )
    ret
  )
)

;;; Return true if a state is in at least one group.
(defun groupstore-state-in (groups stax) ; -> bool.
  ;(format t "~&groupstore-state-in: ~A ~A" (type-of groups) (type-of stax))
  (assert (groupstore-p groups))
  (assert (state-p stax))

  (loop for grpx in (groupstore-groups groups) do 
    (if (region-superset-of-state (group-region grpx) stax)
       (return-from groupstore-state-in true))
  )
  false
)

;;; Return a list of groups invalidate by a sample.
(defun groupstore-groups-invalidated-by-sample (groups smpl) ; -> groupstore instance.
  (assert (groupstore-p groups))
  (assert (sample-p smpl))

  (let ((ret (groupstore-new nil)) (rules (rulestore-new (list (rule-new smpl)))) (stax (sample-initial smpl)))

    (loop for grpx in (groupstore-groups groups) do 

      (when (region-superset-of-state (group-region grpx) stax)

         (if (pn-ne (group-pn grpx) *pn-none*) ; else need pnc square, to invalidate.
            (if (not (rulestore-subset-of :sup (group-rules grpx) :sub rules))
               (groupstore-push ret grpx)))
      )
    ) ; next grpx
    ret
  )
)

;;; Return a list of groups invalidate by a square.
(defun groupstore-groups-invalidated-by-square (groups sqrx) ; -> groupstore instance.
  ;(format t "~&groupstore-groups-invalidated-by-square: ~A" (square-str sqrx))
  (assert (groupstore-p groups))
  (assert (square-p sqrx))

  (let ((ret (groupstore-new nil)) (stax (square-state sqrx)))
    (loop for grpx in (groupstore-groups groups) do 

      (if (region-superset-of-state (group-region grpx) stax)

         (if (rulestore-invalidated-by-square (group-rules grpx) sqrx)
             (groupstore-push ret grpx))
   
      )
    ) ; next grpx
    ;(if (groupstore-is-not-empty ret)
    ;  (format t "~&groupstore-groups-invalidated-by-square: ~A returning ~A" (state-str (square-state sqrx)) (groupstore-str ret))
    ;)
    ret
  )
)

;;; Add group to the end of a groupstore.
(defun groupstore-add-end (storex grpx) ; -> nothing, side-effect groupstore changed.
  (assert (groupstore-p storex))
  (assert (group-p grpx))
  
  (setf (groupstore-groups storex) (append (groupstore-groups storex) (list grpx)))
)

;;; Return true if a group was pushed into a groupstore.
;;; Preserve group order.
(defun groupstore-push-nosubs (storex grpx) ; -> bool, side-effect groupstore is changed.
  ;(format t "~&groupstore-push-nosubs ~A ~A" storex grpx)
  (assert (groupstore-p storex))
  (assert (group-p grpx))

  ;; Check for group in store that is a superset (or dup) of the new group.
  (loop for grpy in (groupstore-groups storex) do
    (if (region-superset-of :sup (group-region grpy) :sub (group-region grpx))
      (return-from groupstore-push-nosubs false))
  )

  ;; Check for groups that are a subset of the new group.
  (let (del-grps)
    ;; Find groups that are a subset of the new group.
    (loop for grpy in (groupstore-groups storex) do
      (if (region-superset-of :sup (group-region grpx) :sub (group-region grpy))
        (push grpy del-grps)
      )
    )
    ;; Remove the subset groups.
    (loop for grpy in del-grps do
      (setf (groupstore-groups storex) (remove grpy (groupstore-groups storex) :test #'group-eq))
    )
  )

  ; Add the group.
  (groupstore-add-end storex grpx)
  true
)

;;; Print a groupstore.
(defun groupstore-print (storex)
  (assert (groupstore-p storex))

  (let ((first true))
    (loop for grpx in (groupstore-groups storex) do
      (if first
        (setf first false)
        (format t ", ")
      )
      (group-print grpx)
    )
  )
)

;;; Return a groupstore with a group removed.
(defun groupstore-remove-group (storex grpx) ; -> groupstore.
  (make-groupstore :groups (remove grpx (groupstore-groups storex)))
)

;;; Returns a group, given a region.
(defun groupstore-find (storex regx) ; -> group, or nil.
    (assert (groupstore-p storex))
    (assert (region-p regx))

    (loop for grpx in (groupstore-groups storex) do
        (if (region-eq (group-region grpx) regx)
          (return-from groupstore-find grpx)) 
    )
    nil 
)

;;; Return the nth element of a GroupStore.
(defun groupstore-nth (storex inx) ; -> group instance, or nil.
  (assert (groupstore-p storex))
  (assert (integerp inx))

  (if (>= inx (groupstore-length storex))
    (return-from groupstore-nth nil))

  (nth inx (groupstore-groups storex))
)

;;; Return the union of two groupstores.
(defun groupstore-union (storex storey) ; -> groupstore
  (assert (groupstore-p storex))
  (assert (groupstore-p storey))

  (let ((ret (groupstore-new nil)))
    (loop for grpx in (groupstore-groups storex) do
      (groupstore-push-nosubs ret grpx)
    )
    (loop for grpy in (groupstore-groups storey) do
      (groupstore-push-nosubs ret grpy)
    )
    ret
  )
)

;;; Return the number of bits used by groups in a non-empty groupstore.
(defun groupstore-num-bits (storex) ; -> number                                                          
  (assert (groupstore-p storex))
  (assert (groupstore-is-not-empty storex))

  (group-num-bits (groupstore-first-group storex))
)

;;; Return the first group of a non-empty groupstore.
(defun groupstore-first-group (storex) ; -> group
  (assert (groupstore-p storex))
  (assert (groupstore-is-not-empty storex))

  (car (groupstore-groups storex))
)

