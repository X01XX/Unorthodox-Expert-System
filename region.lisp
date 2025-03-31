;;;; Implement the region struct and functions.




;;; The region struct.
;;; It represents a 2^x by 2^y region of squares on a K-Map.
(defstruct region
  states	; A StateStore of one, or more, states, no state between two others.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (region-<field name> <instance>) -> struct field.
;   (region-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> region
;   (typep <instance> 'region) -> bool
;
; Probably shouldn't use:
;   (make-region [:<field-name> <field-region>]*), use region-new instead.
;   (copy-region <instance>) copies a region instance.

;;; Return a new region, made up of one, or more, states.
(defun region-new (states) ; -> region.
  (if (listp states)
      (setf states (statestore-new states)))

  (if (state-p states)
      (setf states (statestore-new (list states))))

  (assert (statestore-p states))
  (assert (> (statestore-length states) 0))
  (assert (statestore-same-num-bits states))
  
  (make-region :states (statestore-remove-unneeded states))
)

;;; Return the list of states defining a region.
(defun region-state-list (regx) ; -> a list of states.
  (assert (region-p regx))

  (statestore-state-list (region-states regx))
)

;;; Return the highest state in a region.
(defun region-high-state (regx) ; -> state
  (assert (region-p regx))

  (if (= (statestore-length (region-states regx)) 1)
    (return-from region-high-state (region-first-state regx)))

  (let ((ret (value-new :num-bits (region-num-bits regx) : bits 0)))
    (loop for stax in (region-state-list regx) do
       (setf ret (value-or ret (state-value stax)))
    )
    (state-new ret)
  )
)

;;; Return the lowest state in a region.
(defun region-low-state (regx) ; -> state
  (assert (region-p regx))

  (if (= (statestore-length (region-states regx)) 1)
    (return-from region-low-state (region-first-state regx)))

  (let ((ret (value-not (value-new :num-bits (region-num-bits regx) : bits 0))))
     (loop for stax in (region-state-list regx) do
       (setf ret (value-and ret (state-value stax)))
     )
     (state-new ret)
  )
)

;;; Return the number of bits used by a region's states.
(defun region-num-bits (regx) ; -> number
  (assert (region-p regx))

  (statestore-num-bits (region-states regx))
)

;;; Return the first state in a region.
(defun region-first-state (regx) ; -> state
  (assert (region-p regx))

  (statestore-first-state (region-states regx))
)

;;; Return the x mask of a region.
(defun region-x-mask (regx) ; -> mask
  ;(format t "~&region-x-mask ~A" (type-of regx))
  (assert (region-p regx))

  (mask-new (state-xor (region-high-state regx) (region-low-state regx)))
)

;;; Return the edge 1s mask of a region.
(defun region-1-mask (regx) ; -> mask
  (assert (region-p regx))

  (mask-new (state-and
	  (region-first-state regx)
	  (region-second-state regx)))
)

;;; Return the edge 0s mask of a region.
(defun region-0-mask (regx) ; -> mask
  (assert (region-p regx))

  (mask-new (value-and (state-not (region-first-state regx)) (state-not (region-second-state regx))))
)

;;; Return the second state in a region, really the far state from the first state.
(defun region-second-state (regx) ; -> state
  (assert (region-p regx))

  (let ((len (statestore-length (region-states regx))))
    (cond ((= len 1) (region-first-state regx))
          ((= len 2) (statestore-last-state (region-states regx)))
          (t (state-new (state-xor (region-first-state regx) (region-x-mask regx)))))
  )
)

;;; Return the number of states that define a region.
(defun region-number-states (regx) ; -> integer, gt zero.
  (assert (region-p regx))

  (statestore-length (region-states regx))
)

;;; Return a string for a region.
;;; The state making up a region with one state, is obvious.
;;; The states making up a region with two states, can be read from the string representation.
;;; X01x is made up of (1010, 0011).
;;; A region can have more than two states, typically three, to define a region using available
;;; samples, and will be indicated by a trailing + sign. In that case, the only first state can be read
;;; from the string representation.
;;; X01x+ is made up of (1010, ...).
(defun region-str (regx)  ; -> string.
  (assert (region-p regx))

    (let ((strs "r"))
      (setf strs (concatenate 'string strs (region-str-bits regx)))

      (if (> (region-number-states regx) 2)
          (setf strs (concatenate 'string strs "+")))

      strs
    )
)

;;; Return a string representing just region bit positions.
(defun region-str-bits (regx) ; -> string, like 010X.
  (assert (region-p regx))

    (let (
          (strs "")
	  (xmask (region-x-mask regx))
          (bit-pos (mask-msb (mask-new (state-value (statestore-first-state (region-states regx))))))
          (not-start nil)
	  (first-state (statestore-first-state (region-states regx)))
	  (cnt (region-num-bits regx))
	  xval
	  fval
         )

         (loop while (not (mask-zerop bit-pos)) do

	     (setf xval 1)
             (if (value-zerop (mask-and bit-pos xmask))
                 (setf xval 0))

	     (setf fval 1)
             (if (value-zerop (mask-and bit-pos first-state))
                 (setf fval 0))

             (if (and not-start (zerop (mod cnt 4)))
	       (setf strs (concatenate 'string strs "_"))
	       (setf not-start t))

	     (decf cnt)

             (cond ((and (= xval 1) (= fval 1))
		    (setf strs (concatenate 'string strs "X")))
                   ((and (= xval 1) (= fval 0))
		    (setf strs (concatenate 'string strs "x")))
                   ((zerop fval) (setf strs (concatenate 'string strs "0")))
                   (t (setf strs (concatenate 'string strs "1")))
             )
             (setf bit-pos (mask-shift bit-pos -1))
         ) ; end-while

    strs
    )
)

;;; Return a region instance from a symbol.
;;; Like r1010, r1X10x.
;;; A region can be made of a single state.
;;; A token with an X, or x, will be defined with two states.
;;; An X will cause a 1 in the first state, a zero in the second.
;;; An x will cause a 0 in the first state, a one in the second state.
;;; So the states making up a region can be specified by the symbol representation.
(defun region-from (symx) ; -> region.
  ;(format t "~&region-from ~A" (type-of symx))
  (assert (symbolp symx))

  (let ((strx (symbol-name symx)))
    ;; Check for r prefix.
    (if (not (string-equal (subseq strx 0 1) "r"))
	  (return-from region-from (err-new (format nil "region-from: Region ~A Should begin with a r character" strx))))

    (let ((ret (region-from-str strx)))
      (cond ((err-p ret) (error (err-str ret)))
            ((region-p ret) ret)
             (t (error "Region is not valid"))))
  )
)
;;; Return a region instance from a string.
(defun region-from-str (strx) ; -> region instance.
  ;(format t "~&region-from-str ~A" (type-of strx))
  (assert (stringp strx))

  ;; Check for r prefix.
  (if (not (string-equal (subseq strx 0 1) "r"))
	 (return-from region-from-str (err-new (format nil "region-from-str: Region ~A Should begin with an r character" strx))))

  (setf strx (subseq strx 1))

  (let ((state-first "v") (state-second "v"))
    (loop for chr across strx do
      (cond
	    ((char= chr #\_) nil)
	    ((char= chr #\0) (setf state-first (concatenate 'string state-first "0"))
	                     (setf state-second  (concatenate 'string state-second  "0")))
	    ((char= chr #\1) (setf state-first (concatenate 'string state-first "1"))
	                     (setf state-second  (concatenate 'string state-second  "1")))
	    ((char= chr #\X) (setf state-first (concatenate 'string state-first "1"))
	                     (setf state-second  (concatenate 'string state-second  "0")))
	    ((char= chr #\x) (setf state-first (concatenate 'string state-first "0"))
	                     (setf state-second  (concatenate 'string state-second  "1")))
	    (t (return-from region-from-str (err-new (format nil "region-from-str: Invalid character ~A" chr)))))
    )
    (if (= (length state-first) 1)
      (return-from region-from-str (err-new "region-from-str: No valid character found")))

    (region-new (list (state-new (value-from-str state-first))
                      (state-new (value-from-str state-second))))
  )
)

;;; Return true if two regions are equal.
(defun region-eq (reg1 reg2) ; -> bool
  (assert (region-p reg1))
  (assert (region-p reg2))
  (assert (= (region-num-bits reg1) (region-num-bits reg2)))

  (if (not (state-eq (region-high-state reg1) (region-high-state reg2)))
    (return-from region-eq false))

  (if (not (state-eq (region-low-state reg1) (region-low-state reg2)))
    (return-from region-eq false))

  true
)

;;; Return true if two regions are not equal.
(defun region-ne (reg1 reg2) ; -> bool
  (not (region-eq reg1 reg2))
)

;;; Return true if a list is a list of regions.
;;; An empty list will return true.
(defun region-list-p (reglst) ; -> bool
  ;(format t "~&region-list-p: ~A" reglst)
  (if (not (listp reglst))
    (return-from region-list-p false))

  (loop for regx in reglst do
    (if (not (region-p regx))
      (return-from region-list-p false))
  )
  true
)

;;; Return the intersection of two regions.
(defun region-intersection (reg1 reg2) ; -> region, or nil.
  (assert (region-p reg1))
  (assert (region-p reg2))
  (assert (= (region-num-bits reg1) (region-num-bits reg2)))

  (if (not (region-intersects reg1 reg2))
    (return-from region-intersection nil))

  (region-new (list (state-new (state-and (region-high-state reg1) (region-high-state reg2)))
                    (state-new (state-or  (region-low-state reg1) (region-low-state reg2)))))
)

;;; Return the union of two regions.
(defun region-union (reg1 reg2) ; -> region, or nil?
  (assert (region-p reg1))
  (assert (region-p reg2))
  (assert (= (region-num-bits reg1) (region-num-bits reg2)))

  (region-new (list (state-new (state-or  (region-high-state reg1) (region-high-state reg2)))
                    (state-new (state-and (region-low-state reg1) (region-low-state reg2)))))
)

;;; Return a mask of edge bit positions.
(defun region-edge-mask (regx) ; -> mask
  (assert (region-p regx))

  (mask-new (value-eqv (state-value (region-first-state regx)) (state-value (region-second-state regx))))
)

;;; Return a edge-difference mask of two regions.
(defun region-edge-dif-mask (reg1 reg2) ; -> mask
    (mask-new (value-and (mask-and (region-edge-mask reg1) (region-edge-mask reg2))
                         (state-xor (region-first-state reg1) (region-first-state reg2))))
)

;;; Return the distance between two regions.
(defun region-distance (reg1 reg2) ; -> integer.
  (assert (region-p reg1))
  (assert (region-p reg2))
  (assert (= (region-num-bits reg1) (region-num-bits reg2)))

  (mask-num-ones (region-edge-dif-mask reg1 reg2))
)

;;; Return true if a region intersects another.
(defun region-intersects (reg1 reg2) ; -> bool.
  (assert (region-p reg1))
  (assert (region-p reg2))
  (assert (= (region-num-bits reg1) (region-num-bits reg2)))

  ; (format t "~&distance = ~D" (region-distance reg1 reg2))
  (= (region-distance reg1 reg2) 0)
)

;;; Return true if the first region is a superset of the second.
(defun region-superset-of (&key sub sup) ; -> bool.
  (assert (region-p sub))
  (assert (region-p sup))
  (assert (= (region-num-bits sub) (region-num-bits sup)))

  (if (not (region-intersects sub sup))
    (return-from region-superset-of false))

  (let ((subx (region-x-mask sub))
        (supx (region-x-mask sup)))
    (mask-superset-of :sup-mask supx :sub-mask subx)
  )
)

;;; Return a region with edges of a mask set to ones.
(defun region-set-to-ones (regx mskx) ; -> region.
  (assert (region-p regx))
  (assert (mask-p mskx))
  (assert (= (region-num-bits regx) (mask-num-bits mskx)))

  (region-new (list (state-new (value-or (mask-value mskx) (state-value (region-high-state regx))))
                    (state-new (value-or (mask-value mskx) (state-value (region-low-state regx))))))
)

;;; Return a region with edges of a mask set to zeros.
(defun region-set-to-zeros (regx mskx) ; -> region.
  (assert (region-p regx))
  (assert (mask-p mskx))
  (assert (= (region-num-bits regx) (mask-num-bits mskx)))

  (let ((mskn (mask-new (mask-not mskx))))
    (region-new (list (state-new (value-and (mask-value mskn) (state-value (region-high-state regx))))
                      (state-new (value-and (mask-value mskn) (state-value (region-low-state regx))))))
  )
)

;;; Return the minuend region minus the subtrahend region.
(defun region-subtract (&key min-reg sub-reg) ; -> regionstore.
  (assert (region-p min-reg))
  (assert (region-p sub-reg))
  (assert (= (region-num-bits min-reg) (region-num-bits sub-reg)))

  (if (not (region-intersects min-reg sub-reg))
    (return-from region-subtract (regionstore-new (list min-reg))))

  (if (region-superset-of :sup sub-reg :sub min-reg)
    (return-from region-subtract (regionstore-new nil)))

  (let ((ret (regionstore-new nil))
        (sub-bits (mask-split (mask-new-and (region-x-mask min-reg) (region-edge-mask sub-reg))))
       )
    (loop for bitx in sub-bits do
      (if (mask-is-low (mask-new-and bitx (mask-new (state-value (region-first-state sub-reg)))))
	    (regionstore-push-nosubs ret (region-set-to-ones min-reg bitx))
	    (regionstore-push-nosubs ret (region-set-to-zeros min-reg bitx))
      )
    )
    ;(format t "~&region-subtract minuend ~A subtrahend ~A returns ~A" (region-str min-reg) (region-str sub-reg) (regionstore-str ret)) 
    ret
  )
)

;;; Return a region minus a state.
(defun region-subtract-state (regx stax) ; -> regionstore.
  (assert (region-p regx))
  (assert (state-p  stax))
  (assert (= (region-num-bits regx) (state-num-bits stax)))

  (if (not (region-superset-of-state regx stax))
    (return-from region-subtract-state (regionstore-new (list regx))))

  (if (and (= 1 (region-number-states regx)) (state-eq (region-first-state regx) stax))
    (return-from region-subtract-state (regionstore-new nil)))

  (let ((ret (regionstore-new nil))
        (sub-bits (mask-split (region-x-mask regx))))

    (loop for bitx in sub-bits do
      (if (mask-is-low (mask-new-and bitx (mask-new (state-value stax))))
	    (regionstore-push-nosubs ret (region-set-to-ones regx bitx))
	    (regionstore-push-nosubs ret (region-set-to-zeros regx bitx))
      )
    )
    ;(format t "~&region-subtract-state ~A minus ~A returns ~A" (region-str regx) (state-str stax) (regionstore-str ret)) 
    ret
  )
)
 
;;; Return true if a list is a list of regions of the same number of bits.
;;; An empty list will return true.
(defun region-list-same-num-bits-p (reglst) ; -> bool
  (if (not (listp reglst))
    (return-from region-list-same-num-bits-p false))

  (if (null reglst)
    (return-from region-list-same-num-bits-p true))

  (let ((num-bits (region-num-bits (car reglst))))
    (loop for regx in (cdr reglst) do
      (if (not (region-p regx))
        (return-from region-list-same-num-bits-p false))

      (if (/= num-bits (region-num-bits regx))
        (return-from region-list-same-num-bits-p false))
    )
    true
  )
)

;;; Return a change containing unwanted changes in acheiving a region as a goal.
(defun region-unwanted-changes (regx) ; -> change
  (assert (region-p regx))

  (change-new :m01 (region-0-mask regx)
              :m10 (region-1-mask regx)
  )

)

;;; Return the number of unwanted changes.
(defun region-num-unwanted-changes (regx) ; -> integer.
  (change-num-changes (region-unwanted-changes regx))
)

;;; Return the distance between a region and a state.
(defun region-distance-state (regx stax) ; -> integer.
  (assert (region-p regx))
  (assert (state-p stax)) 
  (assert (= (region-num-bits regx) (state-num-bits stax)))
  
  (mask-num-ones (mask-new (value-and
                  (state-xor (region-first-state regx) stax)
                  (state-xor (region-second-state regx) stax))))
) 

;;; Return true if a region intersects a state.
(defun region-intersects-state (regx stax) ; -> bool.
  (assert (region-p regx))
  (assert (state-p stax))
  (assert (= (region-num-bits regx) (state-num-bits stax)))

  ; (format t "~&distance = ~D" (region-distance reg1 reg2))
  (= (region-distance-state regx stax) 0)
)


;;; Return true if the first region is a superset of a state.
(defun region-superset-of-state (regx stax) ; -> bool.
  ;(format t "~&region-superset-of-state: ~A ~A" (region-str regx) (state-str stax))
  (assert (region-p regx))
  (assert (state-p stax))
  (assert (= (region-num-bits regx) (state-num-bits stax)))

  (= (region-distance-state regx stax) 0)
)

;;; Return the far state for, opposite a given state, in a region.
(defun region-far-state (regx stax) ; -> state
  (assert (region-p regx))
  (assert (state-p stax))
  (assert (= (region-num-bits regx) (state-num-bits stax)))
  (assert (region-intersects-state regx stax))

  (state-new (state-xor stax (region-x-mask regx)))
)

;;; Return the far region for, opposite a given subregion, in a region.
(defun region-far-region (regx subx) ; -> region
  ;(format t "~&region-far-region: ~A ~A" (region-str regx) (region-str subx))
  (assert (region-p regx))
  (assert (region-p subx))
  (assert (= (region-num-bits regx) (region-num-bits subx)))
  (assert (region-ne regx subx))
  (assert (region-superset-of :sup regx :sub subx))

  (let ((msk (mask-new (mask-and (region-x-mask regx) (region-edge-mask subx)))))
    (region-new (list (state-new (state-xor (region-first-state subx) msk))
                      (state-new (state-xor (region-second-state subx) msk))))
  )
)

