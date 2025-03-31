;;;; Implement the rule struct and functions.
;;;;
;;;; The rule struct is a representation of a change, from region to region, like X01X -> 100X.
;;;;
;;;; The rule struct can be manipulated in a number of ways, like union and intersection.




;;; The rule struct.
;;;
;;; Single before/after samples can be directly set, for 0->0, 0->1, 1->1 and 1->0.
;;;
;;; Combinations of single changes can be combined to make
;;; X->X = (0->0, 1->1)
;;; X->0 = (0->0, 1->0)
;;; X->1 = (1->1, 0->1)
;;; X->x = (1->0, 1->0)
;;;
;;; 0->X, 1->X are disallowed.
;;;   Something has to be disallowed, or one rule, with all 0-X,1->X positions, is the end result.
;;;   These are not predictive in forward-chaining.
;;;
;;;   Backward-chaining would seem to disallow X->0 and X->1, but successful backward-chaining
;;;   is converted to forward-chaining to run a plan.
(defstruct rule
  m00  ; A mask, where each bit set to one represents a 0->0 bit position before/after for a sample.
  m01  ; A mask, where each bit set to one represents a 0->1 bit position before/after for a sample.
  m11  ; A mask, where each bit set to one represents a 1->1 bit position before/after for a sample.
  m10  ; A mask, where each bit set to one represents a 1->0 bit position before/after for a sample.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (rule-<field name> <instance>) -> struct field.
;   (rule-p <instance>) -> bool.
;
; Least used:
;   (type-of <instance>) -> rule symbol.
;   (typep <instance> 'rule) -> bool.
;
; Probably shouldn't use:
;   (make-rule [:<field-name> <field-rule>]*), use rule-new if possible..
;   (copy-rule <instance>) copies a rule instance.

;;; Return a new rule, given a sample.
(defun rule-new (smpl) ; -> rule.
  (assert (sample-p smpl))

  (let ((m00 (mask-new (value-and (value-not (state-value (sample-initial smpl))) (value-not (state-value (sample-result smpl))))))
        (m01 (mask-new (value-and (value-not (state-value (sample-initial smpl))) (state-value (sample-result smpl)))))
        (m11 (mask-new (value-and (state-value (sample-initial smpl)) (state-value (sample-result smpl)))))
        (m10 (mask-new (value-and (state-value (sample-initial smpl)) (value-not (state-value (sample-result smpl))))))
       )
    (make-rule :m00 m00 :m01 m01 :m11 m11 :m10 m10)
  )
)

;;; Return a rule from a token, like "[00/01/11/10/x0/X0/x1/X1/XX/xx/Xx/xX]"
;;; XX == xx, x to x.
;;; Xx == xX, x to x-not.
(defun rule-from (rulx) ; -> rule.
  ;(format t "~&rule-from: ~A" rulx)
  (if (symbolp rulx)
     (setf rulx (symbol-name rulx)))

  (let ((ret (rule-from-str rulx)))
    (cond ((err-p ret) (error (err-str ret)))
          ((rule-p ret) ret)
           (t (error "Result is not a rule")))
  )
)

(defun rule-from-str (strx) ; -> rule or err.
  ;(format t "~&rule-from-str: ~A" strx)
    (if (< (length strx) 3)
        (return-from rule-from-str (err-new "String is too short")))

    (if (not (string-equal (subseq strx 0 1) "["))
        (return-from rule-from-str (err-new "String must begin with a [")))
        
    (if (not (string-equal (subseq strx (1- (length strx))) "]"))
        (return-from rule-from-str (err-new "String must end with a ]")))
        
    (let ((m00 "m") (m01 "m") (m11 "m") (m10 "m") bit-i bit-j m00-i m01-i m11-i m10-i)

        (loop for chr across (subseq strx 1) do
 
            ;; Check for invalid character.
            (if (null (or (char= chr #\]) (char= chr #\_) (char= chr #\/) (char= chr #\0) (char= chr #\1) (char= chr #\X) (char= chr #\x)))
               (return-from rule-from-str (err-new "Invalid character")))

            (if (and (or (char= chr #\0) (char= chr #\1)) bit-i bit-j)
                   (return-from rule-from-str (err-new "Too many characters in a bit position")))

            (if (and (or (char= chr #\]) (char= chr #\_) (char= chr #\/)) (or (null bit-i) (null bit-j)))
                   (return-from rule-from-str (err-new "Too few characters in a bit position")))

		    (when (and bit-i bit-j)
	            
                         ;; Set mask strings.
                         (setf m00-i "0" m01-i "0" m11-i "0" m10-i "0")
                         (cond ((and (char= bit-i #\0) (char= bit-j #\0)) (setf m00-i "1"))
                               ((and (char= bit-i #\0) (char= bit-j #\1)) (setf m01-i "1"))
                               ((and (char= bit-i #\1) (char= bit-j #\1)) (setf m11-i "1"))
                               ((and (char= bit-i #\1) (char= bit-j #\0)) (setf m10-i "1"))
                               ((and (or (char= bit-i #\X) (char= bit-i #\x)) (char= bit-j #\0)) (setf m10-i "1") (setf m00-i "1"))
                               ((and (or (char= bit-i #\X) (char= bit-i #\x)) (char= bit-j #\1)) (setf m01-i "1") (setf m11-i "1"))
                               ((and (char= bit-i #\X) (char= bit-j #\X)) (setf m00-i "1") (setf m11-i "1"))
                               ((and (char= bit-i #\x) (char= bit-j #\x)) (setf m00-i "1") (setf m11-i "1"))
                               ((and (char= bit-i #\X) (char= bit-j #\x)) (setf m01-i "1") (setf m10-i "1"))
                               ((and (char= bit-i #\x) (char= bit-j #\X)) (setf m01-i "1") (setf m10-i "1"))
                               ((and (char= bit-i #\0) (char= bit-j #\X)) (setf m01-i "1") (setf m00-i "1"))
                               ((and (char= bit-i #\0) (char= bit-j #\x)) (setf m01-i "1") (setf m00-i "1"))
                               ((and (char= bit-i #\1) (char= bit-j #\X)) (setf m10-i "1") (setf m11-i "1"))
                               ((and (char= bit-i #\1) (char= bit-j #\x)) (setf m10-i "1") (setf m11-i "1"))
                                (t (return-from rule-from-str (err-new "Invalid character or combination")))
                         ) ; end cond 3
                         ;; Add a bit position to the mask strings.
                         (setf m00 (concatenate 'string m00 m00-i))
                         (setf m01 (concatenate 'string m01 m01-i))
                         (setf m11 (concatenate 'string m11 m11-i))
                         (setf m10 (concatenate 'string m10 m10-i))
      
                         (setf bit-i nil bit-j nil) ; Init for next bit position.
		    ) ; end when

            (cond ((char= chr #\]) ; Check for end-of-rule.
		             ;; Return new rule.
                     (return-from rule-from-str
                         (make-rule :m00 (mask-from-str m00)
                                    :m01 (mask-from-str m01)
                                    :m11 (mask-from-str m11)
                                    :m10 (mask-from-str m10)))
                  )
                ((or (char= chr #\/) (char= chr #\_))) ; Check for separators.
                ((null bit-i) (setf bit-i chr)) ; Set first char of bit position.
                ((null bit-j) (setf bit-j chr)) ; Set second char of bit position.
                (t 
                   (return-from rule-from-str (err-new "Unknown problem")))
            )

        ) ; end loop
        (err-new "Did not usderstand string")
    ) ; end let
)

;;; Return a string representation of a rule, like [01/10/XX].
(defun rule-str (rulx) ; -> string.
  (assert (rule-p rulx))

  (let ((strs "[")
        (m00 (rule-m00 rulx))
        (m01 (rule-m01 rulx))
        (m11 (rule-m11 rulx))
        (m10 (rule-m10 rulx))
        bitval
        (bit-pos (mask-msb (rule-m00 rulx))) 
        (not-start nil)
	  (cnt (mask-num-bits (rule-m00 rulx)))
       )

       (loop while (not (mask-zerop bit-pos)) do
	     (setf bitval 0)
           (if (not (value-zerop (mask-and bit-pos m00)))
               (setf bitval 1))
           (if (not (value-zerop (mask-and bit-pos m01)))
               (incf bitval 2))
           (if (not (value-zerop (mask-and bit-pos m11)))
               (incf bitval 4))
           (if (not (value-zerop (mask-and bit-pos m10)))
               (incf bitval 8))

           (if not-start (if (zerop (mod cnt 4))
			     (setf strs (concatenate 'string strs "_"))
			     (setf strs (concatenate 'string strs "/"))))

	     (setf not-start t)
	     (decf cnt)

           (cond ((= bitval  0) (setf strs (concatenate 'string strs "..")))
                 ((= bitval  1) (setf strs (concatenate 'string strs "00")))
                 ((= bitval  2) (setf strs (concatenate 'string strs "01")))
                 ((= bitval  3) (setf strs (concatenate 'string strs "0X")))
                 ((= bitval  4) (setf strs (concatenate 'string strs "11")))
                 ((= bitval  5) (setf strs (concatenate 'string strs "XX")))
                 ((= bitval  6) (setf strs (concatenate 'string strs "X1")))
                 ((= bitval  7) (setf strs (concatenate 'string strs "0X?11")))
                 ((= bitval  8) (setf strs (concatenate 'string strs "10")))
                 ((= bitval  9) (setf strs (concatenate 'string strs "X0")))
                 ((= bitval 10) (setf strs (concatenate 'string strs "Xx")))
                 ((= bitval 11) (setf strs (concatenate 'string strs "0X?10")))
                 ((= bitval 12) (setf strs (concatenate 'string strs "1X")))
                 ((= bitval 13) (setf strs (concatenate 'string strs "1X?00")))
                 ((= bitval 14) (setf strs (concatenate 'string strs "1X?01")))
                 ((= bitval 15) (setf strs (concatenate 'string strs "1X?0X?")))
                 (t (setf strs (concatenate 'string strs "..")))
           )
           (setf bit-pos (mask-shift bit-pos -1))
       ) ; end-while

    (setf strs (concatenate 'string strs "]"))
    strs
    )
)

;;; Return a string representation of a rule, like 01X->10x.
(defun rule-str2 (rulx) ; -> string.
  (assert (rule-p rulx))

  (let ((initial (rule-initial-region rulx))
	(result  (rule-result-region rulx))
	;(x-not-x-mask (mask-new (mask-and (rule-m01 rulx) (rule-m10 rulx))))
	(ret-str ""))

    (setf ret-str (concatenate 'string ret-str (region-str-bits initial)))
    (setf ret-str (concatenate 'string ret-str "->"))
    (setf ret-str (concatenate 'string ret-str (region-str-bits result)))
    ret-str
  )
)

;;; Return the Boolean "or", or union, of two rules.
(defun rule-union (rul1 rul2) ; -> rule or nil.
  (assert (rule-p rul1))
  (assert (rule-p rul2))
  (assert (= (rule-num-bits rul1) (rule-num-bits rul2)))

  (let (rulx)
    (setf rulx (make-rule :m00 (mask-new-or (rule-m00 rul1) (rule-m00 rul2))
                          :m01 (mask-new-or (rule-m01 rul1) (rule-m01 rul2))
                          :m11 (mask-new-or (rule-m11 rul1) (rule-m11 rul2))
                          :m10 (mask-new-or (rule-m10 rul1) (rule-m10 rul2))))

    (if (rule-is-valid-union rulx) rulx nil)
  )
)

;;; Return true if a rule is a valid union, that is no 1X, or 0X, bit positions.
(defun rule-is-valid-union (rul) ; -> bool
  (assert (rule-p rul))

  (and
    (value-zerop (mask-and (rule-m00 rul) (rule-m01 rul)))
    (value-zerop (mask-and (rule-m11 rul) (rule-m10 rul)))
  )
)

;;; Return the Boolean "and", or intersection, of two rules.
(defun rule-intersection (rul1 rul2) ; -> rule, or nil.
  (assert (rule-p rul1))
  (assert (rule-p rul2))
  (assert (= (rule-num-bits rul1) (rule-num-bits rul2)))

    (let (rulx)                                                                                                                                                       
      (setf rulx (make-rule :m00 (mask-new-and (rule-m00 rul1) (rule-m00 rul2))
                            :m01 (mask-new-and (rule-m01 rul1) (rule-m01 rul2))
                            :m11 (mask-new-and (rule-m11 rul1) (rule-m11 rul2))
                            :m10 (mask-new-and (rule-m10 rul1) (rule-m10 rul2))))

      (if (rule-is-valid-intersection rulx) rulx nil)
    )
)

;;; Return true if a rule is a valid intersection, that is no bit position is zero for all four masks.
(defun rule-is-valid-intersection (rul) ; -> bool.
  (assert (rule-p rul))

    (mask-is-high (mask-new-or
		    (rule-m00 rul)
		      (mask-new-or (rule-m01 rul)
			 (mask-new-or (rule-m11 rul) (rule-m10 rul)))))
)

;;; Return true if two rules are equal.
(defun rule-eq (rul1 rul2) ; -> bool.
  (assert (rule-p rul1))
  (assert (rule-p rul2))
  (assert (= (rule-num-bits rul1) (rule-num-bits rul2)))

  (and (mask-eq (rule-m00 rul1) (rule-m00 rul2))
       (mask-eq (rule-m01 rul1) (rule-m01 rul2))
       (mask-eq (rule-m11 rul1) (rule-m11 rul2))
       (mask-eq (rule-m10 rul1) (rule-m10 rul2)))
)

;;; Ruturn the number of bits used by a rules masks.
(defun rule-num-bits (rulx) ; -> a number.
  (mask-num-bits (rule-m00 rulx))
)

;;; Return the initial region of a rule.
(defun rule-initial-region (rulx) ; -> region.
  (assert (rule-p rulx))

  (let (
    (sta1 (state-new (mask-or (rule-m10 rulx) (rule-m11 rulx))))
    (sta2 (state-new (value-not (mask-or (rule-m01 rulx) (rule-m00 rulx))))))

    (if (state-eq sta1 sta2)
      (region-new sta1)
      (region-new (list sta1 sta2)))
  )
)

;;; Return the result region of a rule.
(defun rule-result-region (rulx) ; -> region.
  (assert (rule-p rulx))

  (let (
    (sta1 (state-new (mask-or (rule-m11 rulx) (rule-m01 rulx))))
    (sta2 (state-new (value-not (mask-or (rule-m00 rulx) (rule-m10 rulx)))))
    (x-not-x (mask-and (rule-m01 rulx) (rule-m10 rulx))))

    ;; The initial region for a rule will have all X positions represented by a capitol X.
    ;; To indicate X->x, the result region position needs to be changed to lower-case.
    (setf sta1 (state-new (value-xor x-not-x (state-value sta1))))
    (setf sta2 (state-new (value-xor x-not-x (state-value sta2))))

    (if (state-eq sta1 sta2)
      (region-new sta1)
      (region-new (list sta1 sta2)))
  )
)

;;; Return true if a rule is a subset of another.
(defun rule-subset-of (&key sub sup) ; -> bool.
  (assert (rule-p sub))
  (assert (rule-p sup))
  (assert (= (rule-num-bits sub) (rule-num-bits sup)))

  (if (not (mask-subset-of :sub-mask (rule-m00 sub) :sup-mask (rule-m00 sup)))
    (return-from rule-subset-of false))

  (if (not (mask-subset-of :sub-mask (rule-m01 sub) :sup-mask (rule-m01 sup)))
    (return-from rule-subset-of false))

  (if (not (mask-subset-of :sub-mask (rule-m11 sub) :sup-mask (rule-m11 sup)))
    (return-from rule-subset-of false))

  (if (not (mask-subset-of :sub-mask (rule-m10 sub) :sup-mask (rule-m10 sup)))
    (return-from rule-subset-of false))

  true
)

;;; Return a rule that has the minimun changes, to translate from one region to intersect another.
;;; A rule made this way will never have a X->x (0->1, 1->0) bit position.
;;; The X->x bit position can result from the union of two rules.
(defun rule-new-region-to-region (reg1 reg2) ; -> rule.
  (assert (region-p reg1))
  (assert (region-p reg2))
  (assert (= (region-num-bits reg1) (region-num-bits reg2)))

  (let (v00 vxx vx0 v01 vx1 v11 v10 v0x v1x)

    ; Make masks for each possible bit position, (0, 1, X) to (0, 1, X), 3 X 3 = 9 possibilities.
    (setf v00 (mask-and (region-0-mask reg1) (region-0-mask reg2)))
    (setf v0x (mask-and (region-0-mask reg1) (region-x-mask reg2)))
    (setf vxx (mask-and (region-x-mask reg1) (region-x-mask reg2)))

    (setf vx0 (mask-and (region-x-mask reg1) (region-0-mask reg2)))
    (setf v01 (mask-and (region-0-mask reg1) (region-1-mask reg2)))
    (setf vx1 (mask-and (region-x-mask reg1) (region-1-mask reg2)))
    (setf v11 (mask-and (region-1-mask reg1) (region-1-mask reg2)))
    (setf v10 (mask-and (region-1-mask reg1) (region-0-mask reg2)))
    (setf v1x (mask-and (region-1-mask reg1) (region-x-mask reg2)))

    (make-rule :m00 (mask-new (value-or v00 vxx vx0 v0x))
               :m01 (mask-new (value-or v01 vx1))
               :m11 (mask-new (value-or v11 vxx vx1 v1x))
               :m10 (mask-new (value-or v10 vx0)))
  )
)

;;; Mask off one positions in a rule, that has 0->0, or 0->1, in the same positions.
(defun rule-mask-off-ones (rulex msk-out) ; -> rule.
  (assert (rule-p rulex))
  (assert (mask-p msk-out))
  (assert (= (rule-num-bits rulex) (mask-num-bits msk-out)))

  (let (msk-in rulz)
    (setf msk-in (mask-new (mask-not msk-out)))
 
    (setf rulz (make-rule :m00 (rule-m00 rulex)
                          :m01 (rule-m01 rulex)
                          :m11 (mask-new-and (rule-m11 rulex) msk-in)
                          :m10 (mask-new-and (rule-m10 rulex) msk-in)))

    (assert (rule-is-valid-intersection rulz))
    rulz
  )
)

;;; Mask off zero positions in a rule, that has 1->1, or 1->0, in the same positions.
(defun rule-mask-off-zeros (rulex msk-out) ; -> rule.
  (assert (rule-p rulex))
  (assert (mask-p msk-out))
  (assert (= (rule-num-bits rulex) (mask-num-bits msk-out)))

  (let (msk-in rulz)
    (setf msk-in (mask-new (mask-not msk-out)))
 
    (setf rulz (make-rule :m00 (mask-new-and (rule-m00 rulex) msk-in)
                          :m01 (mask-new-and (rule-m01 rulex) msk-in)
                          :m11 (rule-m11 rulex)
                          :m10 (rule-m10 rulex)))

    (assert (rule-is-valid-intersection rulz))
    rulz
  )
)

;;; Return the combination of two rules where the result region of the first rule
;;; intersects the initial region of the second rule.
(defun rule-combine-sequence2 (rul1 rul2) ; -> rule.
  (assert (rule-p rul1))
  (assert (rule-p rul2))
  (assert (= (rule-num-bits rul1) (rule-num-bits rul2)))
  (assert (region-intersects (rule-result-region rul1) (rule-initial-region rul2)))

  (make-rule :m00 (mask-new-or (mask-new-and (rule-m00 rul1) (rule-m00 rul2)) (mask-new-and (rule-m01 rul1) (rule-m10 rul2))) 
             :m01 (mask-new-or (mask-new-and (rule-m01 rul1) (rule-m11 rul2)) (mask-new-and (rule-m00 rul1) (rule-m01 rul2))) 
             :m11 (mask-new-or (mask-new-and (rule-m11 rul1) (rule-m11 rul2)) (mask-new-and (rule-m10 rul1) (rule-m01 rul2))) 
             :m10 (mask-new-or (mask-new-and (rule-m10 rul1) (rule-m00 rul2)) (mask-new-and (rule-m11 rul1) (rule-m10 rul2)))) 
)

;;; Return the combination of two rules.
;;; The result region of the first rule may, or may not,  intersect the initial region of the second rule.
(defun rule-combine-sequence (rul1 rul2) ; -> rule.
  (assert (rule-p rul1))
  (assert (rule-p rul2))
  (assert (= (rule-num-bits rul1) (rule-num-bits rul2)))

  (if (region-intersects (rule-result-region rul1) (rule-initial-region rul2))
    (return-from rule-combine-sequence (rule-combine-sequence2 rul1 rul2)))

  (let ((rule-between (rule-new-region-to-region (rule-result-region rul1) (rule-initial-region rul2))))
    (rule-combine-sequence2 (rule-combine-sequence2 rul1 rule-between) rul2)
  )
)

;;; Return a rule that has an initial region restricted by a given region.
(defun rule-restrict-initial-region (rulx regx) ; -> rule.
  (assert (rule-p rulx))
  (assert (region-p regx))
  (assert (= (rule-num-bits rulx) (region-num-bits regx)))
  (assert (region-intersects (rule-initial-region rulx) regx))

  (let* ((regint (region-intersection (rule-initial-region rulx) regx))
	 (zeros (mask-new (state-not (region-low-state regint))))
	 (ones  (mask-new (state-value (region-high-state regint)))))

    (make-rule :m00 (mask-new-and (rule-m00 rulx) zeros)
	       :m01 (mask-new-and (rule-m01 rulx) zeros)
	       :m11 (mask-new-and (rule-m11 rulx) ones)
	       :m10 (mask-new-and (rule-m10 rulx) ones))
  )
)

;;; Return a rule that has an result region restricted by a given region.
(defun rule-restrict-result-region (rulx regx) ; -> rule.
  ;(format t "~&rule-restrict-result-region: rule ~A region ~A" rulx regx)
  (assert (rule-p rulx))
  (assert (region-p regx))
  (assert (= (rule-num-bits rulx) (region-num-bits regx)))
  (assert (region-intersects (rule-result-region rulx) regx))

  (let* ((regint (region-intersection (rule-result-region rulx) regx))
	 (zeros (mask-new (state-not (region-low-state regint))))
	 (ones  (mask-new (state-value (region-high-state regint)))))

    (make-rule :m00 (mask-new-and (rule-m00 rulx) zeros)
	       :m01 (mask-new-and (rule-m01 rulx) ones)
	       :m11 (mask-new-and (rule-m11 rulx) ones)
	       :m10 (mask-new-and (rule-m10 rulx) zeros))
  )
)

;;; Return a change form a rule..
(defun rule-changes (rulx) ; -> change
  (assert (rule-p rulx))

  (change-new :m01 (rule-m01 rulx)
              :m10 (rule-m10 rulx))
)

;;; Return the number of changes.
(defun rule-num-changes (rulx) ; -> integer.
  (change-num-changes (rule-changes rulx))
)

;;; Return the intersection of a rule and a change, as a change.
(defun rule-intersection-change (rulx cngx) ; -> change
    (change-new :m01 (mask-new-and (rule-m01 rulx) (change-m01 cngx))
                :m10 (mask-new-and (rule-m10 rulx) (change-m10 cngx)))
)

;;; Return true if two rules run in a given order results in
;;; all needed changes in the first rule being reversed.
(defun rule-sequence-blocks-changes (&key first next wanted) ; -> bool
  (assert (rule-p first))
  (assert (rule-p next))
  (assert (change-p wanted))
  (assert (= (rule-num-bits first) (rule-num-bits next)))
  (assert (= (rule-num-bits first) (change-num-bits wanted)))
  (assert (value-is-low (mask-and (change-m01 wanted) (change-m10 wanted)))) ; 0->1 and 1->0 is never needed for the same bit position.
  (assert (value-is-not-low (mask-or (change-m01 wanted) (change-m10 wanted)))) ; At least one change should be needed.
  
  (let ((rule-comb (rule-combine-sequence first next))
	(msk01 (mask-new-and (rule-m01 first) (change-m01 wanted)))
	(msk10 (mask-new-and (rule-m10 first) (change-m10 wanted)))
       )

    (if (mask-is-not-low (mask-new-and (rule-m01 rule-comb) msk01))
      (return-from rule-sequence-blocks-changes false))

    (if (mask-is-not-low (mask-new-and (rule-m10 rule-comb) msk10))
      (return-from rule-sequence-blocks-changes false))

    true
  )
)

;;; Return true if two rules are mutually exclusive, for a given wanted change.
(defun rule-mutually-exclusive (rul1 rul2 wanted) ; -> bool
  (assert (rule-p rul1))
  (assert (rule-p rul2))
  (assert (change-p wanted))
  (assert (= (rule-num-bits rul1) (rule-num-bits rul2)))
  (assert (= (rule-num-bits rul1) (change-num-bits wanted)))
  (assert (value-is-low (mask-and (change-m01 wanted) (change-m10 wanted)))) ; 0->1 and 1->0 is never needed for the same bit position.
  (assert (value-is-not-low (mask-or (change-m01 wanted) (change-m10 wanted)))) ; At least one change should be needed.
  (assert (change-is-not-low (rule-intersection-change rul1 wanted)))
  (assert (change-is-not-low (rule-intersection-change rul2 wanted)))

  (and (rule-sequence-blocks-changes :first rul1 :next rul2 :wanted wanted)
       (rule-sequence-blocks-changes :first rul2 :next rul1 :wanted wanted))
)

;;; Return true if a list is a list of rules.
;;; An empty list will return true.
(defun rules-list-p (rullst) ; -> bool
  (if (not (listp rullst))
    (return-from rules-list-p false))

  (loop for rulx in rullst do
    (if (not (rule-p rulx))
      (return-from rules-list-p false))
  )
  true
)

;;; Return the result of applying a rule to a state.
(defun rule-result-from-state (rulx stax) ; -> state instance.\
  (assert (rule-p rulx))
  (assert (state-p stax))

  (let (cng1s cng0s)
    (setf cng1s (mask-new (state-and stax (rule-m10 rulx))))
    (setf cng0s (mask-new (mask-and (mask-new (state-not stax)) (rule-m01 rulx))))
    (state-new (state-xor stax (mask-new (mask-or cng1s cng0s))))
  )
)

;;; Return true if a rule makes a change.
(defun rule-makes-change (rulx) ; -> bool
  (assert (rule-p rulx))

  (or (mask-is-not-low (rule-m01 rulx)) (mask-is-not-low (rule-m10 rulx)))
)

;;; Return the initial region, restricted by changes.
;;; That is, X->1 becomes 0, X->0 becomes 1.
(defun rule-change-surface (rulx) ; -> region
  (assert (rule-p rulx))
  (assert (rule-makes-change rulx))

  (rule-initial-region (make-rule :m00 (mask-and (rule-m00 rulx) (mask-not (rule-m10 rulx)))
                                  :m01 (rule-m01 rulx)
                                  :m11 (mask-and (rule-m11 rulx) (mask-not (rule-m01 rulx)))
                                  :m10 (rule-m10 rulx)))
)
