;;;; Implement the Square type.

(defstruct square
    state       ; State the squares stores samples of.
    (count 1)   ; Number results.  (mod count 4) will be the place for the next result to be added.
                ;                  (mod (1- count) 4) will be the place of the most recent result.
    results     ; An array of up to four states.
    pn          ; A pn struct instance.
    pnc         ; bool.
    rules       ; A RuleStore instance, containing zero, one, or two rules.
                ; Rules, if any, must have an initial region of one state, equal to the square state.
                ; If two rules, the rule result regions must be different.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (square-<field name> <instance>) returns struct field.
;   (square-p <instance>) -> t
;
; Least used:
;   (type-of <instance>) -> square
;   (typep <instance> 'square) -> t
;
; Don't use:
;   (make-square [:<field-name> <field-value>]*), use square-new instead.
;   (copy-square <instance>) copies a square instance.

;;; Return a new Square instance.
(defun square-new (smpl)  ; -> square instance
    (assert (sample-p smpl))

    (let (ary)
        (setf ary (make-array '(4)))	; Make a four element array, filled with nils.
        (setf (aref ary 0) (sample-result smpl))
    
        (make-square
            :state (sample-initial smpl)
            :count 1
            :results ary
            :pn *pn-one*
            :pnc nil
            :rules (rulestore-new (list (rule-new smpl)))
        )
    )
)

;;; Return the length of the square results list.
(defun square-results-length (square)  ; -> integer, 1-4
    (assert (square-p square))
    (min (square-count square) 4)
)

;;; Return the pn for a (probably just updated) square.
(defun square-calc-pn (square)  ; -> pn value

    (assert (square-p square))

    (if (= 1 (square-results-length square))
      (return-from square-calc-pn *pn-one*))

    (let ((result0 (aref (square-results square) 0)) (pn-one t))

        ;; Check for pn 1
        (loop for inx from 1 to (1- (square-results-length square)) do
            (if (state-ne (aref (square-results square) inx) result0) (setf pn-one nil))
        )

        ;; Calc pn, pnc values, rules.
        (when pn-one
            (return-from square-calc-pn *pn-one*)
        )

        ;; Try to disprove pn-two
        (when (> (square-count square) 2)

	      (if (state-ne result0 (aref (square-results square) 2))
            (return-from square-calc-pn *pn-none*))

          (when (> (square-count square) 3)

	        (if (state-ne (aref (square-results square) 1) (aref (square-results square) 3))
                    (return-from square-calc-pn *pn-none*))
	      )
	   )

       *pn-two*
    ) ; end-let
) ; end square-calc-pn

;;; Calculate the pnc of a (probably just updated) square.
(defun square-calc-pnc (square) ; -> bool
    (assert (square-p square))

    (if (eq (square-pn square) *pn-one*)
        (if (> (square-count square) 2)
            (return-from square-calc-pnc t)
            (return-from square-calc-pnc nil)))

    (if (eq (square-pn square) *pn-two*)
        (if (> (square-count square) 3)
            (return-from square-calc-pnc t)
            (return-from square-calc-pnc nil)))

    t
)

;;; Add a result to a square.
;;; An existing square will have at least one result already.
;;; Return true if the square pn value or pnc bool changes.
(defun square-add-sample (square smpl) ; -> bool
    (assert (square-p square))
    (assert (sample-p smpl))
    (assert (state-eq (sample-initial smpl) (square-state square)))

    ; Update results
    ; If the number of results are over four, the oldest result will be overlaid.
    (setf (aref (square-results square) (mod (square-count square) 4)) (sample-result smpl))

    (incf (square-count square))


    (let (pnnew pncnew ret)

        (setf pnnew (square-calc-pn square))

        (when (neq pnnew (square-pn square))
            (format t "~&square ~A pn  changed from ~A to ~A" (state-str (square-state square)) (pn-str (square-pn square)) (pn-str pnnew))
            (setf (square-pn square) pnnew) ; set new pn, so subsequent pnc calc works correctly.

	    (cond ((eq pnnew *pn-one*)
	           (setf (square-rules square) (rulestore-new (list (rule-new smpl)))))

	          ((eq pnnew *pn-two*)
	           (setf (square-rules square)
			 (rulestore-new (list (rule-new (sample-new :initial (square-state square) :result (aref (square-results square) 0)))
	                                      (rule-new (sample-new :initial (square-state square) :result (aref (square-results square) 1)))))))

	          ((eq pnnew *pn-none*)
	           (setf (square-rules square) (rulestore-new nil)))

		  (t (error "unrecognized pn value"))
            )

	    (setf ret t)
        )

        (setf pncnew (square-calc-pnc square))
	;(format t "~& sqr ~A pncnew ~A pnc ~A" (state-str (square-state square)) pncnew (square-pnc square))

        (when  (not (eq pncnew (square-pnc square)))
            (format t "~&square ~A pn ~A pnc changed from ~A to ~A"
		    (state-str (square-state square)) (pn-str (square-pn square)) (square-pnc square) pncnew)
            (setf (square-pnc square) pncnew)
            (return-from square-add-sample t)
        )
        ; (if (null ret)
	;    (format t "~&square ~A nothing changed pn ~A pnc ~A" (state-str(square-state square)) (pn-str (square-pn square)) (square-pnc square)))
        ret
    ) ; end let
) ; end square-add-sample

;;; Return the most recent result of a square.
(defun square-most-recent-result (sqrx) ; -> state.
  (aref (square-results sqrx) (mod (1- (square-count sqrx)) 4))
)

;;; Return a string representing a square.
(defun square-str (asqr)  ; -> string
    (assert (square-p asqr))
 
    (let ((str "["))
        (setf str (concatenate 'string str (state-str (square-state asqr))))
        (setf str (concatenate 'string str (format nil " :pn ~D :pnc ~A" (pn-str (square-pn asqr)) (square-pnc asqr))))
        (setf str (concatenate 'string str (format nil " :rules ~A" (rulestore-str (square-rules asqr)))))
        (setf str (concatenate 'string str "]"))
        str
    )
)

;;; Return the number of samples needed to reach pnc
(defun square-number-samples-needed (sqrx)  ; -> integer, 0 - 2
    (assert (square-p sqrx))

    (cond   ((eq *pn-none* (square-pn sqrx)) 0)
            ((eq *pn-one* (square-pn sqrx))
                (if (eq 1 (square-count sqrx)) 1 0))
            ((eq *pn-two* (square-pn sqrx))
                (if (eq 2 (square-count sqrx)) 2
                    (if (eq 3 (square-count sqrx)) 1 0))))
)

;;; Return true if two squares are equal.
(defun square-eq (sqr1 sqr2)  ; -> bool
    (assert (square-p sqr1))
    (assert (square-p sqr2))
    (state-eq (square-state sqr1) (square-state sqr2))
)

;;; Return can combination now indicator
;;; A possible subset relationship will return false, but
;;; with more samples may return true.
(defun square-can-combine-now (sqrx sqry) ; -> bool
    (assert (square-p sqrx))
    (assert (square-p sqry))
    (assert (state-ne (square-state sqrx) (square-state sqry)))

    (let ((pnx (square-pn sqrx)) (pny (square-pn sqry))
          (rulsx (square-rules sqrx)) (rulsy (square-rules sqry)))

      ;; Three possibilities. 1/1, 2/2, 3/3.
      (if (neq pnx pny)
  	    (return-from square-can-combine-now nil))

      (if (eq *pn-none* pnx) (return-from square-can-combine-now t))
  
      (if (rulestore-union rulsx rulsy) true false)
  ) ; end let
)

;;; Return true if two squares may be compatible as-is.
(defun square-compatible (sqrx sqry) ; -> bool
    (assert (square-p sqrx))
    (assert (square-p sqry))

    ; Trying to combine the same square is probably an error in logic.
    (assert (state-ne (square-state sqrx) (square-state sqry)))
    (if (pn-ne (square-pn sqrx) (square-pn sqry))
        (return-from square-compatible nil))

    (if (pn-gt (square-pn sqry) (square-pn sqrx))
        (return-from square-compatible nil))

    (if (and (pn-lt (square-pn sqry) (square-pn sqrx)) (square-pnc sqry))
        (return-from square-compatible nil))

    (if (pn-eq (square-pn sqrx) *pn-none*)
        (return-from square-compatible t))

    ;(format t "~&about to compare ~A and ~A" (rulestore-str (square-rules sqrx)) (rulestore-str (square-rules sqry)))
    (if (rulestore-union (square-rules sqrx) (square-rules sqry))
        (return-from square-compatible t))

    nil
)

;; Return true if two squares are adjacent.
(defun square-is-adjacent (sqr1 sqr2) ; -> bool
  (states-is-adjacent (square-state sqr1) (square-state sqr2))
)

;;; Return true if the argument is a list of squares.
(defun square-list-p (squares) ; -> bool
  ;(format t "~&square-list-p: ~A ~A" (type-of squares) squares)
  (if (not (listp squares))
    (return-from square-list-p false))

  ;; Check for a non-square.
  (loop for stpx in squares do
    (if (not (square-p stpx))
      (return-from square-list-p false))
  )
  true
)

;;; Return a list of squares with the highest number of results.
(defun square-list-sample-next (sqrs) ; -> list of squares. 
  (assert (square-list-p sqrs))

  (let ((ret nil) (max-results 1))

    ;; Generate list of squares with the highest number of previous samples.
    (loop for sqrx in sqrs do
      (when (> (square-results-length sqrx) max-results)
        (setf ret nil) 
        (setf max-results (square-results-length sqrx))
      )    
      (if (= (square-results-length sqrx) max-results)
        (push sqrx ret))
    ) ; next sqrx
    ret
  )
)

;;; Return the number of bits used by a square.
(defun square-num-bits (sqrx) ; -> number
  (assert (square-p sqrx))

  (state-num-bits (square-state sqrx))
)

