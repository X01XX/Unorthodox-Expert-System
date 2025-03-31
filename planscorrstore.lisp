
;;;; Implement a store of planscorr.
;;;;
;;;; From first to last, each planscorr result regions should equal the next
;;;; planscorr initial regions.




; Implement a store of plans.
(defstruct planscorrstore
  planscorrs 	; A list of zero, or more, planscorr.
  value	     	; A LE 0 value representing the negative value of select regions' rate the plans pass through.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (planscorrstore-<field name> <instance>) -> struct field.
;   (planscorrstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> planscorrstore
;   (typep <instance> 'planscorrstore) -> bool
;
; Probably shouldn't use:
;   (make-planscorrstore [:<field-name> <field-planscorrstore>]*), use planscorrstore-new instead.
;   (copy-planscorrstore <instance>) copies a planscorrstore instance.

;;; Return a new planscorrstore instance, from a list of planscorr.
;;; Default value to 0.
(defun planscorrstore-new (planscorr-list) ; -> planscorrstore.
  ;(format t "~&planscorrstore ~A" planscorr-list)
  (assert (planscorr-list-p planscorr-list))

  (let (ret)
    (setf ret (make-planscorrstore :planscorrs planscorr-list :value 0))
    (assert (planscorrstore-is-valid ret))
    ret
  )	
)

;;; Seh the value of a planscorrstore.
(defun planscorrstore-set-value (storex val) ; -> nothing, side-effect planscorrstore-value changed.
  (assert (planscorrstore-p storex))

  (setf (planscorrstore-value storex) val)
)

;;; Add planscorr to the end of a planscorrstore.
(defun planscorrstore-add-end (storex plnscx) ; -> nothing, side-effect planscorrstore changed.
  (assert (planscorrstore-p storex))
  (assert (planscorr-p plnscx))

  (setf (planscorrstore-planscorrs storex) (append (planscorrstore-planscorrs storex) (list plnscx)))
  (assert (planscorrstore-is-valid storex))
)

;;; Return the number of plans in a planscorrstore.
(defun planscorrstore-length (storex) ; -> number.
  (assert (planscorrstore-p storex))

  (length (planscorrstore-planscorrs storex))
)

;;; Return true if a planscorrstore is empty.
(defun planscorrstore-is-empty (storex) ; -> bool
  (assert (planscorrstore-p storex))

  (zerop (planscorrstore-length storex))
)

;;; Return true if a planscorrstore is not empty.
(defun planscorrstore-is-not-empty (storex) ; -> bool
  (assert (planscorrstore-p storex))

  (plusp (planscorrstore-length storex))
)

;;; Return a string representing a planscorrstore.
(defun planscorrstore-str (storex) ; -> string.
  (assert (planscorrstore-p storex))

  (let ((ret "#S(PCST ") (start t))

    (if (not (zerop (planscorrstore-value storex)))
      (setf ret (concatenate 'string ret (format nil " ~D " (planscorrstore-value storex)))))

    (loop for plnx in (planscorrstore-planscorrs storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))

      (setf ret (concatenate 'string ret (planscorr-str plnx)))
    )
    (if (zerop (planscorrstore-length storex))
      (setf ret (concatenate 'string ret "NIL)"))
      (setf ret (concatenate 'string ret ")"))
    )
    ret
  )
)

;;; Check that planscorr items are linked.
(defun planscorrstore-is-valid (storex) ; -> bool
  (assert (planscorrstore-p storex))

  (loop for plnx in (planscorrstore-planscorrs storex)
        for plny in (cdr (planscorrstore-planscorrs storex)) do

    (if (not (planscorr-is-linked-to plnx plny))
      (return-from planscorrstore-is-valid false))

  )
  (loop for plnx in (planscorrstore-planscorrs storex) do
    (if (not (planscorr-act0-steps-valid plnx))
      (return-from planscorrstore-is-valid false))
  )
  true
)

