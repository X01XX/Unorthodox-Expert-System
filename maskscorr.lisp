;;;; Implement a series of masks, with bit-number values corresponding to a list of domains.




; Implement a store of corresponding masks.
(defstruct maskscorr
  maskstore  ; A maskstore of zero, or more, masks.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (maskscorr-<field name> <instance>) -> struct field.
;   (maskscorr-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> maskscorr
;   (typep <instance> 'maskscorr) -> bool
;
; Probably shouldn't use:
;   (make-maskscorr [:<field-name> <field-maskscorr>]*), use maskscorr-new instead.
;   (copy-maskscorr <instance>) copies a maskscorr instance.

;;; Return a new maskscorr instance, from a list of masks.
(defun maskscorr-new (mask-list) ; -> maskscorr, or nil.
  ;(format t "~&maskscorr-new: masks ~A" masks)
  (assert (mask-list-p mask-list))

  (make-maskscorr :maskstore (maskstore-new mask-list))
)

;;; Return a list of masks from a maskscorr.
(defun maskscorr-mask-list (mskscx) ; -> list of masks.
  (assert (maskscorr-p mskscx))

  (maskstore-mask-list (maskscorr-maskstore mskscx))
)

;;; Return a string representing a maskscorr.
(defun maskscorr-str (mskscx) ; -> string.
  (assert (maskscorr-p mskscx))

  (format nil "#S(MASKSCORR ~A )" (maskscorr-maskstore mskscx))
)

;;; Return the number of masks in a maskscorr.
(defun maskscorr-length (mskscx) ; -> number.
  ;(format t "~&maskscorr-length: ~A" (type-of mskscx))
  (assert (maskscorr-p mskscx))

  (maskstore-length (maskscorr-maskstore mskscx))
)

;;; Return true is two maskscorr have similar format.
(defun maskscorr-congruent (msksc1 msksc2) ; -> bool
  (assert (maskscorr-p msksc1))
  (assert (maskscorr-p msksc2))

  (loop for msk1 in (maskscorr-mask-list msksc1)
        for msk2 in (maskscorr-mask-list msksc2) do
	  (if (/= (mask-num-bits msk1) (mask-num-bits msk2))
	    (return-from maskscorr-congruent false))
  )
  true
)

;;; Return true if two maskscorr are equal.
(defun maskscorr-eq (msksc1 msksc2) ; -> bool
  ;(format t "~&maskscorr-eq: ~A ~A" msksc1 msksc2)
  (assert (maskscorr-p msksc1))
  (assert (maskscorr-p msksc2))
  (assert (maskscorr-congruent msksc1 msksc2))

  (loop for msk1 in (maskscorr-mask-list msksc1)
        for msk2 in (maskscorr-mask-list msksc2) do
    (if (not (mask-eq msk1 msk2))
      (return-from maskscorr-eq false))
  )
  true
)

;;; Return the number of bits set to one in a maskscorr.
(defun maskscorr-num-ones (msksc) ; -> integer.
  (assert (maskscorr-p msksc))

  (let ((ret 0))
    (loop for mskx in (maskscorr-mask-list msksc) do
      (incf ret (mask-num-ones mskx))
    )
    ret
  )
)

;;; Return the Boolean "OR" of two maskscorrs.
(defun maskscorr-or (msksc1 msksc2) ; -> maskscorr.
  (assert (maskscorr-p msksc1))
  (assert (maskscorr-p msksc2))
  (assert (maskscorr-congruent msksc1 msksc2))

  (let (mask-list)
    (loop for mskx in (maskscorr-mask-list msksc1)
          for msky in (maskscorr-mask-list msksc2) do
      (setf mask-list (append mask-list (list (mask-new (mask-or mskx msky)))))
    )

    (maskscorr-new mask-list)
  )
)

;;; Return the Boolean "AND" of two maskscorrs.
(defun maskscorr-and (msksc1 msksc2) ; -> maskscorr.
  (assert (maskscorr-p msksc1))
  (assert (maskscorr-p msksc2))
  (assert (maskscorr-congruent msksc1 msksc2))

  (let (mask-list)
    (loop for mskx in (maskscorr-mask-list msksc1)
          for msky in (maskscorr-mask-list msksc2) do
      (setf mask-list (append mask-list (list (mask-new (mask-and mskx msky)))))
    )

    (maskscorr-new mask-list)
  )
)

;;; Return the first mask in a maskscorr.
(defun maskscorr-first-mask (msksc1) ; -> mask
  (assert (maskscorr-p msksc1))

  (maskstore-first-mask (maskscorr-maskstore msksc1))
)

;;; Return the last mask in a maskscorr.
(defun maskscorr-last-mask (msksc1) ; -> mask
  (assert (maskscorr-p msksc1))

  (maskstore-last-mask (maskscorr-maskstore msksc1))
)

;;; Add a mask to the end of the mask list.
(defun maskscorr-add-end (maskscorrx mskx) ; -> nothing, side-effect maskscorr changed.
  (assert (maskscorr-p maskscorrx))
  (assert (mask-p mskx))

  (maskstore-add-end (maskscorr-masksstore maskscorrx) mskx)
)

