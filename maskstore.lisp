;;;; Implement a store of masks.




;;; Implement a store of masks.
(defstruct maskstore
  masks  ; A list of zero, or more, non-duplicate, same number bits, masks.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (maskstore-<field name> <instance>) -> struct field.
;   (maskstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> maskstore
;   (typep <instance> 'maskstore) -> bool
;
; Probably shouldn't use:
;   (make-maskstore [:<field-name> <field-maskstore>]*), use maskstore-new instead.
;   (copy-maskstore <instance>) copies a maskstore instance.

;;; Return a new maskstore instance.
(defun maskstore-new (masks) ; -> maskstore.
  ;(format t "~&maskstore-new ~A" masks)
  (assert (mask-list-p masks))

  (make-maskstore :masks masks)
)

;;; Push a new mask into a maskstore.
(defun maskstore-push (storex maskx) ; -> nothing. Side-effect maskstore is changed.
  (assert (maskstore-p storex))
  (assert (mask-p maskx))

  (push maskx (maskstore-masks storex))
)

;;; Return the number of masks in a maskstore.
(defun maskstore-length (storex) ; -> number.
  (assert (maskstore-p storex))

  (length (maskstore-masks storex))
)

;;; Return true if a maskstore is empty.
(defun maskstore-is-empty (storex) ; -> bool
  (zerop (maskstore-length storex))
)

;;; Return true if a maskstore is not empty.
(defun maskstore-is-not-empty (storex) ; -> bool
  (plusp (maskstore-length storex))
)

;;; Return a string representing a maskstore.
(defun maskstore-str (storex) ; -> string.
  (assert (maskstore-p storex))

  (let ((ret "#S(MASKSTORE ") (start t))

    (loop for mskx in (maskstore-masks storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))    

      (setf ret (concatenate 'string ret (mask-str mskx)))
    )

    ret
  )
)

;;; Return true if a maskstore contains a given mask.
(defun maskstore-member (storex mskx) ; -> bool
  (assert (maskstore-p storex))
  (assert (mask-p mskx))

  (if (member mskx (maskstore-masks storex) :test #'mask-eq) true false)
)

;;; Return the first mask in a store.
(defun maskstore-first-mask (storex) ; -> mask
  ;(format t "~&maskstore-first-mask: ~A" storex)
  (assert (maskstore-p storex))
  (assert (maskstore-is-not-empty storex))

  (car (maskstore-masks storex))
)

;;; Return the last mask in a store.
(defun maskstore-last-mask (storex) ; -> mask
  (assert (maskstore-p storex))
  (assert (maskstore-is-not-empty storex))

  (car (last (maskstore-masks storex)))
)

;;; Add mask to the end of a maskstore.
(defun maskstore-add-end (storex plnx) ; -> nothing, side-effect maskstore changed.
  (assert (maskstore-p storex))
  (assert (mask-p plnx))

  (setf (maskstore-masks storex) (append (maskstore-masks storex) (list plnx)))
)

;;; Return a list of masks.
(defun maskstore-mask-list (storex) ; -> list of masks.
  (assert (maskstore-p storex))

  (maskstore-masks storex)
)
