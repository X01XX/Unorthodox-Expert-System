; Implement a store of selectregionss.




; Implement a store of selectregionss.
(defstruct selectregionsstore
  selectregions  ; A list of zero, or more, non-duplicate, same number bits, selectregionss.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (selectregionsstore-<field name> <instance>) -> struct field.
;   (selectregionsstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> selectregionsstore
;   (typep <instance> 'selectregionsstore) -> bool
;
; Probably shouldn't use:
;   (make-selectregionsstore [:<field-name> <field-selectregionsstore>]*), use selectregionsstore-new instead.
;   (copy-selectregionsstore <instance>) copies a selectregionsstore instance.

;;; Return a new selectregionsstore instance.
(defun selectregionsstore-new (selectregionss) ; -> selectregionsstore.
  ;(format t "~&selectregionsstore-new ~A" selectregionss)
  (assert (selectregions-list-p selectregionss))

  (make-selectregionsstore :selectregions selectregionss)
)

; Push a new selectregions into a selectregionsstore.
(defun selectregionsstore-push (storex selectregionsx) ; -> nothing. Side-effect selectregionsstore is changed.
  (assert (selectregionsstore-p storex))
  (assert (selectregions-p selectregionsx))

  (push selectregionsx (selectregionsstore-selectregions storex))
)

; Return the number of selectregionss in a selectregionsstore.
(defun selectregionsstore-length (storex) ; -> number.
  (assert (selectregionsstore-p storex))

  (length (selectregionsstore-selectregions storex))
)

; Return true if a selectregionsstore is empty.
(defun selectregionsstore-is-empty (storex) ; -> bool
  (zerop (selectregionsstore-length storex))
)

; Return true if a selectregionsstore is not empty.
(defun selectregionsstore-is-not-empty (storex) ; -> bool
  (plusp (selectregionsstore-length storex))
)

; Return a string representing a selectregionsstore.
(defun selectregionsstore-str (storex) ; -> string.
  (assert (selectregionsstore-p storex))

  (let ((ret "#S(SELECTREGIONSSTORE ") (start t))

    (loop for sregsx in (selectregionsstore-selectregions storex) do
      (if start (setf start nil) (setf ret (concatenate 'string ret ", ")))    

      (setf ret (concatenate 'string ret (selectregions-str sregsx)))
    )

    ret
  )
)

; Return true if a selectregionsstore contains a given selectregions.
(defun selectregionsstore-member (storex sregsx) ; -> bool
  (assert (selectregionsstore-p storex))
  (assert (selectregions-p sregsx))

  (if (member sregsx (selectregionsstore-selectregions storex) :test #'selectregions-eq) true false)
)

(defun selectregionsstore-first-selectregions (storex) ; -> selectregions
  (assert (selectregionsstore-p storex))
  (assert (selectregionsstore-is-not-empty storex))

  (car (selectregionsstore-selectregions storex))
)

;;; Return the rate af a given regionscorr, based on superset SelectRegions.
(defun selectregionsstore-rate (storex regscr) ; -> rate
  (assert (selectregionsstore-p storex))
  (assert (regionscorr-p regscr))

  (let ((ret (rate-new :positive 0 :negative 0)))

    (loop for srx in (selectregionsstore-selectregions storex) do
      (when (regionscorr-intersects regscr (selectregions-regionscorr srx))
        (setf ret (rate-union ret (selectregions-rate srx)))
      )
    )

    ret
  )
)

;;; Return a regionscorrstore of all selectregions-regionscorr.
(defun selectregionsstore-regionscorrs (storex) ; -> regionscorrstore
  (assert (selectregionsstore-p storex))

  (let ((ret (regionscorrstore-new nil)))

    (loop for selx in (selectregionsstore-selectregions storex) do
      (regionscorrstore-push ret (selectregions-regionscorr selx))
    )
    ret
  )
)

;;; Return a regionscorrstore of all selectregions-regionscorr,
;;; with rate negative values ge a given number.
(defun selectregionsstore-regionscorr-upto (storex upto) ; -> regionscorrstore
  (assert (selectregionsstore-p storex))
  (assert (integerp upto))

  (let ((ret (regionscorrstore-new nil)))

    (loop for selx in (selectregionsstore-selectregions storex) do
      (if (>= (rate-negative (selectregions-rate selx)))
        (regionscorrstore-push ret (selectregions-regionscorr selx)))
    )
    ret
  )
)

;;; Return selectregions split by intersections.
;;; Return selectregions split by intersections.
;;; Every fragment will be a subset of any original selectregions it intersects.
(defun selectregionsstore-split-by-intersections (storex) ; -> selectregionsstore.
  (let ((ret (selectregionsstore-new nil)) regcorrs)

    (setf regcorrs (regionscorrstore-split-by-intersections (selectregionsstore-regionscorrs storex)))

    (loop for rcx in (regionscorrstore-regionscorrs regcorrs) do
      (selectregionsstore-push ret (selectregionsstore-new rcx (selectregionsstore-rate storex rcx)))
    )
    ret
  )
)

