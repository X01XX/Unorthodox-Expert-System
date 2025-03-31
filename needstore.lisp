;;;; Implement a needstore structxa, a store of need struct instances.




;;; Implement a store of masks.
(defstruct needstore
  needs  ; A list of zero, or more, non-duplicate, same number bits, needs.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (needstore-<field name> <instance>) -> struct field.
;   (needstore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> needstore
;   (typep <instance> 'needstore) -> bool
;
; Probably shouldn't use:
;   (make-needstore [:<field-name> <field-needstore>]*), use needstore-new instead.
;   (copy-needstore <instance>) copies a needstore instance.

;;; Return a new needstore instance.
(defun needstore-new (needs) ; -> needstore.
  ;(format t "~&needstore-new ~A" needs)
  (assert (need-list-p needs))

  (make-needstore :needs needs)
)

;;; Return a string representing a needstore list.
(defun needstore-str (storex)
; (format t "~&needstore-str for ~A" storex)
    (assert (needstore-p storex))

    (let ((str "("))

        (loop for needx in (needstore-needs storex)
              for count from 0 do

                 (when (plusp count)
                     (setf str (concatenate 'string str (format nil " ~& "))))

                 (setf str (concatenate 'string str (need-str needx)))
        )
        (setf str (concatenate 'string str ")"))
        str
    )
)

;;; Returns true if a need of a given kind and target are in a needstore.
(defun needstore-find-kind-target (needs kind target) ; -> bool
    (assert (needstore-p needs))
    (assert (integerp kind))
    (assert (or (zerop kind) (plusp kind)))

    (loop for needx in needs do
        (if (and (= (need-kind needx) kind) (region-eq (need-target needx) target))
            (return-from needstore-find-kind-target t))
    )
    nil
)

;;; Returns the number of a given kind of need in a needstore.
(defun needstore-number-kind (needs kind)
    (assert (needstore-p needs))
    (assert (integerp kind))
    (assert (or (zerop kind) (plusp kind)))

    (let ((cnt 0))
        (loop for needx in needs do
            (if (= (need-kind needx) kind)
                (incf cnt))
        )
        cnt
    )
)

;;; Do various tests
(defun needstore-tests ()

    (format t "~&needstore-tests OK")
    'OK
)

;;; Append two needstores.
;;; Preserve order.
(defun needstore-append (store1 store2) ; -> needstore
  ;(format t "~&needstore-append: ~A ~A" (needstore-str store1) (needstore-str store2))
  (assert (needstore-p store1))
  (assert (needstore-p store2))

  (let ((ret (make-needstore :needs (needstore-needs store1))))

    ;; Add store2 needs.
    (loop for nedx in (needstore-needs store2) do
      (needstore-add-end ret nedx)
    )   
    ;(format t "~&needstore-append: result ~A" (needstore-str ret))
    ret 
  )
)

;;; Return true if a needstore is empty.
(defun needstore-is-empty (storex) ; -> bool
  (assert (needstore-p storex))

  (null (needstore-needs storex))
)

;;; Return true if a needstore is not empty.
(defun needstore-is-not-empty (storex) ; -> bool
  (assert (needstore-p storex))

  (not (null (needstore-needs storex)))
)

;;; Push need into a needstore.
(defun needstore-push (storex nedx) ; -> nothing, side-effect needstore is changed.
  (assert (needstore-p storex))
  (assert (need-p nedx))

  (push nedx (needstore-needs storex))
)

;;; Add need to the end of a needstore.
(defun needstore-add-end (storex nedx) ; -> nothing, side-effect needstore changed.
  ;(format t "~&needstore-add-end: ~A ~A" (needstore-str storex) (need-str nedx))
  (assert (needstore-p storex))
  (assert (need-p nedx))

  (setf (needstore-needs storex) (append (needstore-needs storex) (list nedx)))
  ;(format t "~&needstore-add-end: result ~A" (needstore-str storex))
)

;;; Set the need dom-id for all needs in a needstore.
(defun needstore-set-dom-id (storex val) ; -> side-effect, needs dom-id changed.
  (assert (needstore-p storex))
  (assert (and (integerp val) (>= val 0)))

  (loop for nedx in (needstore-needs storex) do
    (setf (need-dom-id nedx) val)
  )
)

;;; Return the nth element of a NeedStore.
(defun needstore-nth (storex inx) ; -> need instance, or nil.
  (assert (needstore-p storex))
  (assert (integerp inx))

  (if (>= inx (needstore-length storex))
    (return-from needstore-nth nil))

  (nth inx (needstore-needs storex))
)

;;; Return the number of needs in a needstore.
(defun needstore-length (storex) ; -> number.
  (assert (needstore-p storex))

  (length (needstore-needs storex))
)

;;; Return a list of needs.
(defun needstore-need-list (storex) ; -> list of needs.
  (assert (needstore-p storex))

  (needstore-needs storex)
)

