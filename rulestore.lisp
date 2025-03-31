



; Implement a store of rules.
(defstruct rulestore
  rules  ; A list of zero, or two, non-duplicate, same number bits, rules.
)
; Functions automatically created by defstruct:
;
; Most used:
;   (rulestore-<field name> <instance>) -> struct field.
;   (rulestore-p <instance>) -> bool
;
; Least used:
;   (type-of <instance>) -> rulestore
;   (typep <instance> 'rulestore) -> bool
;
; Probably shouldn't use:
;   (make-rulestore [:<field-name> <field-rulestore>]*), use rulestore-new instead.
;   (copy-rulestore <instance>) copies a rulestore instance.

; Return a rulestore given one, or two, rules.
(defun rulestore-new (rules) ; -> rulestore.
  ;(format t "~&rulestore-new")
  (assert (rules-list-p rules))

  (make-rulestore :rules rules)
)

(defun rulestore-length (storex) ; -> integer
  ;(format t "~&rulestore-length: ~A" storex)
  (assert (rulestore-p storex))

  (length (rulestore-rules storex))
)

(defun rulestore-initial-region (storex) ; -> region
  (assert (rulestore-p storex))

  (rule-initial-region (car (rulestore-rules storex)))
)

(defun rulestore-str (storex) ; -> string
  (if (not (rulestore-p storex))
    (format t "~&rulestore-str ? ~A" storex))

  (assert (rulestore-p storex))

  (let ((ret "(") (start t))

    (loop for rulx in (rulestore-rules storex) do
      (if start
        (setf start false)
        (setf ret (concatenate 'string ret ", ")))

      (setf ret (concatenate 'string ret (rule-str rulx)))
    )
    (setf ret (concatenate 'string ret ")"))
    ret
  )
)

; Return true if a two rulestores are equal.
(defun rulestore-eq (store1 store2) ; -> bool
  (assert (rulestore-p store1))
  (assert (rulestore-p store2))

  (if (/= (rulestore-length store1) (rulestore-length store2))
    (return-from rulestore-eq false))

  (let (found-eq)
    (loop for rulx in (rulestore-rules store1) do
      (setf found-eq false)
      (loop for ruly in (rulestore-rules store2) do
	(if (rule-eq rulx ruly)
	  (setf found-eq true))
      )
      (if (not found-eq)
        (return-from rulestore-eq false))
    )
    true
  )
)

; Return true if a rulestore is a subset of another.
; The subset store may have fewer rules that the suberset store.
(defun rulestore-subset-of (&key sub sup) ; -> bool
  (assert (rulestore-p sub))
  (assert (rulestore-p sup))

  (if (> (rulestore-length sub) (rulestore-length sup))
    (return-from rulestore-subset-of false))

  (let (found-sup)
    (loop for rulx in (rulestore-rules sub) do
      ;; Check for a superset rule for each subset rule.
      (setf found-sup false)
      (loop for ruly in (rulestore-rules sup) do

        (if (rule-subset-of :sub rulx :sup ruly)
	      (setf found-sup true))
      )
      (if (not found-sup)
        (return-from rulestore-subset-of false))
    )
    true
  )
)

;;; Return the first rule of a non-empty rulestore.
(defun rulestore-first (storex) ; -> rule.
  (assert (rulestore-p storex))
  (assert (> (rulestore-length storex) 0))

  (car (rulestore-rules storex))
)

;;; Return the second rule of a rulestore that has at least two rules.
(defun rulestore-second (storex) ; -> rule.
  (assert (rulestore-p storex))
  (assert (> (rulestore-length storex) 1))

  (second (rulestore-rules storex))
)

;;; Return true if a list is a list of rulestores.
;;; An empty list will return true.
(defun rulestore-list-p (rullst) ; -> bool
  ;(format t "~&rulestore-list-p: ~A" rullst)
  (if (not (listp rullst))
    (return-from rulestore-list-p false))

  (loop for rulx in rullst do
    (if (not (rulestore-p rulx))
      (return-from rulestore-list-p false))
  )
  true
)

;;; Translate a string into a rulestore.
;;; Like [], [[01/10]], or [[01/10], [00/11/11]].
(defun rulestore-from-str (rsx) ; -> rulestore
   ;(format t "~&rulestore-from-str ~A" rsx)
   (when (stringp rsx)

      (if (not (string-equal (subseq rsx 0 1) "["))
        (return-from rulestore-from-str (err-new "String must begin with a [")))
            
      (if (not (string-equal (subseq rsx (1- (length rsx))) "]"))
        (return-from rulestore-from-str (err-new "String must end with a ]")))
 
     (if (= (length rsx) 2)
        (return-from rulestore-from-str (make-rulestore :rules nil)))
  
     (setf rsx (parse-str (subseq rsx 1 (1- (length rsx)))))
   )

   (assert (listp rsx))

    (let (rules store)
        (loop for tokx in rsx do
            (push (rule-from tokx) rules)
        )
       
       (setf store (rulestore-new (reverse rules)))
       ;(format t "~&   returning store ~A" (rulestore-str store))
       store
  )
)

;;; Return the number of bits used in a rulestore.
(defun rulestore-num-bits (storex) ; -> number bits used.
  (assert (rulestore-p storex))
  (assert (> (rulestore-length storex) 0))

  (rule-num-bits (rulestore-first storex))
)

;;; Return a valid intersection of two rulestares, or nil.
(defun rulestore-intersection (storex storey) ; -> rulestore, or nil.
  (assert (rulestore-p storex))
  (assert (rulestore-p storey))

  (if (not (region-intersects (rulestore-initial-region storex) (rulestore-initial-region storey)))
    (return-from rulestore-intersection nil))

  (when (= (rulestore-length storex) 1)
    (let (ret)
      (setf ret (rule-intersection (rulestore-first storex) (rulestore-first storey)))
      (if (rule-is-valid-intersection ret)
        (return-from rulestore-intersection (rulestore-new (list ret)))
        (return-from rulestore-intersection nil))
    )
  )
  (when (= (rulestore-length storex) 2)
    (let (rul1 rul2 rul3 rul4)
      (setf rul1 (rule-intersection (rulestore-first storex) (rulestore-first storey)))
      (setf rul2 (rule-intersection (rulestore-second storex) (rulestore-second storey)))
      (setf rul3 (rule-intersection (rulestore-first storex) (rulestore-second storey)))
      (setf rul4 (rule-intersection (rulestore-second storex) (rulestore-first storey)))

      (when (and (rule-is-valid-intersection rul1) (rule-is-valid-intersection rul2))
          (return-from rulestore-intersection (rulestore-new (list rul1 rul2)))
      )

      (when (and (rule-is-valid-intersection rul3) (rule-is-valid-intersection rul4))
          (return-from rulestore-intersection (rulestore-new (list rul3 rul4)))
      )
    )
  )
  nil
)

;;; Return t if a rulestore is empty.
(defun rulestore-is-empty (storex) ; -> bool
  ;(format t "~&rulestore-is-empty: ~A" (type-of storex))
  ;(format t "~&rulestore-is-empty: ~A" storex)
  (assert (rulestore-p storex))

  (null (rulestore-rules storex))
)
  
;;; Return t if a rulestore is not empty.
(defun rulestore-is-not-empty (storex) ; -> bool
  (assert (rulestore-p storex))

  (not (null (rulestore-rules storex)))
)
  
;;; Return the nth element of a RuleStore.
(defun rulestore-nth (storex inx) ; -> rule instance, or nil.
  (assert (rulestore-p storex))
  (assert (integerp inx))

  (if (>= inx (rulestore-length storex))
    (return-from rulestore-nth nil))

  (nth inx (rulestore-rules storex))
)

(defun rulestore-union (storex storey) ; -> rulestore instance, or nil.
  ;(format t "~&rulestore-union: ~A ~A" (type-of storex) (type-of storey))
  (assert (rulestore-p storex))
  (assert (rulestore-p storey))
  (assert (= (rulestore-length storex) (rulestore-length storey)))
  (assert (> (rulestore-length storex) 0))
  (assert (< (rulestore-length storex) 3))
  
  (when (= 1 (rulestore-length storex))
    (let (unx)
      (setf unx (rule-union (rulestore-first storex) (rulestore-first storey)))
      (if unx
        (return-from rulestore-union (rulestore-new (list unx)))
        (return-from rulestore-union nil))
    )
  )

  (when (= 2 (rulestore-length storex))
    (let (unx uny rul1 rul2)
      (setf rul1 (rule-union (rulestore-first storex) (rulestore-first storey)))
      (setf rul2 (rule-union (rulestore-second storex) (rulestore-second storey)))
      (if (and (not (null rul1)) (not (null rul2)))
        (setf unx (rulestore-new (list rul1 rul2))))

      (setf rul1 (rule-union (rulestore-first storex) (rulestore-second storey)))
      (setf rul2 (rule-union (rulestore-second storex) (rulestore-first storey)))
      (if (and (not (null rul1)) (not (null rul2)))
        (setf uny (rulestore-new (list rul1 rul2))))

     ;(format t "~&unx ~A uny ~A" (type-of unx) (type-of uny))
     (if (and (null unx) (null uny))
        (return-from rulestore-union nil))

     (if (null unx)
        (return-from rulestore-union uny))

     (if (null uny)
        (return-from rulestore-union unx))

     nil
   )
  )
)

;;; Return true if a rulestore is invalidated by a square.
(defun rulestore-invalidated-by-square(storex sqrx) ; -> bool
  (assert (rulestore-p storex))
  (assert (square-p sqrx))

  ;; Handle group unpredictable, but square is predictable.
  (if (zerop (rulestore-length storex)) ; unpredictable rulestore.

      ;; Handle unpredictable, but square is predictable.
      (if (and (pn-ne (square-pn sqrx) *pn-none*) (square-pnc sqrx))
        (return-from rulestore-invalidated-by-square true)
        (return-from rulestore-invalidated-by-square false)) ; Square unpredictable, or more samples needed.

      ;; Rulestore length ge zero.

      ;; Check for unpredicable square.
      (if (pn-eq (square-pn sqrx) *pn-none*)
        (return-from rulestore-invalidated-by-square true)
                     
        ;; Square rulestore length ge zero.

        ;; Check if square has more rules than the store.
        (if (> (rulestore-length (square-rules sqrx)) (rulestore-length storex))
          (return-from rulestore-invalidated-by-square true)

          ;; Check if square rules are subset of group rules.
          (if (rulestore-subset-of :sub (square-rules sqrx) :sup storex)
            (return-from rulestore-invalidated-by-square false)
            (return-from rulestore-invalidated-by-square true))))
  ) ; end-if
)

;;; Return a list of rules.
(defun rulestore-rules-list (storex) ; -> list of rules.
  (assert (rulestore-p storex))

  (rulestore-rules storex)
)
  
