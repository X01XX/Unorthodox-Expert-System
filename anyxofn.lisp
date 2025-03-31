;;; Given a number of items, return any x number
;;; of unique combinations, where x >= 0 and <= number items,
;;; order does not matter.
;;;
;;; Call with num_items = Number of items in possible combinations.
;;;           items     = Items references to find combinations of.
;;;
;;; Number lists returned = N! / ((N-x)! * x!)
;;;
;;; e.g. any 2 of 4 items.
;;;
;;; 4! / (4-2)! * 2! = 4! / 2!2! = 24 / 4 = 6
;;;
;;; e.g. any 3 of 4 items.
;;;
;;; 4! / (4-3)! * 3! = 4! / 1!3! = 24 / 6 = 4
;;;
;;; ################################################################
(defun any-x-of-n (x alist)
  (assert (> x 0))
  (assert (not (> x (length alist))))

  ; The end of the count down of x.
  ; Return a list of each item as a list.
  (when (= x 1)
    (return-from any-x-of-n (mapcar #'list alist))
  )

  ; So x > 1
  (let (itemx ret)
    ; For each item in the list, return that element
    ; pushed to each item returned by any-x-of-n (x-1) (rest of list).
    (decf x)
    (loop repeat (- (length alist) x) do ; length calculated only once
	  (setq itemx (car alist))
    	  (setq alist (cdr alist))
  	  (setq ret (append ret  (mapcar #'(lambda (y) (push itemx y)) (any-x-of-n x alist))))
    )
    ret
  )
)

; >(any-x-of-n 1 '("a" "b" "c" "d"))
; 
; (("a") ("b") ("c") ("d"))
; 
; >(any-x-of-n 2 '("a" "b" "c" "d"))
; 
; (("a" "b") ("a" "c") ("a" "d") ("b" "c") ("b" "d") ("c" "d"))
; 
; >(any-x-of-n 3 '("a" "b" "c" "d"))
; 
; (("a" "b" "c") ("a" "b" "d") ("a" "c" "d") ("b" "c" "d"))
; 
; >(any-x-of-n 4 '("a" "b" "c" "d"))
; 
; (("a" "b" "c" "d"))

