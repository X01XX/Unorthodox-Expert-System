;; Rust will not let you serialize a struct that contains a reference.
;; Lisp silently converts multiple links to the same structure into multiple copies of the structure.
;; 
(load #p "err.lisp")
(load #p "value.lisp")

(setf x (value-new :num-bits 4 :bits 5))
(setf y (list x x))
(format t "~&y before changing first item ~A" (value-list-str y))

;; Set the bits value for the first item in list y to 6, from 5.
(setf (value-bits (car y)) 6)

;; Both x items in y point to the same place.
(assert (= 6 (value-bits (second y))))

(format t "~&y after changing first item  ~A" (value-list-str y))

;; Serialize list y.
(setf str (format nil "~S~%" y)) 
(format t "~&y serialized   ~A" str)

;; Deserialize list y.
(setf y2 (read-from-string str))
(format t "~&y deserialized ~A" (value-list-str y2))

;; Set the bits value for the first item in list y2 to 7, from 6.
(setf (value-bits (car y2)) 7)

;; Both items in y2 no longer point to the same place.
(assert (= 6 (value-bits (second y))))
(format t "~&y deserialized after changing the first item  ~A" (value-list-str y2))

