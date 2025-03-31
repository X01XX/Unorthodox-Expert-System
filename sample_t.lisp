;;; Run tests.
(defun sample-tests ()
 (format t "~&sample-tests beginning")

 ; Test sample-new.
 (let (smplx sta1 sta2)
   (setf sta1 (state-from 's0001))
   (setf sta2 (state-from 's0010))

   ; Test creating a sample.
   (setf smplx (sample-new :initial sta1 :result sta2))
   (assert (sample-p smplx))

   (assert (state-eq (sample-initial smplx) sta1))
   (assert (state-eq (sample-result smplx) sta2))

   (format t "~&  sample-new OK")
 )

 (format t "~&sample-tests done")
  t
)
