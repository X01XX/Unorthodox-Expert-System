;; Run square tests.
(defun square-tests ()
  (format t "~&square-tests beginning")

  ; Test square-new
  (let (sqr1 smpl)
    (setf smpl (sample-new :initial (state-from 's0001) :result (state-from 's0010)))
    (assert (sample-p smpl))

    (setf sqr1 (square-new smpl))
    (assert (square-p sqr1))

    (format t "~&  square-new OK")
  )

  ;; Test unpredictable square, three different results.
  (let (sqr1 smpl)
    (setf smpl (sample-new :initial (state-from 's0001) :result (state-from 's0010)))
    (setf sqr1 (square-new smpl))

    (setf smpl (sample-new :initial (state-from 's0001) :result (state-from 's1011)))
    (square-add-sample sqr1 smpl)

    (setf smpl (sample-new :initial (state-from 's0001) :result (state-from 's1001)))
    (square-add-sample sqr1 smpl)

    (assert (eq (square-pn sqr1) *pn-none*))
    (assert (square-pnc sqr1))

    (format t "~&  Three different results, OK")
  )

  ;; Test unpredictable square, two different results, with wrong order.
  ;; Change to two-result square, pnc = true, upon more samples.
  ;; Change to one-result square, pnc = true, upon more samples.
  (let (sqr1 smpl)
    (setf smpl (sample-new :initial (state-from 's0101) :result (state-from 's1010)))
    (setf sqr1 (square-new smpl))

    (setf smpl (sample-new :initial (state-from 's0101) :result (state-from 's1010)))
    (square-add-sample sqr1 smpl)

    (setf smpl (sample-new :initial (state-from 's0101) :result (state-from 's1001)))
    (square-add-sample sqr1 smpl)

    (assert (eq (square-pn sqr1) *pn-none*))
    (assert (square-pnc sqr1))

    (setf smpl (sample-new :initial (state-from 's0101) :result (state-from 's1010)))
    (square-add-sample sqr1 smpl)

    (setf smpl (sample-new :initial (state-from 's0101) :result (state-from 's1001)))
    (square-add-sample sqr1 smpl)

    (assert (eq (square-pn sqr1) *pn-two*))
    (assert (square-pnc sqr1))

    (setf smpl (sample-new :initial (state-from 's0101) :result (state-from 's1001)))
    (square-add-sample sqr1 smpl)

    (setf smpl (sample-new :initial (state-from 's0101) :result (state-from 's1001)))
    (square-add-sample sqr1 smpl)

    (setf smpl (sample-new :initial (state-from 's0101) :result (state-from 's1001)))
    (square-add-sample sqr1 smpl)

    (assert (eq (square-pn sqr1) *pn-one*))
    (assert (square-pnc sqr1))

    (format t "~&  Changing Pn with more samples, OK")
  )

  ;; Test two-result square.
  (let (sqr1 smpl)

    (setf smpl (sample-new :initial (state-from 's0101) :result (state-from 's1010)))
    (setf sqr1 (square-new smpl))

    (setf smpl (sample-new :initial (state-from 's0101) :result (state-from 's1011)))
    (square-add-sample sqr1 smpl)

    (assert (eq (square-pn sqr1) *pn-two*))
    (assert (null (square-pnc sqr1)))

    (setf smpl (sample-new :initial (state-from 's0101) :result (state-from 's1010)))
    (square-add-sample sqr1 smpl)

    (assert (eq (square-pn sqr1) *pn-two*))
    (assert (null (square-pnc sqr1)))

    (setf smpl (sample-new :initial (state-from 's0101) :result (state-from 's1011)))
    (square-add-sample sqr1 smpl)

    (assert (eq (square-pn sqr1) *pn-two*))
    (assert (square-pnc sqr1))

    (setf smpl (sample-new :initial (state-from 's0101) :result (state-from 's1010)))
    (square-add-sample sqr1 smpl)

    (assert (eq (square-pn sqr1) *pn-two*))
    (assert (square-pnc sqr1))

    (format t "~&  Two-result square, OK")
  )

  ;; Test unpredictable square, three different results.
  (let (sqr1 smpl)

    (setf smpl (sample-new :initial (state-from 's0101) :result (state-from 's1010)))
    (setf sqr1 (square-new smpl))

    (setf smpl (sample-new :initial (state-from 's0101) :result (state-from 's1011)))
    (square-add-sample sqr1 smpl)

    (setf smpl (sample-new :initial (state-from 's0101) :result (state-from 's1001)))
    (square-add-sample sqr1 smpl)

    (assert (eq (square-pn sqr1) *pn-none*))
    (assert (square-pnc sqr1))

    (format t "~&  Three-different-result square, OK")
  )

  ;; Test unpredictable square, two different results, with wrong order.
  ;; Change to two-result square, pnc = true, upon more samples.
  ;; Change to one-result square, pnc = true, upon more samples.
  (let (sqr1 smpl)

    (setf smpl (sample-new :initial (state-from 's0101) :result (state-from 's1010)))
    (setf sqr1 (square-new smpl))

    (setf smpl (sample-new :initial (state-from 's0101) :result (state-from 's1010)))
    (square-add-sample sqr1 smpl)

    (setf smpl (sample-new :initial (state-from 's0101) :result (state-from 's1001)))
    (square-add-sample sqr1 smpl)

    (assert (eq (square-pn sqr1) *pn-none*))
    (assert (square-pnc sqr1))

    (format t "~&  Two-out-of-order-results square, OK")
  )

  (format t "~&square-tests done")
)
