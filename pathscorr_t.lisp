;;; Run pathscorr tests.
(defun pathscorr-tests ()

  (format t "~&pathscorr-tests beginning")

  ; Test pathscorr-new.
  (let (pathscorr1)
    ; Test new, empty, pathscorr.
    (setf pathscorr1 (pathscorr-new nil))
    (assert (pathscorr-p pathscorr1))
    (assert (pathscorr-is-empty pathscorr1))

    ; Test new, non-empty, pathscorr.
    (setf pathscorr1 (pathscorr-new (list (regionscorr-new (list (region-from 'r01) (region-from 'r00))))))
    (assert (pathscorr-p pathscorr1))
    (assert (pathscorr-is-not-empty pathscorr1))

    (format t "~&  pathscorr-new OK")
  )

  ;; Basic logic.
  (let (paths cur-regs next-regs last-int next-int)
    (setf paths (pathscorr-new (list (regionscorr-new (list (region-from 'r0000)))
                                     (regionscorr-new (list (region-from 'r0x0x)))
                                     (regionscorr-new (list (region-from 'rx1x1)))
                                     (regionscorr-new (list (region-from 'r1x1x)))
                                     (regionscorr-new (list (region-from 'r1010))))))
    (format t "~&paths: ~A~& " (pathscorr-str paths))

    (setf last-int (pathscorr-first-region paths))

    (loop for inx from 0 below (- (pathscorr-length paths) 2) do

      (setf cur-regs  (nth (+ 1 inx) (pathscorr-regionscorr-list paths)))
      (setf next-regs (nth (+ 2 inx) (pathscorr-regionscorr-list paths)))

      (setf next-int (regionscorr-intersection cur-regs next-regs))

      (format t "~&~A to ~A in ~A" (regionscorr-str last-int) (regionscorr-str next-int) (regionscorr-str cur-regs))

      (cond ((= inx 0)
              (assert (and (regionscorr-eq last-int (regionscorr-from '(rc (r0000))))
                           (regionscorr-eq next-int (regionscorr-from '(rc (r0101))))
                           (regionscorr-eq cur-regs (regionscorr-from '(rc (r0X0X))))))
            )
            ((= inx 1)
              (assert (and (regionscorr-eq last-int (regionscorr-from '(rc (r0101))))
                           (regionscorr-eq next-int (regionscorr-from '(rc (r1111))))
                           (regionscorr-eq cur-regs (regionscorr-from '(rc (rX1X1))))))
            )
            ((= inx 2)
              (assert (and (regionscorr-eq last-int (regionscorr-from '(rc (r1111))))
                           (regionscorr-eq next-int (regionscorr-from '(rc (r1010))))
                           (regionscorr-eq cur-regs (regionscorr-from '(rc (r1X1X))))))
            )
      )
      (setf last-int next-int)
    )
    (format t "~& ~&  pathscorr-plan-logic OK")
  )

  ; Test pathscorr-add-start.
  (let (pathscorr1)
    (setf pathscorr1 (pathscorr-new (list (regionscorr-new (list (region-from 'r0x) (region-from 'r0x))))))
    (pathscorr-add-start pathscorr1 (regionscorr-new (list (region-from 'r01) (region-from 'r01))))
    (assert (= (pathscorr-length pathscorr1) 2))
    (assert (regionscorr-eq (pathscorr-first-region pathscorr1) (regionscorr-new (list (region-from 'r01) (region-from 'r01)))))
    (assert (regionscorr-eq (pathscorr-last-region pathscorr1) (regionscorr-new (list (region-from 'r0x) (region-from 'r0x)))))

    (format t "~&  pathscorr-add-start OK")
  )

  ; Test pathscorr-add-end.
  (let (pathscorr1)
    (setf pathscorr1 (pathscorr-new (list (regionscorr-new (list (region-from 'r01) (region-from 'r01))))))
    (pathscorr-add-end pathscorr1 (regionscorr-new (list (region-from 'r0x) (region-from 'r0x))))
    (assert (= (pathscorr-length pathscorr1) 2))
    (assert (regionscorr-eq (pathscorr-first-region pathscorr1) (regionscorr-new (list (region-from 'r01) (region-from 'r01)))))
    (assert (regionscorr-eq (pathscorr-last-region pathscorr1) (regionscorr-new (list (region-from 'r0x) (region-from 'r0x)))))

    (format t "~&  pathscorr-add-end OK")
  )

  (format t "~&pathscorr-tests done")
  t
)

