;;; Run tests.
(defun regionstore-tests ()
  (format t "~&regionstore-tests beginning")

  ; Test regionstore-new.
  (let (reg1 reg2 store1)

    (setf reg1 (region-from 'r0x10))
    (setf reg2 (region-from 'r1x10))

    (setf store1 (regionstore-new (list reg1 reg2)))
    (assert (regionstore-p store1))
    (assert (= (regionstore-length store1) 2))

    (format t "~&  regionstore-new OK")
  )

  ; Test regionstore-subtract-region.
  (let (store1 store2 store3)

    (setf store1 (regionstore-new (list (region-from 'rXXXX))))

    (setf store2 (regionstore-subtract-region store1 (region-from 'rX111)))
    ;(format t "~&store2 ~A" store2)
    (assert (= (regionstore-length store2) 3))
    (assert (regionstore-member store2 (region-from 'rXXX0)))
    (assert (regionstore-member store2 (region-from 'rXX0X)))
    (assert (regionstore-member store2 (region-from 'rX0XX)))

    (setf store3 (regionstore-subtract-region store2 (region-from 'r000x)))
    ;(format t "~&store3 ~A" store3)
    (assert (= (regionstore-length store3) 7))
    (assert (regionstore-member store3 (region-from 'rX01X)))
    (assert (regionstore-member store3 (region-from 'r10XX)))
    (assert (regionstore-member store3 (region-from 'rX10X)))
    (assert (regionstore-member store3 (region-from 'r1X0X)))
    (assert (regionstore-member store3 (region-from 'rXX10)))
    (assert (regionstore-member store3 (region-from 'rX1X0)))
    (assert (regionstore-member store3 (region-from 'r1XX0)))

    (format t "~&  regionstore-subtract-region OK")
  )

  ; Test regionstore-append.
  (let (store1 store2 store3)
    (setf store1 (regionstore-new (list (region-from 'rX000) (region-from 'rX001))))
    (setf store2 (regionstore-new (list (region-from 'rX001) (region-from 'rX011))))
    (setf store3 (regionstore-append store1 store2))
    ;(format t "~&store3 ~A" store3)
    (assert (= 4 (regionstore-length store3)))
    (assert (regionstore-member store3 (region-from 'rX000)))
    (assert (regionstore-member store3 (region-from 'rX001)))
    (assert (regionstore-member store3 (region-from 'rX011)))

    (format t "~&  regionstore-append OK")
  )

  ;; Test regionstore-from.
  (let (regs1 regs2 regs3)
    (setf regs1 (regionstore-from nil))
    (assert (= (regionstore-length regs1) 0))

    (setf regs2 (regionstore-from '(r1010)))
    (assert (= (regionstore-length regs2) 1))

    (setf regs3 (regionstore-from '(r1011 r111)))
    (assert (= (regionstore-length regs3) 2))

    (format t "~&  regionstore-from OK")
  )

  ;; Test regionstore-largest-intersections
  (let (regs1 ints)
    (setf regs1 (regionstore-from '(r0x0x r0XX1 r0x1x)))

    (setf ints (regionstore-largest-intersections regs1))
    ;(format t "~&ints ~A" ints)
    (assert (= 2 (regionstore-length ints)))
    (assert (regionstore-member ints (region-from 'r0X11)))
    (assert (regionstore-member ints (region-from 'r0X01)))

    (format t "~&  regionstore-largest-intersections OK")
  )

  ;; Test regionstore-split-by-intersections
  (let (regst1 fragments)
    (setf regst1 (regionstore-from '(r01x1 r011x)))
    (setf fragments (regionstore-split-by-intersections regst1))
    ;(format t "~&fragments ~A" fragments)
    (assert (= (regionstore-length fragments) 3))
    (assert (regionstore-member fragments (region-from 'r0101)))
    (assert (regionstore-member fragments (region-from 'r0110)))
    (assert (regionstore-member fragments (region-from 'r0101)))

    (setf regst1 (regionstore-from '(r01x1 r011x r0x11)))
    (setf fragments (regionstore-split-by-intersections regst1))
    ;(format t "~&fragments ~A" fragments)
    (assert (= (regionstore-length fragments) 4))
    (assert (regionstore-member fragments (region-from 'r0101)))
    (assert (regionstore-member fragments (region-from 'r0110)))
    (assert (regionstore-member fragments (region-from 'r0011)))
    (assert (regionstore-member fragments (region-from 'r0111)))

    (setf regst1 (regionstore-from '(r01x1 r011x rxx11)))
    (setf fragments (regionstore-split-by-intersections regst1))
    ;(format t "~&fragments ~A" fragments)
    (assert (= (regionstore-length fragments) 5))
    (assert (regionstore-member fragments (region-from 'r0101)))
    (assert (regionstore-member fragments (region-from 'r0110)))
    (assert (regionstore-member fragments (region-from 'rX011)))
    (assert (regionstore-member fragments (region-from 'r1X11)))
    (assert (regionstore-member fragments (region-from 'r0111)))

    (setf regst1 (regionstore-from '(rx10x rx1x1)))
    (setf fragments (regionstore-split-by-intersections regst1))
    ;(format t "~&fragments ~A" fragments)
    (assert (= (regionstore-length fragments) 3))
    (assert (regionstore-member fragments (region-from 'rX100)))
    (assert (regionstore-member fragments (region-from 'rX111)))
    (assert (regionstore-member fragments (region-from 'rX101)))

    ;; Test regions with subsets.
    (setf regst1 (regionstore-from '(rxxxx rx1x1 r01x1 rx101)))
    (setf fragments (regionstore-split-by-intersections regst1))
    ;(format t "~&fragments ~A" fragments)
    (assert (= (regionstore-length fragments) 6))
    (assert (regionstore-member fragments (region-from 'rXXX0)))
    (assert (regionstore-member fragments (region-from 'rX0XX)))
    (assert (regionstore-member fragments (region-from 'r1111)))
    (assert (regionstore-member fragments (region-from 'r0111)))
    (assert (regionstore-member fragments (region-from 'r1101)))
    (assert (regionstore-member fragments (region-from 'r0101)))

    (format t "~&  regionstore-split-by-intersections OK")
  )

  ;; Test adjacent, dinimilar squares.
  (let* ((max-reg (region-from 'rXXXX)) not-5 not-7 pos-57 not-8 not-c pos-8c pos-regs
        )
      (setf not-5 (region-subtract :min-reg max-reg :sub-reg (region-from 'r0101)))
      (setf not-7 (region-subtract :min-reg max-reg :sub-reg (region-from 'r0111)))
      (setf pos-57 (regionstore-union not-5 not-7))
      ;(format t "~&pos-57: ~A" (regionstore-str pos-57))

      (setf not-8 (region-subtract :min-reg max-reg :sub-reg (region-from 'r1000)))
      (setf not-c (region-subtract :min-reg max-reg :sub-reg (region-from 'r1100)))
      (setf pos-8c (regionstore-union not-8 not-c))
      ;(format t "~&pos-8c: ~A" (regionstore-str pos-8c))

      (setf pos-regs (regionstore-intersection pos-57 pos-8c))
      ;(format t "~&pos-regs: ~A" (regionstore-str pos-regs))
      (assert (string-equal (regionstore-str pos-regs)
               "RS[rX1X0, r0XX0, rXX1X, rX0XX, r1XX1, r11XX, rXX01, rX10X, r0X0X]"))
  )

  (format t "~&regionstore-tests done")
  t
)
