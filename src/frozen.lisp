(setq board 'nano)

(cond
  ((eq board 'mega)
   (defun pin-a () (8bits 32))
   (defun ddr-a (val) (8bits 33 val))
   (defun port-a (val) (8bits 34 val))

   (defun pin-b () (8bits 35))
   (defun ddr-b (val) (8bits 36 val))
   (defun port-b (val) (8bits 37 val))

   (defun pin-c () (8bits 38))
   (defun ddr-c (val) (8bits 39 val))
   (defun port-c (val) (8bits 40 val))

   (defun pin-d () (8bits 41))
   (defun ddr-d (val) (8bits 42 val))
   (defun port-d (val) (8bits 43 val))

   (defun pin-e () (8bits 44))
   (defun ddr-e (val) (8bits 45 val))
   (defun port-e (val) (8bits 46 val))

   (defun pin-f () (8bits 47))
   (defun ddr-f (val) (8bits 48 val))
   (defun port-f (val) (8bits 49 val))

   (defun pin-g () (8bits 50))
   (defun ddr-g (val) (8bits 51 val))
   (defun port-g (val) (8bits 52 val)))

  ((eq board 'nano)
   (defun pin-b () (8bits 3))
   (defun ddr-b (val) (8bits 4 val))
   (defun port-b (val) (8bits 5 val))

   (defun pin-c () (8bits 6))
   (defun ddr-c (val) (8bits 7 val))
   (defun port-c (val) (8bits 8 val))

   (defun pin-d () (8bits 9))
   (defun ddr-d (val) (8bits 10 val))
   (defun port-d (val) (8bits 11 val)))

  ((eq board 'uno)
   nil))

