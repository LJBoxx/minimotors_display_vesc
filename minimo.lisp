(uart-start 1200)
(def count 0)
(def rpm 0)
(def y 0)
(def random)

;; Create an array to hold the frame data
(define tx-frame (array-create 15))
(define rx (array-create 15))

;; Initialize the frame data
(bufset-u8 tx-frame 0 0x36)              ;; Set byte 0
(bufset-u8 tx-frame 6 0x00)              ;; Set byte 6

(defun checksum8 ()
{
(bufset-u8 tx-frame 14 0x00)
  (looprange i 0 14
    {
        (setq y (bufget-u8 tx-frame 14))
        (bufset-u8 tx-frame 14 (bitwise-xor y (bufget-u8 tx-frame i)))
    }
  )
}
)


(defun send-frame ()
    {
    (setq count (bufget-u8 rx 2))
    (setq random (bufget-u8 rx 5))
    (setq rpm (/ (abs (get-rpm)) 30))
    (bufset-u8 tx-frame 1 count)
    (bufset-u8 tx-frame 3 random)
    (bufset-u8 tx-frame 4 random)
    (bufset-u8 tx-frame 5 random)
    (bufset-u8 tx-frame 7 (+ random (/ rpm 256)))
    (bufset-u8 tx-frame 8 (+ random rpm))
    (bufset-u8 tx-frame 9 random)
    (bufset-u8 tx-frame 10 random)
    (bufset-u8 tx-frame 11 random)
    (bufset-u8 tx-frame 12 random)
    (bufset-u8 tx-frame 13 random)
    (checksum8)
    (send-data tx-frame)
    (print tx-frame)
    }
)

(defun read-frame ()
    (loopwhile t
        {
        (uart-read rx 15 nil nil 1)
        (print (list rx))
        (send-frame)
        }
    )
)

(read-frame)
;loop



