(uart-start 1200)
(def rpm 0)
(def y 0)
;; modes values
(def limited-speed (/ 25 3.6))
(def limited-watt 500)
(def unlimited-speed (/ 360 3.6))
(def unlimited-watt 25000)
;; Create an array to hold the frame data
(define tx-frame (array-create 15))
(define rx (array-create 15))

;; Initialize the frame data
(bufset-u8 tx-frame 0 0x36)  ; First value: 0x36
(bufset-u8 tx-frame 3 0x5e)  ; Fourth value: 0x68
(bufset-u8 tx-frame 4 0x5e)  ; Fifth value: 0x64
(bufset-u8 tx-frame 5 0x5e)  ; Sixth value: 0x64
(bufset-u8 tx-frame 8 0x1C)  ; Ninth value: 0x1C
(bufset-u8 tx-frame 9 0x5c)  ; Tenth value: 0x64
(bufset-u8 tx-frame 10 0x61) ; Eleventh value: 0x69
(bufset-u8 tx-frame 11 0xD1) ; Twelfth value: 0xD9
(bufset-u8 tx-frame 12 0x8f) ; Thirteenth value: 0x95
(bufset-u8 tx-frame 13 0x5e) ; Fourteenth value: 0x64

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
    (setq rpm (/ (abs (get-rpm)) 21))
    (bufset-u8 tx-frame 7 (+ 94 (/ rpm 256)))
    (bufset-u8 tx-frame 8 (+ 94 rpm))
    (checksum8)
    (uart-write tx-frame)
    }
)


(defun read-frame ()
    (loopwhile t
        {
            (uart-read rx 15 0)
            (if (= (bufget-u8 rx 0) 170)
                {
                    (if (eq (bufget-u8 rx 9) 160)
                        {
                            (conf-set 'max-speed limited-speed) (conf-set 'l-watt-max limited-watt)
                        }

                        {
                            (conf-set 'max-speed unlimited-speed) (conf-set 'l-watt-max unlimited-watt)
                        }
                    )
                }
            )
            (send-frame)
        }
    )
)

(spawn 150 read-frame)
;loop

