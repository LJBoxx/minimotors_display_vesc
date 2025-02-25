;Copyright © UNG Jeremy.  
;Licensed under the Proprietary Software License Agreement.  
;Unauthorized distribution or modification is strictly prohibited.  

(uart-start 1200)
(def rpm 0)
(def y 0)
(define tx-frame (array-create 15))

(bufset-u8 tx-frame 0 0x36)  ; First value: 0x36
(bufset-u8 tx-frame 1 0xCA)  ; Second value: 0xCA
(bufset-u8 tx-frame 2 0x00)  ; Third value: 0x00
(bufset-u8 tx-frame 3 0x68)  ; Fourth value: 0x68
(bufset-u8 tx-frame 4 0x64)  ; Fifth value: 0x64
(bufset-u8 tx-frame 5 0x64)  ; Sixth value: 0x64
(bufset-u8 tx-frame 6 0x00)  ; Seventh value: 0x00
(bufset-u8 tx-frame 7 0x66)  ; Eighth value: 0x65
(bufset-u8 tx-frame 8 0x1C)  ; Ninth value: 0x1C
(bufset-u8 tx-frame 9 0x64)  ; Tenth value: 0x64
(bufset-u8 tx-frame 10 0x69) ; Eleventh value: 0x69
(bufset-u8 tx-frame 11 0xD9) ; Twelfth value: 0xD9
(bufset-u8 tx-frame 12 0x95) ; Thirteenth value: 0x95
(bufset-u8 tx-frame 13 0x64) ; Fourteenth value: 0x64
(bufset-u8 tx-frame 14 0xCB) ; Fifteenth value: 0xC8

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

(defun read-frame ()
    (loopwhile t
        {
        (setq rpm (/ (abs (get-rpm)) 30))
        (bufset-u8 tx-frame 7 (+ 100 (/ rpm 256)))
        (bufset-u8 tx-frame 8 (+ 100 rpm))
        (checksum8)
        (uart-write tx-frame)
        (sleep 0.2)
        }
    )
)

(read-frame)
