;; Copyright (C) UNG Jeremy
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
