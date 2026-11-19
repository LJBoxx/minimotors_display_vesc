;; Copyright (C) ljbox
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



