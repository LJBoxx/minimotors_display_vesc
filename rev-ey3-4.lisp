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

; Minimotors EY3 dash <-> VESC bridge
; Frame bytes are taken 1:1 from bad-but-works-ey3.lisp (the known-good version).

(uart-start 1200)

(def rpm-div 30)       ; ERPM -> dash speed units
(def key 0x64)         ; offset added to every data byte
(def send-period 0.2)  ; seconds between frames

;; Optional: switch speed/power limits from the dash mode button.
;; Untested - leave nil until you've confirmed the dash really sends
;; 0xAA frames with byte 9 = 160 in limited mode.
(def use-dash-mode nil)
(def limited-speed (/ 25 3.6))   ; m/s
(def limited-watt 500)
(def unlimited-speed (/ 360 3.6))
(def unlimited-watt 25000)

(def tx-frame (bufcreate 15))
(def rx (bufcreate 15))
(def mode 'unknown)

;; static part of the frame (same as bad-but-works)
(bufset-u8 tx-frame 0 0x36)
(bufset-u8 tx-frame 1 0xCA)
(bufset-u8 tx-frame 2 0x00)
(bufset-u8 tx-frame 3 0x68)
(bufset-u8 tx-frame 4 0x64)
(bufset-u8 tx-frame 5 0x64)
(bufset-u8 tx-frame 6 0x00)
(bufset-u8 tx-frame 9 0x64)
(bufset-u8 tx-frame 10 0x69)
(bufset-u8 tx-frame 11 0xD9)
(bufset-u8 tx-frame 12 0x95)
(bufset-u8 tx-frame 13 0x64)

(defun u8 (x) (bitwise-and x 0xFF))

;; XOR of bytes 0..13 into byte 14
(defun checksum8 () {
    (var cs 0)
    (looprange i 0 14
        (setq cs (bitwise-xor cs (bufget-u8 tx-frame i))))
    (bufset-u8 tx-frame 14 cs)
})

(defun send-frame () {
    (var rpm (to-i (/ (abs (get-rpm)) rpm-div)))
    (bufset-u8 tx-frame 7 (u8 (+ key (shr rpm 8))))
    (bufset-u8 tx-frame 8 (u8 (+ key (u8 rpm))))
    (checksum8)
    (uart-write tx-frame)
})

(defun set-mode (new-mode)
    (if (not (eq new-mode mode)) {
        (setq mode new-mode)
        (if (eq new-mode 'limited)
            { (conf-set 'max-speed limited-speed) (conf-set 'l-watt-max limited-watt) }
            { (conf-set 'max-speed unlimited-speed) (conf-set 'l-watt-max unlimited-watt) })
    }))

;; reader: sync on the 0xAA header, then read the rest of the frame
(defun read-loop ()
    (loopwhile t {
        (uart-read-bytes rx 1 0)
        (if (= (bufget-u8 rx 0) 0xAA) {
            (uart-read-bytes rx 14 1)
            (set-mode (if (= (bufget-u8 rx 9) 160) 'limited 'unlimited))
        })
    }))

(defun send-loop ()
    (loopwhile t {
        (send-frame)
        (sleep send-period)
    }))

(if use-dash-mode (spawn 150 read-loop))
(spawn send-loop)
