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

; Minimotors EY3 dash <-> VESC bridge
; Controller -> dash frame (15 bytes, 1200 baud),
;   B0  0x36 start
;   B1  sequence number (dash accepts a fixed value)
;   B2  0x00
;   B3  key: every data byte B4..B13 is sent as (key + value)
;   B4  brake / regulator flags
;   B5  regulator offset (also added on top of the speed high byte)
;   B6  0x00
;   B7  speed high = key + regulator + high
;   B8  speed low  = key + low
;   B9..B13 unknown, copied from a real capture
;   B14 XOR of B0..B13
; The static bytes are the known-good frame from bad-but-works-ey3.lisp,
; which is a captured "22 km/h, regulator on" frame.

(uart-start 1200)

(def rpm-div 30)       ; ERPM -> dash speed units
(def key 0x68)         ; B3
(def regulator 0xFC)   ; B5 - B3 in the captured frame
(def send-period 0.2)  ; seconds between frames

;; Speed limiter switch inside the dash.
;; Dash frame B9 is 0xA0 (160) with the switch on, 0x80 with it off,
;; so the switch is bit 5 (0x20).
(def use-limiter-switch t)
(def limited-speed (/ 25 3.6))   ; m/s
(def limited-watt 500)
(def unlimited-speed (/ 360 3.6))
(def unlimited-watt 25000)

;; Dash P-settings -> VESC config. Bytes per sYmphoman/minimo (unverified on
;; this dash). Off by default: if a byte is wrong, PA could read 0 and turn
;; regen braking off. Check the values in the REPL with (print rx) first.
;;   P8 power 5..100 %       (B7)  -> l-current-max-scale
;;   P9 battery save 1..3    (B11) -> extra factor on l-current-max-scale
;;   PA e-brake 0..5         (B10) -> l-current-min-scale (0 = no regen!)
;;   P7 acceleration 0..5    (B12) -> throttle ramp time
(def use-dash-settings nil)
(def p9-factor '(0.7 0.85 1.0))                 ; 1 = max save, 2 = mid, 3 = none
(def p7-ramp-s '(0.1 0.3 0.5 0.8 1.2 1.6))     ; 0 = punchy .. 5 = soft start
(def throttle-ramp 'adc-ramp-time-pos)         ; 'ppm-ramp-time-pos for PPM
(def dash-settings nil)                         ; last applied (p8 p9 pa p7)

(def tx-frame (bufcreate 15))
(def rx (bufcreate 15))
(def mode 'unknown)    ; 'limited / 'unlimited once the dash tells us

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
    (bufset-u8 tx-frame 7 (u8 (+ key regulator (shr rpm 8))))
    (bufset-u8 tx-frame 8 (u8 (+ key (u8 rpm))))
    (checksum8)
    (uart-write tx-frame)
})

;; only touch the config when the mode actually changes
(defun set-mode (new-mode)
    (if (not (eq new-mode mode)) {
        (setq mode new-mode)
        (if (eq new-mode 'limited)
            { (conf-set 'max-speed limited-speed) (conf-set 'l-watt-max limited-watt) }
            { (conf-set 'max-speed unlimited-speed) (conf-set 'l-watt-max unlimited-watt) })
    }))

(defun limiter-on () (> (bitwise-and (bufget-u8 rx 9) 0x20) 0))

(defun in-range (x lo hi) (and (>= x lo) (<= x hi)))

;; only touch the config when a setting changes; ignore out-of-range values
(defun apply-dash-settings () {
    (var p8 (bufget-u8 rx 7))
    (var p9 (bufget-u8 rx 11))
    (var pa (bufget-u8 rx 10))
    (var p7 (bufget-u8 rx 12))
    (var now (list p8 p9 pa p7))
    (if (and (not (eq now dash-settings))
             (in-range p8 5 100) (in-range p9 1 3)
             (in-range pa 0 5) (in-range p7 0 5)) {
        (setq dash-settings now)
        (conf-set 'l-current-max-scale (* (/ p8 100.0) (ix p9-factor (- p9 1))))
        (conf-set 'l-current-min-scale (/ pa 5.0))
        (conf-set throttle-ramp (ix p7-ramp-s p7))
    })
})

(defun rx-checksum-ok () {
    (var cs 0)
    (looprange i 0 14
        (setq cs (bitwise-xor cs (bufget-u8 rx i))))
    (= cs (bufget-u8 rx 14))
})

;; reader: sync on the 0xAA header, read the rest, drop frames with a bad XOR
(defun read-loop ()
    (loopwhile t {
        (uart-read-bytes rx 1 0)
        (if (= (bufget-u8 rx 0) 0xAA) {
            (uart-read-bytes rx 14 1)
            (if (rx-checksum-ok) {
                (if use-limiter-switch (set-mode (if (limiter-on) 'limited 'unlimited)))
                (if use-dash-settings (apply-dash-settings))
            })
        })
    }))

(defun send-loop ()
    (loopwhile t {
        (send-frame)
        (sleep send-period)
    }))

(if (or use-limiter-switch use-dash-settings) (spawn 200 read-loop))
(spawn send-loop)
