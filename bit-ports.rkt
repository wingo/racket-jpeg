#lang racket
;; racket-jpeg
;; Copyright (C) 2014 Andy Wingo <wingo at pobox dot com>

;; This library is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This library is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A layer on top of ports for reading and writing bits in the
;; entropy-coded section of a JPEG.  This isn't a general bit-port
;; facility because it handles byte stuffing.
;;
;;; Code:

(provide make-bit-port
         read-bits
         read-bit
         read-signed-bits
         write-bits
         flush-bits)

(define (make-bit-port port)
  ;; Bit count, values, and the port
  (vector 0 0 port))

(define (next-u8 port)
  (let ((u8 (read-byte port)))
    (cond 
     ((eof-object? u8)
      (error "Got EOF while reading bits"))
     ((eqv? u8 #xff)
      (let ((u8 (read-byte port)))
        (unless (eqv? 0 u8)
          (if (eof-object? u8)
              (error "Got EOF while reading bits")
              (error "Found marker while reading bits"))))))
    u8))

(define (read-bits bit-port n)
  (match bit-port
    ((vector count bits port)
     (let lp ((count count) (bits bits))
       (cond
        ((<= n count)
         (vector-set! bit-port 0 (- count n))
         (bitwise-and (arithmetic-shift bits (- n count))
                      (sub1 (arithmetic-shift 1 n))))
        (else
         (let* ((u8 (next-u8 port))
                ;; We never need more than 16 bits in the buffer.
                (bits (+ (bitwise-and (arithmetic-shift bits 8) #xffff) u8)))
           (vector-set! bit-port 1 bits)
           (lp (+ count 8) bits))))))))

(define (read-bit bit-port)
  (read-bits bit-port 1))

(define (read-signed-bits bit-port n)
  (let ((bits (read-bits bit-port n)))
    (if (< bits (arithmetic-shift 1 (sub1 n)))
        (+ (arithmetic-shift -1 n) 1 bits)
        bits)))

(define (write-byte/stuff u8 port)
  (write-byte u8 port)
  (when (eqv? u8 #xff)
    (write-byte 0 port)))

(define (write-bits bit-port bits len)
  (cond
   ((negative? bits)
    (write-bits bit-port (- bits (add1 (arithmetic-shift -1 len))) len))
   (else
    (match bit-port
      ((vector count buf port)
       (let lp ((count count) (buf buf) (bits bits) (len len))
         (cond
          ((< (+ count len) 8)
           (vector-set! bit-port 0 (+ count len))
           (vector-set! bit-port 1
                        (bitwise-ior (arithmetic-shift buf len) bits)))
          (else
           (let* ((head-len (- 8 count))
                  (head-bits (bitwise-and
                              (arithmetic-shift bits (- head-len len))
                              (sub1 (arithmetic-shift 1 head-len))))
                  (tail-len (- len head-len))
                  (tail-bits (bitwise-and
                              bits
                              (sub1 (arithmetic-shift 1 tail-len)))))
             (write-byte/stuff
              (bitwise-ior (arithmetic-shift buf head-len) head-bits)
              port)
             (lp 0 0 tail-bits tail-len))))))))))

(define (flush-bits bit-port)
  (match bit-port
    ((vector count bits port)
     (unless (zero? count)
       ;; Pad remaining bits with 1, and stuff as needed.
       (let ((bits (bitwise-ior (arithmetic-shift bits (- 8 count))
                                (sub1 (arithmetic-shift 1 (- 8 count))))))
         (write-byte/stuff bits port))
       (vector-set! bit-port 0 0)
       (vector-set! bit-port 1 0)))))
