#lang racket
;; guile-jpeg
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
;; JPEG Huffman coding.
;;
;;; Code:

(require jpeg/bit-ports)
(provide make-huffman-table
         print-huffman-table
         read-huffman-coded-value
         compute-huffman-table-for-freqs)

(define (make-huffman-table size-counts values)
  (let* ((count (bytes-length values))
         (size-offsets (make-vector 16 #f))
         (sizes (make-bytes count 0))
         (codes (make-vector count #f))
         (value-indexes (make-vector 256 #f))
         (max-codes (make-vector 16 -1)))
    ;; A reverse map from value to index.
    (for ((i (in-naturals))
          (value values))
      (vector-set! value-indexes value i))
    ;; Compute sizes for each value.
    (let lp ((size 0) (offset 0))
      (when (< size 16)
        (vector-set! size-offsets size offset)
        (let ((size-count (bytes-ref size-counts size)))
          (let lp ((i 0))
            (when (< i size-count)
              (bytes-set! sizes (+ offset i) (add1 size))
              (lp (add1 i))))
          (lp (add1 size) (+ offset size-count)))))
    ;; Compute codes.  This is the algorithm from Annex C, verbatim.
    (let lp ((k 0) (code 0) (si (bytes-ref sizes 0)))
      (vector-set! max-codes (sub1 si) code)
      (vector-set! codes k code)
      (let ((code (add1 code)) (k (add1 k)))
        (when (< k (bytes-length sizes))
          (let lp2 ((code code) (si si))
            (if (= (bytes-ref sizes k) si)
                (lp k code si)
                (lp2 (arithmetic-shift code 1) (add1 si)))))))
    ;; Done.
    (vector size-counts size-offsets
            values value-indexes sizes codes max-codes)))

(define (print-huffman-table table)
  (match table
    ((vector size-counts size-offsets
             values value-indexes sizes codes max-codes)
     (let lp ((n 0))
       (when (< n (bytes-length values))
         (let ((si (bytes-ref sizes n))
               (code (vector-ref codes n))
               (value (bytes-ref values n)))
           (format #t "~a: #*~v,'0b -> #x~2,0x\n" n si code value)
           (lp (add1 n))))))))

(define (read-huffman-coded-value bit-port table)
  ;(print-huffman-table table)
  (match table
    ((vector size-counts size-offsets
             values value-indexes sizes codes max-codes)
     (let lp ((size-idx 0) (code (read-bit bit-port)))
       (cond
        ((<= code (vector-ref max-codes size-idx))
         (let* ((size-offset (vector-ref size-offsets size-idx))
                (idx (+ size-offset (- code (vector-ref codes size-offset)))))
           (unless (>= code (vector-ref codes size-offset))
             (error "impossaurus"))
           (bytes-ref values idx)))
        (else
         (lp (add1 size-idx)
             (+ (arithmetic-shift code 1) (read-bit bit-port)))))))))

(define (vector-inc! v idx addend)
  (vector-set! v idx (+ (vector-ref v idx) addend)))

(define (compute-huffman-code-sizes-for-freqs freqs)
  (let ((sizes (make-bytes 257 0))
        (others (make-vector 257 #f))
        (max-size 0))
    (define (inc-size! code)
      (let ((size (add1 (bytes-ref sizes code))))
        (bytes-set! sizes code size)
        (when (< max-size size)
          (set! max-size size))))
    (define (find-least-idx)
      (let lp ((i 0) (least-idx #f))
        (if (< i 257)
            (lp (add1 i)
                (let ((x (vector-ref freqs i)))
                  (cond ((zero? x) least-idx)
                        ((not least-idx) i)
                        ((<= x (vector-ref freqs least-idx)) i)
                        (else least-idx))))
            least-idx)))
    (define (find-next-least least-idx)
      (let lp ((i 0) (next-least-idx #f))
        (if (< i 257)
            (lp (add1 i)
                (let ((x (vector-ref freqs i)))
                  (cond ((zero? x) next-least-idx)
                        ((= i least-idx) next-least-idx)
                        ((not next-least-idx) i)
                        ((<= x (vector-ref freqs next-least-idx)) i)
                        (else next-least-idx))))
            next-least-idx)))
    (let lp ((v1 256))
      (cond
       ((find-next-least v1)
        => (lambda (v2)
             (vector-inc! freqs v1 (vector-ref freqs v2))
             (vector-set! freqs v2 0)
             (let lp ((v1 v1))
               (inc-size! v1)
               (cond
                ((vector-ref others v1) => lp)
                (else
                 (vector-set! others v1 v2)
                 (let lp ((v2 v2))
                   (inc-size! v2)
                   (cond
                    ((vector-ref others v2) => lp))))))
             (lp (find-least-idx))))
       (else (values sizes max-size))))))

(define (compute-huffman-table-for-freqs freqs)
  (define (bytes-truncate bv len)
    (if (< len (bytes-length bv))
        (subbytes bv 0 len)
        bv))
  (call-with-values (lambda ()
                      (let ((copy (make-vector 257)))
                        (vector-copy! copy 0 freqs)
                        ;; Add dummy entry.
                        (vector-set! copy 256 1)
                        (compute-huffman-code-sizes-for-freqs copy)))
    (lambda (sizes max-size)
      (let ((size-counts (make-bytes (max max-size 16) 0)))
        (define (inc-size-count! size n)
          (bytes-set! size-counts size
                              (+ (bytes-ref size-counts size) n)))
        (let count-bits ((i 0))
          (when (< i 257)
            (let ((size (bytes-ref sizes i)))
              (unless (zero? size)
                (inc-size-count! (sub1 size) 1)))
            (count-bits (add1 i))))
        (let adjust-bits ((i (sub1 max-size)))
          (cond
           ((zero? (bytes-ref size-counts i))
            (adjust-bits (sub1 i)))
           ((< i 16)
            ;; We're done.  Remove the dummy entry.
            (inc-size-count! i -1))
           (else
            ;; We have a code that is > 16 bits long.  Reshuffle the
            ;; tree to fit the code into 16 bits.
            (let lp ((j (- i 2)))
              (cond
               ((zero? (bytes-ref size-counts j))
                (lp (sub1 j)))
               (else
                (inc-size-count! i -2)
                (inc-size-count! (sub1 i) 1)
                (inc-size-count! (add1 j) 2)
                (inc-size-count! j -1))))
            (adjust-bits i))))
        ;; Sort values, then compute codes.
        (let* ((count (for/fold ((sum 0)) ((count size-counts))
                        (+ sum count)))
               (values (make-bytes count 0)))
          (let visit-size ((size 1) (k 0))
            (when (<= size max-size)
              (let visit-values ((j 0) (k k))
                (cond
                 ((= j 256)
                  (visit-size (add1 size) k))
                 ((= (bytes-ref sizes j) size)
                  (bytes-set! values k j)
                  (visit-values (add1 j) (add1 k)))
                 (else
                  (visit-values (add1 j) k))))))
          (make-huffman-table (bytes-truncate size-counts 16)
                              values))))))
