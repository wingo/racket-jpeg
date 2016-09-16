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
;; Forward and inverse JPEG discrete cosine transforms.
;;
;;; Code:

(require math/array jpeg/jfif jpeg/pixbufs rnrs/bytevectors-6)

(provide jpeg->planar-image planar-image->jpeg)

(define fdct-coefficients
  (let ((pi (* 2 (acos 0))))
    (build-array
     #(8 8)
     (match-lambda
       ((vector u v)
        ;; FIXME: Produce literal f32vector here.
        (for/vector ((k (in-range (* 8 8))))
          (call-with-values (lambda ()
                              (values (quotient k 8) (remainder k 8)))
            (lambda (i j)
              (let ((Cu (if (zero? u) (/ 1 (sqrt 2)) 1))
                    (Cv (if (zero? v) (/ 1 (sqrt 2)) 1)))
                (* 1/4 Cu Cv
                   (cos (/ (* (+ (* 2 i) 1) u pi) 16))
                   (cos (/ (* (+ (* 2 j) 1) v pi) 16))))))))))))

(define idct-coefficients
  (let ((pi (* 2 (acos 0))))
    (build-array
     #(8 8)
     (match-lambda
       ((vector i j)
        (for/vector ((k (in-range (* 8 8))))
          (call-with-values (lambda ()
                              (values (quotient k 8) (remainder k 8)))
            (lambda (u v)
              (let ((Cu (if (zero? u) (/ 1 (sqrt 2)) 1))
                    (Cv (if (zero? v) (/ 1 (sqrt 2)) 1)))
                (* 1/4 Cu Cv
                   (cos (/ (* (+ (* 2 i) 1) u pi) 16))
                   (cos (/ (* (+ (* 2 j) 1) v pi) 16))))))))))))

(define (idct-block block plane pos stride)
  (define (idct i j)
    (let ((coeffs (array-ref idct-coefficients (vector i j))))
      (let lp ((k 0) (sum 0.0))
        (if (< k 64)
            (let ((Suv (vector-ref block k)))
              (lp (add1 k)
                  (if (zero? Suv)
                      sum
                      (+ sum (* (vector-ref coeffs k) Suv)))))
            sum))))
  (let lp ((i 0) (pos pos))
    (when (< i 8)
      (let lp ((j 0))
        (when (< j 8)
          (let* ((s (idct i j))
                 (sq (cond
                      ((< s -128.0) 0)
                      ((> s 127.0) 255)
                      (else (+ 128 (inexact->exact (round s)))))))
            (bytevector-u8-set! plane (+ pos j) sq))
          (lp (add1 j))))
      (lp (add1 i) (+ pos stride)))))

(define (jpeg->planar-image jpeg)
  (match jpeg
    ((jfif frame misc-segments mcu-array)
     (let ((mcu-width (frame-mcu-width frame))
           (mcu-height (frame-mcu-height frame)))
       (planar-image
        (frame-x frame)
        (frame-y frame)
        (* mcu-width (frame-samp-x frame) 8)
        (* mcu-height (frame-samp-y frame) 8)
        (for/vector ((k (in-naturals))
                     (component (in-vector (frame-components frame))))
          (let* ((samp-x (component-samp-x component))
                 (samp-y (component-samp-y component))
                 (block-width (* mcu-width samp-x))
                 (block-height (* mcu-height samp-y))
                 (sample-width (* block-width 8))
                 (sample-height (* block-height 8))
                 (p (make-bytevector (* sample-width sample-height) 0)))
            (for ((mcu-idx (in-naturals)) (mcu (in-array mcu-array)))
              (let* ((i (quotient mcu-idx mcu-width))
                     (j (remainder mcu-idx mcu-width))
                     (mcu-y (* i samp-y 8))
                     (mcu-x  (* j samp-x 8))
                     (offset (+ (* mcu-y sample-width) mcu-x)))
                (for ((block-idx (in-naturals))
                      (block (in-array (vector-ref mcu k))))
                  (let* ((block-i (quotient block-idx samp-x))
                         (block-j (remainder block-idx samp-y))
                         (offset (+ offset (* block-i 8 sample-width)
                                    (* block-j 8))))
                    (idct-block block p offset sample-width)))))
            (plane sample-width sample-height p))))))))

;; Tables K.1 and K.2 from the JPEG specification.
(define *standard-luminance-q-table*
  #(16 11 10 16 24 40 51 61
    12 12 14 19 26 58 60 55
    14 13 16 24 40 57 69 56
    14 17 22 29 51 87 80 62
    18 22 37 56 68 109 103 77
    24 35 55 64 81 104 113 92
    49 64 78 87 103 121 120 101
    72 92 95 98 112 100 103 99))

(define *standard-chrominance-q-table*
  #(17 18 24 47 99 99 99 99
    18 21 26 66 99 99 99 99
    24 26 56 99 99 99 99 99
    47 66 99 99 99 99 99 99
    99 99 99 99 99 99 99 99
    99 99 99 99 99 99 99 99
    99 99 99 99 99 99 99 99
    99 99 99 99 99 99 99 99))

;; As libjpeg does, we consider the above tables to be quality 50, on a
;; scale from 1 (terrible) to 100 (great).  We linearly scale the values
;; so that at quality 100, all values are 1, and at quality 1 all values
;; are 255.
(define (q-tables-for-quality quality)
  ;; This mapping of quality to a linear scale is also from libjpeg.
  (let* ((quality (exact->inexact quality)) ;; allow divide by zero -> inf
         (linear-scale (if (< quality 50)
                           (/ 50. quality)
                           (- 1 (/ (- quality 50) 50)))))
    (define (scale x)
      (let ((x (* x linear-scale)))
        (cond
         ((< x 1) 1)
         ((> x 255) 255)
         (else (inexact->exact (round x))))))
    (vector (array-map scale *standard-luminance-q-table*)
            (array-map scale *standard-chrominance-q-table*)
            #f
            #f)))

(define (fdct-block plane pos stride q-table)
  (define (fdct v u)
    (let ((coeffs (array-ref fdct-coefficients (vector v u))))
      (let lp ((i 0) (pos pos) (sum 0.0))
        (if (< i 8)
            (lp (add1 i)
                (+ pos stride)
                (let lp ((j 0) (k (* i 8)) (pos pos) (sum sum))
                  (if (< j 8)
                      (let ((coeff (vector-ref coeffs k))
                            (sample (- (bytevector-u8-ref plane pos) 128)))
                        (lp (add1 j)
                            (add1 k)
                            (add1 pos)
                            (+ sum (* coeff sample))))
                      sum)))
            sum))))
  (for/vector ((k (in-range (* 8 8))))
    (let ((v (arithmetic-shift k -3))
          (u (bitwise-and k 7))
          (q (vector-ref q-table k)))
      (let ((Svu (fdct v u)))
        (* q (inexact->exact (round (/ Svu q))))))))

(define (planar-image->jpeg yuv
                            #:quality (quality 85)
                            #:q-tables (q-tables (q-tables-for-quality quality))
                            ;; In JFIF baseline JPEG images, component
                            ;; 0 is Y', and components 1 and 2 are Cb
                            ;; and Cr.  Assign the first quantization
                            ;; table for luminance, and the second for
                            ;; chrominance.
                            #:plane-q-table (plane-q-table (lambda (i) (if (zero? i) 0 1))))
  (match yuv
    ((planar-image width height canvas-width canvas-height planes)
     (let ((samp-x (for/fold ((samp-x 1)) ((plane (in-vector planes)))
                     (lcm samp-x (/ canvas-width (plane-width plane)))))
           (samp-y (for/fold ((samp-x 1)) ((plane (in-vector planes)))
                     (lcm samp-x (/ canvas-height (plane-height plane))))))
       (define (plane-samp-x plane)
         (* samp-x (/ (plane-width plane) canvas-width)))
       (define (plane-samp-y plane)
         (* samp-y (/ (plane-height plane) canvas-height)))
       (let ((components
              (for/vector ((plane (in-vector planes))
                           (i (in-naturals)))
                (component i i
                           (plane-samp-x plane) (plane-samp-y plane)
                           (plane-q-table i)))))
         (jfif
          (frame #f 8 height width components samp-x samp-y)
          '()
          (build-array
           (vector (/ canvas-height 8 samp-y) (/ canvas-width 8 samp-x))
           (match-lambda
             ((vector i j)
              (for/vector ((component (in-vector components)))
                (match (vector-ref planes (component-index component))
                  ((plane plane-width plane-height samples)
                   (let ((samp-y (component-samp-y component))
                         (samp-x (component-samp-x component)))
                     (build-array
                      (vector samp-y samp-x)
                      (lambda (y x)
                        (let* ((pos (+ (* (+ (* i samp-y) y) 8 plane-width)
                                       (* (+ (* j samp-x) x) 8)))
                               (q-table-index (component-q-table component))
                               (q-table (vector-ref q-tables q-table-index)))
                          (fdct-block samples pos plane-width q-table)))))))))))))))))
