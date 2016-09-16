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
;; A parser for JPEG.
;;
;;; Code:

(require rnrs/bytevectors-6)
;; FIXME: These constructors should verify that the
;; parameters are valid (e.g. that plane width and height
;; agree with the size of the samples array)
(provide planar-image planar-image?
         planar-image-width planar-image-height
         planar-image-canvas-width planar-image-canvas-height
         planar-image-planes

         plane plane?
         plane-width plane-height plane-samples

         interleaved-image interleaved-image?
         interleaved-image-width interleaved-image-height
         interleaved-image-component-count interleaved-image-stride
         interleaved-image-buffer

         yuv->rgb rgb->yuv

         write-ppm write-pgm)

(struct planar-image
  (width height canvas-width canvas-height planes)
  #:transparent)

(struct plane
  (width height samples)
  #:transparent)

(struct interleaved-image
  (width height component-count stride buffer)
  #:transparent)

(define (shrink-plane-width-by-two/centered in width height)
  (let* ((half-width (/ width 2))
         (out (make-bytevector (* half-width height) 0)))
    (let lp ((i 0))
      (when (< i height)
        (let ((in-pos (* i width))
              (out-pos (* i half-width)))
          (let lp ((j 0))
            (when (< j half-width)
              (let* ((in- (bytevector-u8-ref in (+ in-pos (* j 2))))
                     (in+ (bytevector-u8-ref in (+ in-pos (* j 2) 1)))
                     ;; Dither rounding alternately by column.
                     (out* (arithmetic-shift (+ in- in+ (bitwise-and j 1))
                                             -1)))
                (bytevector-u8-set! out (+ out-pos j) out*)
                (lp (add1 j))))))
        (lp (add1 i))))
    out))

(define (shrink-plane-height-by-two/centered in width height)
  (let* ((half-height (/ height 2))
         (out (make-bytevector (* width half-height) 0)))
    (let lp ((i 0))
      (when (< i half-height)
        (let ((in-pos (* i 2 width))
              (out-pos (* i width)))
          (let lp ((j 0))
            (when (< j width)
              (let* ((in- (bytevector-u8-ref in (+ in-pos j)))
                     (in+ (bytevector-u8-ref in (+ in-pos j width)))
                     ;; Dither rounding alternately by column.
                     (out* (arithmetic-shift (+ in- in+ (bitwise-and j 1))
                                             -1)))
                (bytevector-u8-set! out (+ out-pos j) out*)
                (lp (add1 j))))))
        (lp (add1 i))))
    out))

(define (pad-interleaved-horizontally in width height stride new-width ncomps)
  (let* ((new-stride (* new-width ncomps))
         (out (make-bytevector (* new-stride height) 0)))
    (let lp ((i 0))
      (when (< i height)
        (let ((in-pos (* i stride))
              (out-pos (* i new-stride)))
          (bytevector-copy! in in-pos out out-pos (* width ncomps))
          (let lp ((j (* width ncomps)))
            (when (< j new-stride)
              (let ((x (bytevector-u8-ref out (+ out-pos j (- ncomps)))))
                (bytevector-u8-set! out (+ out-pos j) x)
                (lp (add1 j))))))
        (lp (add1 i))))
    out))

(define (pad-interleaved-vertically in width height stride new-height ncomps)
  (let* ((new-stride (* width ncomps))
         (out (make-bytevector (* new-stride new-height) 0)))
    (let lp ((i 0))
      (when (< i height)
        (let ((in-pos (* i stride))
              (out-pos (* i new-stride)))
          (bytevector-copy! in in-pos out out-pos (* width ncomps))
          (lp (add1 i)))))
    (let lp ((i height))
      (when (< i new-height)
        (let ((prev-pos (* (sub1 i) new-stride))
              (out-pos (* i new-stride)))
          (bytevector-copy! out prev-pos out out-pos new-stride)
          (lp (add1 i)))))
    out))

(define (expand-plane-width-by-two/centered in width height)
  (let* ((out (make-bytevector (* width 2 height) 0)))
    (let lp ((i 0))
      (when (< i height)
        (let ((in-pos (* i width))
              (out-pos (* i width 2)))
          ;; Special case for first column.
          (let* ((j 0)
                 (in (bytevector-u8-ref in (+ in-pos j))))
            (bytevector-u8-set! out (+ out-pos 0) in))
          (let lp ((j 0))
            (when (< j (sub1 width))
              ;; (3x + y + 2) >> 2 is the same as 3x/4 + y/4.  Since
              ;; we're dealing with integers though, we don't want to
              ;; introduce bias by having all 0.5 values round to 1, so
              ;; we add 1 or 2 to the value being shifted, alternating
              ;; by row.
              (let* ((in- (bytevector-u8-ref in (+ in-pos j)))
                     (in+ (bytevector-u8-ref in (+ in-pos (add1 j))))
                     (out- (arithmetic-shift (+ (* 3 in-) in+ 2) -2))
                     (out+ (arithmetic-shift (+ in- (* 3 in+) 1) -2)))
                (bytevector-u8-set! out (+ out-pos j j 1) out-)
                (bytevector-u8-set! out (+ out-pos j j 2) out+)
                (lp (+ j 1)))))
          ;; Special case for last column.
          (let* ((j (sub1 width))
                 (in (bytevector-u8-ref in (+ in-pos j))))
            (bytevector-u8-set! out (+ out-pos width width -1) in)))
        (lp (add1 i))))
    out))

(define (expand-plane-height-by-two/centered in width height)
  (let* ((out (make-bytevector (* width 2 height) 0)))
    ;; Special case for first row.
    (let lp ((j 0))
      (when (< j width)
        (let ((in (bytevector-u8-ref in j)))
          (bytevector-u8-set! out j in)
          (lp (add1 j)))))
    ;; The height-1 spaces between samples.
    (let lp ((i 0))
      (when (< i (sub1 height))
        (let ((in-pos (* i width))
              (out-pos (+ width (* i 2 width))))
          (let lp ((j 0))
            (when (< j width)
              (let* ((in- (bytevector-u8-ref in (+ in-pos j)))
                     (in+ (bytevector-u8-ref in (+ in-pos width j)))
                     ;; Interpolate output; see comment in previous
                     ;; function.
                     (out- (arithmetic-shift (+ (* 3 in-) in+ 2) -2))
                     (out+ (arithmetic-shift (+ in- (* 3 in+) 1) -2)))
                (bytevector-u8-set! out (+ out-pos j) out-)
                (bytevector-u8-set! out (+ out-pos width j) out+)
                (lp (add1 j)))))
          (lp (add1 i)))))
    ;; Special case for the last row.
    (let* ((i (sub1 height))
           (in-pos (* i width))
           (out-pos (+ width (* i 2 width))))
      (let lp ((j 0))
        (when (< j width)
          (let ((in (bytevector-u8-ref in (+ in-pos j))))
            (bytevector-u8-set! out (+ out-pos j) in)
            (lp (add1 j))))))
    out))

(define (upsample-4:2:2 width height y-width y-height y cb cr)
  (define (expand in)
    (expand-plane-width-by-two/centered in (/ y-width 2) y-height))
  (planar-image
   width height y-width y-height
   (vector (plane y-width y-height y)
           (plane y-width y-height (expand cb))
           (plane y-width y-height (expand cr)))))

(define (upsample-4:2:0 width height y-width y-height y cb cr)
  (define (expand in)
    (expand-plane-height-by-two/centered in (/ y-width 2) (/ y-height 2)))
  (upsample-4:2:2 width height y-width y-height y (expand cb) (expand cr)))

(define (convert-yuv out width height stride y cb cr y-stride)
  (let lp ((i 0))
    (when (< i height)
      (let lp ((j 0) (in-pos (* i y-stride)) (out-pos (* i stride)))
        (when (< j width)
          (let ((y (bytevector-u8-ref y in-pos))
                (cb (- (bytevector-u8-ref cb in-pos) 128))
                (cr (- (bytevector-u8-ref cr in-pos) 128)))
            (define (->u8 x)
              (cond ((< x 0) 0)
                    ((> x 255) 255)
                    (else (inexact->exact (round x)))))
            ;; See ITU recommendataion ITU-T T.871, "JPEG File
            ;; Interchange Format (JFIF)", section 7.
            (let ((r (->u8 (+ y (* 1.402 cr))))
                  (g (->u8 (- y (/ (+ (* 0.114 1.772 cb)
                                      (* 0.299 1.402 cr))
                                   0.587))))
                  (b (->u8 (+ y (* 1.772 cb)))))
              (bytevector-u8-set! out (+ out-pos 0) r)
              (bytevector-u8-set! out (+ out-pos 1) g)
              (bytevector-u8-set! out (+ out-pos 2) b)
              (lp (add1 j) (add1 in-pos) (+ out-pos 3))))))
      (lp (add1 i)))))

;; in and out might be the same
(define (rgb->argb in out width height in-stride out-stride)
  (let lp ((i 0))
    (when (< i height)
      (let ((in-pos (* i in-stride))
            (out-pos (* i out-stride)))
        (let lp ((j (sub1 width)))
          (when (<= 0 j)
            (let ((in-pos (+ in-pos (* 3 j)))
                  (out-pos (+ out-pos (* 4 j))))
              (let ((a #xff)
                    (r (bytevector-u8-ref in (+ in-pos 0)))
                    (g (bytevector-u8-ref in (+ in-pos 1)))
                    (b (bytevector-u8-ref in (+ in-pos 2))))
                (let ((argb (bitwise-ior (arithmetic-shift a 24)
                                         (arithmetic-shift r 16)
                                         (arithmetic-shift g 8)
                                         (arithmetic-shift b 0))))
                  (bytevector-u32-native-set! out out-pos argb))
                (lp (sub1 j)))))))
      (lp (add1 i)))))

(define (argb->rgb in out width height in-stride out-stride)
  (let lp ((i 0))
    (when (< i height)
      (let ((in-pos (* i in-stride))
            (out-pos (* i out-stride)))
        (let lp ((j 0))
          (when (< j width)
            (let ((in-pos (+ in-pos (* 4 j)))
                  (out-pos (+ out-pos (* 3 j))))
              (let ((r (bytevector-u8-ref in (+ in-pos 1)))
                    (g (bytevector-u8-ref in (+ in-pos 2)))
                    (b (bytevector-u8-ref in (+ in-pos 3))))
                (bytevector-u8-set! out (+ out-pos 0) r)
                (bytevector-u8-set! out (+ out-pos 1) g)
                (bytevector-u8-set! out (+ out-pos 2) b)
                (lp (add1 j)))))))
      (lp (add1 i)))))

(define (yuv->rgb yuv
                   #:argb? (argb? #f)
                   #:stride (stride (* (planar-image-width yuv) (if argb? 4 3))))
  (match yuv
    ((planar-image width height canvas-width canvas-height planes)
     (match planes
       ((vector (plane y-width y-height y))
        (error "greyscale unimplemented"))
       ((vector (plane y-width y-height y)
                (plane cb-width cb-height cb)
                (plane cr-width cr-height cr))
        (unless (and (= y-width canvas-width) (= y-height canvas-height))
          (error "Expected Y' to have same dimensions as canvas"))
        (match (vector (/ y-width cb-width) (/ y-height cb-height)
                       (/ y-width cr-width) (/ y-height cr-height))
          ((vector 2 2 2 2)                   ; 4:2:0
           (yuv->rgb (upsample-4:2:0 width height y-width y-height y cb cr)
                     #:argb? argb? #:stride stride))
          ((vector 2 1 2 1)                   ; 4:2:2
           (yuv->rgb (upsample-4:2:2 width height y-width y-height y cb cr)
                     #:argb? argb? #:stride stride))
          ((vector 1 1 1 1)                   ; 4:4:4
           (unless (<= (* width (if argb? 4 3)) stride)
             (error "invalid stride" stride))
           (let ((buffer (make-bytevector (* stride height) 0)))
             (convert-yuv buffer width height stride y cb cr y-width)
             (when argb?
               (rgb->argb buffer buffer width height stride stride))
             (interleaved-image width height
                                (if argb? 4 3) stride buffer)))
          ((vector x y z w)                   ; ?
           (error "subsampling unimplemented" x y z w))))
       (_ (error "unknown colorspace"))))))

(define (convert-rgb rgb width height stride)
  (let ((y (make-bytevector (* width height)))
        (cb (make-bytevector (* width height)))
        (cr (make-bytevector (* width height))))
    (let lp ((i 0))
      (when (< i height)
        (let lp ((j 0) (in-pos (* i stride)) (out-pos (* i width)))
          (when (< j width)
            (let ((r (bytevector-u8-ref rgb (+ in-pos 0)))
                  (g (bytevector-u8-ref rgb (+ in-pos 1)))
                  (b (bytevector-u8-ref rgb (+ in-pos 2))))
              (define (->u8 x)
                (cond ((< x 0) 0)
                      ((> x 255) 255)
                      (else (inexact->exact (round x)))))
              ;; See ITU recommendataion ITU-T T.871, "JPEG File
              ;; Interchange Format (JFIF)", section 7.
              (let ((y* (->u8 (+ (* 0.299 r) (* 0.587 g) (* 0.114 b))))
                    (cb* (->u8 (+ (/ (+ (* -0.299 r) (* -0.587 g) (* 0.886 b))
                                     1.772)
                                  128)))
                    (cr* (->u8 (+ (/ (+ (* 0.701 r) (* -0.587 g) (* -0.114 b))
                                     1.402)
                                  128))))
                (bytevector-u8-set! y out-pos y*)
                (bytevector-u8-set! cb out-pos cb*)
                (bytevector-u8-set! cr out-pos cr*)
                (lp (add1 j) (+ in-pos 3) (add1 out-pos))))))
        (lp (add1 i))))
    (values y cb cr)))

(define (ceiling/ a b)
  (inexact->exact (ceiling (/ a b))))

(define (rgb->yuv rgb #:samp-x (samp-x 2) #:samp-y (samp-y 2))
  (define (round-up x y) (* (ceiling/ x y) y))
  (match rgb
    ((interleaved-image width height 4 stride argb)
     (let* ((new-stride (* width 3))
            (rgb (make-bytevector (* height new-stride) 0)))
       (rgb->yuv
        (interleaved-image
         width height 3 new-stride
         (argb->rgb argb rgb width height stride new-stride))
        #:samp-x samp-x #:samp-y samp-y)))
    ((interleaved-image width height 3 stride rgb)
     (let pad ((rgb rgb)
               (canvas-width width)
               (canvas-height height)
               (stride stride))
       (cond
        ((not (integer? (/ canvas-width 8 samp-x)))
         (let ((new-canvas-width (round-up canvas-width (* 8 samp-x))))
           (pad (pad-interleaved-horizontally rgb canvas-width canvas-height
                                              stride new-canvas-width 3)
                new-canvas-width canvas-height (* new-canvas-width 3))))
        ((not (integer? (/ canvas-height 8 samp-y)))
         (let ((new-canvas-height (round-up canvas-height (* 8 samp-y))))
           (pad (pad-interleaved-vertically rgb canvas-width canvas-height
                                            stride new-canvas-height 3)
                canvas-width new-canvas-height (* canvas-width 3))))
        (else
         (call-with-values (lambda ()
                             (convert-rgb rgb canvas-width canvas-height stride))
           (lambda (y cb cr)
             (let lp ((cb cb) (cr cr)
                      (samp-w canvas-width) (samp-h canvas-height))
               (cond
                ((< canvas-width (* samp-w samp-x))
                 (lp (shrink-plane-width-by-two/centered cb samp-w samp-h)
                     (shrink-plane-width-by-two/centered cr samp-w samp-h)
                     (/ samp-w 2)
                     samp-h))
                ((< canvas-height (* samp-h samp-y))
                 (lp (shrink-plane-height-by-two/centered cb samp-w samp-h)
                     (shrink-plane-height-by-two/centered cr samp-w samp-h)
                     samp-w
                     (/ samp-h 2)))
                (else
                 (planar-image
                  width height canvas-width canvas-height
                  (vector (plane canvas-width canvas-height y)
                          (plane samp-w samp-h cb)
                          (plane samp-w samp-h cr))))))))))))))

(define (write-ppm port rgb)
  (match rgb
    ((interleaved-image width height 3 stride buffer)
     (unless (= stride (* 3 width))
       (error "implement me"))
     (fprintf port "P6\n~a ~a\n255\n" width height)
     (write-bytes buffer port))))

(define (write-pgm port p)
  (match p
    ((plane width height samples)
     (fprintf port "P5\n~a ~a\n255\n" width height)
     (write-bytes samples port))))
