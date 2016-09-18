#lang racket
(require jpeg)
(define jpeg (read-jpeg "/home/wingo/src/racket-jpeg/test.jpg"))

(jpeg-dimensions-and-exif jpeg)




























;(require jpeg/gui)
;(define pic (jpeg->bitmap jpeg))
;pic




























;(define rgb (bitmap->rgb pic))
;(define re-encoded (rgb->jpeg rgb))
;(define pic2 (jpeg->bitmap re-encoded))
;pic2





















;(jpeg->bitmap (bitmap->jpeg pic #:quality 5))
























#;
(require jpeg/dct jpeg/jfif jpeg/pixbufs rnrs/bytevectors-6)
#;(define yuv (jpeg->planar-image jpeg))
#;(define (extract-plane yuv k)
  (vector-ref (planar-image-planes yuv) k))
#;(define (plane->rgb p #:zero (zero #(0 0 0)) #:one (one #(255 255 255)))
  (match (vector zero one)
    ((vector (vector r0 g0 b0) (vector r1 b1 g1))
     (define (scale u8 v0 v1)
       (inexact->exact (round (+ v0 (* (- v1 v0) (/ u8 255))))))
     (define (color u8)
       (let ((a #xff)
             (r (scale u8 r0 r1))
             (g (scale u8 g0 g1))
             (b (scale u8 b0 b1)))
         (bitwise-ior (arithmetic-shift a 24)
                      (arithmetic-shift r 16)
                      (arithmetic-shift g 8)
                      (arithmetic-shift b 0))))
     (match p
       ((plane width height samples)
        (let ((pixels (make-bytes (* width height 4))))
          (for ((i (in-range (bytes-length samples)))
                (offset (in-range 0 (bytes-length pixels) 4)))
            (bytevector-u32-native-set! pixels offset
                                        (color (bytevector-u8-ref samples i))))
          (interleaved-image width height 4 (* width 4) pixels)))))))
#;(define y (plane->rgb (extract-plane yuv 0)))
#;(define u (plane->rgb (extract-plane yuv 1)))
#;(define v (plane->rgb (extract-plane yuv 2)))
;(rgb->bitmap y)
;(rgb->bitmap u)
;(rgb->bitmap v)
