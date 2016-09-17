#lang racket
;; racket-jpeg
;; Copyright (C) 2016 Andy Wingo <wingo at pobox dot com>

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

(require rnrs/bytevectors-6
         racket/gui
         jpeg/pixbufs
         jpeg)
(provide rgb->bitmap
         jpeg->bitmap
         bitmap->rgb
         bitmap->jpeg)

(define (swap-u32-byte-order pixels)
  (let ((out (bytes-copy pixels)))
    (for ((i (in-range 0 (bytes-length pixels) 4)))
      (let ((u32 (bytevector-u32-ref pixels i (endianness big))))
        (bytevector-u32-set! out i u32 (endianness little))))
    out))

(define (maybe-swap-u32-byte-order pixels)
  (if (eq? (native-endianness) (endianness big))
      pixels
      (swap-u32-byte-order pixels)))

(define (rgb->bitmap image)
  (match image
    ((interleaved-image width height 4 stride pixels)
     (unless (= stride (* 4 width))
       (error "implement me"))
     (let ((bitmap (make-bitmap width height))
           (argb-pixels (maybe-swap-u32-byte-order pixels)))
       (send bitmap set-argb-pixels 0 0 width height argb-pixels)
       bitmap))
    ((interleaved-image width height 3 stride pixels)
     (rgb->bitmap (rgb->argb image)))))

(define (bitmap->rgb bitmap)
  (let* ((width (send bitmap get-width))
         (height (send bitmap get-height))
         (argb-pixels (make-bytes (* width height 4))))
    (send bitmap get-argb-pixels 0 0 width height argb-pixels)
    (interleaved-image width height 4 (* width 4) argb-pixels)))

(define (jpeg->bitmap jpeg)
  (rgb->bitmap (jpeg->rgb jpeg #:argb? #f)))

(define (bitmap->jpeg bitmap #:quality (quality 50))
  (rgb->jpeg (bitmap->rgb bitmap) #:quality quality))

(module+ test
  (require rackunit)
  (let-values (((width height) (jpeg-dimensions "./test.jpg")))
    (let ((bitmap (jpeg->bitmap "./test.jpg")))
      (check-eqv? (send bitmap get-width) width)
      (check-eqv? (send bitmap get-height) height))))
