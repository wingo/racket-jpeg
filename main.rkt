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

(provide jpeg-dimensions
         jpeg-dimensions-and-exif
         jpeg->rgb
         rgb->jpeg
         rgb-buffer->jpeg
         read-jpeg
         write-jpeg)
(require jpeg/jfif jpeg/exif jpeg/dct jpeg/pixbufs)

(define (jpeg-dimensions jpeg)
  (let* ((jfif (if (jfif? jpeg)
                   jpeg
                   (read-jfif jpeg #:with-body? #f #:with-misc-sections? #f)))
         (frame (jfif-frame jfif)))
    (values (frame-x frame)
            (frame-y frame))))

(define (read-jpeg jpeg)
  (read-jfif jpeg))

(define (write-jpeg port jpeg)
  (write-jfif port jpeg))

(define (find-exif misc-segments)
  (define (bv-prefix? prefix bv)
    (and (>= (bytes-length bv) (bytes-length prefix))
         (let lp ((n 0))
           (or (= n (bytes-length prefix))
               (and (eqv? (bytes-ref prefix n) (bytes-ref bv n))
                    (lp (add1 n)))))))
  (filter-map (lambda (misc)
                (and (= (misc-marker misc) #xffe1) ; APP1
                     (bv-prefix? (bytes 69 120 105 102 0 0) (misc-bytes misc))
                     (parse-exif (subbytes (misc-bytes misc) 6))))
              misc-segments))

(define (jpeg-dimensions-and-exif jpeg)
  (let* ((jfif (if (jfif? jpeg)
                   jpeg
                   (read-jfif jpeg #:with-body? #f)))
         (frame (jfif-frame jfif)))
    (values (frame-x frame)
            (frame-y frame)
            (match (find-exif (jfif-misc-segments jfif))
              ((list (list main thumbnail)) main)
              ((list (list main)) main)
              (_ '())))))

(define (jpeg->rgb in
                   #:argb? (argb? #f)
                   #:stride-for-width (stride-for-width
                                       (lambda (width)
                                         (* width (if argb? 4 3)))))
  (let ((jfif (if (jfif? in) in (read-jfif in))))
    (yuv->rgb (jpeg->planar-image jfif)
              #:argb? argb?
              #:stride (stride-for-width (frame-x (jfif-frame jfif))))))

(define (rgb->jpeg rgb #:samp-x (samp-x 2) #:samp-y (samp-y 2)
                   #:quality (quality 85))
  (planar-image->jpeg (rgb->yuv rgb #:samp-x samp-x #:samp-y samp-y)
                      #:quality quality))

(define (rgb-buffer->jpeg buffer width height #:stride (stride (* width 3))
                          #:samp-x (samp-x 2) #:samp-y (samp-y 2)
                          #:quality (quality 85) #:argb? (argb? #f))
  (rgb->jpeg (interleaved-image width height (if argb? 4 3) stride
                                buffer)
             #:samp-x samp-x #:samp-y samp-y #:quality quality))

(module+ test
  (require rackunit)
  (define test-file-name "./test.jpg")
  (define expected-width 500)
  (define expected-height 375)
  (define expected-exif
    `((make . "CAMERA                   ")
      (model . "DC2302                 ")
      (x-resolution 72 . 1)
      (y-resolution 72 . 1)
      (resolution-unit . "Inches")
      (software . "f-spot version 0.1.11")
      (date-time . "2006:05:14 20:55:54")
      (y-cb-cr-positioning . "Co-sited")
      (exposure-time 1 . 198)
      (f-number 971 . 100)
      (photographic-sensitivity . 50)
      (exif-version . ,(bytes 48 50 49 48))
      (date-time-original . "2004:10:31 03:03:17")
      (date-time-digitized . "2004:10:30 12:03:17")
      (components-configuration . ,(bytes 1 2 3 0))
      (shutter-speed-value 77 . 10)
      (aperture-value 5 . 1)
      (flash (fired? . #f)
             (return-light . "Not available")
             (mode . "Unknown")
             (present? . #f)
             (red-eye? . #f))
      (user-comment . ,(bytes 65 83 67 73 73 0 0 0))
      (flashpix-version . ,(bytes 48 49 48 48))
      (color-space . "sRGB")
      (pixel-x-dimension . 1600)
      (pixel-y-dimension . 1200)
      ;; This is an interoperability offset but because it is before the
      ;; EXIF segment we don't visit it; otherwise we would be exposed to
      ;; loop-like attacks.
      (40965 . 508)))

  (define-values (width height exif)
    (jpeg-dimensions-and-exif test-file-name))

  (check-eqv? width expected-width)
  (check-eqv? height expected-height)
  (check-equal? exif expected-exif)
  (let* ((j1 (read-jpeg test-file-name))
         (j2 (call-with-input-bytes
                 (call-with-output-bytes
                   (lambda (port) (write-jpeg port j1)))
               (lambda (port)
                 (read-jpeg port)))))
    (check-equal? j1 j2)
    (jpeg->rgb j1)
    #t))
