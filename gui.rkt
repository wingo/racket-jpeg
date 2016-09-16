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
         jpeg->bitmap)

(define (rgb->bitmap image)
  (match image
    ((interleaved-image width height 4 stride pixels)
     (unless (= stride (* 4 width))
       (error "implement me"))
     (let ((bitmap (make-bitmap width height)))
       (send bitmap set-argb-pixels 0 0 width height pixels)
       bitmap))
    ((interleaved-image width height 3 stride pixels)
     (rgb->bitmap (rgb->argb image)))))

(define (jpeg->bitmap jpeg)
  (rgb->bitmap (jpeg->rgb jpeg #:argb? #t)))

(module+ test
  (let* ((j1 (read-jpeg "./test.jpg"))
         (rgb (jpeg->rgb j1 #:argb? #t)))
    (rgb->bitmap rgb)
    #t))
