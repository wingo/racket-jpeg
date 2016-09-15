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
;; A parser for EXIF.
;;
;;; Code:

(provide parse-exif)
(require rnrs/bytevectors-6)

;; Exif version 2.3:
;; http://www.cipa.jp/std/documents/e/DC-008-2012_E.pdf

(define *exif-tag-names* (make-hasheqv))

(define-syntax-rule (define-exif-tags table (value name) ...)
  (begin
    (hash-set! table value 'name)
    ...))

;; EXIF v2.3, table 4.
(define-exif-tags *exif-tag-names*
  ;; Image structure.
  (#x100 image-width)
  (#x101 image-length)
  (#x102 bits-per-sample)
  (#x103 compression)
  (#x106 photometric-interpretation)
  (#x112 orientation)
  (#x115 samples-per-pixel)
  (#x11c planar-configuration)
  (#x212 y-cb-cr-sub-sampling)
  (#x213 y-cb-cr-positioning)
  (#x11a x-resolution)
  (#x11b y-resolution)
  (#x128 resolution-unit)
  ;; Offsets.
  (#x111 strip-offsets)
  (#x116 rows-per-strip)
  (#x117 strip-byte-counts)
  (#x201 jpeg-interchange-format)
  (#x202 jpeg-interchange-format-length)
  ;; Image data characteristics.
  (#x12d transfer-function)
  (#x13e white-point)
  (#x13f primary-chromaticities)
  (#x211 y-cb-cr-coefficients)
  (#x214 reference-black-white)
  ;; Other tags.
  (#x132 date-time)
  (#x10e image-description)
  (#x10f make)
  (#x110 model)
  (#x131 software)
  (#x13b artist)
  (#x8298 copyright))

;; EXIF v2.3, table 7.
(define-exif-tags *exif-tag-names*
  ;; Version
  (#x9000 exif-version)
  (#xa000 flashpix-version)
  ;; Image data characteristics.
  (#xa001 color-space)
  (#xa500 gamma)
  ;; Image configuration.
  (#x9101 components-configuration)
  (#x9102 compressed-bits-per-pixel)
  (#xa002 pixel-x-dimension)
  (#xa003 pixel-y-dimension)
  ;; User information.
  (#x927c maker-note)
  (#x9286 user-comment)
  ;; Related files.
  (#xa004 related-sound-file)
  ;; Date and time.
  (#x9003 date-time-original)
  (#x9004 date-time-digitized)
  (#x9290 sub-sec-time)
  (#x9291 sub-sec-time-original)
  (#x9292 sub-sec-time-digitized)
  ;; Other.
  (#xa420 image-unique-id)
  (#xa430 camera-owner-name)
  (#xa431 body-serial-number)
  (#xa432 lens-specification)
  (#xa433 lens-make)
  (#xa434 lens-model)
  (#xa435 lens-serial-number))

;; EXIF v2.3, table 8.
(define-exif-tags *exif-tag-names*
  ;; Picture-taking conditions.
  (#x829a exposure-time)
  (#x829d f-number)
  (#x8822 exposure-program)
  (#x8824 spectral-sensitivity)
  (#x8827 photographic-sensitivity)
  (#x8828 oecf)
  (#x8830 sensitivity-type)
  (#x8831 standard-output-sensitivity)
  (#x8832 recommended-exposureindex)
  (#x8833 iso-speed)
  (#x8834 iso-speed-latitude-yyy)
  (#x8835 iso-speed-latitude-zzz)
  (#x9201 shutter-speed-value)
  (#x9202 aperture-value)
  (#x9203 brightness-value)
  (#x9204 exposure-bias-value)
  (#x9205 max-aperture-value)
  (#x9206 subject-distance)
  (#x9207 metering-mode)
  (#x9208 light-source)
  (#x9209 flash)
  (#x920a focal-length)
  (#x9214 subject-area)
  (#xa20b flash-energy)
  (#xa20c spatial-frequency-response)
  (#xa20e focal-plane-x-resolution)
  (#xa20f focal-plane-y-resolution)
  (#xa210 focal-plane-resolution-unit)
  (#xa214 subject-location)
  (#xa215 exposure-index)
  (#xa217 sensing-method)
  (#xa300 file-source)
  (#xa301 scene-type)
  (#xa302 cfa-pattern)
  (#xa401 custom-rendered)
  (#xa402 exposure-mode)
  (#xa403 white-balance)
  (#xa404 digital-zoom-ratio)
  (#xa405 focal-length-in-35mm-film)
  (#xa406 scene-capture-type)
  (#xa407 gain-control)
  (#xa408 contrast)
  (#xa409 saturation)
  (#xa40a sharpness)
  (#xa40b device-settings-description)
  (#xa40c subject-distance-range))

(define *type-widths*
  #(#f ; 0 is unused.
    1 1 2 4 8 ; BYTE ASCII SHORT LONG RATIONAL
    1 1 2 4 8 ; SBYTE UNDEFINED SSHORT SLONG SRATIONAL
    4 8 ; FLOAT DOUBLE
    ))

(define *type-parsers*
  (vector
   #f                                                  ; 0 is unused.
   (lambda (bv pos order) (bytevector-u8-ref bv pos))  ; BYTE
   (lambda (bv pos order) (error "unreachable"))       ; ASCII
   (lambda (bv pos order) (bytevector-u16-ref bv pos order))  ; SHORT
   (lambda (bv pos order) (bytevector-u32-ref bv pos order))  ; LONG
   (lambda (bv pos order)
     (cons (bytevector-u32-ref bv pos order)
           (bytevector-u32-ref bv (+ pos 4) order)))   ; RATIONAL
   (lambda (bv pos order) (bytevector-s8-ref bv pos))  ; SBYTE
   (lambda (bv pos order) (error "unreachable"))       ; UNDEFINED
   (lambda (bv pos order) (bytevector-s16-ref bv pos order))  ; SSHORT
   (lambda (bv pos order) (bytevector-s32-ref bv pos order))  ; SLONG
   (lambda (bv pos order)
     (cons (bytevector-u32-ref bv pos order)
           (bytevector-u32-ref bv (+ pos 4) order))) ; SRATIONAL
   (lambda (bv pos order) (bytevector-ieee-single-ref bv pos order)) ; FLOAT
   (lambda (bv pos order) (bytevector-ieee-double-ref bv pos order)) ; DOUBLE
   ))

(define (type-width type)
  (and (< type (vector-length *type-widths*))
       (vector-ref *type-widths* type)))

(define (type-parser type)
  (and (< type (vector-length *type-parsers*))
       (vector-ref *type-parsers* type)))

(define (read-value bv pos order type count)
  (case type
    ((2) ; ASCII
     (if (> count 0)
         ;; Trim trailing NUL byte.
         (let ((res (make-bytevector (sub1 count))))
           (bytevector-copy! bv pos res 0 (sub1 count))
           (utf8->string res))
         ""))
    ((7) ; UNDEFINED
     (let ((res (make-bytevector count)))
       (bytevector-copy! bv pos res 0 count)
       res))
    (else
     (let ((parser (type-parser type)))
       (and parser
            (if (= count 1)
                (parser bv pos order)
                (let ((res (make-vector count))
                      (width (type-width type)))
                  (let lp ((n 0) (pos pos))
                    (if (< n count)
                        (begin
                          (vector-set! res n (parser bv pos order))
                          (lp (add1 n) (+ pos width)))
                        res)))))))))

(define *value-interpreters* (make-hasheq))

(define-syntax-rule (define-value-interpreter (name value) body ...)
  (hash-set! *value-interpreters* 'name
             (lambda (value) body ...)))

(define-value-interpreter (orientation value)
  (case value
    ((1) "Normal")
    ((2) "Mirrored")
    ((3) "Rotated 180 degrees")
    ((4) "Rotated 180 degrees then mirrored")
    ((5) "Rotated 90 degrees clockwise then mirrored")
    ((6) "Rotated 90 degrees clockwise")
    ((7) "Rotated 90 degrees counter-clockwise then mirrored")
    ((8) "Rotated 90 degrees counter-clockwise")))

(define-value-interpreter (photometric-interpretation value)
  (case value
    ((2) "RGB")
    ((6) "YCbCr")
    (else value)))

(define-value-interpreter (planar-configuratino value)
  (case value
    ((1) "Chunky")
    ((2) "Planar")
    (else value)))

(define-value-interpreter (y-cb-cr-positioning value)
  (case value
    ((1) "Centered")
    ((2) "Co-sited")
    (else value)))

(define-value-interpreter (resolution-unit value)
  (case value
    ((2) "Inches")
    ((3) "Centimeters")
    (else value)))

(define-value-interpreter (focal-plane-resolution-unit value)
  (case value
    ((2) "Inches")
    ((3) "Centimeters")
    (else value)))

(define-value-interpreter (compression value)
  (case value
    ((1) "Uncompressed")
    ((6) "JPEG")
    (else value)))

(define-value-interpreter (color-space value)
  (case value
    ((1) "sRGB")
    ((#xffff) "Uncalibrated")
    (else value)))

(define-value-interpreter (exposure-program value)
  (case value
    ((1) "Manual")
    ((2) "Normal")
    ((3) "Aperture priority")
    ((4) "Shutter priority")
    ((5) "Creative")
    ((6) "Action")
    ((7) "Portrait")
    ((8) "Landscape")
    (else value)))

(define-value-interpreter (sensitivity-type value)
  (case value
    ((1) "SOS")
    ((2) "REI")
    ((3) "ISO")
    ((4) "SOS+REI")
    ((5) "SOS+ISO")
    ((6) "REI+ISO")
    ((7) "SOS+REI+ISO")
    (else value)))

(define-value-interpreter (metering-mode value)
  (case value
    ((1) "Average")
    ((2) "Center-weighted average")
    ((3) "Spot")
    ((4) "Multi-spot")
    ((5) "Pattern")
    ((6) "Partial")
    (else value)))

(define-value-interpreter (light-source value)
  (case value
    ((1) "Daylight")
    ((2) "Flourescent")
    ((3) "Tungsten")
    ((4) "Flash")
    ((9) "Fine weather")
    ((10) "Cloudy weather")
    ((11) "Shade")
    ((12) "Daylight flourescent")
    ((13) "Day white flourescent")
    ((14) "Cool white flourescent")
    ((15) "White flourescent")
    ((16) "Warm white flourescent")
    ((17) "Standard light A")
    ((18) "Standard light B")
    ((19) "Standard light C")
    ((20) "D55")
    ((21) "D65")
    ((22) "D75")
    ((23) "D50")
    ((24) "ISO studio tungsten")
    (else value)))

(define-value-interpreter (flash value)
  (let ((fired? (bitwise-bit-set? value 0))
        (return (bitwise-and #b11 (arithmetic-shift value -1)))
        (mode (bitwise-and #b11 (arithmetic-shift value -3)))
        (present? (bitwise-bit-set? value 5))
        (red-eye? (bitwise-bit-set? value 6)))
    `((fired? . ,fired?)
      (return-light . ,(case return
                         ((0) "Not available")
                         ((1) "Unknown")
                         ((2) "Not detected")
                         ((3) "Detected")))
      (mode . ,(case mode
                 ((0) "Unknown")
                 ((1) "Compulsory firing")
                 ((2) "Compulsury suppression")
                 ((3) "Auto")))
      (present? . ,present?)
      (red-eye? . ,red-eye?))))

(define-value-interpreter (sensing-method value)
  (case value
    ((1) "Not defined")
    ((2) "One chip color area")
    ((3) "Two chip color area")
    ((4) "Three chip color area")
    ((5) "Color sequental area")
    ((6) "Trilinear")
    ((7) "Color sequental linear")
    (else value)))

(define-value-interpreter (file-source value)
  (case value
    ((1) "Transparent scanner")
    ((2) "Reflex scanner")
    ((3) "DSC")
    (else value)))

(define-value-interpreter (custom-rendered value)
  (case value
    ((1) "Normal")
    ((2) "Custom")
    (else value)))

(define-value-interpreter (exposure-mode value)
  (case value
    ((0) "Auto")
    ((1) "Manual")
    ((2) "Auto bracket")
    (else value)))

(define-value-interpreter (white-balance value)
  (case value
    ((0) "Auto")
    ((1) "Manual")
    (else value)))

(define-value-interpreter (screen-capture-type value)
  (case value
    ((0) "Standard")
    ((1) "Landscape")
    ((2) "Portrait")
    ((3) "Night")
    (else value)))

(define-value-interpreter (gain-control value)
  (case value
    ((0) "None")
    ((1) "Low gain up")
    ((2) "High gain up")
    ((3) "Low gain down")
    ((4) "High gain down")
    (else value)))

(define-value-interpreter (contrast value)
  (case value
    ((0) "Normal")
    ((1) "Soft")
    ((2) "Hard")
    (else value)))

(define-value-interpreter (saturation value)
  (case value
    ((0) "Normal")
    ((1) "Low")
    ((2) "High")
    (else value)))

(define-value-interpreter (sharpness value)
  (case value
    ((0) "Normal")
    ((1) "Soft")
    ((2) "Hard")
    (else value)))

(define-value-interpreter (subject-distance-range value)
  (case value
    ((1) "Macro")
    ((2) "Close")
    ((3) "Distant")
    (else value)))

(define (interpret-value name value)
  (let ((interpret (hash-ref *value-interpreters* name)))
    (if interpret
        (interpret value)
        value)))

(define (parse-ifd-chain bv pos order max-depth)
  (define (inline-value? type count)
    (let ((byte-width (and (< type (vector-length *type-widths*))
                           (vector-ref *type-widths* type))))
      (and byte-width (<= (* byte-width count) 4))))
  (define (parse-tags tag-count)
    (let lp ((n 0))
      (if (< n tag-count)
          (let* ((pos (+ pos 2 (* n 12)))
                 (tag (bytevector-u16-ref bv pos order))
                 (name (hash-ref *exif-tag-names* tag tag))
                 (type (bytevector-u16-ref bv (+ pos 2) order))
                 (count (bytevector-u32-ref bv (+ pos 4) order))
                 (offset (if (inline-value? type count)
                             (+ pos 8)
                             (bytevector-u32-ref bv (+ pos 8) order)))
                 (value (read-value bv offset order type count)))
            (if (and (eqv? tag #x8769) ;; Nested EXIF information.
                     (integer? value)
                     (positive? max-depth))
                (match (parse-ifd-chain bv value order (sub1 max-depth))
                  ((list alist)
                   (append alist (lp (add1 n)))))
                (let ((value* (interpret-value name value)))
                  (cons (cons name value*) (lp (add1 n))))))
          '())))
  (let* ((tag-count (bytevector-u16-ref bv pos order))
         (next-pos-offset (+ pos 2 (* tag-count 12)))
         (next-pos (bytevector-u32-ref bv next-pos-offset order)))
    (cons (parse-tags tag-count)
          (cond
           ((or (zero? next-pos) (not (positive? max-depth))) '())
           (else (parse-ifd-chain bv next-pos order (sub1 max-depth)))))))

(define (parse-exif bv)
  (define (parse-byte-order b0 b1)
    (unless (= b0 b1) (error "Bad TIFF header prefix"))
    (case (integer->char b0)
      ((#\I) (endianness little))
      ((#\M) (endianness big))
      (else (error "Bad TIFF header byte order"))))
  (let ((order (parse-byte-order (bytevector-u8-ref bv 0)
                                 (bytevector-u8-ref bv 1))))
    (unless (= 42 (bytevector-u16-ref bv 2 order))
      (error "Bad TIFF header magic value"))
    (let ((ifd0 (bytevector-u32-ref bv 4 order)))
      ;; Root IFD -> embedded EXIF -> one more
      (define *max-exif-depth* 3)
      (parse-ifd-chain bv ifd0 order *max-exif-depth*))))
