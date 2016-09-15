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
;; Readers and writers for the JPEG File Interchange Format (JFIF)
;;
;;; Code:

;; (require jpeg/pixbufs)
(require math/array jpeg/bit-ports jpeg/huffman)

(provide jfif jfif? jfif-frame jfif-misc-segments jfif-mcu-array

         frame frame?
         frame-marker frame-precision frame-y frame-x frame-components
         frame-samp-x frame-samp-y

         frame-baseline? frame-sequential? frame-progressive?
         frame-huffman-coded? frame-arithmetic-coded? frame-lossless?
         frame-dct?

         frame-component-count frame-mcu-width frame-mcu-height

         component component?
         component-id component-index component-samp-x component-samp-y
         component-q-table

         misc misc? misc-marker misc-bytes

         read-jfif
         ;write-jfif
         )


;; See http://www.w3.org/Graphics/JPEG/itu-t81.pdf for ITU
;; recommendation T.81, which is a freely-available version of the JPEG
;; specification.

;; JPEG := SOI FRAME EOI
;; FRAME := MISC* FHEADER SCAN DNL? SCAN ... 
;; SCAN := MISC* SHEADER ECS (RST ECS)*
;; FHEADER := SOF LEN PRECISION Y X COMP0 COMP1 ...
;; MISC := (DQT | DHT | DAC | DRI | COM | APP) LEN payload...
;; SHEADER := SOS LEN NCOMPONENTS SCOMP0 SCOMP1 ... SS SE A

(struct jfif
  (frame misc-segments mcu-array))

(struct frame
  (marker precision y x components samp-x samp-y) #:transparent)

(struct component
  (id index samp-x samp-y q-table) #:transparent)

(struct misc
  (marker bytes))

(struct params
  (q-tables dc-tables ac-tables restart-interval misc-segments) #:transparent)

(module+ test
  (require rackunit)
  #t)



(define (read-marker port)
  (let ((u8 (read-byte port)))
    (unless (eqv? u8 #xff)
      (error "Unexpected byte while reading marker" u8)))
  (let lp ()
    (let ((u8 (read-byte port)))
      (when (eof-object? u8)
        (error "End of file while reading marker"))
      (case u8
        ((#xff) (lp))
        ((0) (error "Expected a marker, got #xFF00"))
        (else (bitwise-ior #xff00 u8))))))

(define (assert-marker port expected-marker)
  (let ((marker (read-marker port)))
    (unless (eqv? expected-marker marker)
      (error "Unexpected marker" marker expected-marker))))

(define (read-u8 port)
  (let* ((u8 (read-byte port)))
    (when (eof-object? u8)
      (error "EOF while reading byte from port"))
    u8))

(define (read-u16 port)
  (let* ((msb (read-byte port))
         (lsb (read-byte port)))
    (when (eof-object? lsb)
      (error "EOF while reading two-byte value"))
    (bitwise-ior (arithmetic-shift msb 8) lsb)))

(define (read-bytes/exactly n port)
  (let ((bytes (read-bytes n port)))
    (unless (= (bytes-length bytes) n)
      (error "EOF while reading bytes" n))
    bytes))

(define (read-soi port)
  (assert-marker port #xffd8))

(define-syntax eval-at-compile-time
  (lambda (x)
    (syntax-case x ()
      ((eval-at-compile-time expr)
       (datum->syntax #'eval-at-compile-time
                      (eval (syntax->datum #'expr)))))))

(define normal-order
  (eval-at-compile-time
   (let* ((width 8)
          (height 8)
          ;; The padding is to allow the 4-bit offsets in the AC
          ;; coefficient decode loop to increment "k" beyond 63.
          ;; Strictly speaking, at that point we should signal an error,
          ;; but perhaps it's better to keep on trucking.  This trick
          ;; was taken from libjpeg.
          (padding 16)
          (len (* width height))
          (res (make-bytes (+ len padding) (sub1 len))))
     (let lp ((x 0) (y 0) (x-inc 1) (y-inc -1) (pos 0))
       (when (< pos len)
         (cond
          ((< x 0) (lp 0 y (- x-inc) (- y-inc) pos))
          ((< y 0) (lp x 0 (- x-inc) (- y-inc) pos))
          ((and (< x width) (< y height))
           (bytes-set! res pos (+ (* y height) x))
           (lp (+ x x-inc) (+ y y-inc) x-inc y-inc (add1 pos)))
          (else
           (lp (+ x x-inc) (+ y y-inc) x-inc y-inc pos)))))
     res)))

(define (read-q-table port len q-tables)
  (unless (> len 2)
    (error "Invalid DQT segment length" len))
  (let lp ((remaining (- len 2)))
    (unless (zero? remaining)
      (unless (>= remaining 1)
        (error "Invalid DQT segment length" len))
      (let* ((PT (read-u8 port))
             (Pq (arithmetic-shift PT -4))
             (Tq (bitwise-and PT #xf))
             (table (make-vector 64 #f))
             (remaining (- remaining (+ 1 (* 64 (add1 Pq))))))
        (define (zigzag->normal idx) (bytes-ref normal-order idx))
        (unless (< Tq 4)
          (error "Bad Tq value" Tq))
        (when (negative? remaining)
          (error "Invalid DQT segment length" len))
        (case Pq
          ((0)
           (let lp ((n 0))
             (when (< n 64)
               (vector-set! table (zigzag->normal n) (read-u8 port))
               (lp (add1 n)))))
          ((1)
           (let lp ((n 0))
             (when (< n 64)
               (vector-set! table (zigzag->normal n) (read-u16 port))
               (lp (add1 n)))))
          (else
           (error "Bad Pq value" Pq)))
        (vector-set! q-tables Tq table)
        (lp remaining)))))

(define (read-huffman-table port len dc-tables ac-tables)
  (unless (> len 2)
    (error "Invalid DHT segment length" len))
  (let lp ((remaining (- len 2)))
    (unless (zero? remaining)
      (unless (>= remaining 17)
        (error "Invalid DHT segment length" len))
      (let* ((T (read-u8 port))
             (Tc (arithmetic-shift T -4))
             (Th (bitwise-and T #xf))
             (size-counts (read-bytes/exactly 16 port))
             (count (for/fold ((sum 0)) ((count size-counts))
                      (+ sum count)))
             (remaining (- remaining (+ 17 count))))
        (unless (< Th 4)
          (error "Bad Th value" Th))
        (when (negative? remaining)
          (error "Invalid DHT segment length" len))
        (let* ((values (read-bytes/exactly count port))
               (table (make-huffman-table size-counts values)))
          (match Tc
            (0 (vector-set! dc-tables Th table))
            (1 (vector-set! ac-tables Th table))
            (_ (error "Bad Tc value" Tc))))
        (lp remaining)))))

(define *initial-params*
  (params (make-vector 4 #f)
          (make-vector 4 #f)
          (make-vector 4 #f)
          0
          '()))

(define (read-params port previous-params with-misc-sections?)
  (let* ((q-tables (vector-copy (params-q-tables previous-params)))
         (dc-tables (vector-copy (params-dc-tables previous-params)))
         (ac-tables (vector-copy (params-ac-tables previous-params)))
         (restart-interval (params-restart-interval previous-params))
         (misc-segments '())) ;; No sense inheriting this.
    (let lp ()
      (let ((marker (read-marker port)))
        (case marker
          ((#xffdb)                     ; DQT
           (let* ((len (read-u16 port)))
             (read-q-table port len q-tables)
             (lp)))
          ((#xffc4)                     ; DHT
           (let* ((len (read-u16 port)))
             (read-huffman-table port len dc-tables ac-tables)
             (lp)))
          ((#xffcc)                     ; DAC
           (error "Arithmetic coding currently unsupported."))
          ((#xffdd)                     ; DRI
           (let ((len (read-u16 port)))
             (unless (= len 4)
               (error "Unexpected DRI len" len))
             (set! restart-interval (read-u16 port))
             (lp)))
          ((#xfffe                      ; COM
            #xffe0 #xffe1 #xffe2 #xffe3 #xffe4 #xffe5 #xffe6 #xffe7 ; APP0-APP7
            #xffe8 #xffe9 #xffea #xffeb #xffec #xffed #xffee #xffef) ; APP8-APP15
           (let* ((len (read-u16 port))
                  (payload-len (- len 2)))
             (unless (>= payload-len 0)
               (error "Invalid comment/app segment length" marker len))
             (let ((misc (misc marker (read-bytes/exactly payload-len port))))
               (set! misc-segments (cons misc misc-segments))
               (lp))))
          (else
           (values (params q-tables dc-tables ac-tables restart-interval
                           (reverse misc-segments))
                   marker)))))))

(define (skip-params port)
  (let ((marker (read-marker port)))
    (case marker
      ((#xffdb ; DQT
        #xffc4 ; DHT
        #xffcc ; DAC
        #xffdd ; DRI
        #xfffe ; COM
        #xffe0 #xffe1 #xffe2 #xffe3 #xffe4 #xffe5 #xffe6 #xffe7 ; APP0-APP7
        #xffe8 #xffe9 #xffea #xffeb #xffec #xffed #xffee #xffef) ; APP8-APP15
       (let* ((len (read-u16 port))
              (payload-len (- len 2)))
         (unless (>= payload-len 0)
           (error "Invalid marker segment length" marker len))
         (file-position port (+ (file-position port) payload-len))
         (skip-params port)))
      (else marker))))

(define (frame-baseline? frame)
  (case (frame-marker frame)
    ((#xffc0) #t)                       ; SOF0
    (else #f)))

(define (frame-sequential? frame)
  (case (frame-marker frame)
    ((#xffc0 #xffc1 #xffc3 #xffc9 #xffcb) #t) ; SOF0,SOF1,SOF3,SOF9,SOF11
    (else #f)))

(define (frame-progressive? frame)
  (case (frame-marker frame)
    ((#xffc2 #xffca) #t)                ; SOF2,SOF10
    (else #f)))

(define (frame-huffman-coded? frame)
  (case (frame-marker frame)
    ((#xffc0 #xffc1 #xffc2 #xffc3) #t)  ; SOF0,SOF1,SOF2,SOF3
    (else #f)))

(define (frame-arithmetic-coded? frame)
  (case (frame-marker frame)
    ((#xffc9 #xffca #xffcb) #t)         ; SOF9,SOF10,SOF11
    (else #f)))

(define (frame-lossless? frame)
  (case (frame-marker frame)
    ((#xffc3 #xffcb) #t)                ; SOF3,SOF11
    (else #f)))

(define (frame-dct? frame)
  (case (frame-marker frame)
    ((#xffc0 #xffc1 #xffc2 #xffc9 #xffca) #t) ; SOF0,SOF1,SOF2,SOF9,SOF10
    (else #f)))

(define (frame-component-count frame)
  (vector-length (frame-components frame)))

(define (ceiling/ a b)
  (inexact->exact (ceiling (/ a b))))
(define (frame-mcu-width frame)
  (ceiling/ (frame-x frame) (* (frame-samp-x frame) 8)))

(define (frame-mcu-height frame)
  (ceiling/ (frame-y frame) (* (frame-samp-y frame) 8)))

(define (read-frame-header port sof)
  (case sof
    ;; There is no SOF8.
    ((#xffc0 #xffc1 #xffc2 #xffc3 #xffc4 #xffc5 #xffc6 #xffc7 ; SOF0-SOF7
             #xffc9 #xffca #xffcb #xffcc #xffcd #xffce #xffcf) ; SOF9-SOF15
     (let* ((len (read-u16 port)))
       (unless (>= len 8)
         (error "Invalid frame header segment length" sof len))
       (let* ((precision (read-u8 port))
              (y (read-u16 port))
              (x (read-u16 port))
              (component-count (read-u8 port)))
         (unless (= len (+ 8 (* component-count 3)))
           (error "Invalid frame header segment length" sof len))
         (unless (> component-count 0)
           (error "No components in frame"))
         (when (zero? x)
           (error "Invalid zero-width image"))
         (when (zero? y)
           (error "DNL not supported"))
         (let* ((components
                 (for/vector ((n (in-range component-count)))
                   (let* ((id (read-u8 port))
                          (samp (read-u8 port))
                          (samp-x (arithmetic-shift samp -4))
                          (samp-y (bitwise-and samp #xf))
                          (table (read-u8 port)))
                     ;; Although 3 is technically permitted, it's
                     ;; pretty bogus.
                     (unless (memv samp-x '(1 2 4))
                       (error "Bad horizontal sampling value" samp-x))
                     (unless (memv samp-x '(1 2 4))
                       (error "Bad vertical sampling value" samp-y))
                     (unless (< table 4)
                       (error "Bad quantization table value" table))
                     (component id n samp-x samp-y table))))
                (samp-x (for/fold ((samp-x 1)) ((c components))
                          (max samp-x (component-samp-y c))))
                (samp-y (for/fold ((samp-y 1)) ((c components))
                          (max samp-y (component-samp-y c)))))
           (frame sof precision y x components samp-x samp-y)))))
    (else (error "Invalid start-of-frame marker" sof))))

(define (allocate-dct-matrix frame)
  (build-array
   (vector (frame-mcu-height frame) (frame-mcu-width frame))
   (match-lambda
     ((vector i j)
      (for/vector ((component (frame-components frame)))
        (build-array
         (vector (component-samp-y component) (component-samp-x component))
         (match-lambda
           ((vector i j)
            (make-vector (* 8 8) 0)))))))))

;; return current dc
(define (read-block bit-port block prev-dc-q q-table dc-table ac-table)
  (define (record! index quantized-coefficient)
    (let* ((index (bytes-ref normal-order index))
           (q (vector-ref q-table index)))
      (vector-set! block index (* quantized-coefficient q))))
  ;; First, read DC coefficient.
  (let* ((dc-diff-bits (read-huffman-coded-value bit-port dc-table))
         (dc-qdiff (read-signed-bits bit-port dc-diff-bits))
         (dc-q (+ prev-dc-q dc-qdiff)))
    (record! 0 dc-q)
    ;; Now read AC coefficients.
    (let lp ((k 1))
      (let* ((code (read-huffman-coded-value bit-port ac-table)))
        (let ((r (arithmetic-shift code -4))
              (s (bitwise-and code #xf)))
          (cond
           ((zero? s)
            ;; #xf0 indicates 16 zeroes.  Otherwise stop.
            (when (eqv? r #xf)
              (lp (+ k 16))))
           (else
            (let* ((bits (read-signed-bits bit-port s))
                   (k (+ k r)))
              (record! k bits)
              ;; Loop if there are more coefficients.
              (when (< k 63)
                (lp (add1 k)))))))))
    ;; Return DC coefficient.
    dc-q))

(define (read-mcu bit-port scan-components mcu)
  (for ((scan-component scan-components))
    (match scan-component
      ((vector component prev-dc q-table dc-table ac-table)
       (vector-set! scan-component 1
                    (for/fold ((dc prev-dc))
                        ((block (in-array (vector-ref mcu (component-index component)))))
                      (read-block bit-port block
                                  dc q-table dc-table ac-table)))))))

(define (read-dct-scan bit-port scan-components dest Ss Se Ah Al)
  (unless (and (= Ss 0) (= Se 63) (= Ah 0) (= Al 0))
    (error "progressive frame reading not yet supported"))
  (for ((mcu (in-array dest)))
    (read-mcu bit-port scan-components mcu)))

(define (read-scan port frame params dest)
  (define (find-component id)
    (or (for/or ((component (frame-components frame)))
          (and (= (component-id component) id)
               component))
        (error "No component found with id" id)))
  (unless (frame-dct? frame) (error "DCT frame expected" frame))
  (unless (frame-huffman-coded? frame) (error "Huffman coding expected" frame))
  (let ((len (read-u16 port)))
    (unless (>= len 6)
      (error "Unexpected scan segment length" len))
    (let ((scan-component-count (read-u8 port)))
      (unless (= len (+ 6 (* scan-component-count 2)))
        (error "Unexpected scan segment length" len))
      (let ((scan-components (make-vector scan-component-count)))
        (for/fold ((next-component-index 0))
            ((i (in-range scan-component-count)))
          (let* ((id (read-u8 port))
                 (T (read-u8 port))
                 (Td (arithmetic-shift T -4))
                 (Ta (bitwise-and T #xf))
                 (component (find-component id)))
            (unless (< Td 4) (error "Bad Td" Td))
            (unless (< Ta 4) (error "Bad Ta" Ta))
            (unless (<= (component-index component) next-component-index)
              (error "Bad component ordering in scan" component))
            (vector-set! scan-components i
                         (vector
                          component
                          0 ;; Previous DC coefficient.
                          (let ((q (component-q-table component)))
                            (or (vector-ref (params-q-tables params) q)
                                (error "Missing Q table" q)))
                          (or (vector-ref (params-dc-tables params) Td)
                              (error "Missing DC table" Td))
                          (or (vector-ref (params-ac-tables params) Ta)
                              (error "Missing AC table" Ta))))
            (add1 (component-index component))))
        (let* ((Ss (read-u8 port))
               (Se (read-u8 port))
               (A (read-u8 port))
               (Ah (arithmetic-shift A -4))
               (Al (bitwise-and A #xf))
               (bit-port (make-bit-port port)))
          (cond
           ((frame-sequential? frame)
            (unless (zero? Ss) (error "Bad Ss for sequential frame" Ss))
            (unless (= Se 63) (error "Bad Se for sequential frame" Se))
            (unless (zero? Ah) (error "Bad Ah for sequential frame" Ah))
            (unless (zero? Al) (error "Bad Al for sequential frame" Al))
            (read-dct-scan bit-port scan-components dest 0 63 0 0))
           ((frame-progressive? frame)
            (unless (<= Ss Se 63) (error "Bad Ss / Se" Ss Se))
            (unless (< Ah 14) (error "Bad Ah" Ah))
            (unless (< Al 14) (error "Bad Ah" Al))
            (read-dct-scan bit-port scan-components dest Ss Se Ah Al))
           (else (error "Unsupported frame type" frame))))))))

(define (read-jfif port #:with-body? (with-body? #t)
                   #:with-misc-sections? (with-misc-sections? #t))
  (cond
   ((string? port)
    (call-with-input-file port
      (lambda (port)
        (read-jfif port
                   #:with-body? with-body?
                   #:with-misc-sections? with-misc-sections?))))
   ((bytes? port)
    (read-jfif (open-input-bytes port)
               #:with-body? with-body?
               #:with-misc-sections? with-misc-sections?))
   (else
    (read-soi port)
    (call-with-values (lambda ()
                        (read-params port *initial-params* with-misc-sections?))
      (lambda (image-params sof)
        (let* ((frame (read-frame-header port sof))
               (dest (allocate-dct-matrix frame)))
          (let lp ((params image-params) (misc (params-misc-segments image-params)))
            (call-with-values (lambda ()
                                (read-params port params with-misc-sections?))
              (lambda (scan-params marker)
                (case marker
                  ((#xffd9)             ; EOI
                   (jfif frame misc dest))
                  ((#xffda)             ; SOS
                   (cond
                    (with-body?
                     (read-scan port frame scan-params dest)
                     (lp scan-params (append misc (params-misc-segments scan-params))))
                    (else
                     (jfif frame misc dest))))
                  (else
                   (error "Unexpected marker" marker))))))))))))

(define (q-tables-for-mcu-array mcu-array #:max-value (max-value 255))
  (define (gcd* coeff q) (gcd (abs coeff) q))
  (define (meet-tables coeffs q)
    (if q
        (vector-map gcd* coeffs q)
        (vector-map abs coeffs)))
  (define (meet-mcu-blocks sequence q)
    (for/fold ((q q)) ((coeffs sequence))
      (meet-tables coeffs q)))
  (call-with-values
      (lambda ()
        (for/fold ((luma-q #f) (chroma-q #f)) ((mcu (in-array mcu-array)))
          (match mcu
            ((vector y)
             (values (meet-mcu-blocks (in-array y) luma-q)
                     chroma-q))
            ((vector y u v)
             (values (meet-mcu-blocks (in-array y) luma-q)
                     (meet-mcu-blocks (sequence-append (in-array u)
                                                       (in-array v))
                                      chroma-q))))))
    (lambda (luma-q chroma-q)
      (define (fixup q)
        (cond
         ((zero? q) 255)
         ((<= q max-value) q)
         ((zero? (remainder q 2)) (fixup (/ q 2)))
         (else (error "q out of range" q))))
      (vector (vector-map fixup luma-q) (vector-map fixup chroma-q)
              #f #f))))

(define (compute-block-codes block q-table prev-dc)
  (let ((zzq (for/vector ((i (in-range 64)))
               (let ((i (bytes-ref normal-order i)))
                 (/ (vector-ref block i) (vector-ref q-table i))))))
    (define (bit-count x)
      (cond
       ((negative? x) (let lp ((n 1)) (if (< (arithmetic-shift -1 n) x) n (lp (add1 n)))))
       ((zero? x) 0)
       (else  (let lp ((n 1)) (if (< x (arithmetic-shift 1 n)) n (lp (add1 n)))))))
    (define (code-and-bits code bits) (bitwise-ior code (arithmetic-shift bits 8)))
    (define (encode-dc dc) (code-and-bits (bit-count dc) dc))
    (define (encode-ac ac zero-count)
      (code-and-bits (bitwise-ior (arithmetic-shift zero-count 4) (bit-count ac)) ac))
    (define (skip-zeroes i zero-count codes)
      (let ((ac (vector-ref zzq i)))
        (if (zero? ac)
            (if (= i 63)
                (cons 0 codes) ;; EOB.
                (skip-zeroes (add1 i) (add1 zero-count) codes))
            (let lp ((zero-count zero-count) (codes codes))
              (if (< zero-count 16)
                  (encode-next (add1 i)
                               (cons (encode-ac ac zero-count) codes))
                  (lp (- zero-count 16) (cons #xf0 codes))))))) ; ZRL.
    (define (encode-next i codes)
      (if (= i 64)
          codes
          (skip-zeroes i 0 codes)))
    (let ((dc (vector-ref zzq 0)))
      (values dc
              (cons (encode-dc (- dc prev-dc))
                    (reverse (encode-next 1 '())))))))

(define (compute-code-sequences jpeg)
  (define (compute-scan-components frame q-tables)
    (vector-map
     (lambda (component)
       (let ((q-table (vector-ref q-tables (component-q-table component))))
         ;; We don't know the dc and ac huffman tables yet.
         (vector component 0 q-table #f #f)))
     (frame-components frame)))
  (match jpeg
    ((jfif frame misc mcu-array)
     (let* ((q-tables (q-tables-for-mcu-array mcu-array))
            (scan-components (compute-scan-components frame q-tables)))
       (values
        q-tables
        (for/array #:shape (array-shape mcu-array)
                   ((mcu (in-array mcu-array)))
          (vector-map
           (lambda (blocks scan-component)
             (match scan-component
               ((vector component prev-dc q-table dc-table ac-table)
                (call-with-values
                    (lambda ()
                      (for/fold ((dc prev-dc) (out '()))
                          ((block (in-array blocks)))
                        (call-with-values
                            (lambda ()
                              (compute-block-codes block q-table dc))
                          (lambda (dc codes)
                            (values dc (cons codes out))))))
                  (lambda (dc out)
                    ;; good up to here.
                    (vector-set! scan-component 1 dc)
                    (reverse out))))))
           mcu
           scan-components)))))))

(define (compute-code-frequencies codes)
  (let ((dc-freqs (vector (make-vector 256 0) (make-vector 256 0) #f #f))
        (ac-freqs (vector (make-vector 256 0) (make-vector 256 0) #f #f)))
    (define (count! table code)
      (let ((idx (bitwise-and code #xff)))
        (vector-set! table idx (add1 (vector-ref table idx)))))
    (define (accumulate-frequencies codes idx)
      (let ((dc-freqs (vector-ref dc-freqs idx))
            (ac-freqs (vector-ref ac-freqs idx)))
        (match codes
          ((cons dc ac)
           (count! dc-freqs dc)
           (for-each (lambda (ac) (count! ac-freqs ac)) ac)))))
    (for ((mcu (in-array codes)))
      (for ((k (in-naturals))
            (blocks (in-vector mcu)))
        (for ((codes (in-list blocks)))
          (let ((idx (if (zero? k) 0 1)))
            (accumulate-frequencies codes idx)))))
    (vector dc-freqs ac-freqs)))

(define (compute-huffman-code-tables dc-and-ac-freqs)
  (vector-map
   (lambda (freqs-v)
     (vector-map
      (lambda (freqs)
        (and freqs (compute-huffman-table-for-freqs freqs)))
      freqs-v))
   dc-and-ac-freqs))

(define (write-short u16 port)
  (write-byte (arithmetic-shift u16 -8) port)
  (write-byte (bitwise-and u16 #xff) port))

(define (write-soi port)
  (write-short #xffd8 port)) ; SOI.

(define (write-misc-segment port misc)
  (write-short (misc-marker misc) port)
  (write-short (+ 2 (bytes-length (misc-bytes misc))) port)
  (write-bytes (misc-bytes misc) port))

(define (write-baseline-frame port frame)
  (write-short #xffc0 port) ; SOF0.
  (let ((len (+ 8 (* (frame-component-count frame) 3))))
    (write-short len port))
  (write-byte (frame-precision frame) port)
  (write-short (frame-y frame) port)
  (write-short (frame-x frame) port)
  (write-byte (frame-component-count frame) port)
  (for ((component (in-vector (frame-components frame))))
    (write-byte (component-id component) port)
    (write-byte (bitwise-ior (arithmetic-shift (component-samp-x component) 4)
                             (component-samp-y component)) port)
    (write-byte (component-q-table component) port)))

(define (write-q-tables port q-tables)
  (for ((i (in-naturals))
        (table (in-vector q-tables)))
    (when table
      (write-short #xffdb port) ; DQT.
      (let ((len (+ 3 64)))
        (write-short len port))
      (let ((P 0)
            (T i))
        (write-byte (bitwise-ior (arithmetic-shift P 4) T) port))
      (let lp ((i 0))
        (when (< i 64)
          (let ((i (bytes-ref normal-order i)))
            (write-byte (vector-ref table i) port))
          (lp (add1 i)))))))

(define (write-huffman-tables port huffman-tables)
  (define (write-table k table Tc)
    (match table
      (#f #f)
      ((vector size-counts size-offsets
         values value-indexes sizes codes max-codes)
       (write-short #xffc4 port)            ; DHT.
       (let ((len (+ 19 (bytes-length values))))
         (write-short len port))
       (write-byte (bitwise-ior (arithmetic-shift Tc 4) k) port)
       (write-bytes size-counts port)
       (write-bytes values port))))
  (match huffman-tables
    ((vector dc-tables ac-tables)
     (for ((k (in-naturals)) (table (in-vector dc-tables)))
       (write-table k table 0))
     (for ((k (in-naturals)) (table (in-vector ac-tables)))
       (write-table k table 1)))))

(define (write-baseline-scan-header port frame)
  (write-short #xffda port) ; SOS.
  (let ((len (+ 6 (* (frame-component-count frame) 2))))
    (write-short len port))
  (write-byte (frame-component-count frame) port)
  (for ((k (in-naturals)) (component (in-vector (frame-components frame))))
    (let ((Td (if (zero? k) 0 1))
          (Ta (if (zero? k) 0 1)))
      (write-byte (component-id component) port)
      (write-byte (bitwise-ior (arithmetic-shift Td 4) Ta) port)))
  (let ((Ss 0)
        (Se 63)
        (Ah 0)
        (Al 0))
    (write-byte Ss port)
    (write-byte Se port)
    (write-byte (bitwise-ior (arithmetic-shift Ah 4) Al) port)))

(define (write-baseline-entropy-coded-data port codes huffman-tables)
  (let ((port (make-bit-port port)))
    (match huffman-tables
      ((vector dc-tables ac-tables)
       (define (write-code code table)
         (match table
           ((vector size-counts size-offsets
                    values value-indexes sizes codes max-codes)
            (let* ((u8 (bitwise-and code #xff))
                   (diff (arithmetic-shift code -8))
                   (ssss (bitwise-and code #xf))
                   (code-index (vector-ref value-indexes u8))
                   (code (vector-ref codes code-index))
                   (size (bytes-ref sizes code-index)))
              (write-bits port code size)
              (unless (zero? ssss)
                (write-bits port diff ssss))))))
       (define (write-codes codes idx)
         (let ((dc-table (vector-ref dc-tables idx))
               (ac-table (vector-ref ac-tables idx)))
           (match codes
             ((cons dc ac)
              (write-code dc dc-table)
              (for-each (lambda (ac) (write-code ac ac-table)) ac)))))
       (for ((mcu (in-array codes)))
         (for ((k (in-naturals)) (blocks (in-vector mcu)))
           (for ((codes (in-list blocks)))
             (let ((idx (if (zero? k) 0 1)))
               (write-codes codes idx)))))
       (flush-bits port)))))

(define (write-eoi port)
  (write-short #xffd9 port)) ; EOI.

(define (write-jfif port jpeg)
  (cond
   ((string? port)
    (call-with-output-file port
      (lambda (port) (write-jfif port jpeg))))
   (else
    (match jpeg
      ((jfif frame misc mcu-array)
       (call-with-values (lambda () (compute-code-sequences jpeg))
         (lambda (q-tables codes)
           (let* ((frequencies (compute-code-frequencies codes))
                  (huffman-tables (compute-huffman-code-tables frequencies)))
             (write-soi port)
             (for-each (lambda (misc) (write-misc-segment port misc)) misc)
             (write-baseline-frame port frame)
             (write-q-tables port q-tables)
             (write-huffman-tables port huffman-tables)
             (write-baseline-scan-header port frame)
             (write-baseline-entropy-coded-data port codes huffman-tables)
             (write-eoi port)))))))))

(module+ test
  (require rackunit)
  (define test-file-name "./test.jpg")
  (define expected-width 500)
  (define expected-height 375)
  (define test-jfif (read-jfif test-file-name #:with-body? #f))
  (check-eqv? (frame-x (jfif-frame test-jfif)) expected-width)
  (check-eqv? (frame-y (jfif-frame test-jfif)) expected-height))
