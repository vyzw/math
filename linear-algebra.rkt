;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname linear-algebra) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; A set is a predicate S? such that
;; - (S? x) is true if x is an element of the set
;; - (S? x) is false otherwise

(define-struct field (F + *))
;; A field is a triple consisting of
;; - a set F
;; - a map +: F -> F
;; - a map *: F -> F
;; subject to relevant axioms

(define R (make-field real? + *))
(check-expect ((field-F R) (sqrt 2)) true)
(check-expect ((field-F R) (sqrt -1)) false)
(check-expect ((field-+ R) 304 0.11) 304.11)
(check-expect ((field-* R) 89 0.1) 8.9)

(define (GF p)
  (make-field (lambda (x)
                (member? x (build-list p identity)))
              (lambda (x y)
                (modulo (+ x y) p))
              (lambda (x y)
                (modulo (* x y) p))))

(define F7 (GF 7))
(check-expect ((field-F F7) 3) true)
(check-expect ((field-F F7) 8) false)
(check-expect ((field-F F7) 0.5) false)
(check-expect ((field-+ F7) 6 6) 5)
(check-expect ((field-* F7) 5 4) 6)

(define-struct matrix (m n F entries))
;; an m-by-n matrix with entries from field F
;; (list-ref entries (- i 1)) is the ith row vector
(define I3 (make-matrix 3 3 R
                        (list (list 1 0 0)
                              (list 0 1 0)
                              (list 0 0 1))))

;; Kronecker delta
(define (delta i j)
  (if (= i j) 1 0))

(check-expect (transpose I3) I3)
(check-expect (transpose (make-matrix 2 3 F7
                                      (list (list 1 2 3)
                                            (list 4 5 6))))
              (make-matrix 3 2 F7
                           (list (list 1 4)
                                 (list 2 5)
                                 (list 3 6))))
(define (transpose A)
  (make-matrix (matrix-n A)
               (matrix-m A)
               (matrix-F A)
               (build-list (matrix-n A)
                           (lambda (i)
                             (build-list (matrix-m A)
                                         (lambda (j)
                                           (list-ref (list-ref (matrix-entries A)
                                                               j)
                                                     i)))))))

(check-expect (identity-matrix 3 R) I3)
(define (identity-matrix n F)
  (make-matrix n n F
               (build-list n
                           (lambda (i)
                             (build-list n
                                         (lambda (j)
                                           (delta i j)))))))

(check-expect (build-matrix 2 5 R (lambda (v)
                                    (list (+ (list-ref v 0)
                                             (* 2 (list-ref v 1)))
                                          (+ (list-ref v 2)
                                             (* 3 (list-ref v 3))
                                             (list-ref v 4)))))
              (make-matrix 2 5 R
                           (list (list 1 2 0 0 0)
                                 (list 0 0 1 3 1))))
(check-expect (build-matrix 15 15 F7 identity)
              (identity-matrix 15 F7))
(define (build-matrix m n F fn)
  (transpose (make-matrix n m F
                          (map fn
                               (matrix-entries (identity-matrix n F))))))

(check-expect ((matrix-to-fn I3)
               (list 2 354 1))
              (list 2 354 1))
(check-expect (build-matrix 2 5 R
                            (matrix-to-fn (make-matrix 2 5 R
                                                       (list (list 1 2 0 0 0)
                                                             (list 0 0 1 3 1)))))
              (make-matrix 2 5 R
                           (list (list 1 2 0 0 0)
                                 (list 0 0 1 3 1))))
(define (matrix-to-fn A)
  (local [(define At (transpose A))
          (define + (field-+ (matrix-F A)))
          (define * (field-* (matrix-F A)))]
    (foldr (lambda (f g)
             (lambda (v)
               (local [(define fv (f v))
                       (define gv (g v))]
                 (build-list (matrix-m A)
                             (lambda (i)
                               (+ (list-ref fv i)
                                  (list-ref gv i)))))))
           (lambda (v)
             (build-list (matrix-m A)
                         (lambda (i) 0)))
           (build-list (matrix-n A)
                       (lambda (j)
                         (lambda (v)
                           (map (lambda (aij)
                                  (* aij
                                     (list-ref v j)))
                                (list-ref (matrix-entries At) j))))))))

(check-expect (matrix-product (list (make-matrix 3 2 F7
                                                 (list (list 1 4)
                                                       (list 2 5)
                                                       (list 3 6)))
                                    (make-matrix 2 3 F7
                                                 (list (list 0 0 2)
                                                       (list 0 2 5)))))
              (make-matrix 3 3 F7
                           (list (list 0 1 1)
                                 (list 0 3 1)
                                 (list 0 5 1))))
(define (matrix-product matrices)
  (local [(define m (matrix-m (first matrices)))
          (define n (matrix-n (list-ref matrices (- (length matrices) 1))))
          (define F (matrix-F (first matrices)))]
    (build-matrix m n F
                  (foldr compose
                         identity
                         (map matrix-to-fn
                              matrices)))))