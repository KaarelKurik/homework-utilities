(use-modules (srfi srfi-1))

(define (monic n t)
  (typen (list-tabulate (+ n 1) (lambda (x) (if (< x n) 0 1))) t))

(define (degree p) (- (length p) 1))

(define (simplify p)
  (if (and (zero? (last p)) (> (length p) 1))
      (simplify (drop-right! p 1))
      p))

(define (type x)
  (cadr x))

(define (value x)
  (car x))

(define (zero? v)
  (cond ((equal? 'q (type v)) (= 0 (value v)))
        ((equal? '(mod 7) (type v)) (= 0 (modulo (value v) 7)))))

(define (term p i)
  (if (> (length p) i)
      (list-ref p i)
      `(0 ,(type (last p)))))

(define (addf a b)
  (let ((at (type a))
        (bt (type b))
        (av (value a))
        (bv (value b)))
    (cond ((equal? at 'q)
           `(,(+ av bv) ,at))
          ((equal? at '(mod 7))
           `(,(modulo (+ av bv) 7) ,at)))))

(define (negf a)
  (let ((at (type a))
        (av (value a)))
    (cond ((equal? at 'q) `(,(- av) ,at))
          ((equal? at '(mod 7)) `(,(modulo (- av) 7) ,at)))))

(define (invf a)
  (let ((at (type a))
        (av (value a)))
    (cond ((equal? at 'q)
           `(,(/ 1 av) ,at))
          ((equal? at '(mod 7))
           `(,(modulo (expt av 5) 7) ,at)))))

(define (divf a b)
  (mulf a (invf b)))

(define (subf a b)
  (addf a (negf b)))

(define (mulf a b)
  (let ((at (type a))
        (bt (type b))
        (av (value a))
        (bv (value b)))
    (cond ((equal? at 'q)
           `(,(* av bv) q))
          ((equal? at '(mod 7))
           `(,(modulo (* av bv) 7) (mod 7))))))

(define (addp p q)
  (if (< (length p) (length q))
      (addp q p)
      (simplify (append (map (lambda (x) (apply addf x)) (zip p q))
                        (drop p (length q))))))

(define (negp p)
  (map negf p))

(define (subp p q)
  (addp p (negp q)))

(define (mul-coef p q i)
  (cond ((null? p) `(0 ,(type (last q))))
        ((null? q) `(0 ,(type (last p))))
        ((= i 0) (mulf (car p) (car q)))
        (else (addf (mulf (term p 0) (term q i)) (mul-coef (cdr p) q (- i 1))))))

(define (mulp p q)
  (simplify (list-tabulate (+ (degree p) (degree q) 1) (lambda (x) (mul-coef p q x)))))

(define (typen x t) (map (lambda (y) (list y t)) x))

(define (constp c) (list c))

(define (zerop? p)
  (or (null? p) (and (= (degree p) 0) (zero? (term p 0)))))

(define (ediv p d)
  (let lp ((L p)
           (R '()))
    (define ddif (- (degree L) (degree d)))
    (if (or (< ddif 0) (zerop? L))
        (list R L)
        (let* ((qx (divf (last L) (last d)))
               (rd (mulp (constp qx) (monic ddif (type qx)))))
          (lp (subp L (mulp rd d)) (addp R rd))))))

(define (egcd p q)
  (cond ((or (< (degree p) (degree q)) (zerop? p)) (egcd q p))
        (else (let lp ((L (list
                           (list q (constp `(0 ,(type (last p)))) (monic 0 (type (last p))))
                           (list p (monic 0 (type (last p))) (constp `(0 ,(type (last p)))))
                           )))
                (if (zerop? (caar L))
                    L
                    (let* ((t0 (cadr L))
                           (t1 (car L))
                           (edp (ediv (car t0) (car t1)))
                           (quo (car edp))
                           (re (cadr edp)))
                      (lp (cons
                           (list re (subp (cadr t0) (mulp (cadr t1) quo)) (subp (caddr t0) (mulp (caddr t1) quo)))
                           L))))))))

(define (lcmp p q)
  (car (ediv (mulp p q) (caadr (egcd p q)))))

(define fb '(5 4 14 18 7 15))
(define gb '(10 -22 39 -25 25))
(define fq (typen fb 'q))
(define gq (typen gb 'q))
(define f7 (typen (map (lambda (x) (modulo x 7)) fb) '(mod 7)))
(define g7 (typen (map (lambda (x) (modulo x 7)) gb) '(mod 7)))
(define eucl-q (egcd fq gq))
(define eucl-7 (egcd f7 g7))
(define (nicen-p p)
  (reverse (map value p)))
(define (nicen-l L)
  (map (lambda (x) (map nicen-p x)) (reverse L)))
(define nice-q (nicen-l eucl-q))
(define nice-7 (nicen-l eucl-7))
(define nice-lcm-q (nicen-p (lcmp fq gq)))
(define nice-lcm-7 (nicen-p (lcmp f7 g7)))
