(define extended-gcd
  (lambda (a b s z t y)
    (if (= (modulo a b) 0 )
      (list b z y) 
      (extended-gcd b (- a (* b (div a b))) z (- s (* z (div a b))) y  (- t (* y (div a b)))))))

(define mod-inverse
  (lambda (a p)
    (modulo (cadr (extended-gcd a p 1 0 0 1)) p)))

(define square
  (lambda (x)
    (* x x)))

(define elliptic-square
  (lambda (a m p)
    (if (zero? (cadr p))
      '(0 0)
      (let
	[(s (modulo (* (+ (* 3 (square (car p))) a) (mod-inverse (* 2 (cadr p)) m)) m))]
	(let
	  [(x3 (modulo (- (square s) (* 2 (car p))) m))]
	  (list x3 (modulo (* -1 (+ (* (- x3 (car p)) s) (cadr p))) m)))))))

(define elliptic-add
  (lambda (m p1 p2)
    (cond [(and (zero? (car p1)) (zero? (cadr p1))) p2]
          [(and (zero? (car p2)) (zero? (cadr p2))) p1]
          [else (let 
                  [(s (modulo (* ( - (cadr p2) (cadr p1)) (mod-inverse (- (car p2) (car p1)) m)) m))]
                  (let [(x3 (modulo (- (square s) (+ (car p1) (car p2))) m))]
                    (list x3 (modulo (- (* s (- (car p1) x3)) (cadr p1)) m))))])))

(define elliptic-mult
  (lambda (a m d p)
    (cond [(zero? d) '(0 0)]
          [(= 1 d) p]
          [(odd? d) (elliptic-add m p (elliptic-mult a m (- d 1) p))]
          [else (elliptic-mult a m (/ d 2) (elliptic-square a m p))])))

(define legendre
  (lambda (a p)
    (mod-expt a (/ (- p 1) 2) p)))

(define mod-expt
  (lambda (x n m)
    (cond [(< n 0) (mod-expt (mod-inverse x m) (* -1 n) m)]
          [(zero? n) 1]
          [(= 1 n) (modulo x m)]
          [(even? n) (mod-expt (modulo (square x) m) (/ n 2) m)]
          [else (modulo (* x (mod-expt (square x) (/ (- n 1) 2) m)) m)])))

(define mod-sqrt?
  (lambda (a p)
     (= 1 (legendre a p))))  

(define pad-m
  (lambda (m p)
    (cond [(zero? m) 0]
          [(mod-sqrt? m p) (mod m p)]
          [else (pad-m (* m 2) p)]))) 
