#lang racket

(require "2048-canvas.rkt")
(require math/number-theory)

(random-seed 2)

(define (3-eval w x)
  (+ (list-ref w 0)
     (* (list-ref x 0) (list-ref w 1))
     (* (list-ref x 1) (list-ref w 2))
     (* (list-ref x 2) (list-ref w 3))))

(define (4-eval w x)
  (+ (list-ref w 0)
     (* (list-ref x 0) (list-ref w 1))
     (* (list-ref x 1) (list-ref w 2))
     (* (list-ref x 2) (list-ref w 3))
     (* (list-ref x 3) (list-ref w 4))))


(define (first-layer w x)
  (list (3-eval w (list-ref x 0))
        (3-eval (list-tail w 4) (list-ref x 1))
        (3-eval (list-tail w 8) (list-ref x 2))
        (3-eval (list-tail w 12) (list-ref x 3))))

(define (hidden-layer w x)
  (list (4-eval w x)
        (4-eval (list-tail w 5) x)
        (4-eval (list-tail w 10) x)
        (4-eval (list-tail w 15) x)))

(define (eval w x)
  (hidden-layer (list-tail w 56)
                (hidden-layer (list-tail w 36)
                              (hidden-layer (list-tail w 16)
                                            (first-layer w x)))))


(define (crossover w1 w2)
  (map (λ (i j)
         (if (> (random 100) 90)
             j
             i)) w1 w2))

(define (mutation w)
  (map (λ (x)
         (if (> (random 100) 0)
             (+ x (* (- (random) 0.5) 1))
             x)) w))

(define (h1 linha)
  (define count 0)
  (for ([i (range (- (length linha) 1))])
    (if (< (list-ref linha (+ i 1))
           (list-ref linha i))
        (set! count (+ count 1))
        (void)))
  count)

(define (h2 linha)
  (define count 0)
  (for ([i (range (- (length linha) 1))])
    (if (not (= (list-ref linha (+ i 1))
                (list-ref linha i)))
        (set! count (+ count 1))
        (void)))
  count)

(define (h3 linha)
  (define count 0)
  (for ([i linha])
    (if (not (= i 0))
        (set! count (+ count 1))
        (void)))
  count)


(define (heul linha)
  (list (/ (h1 linha) 4)
        (/ (h2 linha) 4)
        (/ (h3 linha) 4)))

(define (heu matriz)
  (list (heul (list-ref matriz 0))
        (heul (list-ref matriz 1))
        (heul (list-ref matriz 2))
        (heul (list-ref matriz 3))))

(define (jogar w m)
  (define nMatriz m)
  (for ([i (range 1000)])
    (define move (eval w (heu nMatriz)))
    (case (get_index_maior move)
      [(0) (set! nMatriz (cima nMatriz))]
      [(1) (set! nMatriz (baixo nMatriz))]
      [(2) (set! nMatriz (esquerda nMatriz))]
      [(3) (set! nMatriz (direita nMatriz))]))
  (get_high_tile nMatriz))

(define (index-of lst ele)
  (let loop ((lst lst)
             (idx 0))
    (cond ((empty? lst) #f)
          ((equal? (first lst) ele) idx)
          (else (loop (rest lst) (add1 idx))))))

(define (get_index_maior lista)
  (index-of lista (getlargest lista)))

(define (genetico p)
  (define per p)
  (for ([i (range 10000)])
    (define matriz '((0 0 0 0) (0 2 0 0)(0 0 0 0)(0 0 0 0)))
    (define best (sort per > #:key (λ (x) (jogar x matriz)) #:cache-keys? #t))
    (if (divides? 10 i) (print (jogar (list-ref best 0) matriz)) (void))
    (set! per (append (list (list-ref best 0))
                      (build-list 9 (λ (x) (mutation (crossover (list-ref best 0)
                                                                (list-ref best 1)))))))))
