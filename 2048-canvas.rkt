#lang racket/gui
(require rackunit
         racket/class
         racket/gui/base)

(provide cima
         baixo
         direita
         esquerda
         print_matriz
         get_high_tile
         transpor
         fim_de_jogo
         getlargest)

(define (lista-no-cheio-direito matriz)
  (define result (list))
  (for ([linha matriz]
        [i (range 4)])
    (if (= (list-ref linha 0) 0)
        (set! result (append result (list i)))
        (void)))
  result)

(define (lista-no-cheio-esquerdo matriz)
  (define result (list))
  (for ([linha matriz]
        [i (range 4)])
    (if (= (list-ref linha 3) 0)
        (set! result (append result (list i)))
        (void)))
  result)

(define (soma-dois-esquerda lista)
  (set! lista (append (remove* (list 0) lista)
                      (make-list (- 4 (length (remove* (list 0) lista))) 0)))
  (if (and (not (= (list-ref lista 1) 0))
           (= (list-ref lista 1) (list-ref lista 0)))
      (set! lista (list-tail (list-set lista 1 (+ (list-ref lista 1)
                                                  (list-ref lista 0))) 1))
      (void))
  (if (and (not (= (list-ref lista 2) 0))
           (= (list-ref lista 1) (list-ref lista 2)))
      (set! lista (append (take (list-set lista 1 (+ (list-ref lista 1)
                                                     (list-ref lista 2))) 2)
                          (list-tail lista 3)))
      (if (and (not (= (list-ref lista 2) 0))
               (> (length lista) 3)
               (= (list-ref lista 2) (list-ref lista 3)))
          (set! lista (drop-right (list-set lista 2 (+ (list-ref lista 2)
                                                       (list-ref lista 3))) 1))
          (void)))
  (append lista (make-list (- 4 (length lista)) 0)))

(define (soma-dois-direita lista)
  (set! lista (append (make-list (- 4 (length (remove* (list 0) lista))) 0)
                      (remove* (list 0) lista)))
  (if (and (not (= (list-ref lista 2) 0))
           (= (list-ref lista 3) (list-ref lista 2)))
      (set! lista (drop-right (list-set lista 2 (+ (list-ref lista 3)
                                                   (list-ref lista 2))) 1))
      (void))
  (if (and (not (= (list-ref lista 2) 0))
           (> (length lista) 3)
           (= (list-ref lista 1) (list-ref lista 2)))
      (set! lista (append (take (list-set lista 1 (+ (list-ref lista 1)
                                                     (list-ref lista 2))) 2)
                          (list-tail lista 3)))
      (if (and (not (= (list-ref lista 1) 0))
               (= (list-ref lista 0) (list-ref lista 1)))
          (set! lista (list-tail (list-set lista 1 (+ (list-ref lista 0)
                                                      (list-ref lista 1))) 1))
          (void)))
  (append (make-list (- 4 (length lista)) 0) lista))

(define (transpor matriz)
  (define result (make-list 4 (list )))
  (for ([i (range 4)])
    (for ([linha matriz])
      (set! result (list-set result i
                             (append (list-ref result i)
                                     (list (list-ref linha i)))))))
  result)

(define (baixo matriz)
  (set! matriz (transpor matriz))
  (define hold matriz)
  (for ([linha matriz] [i (range 4)])
    (set! matriz (list-set matriz i (soma-dois-direita linha))))
  (if (not (empty? (remove* hold matriz)))
      ((lambda ()
         (define lin (list-ref (lista-no-cheio-direito matriz) (random (length (lista-no-cheio-direito matriz)))))
         (set! matriz (list-set matriz lin (list-set (list-ref matriz lin) 0 (expt 2 (+ 1 (random 2))))))
         (for ([linha matriz] [i (range 4)])
           (define falta-0 (- 4 (length linha)))
           (set! matriz (list-set matriz i (append (make-list falta-0 0) linha))))))
      (void))
  (transpor matriz))

(define (cima matriz)
  (set! matriz (transpor matriz))
  (define hold matriz)
  (for ([linha matriz] [i (range 4)])
    (set! matriz (list-set matriz i (soma-dois-esquerda linha))))
  (if (not (empty? (remove* hold matriz)))
      ((lambda ()
         (define lin (list-ref (lista-no-cheio-esquerdo matriz) (random (length (lista-no-cheio-esquerdo matriz)))))
         (set! matriz (list-set matriz lin (list-set (list-ref matriz lin) 3 (expt 2 (+ 1 (random 2))))))
         (for ([linha matriz] [i (range 4)])
           (define falta-0 (- 4 (length linha)))
           (set! matriz (list-set matriz i (append (make-list falta-0 0) linha))))))
      (void))
  (transpor matriz))

(define (esquerda matriz)
  (define hold matriz)
  (for ([linha matriz] [i (range 4)])
    (set! matriz (list-set matriz i (soma-dois-esquerda linha))))
  (if (not (empty? (remove* hold matriz)))
      ((lambda ()
         (define lin (list-ref (lista-no-cheio-esquerdo matriz) (random (length (lista-no-cheio-esquerdo matriz)))))
         (set! matriz (list-set matriz lin (list-set (list-ref matriz lin) 3 (expt 2 (+ 1 (random 2))))))
         (for ([linha matriz] [i (range 4)])
           (define falta-0 (- 4 (length linha)))
           (set! matriz (list-set matriz i (append (make-list falta-0 0) linha))))))
      (void))
  matriz)


(define (direita matriz)
  (define hold matriz)
  (for ([linha matriz] [i (range 4)])
    (set! matriz (list-set matriz i (soma-dois-direita linha))))
  (if (not (empty? (remove* hold matriz)))
      ((lambda ()
         (define lin (list-ref (lista-no-cheio-direito matriz) (random (length (lista-no-cheio-direito matriz)))))
         (set! matriz (list-set matriz lin (list-set (list-ref matriz lin) 0 (expt 2 (+ 1 (random 2))))))
         (for ([linha matriz] [i (range 4)])
           (define falta-0 (- 4 (length linha)))
           (set! matriz (list-set matriz i (append (make-list falta-0 0) linha))))))
      (void))
  matriz)

(define matriz '((0 0 0 0) (0 2 0 0)(0 0 0 0)(0 0 0 0)))

(define jg2048 (class frame%
                 (super-new)
                 (define/override (on-subwindow-char r e)
                   (define key-code (send e get-key-code))
                   (if (eq? key-code 'up)
                       (set! matriz (cima matriz))
                       (if (eq? key-code 'down)
                           (set! matriz (baixo matriz))
                           (if (eq? key-code 'left)
                               (set! matriz (esquerda matriz))
                               (if (eq? key-code 'right)
                                   (set! matriz (direita matriz))
                                   (void)))))
                   (if (eq? key-code 'release)
                       (if (fim_de_jogo matriz)
                           ((λ ()
                              (send jogo show #f)
                              (message-box "Fim de Jogo"
                                           (number->string (get_high_tile matriz)))))
                           (void))

                       (print_matriz matriz)))))

(define (getlargest lst)
  (if (null? lst)
      #f
      (foldl (lambda (e r) (if (> e r) e r))
             (car lst)
             (cdr lst))))

(define (get_high_tile matriz)
 (define list_highs (list
                     (getlargest (list-ref matriz 0))
                     (getlargest (list-ref matriz 1))
                     (getlargest (list-ref matriz 2))
                     (getlargest (list-ref matriz 3))))
  (getlargest list_highs))

(define (fim_de_jogo matriz)
  (if (empty? (remove* matriz (cima matriz)))
      (if (empty? (remove* matriz (baixo matriz)))
          (if (empty? (remove* matriz (esquerda matriz)))
              (if (empty? (remove* matriz (direita matriz)))
                  #t
                  #f)
              #f)
          #f)
      #f))

(define (print_matriz matriz)
  (define dc (send canvas-game get-dc))
  (send canvas-game refresh-now)
  (for ([i (range 4)])
    (for ([j (range 4)])
      (send dc draw-bitmap (send (make-object tiles% (list-ref (list-ref matriz i) j)) get-bitmap)
            (* 100 j)
            (* 100 i)))))

(define jogo (new jg2048
                  [label "2048"]
                  [width 400]
                  [height 400]))

(define canvas-game (make-object canvas% jogo))

(send jogo show #t)

(define tiles% (class object%
                 (init num)
                 (super-new)
                 (define bmp (make-object bitmap% 100 100))
                 (define bitmap-dc (send bmp make-dc))
                 (define/public (get-bitmap) bmp)
                 (define cor (case num
                               [(0) (make-object color% 238 228 218 0.35)]
                               [(2) (make-object color% 238 228 218)]
                               [(4) (make-object color% 237 224 200)]
                               [(8) (make-object color% 242 177 121)]
                               [(16) (make-object color% 245 149 99)]
                               [(32) (make-object color% 246 124 95)]
                               [(64) (make-object color% 246 94 59)]
                               [(128) (make-object color% 237 207 114)]
                               [(256) (make-object color% 237 204 97)]
                               [(512) (make-object color% 237 200 80)]
                               [(1024) (make-object color% 237 197 63)]
                               [(2048) (make-object color% 237 194 46)]
                               [else (make-object color% 60 58 50)]))

                 ((λ ()
                    (send bitmap-dc set-brush cor 'solid)
                    (send bitmap-dc set-font (make-object font% 30 'modern 'normal 'bold))
                    (send bitmap-dc draw-rectangle 0 0 100 100)
                    (send bitmap-dc draw-text (number->string num) 0 0)))))



(test-equal? "Deslocamento Direita" (soma-dois-direita (list 8 8 8 8)) (list 0 0 16 16))
(test-equal? "Deslocamento Direita" (soma-dois-direita (list 8 8 0 0)) (list 0 0 0 16))
(test-equal? "Deslocamento Direita" (soma-dois-direita (list 8 8 0 8)) (list 0 0 8 16))
(test-equal? "Deslocamento Direita" (soma-dois-direita (list 16 16 8 16)) (list 0 32 8 16))

(test-equal? "Deslocamento Esquerda" (soma-dois-esquerda (list 32 16 8 16)) (list 32 16 8 16))
(test-equal? "Deslocamento Esquerda" (soma-dois-esquerda (list 16 16 8 16)) (list 32 8 16 0))
(test-equal? "Deslocamento Esquerda" (soma-dois-esquerda (list 32 16 8 8)) (list 32 16 16 0))
(test-equal? "Deslocamento Esquerda" (soma-dois-esquerda (list 32 8 8 8)) (list 32 16 8 0))

(test-equal? "Testa contador de vazio" (lista-no-cheio-direito '((0 0 0 2) (0 0 2 2) (0 2 2 2) (2 2 2 2))) '(0 1 2))
(test-equal? "Testa contador de vazio" (lista-no-cheio-direito '((0 0 0 0) (0 0 2 2) (0 2 2 2) (0 2 2 2))) '(0 1 2 3))

(test-equal? "Transpor" (transpor '((0 0 0 2) (0 0 2 2) (0 2 2 2) (2 2 2 2))) '((0 0 0 2) (0 0 2 2) (0 2 2 2) (2 2 2 2)))
(test-equal? "Transpor" (transpor '((4 8 16 32) (0 0 2 2) (8 4 2 0) (0 0 0 2))) '((4 0 8 0) (8 0 4 0) (16 2 2 0) (32 2 0 2)))
(test-equal? "Transpor" (transpor '((0 2 0 2) (2 2 2 2) (4 4 4 4) (16 32 64 128))) '((0 2 4 16) (2 2 4 32) (0 2 4 64) (2 2 4 128)))

(test-equal? "Fim de Jogo" (fim_de_jogo '((2 4 2 4) (4 2 4 2) (2 4 2 4) (4 2 4 2))) #t)
(test-equal? "Fim de Jogo" (fim_de_jogo '((2 4 2 4) (2 4 2 4) (2 4 2 4) (2 4 2 4))) #f)

(test-equal? "Maior elemento" (get_high_tile '((2 4 8 16) (64 16 8 4) (1024 32 8 2) (4096 256 8 16))) 4096)
