#lang scheme

;; Calcula la serie de Taylor para el seno
;;
;; n : Número de términos
;; x : Valor en el que evaluar la serie
;; retorna : Aproximación del seno de x usando n términos

(define (taylorSenoSimple n x)
  (if (= n 0)
      x
      (+ (* (if (even? n) 1 -1)
            (/ (expt x (+ (* 2 n) 1))
               (apply * (range 1 (+ (* 2 n) 2)))))
         (taylorSenoSimple (- n 1) x))))

;; Calcula la serie de Taylor para el coseno usando recursión de cola
;;
;; n : Número de términos
;; x : Valor en el que evaluar la serie
;; acum : Acumulador para el resultado final
;; retorna : Aproximación del coseno de x usando n términos

(define (taylorCosenoCola n x)
  (define (aux n x acum)
    (if (< n 0)
        acum
        (aux (- n 1) x (+ acum (* (if (even? n) 1 -1)
                                  (/ (expt x (* 2 n))
                                     (apply * (range 1 (+ (* 2 n) 1)))))))))
  (aux n x 0))
