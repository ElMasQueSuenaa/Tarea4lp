#lang scheme

;; Función buscador: Busca un elemento en una lista y devuelve su posición.
;;
;; lista : Lista donde buscar el elemento
;; elemento : Elemento a buscar
;; retorna : Índice del elemento en la lista o -1 si no está presente

(define (buscador lista elemento)
  (define (aux lista elemento idx)
    (cond
      ((null? lista) -1)
      ((equal? (car lista) elemento) idx)
      (else (aux (cdr lista) elemento (+ idx 1)))))
  (aux lista elemento 1))

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

#lang scheme

;; Evalúa una lista de funciones en orden rotacional sobre una lista de números
;;
;; funciones : Lista de funciones lambda
;; numeros : Lista de números a evaluar
;; retorna : Lista de resultados

(define (evaluador funciones numeros)
  (define (rotate lst)
    (if (null? (cdr lst))
        lst
        (append (cdr lst) (list (car lst)))))
  (define (apply-funcs funcs num)
    (if (null? funcs)
        num
        ((car funcs) (apply-funcs (cdr funcs) num))))
  (if (null? numeros)
      '()
      (cons (apply-funcs funciones (car numeros))
            (evaluador (rotate funciones) (cdr numeros)))))

#lang scheme

;; Calcula las profundidades de los tesoros en un árbol
;;
;; arbol : Árbol representado como una lista anidada
;; retorna : Lista de profundidades de los tesoros ordenada de menor a mayor

(define (profundidades arbol)
  (define (aux arbol depth)
    (cond
      ((null? arbol) '())
      ((not (pair? arbol)) (if (equal? arbol 'T) (list depth) '()))
      (else (foldl append '()
                    (map (lambda (subtree) (aux subtree (+ 1 depth)))
                         arbol)))))
  (sort (aux arbol 0) <))
