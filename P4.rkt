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

