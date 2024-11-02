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
