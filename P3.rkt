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

