#lang scheme

;; Función: rotate
;; Descripción: Rota una lista pasando el primer elemento al final.
;; Parámetros:
;;   - lst: Lista a rotar.
;;
(define (rotate lst)
  (if (null? lst)           
      '()                   
      (append (cdr lst)    
              (list (car lst))))) 

;; Función: apply-funcs
;; Descripción: Aplica recursivamente una lista de funciones a un valor inicial.
;; Parámetros:
;;   - funcs: Lista de funciones a aplicar.
;;   - value: Valor inicial sobre el cual aplicar las funciones.
;;
(define (apply-funcs funcs value)
  (if (null? funcs)       
      value                
      (apply-funcs (cdr funcs) ((car funcs) value)))) 

;; Función: evaluador
;; Descripción: Evalúa una lista de funciones sobre una lista de números aplicando rotaciones.
;; Parámetros:
;;   - funcs: Lista de funciones lambda.
;;   - nums: Lista de números a evaluar.
;;
(define (evaluador funcs nums)
  (define (rotate-n f n)  
    (if (= n 0)
        f
        (rotate-n (rotate f) (- n 1))))
  (define (eval-loop f n remaining-nums)
    (if (null? remaining-nums)
        '()
        (cons (apply-funcs (rotate-n f n) (car remaining-nums))  
              (eval-loop f (+ n 1) (cdr remaining-nums))))) 
  (eval-loop funcs 0 nums)) 