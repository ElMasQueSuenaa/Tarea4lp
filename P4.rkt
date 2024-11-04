#lang scheme

;; Función: profundidades
;; Descripción: Encuentra las profundidades a las que se encuentran los tesoros ('T') en un árbol representado como una lista anidada.
;; Parámetros:
;;   - arbol: Árbol representado como una lista anidada.
;;
(define (profundidades arbol)
  ;; Función auxiliar: explorar-arbol
  ;; Descripción: Explora recursivamente el árbol para encontrar tesoros ('T') y registrar sus profundidades.
  ;; Parámetros:
  ;;   - nodo: Nodo actual del árbol que está siendo evaluado (puede ser un valor o una lista).
  ;;   - profundidad: Nivel de profundidad actual en el árbol, comenzando desde la raíz (0).
  ;;
  (define (explorar-arbol nodo profundidad)
    (cond
      ((null? nodo) '())
      ((not (pair? nodo)) 
       (if (equal? nodo 'T)   
           (list profundidad)
           '()))             
      (else  
       (let* ((first (if (equal? (car nodo) 'T) (list profundidad) '()))  
              (rest (apply append  
                          (map (lambda (item) (explorar-arbol item (+ profundidad 1))) 
                               (cdr nodo)))))
         (append first rest)))))
  
  ;; Función auxiliar: cons-if-not-null
  ;; Descripción: Agrega un elemento a una lista solo si el elemento no es nulo (vacío).
  ;; Parámetros:
  ;;   - item: Elemento a agregar a la lista.
  ;;   - lst: Lista a la cual se le puede agregar el `item`.
  ;;
  (define (cons-if-not-null item lst)
    (if item (cons item lst) lst))  
  (let ((profundidades-encontradas (explorar-arbol arbol 0)))  
    (if (null? profundidades-encontradas)
        '()                              
        (sort profundidades-encontradas <)))) 

