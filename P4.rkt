#lang scheme

;; Función: profundidades
;; Descripción: Encuentra las profundidades a las que se encuentran los tesoros en un árbol.
;; Parámetros:
;;   - arbol: Árbol representado como una lista anidada.
(define (profundidades arbol)
  ;; Función auxiliar para explorar el árbol y registrar la profundidad de los tesoros encontrados.
  (define (explorar-arbol nodo profundidad)
    (cond
      ((null? nodo) '())  ;; Si el nodo es nulo, no hay más que explorar.
      ((not (pair? nodo)) ;; Si el nodo es una hoja, verifica si contiene un tesoro.
       (if (equal? nodo 'T)
           (list profundidad)  ;; Retorna una lista con la profundidad actual si es un tesoro.
           '()))               ;; Retorna una lista vacía si no es un tesoro.
      (else  ;; Si el nodo es una lista, explora cada subnodo.
       (let* ((first (if (equal? (car nodo) 'T) (list profundidad) '()))
              (rest (apply append
                          (map (lambda (item) (explorar-arbol item (+ profundidad 1)))
                               (cdr nodo)))))
         (append first rest)))))  ;; Combina los resultados del primer elemento y el resto de la lista.

  ;; Ordena las profundidades encontradas, asegurándose de que solo se ordenen si hay elementos.
  (let ((profundidades-encontradas (explorar-arbol arbol 0)))
    (if (null? profundidades-encontradas)
        '()  ;; Retorna una lista vacía si no se encontraron profundidades.
        (sort profundidades-encontradas <))))  ;; Ordena y retorna las profundidades encontradas.


