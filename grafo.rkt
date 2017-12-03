#lang racket

(define x '((1 (2)) (2 (1 3)) (3 (2 4 7)) (4 (5 6)) (5 (4)) (6 (4)) (7 (3))))

#|
Funcion: por_rec
Descripcion: genera la lista de nodos a los cuales se pueden llegar en dos pasos
Parametros:
vert entero
grafo grafo representado (vertice (vecino1 vecino2 ... vecinoN))
Retorno: Lista con los vecinos del vertice y los vecinos de estos
|#

(define (por_rec vert grafo)
  (let ((a (car (cdr (list-ref grafo vert)))) (b (list(car (list-ref grafo vert)))))
  (cons a (map (lambda (arg)
             (append b (car (cdr (list-ref grafo (- arg 1))))))
       a))))

#|
Funcion: concatenar
Descripcion: concatena elementos de una lista la cual tiene listas dentro
Parametros:
lista lista
Retorno: Lista con los elementos de las listas internas disueltos
|#

(define (concatenar lista)
  (let fun([ls '()] [ls2 lista])
    (if (null? ls2)
        ls
        (fun (append ls (car ls2)) (cdr ls2)))))

#|
Funcion: remv
Descripcion: Remueve elementos repetidos de una lista dada
Parametros:
ls lista
Retorno: Lista con los elementos repetidos removidos
|#

(define (remv ls)
  (let loop ((ls ls) (visto '()))
     (cond
       ((null? ls) '())
       ((memq (car ls) visto) (loop (cdr ls) visto))
       (else (cons (car ls) (loop (cdr ls) (cons (car ls) visto)))))))

#|
Funcion: smvertice
Descripcion: Obtiene los vertices madre de una grafo
Parametros:
grafo grafo representado (vertice (vecino1 vecino2 ... vecinoN))
Retorno: Lista con los vertices madre del grafo
|#

(define (smvertice grafo)
  (let fun ([ls '()] [pos 0])
    (if (= pos (length grafo))
        ls
        (if (= (length (remv (concatenar (por_rec pos grafo)))) (length grafo))
            (fun (append ls (list (car (list-ref grafo pos)))) (+ pos 1))
            (fun ls (+ pos 1))))))

(smvertice x)