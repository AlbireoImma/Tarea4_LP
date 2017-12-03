#lang racket



(define x '((1 (2)) (2 (1 3)) (3 (2 4 7)) (4 (5 6)) (5 (4)) (6 (4)) (7 (3))))

(define (ism vert grafo rec n_rec)
  (let fun([cant (length grafo)] [recc rec] [n_recc n_rec])
    (if (null? n_rec)
        (if (= cant (length rec))
            #t
            #f)
        (if (member (car n_rec) rec)
            (fun cant rec (cdr n_rec))
            (fun cant (append (car n_rec) rec) (cdr n_rec))
            )
        )))

(define (por_rec vert grafo)
  (let ((a (car (cdr (list-ref grafo vert)))) (b (list(car (list-ref grafo vert)))))
  (cons a (map (lambda (arg)
             (append b (car (cdr (list-ref grafo (- arg 1))))))
       a))))

(define (concatenar lista)
  (let fun([ls '()] [ls2 lista])
    (if (null? ls2)
        ls
        (fun (append ls (car ls2)) (cdr ls2)))))

(define (remv ls)
  (let loop ((ls ls) (visto '()))
     (cond
       ((null? ls) '())
       ((memq (car ls) visto) (loop (cdr ls) visto))
       (else (cons (car ls) (loop (cdr ls) (cons (car ls) visto)))))))

(define (smvertice grafo)
  (let fun ([ls '()] [pos 0])
    (if (= pos (length grafo))
        ls
        (if (= (length (remv (concatenar (por_rec pos grafo)))) (length grafo))
            (fun (append ls (list (car (list-ref grafo pos)))) (+ pos 1))
            (fun ls (+ pos 1))))))

(smvertice x)