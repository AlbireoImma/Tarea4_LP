#lang scheme

(define (lenght lista)
  (if (null? lista)
      0
      (+ 1 (lenght (cdr lista)))
      )
  )

(define (operar fun)
  (lambda (lista1 lista2)
    (define ret (list))
    (define len (lenght lista1))
    (cond ((null? lista1) ret))
    (do ((i 0 (+ i 1)))
      ((= i len) ret)
      (if (= 1 (fun (car lista1) (car lista2)))
          (set! ret (append ret (list 1)))
          (set! ret (append ret (list 0)))
          )
      (set! lista1 (cdr lista1))
      (set! lista2 (cdr lista2))
    )
  )
)


((operar *) (list 0 1 0 1) (list 1 1 1 0))