#lang racket

(define elementos (list "escandio" "titanio" "vanadio" "cromo" "manganeso" 
                        "hierro" "cobalto" "niquel" "cobre" "zinc" "itrio" 
                        "circonio" "niobio" "molibdeno" "tecnecio" "rutenio" 
                        "rodio" "paladio" "plata" "cadmio" "hafnio" "tantalo"
                        "wolframio" "renio" "osmio" "iridio" "platino" "oro"
                        "mercurio" "rutherfordio" "dubnio" "seaborgio" "bohrio"
                        "hasio" "meitnerio" "darmstatio" "roentgenio" "copernicio"))

(define (quimica elemento)
  (let rec((x elemento) (ls elementos) (ls2 (list elemento)) (ls3 (list elemento)))
    (if (null? ls)
        ls3
        (if (equal? (string-ref x (- (string-length x) 2)) (string-ref (car ls) 0))
            (if (member (car ls) (flatten ls2))
                (rec x (cdr ls) ls2 ls3)
                (rec (car ls) elementos (flatten (append ls2 (car ls))) (flatten (append ls3 (car ls)))))
            (rec x (cdr ls) ls2 ls3)))))

(quimica "oro")