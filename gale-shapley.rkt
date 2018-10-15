#lang racket

(define prefs
  (hasheq
   'Xavier (vector-immutable 'Amy 'Bertha 'Clare)
   'Yancey (vector-immutable 'Bertha 'Amy 'Clare)
   'Zeus (vector-immutable 'Amy 'Bertha 'Clare)
   'Amy (vector-immutable 'Yancey 'Xavier 'Zeus)
   'Bertha (vector-immutable 'Xavier 'Yancey 'Zeus)
   'Clare (vector-immutable 'Xavier 'Yancey 'Zeus)))

; Table to find out which man a given woman perfers in constant time
(define ranking
  (hasheq
   'Amy (hasheq 'Yancey 0 'Xavier 1 'Zeus 2)
   'Bertha (hasheq 'Xavier 0 'Yancey 1 'Zeus 2)
   'Clare (hasheq 'Xavier 0 'Yancey 1 'Zeus 2)))

(define (gale-shapley-helper pairs single-men next)
  (for/fold ([pairs pairs] [single-men single-men] [next next])
            ([man single-men])
    (define next-woman
      (vector-ref (hash-ref prefs man) (hash-ref next man)))
    (define next-single?
      (if (hash-has-key? pairs next-woman) #f #t))
    (define (get-woman-spouse)
      (hash-ref pairs next-woman))
    (define (prefer-man?)
      (let ([current-man (hash-ref (hash-ref ranking next-woman) (get-woman-spouse))]
            [proposing-man (hash-ref (hash-ref ranking next-woman) man)])
        (< proposing-man current-man)))
    (define (increment-next)
      ; returns hash table
      (let ([prev (hash-ref next man)])
        (hash-set next man (add1 prev))))
    (define (add-to-pairs)
      ; returns hash table
      (hash-set pairs next-woman man))
    (cond
      [next-single?
       (values
        (add-to-pairs)
        (cdr single-men)
        (increment-next))]
      [(prefer-man?)
       (define old-man
         (get-woman-spouse))
       (values
        (add-to-pairs)
        (cons old-man (cdr single-men))
        (increment-next))]
      [else
       (values
        pairs
        single-men
        (increment-next))])))

(define (gale-shapley)
  (let loop ([pairs (make-immutable-hasheq)] [single-men (list 'Xavier 'Yancey 'Zeus)] [next (hasheq 'Xavier 0 'Yancey 0 'Zeus 0)])
    (define-values (p s n) (gale-shapley-helper pairs single-men next))
    (if (empty? single-men)
        pairs
        (loop p s n))))

(gale-shapley)