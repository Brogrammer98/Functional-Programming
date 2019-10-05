#lang racket

(require "declarations.rkt")

(provide caesar-shift-encryptor caesar-shift-decryptor)

  (define key 0)
  (define (generate-random-key) (set! key (+ 1 (random 24))))

  (define (change c k) 
    (let* ([i (+ (int c) k)])
      (cond [(= k 0) c]
            [(and (<= (int c) 90) (>= (int c) 65)) (char (+ (modulo (- i 65) 26) 65))]
            [else c])))

  (define (encrypt-change c)
    (change c key))
  (define (decrypt-change c)
    (change c (* -1 key)))


(define (caesar-shift-encryptor msg cskey)
  (if (eq? "random" cskey) (begin (generate-random-key)
                                  (let* ([UPcase (string-upcase msg)]
                                         [char-list (string->list UPcase)]
                                         [key-applied-list (map encrypt-change char-list)]
                                         [encrypted-msg (list->string key-applied-list)])
                                    encrypted-msg))
      (begin (set! key cskey)
             (let* ([UPcase (string-upcase msg)]
                    [char-list (string->list UPcase)]
                    [key-applied-list (map encrypt-change char-list)]
                    [encrypted-msg (list->string key-applied-list)])
               encrypted-msg))))

  (define (caesar-shift-decryptor msg)
    (let* ([char-list (string->list msg)]
           [key-applied-list (map decrypt-change char-list)]
           [decrypted-msg (list->string key-applied-list)])
      decrypted-msg)
   )

  

