#lang racket
(require "profileReader.rkt")
(require "allocs.rkt")


(define prefs (readProfile (read) (read)))
(whichResourceNext prefs)

(resolve prefs empty 2)




  
