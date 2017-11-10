#lang racket
(provide assign newAlloc alloc-join prettyPrintAllocDist allocDist-filter sdMetaEnvy)

(struct allocRecord (agent resource prob))

(define (record-lt r1 r2)
  (cond [(< (allocRecord-agent r1) (allocRecord-agent r2)) #true]
        [(< (allocRecord-agent r2) (allocRecord-agent r1)) #false]
        [else (cond [(< (allocRecord-resource r1) (allocRecord-resource r2)) #true]
                    [else #false])]))

(struct allocDist (records  numDistsMerged))

(define (allocDist-filter dist agent)
  (define (adf-helper distRecs)
    (cond [(empty? distRecs) empty]
          [(= agent (allocRecord-agent (first distRecs))) (cons (first distRecs)
                                                                (adf-helper (rest distRecs)))]
          [else (adf-helper (rest distRecs))]))
  (allocDist (adf-helper (allocDist-records dist)) (allocDist-numDistsMerged dist)))

(define (sdEnvy dist1 dist2 prefs)
  (define (getP dist resource)
    (cond [(empty? dist) 0]
          [(= resource (allocRecord-resource (first dist))) (allocRecord-prob (first dist))]
          [else (getP (rest dist) resource)]))
  (define (sdEnvy-helper prefs cdf1 cdf2)
    (cond [(empty? prefs) (- cdf1 cdf2)]
          [else (sdEnvy-helper (rest prefs)
                                (+ cdf1 (getP (allocDist-records dist1) (first prefs)))
                                (+ cdf2 (getP (allocDist-records dist2) (first prefs))))]))
  (sdEnvy-helper prefs 0 0))

;Gives weak-Stochastic Dominance for dist 1 over dist2 under metaPrefence record metaPrefs.
(define (sdMetaEnvy dist1 dist2 metaPrefs cdfAcc cdfAccPip)
  (cond [(empty? metaPrefs) cdfAccPip]
        [else
         (define localCDFDiff (sdEnvy dist1 dist2 (first metaPrefs)))
         (cond [(< ( + localCDFDiff cdfAcc) 0) ;(display "Envy on resource:")
                                               ;(display (first metaPrefs))
                                               ;(display "\n")
                                               ;(prettyPrintAllocDist dist1)
                                               ;(prettyPrintAllocDist dist2)
                                               #false]
               [(> ( + localCDFDiff cdfAcc) 0)
                (sdMetaEnvy dist1 dist2 (rest metaPrefs) (+ cdfAcc localCDFDiff) #true)]
               [else (sdMetaEnvy dist1 dist2 (rest metaPrefs) (+ cdfAcc localCDFDiff) cdfAccPip)])]))
                           


;(struct node (profileList))

(define (newAlloc) (allocDist empty 0))

(define (insertRecord allocList item)
  (define (insertSingleRecord allocList item)
    (cond [(empty? allocList) (cons item allocList)]
          [(record-lt (first allocList) item) (cons (first allocList) (insertRecord (rest allocList) item))]
          [else (cons item allocList)]))
  (cond [(empty? item) allocList]
        [(list? item) (insertRecord (insertSingleRecord allocList (first item)) (rest item))]
        [else (insertSingleRecord allocList item)]))

;; agentList mustn't be empty!
(define (assign alloc agentList resource)
  (allocDist (insertRecord (allocDist-records alloc)  (cons (allocRecord agentList resource 1.0) empty))
             (cond [(> (allocDist-numDistsMerged alloc) 0) (allocDist-numDistsMerged alloc)]
                   [else 1])))

;;Assume both allocDists have already been sorted by agent name.
(define (alloc-join-helper this other)
  (cond [(and (empty? this) (empty? other)) empty]
        [(empty? this) other]
        [(empty? other) this]
        [(record-lt (first this) (first other)) (cons (first this) (alloc-join-helper (rest this) other))]
        [(record-lt (first other) (first this)) (cons (first other) (alloc-join-helper this (rest other)))]
        [else (cons (allocRecord (allocRecord-agent (first this)) (allocRecord-resource (first this))
                                 (+ (allocRecord-prob (first this))  (allocRecord-prob (first other))))
                    (alloc-join-helper (rest this) (rest other)))]))

(define (alloc-join this other)
  ;(display "Joining!\n")
  ;(prettyPrintAllocDist this)
  ;(display "with:\n")
  ;(prettyPrintAllocDist other)
  (define res (allocDist (alloc-join-helper (allocDist-records this) (allocDist-records other))
             (+ (allocDist-numDistsMerged this) (allocDist-numDistsMerged other))))
  ;(display "makes:\n")
  ;(prettyPrintAllocDist res)
  res
    )

(define (prettyPrintAllocDist dist)
  (define (ppHelper lst)
    (cond [(empty? lst) (display "\n")]
          [else (define rec (first lst))
                (display (allocRecord-agent rec))
                (display "->")
                (display (allocRecord-resource rec))
                (display " : " )
                (display (/ (allocRecord-prob rec) (allocDist-numDistsMerged dist)))
                (display "\n")
                (ppHelper (rest lst))]))
  (ppHelper (allocDist-records dist)))
  
