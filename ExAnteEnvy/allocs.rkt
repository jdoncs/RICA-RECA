#lang racket
(provide assign newAlloc alloc-join prettyPrintAllocDist allocDist-filter sdMetaEnvy
         allocDist-InitUniform)
(require math/flonum)

; An allocRecord lists the id of an agent and a resource, with a probability of that assignment
;  occuring.
(struct allocRecord (agent resource prob))

; An allocDist is a list of allocRecords, along with a count of the total number
;  of such record that have been compressed together to make this one.
;  Note that the probabilities in the stored records need to be normalized by numDistsMerged to
;   have semantic meaning.
(struct allocDist (records  numDistsMerged))


;;;;;;;;;;;Some utility methods for the data structures above...;;;;;;;;;;;;;;;;

; Defines a lexicographic ordering over allocRecords, based on their agent and then resource id's.
;  Inputs: r1 & r2: allocRecords.
;  Output: #true if r1 precedes r2 in the lexicographic ordering.
(define (record-lt r1 r2)
  (cond [(< (allocRecord-agent r1) (allocRecord-agent r2)) #true]
        [(< (allocRecord-agent r2) (allocRecord-agent r1)) #false]
        [else (cond [(< (allocRecord-resource r1) (allocRecord-resource r2)) #true]
                    [else #false])]))

;Used to initialzie
(define (allocDist-InitUniform ad prob)
  (allocDist (map (lambda (x) (allocRecord (allocRecord-agent x) (allocRecord-resource x) prob))
                  (allocDist-records ad))
             (allocDist-numDistsMerged ad)))

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
  (cond [(empty? metaPrefs)
         (cond [cdfAccPip #true]
               [else #false])]
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
(define (assign alloc agentList resource prob)
  (allocDist (insertRecord (allocDist-records alloc)  (cons (allocRecord agentList resource prob) empty))
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
  (define res (allocDist (alloc-join-helper (allocDist-records this) (allocDist-records other))
             (+ (allocDist-numDistsMerged this) (allocDist-numDistsMerged other))))
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
                (display (allocRecord-prob rec))
                (display "\n")
                (ppHelper (rest lst))]))
  (display "Total paths: ")
  (display (allocDist-numDistsMerged dist))
  (display "\n")
  (ppHelper (allocDist-records dist)))
  
