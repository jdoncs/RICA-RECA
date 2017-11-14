#lang racket
(provide readProfile whichResourceNext resolve nodeSplit-agent nodeSplit-resource nodeSplit-profile
         enumResources genProfile profileFilter profileSubsetFilter genMetaProf prettyPrintMetaProfile
         prettyPrintProfile profileIsEnvyFree? profileAgentFilter RSDwhichNext metaProfRectify)
(require "allocs.rkt")

(struct nodeSplit (agent resource profile))
(struct record (agent prefList))

(define (extractMetaRecord metaProf agent)
  (cond [(empty? metaProf) empty]
        [else (cons (record-prefList (first
                                      (filter (lambda (x) (= (record-agent x) agent))
                                              (first metaProf))))
                    (extractMetaRecord (rest metaProf) agent))]))                                     

(define (agentHasEnvy metaProf agent fullAlloc)
  (define (agentHasEnvy-helper recList)
    (cond [(empty? recList) #false]
          [(= (record-agent (first recList)) agent) (agentHasEnvy-helper (rest recList))]
          [(sdMetaEnvy (allocDist-filter fullAlloc (record-agent (first recList)))
                       (allocDist-filter fullAlloc agent)
                       (extractMetaRecord metaProf agent) 0 #false) #true]
          [else (agentHasEnvy-helper (rest recList))]))
  (agentHasEnvy-helper (first metaProf)))

(define (profileIsEnvyFree? metaProf fullAlloc)
  (define (profileIsEnvyFree-helper recList)
    (cond [(empty? recList) #true]
          [(agentHasEnvy metaProf (record-agent (first recList)) fullAlloc) #false]
          [else (profileIsEnvyFree-helper (rest recList))]))
  (profileIsEnvyFree-helper (first metaProf)))

;(define (profile-resolve-members profile resource)
;  (cond [(empty? profile) empty]
;        [(member resource (record-prefList (first profile))) (cons (record-agent (first profile))(alloc-generate (rest profile) resource))]
;        [else (alloc-generate (rest profile) resource)]))

;These methods "resolve" a profile by generating the set of profiles that would result
;  from assigning each object to each agent in turn.
;The big problem with this right now is that we need to tag each of the generated profiles with an allocation.
(define (union x y)
  (remove-duplicates (append x y)))

(define (removeResource prefs resource)
  (cond [(empty? prefs) empty]
        [(empty? (filter (lambda (x) (not (= x resource))) (record-prefList (first prefs))))
         (removeResource (rest prefs) resource)]
        [else (cons (record (record-agent (first prefs))
                            (filter (lambda (x) (not (= x resource))) (record-prefList (first prefs))))
                    (removeResource (rest prefs) resource))]))

(define (profileFilter prof allowed)
  (cond [(empty? prof) empty]
        [(empty? (filter (lambda (x) (member x allowed)) (record-prefList (first prof))))
         (profileFilter (rest prof) allowed)]
        [else (cons (record (record-agent (first prof))
                            (filter (lambda (x) (member x allowed)) (record-prefList (first prof))))
                    (profileFilter (rest prof) allowed))]))

(define (metaRecordRectify metaRecord)
  (filter (lambda (x) (not (empty? x))) metaRecord))

(define (metaifyRecord metaProf agent)
  (cond [(empty? metaProf) empty]
        [else
              (define rec (filter (lambda (x) (= agent (record-agent x))) (first metaProf)))
              (define located (cond [(empty? rec) empty]
                                    [else (record-prefList (first rec))]))
              (cond [(empty? located) (metaifyRecord (rest metaProf) agent)]
                    [else (cons located (metaifyRecord (rest metaProf) agent))])]))

(define (metaRecordSmooth metaProf agent metaRecord)
  (cond [(empty? metaProf) empty]
        [else (cons
               (map (lambda (x) (cond [(= (record-agent x) agent)
                                       (record agent (cond [(empty? metaRecord) empty]
                                                           [else (first metaRecord)]))]
                                      [else x])) (first metaProf))
               (metaRecordSmooth (rest metaProf) agent (cond [(empty? metaRecord) empty]
                                                           [else (rest metaRecord)])))]))
              
        

(define (metaProfRectify metaProf agentList)
  (cond [(empty? agentList) empty]
        [else
              (define rec (metaifyRecord metaProf (first agentList)))
              (metaProfRectify (metaRecordSmooth metaProf (first agentList) (metaRecordRectify rec))
                               (rest agentList))]))
  

(define (profileAgentFilter prof allowed)
  (filter (lambda (x) (member (record-agent x) allowed)) prof))
         
;Filters out rows in prof for any agents that do not also appear in otherProf
(define (profileSubsetFilter prof otherProf)
  ;(prettyPrintProfile prof)
   ; (prettyPrintProfile otherProf)
   (cond [(empty? prof) empty]
         [(empty? otherProf) empty]
         [(= (record-agent (first prof)) (record-agent (first otherProf)))
          (cons (first prof) (profileFilter (rest prof) (rest otherProf)))]
         [else (profileFilter (rest prof) (otherProf))]))
            

(define (resolve prefs beforeMe resource metaPrefs)
  ;(display resource)
  ;(display "\n")
  (cond [(empty? prefs) empty]
        [(member resource (record-prefList (first prefs)))
         (cons (nodeSplit (record-agent (first prefs)) resource
                          (cons (removeResource (append beforeMe (rest prefs)) resource) (rest metaPrefs)))
               (resolve (rest prefs) (cons (first prefs) beforeMe) resource metaPrefs))]
        [else (resolve (rest prefs) (cons (first prefs) beforeMe) resource metaPrefs)]))



;These methods can determine which resource to resolve next.
; However, they cannot resolve multiple EQ classes at this time.
; Possible extension: generate _multiple_ preference profiles at the start.
;  Then just select from the first profile, but filter from all of them.
;  Should be simple to set up.
(define (countOccurances prefs resource)
  (cond [(empty? prefs) 0]
        [(member resource (record-prefList (first prefs)) )
         (+ 1 (countOccurances (rest prefs) resource))]
        [else (countOccurances (rest prefs) resource)]))

(struct countThingy (count list))

(define (whichResourceNextHelper prefs resources)
  (cond [(empty? resources) (countThingy -1 empty)]
        [else
         (define res  (whichResourceNextHelper prefs (rest resources)))
         (define localCount (countOccurances prefs (first resources)))
         ;(printf "Counted ~a for ~a~n" localCount (first resources))
        (cond [(= localCount 0) res]
              [(or (< localCount (countThingy-count res)) (= (countThingy-count res) -1))
               (countThingy localCount (cons (first resources) empty))]
              [(= localCount (countThingy-count res))
               ;(printf "~a count = ~a count~n" (first resources) (countThingy-list res))
               (countThingy localCount (cons (first resources) (countThingy-list res)))]
              [else res])]))

(define (whichResourceNext prefProfile resourceSet)
  ;(prettyPrintProfile prefProfile) 
  ;(display (countThingy-list (whichResourceNextHelper prefProfile resourceSet)))
  (countThingy-list (whichResourceNextHelper prefProfile resourceSet)))

(define (picker resourceSet)
  (define (helper resourceSet idx)
    (cond [(= idx 0) (first resourceSet)]
          [else (helper (rest resourceSet) (- idx 1))]))
  (cond [(list? resourceSet) (helper resourceSet (random (length resourceSet)))]
        [else resourceSet]))

(define (RSDwhichNext prefProfile resourceSet)
  (display "RSD Stub!\n")
  )


(define (enumResources profile)
  (cond [(empty? profile) empty]
        [(union (record-prefList (first profile)) (enumResources (rest profile)))]))



;These methods facilitate reading in a profile.
;A profile is a list of pairs. The first element of the pair is an id number.
; the second element is a list of numbers representing the resources that the agent prefers.
(define (read-line numPrefs)
  (cond [ (> numPrefs 0) (cons (read) (read-line (- numPrefs 1)))]
        [else null]))

(define (read-record numPrefs agentID)
  (record agentID (read-line numPrefs)))

(define (readProfile numPrefs numAgents)
  (cond [ (> numAgents 0) (cons (read-record numPrefs numAgents) (readProfile numPrefs (- numAgents 1)))]
        [ (eq? numAgents 1) (read-record numPrefs numAgents) ]
        [ else empty ]))

(define (random-record numPrefs agentID maxRes)
  (define (random-record-helper numPrefs accList)
  (cond [(> numPrefs 0)
         (define candidate (random maxRes))
         (cond [(member candidate accList) (random-record-helper numPrefs accList)]
               [else (random-record-helper (- numPrefs 1) (cons candidate accList))])]
        [else accList]))
  (record agentID (random-record-helper numPrefs empty)))

(define (genProfile numPrefs numRecs numResources)
  (cond [(> numRecs 0) (cons (random-record numPrefs numRecs numResources)
                             (genProfile numPrefs (- numRecs 1) numResources))]
        [else empty]))

(define (genMetaProf eqClassSizes numRecs numResources)
  (define tmpProf (genProfile (foldl + 0 eqClassSizes) numRecs numResources))
  (define (recordChop prefs inset size)
    (cond [(empty? prefs) empty]
          [(= size 0) empty]
          [(> inset 0) (recordChop (rest prefs) (- inset 1) size)]
          [(> size 0) (cons (first prefs) (recordChop (rest prefs) inset (- size 1)))]))
  (define (profChop prof inset size)
    (cond [(empty? prof) empty]
          [else (cons (record (record-agent (first prof))
                         (recordChop (record-prefList (first prof)) inset size))
                 (profChop (rest prof) inset size))]))
  (define (makeMetaProfs prof eqClassSizes inset)
    (cond [(empty? eqClassSizes) empty]
          [else (cons (profChop prof inset (first eqClassSizes))
                      (makeMetaProfs prof (rest eqClassSizes) (+ inset (first eqClassSizes))))]))
  (makeMetaProfs tmpProf eqClassSizes 0))
  
(define (prettyPrintMetaProfile mprof)
  (cond [(empty? mprof) (display "-----\n")]
        [else (prettyPrintProfile (first mprof))
              (prettyPrintMetaProfile (rest mprof))]))

(define (prettyPrintProfile prof)
  (cond [(empty? prof) (display "\n")]
        [else (define this (first prof))
              (display "Agent ")
              (display (record-agent this))
              (display ": ")
              (display (record-prefList this))
              (display "\n")
              (prettyPrintProfile (rest prof))]))


;(prettyPrintMetaProfile (genMetaProf '(1 2 3) 4 6))