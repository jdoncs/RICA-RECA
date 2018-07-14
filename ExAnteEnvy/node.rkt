#lang racket
(require "profileReader.rkt")
(require "allocs.rkt")

#|Notes for next time
- Strong envy seems like fail whale (i.e. we consistenly lose, likely due to giving top preferences
  more certainly).
- Check weak envy for RSD? Seems like a long shot.
- Then check the giving of top preferences (seems like we win here routinely). 
|# 
(struct node (profile alloc resourcesRemaining agentsRemaining probHere))

(define (getAgentList myNodeSplits)
    (cond [(empty? myNodeSplits) empty]
        [else (cons (nodeSplit-agent (first myNodeSplits)) (getAgentList (rest myNodeSplits)))]))

;Produces an allocation distribution that is the distribution for mynode, but
; with the assignments implied by myNodeSplits added in.
(define (generateNextAlloc mynode myNodeSplits agentList agLen)
  (cond [(empty? agentList) empty]
        [else (cons (assign (allocDist-InitUniform (node-alloc mynode)
                                              (* (node-probHere mynode) (/ 1 agLen)))
                            (first agentList) (nodeSplit-resource (first myNodeSplits))
                            (* (node-probHere mynode) (/ 1 agLen)))
                    (generateNextAlloc mynode (rest myNodeSplits) (rest agentList) agLen))]))

(define (filterAndRectifyProfile mynode)
  (rectifyIndiffClasses
   (map (lambda (x)  (profileFilter x (node-agentsRemaining mynode) (node-resourcesRemaining mynode)))
                             (node-profile mynode))
   (node-agentsRemaining mynode)))


(define (updateNodeProfile mynode newProf)
  (node newProf (node-alloc mynode) (node-resourcesRemaining mynode) (node-agentsRemaining mynode)
        (node-probHere mynode)))
  
(define (node-expand mynode RSDMode)
  (define (helper children allocs resources agents probUpdate)
    (foldl (lambda (alloc child results)
             (define candidateNode (node (nodeSplit-profile child) alloc
                                         (remove (nodeSplit-resource child) resources)
                                         (remove (nodeSplit-agent child) agents) probUpdate))
             (alloc-join
              (node-expand
               (if RSDMode (updateNodeProfile candidateNode (filterAndRectifyProfile candidateNode))
                   candidateNode) RSDMode) results))
           (newAlloc) allocs children))
  (cond [(empty? (node-profile mynode)) (node-alloc mynode)]
        [(empty? (first (node-profile mynode)))
                 (cond [(empty? (rest (node-profile mynode))) (node-alloc mynode)]
                       [else (node-expand (updateNodeProfile
                                           mynode (filterAndRectifyProfile mynode)) RSDMode)])]
        [else
         (define targetResource ((if RSDMode RSDwhichNext whichResourceNext) (first (node-profile mynode)) (node-resourcesRemaining mynode)))
         (define children (genKids targetResource mynode))
         (define alloc (generateNextAlloc mynode children (getAgentList children) (length children)))
         (helper children alloc (node-resourcesRemaining mynode) (node-agentsRemaining mynode)
                 (* (node-probHere mynode) (/ 1 (length children))))]))

(define (genKids resList mynode)
  (cond [(empty? resList) empty]
        [(list? resList) (append
                          (splitOnResource (first resList) (first (node-profile mynode)) 
                                           (node-profile mynode))
                          (genKids (rest resList) mynode))]
        [else (splitOnResource resList (first (node-profile mynode))  (node-profile mynode))]))


(define numAgents 5)
(define numResources 5)
(define simReps 1000)
(define-struct results (RICA-envy RSD-envy RICA-time RSD-time))

(define (reps n)
  (if (= 0 (remainder n 100)) (printf "~a \n" n) #false)

  (random-seed n)
  (define prof (genMetaProf '(3 2) 
                            numAgents numResources))
  (define localStart (current-seconds))
  (define result (node-expand (node prof (newAlloc) (build-list numResources values)
                                    (map (lambda (x) (+ x 1)) (build-list numAgents values)) 1)
                              #false))
  (define afterRICA (current-seconds))
  (define RSDresult (node-expand (node prof (newAlloc) (build-list numResources values)
                                       (map (lambda (x) (+ x 1)) (build-list numAgents values)) 1)
                                 #true))
  (define afterRSD (current-seconds))
  (define numEnvious (countEnviousAgents prof result))
  (define RSDnumEnvious (countEnviousAgents prof RSDresult))
  #|(if (> numEnvious RSDnumEnvious) (begin
                                     (printf "Seed: ~a\nRICA:~a\nRSD:~a\n"
                                             n
                                             numEnvious
                                             RSDnumEnvious)
                                     (prettyPrintMetaProfile prof)
                                     (prettyPrintAllocDist result)
                                     (prettyPrintAllocDist RSDresult)
                                     (error "WTF!!!")) #true)|#
  (define sum-record (if (= n 0) 1 (reps (- n 1))))
  (cond [(= n 0) (make-results 0 0 0 0)]
        [else (make-results
               (+ numEnvious (results-RICA-envy sum-record))
               (+ RSDnumEnvious (results-RSD-envy sum-record))
               (+ (- afterRICA localStart) (results-RICA-time sum-record))
               (+ (- afterRSD afterRICA) (results-RSD-time sum-record)))]))

(define simResult (reps simReps))

(printf "RICA:\n\tAverage Envy: ~a\n\tAverage Time: ~a\n\nRSD:\n\tAverage Envy: ~a\n\tAverage Time: ~a\n"
        (* 100.0 (/ (results-RICA-envy simResult) (* numAgents simReps)))
        (* 100.0 (/ (results-RICA-time simResult) simReps))
        (* 100.0 (/ (results-RSD-envy simResult) (* numAgents simReps)))
        (* 100.0 (/ (results-RSD-time simResult) simReps)))
        
