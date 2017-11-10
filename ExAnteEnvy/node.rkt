#lang racket
(require "profileReader.rkt")
(require "allocs.rkt")

(struct node (profile alloc resourcesRemaining agentsRemaining probHere))

(define (getAgentList myNodeSplits)
    (cond [(empty? myNodeSplits) empty]
        [else (cons (nodeSplit-agent (first myNodeSplits)) (getAgentList (rest myNodeSplits)))]))

;Produces an allocation distribution that is the distribution for mynode, but
; with the assignments implied by myNodeSplits added in.
(define (generateNextAlloc mynode myNodeSplits agentList agLen)
  (cond [(empty? agentList) empty]
        [else (cons (assign (allocDist-update (node-alloc mynode)
                                              (* (node-probHere mynode) (/ 1.0 agLen)))
                            (first agentList) (nodeSplit-resource (first myNodeSplits))
                            (* (node-probHere mynode) (/ 1.0 agLen)))
                    (generateNextAlloc mynode (rest myNodeSplits) (rest agentList) agLen))]))


(define (node-expand mynode)
  (define (helper children alloc resources agents probUpdate)
    (cond [(empty? children) (newAlloc)]
          [else (alloc-join (node-expand (node
                                          (nodeSplit-profile (first children)) (first alloc)
                                          resources
                                          (filter (lambda (x)
                                                    (not (= (nodeSplit-agent (first children)) x)))
                                                    agents)
                                          probUpdate))
                            (helper (rest children) (rest alloc) resources agents probUpdate))]))
  ;(display "\n\nEntering\n")
  ;(prettyPrintProfile (first (node-profile mynode)))
  ;(prettyPrintMetaProfile (node-profile mynode))
  (prettyPrintAllocDist (node-alloc mynode))
  (cond [(empty? (node-profile mynode)) (node-alloc mynode)]
        [(empty? (first (node-profile mynode)))
         ;(display "Merging things...\n")
         ;(prettyPrintAllocDist (node-alloc mynode))
         ;(read)
                 (cond [(empty? (rest (node-profile mynode))) (node-alloc mynode)]
                       [else (define nextProf
                               (profileFilter
                                (profileAgentFilter (first (rest (node-profile mynode)))
                                                     (node-agentsRemaining mynode))
                                (node-resourcesRemaining mynode)))
                             ;(prettyPrintProfile nextProf)
                             (node-expand (node (cons nextProf (rest (rest (node-profile mynode))))
                                                (node-alloc mynode)
                                                (node-resourcesRemaining mynode)
                                                (node-agentsRemaining mynode)
                                                (node-probHere mynode)))])]
        [else
         ;(display "looped\n")
         (define targetResource (first (whichResourceNext (first (node-profile mynode)) (node-resourcesRemaining mynode))))
         (define children (resolve (first (node-profile mynode)) empty targetResource (node-profile mynode)))
         (define alloc (generateNextAlloc mynode children (getAgentList children) (length children)))
         (define resourcesLeft (filter (lambda (x) (not (= x targetResource))) (node-resourcesRemaining mynode)))
         (define result (helper children alloc resourcesLeft (node-agentsRemaining mynode) (* (node-probHere mynode) (/ 1.0 (length children)))))
         ;(display "Backing up from ")
         ;(display (prettyPrintAllocDist result))
          result])
  )


(define numPrefs (read))
(define numAgents (read))
(define numResources (read))

(define (reps n)
  
  (define prof (genMetaProf (cons (/ numPrefs 3) (cons (/ numPrefs 3) (cons (/ numPrefs 3) empty)))
                            numAgents numResources))
  ;(prettyPrintMetaProfile prof)
  (define result (node-expand (node prof (newAlloc) (build-list numResources values)
                                    (map (lambda (x) (+ x 1)) (build-list numAgents values)) 1.0)))
  ;(prettyPrintAllocDist result)
  (cond [(profileIsEnvyFree? prof result)
         (cond  [(= n 1) (display "No issues.")]
                [else (reps (- n 1))])]
        [else (display "Found envy!!!\n")
              (prettyPrintMetaProfile prof)
              (prettyPrintAllocDist result)]))
(reps 100)
