#lang racket
(require "profileReader.rkt")
(require "allocs.rkt")

(struct node (profile alloc resourcesRemaining))

(define (getAgentList myNodeSplits)
    (cond [(empty? myNodeSplits) empty]
        [else (cons (nodeSplit-agent (first myNodeSplits)) (getAgentList (rest myNodeSplits)))]))

;Produces an allocation distribution that is the distribution for mynode, but
; with the assignments implied by myNodeSplits added in.
(define (generateNextAlloc mynode myNodeSplits agentList)
  (cond [(empty? agentList) empty]
        [else (cons (assign (node-alloc mynode) (first agentList) (nodeSplit-resource (first myNodeSplits)))
                    (generateNextAlloc mynode (rest myNodeSplits) (rest agentList)))]))


(define (node-expand mynode)
  (define (helper children alloc resources)
    (cond [(empty? children) (newAlloc)]
          [else (alloc-join (node-expand (node (nodeSplit-profile (first children)) (first alloc) resources))
                            (helper (rest children) (rest alloc) resources))]))
  ;(display "\n\nEntering\n")
  ;(prettyPrintProfile (first (node-profile mynode)))
  (cond [(empty? (node-profile mynode)) (node-alloc mynode)]
        [(empty? (first (node-profile mynode)))
         ;(display "Merging things...\n")
                 (cond [(empty? (rest (node-profile mynode))) (node-alloc mynode)]
                       [else (define nextProf
                               (profileFilter
                                (profileSubsetFilter (first (rest (node-profile mynode)))
                                                     (first (node-profile mynode)))
                                (node-resourcesRemaining mynode)))
                             ;(prettyPrintProfile nextProf)
                             (node-expand (node (cons nextProf (rest (rest (node-profile mynode))))
                                                (node-alloc mynode)
                                                (node-resourcesRemaining mynode)))])]
        [else
         ;(display "looped\n")
         (define targetResource (first (whichResourceNext (first (node-profile mynode)) (node-resourcesRemaining mynode))))
         (define children (resolve (first (node-profile mynode)) empty targetResource (node-profile mynode)))
         (define alloc (generateNextAlloc mynode children (getAgentList children)))
         (define resourcesLeft (filter (lambda (x) (not (= x targetResource))) (node-resourcesRemaining mynode)))
         (define result (helper children alloc resourcesLeft ))
         ;(display "Backing up from ")
         ;(display (prettyPrintAllocDist result))
          result])
  )


(define numPrefs (read))
(define numAgents (read))
(define numResources (read))
(display "Done with input\n")
(define prof (genMetaProf (cons (/ numPrefs 3) (cons (/ numPrefs 3) empty)) numAgents numResources))
(prettyPrintMetaProfile prof)
(define result (node-expand (node prof (newAlloc) (build-list numResources values))))

(prettyPrintAllocDist result)
(prettyPrintAllocDist (allocDist-filter result 1))
