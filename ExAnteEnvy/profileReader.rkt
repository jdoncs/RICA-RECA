#lang racket
(provide countEnviousAgents 
         rectifyIndiffClasses
         whichResourceNext RSDwhichNext
         splitOnResource profileFilter nodeSplit-agent nodeSplit-resource nodeSplit-profile
         genProfile genMetaProf
         prettyPrintMetaProfile prettyPrintProfile)
(require "allocs.rkt")

;A nodeSplit is used to represent a node in our search where a given agent was assigned to
;  a given resource. The attached profile is the resulting profile following this assignment.
(struct nodeSplit (agent resource profile))

;A record is used to represent a single agent's single indifference class.
; It is a list of resources with an identifier attached.
;Frusteratingly, it is used for another parallel purpose, with a second meaning,
; in the profile generation code. Ignore this.
(struct record (agent prefList))

;;;;::These functions are related to the computation of envy in a given allocation.;;;;;;;;                                                             

;Computes whether a given agent envies at least one other agent
; under a given allocation.
;Inputs:
;  - indiffClasses: A list of indifference classes representing a complete
;       preference profile with indifferences. A list of lists of records.
;  - agent: A numeric identifier, corresponding to one record in each indifference class.
;  - fullAlloc: An allocation.
(define (agentHasEnvy indiffClasses agent fullAlloc)
  (define agentPrefs (extractAndRectifyPrefList indiffClasses agent))
  (define agentAllocs (allocDist-filter fullAlloc agent))
  (ormap (lambda (x) (not (sdMetaEnvy agentAllocs (allocDist-filter fullAlloc (record-agent x))
                                      agentPrefs 0 #false)))
         (filter (lambda (x) (not (= (record-agent x) agent))) (first indiffClasses))))

;Produces a count of the number of agents who are envious of the allocations
; of at least one other agent.
;Consumes:
; -indiffClasses: a list of indifference classes (i.e. a list of lists of records),
;                 representing a preference profile with indifferences.
; -fullAlloc: an stochastic allocation.
(define (countEnviousAgents indiffClasses fullAlloc)
  (length (filter (lambda (x) (agentHasEnvy indiffClasses (record-agent x) fullAlloc))
                  (first indiffClasses))))

;;;;;;;;These functions are used to rectify a preference profile after an indif class is resolved;;;;
;;;;;;;;They move all empty indifference classes to the end of each agent's preferences.
;Accepts:
; -indiffClasses: A list of indifference classes (i.e. a list of lists of records)
; -agent: An identifier for a single agent.
;Produces:
; A list of list of resources (note: _not_ a list of records. Why?) containing
;  all the entries for that agent in the indifference classes stuck together.
(define (extractAndRectifyPrefList indiffClasses agent)
 (sort (map (lambda (x) (if (empty? x) x (record-prefList (first x))))
            (map (lambda (x) (filter (lambda (y) (= agent (record-agent y))) x))
                 indiffClasses))
       (lambda (x y) (empty? y))))

;Accepts:
; -indiffClasses: A list of indifference classes representing a profile with indifferences
; -agent: An identifier for an agent.
; -agentPrefs: A list of indiff classes for a single agent (the one with identifier _agent_)
; Produces:
;  - A version of indiffClasses where each agent's preferences
(define (realignIDClasses indiffClasses agent agentPrefs)
  (map (lambda (idClass agentPref)
         (cons (record agent agentPref)
               (filter (lambda (rec) (not (= (record-agent rec) agent))) idClass)))
         indiffClasses agentPrefs))

;Accepts:
;  - indiffClasses: A list of indifference classes representing a profile with indifferences.
;  - allowedAgents: A list of agents who have not yet been assigned resources.
;Produces
; - A profile in which all agents' indifference classes have been slid over into a single block
;   on the lefthand side, padded with empty classes at the end.
(define (rectifyIndiffClasses indiffClasses allowedAgents)
  (filter (lambda (x) (not (empty? x)))
          (cond [(empty? allowedAgents) indiffClasses]
                [else
                 (define rec (extractAndRectifyPrefList  indiffClasses (first allowedAgents)))
                 (rectifyIndiffClasses (realignIDClasses indiffClasses (first allowedAgents) rec)
                                       (rest allowedAgents))])))

;;;;;;;;;;;These three functions are used to "split" a search node for a new assignment;;;;;;;;;;

;Accepts:
;resource: The resource we are assigning on in each split.
;frontIDClass: An indifference class. A list of records.
;indifferenceClasses: A list of indifference classes.
;
;Produces: A list of new node-splits, in which the given resource has
;  been assigned to each agent that listed it in their top indifference class (prefs),
; and the resource has been filtered out of this indifference class (but not metaPrefs),
;  with the rest of metaPrefs appended to produce a new profile.
(define (splitOnResource resource frontIDClass indifferenceClasses)
  (map (lambda (x) (nodeSplit (record-agent x) resource
                              (cons (removeResource (filter (lambda (y) (not (equal? y x))) frontIDClass)
                                                    resource)
                                    (rest indifferenceClasses))))
  (filter (lambda (x) (member resource (record-prefList x))) frontIDClass)))  

;Removes a resource from every record in the given indifference class.
; Input:
;   - idClass: A list of records.
;   - resource a resource.
;Output an updated idClass that contains no mention of resouce in any record.
; Note: empty records are omitted entirely. This gets patched up in rectifyIndiffClasses above.
(define (removeResource idClass resource)
  (foldl (lambda (y z)
             (define shortened (filter (lambda (x) (not (= x resource))) (record-prefList y)))
            (if (empty? shortened) z (cons (record (record-agent y) shortened) z)))
         '() idClass))

;Given an indifference class, filters out all agents that have already been assigned an item
; and filters out preferences for every item that has already been assigned to an agent.
;Input:
;  - indiffClass: A list of records representing one indifference class.
;  - allowedAgents: A list of agent identifiers that haven't been assigned items yet.
;  - allowedResources: A list of resource identifiers that haven't been assigned to agents yet.
;Output: A truncated version of indiffClass.
; Note: empty records are omitted entirely. This gets patched up in rectifyIndiffClasses above.
(define (profileFilter indiffClass allowedAgents allowedResources)
  (filter (lambda (x) (not (empty? (record-prefList x))))
          (map (lambda (x) (cond [(not (member (record-agent x) allowedAgents)) (record empty empty)]
                                 [else (record (record-agent x)
                                               (filter (lambda (y) (member y allowedResources))
                                                       (record-prefList x)))]))
               indiffClass)))

;;;;;;;;; These two functions form the core of the differences between our methods and RSD.;;;;;;;;;
;; RICA uses "whichResourceNext" to select the next resource to allocate and assign it.
;; RSD uses "RSDwhichNext". Each produces a _list_ of the highest priority resources.
; They both consume an indifference class: that is, the top indifference grouping of
;; each agent's preferences, stuck together in a list.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Produces a list of the resources which are in the top indifference class of the
;; _fewest_ agents.
;; Out in the fold is a pair of the counted occurences for the minimum so far,
;;   and a list of all resources occuring that often.
(define (whichResourceNext indiffClass resources)
    (second (foldl (lambda (count res out) (cond [(= count 0) out]
                                       [(or (= (first out) -1) (< count (first out)))
                                        (list count (list res))]
                                       [(= count (first out)) (list count (cons res (second out)))]
                                       [else out]))
         (list -1 empty)
         (map (lambda (x) ;counts the number of occurences of each resource in the preferences.
                (length (filter (lambda (y) (member x (record-prefList y))) indiffClass)))
               resources)
         resources)))

;;Just produces a list of the top preference for every agent.
(define (RSDwhichNext indiffClass resourceSet)
  (remove-duplicates (map (lambda (x) (first (record-prefList x))) indiffClass)))



;;;;;;;;;;;;;These functions are used to generate new preference profiles with indifference;;;;;;;;;;

;Generates a flat record, consisting of a list of resources,
; representing a total order over maxRes resources
  (define (random-record agentID maxRes)
    (define (random-record-helper numPrefs accList)
      (cond [(> numPrefs 0)
             (define candidate (random maxRes))
             (cond [(member candidate accList) (random-record-helper numPrefs accList)]
                   [else (random-record-helper (- numPrefs 1) (cons candidate accList))])]
            [else accList]))
    (record agentID (random-record-helper maxRes empty)))

;Generates a profile of total orders over numResources resources,
;containing preferences for numRecs agents.
  (define (genProfile numRecs numResources)
    (build-list numRecs (lambda (x) (random-record (add1 x) numResources))))


;Converts a profile of total orders into a profile of partial orders.
; The so-called "meta-profile" consists of a list of indiference classes.
; Each indifference class is a list of agent preferences for resources that
;  should be considered equivilant to each other.
  (define (genMetaProf indifClassSizes numRecs numResources)
    (define tmpProf (genProfile numRecs numResources))
    (define (profChop prof inset size)
      (map (lambda (x) (record (record-agent x)
                               (take (list-tail (record-prefList x) inset) size))) prof))
    (first (foldl (lambda (x y) (list (cons (profChop tmpProf (second y) x) (first y))
                                      (+ x (second y)))) (list empty 0) indifClassSizes)))


;;;;;;;;;;;;;Output methods, mostly for debugging purposes only;;;;;;;;
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