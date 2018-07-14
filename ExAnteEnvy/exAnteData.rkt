#lang racket

(struct record (agent prefList))

(struct profile (recordList))

(struct allocRecord (agent resource prob))

(struct allocDist (records  numDistsMerged))

(struct node (profileList))