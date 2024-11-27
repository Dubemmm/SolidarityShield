;; SolidarityShield: Decentralized Mutual Insurance Platform
;; SPDX-License-Identifier: MIT

;; Error Constants
(define-constant err-not-authorized (err u300))
(define-constant err-not-registered (err u100))
(define-constant err-insufficient-contribution (err u101))
(define-constant err-claim-limit-exceeded (err u103))
(define-constant err-oracle-validation-failed (err u201))
(define-constant err-invalid-claim-type (err u202))
(define-constant err-invalid-severity (err u203))
(define-constant err-invalid-evidence (err u204))
(define-constant err-invalid-claim-id (err u205))

;; Contract Constants
(define-constant contract-owner tx-sender)
(define-constant min-contribution u10000000) ;; 0.1 STX
(define-constant voting-period u604800) ;; 7 days in seconds
(define-constant claim-approval-threshold u66) ;; 66% approval required

;; Claim Type Definitions
(define-map claim-parameters
  {claim-type: (string-ascii 50)}
  {
    min-severity: uint,
    max-severity: uint,
    required-evidence: (list 3 (string-ascii 50))
  }
)

;; Oracle Trait Definition
(define-trait oracle-trait
  (
    (validate-claim 
      (
        (string-ascii 50)  ;; claim type
        (list 10 (string-ascii 100))  ;; claim evidence
      ) 
      (response 
        {
          verified: bool, 
          confidence: uint
        } 
        uint
      )
    )
  )
)

;; Member Tracking
(define-map members 
  {member: principal}
  {
    contributed-amount: uint,
    last-contribution-timestamp: uint,
    is-active: bool
  }
)

;; Claim Status Enum
(define-constant claim-status-pending u0)
(define-constant claim-status-oracle-verified u1)
(define-constant claim-status-approved u2)
(define-constant claim-status-rejected u3)

;; Claims Tracking
(define-map claims 
  {claim-id: uint}
  {
    claimant: principal,
    amount: uint,
    claim-type: (string-ascii 50),
    status: uint,
    oracle-confidence: uint,
    voting-deadline: uint,
    yes-votes: uint,
    no-votes: uint,
    claim-evidence: (list 10 (string-ascii 100))
  }
)

;; Global State Variables
(define-data-var total-pool-funds uint u0)
(define-data-var member-count uint u0)
(define-data-var claim-counter uint u0)

;; Member Registration
(define-public (register-member)
  (let 
    (
      (contribution (stx-get-balance tx-sender))
    )
    ;; Check if member is already registered
    (asserts! (is-none (map-get? members {member: tx-sender})) err-not-registered)
    
    ;; Check minimum contribution
    (asserts! (>= contribution min-contribution) err-insufficient-contribution)
    
    ;; Transfer contribution to contract
    (try! (stx-transfer? contribution tx-sender (as-contract tx-sender)))
    
    ;; Register member
    (map-set members 
      {member: tx-sender}
      {
        contributed-amount: contribution,
        last-contribution-timestamp: block-height,
        is-active: true
      }
    )
    
    ;; Update pool and member count
    (var-set total-pool-funds (+ (var-get total-pool-funds) contribution))
    (var-set member-count (+ (var-get member-count) u1))
    
    (ok true)
  )
)

;; Add Claim Parameters (Governance Function)
(define-public (add-claim-parameters 
  (claim-type (string-ascii 50))
  (min-severity uint)
  (max-severity uint)
  (required-evidence (list 3 (string-ascii 50)))
)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (> (len claim-type) u0) err-invalid-claim-type)
    (asserts! (<= min-severity max-severity) err-invalid-severity)
    (asserts! (is-eq (len required-evidence) u3) err-invalid-evidence)
    
    (map-set claim-parameters 
      {claim-type: claim-type}
      {
        min-severity: min-severity,
        max-severity: max-severity,
        required-evidence: required-evidence
      }
    )
    
    (ok true)
  )
)

;; Submit a Claim with Oracle Validation
(define-public (submit-claim 
  (claim-amount uint)
  (claim-type (string-ascii 50))
  (claim-evidence (list 10 (string-ascii 100)))
  (oracle-contract <oracle-trait>)
)
  (let 
    (
      ;; Verify member is registered
      (member (unwrap! (map-get? members {member: tx-sender}) err-not-registered))
      
      ;; Get claim parameters
      (claim-params (unwrap! 
        (map-get? claim-parameters {claim-type: claim-type}) 
        err-not-authorized
      ))
      
      ;; Validate claim with oracle
      (oracle-result (unwrap! 
        (contract-call? oracle-contract validate-claim 
          claim-type 
          claim-evidence
        )
        err-oracle-validation-failed
      ))
      
      ;; Current claim ID
      (current-claim-id (var-get claim-counter))
    )
    ;; Validate oracle response
    (asserts! 
      (and
        (get verified oracle-result)
        (>= (get confidence oracle-result) (get min-severity claim-params))
        (<= (get confidence oracle-result) (get max-severity claim-params))
      )
      err-oracle-validation-failed
    )
    
    ;; Ensure claim is within pool limits
    (asserts! (<= claim-amount (/ (var-get total-pool-funds) u2)) err-claim-limit-exceeded)
    
    ;; Create new claim
    (map-set claims 
      {claim-id: current-claim-id}
      {
        claimant: tx-sender,
        amount: claim-amount,
        claim-type: claim-type,
        status: claim-status-oracle-verified,
        oracle-confidence: (get confidence oracle-result),
        voting-deadline: (+ block-height voting-period),
        yes-votes: u0,
        no-votes: u0,
        claim-evidence: claim-evidence
      }
    )
    
    ;; Increment claim counter
    (var-set claim-counter (+ current-claim-id u1))
    
    (ok current-claim-id)
  )
)

;; Vote on a Claim
(define-public (vote-claim (claim-id uint) (vote bool))
  (let 
    (
      (member (unwrap! (map-get? members {member: tx-sender}) err-not-registered))
      (current-claim (unwrap! (map-get? claims {claim-id: claim-id}) err-invalid-claim-id))
    )
    ;; Check voting deadline
    (asserts! (< block-height (get voting-deadline current-claim)) err-not-authorized)
    
    ;; Update votes based on vote
    (if vote 
      (map-set claims 
        {claim-id: claim-id}
        (merge current-claim {yes-votes: (+ (get yes-votes current-claim) u1)})
      )
      (map-set claims 
        {claim-id: claim-id}
        (merge current-claim {no-votes: (+ (get no-votes current-claim) u1)})
      )
    )
    
    ;; Resolve claim if voting threshold is met
    (try! (resolve-claim claim-id))
    
    (ok true)
  )
)

;; Internal Claim Resolution
(define-private (resolve-claim (claim-id uint))
  (let 
    (
      (current-claim (unwrap! (map-get? claims {claim-id: claim-id}) err-invalid-claim-id))
      (total-votes (+ (get yes-votes current-claim) (get no-votes current-claim)))
      (approval-percentage 
        (if (> total-votes u0)
            (/ (* (get yes-votes current-claim) u100) total-votes)
            u0
        )
      )
    )
    ;; Check if claim meets approval threshold
    (if (>= approval-percentage claim-approval-threshold)
      (begin
        ;; Approve claim and transfer funds
        (map-set claims 
          {claim-id: claim-id}
          (merge current-claim {status: claim-status-approved})
        )
        (try! (as-contract (stx-transfer? 
          (get amount current-claim) 
          tx-sender 
          (get claimant current-claim)
        )))
        (ok true)
      )
      ;; Reject claim if threshold not met
      (begin
        (map-set claims 
          {claim-id: claim-id}
          (merge current-claim {status: claim-status-rejected})
        )
        (ok false)
      )
    )
  )
)

;; Read-only Functions for Transparency
(define-read-only (get-total-pool-funds)
  (var-get total-pool-funds)
)

(define-read-only (get-member-info (member principal))
  (map-get? members {member: member})
)

(define-read-only (get-claim-info (claim-id uint))
  (map-get? claims {claim-id: claim-id})
)

(define-read-only (get-claim-parameters (claim-type (string-ascii 50)))
  (map-get? claim-parameters {claim-type: claim-type})
)