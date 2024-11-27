;; SolidarityShield: Decentralized Mutual Insurance Platform
;; SPDX-License-Identifier: MIT

(define-constant contract-owner tx-sender)
(define-constant min-contribution u10000000) ;; 0.1 STX
(define-constant voting-period u604800) ;; 7 days in seconds
(define-constant claim-approval-threshold u66) ;; 66% approval required

;; Error constants
(define-constant err-not-registered (err u100))
(define-constant err-already-registered (err u101))
(define-constant err-insufficient-contribution (err u102))
(define-constant err-claim-limit-exceeded (err u103))
(define-constant err-voting-ended (err u104))
(define-constant err-already-voted (err u105))

;; Store member information
(define-map members 
  {member: principal}
  {
    contributed-amount: uint,
    last-contribution-timestamp: uint,
    is-active: bool
  }
)

;; Claim status enum (using integers)
(define-constant claim-status-pending u0)
(define-constant claim-status-approved u1)
(define-constant claim-status-rejected u2)

;; Store claim information
(define-map claims 
  {claim-id: uint}
  {
    claimant: principal,
    amount: uint,
    status: uint,
    voting-deadline: uint,
    yes-votes: uint,
    no-votes: uint
  }
)

;; Track total pool funds and claim counter
(define-data-var total-pool-funds uint u0)
(define-data-var member-count uint u0)
(define-data-var claim-counter uint u0)

;; Member registration
(define-public (register-member)
  (let 
    (
      (contribution (stx-get-balance tx-sender))
    )
    ;; Check if member is already registered
    (asserts! (is-none (map-get? members {member: tx-sender})) err-already-registered)
    
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

;; Submit a claim
(define-public (submit-claim (claim-amount uint))
  (let 
    (
      (member (unwrap! (map-get? members {member: tx-sender}) err-not-registered))
      (current-claim-id (var-get claim-counter))
    )
    ;; Ensure claim is within pool limits
    (asserts! (<= claim-amount (/ (var-get total-pool-funds) u2)) err-claim-limit-exceeded)
    
    ;; Create new claim
    (map-set claims 
      {claim-id: current-claim-id}
      {
        claimant: tx-sender,
        amount: claim-amount,
        status: claim-status-pending,
        voting-deadline: (+ block-height voting-period),
        yes-votes: u0,
        no-votes: u0
      }
    )
    
    ;; Increment claim counter
    (var-set claim-counter (+ current-claim-id u1))
    
    (ok current-claim-id)
  )
)

;; Vote on a claim
(define-public (vote-claim (claim-id uint) (vote bool))
  (let 
    (
      (member (unwrap! (map-get? members {member: tx-sender}) err-not-registered))
      (current-claim (unwrap! (map-get? claims {claim-id: claim-id}) err-not-registered))
    )
    ;; Check voting deadline
    (asserts! (< block-height (get voting-deadline current-claim)) err-voting-ended)
    
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

;; Resolve claim based on voting results
(define-private (resolve-claim (claim-id uint))
  (let 
    (
      (current-claim (unwrap! (map-get? claims {claim-id: claim-id}) err-not-registered))
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

;; Read-only functions to get contract information
(define-read-only (get-total-pool-funds)
  (var-get total-pool-funds)
)

(define-read-only (get-member-info (member principal))
  (map-get? members {member: member})
)

(define-read-only (get-claim-info (claim-id uint))
  (map-get? claims {claim-id: claim-id})
)