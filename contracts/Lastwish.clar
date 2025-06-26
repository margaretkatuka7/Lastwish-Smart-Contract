(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_WILL_NOT_FOUND (err u101))
(define-constant ERR_WILL_ALREADY_EXISTS (err u102))
(define-constant ERR_NOT_DECEASED (err u103))
(define-constant ERR_ALREADY_CLAIMED (err u104))
(define-constant ERR_INSUFFICIENT_BALANCE (err u105))
(define-constant ERR_INVALID_BENEFICIARY (err u106))
(define-constant ERR_WILL_EXPIRED (err u107))
(define-constant ERR_TOO_EARLY (err u108))

(define-map wills
  { testator: principal }
  {
    beneficiaries: (list 10 principal),
    amounts: (list 10 uint),
    death-block: uint,
    expiry-block: uint,
    is-active: bool,
    total-amount: uint,
    created-at: uint
  }
)

(define-map beneficiary-claims
  { testator: principal, beneficiary: principal }
  { claimed: bool, claim-block: uint }
)

(define-map testator-status
  { testator: principal }
  { last-heartbeat: uint, is-deceased: bool, declared-dead-at: uint }
)

(define-data-var heartbeat-threshold uint u144)

(define-public (create-will 
  (beneficiaries (list 10 principal))
  (amounts (list 10 uint))
  (expiry-blocks uint))
  (let (
    (testator tx-sender)
    (current-block stacks-block-height)
    (total-amount (fold + amounts u0))
  )
    (asserts! (is-none (map-get? wills { testator: testator })) ERR_WILL_ALREADY_EXISTS)
    (asserts! (> total-amount u0) ERR_INSUFFICIENT_BALANCE)
    (asserts! (> expiry-blocks u0) ERR_INVALID_BENEFICIARY)
    (asserts! (is-eq (len beneficiaries) (len amounts)) ERR_INVALID_BENEFICIARY)
    
    (try! (stx-transfer? total-amount testator (as-contract tx-sender)))
    
    (map-set wills
      { testator: testator }
      {
        beneficiaries: beneficiaries,
        amounts: amounts,
        death-block: u0,
        expiry-block: (+ current-block expiry-blocks),
        is-active: true,
        total-amount: total-amount,
        created-at: current-block
      }
    )
    
    (map-set testator-status
      { testator: testator }
      { last-heartbeat: current-block, is-deceased: false, declared-dead-at: u0 }
    )
    
    (ok true)
  )
)

(define-public (heartbeat)
  (let (
    (testator tx-sender)
    (current-block stacks-block-height)
  )
    (map-set testator-status
      { testator: testator }
      { last-heartbeat: current-block, is-deceased: false, declared-dead-at: u0 }
    )
    (ok current-block)
  )
)

(define-public (declare-death (testator principal))
  (let (
    (current-block stacks-block-height)
    (status (unwrap! (map-get? testator-status { testator: testator }) ERR_WILL_NOT_FOUND))
    (will-data (unwrap! (map-get? wills { testator: testator }) ERR_WILL_NOT_FOUND))
    (last-heartbeat (get last-heartbeat status))
    (threshold (var-get heartbeat-threshold))
  )
    (asserts! (get is-active will-data) ERR_WILL_NOT_FOUND)
    (asserts! (> (- current-block last-heartbeat) threshold) ERR_TOO_EARLY)
    (asserts! (not (get is-deceased status)) ERR_NOT_DECEASED)
    
    (map-set testator-status
      { testator: testator }
      (merge status { is-deceased: true, declared-dead-at: current-block })
    )
    
    (map-set wills
      { testator: testator }
      (merge will-data { death-block: current-block })
    )
    
    (ok true)
  )
)

(define-public (claim-inheritance (testator principal))
  (let (
    (beneficiary tx-sender)
    (current-block stacks-block-height)
    (will-data (unwrap! (map-get? wills { testator: testator }) ERR_WILL_NOT_FOUND))
    (status (unwrap! (map-get? testator-status { testator: testator }) ERR_WILL_NOT_FOUND))
    (claim-key { testator: testator, beneficiary: beneficiary })
    (existing-claim (map-get? beneficiary-claims claim-key))
    (beneficiary-index (unwrap! (index-of (get beneficiaries will-data) beneficiary) ERR_INVALID_BENEFICIARY))
    (inheritance-amount (unwrap! (element-at (get amounts will-data) beneficiary-index) ERR_INVALID_BENEFICIARY))
  )
    (asserts! (get is-active will-data) ERR_WILL_NOT_FOUND)
    (asserts! (get is-deceased status) ERR_NOT_DECEASED)
    (asserts! (< current-block (get expiry-block will-data)) ERR_WILL_EXPIRED)
    (asserts! (is-none existing-claim) ERR_ALREADY_CLAIMED)
    (asserts! (> inheritance-amount u0) ERR_INSUFFICIENT_BALANCE)
    
    (try! (as-contract (stx-transfer? inheritance-amount tx-sender beneficiary)))
    
    (map-set beneficiary-claims
      claim-key
      { claimed: true, claim-block: current-block }
    )
    
    (ok inheritance-amount)
  )
)

(define-public (update-will 
  (beneficiaries (list 10 principal))
  (amounts (list 10 uint))
  (expiry-blocks uint))
  (let (
    (testator tx-sender)
    (current-block stacks-block-height)
    (existing-will (unwrap! (map-get? wills { testator: testator }) ERR_WILL_NOT_FOUND))
    (status (unwrap! (map-get? testator-status { testator: testator }) ERR_WILL_NOT_FOUND))
    (new-total (fold + amounts u0))
    (old-total (get total-amount existing-will))
  )
    (asserts! (get is-active existing-will) ERR_WILL_NOT_FOUND)
    (asserts! (not (get is-deceased status)) ERR_NOT_DECEASED)
    (asserts! (is-eq (len beneficiaries) (len amounts)) ERR_INVALID_BENEFICIARY)
    
    (if (> new-total old-total)
      (try! (stx-transfer? (- new-total old-total) testator (as-contract tx-sender)))
      (if (< new-total old-total)
        (try! (as-contract (stx-transfer? (- old-total new-total) tx-sender testator)))
        true
      )
    )
    
    (map-set wills
      { testator: testator }
      (merge existing-will {
        beneficiaries: beneficiaries,
        amounts: amounts,
        expiry-block: (+ current-block expiry-blocks),
        total-amount: new-total
      })
    )
    
    (ok true)
  )
)

(define-public (revoke-will)
  (let (
    (testator tx-sender)
    (will-data (unwrap! (map-get? wills { testator: testator }) ERR_WILL_NOT_FOUND))
    (status (unwrap! (map-get? testator-status { testator: testator }) ERR_WILL_NOT_FOUND))
  )
    (asserts! (get is-active will-data) ERR_WILL_NOT_FOUND)
    (asserts! (not (get is-deceased status)) ERR_NOT_DECEASED)
    
    (try! (as-contract (stx-transfer? (get total-amount will-data) tx-sender testator)))
    
    (map-set wills
      { testator: testator }
      (merge will-data { is-active: false })
    )
    
    (ok (get total-amount will-data))
  )
)

(define-public (set-heartbeat-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set heartbeat-threshold new-threshold)
    (ok new-threshold)
  )
)

(define-read-only (get-will (testator principal))
  (map-get? wills { testator: testator })
)

(define-read-only (get-testator-status (testator principal))
  (map-get? testator-status { testator: testator })
)

(define-read-only (get-claim-status (testator principal) (beneficiary principal))
  (map-get? beneficiary-claims { testator: testator, beneficiary: beneficiary })
)

(define-read-only (get-heartbeat-threshold)
  (var-get heartbeat-threshold)
)

(define-read-only (is-will-claimable (testator principal))
  (match (map-get? wills { testator: testator })
    will-data (match (map-get? testator-status { testator: testator })
      status (and 
        (get is-active will-data)
        (get is-deceased status)
        (< stacks-block-height (get expiry-block will-data))
      )
      false
    )
    false
  )
)

(define-read-only (get-inheritance-amount (testator principal) (beneficiary principal))
  (match (map-get? wills { testator: testator })
    will-data (match (index-of (get beneficiaries will-data) beneficiary)
      beneficiary-index (element-at (get amounts will-data) beneficiary-index)
      none
    )
    none
  )
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)