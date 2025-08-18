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
(define-constant ERR_NOT_WITNESS (err u109))
(define-constant ERR_DISPUTE_EXISTS (err u110))
(define-constant ERR_DISPUTE_NOT_FOUND (err u111))
(define-constant ERR_DISPUTE_RESOLVED (err u112))
(define-constant ERR_INSUFFICIENT_WITNESSES (err u113))
(define-constant ERR_WITNESS_ALREADY_VOTED (err u114))
(define-constant ERR_INVALID_DISPUTE_TYPE (err u115))
(define-constant ERR_DISTRIBUTION_NOT_FOUND (err u116))
(define-constant ERR_DISTRIBUTION_EXHAUSTED (err u117))
(define-constant ERR_DISTRIBUTION_LOCKED (err u118))
(define-constant ERR_INVALID_SCHEDULE (err u119))
(define-constant ERR_SCHEDULE_NOT_FOUND (err u120))
(define-constant ERR_INVALID_DISTRIBUTION_TYPE (err u121))

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
(define-data-var witness-requirement uint u3)
(define-data-var dispute-period uint u1008)

(define-map registered-witnesses
  { witness: principal }
  { is-active: bool, registered-at: uint, reputation-score: uint }
)

(define-map will-witnesses
  { testator: principal }
  { witnesses: (list 5 principal), witness-count: uint }
)

(define-map disputes
  { dispute-id: uint }
  {
    testator: principal,
    initiator: principal,
    dispute-type: uint,
    created-at: uint,
    expiry-block: uint,
    is-resolved: bool,
    resolution: uint,
    votes-for: uint,
    votes-against: uint,
    voted-witnesses: (list 10 principal)
  }
)

(define-map dispute-votes
  { dispute-id: uint, witness: principal }
  { vote: bool, voted-at: uint }
)

(define-data-var next-dispute-id uint u1)

(define-map distribution-schedules
  { testator: principal, beneficiary: principal }
  {
    total-amount: uint,
    remaining-amount: uint,
    installment-amount: uint,
    release-interval: uint,
    next-release-block: uint,
    final-release-block: uint,
    distribution-type: uint,
    is-active: bool,
    created-at: uint
  }
)

(define-map distribution-history
  { testator: principal, beneficiary: principal, release-id: uint }
  { amount: uint, released-at: uint, block-height: uint }
)

(define-map beneficiary-conditions
  { testator: principal, beneficiary: principal }
  {
    min-age-requirement: uint,
    achievement-required: bool,
    external-approval-required: bool,
    conditions-met: bool,
    verified-at: uint
  }
)

(define-data-var next-release-id uint u1)

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

(define-public (register-witness)
  (let (
    (witness tx-sender)
    (current-block stacks-block-height)
  )
    (map-set registered-witnesses
      { witness: witness }
      { is-active: true, registered-at: current-block, reputation-score: u100 }
    )
    (ok true)
  )
)

(define-public (assign-witnesses (testator principal) (witnesses (list 5 principal)))
  (let (
    (current-block stacks-block-height)
    (witness-count (len witnesses))
    (required-witnesses (var-get witness-requirement))
  )
    (asserts! (is-eq tx-sender testator) ERR_UNAUTHORIZED)
    (asserts! (>= witness-count required-witnesses) ERR_INSUFFICIENT_WITNESSES)
    
    (map-set will-witnesses
      { testator: testator }
      { witnesses: witnesses, witness-count: witness-count }
    )
    (ok witness-count)
  )
)

(define-public (witness-death-declaration (testator principal) (support-declaration bool))
  (let (
    (witness tx-sender)
    (current-block stacks-block-height)
    (status (unwrap! (map-get? testator-status { testator: testator }) ERR_WILL_NOT_FOUND))
    (will-data (unwrap! (map-get? wills { testator: testator }) ERR_WILL_NOT_FOUND))
    (witness-data (unwrap! (map-get? registered-witnesses { witness: witness }) ERR_NOT_WITNESS))
    (will-witnesses-data (map-get? will-witnesses { testator: testator }))
  )
    (asserts! (get is-active witness-data) ERR_NOT_WITNESS)
    (asserts! (get is-active will-data) ERR_WILL_NOT_FOUND)
    
    (match will-witnesses-data
      witnesses-info
        (asserts! (is-some (index-of (get witnesses witnesses-info) witness)) ERR_NOT_WITNESS)
        true
    )
    
    (if support-declaration
      (map-set registered-witnesses
        { witness: witness }
        (merge witness-data { reputation-score: (+ (get reputation-score witness-data) u10) })
      )
      (map-set registered-witnesses
        { witness: witness }
        (merge witness-data { reputation-score: (- (get reputation-score witness-data) u5) })
      )
    )
    
    (ok support-declaration)
  )
)

(define-public (create-dispute (testator principal) (dispute-type uint))
  (let (
    (initiator tx-sender)
    (current-block stacks-block-height)
    (dispute-id (var-get next-dispute-id))
    (dispute-expiry (+ current-block (var-get dispute-period)))
    (will-data (unwrap! (map-get? wills { testator: testator }) ERR_WILL_NOT_FOUND))
  )
    (asserts! (get is-active will-data) ERR_WILL_NOT_FOUND)
    (asserts! (or (is-eq dispute-type u1) (is-eq dispute-type u2)) ERR_INVALID_DISPUTE_TYPE)
    (asserts! (is-none (map-get? disputes { dispute-id: dispute-id })) ERR_DISPUTE_EXISTS)
    
    (map-set disputes
      { dispute-id: dispute-id }
      {
        testator: testator,
        initiator: initiator,
        dispute-type: dispute-type,
        created-at: current-block,
        expiry-block: dispute-expiry,
        is-resolved: false,
        resolution: u0,
        votes-for: u0,
        votes-against: u0,
        voted-witnesses: (list)
      }
    )
    
    (var-set next-dispute-id (+ dispute-id u1))
    (ok dispute-id)
  )
)

(define-public (vote-on-dispute (dispute-id uint) (vote-for bool))
  (let (
    (witness tx-sender)
    (current-block stacks-block-height)
    (dispute-data (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
    (witness-data (unwrap! (map-get? registered-witnesses { witness: witness }) ERR_NOT_WITNESS))
    (existing-vote (map-get? dispute-votes { dispute-id: dispute-id, witness: witness }))
    (voted-witnesses (get voted-witnesses dispute-data))
  )
    (asserts! (get is-active witness-data) ERR_NOT_WITNESS)
    (asserts! (not (get is-resolved dispute-data)) ERR_DISPUTE_RESOLVED)
    (asserts! (< current-block (get expiry-block dispute-data)) ERR_WILL_EXPIRED)
    (asserts! (is-none existing-vote) ERR_WITNESS_ALREADY_VOTED)
    (asserts! (is-none (index-of voted-witnesses witness)) ERR_WITNESS_ALREADY_VOTED)
    
    (map-set dispute-votes
      { dispute-id: dispute-id, witness: witness }
      { vote: vote-for, voted-at: current-block }
    )
    
    (let (
      (new-votes-for (if vote-for (+ (get votes-for dispute-data) u1) (get votes-for dispute-data)))
      (new-votes-against (if vote-for (get votes-against dispute-data) (+ (get votes-against dispute-data) u1)))
      (new-voted-witnesses (unwrap-panic (as-max-len? (append voted-witnesses witness) u10)))
    )
      (map-set disputes
        { dispute-id: dispute-id }
        (merge dispute-data {
          votes-for: new-votes-for,
          votes-against: new-votes-against,
          voted-witnesses: new-voted-witnesses
        })
      )
      
      (ok vote-for)
    )
  )
)

(define-public (resolve-dispute (dispute-id uint))
  (let (
    (current-block stacks-block-height)
    (dispute-data (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR_DISPUTE_NOT_FOUND))
    (total-votes (+ (get votes-for dispute-data) (get votes-against dispute-data)))
    (required-votes (var-get witness-requirement))
    (votes-for (get votes-for dispute-data))
    (votes-against (get votes-against dispute-data))
  )
    (asserts! (not (get is-resolved dispute-data)) ERR_DISPUTE_RESOLVED)
    (asserts! (>= total-votes required-votes) ERR_INSUFFICIENT_WITNESSES)
    
    (let (
      (resolution (if (> votes-for votes-against) u1 u2))
    )
      (map-set disputes
        { dispute-id: dispute-id }
        (merge dispute-data { is-resolved: true, resolution: resolution })
      )
      
      (ok resolution)
    )
  )
)

(define-public (set-witness-requirement (new-requirement uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set witness-requirement new-requirement)
    (ok new-requirement)
  )
)

(define-public (set-dispute-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set dispute-period new-period)
    (ok new-period)
  )
)

(define-read-only (get-witness-info (witness principal))
  (map-get? registered-witnesses { witness: witness })
)

(define-read-only (get-will-witnesses (testator principal))
  (map-get? will-witnesses { testator: testator })
)

(define-read-only (get-dispute-info (dispute-id uint))
  (map-get? disputes { dispute-id: dispute-id })
)

(define-read-only (get-dispute-vote (dispute-id uint) (witness principal))
  (map-get? dispute-votes { dispute-id: dispute-id, witness: witness })
)

(define-read-only (get-witness-requirement)
  (var-get witness-requirement)
)

(define-read-only (get-dispute-period)
  (var-get dispute-period)
)

(define-read-only (get-next-dispute-id)
  (var-get next-dispute-id)
)

(define-public (create-distribution-schedule 
  (beneficiary principal) 
  (total-amount uint) 
  (installment-amount uint) 
  (release-interval uint) 
  (distribution-type uint))
  (let (
    (testator tx-sender)
    (current-block stacks-block-height)
    (will-data (unwrap! (map-get? wills { testator: testator }) ERR_WILL_NOT_FOUND))
    (inheritance-amount (unwrap! (get-inheritance-amount testator beneficiary) ERR_INVALID_BENEFICIARY))
    (final-release-block (+ current-block (* release-interval (/ total-amount installment-amount))))
  )
    (asserts! (get is-active will-data) ERR_WILL_NOT_FOUND)
    (asserts! (> total-amount u0) ERR_INSUFFICIENT_BALANCE)
    (asserts! (> installment-amount u0) ERR_INVALID_SCHEDULE)
    (asserts! (> release-interval u0) ERR_INVALID_SCHEDULE)
    (asserts! (<= total-amount inheritance-amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (or (is-eq distribution-type u1) (is-eq distribution-type u2) (is-eq distribution-type u3)) ERR_INVALID_DISTRIBUTION_TYPE)
    (asserts! (is-none (map-get? distribution-schedules { testator: testator, beneficiary: beneficiary })) ERR_DISTRIBUTION_NOT_FOUND)
    
    (map-set distribution-schedules
      { testator: testator, beneficiary: beneficiary }
      {
        total-amount: total-amount,
        remaining-amount: total-amount,
        installment-amount: installment-amount,
        release-interval: release-interval,
        next-release-block: (+ current-block release-interval),
        final-release-block: final-release-block,
        distribution-type: distribution-type,
        is-active: true,
        created-at: current-block
      }
    )
    
    (ok true)
  )
)

(define-public (set-beneficiary-conditions 
  (beneficiary principal) 
  (min-age uint) 
  (achievement-required bool) 
  (external-approval bool))
  (let (
    (testator tx-sender)
    (current-block stacks-block-height)
    (will-data (unwrap! (map-get? wills { testator: testator }) ERR_WILL_NOT_FOUND))
  )
    (asserts! (get is-active will-data) ERR_WILL_NOT_FOUND)
    (asserts! (is-some (get-inheritance-amount testator beneficiary)) ERR_INVALID_BENEFICIARY)
    
    (map-set beneficiary-conditions
      { testator: testator, beneficiary: beneficiary }
      {
        min-age-requirement: min-age,
        achievement-required: achievement-required,
        external-approval-required: external-approval,
        conditions-met: false,
        verified-at: u0
      }
    )
    
    (ok true)
  )
)

(define-public (verify-beneficiary-conditions (testator principal) (beneficiary principal))
  (let (
    (current-block stacks-block-height)
    (conditions (unwrap! (map-get? beneficiary-conditions { testator: testator, beneficiary: beneficiary }) ERR_SCHEDULE_NOT_FOUND))
    (will-data (unwrap! (map-get? wills { testator: testator }) ERR_WILL_NOT_FOUND))
    (status (unwrap! (map-get? testator-status { testator: testator }) ERR_WILL_NOT_FOUND))
  )
    (asserts! (get is-active will-data) ERR_WILL_NOT_FOUND)
    (asserts! (get is-deceased status) ERR_NOT_DECEASED)
    (asserts! (not (get conditions-met conditions)) ERR_ALREADY_CLAIMED)
    
    (map-set beneficiary-conditions
      { testator: testator, beneficiary: beneficiary }
      (merge conditions { conditions-met: true, verified-at: current-block })
    )
    
    (ok true)
  )
)

(define-public (claim-scheduled-distribution (testator principal))
  (let (
    (beneficiary tx-sender)
    (current-block stacks-block-height)
    (distribution (unwrap! (map-get? distribution-schedules { testator: testator, beneficiary: beneficiary }) ERR_DISTRIBUTION_NOT_FOUND))
    (conditions (map-get? beneficiary-conditions { testator: testator, beneficiary: beneficiary }))
    (will-data (unwrap! (map-get? wills { testator: testator }) ERR_WILL_NOT_FOUND))
    (status (unwrap! (map-get? testator-status { testator: testator }) ERR_WILL_NOT_FOUND))
    (release-id (var-get next-release-id))
    (claimable-amount (get installment-amount distribution))
    (remaining-amount (get remaining-amount distribution))
  )
    (asserts! (get is-active distribution) ERR_DISTRIBUTION_NOT_FOUND)
    (asserts! (get is-active will-data) ERR_WILL_NOT_FOUND)
    (asserts! (get is-deceased status) ERR_NOT_DECEASED)
    (asserts! (> remaining-amount u0) ERR_DISTRIBUTION_EXHAUSTED)
    (asserts! (>= current-block (get next-release-block distribution)) ERR_DISTRIBUTION_LOCKED)
    
    (match conditions
      condition-data
        (asserts! (get conditions-met condition-data) ERR_DISTRIBUTION_LOCKED)
        true
    )
    
    (let (
      (final-amount (if (< remaining-amount claimable-amount) remaining-amount claimable-amount))
      (new-remaining (- remaining-amount final-amount))
      (new-next-release (+ current-block (get release-interval distribution)))
    )
      (try! (as-contract (stx-transfer? final-amount tx-sender beneficiary)))
      
      (map-set distribution-schedules
        { testator: testator, beneficiary: beneficiary }
        (merge distribution {
          remaining-amount: new-remaining,
          next-release-block: (if (> new-remaining u0) new-next-release u0),
          is-active: (> new-remaining u0)
        })
      )
      
      (map-set distribution-history
        { testator: testator, beneficiary: beneficiary, release-id: release-id }
        { amount: final-amount, released-at: current-block, block-height: current-block }
      )
      
      (var-set next-release-id (+ release-id u1))
      (ok final-amount)
    )
  )
)

(define-public (modify-distribution-schedule 
  (beneficiary principal) 
  (new-installment-amount uint) 
  (new-release-interval uint))
  (let (
    (testator tx-sender)
    (current-block stacks-block-height)
    (distribution (unwrap! (map-get? distribution-schedules { testator: testator, beneficiary: beneficiary }) ERR_DISTRIBUTION_NOT_FOUND))
    (will-data (unwrap! (map-get? wills { testator: testator }) ERR_WILL_NOT_FOUND))
    (status (unwrap! (map-get? testator-status { testator: testator }) ERR_WILL_NOT_FOUND))
  )
    (asserts! (get is-active distribution) ERR_DISTRIBUTION_NOT_FOUND)
    (asserts! (get is-active will-data) ERR_WILL_NOT_FOUND)
    (asserts! (not (get is-deceased status)) ERR_NOT_DECEASED)
    (asserts! (> new-installment-amount u0) ERR_INVALID_SCHEDULE)
    (asserts! (> new-release-interval u0) ERR_INVALID_SCHEDULE)
    
    (map-set distribution-schedules
      { testator: testator, beneficiary: beneficiary }
      (merge distribution {
        installment-amount: new-installment-amount,
        release-interval: new-release-interval,
        next-release-block: (+ current-block new-release-interval)
      })
    )
    
    (ok true)
  )
)

(define-public (cancel-distribution-schedule (beneficiary principal))
  (let (
    (testator tx-sender)
    (distribution (unwrap! (map-get? distribution-schedules { testator: testator, beneficiary: beneficiary }) ERR_DISTRIBUTION_NOT_FOUND))
    (will-data (unwrap! (map-get? wills { testator: testator }) ERR_WILL_NOT_FOUND))
    (status (unwrap! (map-get? testator-status { testator: testator }) ERR_WILL_NOT_FOUND))
    (remaining-amount (get remaining-amount distribution))
  )
    (asserts! (get is-active distribution) ERR_DISTRIBUTION_NOT_FOUND)
    (asserts! (get is-active will-data) ERR_WILL_NOT_FOUND)
    (asserts! (not (get is-deceased status)) ERR_NOT_DECEASED)
    
    (if (> remaining-amount u0)
      (try! (as-contract (stx-transfer? remaining-amount tx-sender testator)))
      true
    )
    
    (map-set distribution-schedules
      { testator: testator, beneficiary: beneficiary }
      (merge distribution { is-active: false, remaining-amount: u0 })
    )
    
    (ok remaining-amount)
  )
)

(define-public (emergency-release-all (testator principal) (beneficiary principal))
  (let (
    (current-block stacks-block-height)
    (distribution (unwrap! (map-get? distribution-schedules { testator: testator, beneficiary: beneficiary }) ERR_DISTRIBUTION_NOT_FOUND))
    (will-data (unwrap! (map-get? wills { testator: testator }) ERR_WILL_NOT_FOUND))
    (status (unwrap! (map-get? testator-status { testator: testator }) ERR_WILL_NOT_FOUND))
    (remaining-amount (get remaining-amount distribution))
    (release-id (var-get next-release-id))
  )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (get is-active distribution) ERR_DISTRIBUTION_NOT_FOUND)
    (asserts! (get is-active will-data) ERR_WILL_NOT_FOUND)
    (asserts! (get is-deceased status) ERR_NOT_DECEASED)
    (asserts! (> remaining-amount u0) ERR_DISTRIBUTION_EXHAUSTED)
    
    (try! (as-contract (stx-transfer? remaining-amount tx-sender beneficiary)))
    
    (map-set distribution-schedules
      { testator: testator, beneficiary: beneficiary }
      (merge distribution { remaining-amount: u0, is-active: false })
    )
    
    (map-set distribution-history
      { testator: testator, beneficiary: beneficiary, release-id: release-id }
      { amount: remaining-amount, released-at: current-block, block-height: current-block }
    )
    
    (var-set next-release-id (+ release-id u1))
    (ok remaining-amount)
  )
)

(define-read-only (get-distribution-schedule (testator principal) (beneficiary principal))
  (map-get? distribution-schedules { testator: testator, beneficiary: beneficiary })
)

(define-read-only (get-distribution-history (testator principal) (beneficiary principal) (release-id uint))
  (map-get? distribution-history { testator: testator, beneficiary: beneficiary, release-id: release-id })
)

(define-read-only (get-beneficiary-conditions (testator principal) (beneficiary principal))
  (map-get? beneficiary-conditions { testator: testator, beneficiary: beneficiary })
)

(define-read-only (get-next-release-block (testator principal) (beneficiary principal))
  (match (map-get? distribution-schedules { testator: testator, beneficiary: beneficiary })
    distribution (some (get next-release-block distribution))
    none
  )
)

(define-read-only (get-remaining-distribution (testator principal) (beneficiary principal))
  (match (map-get? distribution-schedules { testator: testator, beneficiary: beneficiary })
    distribution (some (get remaining-amount distribution))
    none
  )
)

(define-read-only (is-distribution-claimable (testator principal) (beneficiary principal))
  (match (map-get? distribution-schedules { testator: testator, beneficiary: beneficiary })
    distribution 
      (let (
        (current-block stacks-block-height)
        (conditions (map-get? beneficiary-conditions { testator: testator, beneficiary: beneficiary }))
        (conditions-met (match conditions
          condition-data (get conditions-met condition-data)
          true
        ))
      )
        (and 
          (get is-active distribution)
          (> (get remaining-amount distribution) u0)
          (>= current-block (get next-release-block distribution))
          conditions-met
        )
      )
    false
  )
)