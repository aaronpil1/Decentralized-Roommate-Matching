;; Decentralized Roommate Matching Smart Contract
;; A trustless platform for finding compatible roommates

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-input (err u104))
(define-constant err-insufficient-funds (err u105))
(define-constant err-match-not-found (err u106))
(define-constant err-already-matched (err u107))

;; Data Variables
(define-data-var next-user-id uint u1)
(define-data-var next-match-id uint u1)
(define-data-var platform-fee uint u1000000) ;; 1 STX in microSTX
(define-data-var contract-balance uint u0)

;; Data Maps
(define-map users
  { user-id: uint }
  {
    owner: principal,
    name: (string-ascii 50),
    age: uint,
    location: (string-ascii 100),
    budget-min: uint,
    budget-max: uint,
    preferences: (string-ascii 200),
    contact-info: (string-ascii 100),
    is-active: bool,
    reputation-score: uint,
    created-at: uint
  }
)

(define-map user-principals
  { owner: principal }
  { user-id: uint }
)

(define-map matches
  { match-id: uint }
  {
    user1-id: uint,
    user2-id: uint,
    compatibility-score: uint,
    status: (string-ascii 20), ;; "pending", "accepted", "rejected", "completed"
    created-at: uint,
    accepted-at: (optional uint),
    completed-at: (optional uint)
  }
)

(define-map match-requests
  { requester-id: uint, requested-id: uint }
  {
    match-id: uint,
    message: (string-ascii 500),
    created-at: uint
  }
)

(define-map user-ratings
  { rater-id: uint, rated-id: uint }
  {
    rating: uint, ;; 1-5 scale
    feedback: (string-ascii 300),
    created-at: uint
  }
)

;; Read-only functions
(define-read-only (get-user (user-id uint))
  (map-get? users { user-id: user-id })
)

(define-read-only (get-user-by-principal (owner principal))
  (match (map-get? user-principals { owner: owner })
    user-data (map-get? users { user-id: (get user-id user-data) })
    none
  )
)

(define-read-only (get-match (match-id uint))
  (map-get? matches { match-id: match-id })
)

(define-read-only (get-match-request (requester-id uint) (requested-id uint))
  (map-get? match-requests { requester-id: requester-id, requested-id: requested-id })
)

(define-read-only (get-user-rating (rater-id uint) (rated-id uint))
  (map-get? user-ratings { rater-id: rater-id, rated-id: rated-id })
)

(define-read-only (get-platform-fee)
  (var-get platform-fee)
)

(define-read-only (calculate-compatibility-score (user1-id uint) (user2-id uint))
  (let (
    (user1 (unwrap! (get-user user1-id) u0))
    (user2 (unwrap! (get-user user2-id) u0))
    (budget-overlap (calculate-budget-overlap 
      (get budget-min user1) (get budget-max user1)
      (get budget-min user2) (get budget-max user2)))
    (age-compatibility (calculate-age-compatibility (get age user1) (get age user2)))
    (location-match (if (is-eq (get location user1) (get location user2)) u30 u0))
  )
    (+ budget-overlap age-compatibility location-match)
  )
)

(define-read-only (calculate-budget-overlap (min1 uint) (max1 uint) (min2 uint) (max2 uint))
  (let (
    (overlap-min (if (> min1 min2) min1 min2))
    (overlap-max (if (< max1 max2) max1 max2))
  )
    (if (> overlap-min overlap-max)
      u0
      u40) ;; 40 points for budget compatibility
  )
)

(define-read-only (calculate-age-compatibility (age1 uint) (age2 uint))
  (let (
    (age-diff (if (> age1 age2) (- age1 age2) (- age2 age1)))
  )
    (if (<= age-diff u5)
      u30
      (if (<= age-diff u10)
        u20
        u10))
  )
)

;; Public functions
(define-public (register-user 
  (name (string-ascii 50))
  (age uint)
  (location (string-ascii 100))
  (budget-min uint)
  (budget-max uint)
  (preferences (string-ascii 200))
  (contact-info (string-ascii 100))
)
  (let (
    (user-id (var-get next-user-id))
    (current-block-height block-height)
  )
    ;; Check if user already exists
    (asserts! (is-none (map-get? user-principals { owner: tx-sender })) err-already-exists)
    ;; Validate input
    (asserts! (and (> age u17) (< age u100)) err-invalid-input)
    (asserts! (< budget-min budget-max) err-invalid-input)
    (asserts! (> (len name) u0) err-invalid-input)

    ;; Pay registration fee
    (try! (stx-transfer? (var-get platform-fee) tx-sender (as-contract tx-sender)))
    (var-set contract-balance (+ (var-get contract-balance) (var-get platform-fee)))

    ;; Create user record
    (map-set users
      { user-id: user-id }
      {
        owner: tx-sender,
        name: name,
        age: age,
        location: location,
        budget-min: budget-min,
        budget-max: budget-max,
        preferences: preferences,
        contact-info: contact-info,
        is-active: true,
        reputation-score: u100, ;; Starting reputation
        created-at: current-block-height
      }
    )

    ;; Map principal to user-id
    (map-set user-principals { owner: tx-sender } { user-id: user-id })
    
    ;; Increment user counter
    (var-set next-user-id (+ user-id u1))
    
    (ok user-id)
  )
)

(define-public (update-user-profile
  (name (string-ascii 50))
  (age uint)
  (location (string-ascii 100))
  (budget-min uint)
  (budget-max uint)
  (preferences (string-ascii 200))
  (contact-info (string-ascii 100))
)
  (let (
    (user-data (unwrap! (map-get? user-principals { owner: tx-sender }) err-not-found))
    (user-id (get user-id user-data))
    (current-user (unwrap! (get-user user-id) err-not-found))
  )
    ;; Validate input
    (asserts! (and (> age u17) (< age u100)) err-invalid-input)
    (asserts! (< budget-min budget-max) err-invalid-input)
    (asserts! (> (len name) u0) err-invalid-input)

    ;; Update user record
    (map-set users
      { user-id: user-id }
      (merge current-user {
        name: name,
        age: age,
        location: location,
        budget-min: budget-min,
        budget-max: budget-max,
        preferences: preferences,
        contact-info: contact-info
      })
    )
    
    (ok true)
  )
)

(define-public (deactivate-profile)
  (let (
    (user-data (unwrap! (map-get? user-principals { owner: tx-sender }) err-not-found))
    (user-id (get user-id user-data))
    (current-user (unwrap! (get-user user-id) err-not-found))
  )
    (map-set users
      { user-id: user-id }
      (merge current-user { is-active: false })
    )
    (ok true)
  )
)

(define-public (send-match-request
  (requested-user-id uint)
  (message (string-ascii 500))
)
  (let (
    (requester-data (unwrap! (map-get? user-principals { owner: tx-sender }) err-not-found))
    (requester-id (get user-id requester-data))
    (requested-user (unwrap! (get-user requested-user-id) err-not-found))
    (match-id (var-get next-match-id))
    (compatibility-score (calculate-compatibility-score requester-id requested-user-id))
    (current-block-height block-height)
  )
    ;; Validate that requested user exists and is active
    (asserts! (get is-active requested-user) err-not-found)
    ;; Check if match request already exists
    (asserts! (is-none (get-match-request requester-id requested-user-id)) err-already-exists)
    ;; Cannot request match with self
    (asserts! (not (is-eq requester-id requested-user-id)) err-invalid-input)

    ;; Create match record
    (map-set matches
      { match-id: match-id }
      {
        user1-id: requester-id,
        user2-id: requested-user-id,
        compatibility-score: compatibility-score,
        status: "pending",
        created-at: current-block-height,
        accepted-at: none,
        completed-at: none
      }
    )

    ;; Create match request
    (map-set match-requests
      { requester-id: requester-id, requested-id: requested-user-id }
      {
        match-id: match-id,
        message: message,
        created-at: current-block-height
      }
    )

    (var-set next-match-id (+ match-id u1))
    (ok match-id)
  )
)

(define-public (respond-to-match-request (requester-id uint) (accept bool))
  (let (
    (responder-data (unwrap! (map-get? user-principals { owner: tx-sender }) err-not-found))
    (responder-id (get user-id responder-data))
    (match-request (unwrap! (get-match-request requester-id responder-id) err-not-found))
    (match-id (get match-id match-request))
    (current-match (unwrap! (get-match match-id) err-not-found))
    (current-block-height block-height)
  )
    ;; Update match status
    (map-set matches
      { match-id: match-id }
      (merge current-match {
        status: (if accept "accepted" "rejected"),
        accepted-at: (if accept (some current-block-height) none)
      })
    )
    
    (ok true)
  )
)

(define-public (complete-match (match-id uint))
  (let (
    (user-data (unwrap! (map-get? user-principals { owner: tx-sender }) err-not-found))
    (user-id (get user-id user-data))
    (current-match (unwrap! (get-match match-id) err-match-not-found))
    (current-block-height block-height)
  )
    ;; Verify user is part of this match
    (asserts! (or (is-eq user-id (get user1-id current-match))
                  (is-eq user-id (get user2-id current-match))) err-unauthorized)
    ;; Verify match is accepted
    (asserts! (is-eq (get status current-match) "accepted") err-unauthorized)

    ;; Update match status
    (map-set matches
      { match-id: match-id }
      (merge current-match {
        status: "completed",
        completed-at: (some current-block-height)
      })
    )
    
    (ok true)
  )
)

(define-public (rate-user (rated-user-id uint) (rating uint) (feedback (string-ascii 300)))
  (let (
    (rater-data (unwrap! (map-get? user-principals { owner: tx-sender }) err-not-found))
    (rater-id (get user-id rater-data))
    (rated-user (unwrap! (get-user rated-user-id) err-not-found))
    (current-block-height block-height)
  )
    ;; Validate rating scale
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-input)
    ;; Cannot rate self
    (asserts! (not (is-eq rater-id rated-user-id)) err-invalid-input)
    ;; Check if already rated
    (asserts! (is-none (get-user-rating rater-id rated-user-id)) err-already-exists)

    ;; Create rating record
    (map-set user-ratings
      { rater-id: rater-id, rated-id: rated-user-id }
      {
        rating: rating,
        feedback: feedback,
        created-at: current-block-height
      }
    )

    ;; Update rated user's reputation score (simplified calculation)
    (let (
      (current-reputation (get reputation-score rated-user))
      (new-reputation (/ (+ (* current-reputation u9) (* rating u20)) u10))
    )
      (map-set users
        { user-id: rated-user-id }
        (merge rated-user { reputation-score: new-reputation })
      )
    )
    
    (ok true)
  )
)

;; Admin functions
(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set platform-fee new-fee)
    (ok true)
  )
)

(define-public (withdraw-fees (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= amount (var-get contract-balance)) err-insufficient-funds)
    (try! (as-contract (stx-transfer? amount tx-sender contract-owner)))
    (var-set contract-balance (- (var-get contract-balance) amount))
    (ok true)
  )
)