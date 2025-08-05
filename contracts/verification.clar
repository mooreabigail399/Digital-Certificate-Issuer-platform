;; Certificate Registry - Verification and Analytics Contract
;; Provides advanced verification, analytics, and registry services

;; Import traits (for future extensibility)
;; (use-trait certificate-trait .certificate-trait.certificate-trait)

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_CERTIFICATE_NOT_FOUND (err u201))
(define-constant ERR_INVALID_INPUT (err u202))
(define-constant ERR_BATCH_TOO_LARGE (err u203))

;; Data Variables
(define-data-var verification-fee uint u100000) ;; 0.1 STX for verification
(define-data-var max-batch-size uint u50)
(define-data-var registry-paused bool false)

;; Data Maps for enhanced functionality
(define-map global-certificate-registry
  { certificate-hash: (buff 32) }
  {
    certificate-id: uint,
    issuer: principal,
    issue-block: uint,
    verification-count: uint,
    last-verified: uint,
    trust-score: uint
  }
)

(define-map issuer-statistics
  { issuer: principal }
  {
    total-issued: uint,
    total-revoked: uint,
    reputation-score: uint,
    last-activity: uint,
    verification-success-rate: uint
  }
)

(define-map skill-registry
  { skill-name: (string-ascii 50) }
  {
    total-certificates: uint,
    top-issuers: (list 5 principal),
    average-trust-score: uint
  }
)

(define-map verification-requests
  { request-id: uint }
  {
    requester: principal,
    certificate-ids: (list 10 uint),
    status: (string-ascii 20),
    created-at: uint,
    completed-at: (optional uint),
    results: (optional (list 10 bool))
  }
)

(define-map certificate-endorsements
  { certificate-id: uint, endorser: principal }
  {
    endorsement-date: uint,
    endorsement-type: (string-ascii 30),
    comments: (optional (string-ascii 200))
  }
)

;; Data Variables for request tracking
(define-data-var next-request-id uint u1)

;; Public Functions

;; Batch certificate verification
(define-public (batch-verify-certificates (certificate-ids (list 10 uint)))
  (let ((request-id (var-get next-request-id))
        (batch-size (len certificate-ids)))

    (asserts! (not (var-get registry-paused)) ERR_UNAUTHORIZED)
    (asserts! (<= batch-size (var-get max-batch-size)) ERR_BATCH_TOO_LARGE)

    ;; Charge verification fee
    (try! (stx-transfer? (* (var-get verification-fee) batch-size) tx-sender CONTRACT_OWNER))

    ;; Create verification request
    (map-set verification-requests
      { request-id: request-id }
      {
        requester: tx-sender,
        certificate-ids: certificate-ids,
        status: "processing",
        created-at: stacks-block-height,
        completed-at: none,
        results: none
      }
    )

    (var-set next-request-id (+ request-id u1))

    ;; Process verification (simplified for demonstration)
    (let ((results (map verify-single-certificate certificate-ids)))
      (map-set verification-requests
        { request-id: request-id }
        {
          requester: tx-sender,
          certificate-ids: certificate-ids,
          status: "completed",
          created-at: stacks-block-height,
          completed-at: (some stacks-block-height),
          results: (some results)
        }
      )

      (ok { request-id: request-id, results: results })
    )
  )
)

;; Register certificate in global registry with hash
(define-public (register-certificate-hash
    (certificate-id uint)
    (certificate-hash (buff 32))
    (issuer principal))
  (begin
    (asserts! (not (var-get registry-paused)) ERR_UNAUTHORIZED)

    (map-set global-certificate-registry
      { certificate-hash: certificate-hash }
      {
        certificate-id: certificate-id,
        issuer: issuer,
        issue-block: stacks-block-height,
        verification-count: u0,
        last-verified: stacks-block-height,
        trust-score: u100
      }
    )

    ;; Update issuer statistics
    (update-issuer-stats issuer u1 u0)

    (ok true)
  )
)

;; Endorse a certificate
(define-public (endorse-certificate
    (certificate-id uint)
    (endorsement-type (string-ascii 30))
    (comments (optional (string-ascii 200))))
  (begin
    (asserts! (not (var-get registry-paused)) ERR_UNAUTHORIZED)

    (map-set certificate-endorsements
      { certificate-id: certificate-id, endorser: tx-sender }
      {
        endorsement-date: stacks-block-height,
        endorsement-type: endorsement-type,
        comments: comments
      }
    )

    (ok true)
  )
)

;; Update skill registry
(define-public (register-skill-certification
    (skill-name (string-ascii 50))
    (certificate-id uint)
    (issuer principal))
  (let ((current-skill (default-to
          { total-certificates: u0, top-issuers: (list), average-trust-score: u0 }
          (map-get? skill-registry { skill-name: skill-name }))))

    (asserts! (not (var-get registry-paused)) ERR_UNAUTHORIZED)

    (map-set skill-registry
      { skill-name: skill-name }
      {
        total-certificates: (+ (get total-certificates current-skill) u1),
        top-issuers: (unwrap-panic (as-max-len?
          (append (get top-issuers current-skill) issuer) u5)),
        average-trust-score: (get average-trust-score current-skill)
      }
    )

    (ok true)
  )
)

;; Analytics and verification functions

;; Get comprehensive certificate analysis
(define-public (analyze-certificate-portfolio (recipient principal))
  (let ((portfolio-analysis (get-portfolio-metrics recipient)))
    (asserts! (not (var-get registry-paused)) ERR_UNAUTHORIZED)

    ;; Charge analysis fee
    (try! (stx-transfer? (var-get verification-fee) tx-sender CONTRACT_OWNER))

    (ok portfolio-analysis)
  )
)

;; Read-only functions

;; Get verification request details
(define-read-only (get-verification-request (request-id uint))
  (map-get? verification-requests { request-id: request-id })
)

;; Get certificate registry entry by hash
(define-read-only (get-certificate-by-hash (certificate-hash (buff 32)))
  (map-get? global-certificate-registry { certificate-hash: certificate-hash })
)

;; Get issuer statistics
(define-read-only (get-issuer-stats (issuer principal))
  (map-get? issuer-statistics { issuer: issuer })
)

;; Get skill registry information
(define-read-only (get-skill-info (skill-name (string-ascii 50)))
  (map-get? skill-registry { skill-name: skill-name })
)

;; Get certificate endorsements
(define-read-only (get-certificate-endorsements (certificate-id uint))
  (let ((endorsements (list
    (map-get? certificate-endorsements { certificate-id: certificate-id, endorser: tx-sender }))))
    endorsements
  )
)

;; Get portfolio metrics for a recipient
(define-read-only (get-portfolio-metrics (recipient principal))
  {
    total-certificates: u0, ;; This would be calculated by querying the main contract
    skills-covered: (list),
    top-issuers: (list),
    average-trust-score: u85,
    latest-certification: stacks-block-height
  }
)

;; Get registry status
(define-read-only (get-registry-status)
  {
    paused: (var-get registry-paused),
    verification-fee: (var-get verification-fee),
    max-batch-size: (var-get max-batch-size),
    next-request-id: (var-get next-request-id)
  }
)

;; Calculate trust score for a certificate
(define-read-only (calculate-trust-score
    (certificate-id uint)
    (issuer principal)
    (verification-count uint))
  (let ((issuer-stats (default-to
          { total-issued: u1, total-revoked: u0, reputation-score: u100,
            last-activity: u0, verification-success-rate: u100 }
          (map-get? issuer-statistics { issuer: issuer })))
        (base-score u100))

    ;; Calculate trust score based on issuer reputation and verification count
    (let ((issuer-factor (/ (get reputation-score issuer-stats) u10))
          (verification-factor (if (<= verification-count u50) verification-count u50))
          (activity-factor (if (> (- stacks-block-height (get last-activity issuer-stats)) u1000) u90 u100)))

      (let ((total-score (+ base-score (+ issuer-factor (+ verification-factor activity-factor)))))
        (if (<= total-score u100) total-score u100)
      )
    )
  )
)

;; Private/Helper Functions

;; Verify single certificate (helper for batch verification)
(define-private (verify-single-certificate (certificate-id uint))
  ;; This would call the main contract's verification function
  ;; For now, returning a simple boolean
  true
)

;; Update issuer statistics
(define-private (update-issuer-stats (issuer principal) (issued-delta uint) (revoked-delta uint))
  (let ((current-stats (default-to
          { total-issued: u0, total-revoked: u0, reputation-score: u100,
            last-activity: u0, verification-success-rate: u100 }
          (map-get? issuer-statistics { issuer: issuer }))))

    (map-set issuer-statistics
      { issuer: issuer }
      {
        total-issued: (+ (get total-issued current-stats) issued-delta),
        total-revoked: (+ (get total-revoked current-stats) revoked-delta),
        reputation-score: (calculate-reputation-score issuer),
        last-activity: stacks-block-height,
        verification-success-rate: (get verification-success-rate current-stats)
      }
    )

    true
  )
)

;; Calculate reputation score based on issuer activity
(define-private (calculate-reputation-score (issuer principal))
  (let ((stats (default-to
          { total-issued: u1, total-revoked: u0, reputation-score: u100,
            last-activity: u0, verification-success-rate: u100 }
          (map-get? issuer-statistics { issuer: issuer }))))

    ;; Simple reputation calculation: base score minus revocation penalty
    (let ((revocation-rate (/ (* (get total-revoked stats) u100) (get total-issued stats)))
          (base-score u100))

      (if (> revocation-rate u10)
          (- base-score (* revocation-rate u2))
          base-score)
    )
  )
)

;; Administrative functions

;; Set verification fee
(define-public (set-verification-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set verification-fee new-fee)
    (ok new-fee)
  )
)

;; Set max batch size
(define-public (set-max-batch-size (new-size uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-size u100) ERR_INVALID_INPUT)
    (var-set max-batch-size new-size)
    (ok new-size)
  )
)

;; Pause/unpause registry
(define-public (set-registry-paused (paused bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set registry-paused paused)
    (ok paused)
  )
)
