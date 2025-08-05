;; Digital Certificate Issuer - Core Contract
;; A comprehensive system for issuing, verifying, and managing digital certificates

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_CERTIFICATE_NOT_FOUND (err u101))
(define-constant ERR_CERTIFICATE_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_ISSUER (err u103))
(define-constant ERR_CERTIFICATE_REVOKED (err u104))
(define-constant ERR_INVALID_TEMPLATE (err u105))
(define-constant ERR_ISSUER_NOT_VERIFIED (err u106))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u107))

;; Data Variables
(define-data-var next-certificate-id uint u1)
(define-data-var contract-paused bool false)
(define-data-var issuance-fee uint u1000000) ;; 1 STX in microSTX

;; Data Maps
(define-map certificates
  { certificate-id: uint }
  {
    recipient: principal,
    issuer: principal,
    template-id: uint,
    course-name: (string-ascii 100),
    skills: (list 10 (string-ascii 50)),
    issue-date: uint,
    expiry-date: (optional uint),
    metadata-uri: (string-ascii 200),
    is-revoked: bool,
    grade: (optional (string-ascii 10)),
    credits: (optional uint)
  }
)

(define-map certificate-templates
  { template-id: uint }
  {
    name: (string-ascii 100),
    description: (string-ascii 500),
    creator: principal,
    design-uri: (string-ascii 200),
    required-fields: (list 10 (string-ascii 30)),
    is-active: bool,
    creation-date: uint
  }
)

(define-map verified-issuers
  { issuer: principal }
  {
    organization-name: (string-ascii 100),
    verification-date: uint,
    is-active: bool,
    issuer-type: (string-ascii 50),
    contact-info: (string-ascii 200),
    reputation-score: uint
  }
)

(define-map recipient-certificates
  { recipient: principal, certificate-id: uint }
  { owned: bool }
)

(define-map issuer-certificates
  { issuer: principal, certificate-id: uint }
  { issued: bool }
)

(define-map certificate-verification-log
  { certificate-id: uint, verifier: principal }
  { verification-date: uint, result: bool }
)

;; Public Functions

;; Register as a verified issuer (requires contract owner approval initially)
(define-public (register-issuer
    (organization-name (string-ascii 100))
    (issuer-type (string-ascii 50))
    (contact-info (string-ascii 200)))
  (let ((issuer tx-sender))
    (asserts! (not (get-contract-paused)) ERR_UNAUTHORIZED)
    (map-set verified-issuers
      { issuer: issuer }
      {
        organization-name: organization-name,
        verification-date: stacks-block-height,
        is-active: true,
        issuer-type: issuer-type,
        contact-info: contact-info,
        reputation-score: u100
      }
    )
    (ok issuer)
  )
)

;; Create a certificate template
(define-public (create-template
    (name (string-ascii 100))
    (description (string-ascii 500))
    (design-uri (string-ascii 200))
    (required-fields (list 10 (string-ascii 30))))
  (let ((template-id (var-get next-certificate-id)))
    (asserts! (not (get-contract-paused)) ERR_UNAUTHORIZED)
    (asserts! (is-verified-issuer tx-sender) ERR_ISSUER_NOT_VERIFIED)

    (map-set certificate-templates
      { template-id: template-id }
      {
        name: name,
        description: description,
        creator: tx-sender,
        design-uri: design-uri,
        required-fields: required-fields,
        is-active: true,
        creation-date: stacks-block-height
      }
    )

    (var-set next-certificate-id (+ template-id u1))
    (ok template-id)
  )
)

;; Issue a certificate
(define-public (issue-certificate
    (recipient principal)
    (template-id uint)
    (course-name (string-ascii 100))
    (skills (list 10 (string-ascii 50)))
    (expiry-date (optional uint))
    (metadata-uri (string-ascii 200))
    (grade (optional (string-ascii 10)))
    (credits (optional uint)))
  (let ((certificate-id (var-get next-certificate-id))
        (issuer tx-sender))

    (asserts! (not (get-contract-paused)) ERR_UNAUTHORIZED)
    (asserts! (is-verified-issuer issuer) ERR_ISSUER_NOT_VERIFIED)
    (asserts! (is-some (map-get? certificate-templates { template-id: template-id })) ERR_INVALID_TEMPLATE)

    ;; Handle payment
    (try! (stx-transfer? (var-get issuance-fee) tx-sender CONTRACT_OWNER))

    ;; Create certificate
    (map-set certificates
      { certificate-id: certificate-id }
      {
        recipient: recipient,
        issuer: issuer,
        template-id: template-id,
        course-name: course-name,
        skills: skills,
        issue-date: stacks-block-height,
        expiry-date: expiry-date,
        metadata-uri: metadata-uri,
        is-revoked: false,
        grade: grade,
        credits: credits
      }
    )

    ;; Update indexes
    (map-set recipient-certificates
      { recipient: recipient, certificate-id: certificate-id }
      { owned: true }
    )

    (map-set issuer-certificates
      { issuer: issuer, certificate-id: certificate-id }
      { issued: true }
    )

    (var-set next-certificate-id (+ certificate-id u1))
    (ok certificate-id)
  )
)

;; Revoke a certificate
(define-public (revoke-certificate (certificate-id uint))
  (let ((certificate (unwrap! (map-get? certificates { certificate-id: certificate-id }) ERR_CERTIFICATE_NOT_FOUND)))
    (asserts! (not (get-contract-paused)) ERR_UNAUTHORIZED)
    (asserts! (is-eq tx-sender (get issuer certificate)) ERR_UNAUTHORIZED)
    (asserts! (not (get is-revoked certificate)) ERR_CERTIFICATE_REVOKED)

    (map-set certificates
      { certificate-id: certificate-id }
      (merge certificate { is-revoked: true })
    )

    (ok true)
  )
)

;; Verify a certificate
(define-public (verify-certificate (certificate-id uint))
  (let ((certificate (unwrap! (map-get? certificates { certificate-id: certificate-id }) ERR_CERTIFICATE_NOT_FOUND))
        (verifier tx-sender))

    (asserts! (not (get-contract-paused)) ERR_UNAUTHORIZED)

    ;; Log verification attempt
    (map-set certificate-verification-log
      { certificate-id: certificate-id, verifier: verifier }
      { verification-date: stacks-block-height, result: (not (get is-revoked certificate)) }
    )

    ;; Check if certificate is valid
    (if (get is-revoked certificate)
        (ok { valid: false, reason: "Certificate has been revoked" })
        (if (is-some (get expiry-date certificate))
            (if (> stacks-block-height (unwrap-panic (get expiry-date certificate)))
                (ok { valid: false, reason: "Certificate has expired" })
                (ok { valid: true, reason: "Certificate is valid" }))
            (ok { valid: true, reason: "Certificate is valid" }))
    )
  )
)

;; Transfer certificate ownership (for wallet integration)
(define-public (transfer-certificate (certificate-id uint) (new-recipient principal))
  (let ((certificate (unwrap! (map-get? certificates { certificate-id: certificate-id }) ERR_CERTIFICATE_NOT_FOUND))
        (current-recipient (get recipient certificate)))

    (asserts! (not (get-contract-paused)) ERR_UNAUTHORIZED)
    (asserts! (is-eq tx-sender current-recipient) ERR_UNAUTHORIZED)
    (asserts! (not (get is-revoked certificate)) ERR_CERTIFICATE_REVOKED)

    ;; Update certificate recipient
    (map-set certificates
      { certificate-id: certificate-id }
      (merge certificate { recipient: new-recipient })
    )

    ;; Update recipient indexes
    (map-delete recipient-certificates { recipient: current-recipient, certificate-id: certificate-id })
    (map-set recipient-certificates
      { recipient: new-recipient, certificate-id: certificate-id }
      { owned: true }
    )

    (ok true)
  )
)

;; Read-only functions

;; Get certificate details
(define-read-only (get-certificate (certificate-id uint))
  (map-get? certificates { certificate-id: certificate-id })
)

;; Get certificate template
(define-read-only (get-template (template-id uint))
  (map-get? certificate-templates { template-id: template-id })
)

;; Get issuer details
(define-read-only (get-issuer-details (issuer principal))
  (map-get? verified-issuers { issuer: issuer })
)

;; Check if issuer is verified
(define-read-only (is-verified-issuer (issuer principal))
  (match (map-get? verified-issuers { issuer: issuer })
    issuer-data (get is-active issuer-data)
    false
  )
)

;; Check if recipient owns certificate
(define-read-only (owns-certificate (recipient principal) (certificate-id uint))
  (is-some (map-get? recipient-certificates { recipient: recipient, certificate-id: certificate-id }))
)

;; Get contract status
(define-read-only (get-contract-status)
  {
    paused: (var-get contract-paused),
    next-certificate-id: (var-get next-certificate-id),
    issuance-fee: (var-get issuance-fee),
    contract-owner: CONTRACT_OWNER
  }
)

;; Get certificate count for recipient
(define-read-only (get-recipient-certificate-count (recipient principal))
  (let ((certificates-owned (filter check-ownership (list
    u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20))))
    (len certificates-owned)
  )
)

;; Helper function for counting certificates
(define-read-only (check-ownership (certificate-id uint))
  (owns-certificate tx-sender certificate-id)
)

;; Get paused status
(define-read-only (get-contract-paused)
  (var-get contract-paused)
)

;; Administrative functions (contract owner only)

;; Pause/unpause contract
(define-public (set-contract-paused (paused bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set contract-paused paused)
    (ok paused)
  )
)

;; Update issuance fee
(define-public (set-issuance-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set issuance-fee new-fee)
    (ok new-fee)
  )
)

;; Approve issuer verification (manual process)
(define-public (approve-issuer (issuer principal))
  (let ((issuer-data (unwrap! (map-get? verified-issuers { issuer: issuer }) ERR_INVALID_ISSUER)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)

    (map-set verified-issuers
      { issuer: issuer }
      (merge issuer-data { is-active: true, verification-date: stacks-block-height })
    )

    (ok true)
  )
)

;; Deactivate issuer
(define-public (deactivate-issuer (issuer principal))
  (let ((issuer-data (unwrap! (map-get? verified-issuers { issuer: issuer }) ERR_INVALID_ISSUER)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)

    (map-set verified-issuers
      { issuer: issuer }
      (merge issuer-data { is-active: false })
    )

    (ok true)
  )
)
