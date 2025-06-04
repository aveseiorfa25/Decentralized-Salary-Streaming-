(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-stream-active (err u103))
(define-constant err-stream-inactive (err u104))
(define-constant err-invalid-amount (err u105))

(define-data-var total-streams uint u0)

(define-map salary-streams
  { stream-id: uint }
  {
    employer: principal,
    employee: principal,
    amount-per-second: uint,
    total-amount: uint,
    start-time: uint,
    end-time: uint,
    is-active: bool,
    last-withdrawal: uint,
    withdrawn-amount: uint
  }
)

(define-map employer-deposits
  { employer: principal }
  { balance: uint }
)

(define-read-only (get-stream (stream-id uint))
  (map-get? salary-streams { stream-id: stream-id })
)

(define-read-only (get-employer-balance (employer principal))
  (default-to { balance: u0 }
    (map-get? employer-deposits { employer: employer }))
)

(define-read-only (calculate-available-amount (stream-id uint))
  (let (
    (stream (unwrap! (get-stream stream-id) (err u0)))
    (current-time stacks-block-height)
  )
    (if (get is-active stream)
      (let (
        (elapsed-time (- current-time (get last-withdrawal stream)))
        (available (* elapsed-time (get amount-per-second stream)))
      )
        (ok available))
      (err u0))
  )
)

(define-public (deposit)
  (let ((amount (stx-get-balance tx-sender)))
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set employer-deposits
      { employer: tx-sender }
      { balance: (+ (get balance (get-employer-balance tx-sender)) amount) }
    )
    (ok amount))
)

(define-public (create-stream (employee principal) (amount-per-second uint) (duration uint))
  (let (
    (total-amount (* amount-per-second duration))
    (employer-balance (get balance (get-employer-balance tx-sender)))
    (start-time stacks-block-height)
  )
    (asserts! (>= employer-balance total-amount) err-insufficient-balance)
    (map-set employer-deposits
      { employer: tx-sender }
      { balance: (- employer-balance total-amount) }
    )
    (map-set salary-streams
      { stream-id: (var-get total-streams) }
      {
        employer: tx-sender,
        employee: employee,
        amount-per-second: amount-per-second,
        total-amount: total-amount,
        start-time: start-time,
        end-time: (+ start-time duration),
        is-active: true,
        last-withdrawal: start-time,
        withdrawn-amount: u0
      }
    )
    (var-set total-streams (+ (var-get total-streams) u1))
    (ok (- (var-get total-streams) u1)))
)

(define-public (withdraw (stream-id uint))
  (let (
    (stream (unwrap! (get-stream stream-id) err-not-found))
    (available (unwrap! (calculate-available-amount stream-id) err-stream-inactive))
  )
    (asserts! (is-eq tx-sender (get employee stream)) err-owner-only)
    (asserts! (> available u0) err-invalid-amount)
    (try! (as-contract (stx-transfer? available (as-contract tx-sender) (get employee stream))))
    (map-set salary-streams
      { stream-id: stream-id }
      (merge stream {
        last-withdrawal: stacks-block-height,
        withdrawn-amount: (+ (get withdrawn-amount stream) available)
      })
    )
    (ok available))
)

(define-public (pause-stream (stream-id uint))
  (let ((stream (unwrap! (get-stream stream-id) err-not-found)))
    (asserts! (is-eq tx-sender (get employer stream)) err-owner-only)
    (asserts! (get is-active stream) err-stream-inactive)
    (map-set salary-streams
      { stream-id: stream-id }
      (merge stream { is-active: false })
    )
    (ok true))
)

(define-public (resume-stream (stream-id uint))
  (let ((stream (unwrap! (get-stream stream-id) err-not-found)))
    (asserts! (is-eq tx-sender (get employer stream)) err-owner-only)
    (asserts! (not (get is-active stream)) err-stream-active)
    (map-set salary-streams
      { stream-id: stream-id }
      (merge stream { is-active: true })
    )
    (ok true))
)
