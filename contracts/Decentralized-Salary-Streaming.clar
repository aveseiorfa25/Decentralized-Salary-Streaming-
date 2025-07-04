(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-stream-active (err u103))
(define-constant err-stream-inactive (err u104))
(define-constant err-invalid-amount (err u105))

(define-data-var total-streams uint u0)

(define-constant err-emergency-exists (err u106))
(define-constant err-emergency-not-ready (err u107))
(define-constant err-batch-size-exceeded (err u108))


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

(define-constant emergency-delay u144)

(define-map emergency-requests
  { stream-id: uint }
  {
    requester: principal,
    request-time: uint,
    is-active: bool
  }
)

(define-read-only (get-emergency-request (stream-id uint))
  (map-get? emergency-requests { stream-id: stream-id })
)

(define-read-only (can-execute-emergency (stream-id uint))
  (match (get-emergency-request stream-id)
    request (>= stacks-block-height (+ (get request-time request) emergency-delay))
    false)
)

(define-public (request-emergency-withdrawal (stream-id uint))
  (let ((stream (unwrap! (get-stream stream-id) err-not-found)))
    (asserts! (is-eq tx-sender (get employer stream)) err-owner-only)
    (asserts! (is-none (get-emergency-request stream-id)) (err u106))
    (map-set emergency-requests
      { stream-id: stream-id }
      {
        requester: tx-sender,
        request-time: stacks-block-height,
        is-active: true
      }
    )
    (ok true))
)

(define-public (execute-emergency-withdrawal (stream-id uint))
  (let (
    (stream (unwrap! (get-stream stream-id) err-not-found))
    (request (unwrap! (get-emergency-request stream-id) err-not-found))
  )
    (asserts! (is-eq tx-sender (get employer stream)) err-owner-only)
    (asserts! (get is-active request) err-stream-inactive)
    (asserts! (can-execute-emergency stream-id) (err u107))
    (let (
      (remaining-amount (- (get total-amount stream) (get withdrawn-amount stream)))
    )
      (try! (as-contract (stx-transfer? remaining-amount (as-contract tx-sender) (get employer stream))))
      (map-set salary-streams
        { stream-id: stream-id }
        (merge stream { is-active: false })
      )
      (map-delete emergency-requests { stream-id: stream-id })
      (ok remaining-amount)))
)

(define-public (cancel-emergency-request (stream-id uint))
  (let (
    (stream (unwrap! (get-stream stream-id) err-not-found))
    (request (unwrap! (get-emergency-request stream-id) err-not-found))
  )
    (asserts! (is-eq tx-sender (get employer stream)) err-owner-only)
    (map-delete emergency-requests { stream-id: stream-id })
    (ok true))
)

(define-constant max-batch-size u10)

(define-private (batch-create-single (params { employee: principal, amount-per-second: uint, duration: uint }))
  (create-stream (get employee params) (get amount-per-second params) (get duration params))
)

(define-private (batch-pause-single (stream-id uint))
  (pause-stream stream-id)
)

(define-private (batch-resume-single (stream-id uint))
  (resume-stream stream-id)
)

(define-public (batch-create-streams (stream-params (list 10 { employee: principal, amount-per-second: uint, duration: uint })))
  (let (
    (total-cost (get-batch-cost stream-params))
    (employer-balance (get balance (get-employer-balance tx-sender)))
  )
    (asserts! (<= (len stream-params) max-batch-size) (err u108))
    (asserts! (>= employer-balance total-cost) err-insufficient-balance)
    (ok (map batch-create-single stream-params)))
)

(define-public (batch-pause-streams (stream-ids (list 10 uint)))
  (begin
    (asserts! (<= (len stream-ids) max-batch-size) (err u108))
    (ok (map batch-pause-single stream-ids)))
)

(define-public (batch-resume-streams (stream-ids (list 10 uint)))
  (begin
    (asserts! (<= (len stream-ids) max-batch-size) (err u108))
    (ok (map batch-resume-single stream-ids)))
)

(define-private (calculate-stream-cost (params { employee: principal, amount-per-second: uint, duration: uint }))
  (* (get amount-per-second params) (get duration params))
)

(define-read-only (get-batch-cost (stream-params (list 10 { employee: principal, amount-per-second: uint, duration: uint })))
  (fold + (map calculate-stream-cost stream-params) u0)
)

(define-private (is-employer-stream (employer principal) (stream-id uint))
  (match (get-stream stream-id)
    stream (is-eq (get employer stream) employer)
    false)
)

(define-private (is-employee-stream (employee principal) (stream-id uint))
  (match (get-stream stream-id)
    stream (is-eq (get employee stream) employee)
    false)
)

(define-read-only (get-streams-by-employer (employer principal))
  (let (
    (all-streams (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9))
    (result (fold check-employer-stream-simple all-streams { employer: employer, result: (list) }))
  )
    (get result result))
)

(define-read-only (get-streams-by-employee (employee principal))
  (let (
    (all-streams (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9))
    (result (fold check-employee-stream-simple all-streams { employee: employee, result: (list) }))
  )
    (get result result))
)

(define-private (check-employer-stream-simple (stream-id uint) (acc { employer: principal, result: (list 10 uint) }))
  (if (< stream-id (var-get total-streams))
    (match (get-stream stream-id)
      stream (if (is-eq (get employer stream) (get employer acc))
               { employer: (get employer acc), result: (unwrap-panic (as-max-len? (append (get result acc) stream-id) u10)) }
               acc)
      acc)
    acc)
)

(define-private (check-employee-stream-simple (stream-id uint) (acc { employee: principal, result: (list 10 uint) }))
  (if (< stream-id (var-get total-streams))
    (match (get-stream stream-id)
      stream (if (is-eq (get employee stream) (get employee acc))
               { employee: (get employee acc), result: (unwrap-panic (as-max-len? (append (get result acc) stream-id) u10)) }
               acc)
      acc)
    acc)
)


