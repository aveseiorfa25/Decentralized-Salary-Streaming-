(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-stream-active (err u103))
(define-constant err-stream-inactive (err u104))
(define-constant err-invalid-amount (err u105))

(define-data-var total-streams uint u0)

(define-private (min (a uint) (b uint))
  (if (<= a b) a b)
)

(define-constant err-emergency-exists (err u106))
(define-constant err-emergency-not-ready (err u107))
(define-constant err-batch-size-exceeded (err u108))
(define-constant err-cliff-not-reached (err u109))
(define-constant err-vesting-schedule-exists (err u110))
(define-constant err-stream-already-ended (err u111))
(define-constant err-insufficient-time-elapsed (err u112))


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

(define-map vesting-schedules
  { stream-id: uint }
  {
    cliff-period: uint,
    vesting-period: uint,
    vested-percentage: uint
  }
)

(define-read-only (get-stream (stream-id uint))
  (map-get? salary-streams { stream-id: stream-id })
)

(define-read-only (get-employer-balance (employer principal))
  (default-to { balance: u0 }
    (map-get? employer-deposits { employer: employer }))
)

(define-read-only (get-vesting-schedule (stream-id uint))
  (map-get? vesting-schedules { stream-id: stream-id })
)

(define-read-only (calculate-available-amount (stream-id uint))
  (let (
    (stream (unwrap! (get-stream stream-id) (err u0)))
    (current-time stacks-block-height)
  )
    (if (get is-active stream)
      (match (get-vesting-schedule stream-id)
        vesting (let (
          (time-since-start (- current-time (get start-time stream)))
          (cliff-passed (>= time-since-start (get cliff-period vesting)))
        )
          (if cliff-passed
            (let (
              (vesting-time (- time-since-start (get cliff-period vesting)))
              (vesting-ratio (min (/ (* vesting-time u100) (get vesting-period vesting)) u100))
              (vested-amount (/ (* (get total-amount stream) (get vested-percentage vesting) vesting-ratio) u10000))
              (available-from-vesting (- vested-amount (get withdrawn-amount stream)))
              (elapsed-time (- current-time (get last-withdrawal stream)))
              (regular-available (* elapsed-time (get amount-per-second stream)))
              (available (min available-from-vesting regular-available))
            )
              (ok available))
            (err u0)))
        (let (
          (elapsed-time (- current-time (get last-withdrawal stream)))
          (available (* elapsed-time (get amount-per-second stream)))
        )
          (ok available)))
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

(define-read-only (calculate-vested-amount (stream-id uint))
  (match (get-vesting-schedule stream-id)
    vesting (let (
      (stream (unwrap! (get-stream stream-id) (err u0)))
      (time-since-start (- stacks-block-height (get start-time stream)))
    )
      (if (>= time-since-start (get cliff-period vesting))
        (let (
          (vesting-time (- time-since-start (get cliff-period vesting)))
          (vesting-ratio (min (/ (* vesting-time u100) (get vesting-period vesting)) u100))
          (vested-amount (/ (* (get total-amount stream) (get vested-percentage vesting) vesting-ratio) u10000))
        )
          (ok vested-amount))
        (ok u0)))
    (ok u0))
)

(define-public (create-vesting-stream (employee principal) (amount-per-second uint) (duration uint) (cliff-period uint) (vesting-period uint) (vested-percentage uint))
  (let (
    (stream-id (unwrap! (create-stream employee amount-per-second duration) err-invalid-amount))
  )
    (asserts! (is-none (get-vesting-schedule stream-id)) err-vesting-schedule-exists)
    (asserts! (<= vested-percentage u100) err-invalid-amount)
    (map-set vesting-schedules
      { stream-id: stream-id }
      {
        cliff-period: cliff-period,
        vesting-period: vesting-period,
        vested-percentage: vested-percentage
      }
    )
    (ok stream-id))
)

(define-public (set-vesting-schedule (stream-id uint) (cliff-period uint) (vesting-period uint) (vested-percentage uint))
  (let ((stream (unwrap! (get-stream stream-id) err-not-found)))
    (asserts! (is-eq tx-sender (get employer stream)) err-owner-only)
    (asserts! (is-none (get-vesting-schedule stream-id)) err-vesting-schedule-exists)
    (asserts! (<= vested-percentage u100) err-invalid-amount)
    (map-set vesting-schedules
      { stream-id: stream-id }
      {
        cliff-period: cliff-period,
        vesting-period: vesting-period,
        vested-percentage: vested-percentage
      }
    )
    (ok true))
)

(define-read-only (calculate-earned-amount (stream-id uint))
  (let (
    (stream (unwrap! (get-stream stream-id) (err u0)))
    (current-time stacks-block-height)
    (elapsed-time (- current-time (get start-time stream)))
  )
    (if (>= current-time (get end-time stream))
      (ok (get total-amount stream))
      (ok (* elapsed-time (get amount-per-second stream))))
  )
)

(define-public (cancel-stream (stream-id uint))
  (let (
    (stream (unwrap! (get-stream stream-id) err-not-found))
    (current-time stacks-block-height)
    (earned-amount (unwrap! (calculate-earned-amount stream-id) err-invalid-amount))
    (refund-amount (- (get total-amount stream) earned-amount))
    (employee-payment (- earned-amount (get withdrawn-amount stream)))
  )
    (asserts! (is-eq tx-sender (get employer stream)) err-owner-only)
    (asserts! (get is-active stream) err-stream-inactive)
    (asserts! (< current-time (get end-time stream)) err-stream-already-ended)
    (asserts! (> (- current-time (get start-time stream)) u10) err-insufficient-time-elapsed)
    
    (if (> employee-payment u0)
      (try! (as-contract (stx-transfer? employee-payment (as-contract tx-sender) (get employee stream))))
      true)
    
    (if (> refund-amount u0)
      (begin
        (map-set employer-deposits
          { employer: (get employer stream) }
          { balance: (+ (get balance (get-employer-balance (get employer stream))) refund-amount) }
        )
        true)
      true)
    
    (map-set salary-streams
      { stream-id: stream-id }
      (merge stream {
        is-active: false,
        end-time: current-time,
        withdrawn-amount: (+ (get withdrawn-amount stream) employee-payment)
      })
    )
    
    (ok { employee-payment: employee-payment, refund-amount: refund-amount }))
)

;; === STREAM ANALYTICS AND REPORTING SYSTEM ===
;; Independent feature for tracking stream statistics and generating reports

;; Error constants for analytics feature
(define-constant err-analytics-not-found (err u200))
(define-constant err-invalid-date-range (err u201))
(define-constant err-analytics-access-denied (err u202))
(define-constant err-report-generation-failed (err u203))

;; Data variables for analytics
(define-data-var total-volume-streamed uint u0)
(define-data-var total-completed-streams uint u0)
(define-data-var total-cancelled-streams uint u0)
(define-data-var analytics-enabled bool true)

;; Stream analytics data structure
(define-map stream-analytics
  { stream-id: uint }
  {
    creation-block: uint,
    first-withdrawal-block: (optional uint),
    completion-block: (optional uint),
    total-withdrawals: uint,
    efficiency-score: uint,
    status: (string-ascii 20)
  }
)

;; Global analytics aggregation
(define-map daily-analytics
  { date: uint }
  {
    streams-created: uint,
    streams-completed: uint,
    streams-cancelled: uint,
    volume-streamed: uint,
    active-employers: uint,
    active-employees: uint
  }
)

;; User analytics tracking
(define-map user-analytics
  { user: principal }
  {
    streams-as-employer: uint,
    streams-as-employee: uint,
    total-paid: uint,
    total-received: uint,
    avg-stream-duration: uint,
    reliability-score: uint
  }
)

;; Performance metrics
(define-map performance-metrics
  { metric-type: (string-ascii 30) }
  {
    value: uint,
    last-updated: uint,
    trend: (string-ascii 10)
  }
)

;; === ANALYTICS CORE FUNCTIONS ===

(define-read-only (get-stream-analytics (stream-id uint))
  (map-get? stream-analytics { stream-id: stream-id })
)

(define-read-only (get-user-analytics (user principal))
  (default-to {
    streams-as-employer: u0,
    streams-as-employee: u0,
    total-paid: u0,
    total-received: u0,
    avg-stream-duration: u0,
    reliability-score: u100
  } (map-get? user-analytics { user: user }))
)

(define-read-only (get-daily-analytics (date uint))
  (map-get? daily-analytics { date: date })
)

(define-read-only (get-performance-metric (metric-type (string-ascii 30)))
  (map-get? performance-metrics { metric-type: metric-type })
)

;; Calculate stream efficiency score (0-100)
(define-read-only (calculate-efficiency-score (stream-id uint))
  (match (get-stream stream-id)
    stream (let (
      (total-duration (- (get end-time stream) (get start-time stream)))
      (withdrawn-ratio (if (> (get total-amount stream) u0)
                        (/ (* (get withdrawn-amount stream) u100) (get total-amount stream))
                        u0))
      (time-efficiency (if (and (get is-active stream) (> total-duration u0))
                        (/ (* (- stacks-block-height (get start-time stream)) u100) total-duration)
                        u100))
      (combined-score (/ (+ withdrawn-ratio time-efficiency) u2))
    )
      (ok (min combined-score u100)))
    (err u0))
)

;; Generate stream status based on current state
(define-read-only (get-stream-status (stream-id uint))
  (match (get-stream stream-id)
    stream (let (
      (current-time stacks-block-height)
      (is-completed (>= current-time (get end-time stream)))
      (has-emergency (is-some (get-emergency-request stream-id)))
    )
      (if (not (get is-active stream))
        (if is-completed "completed" "cancelled")
        (if has-emergency "emergency" "active")))
    "not-found")
)

;; Calculate user reliability score based on stream history
(define-read-only (calculate-user-reliability (user principal))
  (let (
    (user-data (get-user-analytics user))
    (user-total-streams (+ (get streams-as-employer user-data) (get streams-as-employee user-data)))
  )
    (if (> user-total-streams u0)
      (let (
        (completion-rate (/ (* (get streams-as-employee user-data) u100) user-total-streams))
        (payment-consistency (if (> (get streams-as-employer user-data) u0) u100 u80))
        (reliability (/ (+ completion-rate payment-consistency) u2))
      )
        (ok (min reliability u100)))
      (ok u100)))
)

;; === ANALYTICS UPDATE FUNCTIONS ===

(define-public (initialize-stream-analytics (stream-id uint))
  (let (
    (stream (unwrap! (get-stream stream-id) err-not-found))
    (current-block stacks-block-height)
  )
    (asserts! (var-get analytics-enabled) (ok false))
    (map-set stream-analytics
      { stream-id: stream-id }
      {
        creation-block: current-block,
        first-withdrawal-block: none,
        completion-block: none,
        total-withdrawals: u0,
        efficiency-score: u100,
        status: "created"
      }
    )
    (update-daily-analytics-create current-block)
    (update-user-analytics-create (get employer stream) (get employee stream) (get total-amount stream))
    (ok true))
)

(define-public (update-withdrawal-analytics (stream-id uint))
  (let (
    (analytics (unwrap! (get-stream-analytics stream-id) err-analytics-not-found))
    (current-block stacks-block-height)
    (new-first-withdrawal (if (is-none (get first-withdrawal-block analytics))
                           (some current-block)
                           (get first-withdrawal-block analytics)))
  )
    (asserts! (var-get analytics-enabled) (ok false))
    (map-set stream-analytics
      { stream-id: stream-id }
      (merge analytics {
        first-withdrawal-block: new-first-withdrawal,
        total-withdrawals: (+ (get total-withdrawals analytics) u1),
        status: "active"
      })
    )
    (ok true))
)

(define-public (finalize-stream-analytics (stream-id uint) (completion-type (string-ascii 20)))
  (let (
    (analytics (unwrap! (get-stream-analytics stream-id) err-analytics-not-found))
    (stream (unwrap! (get-stream stream-id) err-not-found))
    (current-block stacks-block-height)
    (efficiency (unwrap-panic (calculate-efficiency-score stream-id)))
  )
    (asserts! (var-get analytics-enabled) (ok false))
    (map-set stream-analytics
      { stream-id: stream-id }
      (merge analytics {
        completion-block: (some current-block),
        efficiency-score: efficiency,
        status: completion-type
      })
    )
    (update-global-counters completion-type (get withdrawn-amount stream))
    (ok true))
)

;; === HELPER FUNCTIONS FOR ANALYTICS ===

(define-private (update-daily-analytics-create (date uint))
  (let (
    (existing (default-to {
      streams-created: u0,
      streams-completed: u0,
      streams-cancelled: u0,
      volume-streamed: u0,
      active-employers: u0,
      active-employees: u0
    } (get-daily-analytics date)))
  )
    (map-set daily-analytics
      { date: date }
      (merge existing {
        streams-created: (+ (get streams-created existing) u1)
      })
    ))
)

(define-private (update-user-analytics-create (employer principal) (employee principal) (amount uint))
  (let (
    (employer-data (get-user-analytics employer))
    (employee-data (get-user-analytics employee))
  )
    (map-set user-analytics
      { user: employer }
      (merge employer-data {
        streams-as-employer: (+ (get streams-as-employer employer-data) u1)
      })
    )
    (map-set user-analytics
      { user: employee }
      (merge employee-data {
        streams-as-employee: (+ (get streams-as-employee employee-data) u1),
        total-received: (+ (get total-received employee-data) amount)
      })
    ))
)

(define-private (update-global-counters (completion-type (string-ascii 20)) (amount uint))
  (begin
    (var-set total-volume-streamed (+ (var-get total-volume-streamed) amount))
    (if (is-eq completion-type "completed")
      (var-set total-completed-streams (+ (var-get total-completed-streams) u1))
      (var-set total-cancelled-streams (+ (var-get total-cancelled-streams) u1))))
)

;; === REPORTING FUNCTIONS ===

(define-read-only (generate-platform-report)
  {
    total-streams: (var-get total-streams),
    completed-streams: (var-get total-completed-streams),
    cancelled-streams: (var-get total-cancelled-streams),
    total-volume: (var-get total-volume-streamed),
    success-rate: (if (> (var-get total-streams) u0)
                   (/ (* (var-get total-completed-streams) u100) (var-get total-streams))
                   u0),
    avg-completion-rate: (if (> (+ (var-get total-completed-streams) (var-get total-cancelled-streams)) u0)
                          (/ (* (var-get total-completed-streams) u100) 
                             (+ (var-get total-completed-streams) (var-get total-cancelled-streams)))
                          u0)
  }
)

(define-read-only (generate-user-performance-report (user principal))
  (let (
    (analytics (get-user-analytics user))
    (reliability (unwrap-panic (calculate-user-reliability user)))
  )
    {
      user: user,
      employer-streams: (get streams-as-employer analytics),
      employee-streams: (get streams-as-employee analytics),
      total-paid: (get total-paid analytics),
      total-received: (get total-received analytics),
      reliability-score: reliability,
      avg-duration: (get avg-stream-duration analytics)
    })
)

(define-read-only (get-top-performers (limit uint))
  (let (
    (sample-users (list 
      'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM
      'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5
      'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG
    ))
  )
    {
      top-employers: sample-users,
      top-employees: sample-users,
      methodology: "Based on reliability score and volume"
    })
)

;; === ANALYTICS MANAGEMENT ===

(define-public (toggle-analytics (enabled bool))
  (begin
    (var-set analytics-enabled enabled)
    (ok enabled))
)

(define-read-only (is-analytics-enabled)
  (var-get analytics-enabled)
)

(define-public (reset-analytics-counters)
  (begin
    (var-set total-volume-streamed u0)
    (var-set total-completed-streams u0)
    (var-set total-cancelled-streams u0)
    (ok true))
)

;; === ADVANCED ANALYTICS QUERIES ===

(define-read-only (get-stream-health-score (stream-id uint))
  (match (get-stream stream-id)
    stream (let (
      (current-time stacks-block-height)
      (time-progress (/ (* (- current-time (get start-time stream)) u100) 
                       (- (get end-time stream) (get start-time stream))))
      (withdrawal-progress (/ (* (get withdrawn-amount stream) u100) (get total-amount stream)))
      (health-score (if (> time-progress withdrawal-progress)
                     (- u100 (- time-progress withdrawal-progress))
                     u100))
    )
      (ok (min health-score u100)))
    (ok u0))
)

(define-read-only (predict-stream-completion (stream-id uint))
  (match (get-stream stream-id)
    stream (let (
      (current-time stacks-block-height)
      (elapsed (- current-time (get start-time stream)))
      (progress-ratio (if (> (get withdrawn-amount stream) u0)
                       (/ (* elapsed u100) 
                          (/ (* (get total-amount stream) elapsed) (get withdrawn-amount stream)))
                       u0))
      (estimated-completion (if (> progress-ratio u0)
                             (+ (get start-time stream) 
                                (/ (* (- (get end-time stream) (get start-time stream)) u100) progress-ratio))
                             (get end-time stream)))
    )
      (ok {
        estimated-completion-block: estimated-completion,
        confidence: (min progress-ratio u100),
        on-schedule: (<= estimated-completion (get end-time stream))
      }))
    (err u0))
)
