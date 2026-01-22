;; ============================================================
;; Contract: usage-quota-enforcer
;; Purpose : Rolling-window quota enforcement
;; ============================================================

;; ------------------------------------------------------------
;; Error Codes
;; ------------------------------------------------------------
(define-constant ERR-NOT-ADMIN          (err u3800))
(define-constant ERR-QUOTA-EXCEEDED     (err u3801))
(define-constant ERR-INVALID-PARAMS     (err u3802))

;; ------------------------------------------------------------
;; Data Variables
;; ------------------------------------------------------------
(define-data-var admin principal tx-sender)
(define-data-var window-size uint u100)

;; ------------------------------------------------------------
;; Maps
;; ------------------------------------------------------------

(define-map quotas
  { user: principal }
  {
    limit: uint,
    used: uint,
    window-start: uint
  }
)

;; ------------------------------------------------------------
;; Read-Only Functions
;; ------------------------------------------------------------

(define-read-only (get-quota (user principal))
  (map-get? quotas { user: user })
)

;; ------------------------------------------------------------
;; Internal Helpers
;; ------------------------------------------------------------

(define-private (is-admin)
  (is-eq tx-sender (var-get admin))
)

(define-private (window-expired (start uint))
  (>= stacks-block-height (+ start (var-get window-size)))
)

;; ------------------------------------------------------------
;; Public Functions - Admin Control
;; ------------------------------------------------------------

(define-public (set-window-size (size uint))
  (begin
    (asserts! (is-admin) ERR-NOT-ADMIN)
    (asserts! (> size u0) ERR-INVALID-PARAMS)

    (var-set window-size size)
    (ok true)
  )
)

(define-public (set-user-quota (user principal) (limit uint))
  (begin
    (asserts! (is-admin) ERR-NOT-ADMIN)

    (map-set quotas
      { user: user }
      {
        limit: limit,
        used: u0,
        window-start: stacks-block-height
      }
    )
    (ok true)
  )
)

;; ------------------------------------------------------------
;; Public Functions - Consumption
;; ------------------------------------------------------------

(define-public (consume (amount uint))
  (begin
    (asserts! (> amount u0) ERR-INVALID-PARAMS)

    (let (
          (q (default-to
                { limit: u0, used: u0, window-start: stacks-block-height }
                (map-get? quotas { user: tx-sender })
             ))
         )
      ;; Reset window if expired
      (let (
            (used-new
              (if (window-expired (get window-start q))
                u0
                (get used q)
              ))
            (start-new
              (if (window-expired (get window-start q))
                stacks-block-height
                (get window-start q)
              ))
           )
        (asserts!
          (<= (+ used-new amount) (get limit q))
          ERR-QUOTA-EXCEEDED
        )

        (map-set quotas
          { user: tx-sender }
          {
            limit: (get limit q),
            used: (+ used-new amount),
            window-start: start-new
          }
        )
        (ok true)
      )
    )
  )
)
