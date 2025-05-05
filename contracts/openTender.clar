(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-TENDER-CLOSED (err u101))
(define-constant ERR-TENDER-OPEN (err u102))
(define-constant ERR-LOW-BID (err u103))
(define-constant ERR-NO-BIDS (err u104))
(define-constant ERR-ALREADY-EXISTS (err u105))
(define-constant ERR-NOT-FOUND (err u106))
(define-constant ERR-DEADLINE-PASSED (err u107))

(define-data-var tender-counter uint u0)

(define-map tenders
    { tender-id: uint }
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        owner: principal,
        deadline: uint,
        minimum-bid: uint,
        status: (string-ascii 20),
        winner: (optional principal)
    }
)

(define-map bids
    { tender-id: uint, bidder: principal }
    {
        amount: uint,
        proposal: (string-ascii 500),
        timestamp: uint
    }
)

(define-read-only (get-tender (tender-id uint))
    (map-get? tenders { tender-id: tender-id })
)

(define-read-only (get-bid (tender-id uint) (bidder principal))
    (map-get? bids { tender-id: tender-id, bidder: bidder })
)

(define-public (create-tender (title (string-ascii 100)) (description (string-ascii 500)) (deadline uint) (minimum-bid uint))
    (let ((tender-id (+ (var-get tender-counter) u1)))
        (if (> deadline stacks-block-height)
            (begin
                (map-set tenders
                    { tender-id: tender-id }
                    {
                        title: title,
                        description: description,
                        owner: tx-sender,
                        deadline: deadline,
                        minimum-bid: minimum-bid,
                        status: "open",
                        winner: none
                    }
                )
                (var-set tender-counter tender-id)
                (ok tender-id))
            (err ERR-DEADLINE-PASSED)
        )
    )
)

(define-public (submit-bid (tender-id uint) (amount uint) (proposal (string-ascii 500)))
    (let ((tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND))))
        (asserts! (is-eq (get status tender) "open") (err ERR-TENDER-CLOSED))
        (asserts! (>= amount (get minimum-bid tender)) (err ERR-LOW-BID))
        (asserts! (< stacks-block-height (get deadline tender)) (err ERR-DEADLINE-PASSED))
        (map-set bids
            { tender-id: tender-id, bidder: tx-sender }
            {
                amount: amount,
                proposal: proposal,
                timestamp: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-public (close-tender (tender-id uint))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (owner (get owner tender))
    )
        (asserts! (is-eq tx-sender owner) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status tender) "open") (err ERR-TENDER-CLOSED))
        (map-set tenders
            { tender-id: tender-id }
            (merge tender { status: "closed" })
        )
        (ok true)
    )
)

(define-public (select-winner (tender-id uint) (winner principal))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (owner (get owner tender))
        (bid (unwrap! (get-bid tender-id winner) (err ERR-NOT-FOUND)))
    )
        (asserts! (is-eq tx-sender owner) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status tender) "closed") (err ERR-TENDER-OPEN))
        (map-set tenders
            { tender-id: tender-id }
            (merge tender { winner: (some winner) })
        )
        (ok true)
    )
)

(define-read-only (get-all-bids (tender-id uint))
    (map-get? bids { tender-id: tender-id, bidder: tx-sender })
)


(define-constant ERR-NO-BID-EXISTS (err u108))
(define-constant ERR-BID-LOCKED (err u109))

(define-public (withdraw-bid (tender-id uint))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (bid (unwrap! (get-bid tender-id tx-sender) (err ERR-NO-BID-EXISTS)))
    )
        (asserts! (is-eq (get status tender) "open") (err ERR-TENDER-CLOSED))
        (asserts! (< stacks-block-height (get deadline tender)) (err ERR-DEADLINE-PASSED))
        (map-delete bids { tender-id: tender-id, bidder: tx-sender })
        (ok true)
    )
)


(define-map tender-categories 
    { tender-id: uint }
    { category: (string-ascii 50) }
)

(define-public (create-tender-with-category 
    (title (string-ascii 100)) 
    (description (string-ascii 500)) 
    (deadline uint) 
    (minimum-bid uint)
    (category (string-ascii 50)))
    (let ((tender-id (+ (var-get tender-counter) u1)))
        (if (> deadline stacks-block-height)
            (begin
                (map-set tenders
                    { tender-id: tender-id }
                    {
                        title: title,
                        description: description,
                        owner: tx-sender,
                        deadline: deadline,
                        minimum-bid: minimum-bid,
                        status: "open",
                        winner: none
                    }
                )
                (map-set tender-categories
                    { tender-id: tender-id }
                    { category: category }
                )
                (var-set tender-counter tender-id)
                (ok tender-id))
            (err ERR-DEADLINE-PASSED)
        )
    )
)

(define-read-only (get-tender-category (tender-id uint))
    (map-get? tender-categories { tender-id: tender-id })
)


(define-public (update-tender-category (tender-id uint) (new-category (string-ascii 50)))
    (let ((tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND))))
        (asserts! (is-eq tx-sender (get owner tender)) (err ERR-NOT-AUTHORIZED))
        (map-set tender-categories
            { tender-id: tender-id }
            { category: new-category }
        )
        (ok true)
    )
)