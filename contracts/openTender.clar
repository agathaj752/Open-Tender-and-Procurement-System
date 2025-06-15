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


(define-constant ERR-RATING-EXISTS (err u112))
(define-constant ERR-INVALID-RATING (err u113))
(define-constant ERR-TENDER-NOT-COMPLETED (err u114))
(define-constant ERR-NOT-PARTICIPANT (err u115))

(define-map user-ratings
    { user: principal }
    {
        total-rating: uint,
        rating-count: uint,
        completed-tenders: uint
    }
)

(define-map tender-ratings
    { tender-id: uint, rater: principal, rated: principal }
    {
        rating: uint,
        comment: (string-ascii 200),
        timestamp: uint
    }
)

(define-public (rate-user (tender-id uint) (rated-user principal) (rating uint) (comment (string-ascii 200)))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (tender-owner (get owner tender))
        (tender-winner (get winner tender))
    )
        (asserts! (and (>= rating u1) (<= rating u5)) (err ERR-INVALID-RATING))
        (asserts! (is-eq (get status tender) "completed") (err ERR-TENDER-NOT-COMPLETED))
        (asserts! (is-none (map-get? tender-ratings { tender-id: tender-id, rater: tx-sender, rated: rated-user })) (err ERR-RATING-EXISTS))
        (asserts! 
            (or 
                (and (is-eq tx-sender tender-owner) (is-eq rated-user (unwrap! tender-winner (err ERR-NOT-PARTICIPANT))))
                (and (is-eq tx-sender (unwrap! tender-winner (err ERR-NOT-PARTICIPANT))) (is-eq rated-user tender-owner))
            ) 
            (err ERR-NOT-PARTICIPANT)
        )
        (let (
            (current-ratings (default-to { total-rating: u0, rating-count: u0, completed-tenders: u0 } 
                                       (map-get? user-ratings { user: rated-user })))
            (new-total (+ (get total-rating current-ratings) rating))
            (new-count (+ (get rating-count current-ratings) u1))
            (new-completed (if (is-eq rated-user (unwrap! tender-winner (err ERR-NOT-PARTICIPANT)))
                             (+ (get completed-tenders current-ratings) u1)
                             (get completed-tenders current-ratings)))
        )
            (map-set tender-ratings
                { tender-id: tender-id, rater: tx-sender, rated: rated-user }
                {
                    rating: rating,
                    comment: comment,
                    timestamp: stacks-block-height
                }
            )
            (map-set user-ratings
                { user: rated-user }
                {
                    total-rating: new-total,
                    rating-count: new-count,
                    completed-tenders: new-completed
                }
            )
            (ok true)
        )
    )
)

(define-read-only (get-user-reputation (user principal))
    (match (map-get? user-ratings { user: user })
        rating-data
        (if (> (get rating-count rating-data) u0)
            (some {
                average-rating: (/ (* (get total-rating rating-data) u100) (get rating-count rating-data)),
                total-ratings: (get rating-count rating-data),
                completed-tenders: (get completed-tenders rating-data)
            })
            (some { average-rating: u0, total-ratings: u0, completed-tenders: u0 }))
        none
    )
)

(define-read-only (get-tender-rating (tender-id uint) (rater principal) (rated principal))
    (map-get? tender-ratings { tender-id: tender-id, rater: rater, rated: rated })
)

(define-read-only (get-user-rating-summary (user principal))
    (let ((reputation (get-user-reputation user)))
        (match reputation
            rep-data
            (some {
                average-rating-display: (/ (get average-rating rep-data) u20),
                star-rating: (if (>= (get average-rating rep-data) u500) u5
                            (if (>= (get average-rating rep-data) u400) u4
                            (if (>= (get average-rating rep-data) u300) u3
                            (if (>= (get average-rating rep-data) u200) u2 u1)))),
                total-reviews: (get total-ratings rep-data),
                projects-completed: (get completed-tenders rep-data)
            })
            none
        )
    )
)

(define-read-only (is-reputable-user (user principal) (min-rating uint) (min-completed uint))
    (match (get-user-reputation user)
        reputation
        (and 
            (>= (get average-rating reputation) (* min-rating u20))
            (>= (get completed-tenders reputation) min-completed)
            (>= (get total-ratings reputation) u1)
        )
        false
    )
)

(define-public (complete-tender-with-rating (tender-id uint) (winner principal) (rating uint) (comment (string-ascii 200)))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (owner (get owner tender))
        (bid (unwrap! (get-bid tender-id winner) (err ERR-NOT-FOUND)))
    )
        (asserts! (is-eq tx-sender owner) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status tender) "closed") (err ERR-TENDER-OPEN))
        (asserts! (and (>= rating u1) (<= rating u5)) (err ERR-INVALID-RATING))
        (map-set tenders
            { tender-id: tender-id }
            (merge tender { winner: (some winner), status: "completed" })
        )
        (rate-user tender-id winner rating comment)
    )
)