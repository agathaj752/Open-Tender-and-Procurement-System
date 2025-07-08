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
    (let (
        (tender-id (+ (var-get tender-counter) u1))
        (required-deposit (/ (* minimum-bid (var-get deposit-percentage)) u100))
    )
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
                (map-set tender-deposit-requirements
                    { tender-id: tender-id }
                    {
                        required-deposit: required-deposit,
                        total-deposits: u0
                    }
                )
                (var-set tender-counter tender-id)
                (ok tender-id))
            (err ERR-DEADLINE-PASSED)
        )
    )
)

(define-public (submit-bid (tender-id uint) (amount uint) (proposal (string-ascii 500)))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (deposit-req (unwrap! (map-get? tender-deposit-requirements { tender-id: tender-id }) (err ERR-NOT-FOUND)))
        (required-deposit (get required-deposit deposit-req))
        (existing-bid (map-get? bids { tender-id: tender-id, bidder: tx-sender }))
    )
        (asserts! (is-eq (get status tender) "open") (err ERR-TENDER-CLOSED))
        (asserts! (>= amount (get minimum-bid tender)) (err ERR-LOW-BID))
        (asserts! (< stacks-block-height (get deadline tender)) (err ERR-DEADLINE-PASSED))
        (if (is-none existing-bid)
            (begin
                (unwrap! (stx-transfer? required-deposit tx-sender (as-contract tx-sender)) (err ERR-DEPOSIT-TRANSFER-FAILED))
                (map-set bid-deposits
                    { tender-id: tender-id, bidder: tx-sender }
                    {
                        amount: required-deposit,
                        refunded: false,
                        timestamp: stacks-block-height
                    }
                )
                (map-set tender-deposit-requirements
                    { tender-id: tender-id }
                    (merge deposit-req { total-deposits: (+ (get total-deposits deposit-req) required-deposit) })
                )
            )
            true
        )
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
(define-constant ERR-INSUFFICIENT-DEPOSIT (err u110))
(define-constant ERR-DEPOSIT-TRANSFER-FAILED (err u111))
(define-constant ERR-REFUND-FAILED (err u116))
(define-constant ERR-DEPOSIT-ALREADY-REFUNDED (err u117))
(define-constant ERR-DEPOSIT-NOT-FOUND (err u118))

(define-data-var deposit-percentage uint u10)

(define-map bid-deposits
    { tender-id: uint, bidder: principal }
    {
        amount: uint,
        refunded: bool,
        timestamp: uint
    }
)

(define-map tender-deposit-requirements
    { tender-id: uint }
    {
        required-deposit: uint,
        total-deposits: uint
    }
)

(define-private (refund-losing-bidders (tender-id uint) (winner principal))
    (ok true)
)

(define-public (refund-deposit (tender-id uint) (bidder principal))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (deposit (unwrap! (map-get? bid-deposits { tender-id: tender-id, bidder: bidder }) (err ERR-DEPOSIT-NOT-FOUND)))
        (tender-winner (get winner tender))
    )
        (asserts! (is-some tender-winner) (err ERR-TENDER-OPEN))
        (asserts! (not (is-eq bidder (unwrap-panic tender-winner))) (err ERR-NOT-AUTHORIZED))
        (asserts! (not (get refunded deposit)) (err ERR-DEPOSIT-ALREADY-REFUNDED))
        (unwrap! (as-contract (stx-transfer? (get amount deposit) tx-sender bidder)) (err ERR-REFUND-FAILED))
        (map-set bid-deposits
            { tender-id: tender-id, bidder: bidder }
            (merge deposit { refunded: true })
        )
        (ok true)
    )
)

(define-public (claim-winner-deposit (tender-id uint))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (winner (unwrap! (get winner tender) (err ERR-NOT-FOUND)))
        (deposit (unwrap! (map-get? bid-deposits { tender-id: tender-id, bidder: tx-sender }) (err ERR-DEPOSIT-NOT-FOUND)))
    )
        (asserts! (is-eq tx-sender winner) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status tender) "completed") (err ERR-TENDER-NOT-COMPLETED))
        (asserts! (not (get refunded deposit)) (err ERR-DEPOSIT-ALREADY-REFUNDED))
        (unwrap! (as-contract (stx-transfer? (get amount deposit) tx-sender tx-sender)) (err ERR-REFUND-FAILED))
        (map-set bid-deposits
            { tender-id: tender-id, bidder: tx-sender }
            (merge deposit { refunded: true })
        )
        (ok true)
    )
)

(define-read-only (get-bid-deposit (tender-id uint) (bidder principal))
    (map-get? bid-deposits { tender-id: tender-id, bidder: bidder })
)

(define-read-only (get-deposit-requirements (tender-id uint))
    (map-get? tender-deposit-requirements { tender-id: tender-id })
)

(define-public (set-deposit-percentage (new-percentage uint))
    (begin
        (asserts! (<= new-percentage u50) (err ERR-INVALID-RATING))
        (var-set deposit-percentage new-percentage)
        (ok true)
    )
)

(define-read-only (get-deposit-percentage)
    (var-get deposit-percentage)
)

(define-public (emergency-refund (tender-id uint))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (deposit (unwrap! (map-get? bid-deposits { tender-id: tender-id, bidder: tx-sender }) (err ERR-DEPOSIT-NOT-FOUND)))
    )
        (asserts! (> stacks-block-height (+ (get deadline tender) u1000)) (err ERR-NOT-AUTHORIZED))
        (asserts! (not (get refunded deposit)) (err ERR-DEPOSIT-ALREADY-REFUNDED))
        (unwrap! (as-contract (stx-transfer? (get amount deposit) tx-sender tx-sender)) (err ERR-REFUND-FAILED))
        (map-set bid-deposits
            { tender-id: tender-id, bidder: tx-sender }
            (merge deposit { refunded: true })
        )
        (ok true)
    )
)

(define-public (withdraw-bid (tender-id uint))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (bid (unwrap! (get-bid tender-id tx-sender) (err ERR-NO-BID-EXISTS)))
        (deposit (map-get? bid-deposits { tender-id: tender-id, bidder: tx-sender }))
    )
        (asserts! (is-eq (get status tender) "open") (err ERR-TENDER-CLOSED))
        (asserts! (< stacks-block-height (get deadline tender)) (err ERR-DEADLINE-PASSED))
        (map-delete bids { tender-id: tender-id, bidder: tx-sender })
        (match deposit
            deposit-data
            (begin
                (unwrap! (as-contract (stx-transfer? (get amount deposit-data) tx-sender tx-sender)) (err ERR-REFUND-FAILED))
                (map-set bid-deposits
                    { tender-id: tender-id, bidder: tx-sender }
                    (merge deposit-data { refunded: true })
                )
            )
            true
        )
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
    (let (
        (tender-id (+ (var-get tender-counter) u1))
        (required-deposit (/ (* minimum-bid (var-get deposit-percentage)) u100))
    )
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
                (map-set tender-deposit-requirements
                    { tender-id: tender-id }
                    {
                        required-deposit: required-deposit,
                        total-deposits: u0
                    }
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

(define-constant ERR-MILESTONE-NOT-FOUND (err u201))
(define-constant ERR-MILESTONE-ALREADY-COMPLETED (err u202))
(define-constant ERR-MILESTONE-NOT-APPROVED (err u203))
(define-constant ERR-INSUFFICIENT-ESCROW (err u204))
(define-constant ERR-INVALID-MILESTONE-INDEX (err u205))
(define-constant ERR-MILESTONE-DISPUTED (err u206))
(define-constant ERR-ESCROW-RELEASE-FAILED (err u207))
(define-constant ERR-DISPUTE-PERIOD-EXPIRED (err u208))

(define-data-var dispute-period-blocks uint u1440)

(define-map tender-milestones
    { tender-id: uint }
    {
        milestone-count: uint,
        current-milestone: uint,
        total-escrow: uint,
        released-escrow: uint,
        milestones-enabled: bool
    }
)

(define-map milestone-details
    { tender-id: uint, milestone-index: uint }
    {
        title: (string-ascii 100),
        description: (string-ascii 300),
        payment-amount: uint,
        deadline: uint,
        status: (string-ascii 20),
        submitted-at: (optional uint),
        approved-at: (optional uint),
        disputed-at: (optional uint),
        deliverable-hash: (optional (string-ascii 64))
    }
)

(define-map milestone-disputes
    { tender-id: uint, milestone-index: uint }
    {
        disputed-by: principal,
        dispute-reason: (string-ascii 300),
        resolved: bool,
        resolution: (string-ascii 300),
        resolved-at: (optional uint)
    }
)

(define-map tender-escrow
    { tender-id: uint }
    {
        total-amount: uint,
        locked-amount: uint,
        released-amount: uint,
        depositor: principal
    }
)

(define-private (create-single-milestone 
    (tender-id uint)
    (milestone-index uint)
    (title (string-ascii 100))
    (description (string-ascii 300))
    (payment-amount uint)
    (deadline uint))
    (begin
        (map-set milestone-details
            { tender-id: tender-id, milestone-index: milestone-index }
            {
                title: title,
                description: description,
                payment-amount: payment-amount,
                deadline: deadline,
                status: "pending",
                submitted-at: none,
                approved-at: none,
                disputed-at: none,
                deliverable-hash: none
            }
        )
        (ok true)
    )
)

(define-public (create-milestone-tender 
    (title (string-ascii 100)) 
    (description (string-ascii 500)) 
    (deadline uint) 
    (minimum-bid uint)
    (milestone-titles (list 10 (string-ascii 100)))
    (milestone-descriptions (list 10 (string-ascii 300)))
    (milestone-payments (list 10 uint))
    (milestone-deadlines (list 10 uint)))
    (let (
        (tender-id (+ (var-get tender-counter) u1))
        (required-deposit (/ (* minimum-bid (var-get deposit-percentage)) u100))
        (milestone-count (len milestone-titles))
        (total-milestone-payment (fold + milestone-payments u0))
    )
        (asserts! (and (> milestone-count u0) (<= milestone-count u10)) (err ERR-INVALID-MILESTONE-INDEX))
        (asserts! (is-eq total-milestone-payment minimum-bid) (err ERR-INSUFFICIENT-ESCROW))
        (asserts! (> deadline stacks-block-height) (err ERR-DEADLINE-PASSED))
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
        (map-set tender-milestones
            { tender-id: tender-id }
            {
                milestone-count: milestone-count,
                current-milestone: u0,
                total-escrow: u0,
                released-escrow: u0,
                milestones-enabled: true
            }
        )
        (map-set tender-deposit-requirements
            { tender-id: tender-id }
            {
                required-deposit: required-deposit,
                total-deposits: u0
            }
        )
        (var-set tender-counter tender-id)
        (ok tender-id)
    )
)

(define-public (setup-milestone (tender-id uint) (milestone-index uint) (title (string-ascii 100)) (description (string-ascii 300)) (payment-amount uint) (deadline uint))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (milestone-data (unwrap! (map-get? tender-milestones { tender-id: tender-id }) (err ERR-MILESTONE-NOT-FOUND)))
    )
        (asserts! (is-eq tx-sender (get owner tender)) (err ERR-NOT-AUTHORIZED))
        (asserts! (< milestone-index (get milestone-count milestone-data)) (err ERR-INVALID-MILESTONE-INDEX))
        (asserts! (is-eq (get status tender) "open") (err ERR-TENDER-CLOSED))
        (create-single-milestone tender-id milestone-index title description payment-amount deadline)
    )
)

(define-public (fund-milestone-escrow (tender-id uint) (amount uint))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (milestone-data (unwrap! (map-get? tender-milestones { tender-id: tender-id }) (err ERR-MILESTONE-NOT-FOUND)))
        (current-escrow (default-to { total-amount: u0, locked-amount: u0, released-amount: u0, depositor: tx-sender } 
                                   (map-get? tender-escrow { tender-id: tender-id })))
    )
        (asserts! (is-eq tx-sender (get owner tender)) (err ERR-NOT-AUTHORIZED))
        (asserts! (get milestones-enabled milestone-data) (err ERR-MILESTONE-NOT-FOUND))
        (unwrap! (stx-transfer? amount tx-sender (as-contract tx-sender)) (err ERR-DEPOSIT-TRANSFER-FAILED))
        (map-set tender-escrow
            { tender-id: tender-id }
            {
                total-amount: (+ (get total-amount current-escrow) amount),
                locked-amount: (+ (get locked-amount current-escrow) amount),
                released-amount: (get released-amount current-escrow),
                depositor: tx-sender
            }
        )
        (ok true)
    )
)

(define-public (submit-milestone-deliverable (tender-id uint) (milestone-index uint) (deliverable-hash (string-ascii 64)))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (milestone (unwrap! (map-get? milestone-details { tender-id: tender-id, milestone-index: milestone-index }) (err ERR-MILESTONE-NOT-FOUND)))
        (milestone-data (unwrap! (map-get? tender-milestones { tender-id: tender-id }) (err ERR-MILESTONE-NOT-FOUND)))
        (winner (unwrap! (get winner tender) (err ERR-NOT-FOUND)))
    )
        (asserts! (is-eq tx-sender winner) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status milestone) "pending") (err ERR-MILESTONE-ALREADY-COMPLETED))
        (asserts! (is-eq milestone-index (get current-milestone milestone-data)) (err ERR-INVALID-MILESTONE-INDEX))
        (map-set milestone-details
            { tender-id: tender-id, milestone-index: milestone-index }
            (merge milestone {
                status: "submitted",
                submitted-at: (some stacks-block-height),
                deliverable-hash: (some deliverable-hash)
            })
        )
        (ok true)
    )
)

(define-public (approve-milestone (tender-id uint) (milestone-index uint))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (milestone (unwrap! (map-get? milestone-details { tender-id: tender-id, milestone-index: milestone-index }) (err ERR-MILESTONE-NOT-FOUND)))
        (milestone-data (unwrap! (map-get? tender-milestones { tender-id: tender-id }) (err ERR-MILESTONE-NOT-FOUND)))
        (escrow (unwrap! (map-get? tender-escrow { tender-id: tender-id }) (err ERR-INSUFFICIENT-ESCROW)))
        (winner (unwrap! (get winner tender) (err ERR-NOT-FOUND)))
    )
        (asserts! (is-eq tx-sender (get owner tender)) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status milestone) "submitted") (err ERR-MILESTONE-NOT-APPROVED))
        (asserts! (>= (get locked-amount escrow) (get payment-amount milestone)) (err ERR-INSUFFICIENT-ESCROW))
        (unwrap! (as-contract (stx-transfer? (get payment-amount milestone) tx-sender winner)) (err ERR-ESCROW-RELEASE-FAILED))
        (map-set milestone-details
            { tender-id: tender-id, milestone-index: milestone-index }
            (merge milestone {
                status: "completed",
                approved-at: (some stacks-block-height)
            })
        )
        (map-set tender-escrow
            { tender-id: tender-id }
            {
                total-amount: (get total-amount escrow),
                locked-amount: (- (get locked-amount escrow) (get payment-amount milestone)),
                released-amount: (+ (get released-amount escrow) (get payment-amount milestone)),
                depositor: (get depositor escrow)
            }
        )
        (map-set tender-milestones
            { tender-id: tender-id }
            (merge milestone-data {
                current-milestone: (+ (get current-milestone milestone-data) u1),
                released-escrow: (+ (get released-escrow milestone-data) (get payment-amount milestone))
            })
        )
        (ok true)
    )
)

(define-public (dispute-milestone (tender-id uint) (milestone-index uint) (reason (string-ascii 300)))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (milestone (unwrap! (map-get? milestone-details { tender-id: tender-id, milestone-index: milestone-index }) (err ERR-MILESTONE-NOT-FOUND)))
        (submitted-at (unwrap! (get submitted-at milestone) (err ERR-MILESTONE-NOT-APPROVED)))
    )
        (asserts! (is-eq tx-sender (get owner tender)) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status milestone) "submitted") (err ERR-MILESTONE-NOT-APPROVED))
        (asserts! (< (- stacks-block-height submitted-at) (var-get dispute-period-blocks)) (err ERR-DISPUTE-PERIOD-EXPIRED))
        (map-set milestone-details
            { tender-id: tender-id, milestone-index: milestone-index }
            (merge milestone {
                status: "disputed",
                disputed-at: (some stacks-block-height)
            })
        )
        (map-set milestone-disputes
            { tender-id: tender-id, milestone-index: milestone-index }
            {
                disputed-by: tx-sender,
                dispute-reason: reason,
                resolved: false,
                resolution: "",
                resolved-at: none
            }
        )
        (ok true)
    )
)

(define-public (resolve-milestone-dispute (tender-id uint) (milestone-index uint) (approve bool) (resolution (string-ascii 300)))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (milestone (unwrap! (map-get? milestone-details { tender-id: tender-id, milestone-index: milestone-index }) (err ERR-MILESTONE-NOT-FOUND)))
        (dispute (unwrap! (map-get? milestone-disputes { tender-id: tender-id, milestone-index: milestone-index }) (err ERR-MILESTONE-NOT-FOUND)))
        (escrow (unwrap! (map-get? tender-escrow { tender-id: tender-id }) (err ERR-INSUFFICIENT-ESCROW)))
        (milestone-data (unwrap! (map-get? tender-milestones { tender-id: tender-id }) (err ERR-MILESTONE-NOT-FOUND)))
        (winner (unwrap! (get winner tender) (err ERR-NOT-FOUND)))
    )
        (asserts! (is-eq tx-sender (get owner tender)) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status milestone) "disputed") (err ERR-MILESTONE-NOT-FOUND))
        (asserts! (not (get resolved dispute)) (err ERR-MILESTONE-ALREADY-COMPLETED))
        (map-set milestone-disputes
            { tender-id: tender-id, milestone-index: milestone-index }
            (merge dispute {
                resolved: true,
                resolution: resolution,
                resolved-at: (some stacks-block-height)
            })
        )
        (if approve
            (begin
                (unwrap! (as-contract (stx-transfer? (get payment-amount milestone) tx-sender winner)) (err ERR-ESCROW-RELEASE-FAILED))
                (map-set milestone-details
                    { tender-id: tender-id, milestone-index: milestone-index }
                    (merge milestone { status: "completed", approved-at: (some stacks-block-height) })
                )
                (map-set tender-escrow
                    { tender-id: tender-id }
                    {
                        total-amount: (get total-amount escrow),
                        locked-amount: (- (get locked-amount escrow) (get payment-amount milestone)),
                        released-amount: (+ (get released-amount escrow) (get payment-amount milestone)),
                        depositor: (get depositor escrow)
                    }
                )
                (map-set tender-milestones
                    { tender-id: tender-id }
                    (merge milestone-data {
                        current-milestone: (+ (get current-milestone milestone-data) u1),
                        released-escrow: (+ (get released-escrow milestone-data) (get payment-amount milestone))
                    })
                )
            )
            (map-set milestone-details
                { tender-id: tender-id, milestone-index: milestone-index }
                (merge milestone { status: "rejected" })
            )
        )
        (ok true)
    )
)

(define-read-only (get-milestone-details (tender-id uint) (milestone-index uint))
    (map-get? milestone-details { tender-id: tender-id, milestone-index: milestone-index })
)

(define-read-only (get-tender-milestones (tender-id uint))
    (map-get? tender-milestones { tender-id: tender-id })
)

(define-read-only (get-milestone-dispute (tender-id uint) (milestone-index uint))
    (map-get? milestone-disputes { tender-id: tender-id, milestone-index: milestone-index })
)

(define-read-only (get-tender-escrow (tender-id uint))
    (map-get? tender-escrow { tender-id: tender-id })
)

(define-read-only (get-milestone-progress (tender-id uint))
    (match (map-get? tender-milestones { tender-id: tender-id })
        milestone-data
        (some {
            current-milestone: (get current-milestone milestone-data),
            total-milestones: (get milestone-count milestone-data),
            completion-percentage: (if (> (get milestone-count milestone-data) u0)
                                     (/ (* (get current-milestone milestone-data) u100) (get milestone-count milestone-data))
                                     u0),
            total-escrow: (get total-escrow milestone-data),
            released-escrow: (get released-escrow milestone-data)
        })
        none
    )
)

(define-public (withdraw-remaining-escrow (tender-id uint))
    (let (
        (tender (unwrap! (get-tender tender-id) (err ERR-NOT-FOUND)))
        (escrow (unwrap! (map-get? tender-escrow { tender-id: tender-id }) (err ERR-INSUFFICIENT-ESCROW)))
        (milestone-data (unwrap! (map-get? tender-milestones { tender-id: tender-id }) (err ERR-MILESTONE-NOT-FOUND)))
    )
        (asserts! (is-eq tx-sender (get owner tender)) (err ERR-NOT-AUTHORIZED))
        (asserts! (is-eq (get status tender) "completed") (err ERR-TENDER-NOT-COMPLETED))
        (asserts! (>= (get current-milestone milestone-data) (get milestone-count milestone-data)) (err ERR-MILESTONE-NOT-FOUND))
        (asserts! (> (get locked-amount escrow) u0) (err ERR-INSUFFICIENT-ESCROW))
        (unwrap! (as-contract (stx-transfer? (get locked-amount escrow) tx-sender tx-sender)) (err ERR-ESCROW-RELEASE-FAILED))
        (map-set tender-escrow
            { tender-id: tender-id }
            (merge escrow {
                locked-amount: u0,
                released-amount: (get total-amount escrow)
            })
        )
        (ok true)
    )
)