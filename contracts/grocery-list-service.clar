;; ========================================
;; DIGITAL GROCERY LIST SHARER
;; ========================================
;; A comprehensive household shopping coordination system
;; Features: Shared lists, budget tracking, purchase tracking, store optimization

;; ========================================
;; CONTRACT 1: HOUSEHOLD MANAGEMENT
;; ========================================

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-HOUSEHOLD-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-MEMBER (err u102))
(define-constant ERR-MEMBER-NOT-FOUND (err u103))
(define-constant ERR-INVALID-INPUT (err u104))
(define-constant ERR-INSUFFICIENT-BALANCE (err u105))

;; Data structures
(define-map households
    { household-id: uint }
    {
        name: (string-ascii 50),
        creator: principal,
        created-at: uint,
        monthly-budget: uint,
        current-spent: uint,
        is-active: bool
    }
)

(define-map household-members
    { household-id: uint, member: principal }
    {
        role: (string-ascii 20),
        joined-at: uint,
        spending-limit: uint,
        is-active: bool
    }
)

(define-map member-households
    { member: principal }
    { household-ids: (list 10 uint) }
)

;; Global counters
(define-data-var next-household-id uint u1)

;; Public functions

;; Create a new household
(define-public (create-household (name (string-ascii 50)) (monthly-budget uint))
    (let (
        (household-id (var-get next-household-id))
        (caller tx-sender)
        (current-height stacks-block-height)
    )
        (asserts! (> (len name) u0) ERR-INVALID-INPUT)
        (asserts! (> monthly-budget u0) ERR-INVALID-INPUT)

        ;; Create household
        (map-set households
            { household-id: household-id }
            {
                name: name,
                creator: caller,
                created-at: current-height,
                monthly-budget: monthly-budget,
                current-spent: u0,
                is-active: true
            }
        )

        ;; Add creator as admin
        (map-set household-members
            { household-id: household-id, member: caller }
            {
                role: "admin",
                joined-at: current-height,
                spending-limit: monthly-budget,
                is-active: true
            }
        )

        ;; Update member's household list
        (update-member-households caller household-id)

        ;; Increment counter
        (var-set next-household-id (+ household-id u1))

        (ok household-id)
    )
)

;; Add member to household
(define-public (add-member (household-id uint) (new-member principal) (role (string-ascii 20)) (spending-limit uint))
    (let (
        (household (unwrap! (map-get? households { household-id: household-id }) ERR-HOUSEHOLD-NOT-FOUND))
        (caller tx-sender)
        (current-height stacks-block-height)
    )
        (asserts! (is-household-admin caller household-id) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (map-get? household-members { household-id: household-id, member: new-member })) ERR-ALREADY-MEMBER)
        (asserts! (or (is-eq role "admin") (is-eq role "member") (is-eq role "viewer")) ERR-INVALID-INPUT)

        ;; Add member
        (map-set household-members
            { household-id: household-id, member: new-member }
            {
                role: role,
                joined-at: current-height,
                spending-limit: spending-limit,
                is-active: true
            }
        )

        ;; Update member's household list
        (update-member-households new-member household-id)

        (ok true)
    )
)

;; Update household budget
(define-public (update-budget (household-id uint) (new-budget uint))
    (let (
        (household (unwrap! (map-get? households { household-id: household-id }) ERR-HOUSEHOLD-NOT-FOUND))
        (caller tx-sender)
    )
        (asserts! (is-household-admin caller household-id) ERR-NOT-AUTHORIZED)
        (asserts! (> new-budget u0) ERR-INVALID-INPUT)

        (map-set households
            { household-id: household-id }
            (merge household { monthly-budget: new-budget })
        )

        (ok true)
    )
)

;; Private functions

;; Check if user is household admin
(define-private (is-household-admin (user principal) (household-id uint))
    (match (map-get? household-members { household-id: household-id, member: user })
        member-data (is-eq (get role member-data) "admin")
        false
    )
)

;; Update member's household list
(define-private (update-member-households (member principal) (household-id uint))
    (let (
        (current-households (default-to { household-ids: (list) } (map-get? member-households { member: member })))
        (updated-list (unwrap-panic (as-max-len? (append (get household-ids current-households) household-id) u10)))
    )
        (map-set member-households
            { member: member }
            { household-ids: updated-list }
        )
    )
)

;; Read-only functions

;; Get household info
(define-read-only (get-household (household-id uint))
    (map-get? households { household-id: household-id })
)

;; Get member info
(define-read-only (get-member-info (household-id uint) (member principal))
    (map-get? household-members { household-id: household-id, member: member })
)

;; Get member's households
(define-read-only (get-member-households (member principal))
    (map-get? member-households { member: member })
)

;; ========================================
;; CONTRACT 2: GROCERY LIST MANAGEMENT
;; ========================================

;; Additional error codes
(define-constant ERR-LIST-NOT-FOUND (err u200))
(define-constant ERR-ITEM-NOT-FOUND (err u201))
(define-constant ERR-ALREADY-PURCHASED (err u202))
(define-constant ERR-BUDGET-EXCEEDED (err u203))

;; Data structures
(define-map grocery-lists
    { list-id: uint }
    {
        household-id: uint,
        name: (string-ascii 50),
        creator: principal,
        created-at: uint,
        target-store: (string-ascii 30),
        estimated-total: uint,
        actual-total: uint,
        status: (string-ascii 20),
        is-active: bool
    }
)

(define-map list-items
    { list-id: uint, item-id: uint }
    {
        name: (string-ascii 50),
        category: (string-ascii 30),
        quantity: uint,
        unit: (string-ascii 10),
        estimated-price: uint,
        actual-price: uint,
        priority: (string-ascii 10),
        purchased-by: (optional principal),
        purchased-at: (optional uint),
        notes: (string-ascii 100),
        is-purchased: bool
    }
)

(define-map household-lists
    { household-id: uint }
    { list-ids: (list 50 uint) }
)

;; Global counters
(define-data-var next-list-id uint u1)
(define-data-var next-item-id uint u1)

;; Public functions

;; Create grocery list
(define-public (create-grocery-list (household-id uint) (name (string-ascii 50)) (target-store (string-ascii 30)))
    (let (
        (list-id (var-get next-list-id))
        (caller tx-sender)
        (current-height stacks-block-height)
    )
        (asserts! (is-household-member caller household-id) ERR-NOT-AUTHORIZED)
        (asserts! (> (len name) u0) ERR-INVALID-INPUT)

        ;; Create list
        (map-set grocery-lists
            { list-id: list-id }
            {
                household-id: household-id,
                name: name,
                creator: caller,
                created-at: current-height,
                target-store: target-store,
                estimated-total: u0,
                actual-total: u0,
                status: "active",
                is-active: true
            }
        )

        ;; Update household lists
        (update-household-lists household-id list-id)

        ;; Increment counter
        (var-set next-list-id (+ list-id u1))

        (ok list-id)
    )
)

;; Add item to list
(define-public (add-item-to-list
    (list-id uint)
    (name (string-ascii 50))
    (category (string-ascii 30))
    (quantity uint)
    (unit (string-ascii 10))
    (estimated-price uint)
    (priority (string-ascii 10))
    (notes (string-ascii 100))
)
    (let (
        (item-id (var-get next-item-id))
        (list-info (unwrap! (map-get? grocery-lists { list-id: list-id }) ERR-LIST-NOT-FOUND))
        (caller tx-sender)
    )
        (asserts! (is-household-member caller (get household-id list-info)) ERR-NOT-AUTHORIZED)
        (asserts! (> (len name) u0) ERR-INVALID-INPUT)
        (asserts! (> quantity u0) ERR-INVALID-INPUT)
        (asserts! (or (is-eq priority "high") (is-eq priority "medium") (is-eq priority "low")) ERR-INVALID-INPUT)

        ;; Add item
        (map-set list-items
            { list-id: list-id, item-id: item-id }
            {
                name: name,
                category: category,
                quantity: quantity,
                unit: unit,
                estimated-price: estimated-price,
                actual-price: u0,
                priority: priority,
                purchased-by: none,
                purchased-at: none,
                notes: notes,
                is-purchased: false
            }
        )

        ;; Update list estimated total
        (update-list-estimated-total list-id estimated-price true)

        ;; Increment counter
        (var-set next-item-id (+ item-id u1))

        (ok item-id)
    )
)

;; Mark item as purchased
(define-public (mark-item-purchased (list-id uint) (item-id uint) (actual-price uint))
    (let (
        (list-info (unwrap! (map-get? grocery-lists { list-id: list-id }) ERR-LIST-NOT-FOUND))
        (item (unwrap! (map-get? list-items { list-id: list-id, item-id: item-id }) ERR-ITEM-NOT-FOUND))
        (caller tx-sender)
        (current-height stacks-block-height)
        (household-id (get household-id list-info))
    )
        (asserts! (is-household-member caller household-id) ERR-NOT-AUTHORIZED)
        (asserts! (not (get is-purchased item)) ERR-ALREADY-PURCHASED)
        (asserts! (has-spending-permission caller household-id actual-price) (ok false)) ;; Allow but don't fail

        ;; Update item
        (map-set list-items
            { list-id: list-id, item-id: item-id }
            (merge item {
                actual-price: actual-price,
                purchased-by: (some caller),
                purchased-at: (some current-height),
                is-purchased: true
            })
        )

        ;; Update list actual total
        (update-list-actual-total list-id actual-price true)

        ;; Update household spending
        (update-household-spending household-id actual-price)

        (ok true)
    )
)

;; Complete shopping list
(define-public (complete-list (list-id uint))
    (let (
        (list-info (unwrap! (map-get? grocery-lists { list-id: list-id }) ERR-LIST-NOT-FOUND))
        (caller tx-sender)
    )
        (asserts! (is-household-member caller (get household-id list-info)) ERR-NOT-AUTHORIZED)

        (map-set grocery-lists
            { list-id: list-id }
            (merge list-info { status: "completed" })
        )

        (ok true)
    )
)

;; Private functions

;; Check if user is household member
(define-private (is-household-member (user principal) (household-id uint))
    (is-some (map-get? household-members { household-id: household-id, member: user }))
)

;; Check spending permission
(define-private (has-spending-permission (user principal) (household-id uint) (amount uint))
    (match (map-get? household-members { household-id: household-id, member: user })
        member-data (<= amount (get spending-limit member-data))
        false
    )
)

;; Update household lists
(define-private (update-household-lists (household-id uint) (list-id uint))
    (let (
        (current-lists (default-to { list-ids: (list) } (map-get? household-lists { household-id: household-id })))
        (updated-list (unwrap-panic (as-max-len? (append (get list-ids current-lists) list-id) u50)))
    )
        (map-set household-lists
            { household-id: household-id }
            { list-ids: updated-list }
        )
    )
)

;; Update list estimated total
(define-private (update-list-estimated-total (list-id uint) (amount uint) (add bool))
    (match (map-get? grocery-lists { list-id: list-id })
        list-info
            (map-set grocery-lists
                { list-id: list-id }
                (merge list-info {
                    estimated-total: (if add
                        (+ (get estimated-total list-info) amount)
                        (- (get estimated-total list-info) amount)
                    )
                })
            )
        false
    )
)

;; Update list actual total
(define-private (update-list-actual-total (list-id uint) (amount uint) (add bool))
    (match (map-get? grocery-lists { list-id: list-id })
        list-info
            (map-set grocery-lists
                { list-id: list-id }
                (merge list-info {
                    actual-total: (if add
                        (+ (get actual-total list-info) amount)
                        (- (get actual-total list-info) amount)
                    )
                })
            )
        false
    )
)

;; Update household spending
(define-private (update-household-spending (household-id uint) (amount uint))
    (match (map-get? households { household-id: household-id })
        household-info
            (map-set households
                { household-id: household-id }
                (merge household-info {
                    current-spent: (+ (get current-spent household-info) amount)
                })
            )
        false
    )
)

;; Read-only functions

;; Get grocery list
(define-read-only (get-grocery-list (list-id uint))
    (map-get? grocery-lists { list-id: list-id })
)

;; Get list item
(define-read-only (get-list-item (list-id uint) (item-id uint))
    (map-get? list-items { list-id: list-id, item-id: item-id })
)

;; Get household lists
(define-read-only (get-household-lists (household-id uint))
    (map-get? household-lists { household-id: household-id })
)

;; Get budget status
(define-read-only (get-budget-status (household-id uint))
    (match (map-get? households { household-id: household-id })
        household-info (some {
            monthly-budget: (get monthly-budget household-info),
            current-spent: (get current-spent household-info),
            remaining: (- (get monthly-budget household-info) (get current-spent household-info)),
            utilization-percent: (/ (* (get current-spent household-info) u100) (get monthly-budget household-info))
        })
        none
    )
)
