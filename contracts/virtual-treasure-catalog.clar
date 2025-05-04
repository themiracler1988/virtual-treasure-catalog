;; Virtual deTreasure Catalog Manager
;; This contract manages a catalog of virtual treasures with comprehensive verification, storage, and permission controls. It provides robust mechanisms for creating, updating, and transferring ownership of virtual treasures while maintaining data integrity and proper access restrictions.

;; -----------------------------
;; Core Constants
;; -----------------------------
;; Contract administrator (set at deployment)
(define-constant SYSTEM-ADMINISTRATOR tx-sender)

;; Response codes for operations
(define-constant ERROR-INVALID-LABEL (err u303))
(define-constant ERROR-INVALID-DIMENSION (err u304))
(define-constant ERROR-PERMISSION-DENIED (err u305))
(define-constant ERROR-ADMIN-RESTRICTED (err u307))
(define-constant ERROR-ACCESS-VIOLATION (err u308))
(define-constant ERROR-ITEM-NOT-FOUND (err u301))
(define-constant ERROR-DUPLICATE-ENTRY (err u302))


(define-constant ERROR-INVALID-TARGET (err u306))

;; -----------------------------
;; State Management
;; -----------------------------
;; Track total items in the catalog
(define-data-var catalog-item-count uint u0)

;; Primary treasure catalog storage
(define-map treasure-catalog
  { item-code: uint }
  {
    label: (string-ascii 64),
    originator: principal,
    dimension: uint,
    timestamp: uint,
    notes: (string-ascii 128),
    categories: (list 10 (string-ascii 32))
  }
)

;; Permission matrix for treasure viewing
(define-map permission-matrix
  { item-code: uint, viewer: principal }
  { can-view: bool }
)

;; -----------------------------
;; Validation Functions
;; -----------------------------
;; Validate if user is the originator of an item
(define-private (verify-originator? (item-code uint) (originator principal))
  (match (map-get? treasure-catalog { item-code: item-code })
    catalog-entry (is-eq (get originator catalog-entry) originator)
    false
  )
)

;; Extract dimension value for a catalog item
(define-private (extract-dimension (item-code uint))
  (default-to u0 
    (get dimension 
      (map-get? treasure-catalog { item-code: item-code })
    )
  )
)

;; Validate single category format
(define-private (validate-category? (category (string-ascii 32)))
  (and 
    (> (len category) u0)
    (< (len category) u33)
  )
)

;; Validate all categories in a list
(define-private (validate-categories? (categories (list 10 (string-ascii 32))))
  (and
    (> (len categories) u0)
    (<= (len categories) u10)
    (is-eq (len (filter validate-category? categories)) (len categories))
  )
)

;; Verify text field length
(define-private (validate-text-length (text (string-ascii 64)) (min-chars uint) (max-chars uint))
  (and 
    (>= (len text) min-chars)
    (<= (len text) max-chars)
  )
)

;; Increment catalog counter safely
(define-private (increment-catalog-counter)
  (let ((current-value (var-get catalog-item-count)))
    (var-set catalog-item-count (+ current-value u1))
    (ok current-value)
  )
)

;; Verify if item exists in the catalog
(define-private (item-registered? (item-code uint))
  (is-some (map-get? treasure-catalog { item-code: item-code }))
)


;; -----------------------------
;; Public Operations
;; -----------------------------

;; Validate label format
(define-public (check-label-format (label (string-ascii 64)))
  (ok (and (> (len label) u0) (<= (len label) u64)))
)

;; Transfer item ownership to another user
(define-public (transfer-treasure (item-code uint) (new-originator principal))
  (let
    (
      (catalog-entry (unwrap! (map-get? treasure-catalog { item-code: item-code }) ERROR-ITEM-NOT-FOUND))
    )
    (asserts! (item-registered? item-code) ERROR-ITEM-NOT-FOUND)
    (asserts! (is-eq (get originator catalog-entry) tx-sender) ERROR-PERMISSION-DENIED)

    ;; Update catalog with new owner
    (map-set treasure-catalog
      { item-code: item-code }
      (merge catalog-entry { originator: new-originator })
    )
    (ok true)
  )
)

;; Register a new treasure in the catalog
(define-public (register-treasure (label (string-ascii 64)) (dimension uint) (notes (string-ascii 128)) (categories (list 10 (string-ascii 32))))
  (let
    (
      (new-item-code (+ (var-get catalog-item-count) u1))
    )
    ;; Input validation
    (asserts! (and (> (len label) u0) (< (len label) u65)) ERROR-INVALID-LABEL)
    (asserts! (and (> dimension u0) (< dimension u1000000000)) ERROR-INVALID-DIMENSION)
    (asserts! (and (> (len notes) u0) (< (len notes) u129)) ERROR-INVALID-LABEL)
    (asserts! (validate-categories? categories) ERROR-INVALID-LABEL)

    ;; Store new catalog entry
    (map-insert treasure-catalog
      { item-code: new-item-code }
      {
        label: label,
        originator: tx-sender,
        dimension: dimension,
        timestamp: block-height,
        notes: notes,
        categories: categories
      }
    )

    ;; Initialize permission for originator
    (map-insert permission-matrix
      { item-code: new-item-code, viewer: tx-sender }
      { can-view: true }
    )

    ;; Update counter and return new code
    (var-set catalog-item-count new-item-code)
    (ok new-item-code)
  )
)

;; Retrieve item description
(define-public (fetch-item-notes (item-code uint))
  (let
    (
      (catalog-entry (unwrap! (map-get? treasure-catalog { item-code: item-code }) ERROR-ITEM-NOT-FOUND))
    )
    (ok (get notes catalog-entry))
  )
)

;; Check if user has viewing permissions
(define-public (verify-viewing-permission (item-code uint) (viewer principal))
  (let
    (
      (permission-entry (map-get? permission-matrix { item-code: item-code, viewer: viewer }))
    )
    (ok (is-some permission-entry))
  )
)

;; Count categories for an item
(define-public (count-item-categories (item-code uint))
  (let
    (
      (catalog-entry (unwrap! (map-get? treasure-catalog { item-code: item-code }) ERROR-ITEM-NOT-FOUND))
    )
    (ok (len (get categories catalog-entry)))
  )
)

;; Modify existing catalog entry
(define-public (modify-treasure (item-code uint) (updated-label (string-ascii 64)) (updated-dimension uint) (updated-notes (string-ascii 128)) (updated-categories (list 10 (string-ascii 32))))
  (let
    (
      (catalog-entry (unwrap! (map-get? treasure-catalog { item-code: item-code }) ERROR-ITEM-NOT-FOUND))
    )
    ;; Validation
    (asserts! (item-registered? item-code) ERROR-ITEM-NOT-FOUND)
    (asserts! (is-eq (get originator catalog-entry) tx-sender) ERROR-PERMISSION-DENIED)
    (asserts! (and (> (len updated-label) u0) (< (len updated-label) u65)) ERROR-INVALID-LABEL)
    (asserts! (and (> updated-dimension u0) (< updated-dimension u1000000000)) ERROR-INVALID-DIMENSION)
    (asserts! (and (> (len updated-notes) u0) (< (len updated-notes) u129)) ERROR-INVALID-LABEL)
    (asserts! (validate-categories? updated-categories) ERROR-INVALID-LABEL)

    ;; Update catalog entry
    (map-set treasure-catalog
      { item-code: item-code }
      (merge catalog-entry { 
        label: updated-label, 
        dimension: updated-dimension, 
        notes: updated-notes, 
        categories: updated-categories 
      })
    )
    (ok true)
  )
)

;; Remove item from catalog permanently
(define-public (remove-treasure (item-code uint))
  (let
    (
      (catalog-entry (unwrap! (map-get? treasure-catalog { item-code: item-code }) ERROR-ITEM-NOT-FOUND))
    )
    (asserts! (item-registered? item-code) ERROR-ITEM-NOT-FOUND)
    (asserts! (is-eq (get originator catalog-entry) tx-sender) ERROR-PERMISSION-DENIED)

    ;; Delete from catalog
    (map-delete treasure-catalog { item-code: item-code })
    (ok true)
  )
)

