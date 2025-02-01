(define-constant escrow-timeout 1000)

(define-map escrow-contracts
  {id: uint}
  (tuple (party1 principal) (party2 principal) (arbiter principal) (amount uint) (released bool) (timestamp uint)))

(define-private (validate-party (escrow-id uint) (caller principal))
  (let ((escrow (map-get? escrow-contracts {id: escrow-id})))
    (match escrow
      escrow-data
        (if (or (is-eq caller (get party1 escrow-data))
                (is-eq caller (get party2 escrow-data))
                (is-eq caller (get arbiter escrow-data)))
            (ok true)
            (err "Caller is not authorized for this escrow"))
      (err "Escrow contract not found"))))

(define-public (create-escrow (escrow-id uint) (party1 principal) (party2 principal) (arbiter principal) (amount uint))
  (begin
    (asserts! (is-none (map-get? escrow-contracts {id: escrow-id})) (err "Escrow ID already exists"))
    (map-set escrow-contracts {id: escrow-id}
      (tuple (party1 party1) (party2 party2) (arbiter arbiter) (amount amount) (released false) (timestamp (block-height))))
    (ok "Escrow created successfully")))

(define-public (release-funds (escrow-id uint))
  (begin
    (asserts! (is-ok (validate-party escrow-id tx-sender)) (err "Unauthorized access"))
    (let ((escrow (unwrap-panic (map-get escrow-contracts {id: escrow-id}))))
      (asserts! (not (get released escrow)) (err "Funds have already been released"))
      (map-set escrow-contracts {id: escrow-id} (merge escrow (tuple (released true))))
      (ok (tuple (message "Funds released successfully") (to (get party2 escrow)) (amount (get amount escrow)))))))

(define-public (refund (escrow-id uint))
  (begin
    (asserts! (is-ok (validate-party escrow-id tx-sender)) (err "Unauthorized access"))
    (let ((escrow (unwrap-panic (map-get escrow-contracts {id: escrow-id}))))
      (asserts! (>= (- (block-height) (get timestamp escrow)) escrow-timeout) (err "Escrow timeout not reached"))
      (asserts! (not (get released escrow)) (err "Funds have already been released"))
      (map-delete escrow-contracts {id: escrow-id})
      (ok (tuple (message "Funds refunded successfully") (to (get party1 escrow)) (amount (get amount escrow)))))))

;; New reclaim function
(define-public (reclaim-funds (escrow-id uint))
  (begin
    (asserts! (is-ok (validate-party escrow-id tx-sender)) (err "Unauthorized access"))
    (let ((escrow (unwrap-panic (map-get escrow-contracts {id: escrow-id}))))
      (asserts! (>= (- (block-height) (get timestamp escrow)) escrow-timeout) (err "Escrow timeout not reached"))
      (asserts! (not (get released escrow)) (err "Funds have already been released"))
      (map-delete escrow-contracts {id: escrow-id})
      (ok (tuple (message "Funds reclaimed successfully") (to (get party1 escrow)) (amount (get amount escrow)))))))
