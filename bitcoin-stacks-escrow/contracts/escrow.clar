(define-constant escrow-timeout 1000)

(define-map escrow-contracts
  (principal principal)
  (tuple (party1 principal) (party2 principal) (arbiter principal) (amount uint) (released bool)))

;; Create a new escrow contract
(define-public (create-escrow (party1 principal) (party2 principal) (arbiter principal) (amount uint))
  (begin
    (map-set escrow-contracts party1
      (tuple (party1 party1) (party2 party2) (arbiter arbiter) (amount amount) (released false)))
    (ok "Escrow created successfully")))

;; Deposit funds into escrow
(define-public (deposit (party principal) (amount uint))
  (begin
    (let ((escrow (map-get? escrow-contracts party)))
      (match escrow
        (some (tuple (party1 p1) (party2 p2) (arbiter arb) (amount a) (released r)))
        (begin
          (map-set escrow-contracts party (tuple (party1 p1) (party2 p2) (arbiter arb) (amount (+ a amount)) (released r)))
          (ok "Deposit successful"))
        (none (err "Escrow does not exist"))))))

;; Release funds to the agreed party
(define-public (release-funds (party principal))
  (begin
    (let ((escrow (map-get? escrow-contracts party)))
      (match escrow
        (some (tuple (party1 p1) (party2 p2) (arbiter arb) (amount a) (released r)))
        (begin
          (if (is-true released)
              (ok "Funds already released")
              (begin
                (map-set escrow-contracts party (tuple (party1 p1) (party2 p2) (arbiter arb) (amount a) (released true)))
                (ok "Funds released successfully"))))
        (none (err "Escrow does not exist"))))))

;; Resolve a dispute and release funds to the correct party
(define-public (resolve-dispute (arbiter principal) (winner-party principal))
  (begin
    (let ((escrow (map-get? escrow-contracts winner-party)))
      (match escrow
        (some (tuple (party1 p1) (party2 p2) (arbiter arb) (amount a) (released r)))
        (begin
          (if (is-eq arbiter arb)
              (begin
                (map-set escrow-contracts winner-party (tuple (party1 p1) (party2 p2) (arbiter arb) (amount a) (released true)))
                (ok "Dispute resolved and funds released"))
              (err "Only arbiter can resolve dispute")))
        (none (err "Escrow does not exist"))))))
