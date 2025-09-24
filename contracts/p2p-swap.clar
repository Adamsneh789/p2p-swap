;; p2p-swap
;; Peer-to-peer token swap contract

;; Constants & Error codes
(define-constant ERR-NOT-FOUND u404)
(define-constant ERR-NOT-ACTIVE u100)
(define-constant ERR-NOT-MAKER u101)
(define-constant ERR-BAD-AMOUNT u102)
(define-constant ERR-EXPIRED u103)
(define-constant ERR-TRANSFER u104)
(define-constant ERR-OVERFILL u105)
(define-constant ERR-UNAUTHORIZED u401)
(define-constant ERR-INVALID-FEE u106)
(define-constant ERR-INVALID-TOKEN u107)
(define-constant ERR-INVALID-EXPIRY u108)
(define-constant ERR-MAX-ORDERS u109)
(define-constant ERR-INVALID-CONTRACT u110)
(define-constant MAX-BPS u10000)
(define-constant MAX-FEE-BPS u1000)
(define-constant MAX-ORDERS u100)

;; Define trait for fungible tokens
(define-trait ft-trait
  ((transfer? (uint principal principal (optional (buff 34))) (response bool uint))
   (get-balance (principal) (response uint uint))
   (get-decimals () (response uint uint))
   (get-name () (response (string-ascii 32) uint))
   (get-symbol () (response (string-ascii 32) uint))
   (get-total-supply () (response uint uint))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Storage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contract owner - initialized to contract deployer
(define-data-var contract-owner principal tx-sender)

(define-data-var order-counter uint u0)

;; Protocol fee (basis points). Example: u25 = 0.25%
(define-data-var fee-bps uint u0)
(define-data-var fee-recipient (optional principal) none)

;; Map definitions
(define-map token-whitelist principal bool)

(define-map order-data uint
  {
    maker: principal,
    token-sell: principal,
    token-buy: principal,
    amount-sell: uint,     ;; total escrowed
    amount-buy: uint,      ;; total expected
    filled: uint,          ;; how much of sell already filled
    expiry: uint,          ;; 0 = no expiry; otherwise last-valid block
    active: bool
  })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-read-only (get-contract-owner)
  (var-get contract-owner))

(define-read-only (get-order (id uint))
  (map-get? order-data id))

(define-read-only (get-order-count)
  (var-get order-counter))

(define-read-only (get-fee-config)
  { bps: (var-get fee-bps), recipient: (var-get fee-recipient) })

(define-read-only (get-ft-contract? (token-principal principal)) 
  (match (map-get? token-whitelist token-principal)
    whitelisted (ok whitelisted)
    (err ERR-INVALID-CONTRACT)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Admin: configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public (set-fee (bps uint) (recipient (optional principal)))
  (begin
    (asserts! (is-eq tx-sender (get-contract-owner)) (err ERR-UNAUTHORIZED))
    ;; Check fee is valid (max 10%)
    (asserts! (<= bps MAX-FEE-BPS) (err ERR-INVALID-FEE))
    (var-set fee-bps bps)
    (var-set fee-recipient recipient)
    (ok true)))

(define-public (set-token-whitelist (token-contract <ft-trait>) (is-whitelisted bool))
  (begin
    (asserts! (is-eq tx-sender (get-contract-owner)) (err ERR-UNAUTHORIZED))
    ;; Validate token contract implements trait
    (try! (contract-call? token-contract get-name))
    (map-set token-whitelist (contract-of token-contract) is-whitelisted)
    (ok true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maker: create order (escrows token-sell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public (create-order
  (token-sell <ft-trait>) (token-buy <ft-trait>)
  (amount-sell uint) (amount-buy uint) (expiry uint))
  (begin
    ;; Validate inputs
    (asserts! (> amount-sell u0) (err ERR-BAD-AMOUNT))
    (asserts! (> amount-buy u0) (err ERR-BAD-AMOUNT))
    (asserts! (< (var-get order-counter) MAX-ORDERS) (err ERR-MAX-ORDERS))
    
    ;; Validate token contracts
    (try! (get-ft-contract? (contract-of token-sell)))
    (try! (get-ft-contract? (contract-of token-buy)))
    
    ;; Validate expiry if set
    (asserts! (or (is-eq expiry u0) (> expiry burn-block-height)) (err ERR-INVALID-EXPIRY))
    
    ;; Move sell tokens from maker to contract escrow
    (try! (contract-call? token-sell transfer? amount-sell tx-sender (as-contract tx-sender) none))
    
    ;; Create new order
    (let ((id (+ (var-get order-counter) u1)))
      (var-set order-counter id)
      (map-set order-data id
        {
          maker: tx-sender,
          token-sell: (contract-of token-sell),
          token-buy: (contract-of token-buy),
          amount-sell: amount-sell,
          amount-buy: amount-buy,
          filled: u0,
          expiry: expiry,
          active: true
        })
      (ok id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Taker: fill order
(define-public (fill-order (order-id uint) (sell-amount uint) (token-sell <ft-trait>) (token-buy <ft-trait>))
  (let ((order (unwrap! (get-order order-id) (err ERR-NOT-FOUND)))
        (expiry (get expiry order))
        (filled (get filled order)))
    
    ;; Validate order
    (asserts! (get active order) (err ERR-NOT-ACTIVE))
    (asserts! (> sell-amount u0) (err ERR-BAD-AMOUNT))
    (asserts! (<= (+ filled sell-amount) (get amount-sell order)) (err ERR-OVERFILL))
    (asserts! (or (is-eq expiry u0) (> expiry burn-block-height)) (err ERR-EXPIRED))
    
    ;; Validate token contracts
    (asserts! (is-eq (contract-of token-sell) (get token-sell order)) (err ERR-INVALID-CONTRACT))
    (asserts! (is-eq (contract-of token-buy) (get token-buy order)) (err ERR-INVALID-CONTRACT))
    
    ;; Calculate amounts
    (let ((buy-amount (/ (* sell-amount (get amount-buy order)) (get amount-sell order)))
          (fee (/ (* buy-amount (var-get fee-bps)) MAX-BPS))
          (maker-amount (- buy-amount fee)))
      
      ;; Transfer buy tokens from taker to maker
      (try! (contract-call? token-buy transfer? maker-amount tx-sender (get maker order) none))
      
      ;; Transfer fee if configured
      (match (var-get fee-recipient) recipient 
        (try! (contract-call? token-buy transfer? fee tx-sender recipient none))
        true)

      ;; Transfer sell tokens from contract to taker
      (try! (as-contract (contract-call? token-sell transfer? sell-amount tx-sender tx-sender none)))
      
      ;; Update order state
      (map-set order-data order-id 
        (merge order {
          filled: (+ filled sell-amount),
          active: (<= (+ filled sell-amount) (get amount-sell order))
        }))

      (ok { 
        order-id: order-id,
        amount-sold: sell-amount,
        amount-bought: buy-amount,
        fee: fee
      }))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maker: cancel (refund remaining escrow)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-public (cancel-order (id uint) (token-sell <ft-trait>))
  (let ((order (unwrap! (map-get? order-data id) (err ERR-NOT-FOUND)))
        (rem (- (get amount-sell order) (get filled order))))
    
    (asserts! (is-eq tx-sender (get maker order)) (err ERR-NOT-MAKER))
    (asserts! (get active order) (err ERR-NOT-ACTIVE))
    (asserts! (is-eq (contract-of token-sell) (get token-sell order)) (err ERR-INVALID-CONTRACT))
    
    (begin 
      (if (> rem u0)
          (try! (as-contract (contract-call? token-sell transfer? rem tx-sender tx-sender none)))
          true)
      
      ;; Mark order as inactive
      (map-set order-data id (merge order { active: false }))
      (ok rem))))