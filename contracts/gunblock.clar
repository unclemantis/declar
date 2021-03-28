(define-constant ERR_NO_OWNER_PRINCIPAL u1)
(define-constant ERR_UNABLE_TO_APPEND_OWNER u2)
(define-constant ERR_UNABLE_TO_SET_OWNER u3)
(define-constant ERR_UNABLE_TO_GET_OWNERS u4)
(define-constant ERR_UNABLE_TO_REGISTER_OWNER u5)
(define-constant ERR_UNABLE_TO_REGISTER_GUN u6)

(define-non-fungible-token gunblock (string-ascii 50))

(define-map guns { serial-number: (string-ascii 50) }
                 { manufacturer: (string-ascii 50),
                   manufacture-date: int,
                   model: (string-ascii 50),
                   caliber: (string-ascii 50),
                   finish: (string-ascii 50),
                   shipment-date: int })

(define-map owners { owner-principal: principal }
                   (list 100 
                     { serial-number: (string-ascii 50),
                       sale-date: int,
                       photo-url: (string-ascii 255),
                       sale-price: uint,
                       condition: (string-ascii 50),
                       modifications: (string-ascii 255),
                       atf-gunsmith-name: (string-ascii 100)}))

(define-public (register-gun (serial-number (string-ascii 50))
                             (manufacturer (string-ascii 50))
                             (manufacture-date int)
                             (model (string-ascii 50))
                             (caliber (string-ascii 50))
                             (finish (string-ascii 50))
                             (shipment-date int)
                             (sale-date int)
                             (photo-url (string-ascii 255))
                             (sale-price uint)
                             (condition (string-ascii 50))
                             (modifications (string-ascii 50))
                             (atf-gunsmith-name (string-ascii 100)))

  (if (map-insert guns { serial-number: serial-number }
                       { manufacturer: manufacturer,
                         manufacture-date: manufacture-date,
                         model: model,
                         caliber: caliber,
                         finish: finish,
                         shipment-date: shipment-date })

      (match (register-owner serial-number 
                             sale-date
                             photo-url
                             sale-price
                             condition
                             modifications
                             atf-gunsmith-name)
        register (mint-token serial-number)
        error (err ERR_UNABLE_TO_REGISTER_OWNER))
    (err ERR_UNABLE_TO_REGISTER_GUN)))

(define-private (get-guns-by-owner)
  (let ((guns-by-owner (unwrap! (map-get? owners { owner-principal: tx-sender }) (err ERR_NO_OWNER_PRINCIPAL))))
    (ok guns-by-owner)))

(define-private (append-owner (serial-number (string-ascii 50))
                              (sale-date int)
                              (photo-url (string-ascii 255))
                              (sale-price uint)
                              (condition (string-ascii 50))
                              (modifications (string-ascii 50))
                              (atf-gunsmith-name (string-ascii 100)))
  (match (get-guns-by-owner)
    ownrs (if (map-set owners { owner-principal: tx-sender }
                (unwrap! (as-max-len? (append ownrs { serial-number: serial-number,
                                                      sale-date: sale-date,
                                                      photo-url: photo-url,
                                                      sale-price: sale-price,
                                                      condition: condition,
                                                      modifications: modifications,
                                                      atf-gunsmith-name: atf-gunsmith-name}) u100)
                  (err ERR_UNABLE_TO_APPEND_OWNER)))
                (ok true)
                (err ERR_UNABLE_TO_SET_OWNER))
    error (err ERR_UNABLE_TO_GET_OWNERS)))

(define-public (register-owner (serial-number (string-ascii 50))
                               (sale-date int)
                               (photo-url (string-ascii 255))
                               (sale-price uint)
                               (condition (string-ascii 50))
                               (modifications (string-ascii 50))
                               (atf-gunsmith-name (string-ascii 100)))

  (if (map-insert owners { owner-principal: tx-sender }
                         (list
                           { serial-number: serial-number,
                             sale-date: sale-date,
                             photo-url: photo-url,
                             sale-price: sale-price,
                             condition: condition,
                             modifications: modifications,
                             atf-gunsmith-name: atf-gunsmith-name}))
    (ok true)
    (append-owner serial-number
                  sale-date
                  photo-url
                  sale-price
                  condition
                  modifications
                  atf-gunsmith-name)))

(define-private (mint-token (serial-number (string-ascii 50)))
  (nft-mint? gunblock serial-number tx-sender))