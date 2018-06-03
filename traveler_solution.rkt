#lang scheme
;2016400264

;---------------------------Simple Queries---------------------------

;Get Location Information based on City Name
(define (getLocationInfo cityName) (filter (lambda (location) (equal? cityName (car location))) LOCATIONS))

;Takes a location and returns the list involving the neigbor locations
(define (RAILWAY-CONNECTION cityName) (if (null? (getLocationInfo cityName)) '() (caddr (car (getLocationInfo cityName)))))

;Takes a location and returns one night accommodation cost in that location
(define (ACCOMMODATION-COST cityName) (if (null? (getLocationInfo cityName)) 0 (cadr (car (getLocationInfo cityName)))))

;Get Traveler Information based on Traveler Name
(define (getTravelerInfo travelerName) (filter (lambda (traveler) (equal? travelerName (car traveler))) TRAVELERS))

;Takes a traveler and returns a list involving his/her interested cities
(define (INTERESTED-CITIES travelerName) (if (null? (getTravelerInfo travelerName)) '() (cadr (car (getTravelerInfo travelerName)))))

;Takes a traveler and returns a list involving his/her activities
(define (INTERESTED-ACTIVITIES travelerName) (if (null? (getTravelerInfo travelerName)) '() (caddr (car (getTravelerInfo travelerName)))))

;Takes a traveler and returns a list involving his/her hometown
(define (HOME travelerName) (if (null? (getTravelerInfo travelerName)) '() (cadddr (car (getTravelerInfo travelerName)))))

;---------------------------Constructing Lists---------------------------

;Get Traveler Information based on Hometown
(define (getTravelerInfoByHome home) (filter (lambda (traveler) (equal? home (cadddr traveler))) TRAVELERS))

;Takes a location and returns a list involving the travlers whose hometown is that location
(define (TRAVELER-FROM home) (if (null? (getTravelerInfoByHome home)) '() (map (lambda (traveler) (car traveler)) (getTravelerInfoByHome home))))

;The contains? function determines whether an element is a member of a list. 
(define (contains? x list) (if (null? list) #f (if (equal? x (car list)) #t (contains? x (cdr list)))))

;Get Traveler Information based on Interested City
(define (getTravelerInfoByCity city) (filter (lambda (traveler) (contains? city (cadr traveler))) TRAVELERS))

;Takes a city and returns a list of travlers who wanto to visit that city
(define (INTERESTED-IN-CITY city) (if (null? (getTravelerInfoByCity city)) '() (map (lambda (traveler) (car traveler)) (getTravelerInfoByCity city))))

;Get Traveler Information based on Interested Activity
(define (getTravelerInfoByActivity activity) (filter (lambda (traveler) (contains? activity (caddr traveler))) TRAVELERS))

;Takes a activity and returns a list of travlers who enjoy that activity
(define (INTERESTED-IN-ACTIVITY activity) (if (null? (getTravelerInfoByActivity activity)) '() (map (lambda (traveler) (car traveler)) (getTravelerInfoByActivity activity))))

;---------------------------Connected Cities---------------------------

;Continue From Here - Write Recursive Formula
(define (RAILWAY-NETWORK cityName) (if (null? (RAILWAY-CONNECTION cityName)) '() (remove cityName (remove-duplicates (TRAVERSE (RAILWAY-CONNECTION cityName) (length LOCATIONS))))))

(define (TRAVERSE cityList tourLeft) (if (zero? tourLeft) cityList (TRAVERSE (append cityList (remove-duplicates (append-map RAILWAY-CONNECTION cityList))) (- tourLeft 1))))

;---------------------------Expenses---------------------------
;Check whether two list have a common element, if then returns true
(define (inCommon? check_list base_list) (if (or (null? check_list) (null? base_list)) #f (if (eqv? 1 (length check_list)) (contains? (car check_list) base_list) (or (contains? (car check_list) base_list) (inCommon? (cdr check_list) base_list)))))

;Takes a traveler and a location and returns the accommodation cost for the traveler
(define (ACCOMMODATION-EXPENSES travelerName cityName) (cond ((equal? cityName (HOME travelerName))  0) ((inCommon? (INTERESTED-ACTIVITIES travelerName) (cadddr (car (getLocationInfo cityName)))) (* 3 (ACCOMMODATION-COST cityName))) (else (ACCOMMODATION-COST cityName))))

;Takes a traveler and a location and returns the travel cost for the visitor
(define (TRAVEL-EXPENSES travelerName cityName) (cond ((equal? cityName (HOME travelerName))  0) ((contains? cityName (caddr (car (getLocationInfo (HOME travelerName))))) 100) (else 200)))

;Takes a traveler and a location and returns the sum of travel cost and accommodation cost for the traveler
(define (TOTAL-EXPENSES travelerName cityName) (+ (ACCOMMODATION-EXPENSES travelerName cityName) (TRAVEL-EXPENSES travelerName cityName)))

;---------------------------Categorizing Cities---------------------------

;Takes two limits and returns the cities whose accomodation costs are between these limits, inclusively
(define (getLocationInfoInBetween minimumCost maximumCost) (filter (lambda (location) (and (<= minimumCost (cadr location)) (>= maximumCost (cadr location)))) LOCATIONS))

;Takes two limits and returns the cities whose accomodation costs are between these limits, inclusively
(define (IN-BETWEEN minimumCost maximumCost) (if (null? (getLocationInfoInBetween minimumCost maximumCost)) '() (map (lambda (location) (car location)) (getLocationInfoInBetween minimumCost maximumCost))))




