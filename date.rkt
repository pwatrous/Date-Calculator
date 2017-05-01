;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname date) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; Converts day of week into its respective number
(define (day-to-number day-of-week)
  (cond [(equal? day-of-week "Monday") 0]
        [(equal? day-of-week "Tuesday") 1]
        [(equal? day-of-week "Wednesday") 2]
        [(equal? day-of-week "Thursday") 3]
        [(equal? day-of-week "Friday") 4]
        [(equal? day-of-week "Saturday") 5]
        [(equal? day-of-week "Sunday") 6]))

(check-expect (day-to-number "Monday") 0)
(check-expect (day-to-number "Wednesday") 2)

; Converts month in to its respective numerical code
(define (month-to-number month)
  (cond [(equal? month "January") 6] ; becomes 5 on leap years
        [(equal? month "February") 2] ; becomes 1 on leap years
        [(equal? month "March") 2]
        [(equal? month "April") 5]
        [(equal? month "May") 0]
        [(equal? month "June") 3]
        [(equal? month "July") 5]
        [(equal? month "August") 1]
        [(equal? month "September") 4]
        [(equal? month "October") 6]
        [(equal? month "November") 2]
        [(equal? month "December") 4]))

(check-expect (month-to-number "February") 2)
(check-expect (month-to-number "October") 6)

;; Determines whether a year is a leap year or not
(define (is-leap-year? year)
  (cond [(equal? (modulo year 400) 0) #t]
        [(equal? (modulo year 100) 0) #f]
        [(equal? (modulo year 4) 0) #t]
        [else #f]))

(check-expect (is-leap-year? 2016) #t)
(check-expect (is-leap-year? 1700) #f)
(check-expect (is-leap-year? 2400) #t)
(check-expect (is-leap-year? 2017) #f)

;; Calculates codes of leap years between 2000-2096
(define (21st-century-leap-year-to-number year)
  (cond [(and (>= year 2000) (<= year 2024))
         (cond [(equal? year 2000) 0]
               [(equal? year 2004) 5]
               [(equal? year 2008) 3]
               [(equal? year 2012) 1]
               [(equal? year 2016) 6]
               [(equal? year 2020) 4]
               [(equal? year 2024) 2])]
         [else (21st-century-leap-year-to-number (- year 28))]))

(check-expect (21st-century-leap-year-to-number 2000) 0)
(check-expect (21st-century-leap-year-to-number 2028) 0)
(check-expect (21st-century-leap-year-to-number 2032) 5)
(check-expect (21st-century-leap-year-to-number 2064) 3)
(check-expect (21st-century-leap-year-to-number 2072) 6)
(check-expect (21st-century-leap-year-to-number 2096) 1)

;; Calculates all 21st century years
(define (21st-century-year-to-number year)
  (cond [(is-leap-year? year) (21st-century-leap-year-to-number year)]
        [else (+ (21st-century-year-to-number (- year 1)) 1)]))

(check-expect (21st-century-year-to-number 2000) 0)
(check-expect (21st-century-year-to-number 2009) 4)
(check-expect (21st-century-year-to-number 2001) 1)
(check-expect (21st-century-year-to-number 2010) 5)

;; Calculates year code for any year before 2000
(define (before-2000-to-number year)
  (cond [(and (>= year 1900) (< year 2000))
         (+ (21st-century-year-to-number (+ year 100)) 1)]
        [(and (>= year 1800) (< year 1900))
         (+ (21st-century-year-to-number (+ year 200)) 3)]
        [(and (>= year 1700) (< year 1800))
         (+ (21st-century-year-to-number (+ year 300)) 5)]
        [else (21st-century-year-to-number (+ year 400))]))

(check-expect (before-2000-to-number 1900) 1)
(check-expect (before-2000-to-number 1800) 3)
(check-expect (before-2000-to-number 1700) 5)
(check-expect (before-2000-to-number 1600) 0)
(check-expect (before-2000-to-number 1999) 5)
(check-expect (before-2000-to-number 1810) 8)

;; Calculates year code for any year after 2100
(define (after-2100-to-number year)
  (cond [(and (>= year 2100) (< year 2200))
         (+ (21st-century-year-to-number (- year 100)) 5)]
        [(and (>= year 2200) (< year 2300))
         (+ (21st-century-year-to-number (- year 200)) 3)]
        [(and (>= year 2300) (< year 2400))
         (+ (21st-century-year-to-number (- year 300)) 1)]
        [else (21st-century-year-to-number (- year 400))]))

(check-expect (after-2100-to-number 2100) 5)
(check-expect (after-2100-to-number 2200) 3)
(check-expect (after-2100-to-number 2300) 1)
(check-expect (after-2100-to-number 2400) 0)
(check-expect (after-2100-to-number 2109) 9)
(check-expect (after-2100-to-number 2201) 4)
(check-expect (after-2100-to-number 2301) 2)
