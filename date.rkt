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
  (cond [(equal? month "January") 6]
        [(equal? month "February") 2]
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
        [(equal? (modulo year 4) 0) #t]))

(check-expect (is-leap-year? 2016) #t)
(check-expect (is-leap-year? 1700) #f)
(check-expect (is-leap-year? 2400) #t)
