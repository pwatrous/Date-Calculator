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