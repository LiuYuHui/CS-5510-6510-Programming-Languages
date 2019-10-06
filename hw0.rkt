#lang plai-typed





(define (plural (s : string)) : string
   (cond
     [ ( > (string-length s) 1) (string-append (substring s 0 1)
                                               (plural (substring s 1 (string-length s))))]
     [else ( if (string=? s "y")
                "ies"
                (string-append s "s")
            )]))

(test (plural "y") "ies")
(test (plural "") "s")
(test (plural "baby") "babies")
(test (plural "fish") "fishs")


 (define-type Light
    [bulb (watts : number)
          (technology : symbol)]
    [candle (inches : number)])

(define (enery-usage (l : Light)) : number
    (type-case Light l
      [bulb (w t) (* 0.024 w)]
      [else 0]))

(test (enery-usage (bulb 100 'test)) 2.4)
(test (enery-usage (candle 10)) 0)
(test (enery-usage (bulb 100.0 'halogen)) 2.4)
(test (enery-usage (bulb 75 'led)) 1.8)
(test (enery-usage (candle 10.0)) 0)



(define (sub-one n)
  (if (> n 0)
      (- n 1)
      n))
(define (use-for-one-hour (l : Light)) : Light
    (type-case Light l
      [candle (inches) (candle (sub-one inches))]
      [else l]))

(test (use-for-one-hour (bulb 100.0 'halogen)) (bulb 100.0 'halogen))
(test (use-for-one-hour (candle 10.0)) (candle 9.0))
(test (use-for-one-hour (candle 1.0)) (candle 0.0))
(test (use-for-one-hour (candle 0.0)) (candle 0.0))