;;; MINIPROJECT IN PROGRAMMING PARADIGMS
;;; Software, Aalborg University
;;; 
;;; Lukas Rønsholt
;;; 20166653
;;; Lransh16@student.aau.dk
;;;
;;; Calls to solve the diffrent tasks in the assignemt is showed in the bottom of the file.
;;; The scheme system used for the assignemt is Racket.
;;;


#lang racket

;;; Setup

; Load in the students file
(define students (call-with-input-file "students.rkt" read))


;;; Main procedures, main procedures to solve the tasks.

; Create groups with group sizes defined in form using a provided soring function
; @input sl: list of students
; @input gsl: group formation, either in the form a list with group sizes or a single number repersenting the amount of groups
; @input pred: the predicate used to create the groups.
(define (group-students sl gsl pred)
  (cond 
    [(list? gsl) (run-list-pred sl gsl pred)]
    [else (pred sl gsl 1)])
  )


;; Main task predicates

; Creates random groups based on a group lenght list
; @input sl: list of students
; @input gsl: group formation, either in the form a list with group sizes or a single number
; @input start: the group id to start at, should always be 1.
(define (random-grouping sl gsl start)
  (let [(shuffled (shuffle sl))]
    (if (null? gsl)
    '()
    (append 
      (create-group-by-lenght shuffled (car gsl) start) 
      (random-grouping (list-tail shuffled (car gsl)) (cdr gsl) (+ start 1))
    ))))

; Creates groups based counting up to k, then starts again untill all students are in a group.
; @input sl: list of students
; @input k: the number repersenting the amount of groups
; @input start: the group id to start at, should always be 1.
(define (counting-grouping sl k start)
  (if (or (null? sl) (equal? k 0))
    '()
    (append (create-groups-by-count sl k start) (counting-grouping (my-list-tail sl k) k start))))

; Creates groups balenced with gender and ethnicality 
; @input sl: list of students
; @input k: the number repersenting the amount of groups
; @input start: the group id to start at, should always be 1.
(define (balanced-grouping sl k start)
  (let [(f-male (get-all-foreign-male sl))
        (f-female (get-all-foreign-female sl))
        (d-male (get-all-danish-male sl))
        (d-female (get-all-danish-female sl))]
    (counting-grouping (append d-male f-female f-male d-female) k start)))

; Generate random groups which upholds a predicate, with a timeout of 1000 tries.
; @input predicate: the predicate that all groups should uphold
; @output procedure: new procedure to be used in group students
(define (predicate-random-grouping predicate)
  (lambda(sl gsl start)
    (generate-until-match sl gsl start predicate 1000)))

; Generate random groups until one grouping match the predicate
; @input sl: list of students
; @input gsl: group formation, either in the form a list with group sizes or a single number
; @input start: the group id to start at, should always be 1.
; @input pred: the predicate to run on the groups
; @input timeout: the amount of times to try and satisfy the predicate
(define (generate-until-match sl gsl start pred timeout)
  (let [(grouping (random-grouping sl gsl start))]
    (cond 
      [(check-grouping grouping pred sl) grouping]
      [(= timeout 0) '"No group could be made using that predicate"]
      [else (generate-until-match sl gsl start pred (- timeout 1))])))

;; predicate grouping predicates

; predicate to check if there are at least n students of an age in a group
; @input age: the minimum age
; @input n: the minimum amount of students
; @output prodecure with input: prodecure that takes in a group and will check it for the students of age
(define n-students-of-age
  (lambda(age n)
    (lambda(group)
      (let [(result (group-checker group (student-above-age age)))]
        (>= (length (filter true? result)) n)))))

; predicate to check if all the groups only consists of females
; @output prodecure with input: prodecure that takes in a group
(define all-female-group
  (lambda(group) 
    (let [(result (group-checker group (match-student-index-pred 2 "female")))]
        (>= (length (filter true? result)) (length group)))))

; predicate to check that no students in a group have the same age
; @output prodecure with input: prodecure that takes in a group
(define no-student-of-same-age
  (lambda(group) 
    (eq? (check-duplicates (get-group-ages group)) #f)))


;;; Helping procedures used to solve bigger problems

; Get the sum of integers in a list
(define (sum L)
  (apply + L))

; Helper procedure to get the max group size
(define (get-max lst max id)
  (let ((size (get-group-size lst id)))
    (cond
      [(= size 0) max]
      [(> size max) (get-max lst size (+ id 1))]
      [else (get-max lst max (+ id 1))])))

; Get the size of a group
(define (get-group-size lst id)
  (length(get-group-by-id lst id)))

; Helper procedure to get the smallest group size
(define (get-min lst min id)
  (let ((size (get-group-size lst id)))
    (cond
      [(= size 0) min]
      [(< size min) (get-min lst size (+ id 1))]
      [else (get-min lst min (+ id 1))])))

; run a predicate with a list of group lenghts
(define (run-list-pred sl gsl pred)
  (if (equal? (length sl) (sum gsl))
      (pred sl gsl 1)
      '"Group formation dont match up with the number of students"))

; Create a group with same id by a given lenght on a list of students
(define (create-group-by-lenght sl size id)
  (if (= size 0)
    '()
    (cons (assign-group (car sl) id) (create-group-by-lenght (cdr sl) (- size 1) id))))

; Assign a student to a group id
(define (assign-group student id)
  (cons id (car student)))

; Create groups using a count
(define (create-groups-by-count sl k group)
  (if (or (> group k) (null? sl))
    '()
    (cons (assign-group (car sl) group) (create-groups-by-count (cdr sl) k (+ group 1)))))

; List tail procedure which will return the empty list if n is bigger then the list
(define (my-list-tail lst n)
  (cond 
    [(= n 0) lst]
    [(> n (length lst)) '()]
    [else (my-list-tail (cdr lst) (- n 1))]))

(define (get-all-foreign-male sl)
  (get-all-male (get-all-foreigners sl)))

(define (get-all-danish-male sl)
  (get-all-male (get-all-danish sl)))

(define (get-all-foreign-female sl)
  (get-all-female (get-all-foreigners sl)))

(define (get-all-danish-female sl)
  (get-all-female (get-all-danish sl)))

(define (get-all-foreigners sl)
  (filter (match-student-index-not-pred 3 "Danish") sl))

(define (get-all-danish sl)
  (filter (match-student-index-pred 3 "Danish") sl))

(define (get-all-female sl)
  (filter (match-student-index-pred 2 "female") sl))

(define (get-all-male sl)
  (filter (match-student-index-pred 2 "male") sl))

; Check that a predicate is uphold on all groups in a grouping
(define (check-grouping groups pred sl)
  (check-grouping-helper groups pred sl (get-group-amount groups))) 

; helper to go though each of the groups in a grouping
(define (check-grouping-helper groups pred sl groupid)
  (if (= groupid 0)
    #t
    (if (pred (get-group-students-by-id (get-group-by-id groups groupid) sl))
      (check-grouping-helper groups pred sl (- groupid 1))
      #f)))

; check if a single group uphold a predicate 
(define (group-checker group pred)
  (check-group-for-pred group pred '()))

; Check the predicate on each student in a group
(define (check-group-for-pred group pred result)
  (if (null? group)
    result
    (check-group-for-pred (cdr group) pred (cons (pred (car group)) result))))


;;; Student procedures

(define (get-group-students-by-id group students)
  (if (null? group)
    '()
    (cons (get-student-by-id (cdar group) students) (get-group-students-by-id (cdr group) students))))

(define (get-student-by-id id students)
  (car (filter (match-student-index-pred 0 id) students)))

(define (get-student-age student)
  (list-ref student 4))

; Predicate to check if a student is above an age
(define student-above-age
  (lambda(age)
    (lambda(student)
      (>= (get-student-age student) age))))

;;; Group procedures

; Get all ages in a group
(define (get-group-ages group)
  (if (null? group)
    '()
    (cons (get-student-age (car group)) (get-group-ages (cdr group)))))

; Get a group by its id
(define (get-group-by-id lst id)
  (filter (match-group-id-pred id) lst))

; Get the number of groups in a grouping
(define (get-group-amount lst)
  (car(last(sort lst sort-by-group-id))))

; Get the largest group size in a grouping
(define (get-max-group-size lst)
  (get-max lst 0 1))

; Get the smallest group size in a grouping
(define (get-min-group-size lst)
  (get-min lst (length students) 1))


;;; Predicate procedures

; predicate to match group id
(define match-group-id-pred
  (lambda(id)
    (lambda(x)
      (equal? (car x) id))))

; Sort group by id predicatate
(define (sort-by-group-id x y)
  (< (car x) (car y)))

; predicate to find students where its index matches a given input
(define match-student-index-pred
  (lambda(index matches)
    (match-pred index matches string=?)))

; Not equals version of string=?
(define string!=?
  (lambda(x y)
    (not (string=? x y))))

; predicate to find students where its index dont matches a given input
(define match-student-index-not-pred 
  (lambda(index matches)
    (match-pred index matches string!=?)))

; Match a student using a given match predicate, where index have to match matches using the given matcher.
(define match-pred
  (lambda(index matches matcher) ; the prop we want to look at, and what we want it to be
    (lambda(student) ; the student we want to check on, given by filter
      (matcher (list-ref student index) matches))))

; Check if a elements is true
(define true?
  (lambda(x)
    (equal? x #t)))


;;; Main



(define small-student-list (list 
  '("20204988" "Adam Carl Hjorth" "male" "Syrian" 22)
  '("20204882" "Adam Justesen" "female" "Syrian" 22)
  '("20209775" "Adam Marius Holst" "female" "Danish" 23)
  '("20207662" "Adam Nohr Brodersen" "female" "Danish" 24)
  '("20202610" "Agnes Johnsen" "female" "Danish" 25)
  '("20207159" "Ahmed Ghufran" "female" "Syrian" 26)
  '("20205976" "Aksel Bundgaard" "female" "Syrian" 27)
  '("20201188" "Aksel Hedegaard" "female" "Syrian" 28)
  '("20201654" "Aksel Ibsen" "female" "Danish" 29)
  '("20202858" "Aksel Matthias Jacobsen" "female" "Danish" 29)
 ))

;; Test on small small student list
;; To easiere check the results given.

; (group-students small-student-list '(5 5) random-grouping)
; (group-students small-student-list 2 counting-grouping)
; (group-students small-student-list 2 balanced-grouping)
; (group-students small-student-list '(5 5) (predicate-random-grouping (n-students-of-age 20 2)))
; (group-students small-student-list '(5 5) (predicate-random-grouping all-female-group))
; (group-students small-student-list '(5 5) (predicate-random-grouping no-student-of-same-age))

;; Run on full student list
;; uncomment the task you want to run

;; Random grouping
; (group-students students '(100 100) random-grouping)

;; Grouping by counting
; (group-students students 2 counting-grouping)

;; Balanced grouping by counting
; (group-students students 2 balanced-grouping)

;; Random grouping with group predicate
; (group-students students '(100 100) (predicate-random-grouping (n-students-of-age 20 2)))
; (group-students students '(100 100) (predicate-random-grouping all-female-group))
; (group-students students '(100 100) (predicate-random-grouping no-student-of-same-age))