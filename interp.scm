;; get the first/second/third element of a list
(define 1st (lambda (p) (car p)))
(define 2nd (lambda (p) (cadr p)))
(define 3rd (lambda (p) (caddr p)))

;; ====================

;; build a new entry from a set of names and a list of values
(define new-entry
  (lambda (a b)
    (cons a (cons b '()))))

(print
 "make a new entry: "
 (new-entry '(1 2 3 4) '(a b b c)))

;; lookup a name in an entry, the function f we here use as a handler
;; for when a name is not found in the entry
(define (lookup-in-entry-helper name names values f)
  (cond
   [(null? names) (f name)]
   [(eq? name (car names)) (car values)]
   [else (lookup-in-entry-helper name (cdr names) (cdr values) f)]))

(define (lookup-in-entry name entry f)
  (lookup-in-entry-helper name (1st entry) (2nd entry) f))

(print
 "lookup a value in an entry: "
 (lookup-in-entry '3
                  (new-entry '(1 2 3 4 5) '(a b c d e))
                  (lambda (x) x)))

;; a table (environment) is a list of entries, this function extends
;; an environment
(define extend-table cons)

(print
 "extend some tables: "
 (extend-table
  (new-entry '(4 5 6 7) '(x y z f))
  (extend-table
   (new-entry '(1 2 3 4) '(a b b c))
   '())))

;; now we should be able to lookup into a table
(define (lookup-in-table name table f)
  (cond
   [(null? table) (f name)]
   [else (lookup-in-entry name (car table)
                          (lambda (x)
                            (lookup-in-table name (cdr table) f)))]))

;; now try to look something up
(print
 "look up an entry in a table: "
 (lookup-in-table '2
                  (extend-table
                   (new-entry '(4 5 6 7) '(x y z f))
                   (extend-table
                    (new-entry '(1 2 3 4) '(a b b c))
                    '()))
                  (lambda (x) x)))

;; notice how the newest entry 'shadows' the earlier entry
(print
 "look up an entry in a table: "
 (lookup-in-table '4
                  (extend-table
                   (new-entry '(4 5 6 7) '(x y z f))
                   (extend-table
                    (new-entry '(1 2 3 4) '(a b b c))
                    '()))
                  (lambda (x) x)))

;; given an atom expression, what is its action?
(define (atom-to-action a)
  (cond
   [(number? a) *const]
   [(eq? a #f) *const]
   [(eq? a #t) *const]
   [(eq? a 'cons) *const]
   [(eq? a 'car) *const]
   [(eq? a 'cdr) *const]
   [(eq? a 'null?) *const]
   [(eq? a 'eq?) *const]
   [(eq? a 'atom?) *const]
   [(eq? a 'zero?) *const]
   [(eq? a 'add1) *const]
   [(eq? a 'sub1) *const]
   [(eq? a 'number?) *const]
   [else *identifier]))

;; given a list expression, what is its action?
(define (list-to-action l)
  (cond
   [(atom? (car l))
    (cond
     [(eq? (car l) 'quote) *quote]
     [(eq? (car l) 'lambda) *lambda]
     [(eq? (car l) 'cond) *cond]
     [else *application])]
   [else *application]))

;; given an expression, determine its action
(define (expression-to-action e)
  (cond
   [(atom? e) (atom-to-action e)]
   [else (list-to-action e)]))

;; determine the meaning in some environment
(define (meaning e table)
  ((expression-to-action e) e table))

;; figure out the value of an expression, the initial table is empty
(define (value e)
  (meaning e '()))

;; the action for a const expression
(define (*const e env)
  (cond
   ;; numbers are constants, i.e. evaluate to themselves
   [(number? e) e]
   ;; truth values are constants as well
   [(eq? e #t) #t]
   [(eq? e #f) #f]
   ;; otherwise we are playing with a primitive
   [else (new-entry 'primitive e)]))

;; helper to grab the text of a quote expression
(define text-of 2nd)

;; the action for a quote expresison
(define (*quote e env)
  (text-of e))

;; the empty table
(define (initial-table name) '())

;; action for an identifier expression, when we meet an identifier, we
;; want to look it up in the current environment
(define (*identifier e env)
  (lookup-in-table e env initial-table))

;; the action for a lambda expression, here we need to save the things
;; needed for the lambda expression to give meaning, we save the
;; environment, its formals (arguments), and its function body, we
;; will use these to evaluate it later
(define (*lambda e env)
  (new-entry 'non-primitive (cons env (cdr e))))

;; helpers to grab the three parts of the above list we made for
;; lambda expressions
(define table-of 1st)
(define formals-of 2nd)
(define body-of 3rd)

;; helpers to grab things from cond lines
(define question-of 1st)
(define answer-of 2nd)
(define (else? e)
  (cond
   [(atom? e) (eq? e 'else)]
   [else #f]))

;; evaluate a cond expression
(define (eval-cond lines table)
  (cond
   ;; if this line is an else line, treat the question as true, and
   ;; evaluate the answer part
   [(else? (question-of (car lines)))
    (meaning (answer-of (car lines)) table)]
   ;; otherwise try to figure out the truth value of the question of
   ;; the line we are at, and evaluate the answer part if true
   [(meaning (question-of (car lines)) table)
    (meaning (answer-of (car lines)) table)]
   ;; if this line was not true, or an else line, move to the next line
   [else (eval-cond (cdr lines) table)]))

;; helper to grab the cond lines from a cond expression
(define cond-lines-of cdr)

;; the action for a cond expression
(define (*cond e env)
  (eval-cond (cond-lines-of e) env))

;; a little test,
(print
 "testing cond in some environment: "
 (*cond
  '(cond
    (coffee klatsch)
    (else party))
  '(((coffee) (#t))
    ((klatsch party)
     (5 (6))))))

;; helpers to grab things from an application statement
(define function-of car)
(define arguments-of cdr)

;; helpers to figure out which kind of function we are dealing with
(define (primitive? f)
  (eq? (function-of f) 'primitive))
(define (non-primitive? f)
  (eq? (function-of f) 'non-primitive))

;; helper to figure out if something is an atom, we need to add this
;; because we also need to handle our own primitive / nonprimitive
;; functions as atoms
(define (:atom? a)
  (cond
   [(atom? a) #t]
   [(null? a) #f]
   [(eq? (car a) 'primitive) #t]
   [(eq? (car a) 'non-primitive) #t]
   [else #f]))

;; these are the primitive functions, we apply to the values given, in
;; effect this is the 'implementation' of the built-in functions of
;; our interpreter
(define (apply-primitive name vals)
  (cond
   [(eq? name 'cons) (cons (1st vals) (2nd vals))]
   [(eq? name 'car) (car (1st vals))]
   [(eq? name 'cdr) (cdr (1st vals))]
   [(eq? name 'null?) (null? (1st vals))]
   [(eq? name 'eq?) (eq? (1st vals) (2nd vals))]
   [(eq? name 'atom?) (:atom? (1st vals))]
   [(eq? name 'zero?) (zero? (1st vals))]
   [(eq? name 'add1) (add1 (1st vals))]
   [(eq? name 'sub1) (sub1 (1st vals))]
   [(eq? name 'number?) (number? (1st vals))]))

(define (apply-closure closure vals)
  (meaning (body-of closure)
           (extend-table (new-entry (formals-of closure) vals)
                         (table-of closure))))

;; the actual apply function
(define (:apply fun vals)
  (cond
   [(primitive? fun) (apply-primitive (2nd fun) vals)]
   [(non-primitive? fun) (apply-closure (2nd fun) vals)]))

;; action for an application (apply the meaning of a function, to the
;; meaning of its arguments)
(define (*application e env)
  (:apply (meaning (function-of e) env)
          (meaning (arguments-of e) env)))


;; now try interpreting something
(print "\n" "Interpret some more in-depth lisp, length-of-list using applicative-order Y-combinator")
(let ((test-list '(a b c (d 4) f 6 7)))
  (print "length of " test-list ": "
         (value
          (((lambda (Y)
              ((lambda (f)
                 (f f))
               (lambda (f)
                 (Y (lambda (x)
                      ((f f) x))))))
            (lambda (length)
              (lambda (l)
                (cond
                 [(null? l) 0]
                 [else (add1 (length (cdr l)))]))))
           test-list))))

;; now we have enough lisp to do most things, we can emulate
;; define/recursion as seen in the example above, so we could implement
;; this entire interpreter in our new lisp, but that is left as an
;; exercise for the reader.
