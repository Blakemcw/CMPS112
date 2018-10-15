#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.4 2018-04-11 16:31:36-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath)))

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1))

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename")))

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

(define (symbol-get table key)
    (hash-ref table key))

(define (symbol-put! table key value)
    (hash-set! table key value))

(define *function-table* (make-hash))
(define *label-table* (make-hash))
(define *variable-table* (make-hash))

(define (custom-division x y)
        (if (and (real? x) (real? y))
            (cond 
                ((and (> x 0) (= (+ y 0.0) 0)) +inf.0)
                ((and (< x 0) (= (+ y 0.0) 0)) -inf.0)
                ((and (= x 0) (= (+ y 0.0) 0)) +nan.0)
                (else (/ x y)))
            +nan.0))

(define (custom-log x)
        (cond 
            ((= x 0) +nan.0)
            (else (log x))))

(for-each
    (lambda (pair)
            (symbol-put! *variable-table* (car pair) (cadr pair)))
    `(
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (abs     ,abs)
        (acos    ,acos)
        (asin    ,asin)
        (atan    ,atan)
        (ceil    ,ceiling)
        (cos     ,cos)
        (exp     ,exp)
        (floor   ,floor)
        (log     ,custom-log)
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (log2    ,(lambda (x) (/ (log x) (log 2.0))))
        (round   ,round)
        (sin     ,sin)
        (sqrt    ,sqrt)
        (tan     ,tan)
        (trunc   ,truncate)
        (/       ,custom-division)
        (%       ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+       ,+)
        (-       ,-)
        (^       ,expt)
        (<       ,<)
        (>       ,>)
        (=       ,=)
        (*       ,*)
        (<=      ,<=)
        (>=      ,>=)
        (<>      ,(lambda (x y) (not (= x y))))
     )
)

(define (insert-into-symbol-table program)
    (cond 
        ((null? program) null)
        ((label? (car program))
            (symbol-put! *label-table* (cadar program) program)
            (insert-into-symbol-table (cdr program)))
        ((not (symbol? (car program)))
            (insert-into-symbol-table (cdr program)))))

(define (label? line)
    (cond
        ((null? line) #f)
        ((symbol? (car line)) #t)
        ((not (symbol? (car line))) 
            (label? (cdr line)))))

(define (interpret-program program)
    ;(printf "~n~s~n" *variable-table*)
    (cond 
        ((null? program) null)
        ((not (statement? (car program))) (interpret-program (cdr program)))
        ((statement? (car program))
            (let 
                ([statement-return-value (interpret-statement(extract-statement (car program)))])
                    (if (null? statement-return-value)
                        (interpret-program (cdr program))
                        (interpret-program (symbol-get *label-table* statement-return-value)))))
        (else null)))

;; Recursively checks line until it finds a pair aka a statement. Working :)
(define (statement? line)
    (cond
        ((null? line)   #f)
        ((and 
            (pair? line) 
            (or
                (equal? 'dim    (car line))
                (equal? 'let    (car line))
                (equal? 'goto   (car line))
                (equal? 'if     (car line))
                (equal? 'print  (car line))
                (equal? 'input  (car line))))  
                        #t)
        ((not (pair? (car line))) (statement? (cdr line)))))

(define (extract-statement line)
    (cond
        ((null? (cdr line)) null)
        ((or (not (pair? (cdr line))) (symbol? (cadr line)))  (extract-statement (cdr line)))
        ((pair? (cdr line)) (cadr line))))

(define (interpret-statement statement)
    (cond
        ((equal? 'dim    (car statement))    (interpret-dim statement))
        ((equal? 'let    (car statement))    (interpret-let statement))
        ((equal? 'goto   (car statement))   (interpret-goto statement))
        ((equal? 'if     (car statement))     (interpret-if statement))
        ((equal? 'print  (car statement))  (interpret-print statement))
        ((equal? 'input  (car statement))  (interpret-input statement))))

;; ------------------------------
;;     STATEMENT PROCEDURES
;; ------------------------------

(define (interpret-dim statement)

        (let ([array-name                                  (caadr statement)]
              [array-length   (+ (evaluate-expression (cadadr statement)) 1)])
              (symbol-put! *variable-table* array-name (make-vector array-length 0)))
        null)

(define (interpret-let statement)
        (cond
            ((pair? (cadr statement))
                (vector-set! 
                    (symbol-get *variable-table* (caadr statement)) (evaluate-expression (cadadr statement)) (evaluate-expression (caddr statement)))
            )
            ((not (pair? (cadr statement)))
                (symbol-put! *variable-table* (cadr statement) (evaluate-expression (caddr statement)))
            )
        )
        null
)

(define (interpret-goto statement)
        ;; Returns a label to jump to        
        (cadr statement)
)

(define (interpret-if statement)
        ;; The format of the statement follows,
        ;; (if (relop expression1 expression2) labelToJumpTo)

        (if
            (let ([relop                  (caadr statement)]
                  [expression1           (cadadr statement)]
                  [expression2     (cadr (cdadr statement))])
                  ((evaluate-expression relop) (evaluate-expression expression1) (evaluate-expression expression2)))
            (caddr statement) ;; Label to jump to
            null
        )
)

(define (interpret-print statement)
        (map (lambda (list-of-printables) 
                     (let ([printable list-of-printables])
                     (cond
                        ((string? printable) (printf "~A" printable))
                        ((evaluate-expression printable) (printf " ~s" (evaluate-expression printable))))))
             (cdr statement))
        (printf "~n")
        null
)

(define (update-inputcount num)
        (symbol-put! *variable-table* 'inputcount num)
)

(define (interpret-input statement)
    (update-inputcount 0)
    (map     
            (lambda (variable)
            (let ([user-input (read)])
            (cond
                ((eof-object? user-input) 
                    (update-inputcount (- 1)))
                ((pair? variable)
                    (vector-set! (symbol-get *variable-table* (car variable)) (evaluate-expression (cadr variable)) (evaluate-expression user-input))
                    (update-inputcount (+ (symbol-get *variable-table* 'inputcount) 1)))
                ((not (pair? variable))
                    (symbol-put! *variable-table* variable (evaluate-expression user-input))
                    (update-inputcount (+ (symbol-get *variable-table* 'inputcount) 1))))))
        (cdr statement))
    null)

(define (evaluate-expression expression)
    (cond 
        ((number? expression) expression)
        ((symbol? expression) (hash-ref *variable-table* expression #f))
        ((and [pair? expression] [vector? (symbol-get *variable-table* (car expression))])
            (vector-ref (symbol-get *variable-table* (car expression)) (evaluate-expression (cadr expression)))
        )
        ((pair? expression) (apply (hash-ref *variable-table* (car expression))
                            (map evaluate-expression (cdr expression))))
        (else #f)))

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (insert-into-symbol-table program)
              (interpret-program program))))

(when (terminal-port? *stdin*)
      (main (vector->list (current-command-line-arguments))))