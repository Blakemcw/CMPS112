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
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

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

(for-each
    (lambda (pair)
            (symbol-put! *function-table* (car pair) (cadr pair)))
    `(

        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+       ,+)
        (^       ,expt)
        (ceil    ,ceiling)
        (exp     ,exp)
        (floor   ,floor)
        (log     ,log)
        (sqrt    ,sqrt)
        (abs     ,abs)
        (acos    ,acos)
        (asin    ,atan)
     ))

(define (insert-into-symbol-table program)
    (map (lambda (line) (insert line)) program)
)

(define (insert line)
    (cond 
        ((null? (cdr line)) null)
        ((symbol? (cadr line))
            (symbol-put! *label-table* (cadr line) line)
        )
    )
)

(define (interpret-program program)
    (cond 
        ((null? program) null)
        ((not (statement? (car program))) (interpret-program (cdr program)))
        
        ;; if it is a statement it will be interpreted by another function in place of the printf statement below
        ((statement? (car program)) 
            (interpret-statement (car program)) 
            (interpret-program (cdr program))
        )
    )
)

;; Recursively checks line until it finds a pair aka a statement. Working :)
(define (statement? line)
    (cond 
        ((null? (cdr line)) #f)
        ((pair? (cdr line)) #t)
        ((not (pair? (cdr line))) (statement? (cdr line))))
)

(define (interpret-statement statement)
        (cond 
            ((= "dim"   (car statement))    (interpret-dim statement))
            ((= "let"   (car statement))    (interpret-let statement))
            ((= "goto"  (car statement))   (interpret-goto statement))
            ((= "if"    (car statement))     (interpret-if statement))
            ((= "print" (car statement))  (interpret-print statement))
            ((= "input" (car statement))  (interpret-input statement))
        )
)
;; ------------------------------
;;     STATEMENT PROCEDURES
;; ------------------------------

(define (interpret-dim statement)

)

(define (interpret-let statement)

)

(define (interpret-goto statement)

)

(define (interpret-if statement)

)

(define (interpret-print statement)

)

(define (interpret-input statement)

)

(define (evaluate-expression expression)
        
)

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (insert-into-symbol-table program)
              (interpret-program program)))
)

(when (terminal-port? *stdin*)
      (main (vector->list (current-command-line-arguments))))