#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
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


(define *variable-table* (make-hash))
(define (variable-get key)
        (hash-ref *variable-table* key #f))
(define (variable-put! key value)
        (hash-set! *variable-table* key value))

(for-each
   (lambda (pair)
           (variable-put! (car pair) (cadr pair)))
     (list
        (list 'e       2.718281828459045235360287471352662497757247093)
        (list 'pi      3.141592653589793238462643383279502884197169399)
     ))


(define *function-table* (make-hash))
(define (function-get key )
        (hash-ref *function-table* key #f))
(define (function-put! key value)
        (hash-set! *function-table* key value))
(for-each
   (lambda (pair)
           (function-put! (car pair) (cadr pair)))
     (list
        (list '+      +)
        (list '*      *)
        (list '/      (lambda(dividend divisor)
                      (handle-division dividend divisor)))
        (list '-      -)
        (list '^      expt)
        (list '%      (lambda(x y) (- x (* (truncate(/ x y)) y))))
        (list 'abs    abs)
        (list 'acos   acos)
        (list 'asin   asin)
        (list 'atan   atan)
        (list 'ceil   ceiling)
        (list 'cos    cos)
        (list 'exp    exp)
        (list 'floor  floor)
        (list 'log    (lambda (x) (handle-log x)))
        (list 'log10  (lambda (x) (/ (log x) (log 10.0))))
        (list 'log2   (lambda (x) (/ (log x) (log 2.0))))
        (list 'round  round)
        (list 'sin    sin)
        (list 'sqrt   sqrt)
        (list 'tan    tan)
        (list 'trunc  truncate)
        (list 'dim    (lambda(x)(variable-put! (caar x) 
                      (make-vector (+ (evalexpr (cadar x))1)))))
        (list 'let    (lambda(arguments)(handle-let arguments)))
        (list 'goto   (lambda(argument)(handle-goto argument)))
        (list 'if     (lambda(arguments)(handle-if arguments)))
        (list 'print  (lambda(arguments)(handle-print arguments)))
        (list 'input  (lambda(arguments)(handle-input arguments)))
        (list '<      <)
        (list '<>     (lambda(x y)(if(not (= x y)) #t #f)))
        (list '>      >)
        (list '=      =)
        (list '>=     >=)
        (list '<=     <=)
     ))

(define *label-table* (make-hash))
(define (label-get key)
        (hash-ref *label-table* key #f))
(define (label-put! key value)
        (hash-set! *label-table* key value))


(define (handle-log x)
    (if (= x 0) "-inf"
      (log x)
    )
)
  
(define (handle-division dividend divisor)
    (if (= divisor 0) 
        (cond
           [(= dividend 0) #f]
           [(negative? dividend) "-inf"]
           [(positive? dividend) "inf"]
        )
     (/ dividend divisor)
    )
)

(define (handle-arr pair)
    (let*([vec (variable-get (car pair))]
         [ind (cadr pair)])
    (if (vector? vec)
         (vector-ref vec (evalexpr ind)) 
    -1)))

(define (handle-pair pair)
        (cond
         [(function-get (car pair)) (apply (function-get (car pair))
                                     (map evalexpr (cdr pair)))]

         [(variable-get (car pair)) (handle-arr pair)]
         (else #f)))


(define (evalexpr expr)
   (cond ((number? expr) expr)
         ((symbol? expr) (variable-get expr))
         ((pair? expr) (handle-pair expr))
         (else #f))
)

(define (handle-var-insertion arguments)
  (variable-put! (car arguments) (evalexpr (cadr arguments)))
)

(define (handle-vec-insertion arguments)
  (let* ([possible-vector (variable-get (caar arguments))])
   (if (vector? possible-vector) 
       (vector-set! possible-vector 
        (evalexpr (cadar arguments))(evalexpr (cadr arguments)))
   #f)
  )
)

(define (handle-let statement)
 (let * ([first-argument (car statement)])
 (cond ((symbol? first-argument) (handle-var-insertion  statement))
       ((pair?   first-argument) (handle-vec-insertion  statement))
       (else #f)))
)

(define (handle-goto argument)
   (let* ([where  (label-get (car argument))])
         (if (empty? where) #f (eval-p where))
   )
) 
   
(define (handle-if arguments)
    (let* ([op (function-get (caar arguments))]
           [expr1 (evalexpr (cadar arguments))]
           [expr2 (evalexpr (caddar arguments))])
     (if (op expr1 expr2) (handle-goto (cdr arguments)) #f)
    )
)

(define (handle-print arguments)
    ( if ( empty? arguments) (display "\n")
     (let ([arg (car arguments)]) 
      (cond 
         [(string? arg) (display arg)]
         [else (display (evalexpr arg))]
      )
      (handle-print (cdr arguments))
     )   
    )
)

(define (handle-input-insertion arg number)
    (let* ([new_value (+ 1 (variable-get 'inputcount))])
       (variable-put! 'inputcount new_value)
       (handle-let (list arg number))
    )
)

(define (handle-input arguments)
  (variable-put! 'inputcount 0)
 (define (helper arguments)
  (if (empty? arguments) #t
   (let ([arg (car arguments)] [number (read)])
       (if(eof-object? number) (variable-put! 'inputcount -1)
         (begin (handle-input-insertion arg number)
         (helper (cdr arguments)))
       )
   )
  )
 )
 (helper arguments)
)

(define (evalstatement statement)
  (let* ([func (function-get (car statement))])
     (func (cdr statement))
  )    
)
(define (get-label line)
    (cond
       [(= (length line) 1) '()]
       [(symbol? (cadr line)) (cadr line) ]
       [else '()]
    )
)

(define (handle-labels program)
    (if(empty? program) #t
     (begin
       (let ([label (get-label (car program))])
          (if(empty? label) #f (label-put! label program) )
         
       )
       (handle-labels (cdr program)))
    )
)

(define (get-pair line)
  (cond
    [(empty? line) '()]
    [(pair? (car line)) (car line)]
    [else (get-pair (cdr line))]
  )  
)

(define (eval-p program)
    (if (empty? program) (exit #t)
      (let ([line (get-pair (car program))])
         (if (empty? line) #f (evalstatement line))
       (eval-p (cdr program))
      )
    ) 
)


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
    (handle-labels program))

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program)
              (eval-p program)
       )))

(main (vector->list (current-command-line-arguments)))
