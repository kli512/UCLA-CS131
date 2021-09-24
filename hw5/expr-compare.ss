#lang racket
(provide (all-defined-out))

(define (expr-compare a b)
    (if (and (list? a) (list? b))                                       ; if both lists, then we need to analyze them
        (cond
            [(or (empty? a) (empty? b))                                 ; if either empty, just combine into if % list
                (list 'if '% a b)]
            [(= (length a) (length b))                                  ; if list lengths equal, then it gets interesting...
                (let ([af (car a)] [bf (car b)] )                       ; get the front of a and b
                    (cond
                        [(and (equal? af 'quote) (equal? bf 'quote))    ; if both 'quote, then compare quotes
                            (cmp-quotes a b)]
                        [(or (equal? af 'quote) (equal? bf 'quote))     ; if only one 'quote, then no comparison - TODO check (expr-compare ''(a b) '(a b))
                            (list 'if '% a b)]
                        [(and (equal? af 'if) (equal? bf 'if))          ; if both 'if, then compare if operands
                            (cons 'if (cmp-lists (cdr a) (cdr b)))]
                        [(or (equal? af 'if) (equal? bf 'if))           ; if only one 'if, then no comparison
                            (list 'if '% a b)]
                        [(and (equal-lambda? af) (equal-lambda? bf))    ; if both lambda, then do lambda comparison
                            (cmp-lambdas a b)]
                        [(or (equal-lambda? af) (equal-lambda? bf))     ; if only one lambda, then no comparison
                            (list 'if '% a b)]
                        [else                                           ; treat them as standard lists
                            (cmp-lists a b)]
                    )
                )]
            [else
                (cmp-lists a b)]                                        ; if different length lists, then do a standard list compare
        )
        (cmp-symbol a b)                                                ; and if they're both non-lists (atomic), then simply compare their symbols
    )
)

(define (cmp-symbol a b)
    (cond
        [(equal? a b)                       ; if symbols equal, send either back
            a]
        [(and (boolean? a) (boolean? b))    ; if symbols are both boolean and not equal...
            (if a
                '%                          ; if a true, then %
                (list 'not '%)              ; if a false, then not %
            )]
        [else
            (list 'if '% a b)]              ; otherwise, combine into if % list
    )
)

(define (cmp-lists a b)
    (if (= (length a) (length b))               ; check if lengths equal
        (if (empty? a)                          ; if a is empty, then b is too 
            '()                                 ;   and return '()
            (cons
                (expr-compare (car a) (car b))  ; otherwise, apply expr-compare to the first item in the list
                (cmp-lists (cdr a) (cdr b))     ; and continue cmp-listing the cdr's of both
            )
        )
        (list 'if '% a b)                       ; if non-equal length, combine into if % list
    )
)

(define (cmp-quotes a b)
    (if (equal? a b)                ; if quote expr completely equal,
        (car (cdr a))               ;   just return either
        (list 'if '% a b)           ; if not equal, then combine into if % list
    )
)

(define (equal-lambda? l)                   ; helper function to check if a symbol is either form of lambda
    (or (equal? l 'lambda) (equal? l'λ))
)

(define (cmp-lambdas a b)
    (let ([la (car a)] [lb (car b)] [af (car (cdr a))] [bf (car (cdr b))])  ; get a and b's lambda and formals
        (if (= (length af) (length bf))                                     ; check if formal lengths are same
            (let ([cf (map bind-vars af bf)])                               ;   if they are, combine into combined formals
                (append
                    (list (if (equal? la lb) la 'λ))                        ; pick which lambda to return in new lambda
                    (list cf)                                               ; use the combined formals in the new lambda
                    (expr-compare                                           ; Now, we do the hard part. We apply expr-compare
                        (replace-formals (car (cdr (cdr a))) af cf)         ;   to the expressions of a and b AFTER replacing 
                        (replace-formals (car (cdr (cdr b))) bf cf)         ;   them with the new formals
                    )
                )
            )
            (list 'if '% a b)                                               ; if formal lengths nonequal, combine into if % list
        )
    )
)

(define (bind-vars a b) ; helper function to bind variables together
    (if (equal? a b)
        a                                                                           ; if same, no change
        (string->symbol (string-append (symbol->string a) "!" (symbol->string b)))  ; otherwise, concatenate with a ! in between
    )
)

(define (replace-formals expr ofs nfs)
    (if (empty? ofs)                                                                ; try to replace all of in ofs with nf in nfs
        (list expr)                                                                 ; if no ofs left, return the expr
        (replace-formals (fix-formal expr (car ofs) (car nfs)) (cdr ofs) (cdr nfs)) ; apply each of/nf pair recursively to expr
    )
)

(define (expr-fix-formal expr of nf)
    (if (empty? expr)
        '()                                     ; if expr is empty, return empty list
        (cons
            (fix-formal (car expr) of nf)       ; otherwise, recurse through expr replacing OF's with NF
            (expr-fix-formal (cdr expr) of nf)  ; recursing
        )
    )
)

(define (fix-formal stmt of nf)
    (if (list? stmt)                                ; if we find a list as an element of an expr, then we treat it differently
        (cond
            [(empty? stmt)                          ; if its an empty list, simply send back an empty list
                '()]
            [(or                                    ; if it is either
                (equal? (car stmt) 'quote)          ; a quote or
                (and                            
                    (equal-lambda? (car stmt))      ; a lambda
                    (member of (car (cdr stmt)))    ; AND the OF we're trying to replace gets redefined (rescoped) in this lambda...
                )
            )
                stmt]                               ; then we don't modify the list (stmt) at all
            [else
                (expr-fix-formal stmt of nf)]       ; otherwise, we recurse through it trying to replace all OF with NF
        )
        (if (equal? stmt of)                        ; if we find a non-list element (atomic), we simply
            nf                                      ;   replace with NF if it is OF
            stmt                                    ;   or return it back if not
        )
    )
)

(define (test-expr-compare x y)
    (and
        (equal? (eval x) (eval `(let '([% #t]), (expr-compare x y))))   ; tests if eval on x is the same as eval in expr-compare with %=#t
        (equal? (eval y) (eval `(let '([% #f]), (expr-compare x y))))   ; tests if eval on y is the same as eval in expr-compare with %=#f
    )
)

(define test-expr-x '(+ 3 ((lambda (a b) (list a b)) 1 2) ''(f h) '(x y z) ''(l m)))
(define test-expr-y '(+ 2 ((lambda (a c) (list a c)) 1 2) ''(f g) '(x y n) ''(l m)))
