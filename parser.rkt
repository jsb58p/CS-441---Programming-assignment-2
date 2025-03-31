#lang racket

;; A simple recursive-descent parser for a calculator-style language
;; Grammar:
;; program -> {stmt_list} $$
;; stmt_list -> stmt stmt_list
;; stmt_list -> epsilon
;; stmt -> id = expr;
;;       | if (expr) stmt_list endif;
;;       | read id;
;;       | write expr;
;; expr -> id etail
;;       | num etail
;; etail -> + expr
;;       | - expr
;;       | compare expr
;;       | epsilon
;; id -> [a-zA-Z]+
;; num -> numsign digit digit*
;; numsign -> + | - | epsilon
;; compare -> < | <= | > | >= | == | != 

;; Define structure for position in input
(struct position (line col) #:transparent)

;; Define structure for token
(struct token (type val pos) #:transparent)

;; Define structure for syntax error
(struct syntax-error (message pos) #:transparent)

;; Define structure for parse error
(struct parse-error (message) #:transparent)

;; Lexer - converts input string to tokens
(define (tokenize input-string)
  (define lines (string-split input-string "\n"))
  
  (define (char-list-from-lines lines)
    (for/list ([line lines]
               [line-num (in-naturals 1)])
      (for/list ([ch (string->list line)]
                 [col-num (in-naturals 1)])
        (cons ch (position line-num col-num)))))
  
  (define char-lists (char-list-from-lines lines))
  (define flattened-chars (apply append char-lists))
  
  (define (get-tokens chars tokens)
    (cond
      [(null? chars) (reverse tokens)]
      [(char-whitespace? (caar chars)) 
       (get-tokens (cdr chars) tokens)]
      [(char-alphabetic? (caar chars)) 
       (let-values ([(id rest) (get-identifier chars)])
         (get-tokens rest (cons id tokens)))]
      [(char-numeric? (caar chars)) 
       (let-values ([(num rest) (get-number chars)])
         (get-tokens rest (cons num tokens)))]
      [else
       (case (caar chars)
         ; Add these to the get-tokens function's else clause
         [(#\<) 
          (cond 
            [(and (not (null? (cdr chars))) (char=? (caadr chars) #\=))
             (get-tokens (cddr chars) (cons (token 'LESS_EQUAL "<=" (cdar chars)) tokens))]
            [else (get-tokens (cdr chars) (cons (token 'LESS "<" (cdar chars)) tokens))])]
         [(#\>) 
          (cond 
            [(and (not (null? (cdr chars))) (char=? (caadr chars) #\=))
             (get-tokens (cddr chars) (cons (token 'GREATER_EQUAL ">=" (cdar chars)) tokens))]
            [else (get-tokens (cdr chars) (cons (token 'GREATER ">" (cdar chars)) tokens))])]
         [(#\=) 
           (cond 
             [(and (not (null? (cdr chars))) (char=? (caadr chars) #\=))
              (get-tokens (cddr chars) (cons (token 'EQUAL "==" (cdar chars)) tokens))]
             [else (get-tokens (cdr chars) (cons (token 'EQUALS "=" (cdar chars)) tokens))])]
         [(#\!) 
           (cond 
             [(and (not (null? (cdr chars))) (char=? (caadr chars) #\=))
              (get-tokens (cddr chars) (cons (token 'NOT_EQUAL "!=" (cdar chars)) tokens))]
             [else (get-tokens (cdr chars) (cons (token 'ERROR "Unexpected !" (cdar chars)) tokens))])]
         [(#\=) (get-tokens (cdr chars) (cons (token 'EQUALS "=" (cdar chars)) tokens))]
         [(#\;) (get-tokens (cdr chars) (cons (token 'SEMICOLON ";" (cdar chars)) tokens))]
         [(#\() (get-tokens (cdr chars) (cons (token 'LPAREN "(" (cdar chars)) tokens))]
         [(#\)) (get-tokens (cdr chars) (cons (token 'RPAREN ")" (cdar chars)) tokens))]
         [(#\+) (get-tokens (cdr chars) (cons (token 'PLUS "+" (cdar chars)) tokens))]
         [(#\-) (get-tokens (cdr chars) (cons (token 'MINUS "-" (cdar chars)) tokens))]
         [(#\$) (if (and (not (null? (cdr chars))) (char=? (caadr chars) #\$))
                    (get-tokens (cddr chars) (cons (token 'END "$$" (cdar chars)) tokens))
                    (get-tokens (cdr chars) (cons (token 'ERROR (format "Unexpected character: ~a" (caar chars)) (cdar chars)) tokens)))]
         [else (get-tokens (cdr chars) (cons (token 'ERROR (format "Unexpected character: ~a" (caar chars)) (cdar chars)) tokens))])]))
  
  (define (get-identifier chars)
    (define (collect-chars chars collected)
      (cond
        [(null? chars) (values (reverse collected) '())]
        [(char-alphabetic? (caar chars)) 
         (collect-chars (cdr chars) (cons (caar chars) collected))]
        [else (values (reverse collected) chars)]))
    
    (let-values ([(chars-list rest) (collect-chars chars '())])
      (let* ([id-str (list->string chars-list)]
             [id-token (cond
                        [(string=? id-str "if") (token 'IF "if" (cdar chars))]
                        [(string=? id-str "endif") (token 'ENDIF "endif" (cdar chars))]
                        [(string=? id-str "read") (token 'READ "read" (cdar chars))]
                        [(string=? id-str "write") (token 'WRITE "write" (cdar chars))]
                        [else (token 'ID id-str (cdar chars))])])
        (values id-token rest))))
  
  (define (get-number chars)
    (define (collect-digits chars collected [sign 1])
      (cond
        [(null? chars) (values (reverse collected) '() sign)]
        [(char-numeric? (caar chars)) 
         (collect-digits (cdr chars) (cons (caar chars) collected) sign)]
        [else (values (reverse collected) chars sign)]))
    
    (define sign 1)
    (define start-pos (cdar chars))
    
    ; Check for sign
    (define-values (updated-chars updated-sign)
      (cond
        [(null? chars) (values chars sign)]
        [(char=? (caar chars) #\+) (values (cdr chars) 1)]
        [(char=? (caar chars) #\-) (values (cdr chars) -1)]
        [else (values chars sign)]))
    
    (let-values ([(digits-list rest sign) (collect-digits updated-chars '() updated-sign)])
      (if (null? digits-list)
          (values (token 'ERROR "Invalid number" start-pos) chars)
          (let* ([num-str (list->string digits-list)]
                 [num-val (* sign (string->number num-str))])
            (values (token 'NUM (number->string num-val) start-pos) rest)))))
  
  (get-tokens flattened-chars '()))

;; Parser - parses tokens into a parse tree or returns an error
(define (parse-from-tokens tokens)
  (define current-token-index 0)
  
  (define (current-token)
    (if (< current-token-index (length tokens))
        (list-ref tokens current-token-index)
        (token 'EOF "" (position 0 0))))
  
  (define (advance)
    (set! current-token-index (add1 current-token-index))
    (current-token))
  
  (define (match expected-type)
    (let ([ct (current-token)])
      (if (eq? (token-type ct) expected-type)
          (begin
            (advance)
            ct)
          (raise (syntax-error (format "Expected ~a but got ~a" expected-type (token-type ct))
                              (token-pos ct))))))
  
  (define (parse-program)
    (let ([stmts (parse-stmt-list)])
      (match 'END)
      (list 'program stmts)))
  
  (define (parse-stmt-list)
    (let ([ct (current-token)])
      (case (token-type ct)
        [(ID READ WRITE IF) 
         (let ([stmt (parse-stmt)]
               [stmt-list (parse-stmt-list)])
           (cons stmt stmt-list))]
        [else '()])))
  
  (define (parse-stmt)
    (let ([ct (current-token)])
      (case (token-type ct)
        [(ID) 
         (let ([id (match 'ID)])
           (match 'EQUALS)
           (let ([expr (parse-expr)])
             (match 'SEMICOLON)
             (list 'assign (token-val id) expr)))]
        [(IF) 
         (begin
           (match 'IF)
           (match 'LPAREN)
           (let ([expr (parse-expr)])
             (match 'RPAREN)
             (let ([stmt-list (parse-stmt-list)])
               (match 'ENDIF)
               (match 'SEMICOLON)
               (list 'if expr stmt-list))))]
        [(READ) 
         (begin
           (match 'READ)
           (let ([id (match 'ID)])
             (match 'SEMICOLON)
             (list 'read (token-val id))))]
        [(WRITE) 
         (begin
           (match 'WRITE)
           (let ([expr (parse-expr)])
             (match 'SEMICOLON)
             (list 'write expr)))]
        [else (raise (syntax-error (format "Expected statement but got ~a" (token-type ct))
                                 (token-pos ct)))])))
  
  (define (parse-expr)
    (let ([ct (current-token)])
      (case (token-type ct)
        [(ID) 
         (let ([id (match 'ID)])
           (let ([etail (parse-etail)])
             (if (null? etail)
                 (list 'expr (token-val id))
                 (list 'expr (token-val id) (car etail) (cadr etail)))))]
        [(NUM PLUS MINUS) 
         (let ([num (parse-num)])
           (let ([etail (parse-etail)])
             (if (null? etail)
                 (list 'expr num)
                 (list 'expr num (car etail) (cadr etail)))))]
        [else (raise (syntax-error (format "Expected expression but got ~a" (token-type ct))
                                 (token-pos ct)))])))
  
  (define (parse-etail)
    (let ([ct (current-token)])
      (case (token-type ct)
        [(PLUS) 
         (begin
           (match 'PLUS)
           (let ([expr (parse-expr)])
             (list '+ expr)))]
        [(MINUS) 
         (begin
           (match 'MINUS)
           (let ([expr (parse-expr)])
             (list '- expr)))]
        [(LESS LESS_EQUAL GREATER GREATER_EQUAL EQUAL NOT_EQUAL)
       (begin
         (let ([op (token-type ct)]
               [op-val (token-val ct)])
           (advance)
           (let ([expr (parse-expr)])
             (list op expr))))]
        [else '()])))
  
  (define (parse-num)
    (let ([ct (current-token)])
      (case (token-type ct)
        [(NUM) (token-val (match 'NUM))]
        [(PLUS) 
         (begin
           (match 'PLUS)
           (token-val (match 'NUM)))]
        [(MINUS) 
         (begin
           (match 'MINUS)
           (string-append "-" (token-val (match 'NUM))))]
        [else (raise (syntax-error (format "Expected number but got ~a" (token-type ct))
                                 (token-pos ct)))])))
  
  (with-handlers ([syntax-error? (lambda (err) (cons 'error err))])
    (let ([tree (parse-program)])
      (if (eq? (token-type (current-token)) 'EOF)
          (cons 'accept tree)
          (cons 'error (syntax-error (format "Unexpected token: ~a" (token-type (current-token)))
                                    (token-pos (current-token))))))))

;; Main parse function - reads from file and parses
(define (parse filename)
  (if (file-exists? filename)
      (let* ([input (file->string filename)]
             [tokens (tokenize input)]
             [error-tokens (filter (lambda (t) (eq? (token-type t) 'ERROR)) tokens)])
        (if (null? error-tokens)
            (let ([result (parse-from-tokens tokens)])
              (if (eq? (car result) 'accept)
                  (begin
                    (printf "Accept\n")
                    (printf "Parse Tree: ~a\n" (cdr result))
                    "Accept")
                  (let ([err (cdr result)])
                    (define pos (syntax-error-pos err))
                    (define line-num (position-line pos))
                    (define line-text 
                      (let ([lines (string-split input "\n")])
                        (if (< line-num (length lines))
                            (list-ref lines (sub1 line-num))
                            "")))
                    (printf "Syntax Error at line ~a: ~a\n" 
                            line-num 
                            (syntax-error-message err))
                    (printf "Line ~a: ~a\n" line-num line-text)
                    (format "Syntax Error at line ~a" line-num))))
            (let ([err (car error-tokens)])
              (define pos (token-pos err))
              (define line-num (position-line pos))
              (define line-text 
                (let ([lines (string-split input "\n")])
                  (if (< line-num (length lines))
                      (list-ref lines (sub1 line-num))
                      "")))
              (printf "Lexical Error at line ~a: ~a\n" 
                      line-num 
                      (token-val err))
              (printf "Line ~a: ~a\n" line-num line-text)
              (format "Lexical Error at line ~a" line-num))))
      (begin
        (printf "Error: File ~a not found\n" filename)
        (format "Error: File ~a not found" filename))))

;; Test the parser
(module+ main
  (printf "Parser ready. Use (parse \"filename\") to parse a file.\n"))
(parse "input.txt")
