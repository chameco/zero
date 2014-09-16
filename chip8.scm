(use (srfi 1)
     (srfi 4)
     ncurses)

(define logfile (open-output-file "error.log"))

(define (debug msg)
  (format logfile "~A~%" msg)
  msg)

(define (make-instruction opcode callback) (list opcode callback)) ; (callback opcode memory regs screen delay sound stack)
(define (instruction-opcode ins) (car ins))
(define (instruction-callback ins) (cadr ins))

(define (timer-down t)
  (let ([dec (- t 1)])
   (if (>= dec 0) dec 0)))

(define (get-mem mem i)
  (u8vector-ref mem i))

(define (set-mem mem i value)
  (u8vector-set! mem i (inexact->exact (modulo value #xFF)))
  mem)

(define (get-sprite mem index size)
  (let loop ([s size]
             [i index])
    (if (= s 0)
      '()
      (cons (get-mem mem i) (loop (- s 1) (+ i 1))))))

(define (clear-screen)
  (make-u8vector (* 64 32) 0))

(define (get-pixel screen x y)
  (u8vector-ref screen (+ (* y 64) x)))

(define (set-pixel screen x y)
  (u8vector-set! screen (+ (* y 64) x) 1)
  screen)

(define (unset-pixel screen x y)
  (u8vector-set! screen (+ (* y 64) x) 0)
  screen)

(define (get-reg regs reg)
  (cdr (assq reg regs)))

(define (set-reg regs reg value)
  (map (lambda (r)
         (if (eq? (car r) reg)
           (if (or (eq? reg 'pc) (eq? reg 'i))
             (cons reg (modulo value #xFFF))
             (cons reg (modulo value #xFF)))
           r))
       regs))

(define (push-stack stack pc)
  (vector-set! (car stack) (+ (cdr stack) 1) pc)
  (cons (car stack) (+ (cdr stack) 1)))

(define (get-stack-top stack)
  (vector-ref (car stack) (cdr stack)))

(define (pop-stack stack)
  (cons (car stack) (- (cdr stack) 1)))

(define (flip-alist lst)
  (let loop ([l lst])
   (if (null? l)
     '()
     (cons (cons (cdar l) (caar l)) (loop (cdr l))))))

(define *chip8-keypad*
  '((#x1 . #\1)
    (#x2 . #\2)
    (#x3 . #\3)
    (#xC . #\4)
    (#x4 . #\q)
    (#x5 . #\w)
    (#x6 . #\e)
    (#xD . #\r)
    (#x7 . #\a)
    (#x8 . #\s)
    (#x9 . #\d)
    (#xE . #\f)
    (#xA . #\z)
    (#x0 . #\x)
    (#xB . #\c)
    (#xF . #\v)))

(define *chip8-keypad-reverse* (flip-alist *chip8-keypad*))

(define (key-pressed hex)
  ((condition-case
     (lambda ()
       (let ([c (getch)])
        (eq? c (cdr (assq hex *chip8-keypad*)))))
     [(exn ncurses) #f])))

(define (poll-key)
  (nodelay (stdscr) #f)
  (let ([c (getch)])
   (nodelay (stdscr) #t)
   (cdr (assq c *chip8-keypad-reverse*))))

(define *chip8-registers*
  '((#x0 . #x00)
    (#x1 . #x00)
    (#x2 . #x00)
    (#x3 . #x00)
    (#x4 . #x00)
    (#x5 . #x00)
    (#x6 . #x00)
    (#x7 . #x00)
    (#x8 . #x00)
    (#x9 . #x00)
    (#xA . #x00)
    (#xB . #x00)
    (#xC . #x00)
    (#xD . #x00)
    (#xE . #x00)
    (#xF . #x00)
    (i  . #x000)
    (pc . #x200)))

(define *chip8-opcodes*
  (list
    (make-instruction #x0000 (lambda (opcode mem regs screen delay-timer sound-timer stack)
                               (cond [(eq? opcode #x00E0) (values mem regs (clear-screen) delay-timer sound-timer stack)]
                                     [(eq? opcode #x00EE) (values mem (set-reg regs 'pc (get-stack-top stack)) screen delay-timer sound-timer (pop-stack stack))]
                                     [else (format #t "invalid opcode ~X~%" opcode)])))
    (make-instruction #x1000 (lambda (opcode mem regs screen delay-timer sound-timer stack)
                               (values mem (set-reg regs 'pc (- (bitwise-and opcode #x0FFF) 2)) screen delay-timer sound-timer stack)))
    (make-instruction #x2000 (lambda (opcode mem regs screen delay-timer sound-timer stack)
                               (values mem (set-reg regs 'pc (- (bitwise-and opcode #x0FFF) 2)) screen delay-timer sound-timer (push-stack stack (get-reg regs 'pc)))))
    (make-instruction #x3000 (lambda (opcode mem regs screen delay-timer sound-timer stack)
                               (if (= (get-reg regs (arithmetic-shift (bitwise-and opcode #x0F00) -8)) (bitwise-and opcode #x00FF))
                                 (values mem (set-reg regs 'pc (+ (get-reg regs 'pc) 2)) screen delay-timer sound-timer stack)
                                 (values mem regs screen delay-timer sound-timer stack))))
    (make-instruction #x4000 (lambda (opcode mem regs screen delay-timer sound-timer stack)
                               (if (= (get-reg regs (arithmetic-shift (bitwise-and opcode #x0F00) -8)) (bitwise-and opcode #x00FF))
                                 (values mem regs screen delay-timer sound-timer stack)
                                 (values mem (set-reg regs 'pc (+ (get-reg regs 'pc) 2)) screen delay-timer sound-timer stack))))
    (make-instruction #x5000 (lambda (opcode mem regs screen delay-timer sound-timer stack)
                               (if (= (get-reg regs (arithmetic-shift (bitwise-and opcode #x0F00) -8)) (get-reg regs (arithmetic-shift (bitwise-and opcode #x00F0) -4)))
                                 (values mem (set-reg regs 'pc (+ (get-reg regs 'pc) 2)) screen delay-timer sound-timer stack)
                                 (values mem regs screen delay-timer sound-timer stack))))
    (make-instruction #x6000 (lambda (opcode mem regs screen delay-timer sound-timer stack)
                               (values mem (set-reg regs (arithmetic-shift (bitwise-and opcode #x0F00) -8) (bitwise-and opcode #x00FF)) screen delay-timer sound-timer stack)))
    (make-instruction #x7000 (lambda (opcode mem regs screen delay-timer sound-timer stack)
                               (values mem (set-reg regs (arithmetic-shift (bitwise-and opcode #x0F00) -8) (+ (get-reg regs (arithmetic-shift (bitwise-and opcode #x0F00) -8)) (bitwise-and opcode #x00FF))) screen delay-timer sound-timer stack)))
    (make-instruction #x8000 (lambda (opcode mem regs screen delay-timer sound-timer stack)
                               (let ([hex (bitwise-and opcode #x000F)]
                                     [x (arithmetic-shift (bitwise-and opcode #x0F00) -8)]
                                     [y (arithmetic-shift (bitwise-and opcode #x00F0) -4)])
                                (cond [(= hex #x0) (values mem (set-reg regs x (get-reg regs y)) screen delay-timer sound-timer stack)]
                                      [(= hex #x1) (values mem (set-reg regs x (bitwise-ior (get-reg regs x) (get-reg regs y))) screen delay-timer sound-timer stack)]
                                      [(= hex #x2) (values mem (set-reg regs x (bitwise-and (get-reg regs x) (get-reg regs y))) screen delay-timer sound-timer stack)]
                                      [(= hex #x3) (values mem (set-reg regs x (bitwise-xor (get-reg regs x) (get-reg regs y))) screen delay-timer sound-timer stack)]
                                      [(= hex #x4) (let* ([sum (+ (get-reg regs x) (get-reg regs y))]
                                                          [regs (if (> sum 255) (set-reg regs #xF 1) regs)])
                                                     (values mem (set-reg regs x (modulo sum 255)) screen delay-timer sound-timer stack))]
                                      [(= hex #x5) (let* ([diff (- (get-reg regs x) (get-reg regs y))]
                                                          [regs (if (<= 0 diff) (set-reg regs #xF 1) regs)])
                                                     (values mem (set-reg regs x (modulo diff 255)) screen delay-timer sound-timer stack))]
                                      [(= hex #x6) (let ([regs (if (= (bitwise-and x #x01) 1) (set-reg regs #xF 1) regs)])
                                                     (values mem (set-reg regs x (/ (get-reg regs x) 2)) screen delay-timer sound-timer stack))]
                                      [(= hex #x7) (let* ([diff (- (get-reg regs y) (get-reg regs x))]
                                                          [regs (if (<= 0 diff) (set-reg regs #xF 1) regs)])
                                                     (values mem (set-reg regs x (modulo diff 255)) screen delay-timer sound-timer stack))]
                                      [(= hex #xE) (let ([regs (if (= (arithmetic-shift opcode -7) 1) (set-reg regs #xF 1) regs)])
                                                     (values mem (set-reg regs x (* (get-reg regs x) 2)) screen delay-timer sound-timer stack))]))))
    (make-instruction #x9000 (lambda (opcode mem regs screen delay-timer sound-timer stack)
                               (if (= (get-reg regs (arithmetic-shift (bitwise-and opcode #x0F00) -8)) (arithmetic-shift (bitwise-and opcode #x00F0) -4))
                                 (values mem regs screen delay-timer sound-timer stack)
                                 (values mem (set-reg regs 'pc (+ (get-reg regs 'pc) 2)) screen delay-timer sound-timer stack))))
    (make-instruction #xA000 (lambda (opcode mem regs screen delay-timer sound-timer stack)
                               (values mem (set-reg regs 'i (bitwise-and opcode #x0FFF)) screen delay-timer sound-timer stack)))
    (make-instruction #xB000 (lambda (opcode mem regs screen delay-timer sound-timer stack)
                               (values mem (set-reg regs 'pc (- (+ (get-reg regs #x0) (bitwise-and opcode #x0FFF)) 2)) screen delay-timer sound-timer stack)))
    (make-instruction #xC000 (lambda (opcode mem regs screen delay-timer sound-timer stack)
                               (values mem (set-reg regs (arithmetic-shift (bitwise-and opcode #x0F00) -8) (bitwise-and (random 256) (bitwise-and opcode #x00FF))) screen delay-timer sound-timer stack)))
    (make-instruction #xD000 (lambda (opcode mem regs screen delay-timer sound-timer stack)
                               (let* ([x (get-reg regs (arithmetic-shift (bitwise-and opcode #x0F00) -8))]
                                      [y (get-reg regs (arithmetic-shift (bitwise-and opcode #x00F0) -4))]
                                      [len (bitwise-and opcode #x000F)]
                                      [maxx (+ x 8)]
                                      [composite (let loopy ([sy (cons screen 0)]
                                                             [iy y]
                                                             [sprite (get-sprite mem (get-reg regs 'i) len)])
                                                   (if (null? sprite)
                                                     sy
                                                     (loopy (let loopx ([sx sy]
                                                                        [ix x]
                                                                        [row (car sprite)])
                                                              (if (< ix maxx)
                                                                (loopx (if (= (arithmetic-shift (bitwise-and row #b10000000) -7) 1)
                                                                         (if (= (get-pixel (car sx) ix iy) 1)
                                                                           (cons (unset-pixel (car sx) ix iy) 1)
                                                                           (cons (set-pixel (car sx) ix iy) 0))
                                                                         sx)
                                                                       (+ ix 1)
                                                                       (arithmetic-shift row 1))
                                                                sx))
                                                            (+ iy 1)
                                                            (cdr sprite))))])
                                 (values mem (set-reg regs #xF (cdr composite)) (car composite) delay-timer sound-timer stack))))
    (make-instruction #xE000 (lambda (opcode mem regs screen delay-timer sound-timer stack)
                               (cond [(= (bitwise-and opcode #x00FF) #x9E)
                                      (if (key-pressed (get-reg regs (arithmetic-shift (bitwise-and opcode #x0F00) -8)))
                                        (values mem (set-reg regs 'pc (+ (get-reg regs 'pc) 2)) screen delay-timer sound-timer stack)
                                        (values mem regs screen delay-timer sound-timer stack))]
                                     [(= (bitwise-and opcode #x00FF) #xA1)
                                      (if (key-pressed (get-reg regs (arithmetic-shift (bitwise-and opcode #x0F00) -8)))
                                        (values mem regs screen delay-timer sound-timer stack)
                                        (values mem (set-reg regs 'pc (+ (get-reg regs 'pc) 2)) screen delay-timer sound-timer stack))]
                                     [else (format #t "invalid opcode ~X~%" opcode)])))
    (make-instruction #xF000 (lambda (opcode mem regs screen delay-timer sound-timer stack)
                               (let ([byte (bitwise-and opcode #x00FF)]
                                     [x (arithmetic-shift (bitwise-and opcode #x0F00) -8)])
                                 (cond [(= byte #x07) (values mem (set-reg regs x delay-timer) screen delay-timer sound-timer stack)]
                                       [(= byte #x0A) (values mem (set-reg regs x (poll-key)) screen delay-timer sound-timer stack)]
                                       [(= byte #x15) (values mem regs screen (get-reg regs x) sound-timer stack)]
                                       [(= byte #x18) (values mem regs screen delay-timer (get-reg regs x) stack)]
                                       [(= byte #x1E) (values mem (set-reg regs 'i (+ (get-reg regs 'i) (get-reg regs x))) screen delay-timer sound-timer stack)]
                                       [(= byte #x29) (values mem (set-reg regs 'i (+ #x50 (* (get-reg regs x) 5))) screen delay-timer sound-timer stack)]
                                       [(= byte #x33) (values (set-mem (set-mem (set-mem mem (get-reg regs 'i) (truncate (/ (get-reg regs x) 100)))
                                                                                (+ (get-reg regs 'i) 1) (modulo (truncate (/ (get-reg regs x) 10)) 10))
                                                                       (+ (get-reg regs 'i) 2) (modulo (modulo (get-reg regs x) 100) 10))
                                                              regs screen delay-timer sound-timer stack)]
                                       [(= byte #x55) (values (let loop ([m mem]
                                                                         [c 0])
                                                                (if (< c #x10)
                                                                  (loop (set-mem m (+ (get-reg regs 'i) c) (get-reg regs c)) (+ c 1))
                                                                  m)) regs screen delay-timer sound-timer stack)]
                                       [(= byte #x65) (values mem (let loop ([r regs]
                                                                             [c 0])
                                                                    (if (< c x)
                                                                      (loop (set-reg r c (get-mem mem (+ (get-reg regs 'i) c))) (+ c 1))
                                                                      r)) screen delay-timer sound-timer stack)]
                                       [else (format #t "invalid opcode ~X~%" opcode)]))))))

(define *chip8-font*
  '(#xF0 #x90 #x90 #x90 #xF0 ; 0
    #x20 #x60 #x20 #x20 #x70 ; 1
    #xF0 #x10 #xF0 #x80 #xF0 ; 2
    #xF0 #x10 #xF0 #x10 #xF0 ; 3
    #x90 #x90 #xF0 #x10 #x10 ; 4
    #xF0 #x80 #xF0 #x10 #xF0 ; 5
    #xF0 #x90 #xF0 #x90 #xF0 ; 6
    #xF0 #x10 #x20 #x40 #x40 ; 7
    #xF0 #x90 #xF0 #x90 #xF0 ; 8
    #xF0 #x90 #xF0 #x10 #xF0 ; 9
    #xF0 #x90 #xF0 #x90 #x90 ; A
    #xE0 #x90 #xE0 #x90 #xE0 ; B
    #xF0 #x80 #x80 #x80 #xF0 ; C
    #xE0 #x90 #x90 #x90 #xE0 ; D
    #xF0 #x80 #xF0 #x80 #xF0 ; E
    #xF0 #x80 #xF0 #x80 #x80 ; F
    ))

(define (load-font mem)
  (let loop ([i 0]
             [l *chip8-font*])
   (if (< i 80)
     (begin
       (u8vector-set! mem (+ i 80) (car l))
       (loop (+ i 1) (cdr l)))
     mem)))

(define (load-program mem path)
  (call-with-input-file path (lambda (port) (read-u8vector! 3584 mem port 512)))
  mem)

(define (draw-screen screen)
  (wclear (stdscr))
  (let loop ([i 0])
   (if (= i (* 64 32))
     #t
     (let ([x (modulo i 64)]
           [y (arithmetic-shift i -6)])
       (mvaddch y x (if (= (get-pixel screen x y) 1) #\# #\.))
       (loop (+ i 1)))))
  (wrefresh (stdscr)))

(define (draw-screen-debug screen)
  (format #t "~A~%" screen))

(define (emulator-loop opcode-table memory registers)
  (initscr)
  (cbreak)
  (noecho)
  (keypad (stdscr) #t)
  (nodelay (stdscr) #t)
  (let loop ([mem memory]
             [regs registers]
             [screen (clear-screen)]
             [delay-timer 0]
             [sound-timer 0]
             [stack (cons (make-vector 16) 0)]
             [current-time (current-milliseconds)])
    (let* ([new-time (current-milliseconds)]
           [delta (- new-time current-time)])
      (if (> delta 16.6667)
        (begin
          (draw-screen screen)
          (let* ([pos (get-reg regs 'pc)]
                 [opcode (bitwise-ior (arithmetic-shift (get-mem memory pos) 8) (get-mem memory (+ pos 1)))]
                 [ins (assv (bitwise-and opcode #xF000) opcode-table)]
                 [dummy (format logfile "op: ~X, adjusted: ~X, regs: ~A~%" opcode (bitwise-and opcode #xF000) regs)]
                 [cb (instruction-callback ins)])
            (if (not (= opcode 0))
              (call-with-values (lambda () (cb opcode mem regs screen delay-timer sound-timer stack))
                                (lambda (mem regs screen delay-timer sound-timer stack) (loop mem (set-reg regs 'pc (+ (get-reg regs 'pc) 2)) screen (timer-down delay-timer) (timer-down sound-timer) stack new-time)))
              (endwin))))
        (loop mem regs screen delay-timer sound-timer stack current-time)))))

(emulator-loop *chip8-opcodes* (load-program (load-font (make-u8vector 4096 0)) (cadr (argv))) *chip8-registers*)
