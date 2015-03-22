(define (make-instruction opcode size cycles callback) (list opcode size cycles callback)) ; (callback memory regs)
(define (instruction-opcode ins) (car ins))
(define (instruction-size ins) (cadr ins))
(define (instruction-cycles ins) (caddr ins))
(define (instruction-callback ins) (cadddr ins))

(define (get-mem mem i)
  (vector-ref mem i))

(define (set-mem mem i value)
  (vector-set! mem i (modulo value #xFF))
  mem)

(define (get-reg regs reg)
  (let ([majorminor (cond [(eq? reg 'bc) (cons 'b 'c)]
                          [(eq? reg 'de) (cons 'd 'e)]
                          [(eq? reg 'hl) (cons 'h 'l)]
                          [else #f])])
    (if majorminor
      (+ (arithmetic-shift (assq (car majorminor) regs) 8) (assq (cdr majorminor)))
      (cdr (assq reg regs)))))

(define (set-reg regs reg value)
  (let ([majorminor (cond [(eq? reg 'bc) (cons 'b 'c)]
                          [(eq? reg 'de) (cons 'd 'e)]
                          [(eq? reg 'hl) (cons 'h 'l)]
                          [else #f])])
    (if majorminor
      (map (lambda (r)
             (cond [(eq? (car r) (car majorminor)) (cons ((car majorminor) (modulo value #xFF)))]
                   [(eq? (car r) (cdr majorminor)) (cons ((cdr majorminor) (modulo value #xFF)))]
                   [else r])))
      (map (lambda (r)
             (if (eq? (car r) reg)
               (cons reg (modulo value (expt #xFF (string-length (symbol->string reg)))))
               r))
           regs))))

(define (emulator-loop opcode-table memory registers)
  (let loop ([mem memory]
             [regs registers])
   (let* ([pos (get-reg regs 'pc)]
          [opcode (get-mem memory pos)]
          [ins (assv opcode opcode-table)]
          [cb (instruction-callback ins)])
     (call-with-values (lambda () (cb mem regs))
                       (lambda (mem regs) (loop mem (set-reg regs 'pc (+ pos (instruction-size ins)))))))))

(define *z80-registers*
; AF (accumulator, flags)
  '((a . #x00) (f . #x00)
; BC (general)
    (b . #x00) (c . #x00)
; DE (general)
    (d . #x00) (e . #x00)
; HL (indirect address)
    (h . #x00) (l . #x00)
; SP (stack pointer)
    (sp . #x0000)
; PC (program counter)
    (pc . #x0000)
; IX (index x)
    (ix . #x0000)
; IY (index y)
    (iy . #x0000)
; I (interrupt vector)
    (i . #x00)
; R (refresh counter)
    (r . #x00)
; AF' (accumulator, flags)
    (a-prime . #x00) (f-prime . #x00)
; BC' (general)
    (b-prime . #x00) (c-prime . #x00)
; DE' (general)
    (d-prime . #x00) (e-prime . #x00)
; HL' (indirect address)
    (h-prime . #x00) (l-prime . #x00)))

(define (get-carry regs) (bitwise-and #b00000001))
(define (get-subtraction regs) (bitwise-and #b00000010))
(define (get-parity regs) (bitwise-and #b00000100))
(define (get-half-carry regs) (bitwise-and #b00010000))
(define (get-zero regs) (bitwise-and #b01000000))
(define (get-sign regs) (bitwise-and #b10000000))

(define (set-carry regs) (set-reg regs 'f (bitwise-ior #b00000001)))
(define (set-subtraction regs) (set-reg regs 'f (bitwise-ior #b00000010)))
(define (set-parity regs) (set-reg regs 'f (bitwise-ior #b00000100)))
(define (set-half-carry regs) (set-reg regs 'f (bitwise-ior #b00010000)))
(define (set-zero regs) (set-reg regs 'f (bitwise-ior #b01000000)))
(define (set-sign regs) (set-reg regs 'f (bitwise-ior #b10000000)))

(define (make-z80-ld-8-from-mem-ins opcode reg)
  (make-instruction opcode 2 7
                    (lambda (mem regs)
                      (set-reg regs reg (get-mem mem (- (get-reg 'pc) 1))))))

(define (make-z80-ld-16-from-mem-ins opcode reg)
  (let ([majorminor (cond [(eq? reg 'bc) (cons 'b 'c)]
                          [(eq? reg 'de) (cons 'd 'e)]
                          [(eq? reg 'hl) (cons 'h 'l)]
                          [else #f])])
    (if majorminor
      (make-instruction opcode 3 10
                        (lambda (mem regs)
                          (let* ([pc (get-reg 'pc)]
                                 [regs (set-reg regs (cadr majorminor) (get-mem mem (+ pc 1)))])
                            (values mem (set-reg regs (car majorminor) (get-mem mem (+ pc 2)))))))
      (if (eq? reg 'sp)
        (make-instruction opcode 3 10
                          (lambda (mem regs)
                            (let ([pc (get-reg 'pc)])
                             (values mem (set-reg regs reg (+ (arithmetic-shift (vector-ref mem (+ pc 2)) 8) (vector-ref mem (+ pc 1))))))))
        (error "Invalid register:" reg)))))

(define *z80-opcodes* ; no flags are currently set
  (list
    (make-instruction #x00 1 4 (lambda (mem regs) (values mem regs))) ; nop
    (make-z80-ld-16-from-mem-ins #x01 'bc) ; ld bc, **
    (make-instruction #x02 1 7 (lambda (mem regs) (values (set-mem mem (get-reg regs 'bc) (get-reg regs 'a)) regs))) ; ld (bc), a
    (make-instruction #x03 1 6 (lambda (mem regs) (values mem (set-reg regs 'bc (+ (get-reg regs 'bc) 1))))) ; inc bc
    (make-instruction #x04 1 4 (lambda (mem regs) (values mem (set-reg regs 'b (+ (get-reg regs 'b) 1))))) ; inc b
    (make-instruction #x05 1 4 (lambda (mem regs) (values mem (set-reg regs 'b (- (get-reg regs 'b) 1))))) ; dec b
    (make-z80-ld-8-from-mem-ins #x06 'b) ; ld b, *
    (make-instruction #x07 1 4 (lambda (mem regs)
                                 (let* ([a (get-reg regs 'a)]
                                        [set (bit-set? a 7)]
                                        [shift (arithmetic-shift a 1)])
                                   (values mem (set-reg regs 'a (if set (bitwise-ior shift #b00000001) shift)))))) ; rlc a
    (make-instruction #x08 1 4 (lambda (mem regs)
                                 (let* ([temp-a (get-reg regs 'a)]
                                        [temp-f (get-reg regs 'f)]
                                        [regs (set-reg regs 'a (get-reg regs 'a-prime))]
                                        [regs (set-reg regs 'f (get-reg regs 'f-prime))]
                                        [regs (set-reg regs 'a-prime temp-a)])
                                   (values mem (set-reg regs 'f-prime temp-f))))) ; ex af, af'
    (make-instruction #x09 1 11 (lambda (mem regs) (values mem (set-reg regs 'hl (+ (get-reg regs 'hl) (get-reg regs 'bc)))))) ; add hl, bc
    (make-instruction #x0A 1 7 (lambda (mem regs) (values mem (set-reg regs 'a (get-mem mem (get-reg regs 'bc)))))) ; ld a, (bc)
    (make-instruction #x0B 1 6 (lambda (mem regs) (values mem (set-reg regs 'bc (- (get-reg regs 'bc) 1))))) ; dec bc
    (make-instruction #x0C 1 4 (lambda (mem regs) (values mem (set-reg regs 'c (+ (get-reg regs 'c) 1))))) ; inc c
    (make-instruction #x0D 1 4 (lambda (mem regs) (values mem (set-reg regs 'c (- (get-reg regs 'c) 1))))) ; dec c
    (make-z80-ld-8-from-mem-ins #x0E 'c) ; ld c, *
    (make-instruction #x0F 1 4 (lambda (mem regs)
                                 (let* ([a (get-reg regs 'a)]
                                        [set (bit-set? a 0)]
                                        [shift (arithmetic-shift a -1)])
                                   (values mem (set-reg regs 'a (if set (bitwise-ior shift #b10000000) shift)))))) ; rrc a
    (make-instruction #x10 2 13 (lambda (mem regs) ; TODO: takes 13/7 clock cycles, when timing is implemented this needs to be changed
                                  (let* ([new-b (- (get-reg regs 'b) 1)]
                                         [regs (set-reg regs 'b new-b)])
                                   (if (= new-b 0)
                                     (values mem regs)
                                     (let ([pc (get-reg regs 'pc)])
                                      (values mem (set-reg regs 'pc (+ pc (get-mem mem (+ pc 1)))))))))) ; djn z *
    (make-z80-ld-16-from-mem-ins #x11 'de) ; ld de, **
    (make-instruction #x12 1 7 (lambda (mem regs) (values (set-mem mem (get-reg regs 'de) (get-reg regs 'a)) regs))) ; ld (de), a
    (make-instruction #x13 1 6 (lambda (mem regs) (values mem (set-reg regs 'de (+ (get-reg regs 'de) 1))))) ; inc de
    (make-instruction #x14 1 4 (lambda (mem regs) (values mem (set-reg regs 'd (+ (get-reg regs 'd) 1))))) ; inc d
    (make-instruction #x15 1 4 (lambda (mem regs) (values mem (set-reg regs 'd (- (get-reg regs 'd) 1))))) ; dec d
    (make-z80-ld-8-from-mem-ins #x16 'd) ; ld d, *
    (make-instruction #x17 1 4 (lambda (mem regs)
                                 (let* ([a (get-reg regs 'a)]
                                        [set (bit-set? a 7)]
                                        [carry-set (get-carry regs)]
                                        [shift (arithmetic-shift a 1)]
                                        [regs (if set (set-carry regs) regs)])
                                   (values mem (set-reg regs 'a (if carry-set (bitwise-ior shift #b00000001) shift)))))) ; rla
    (make-instruction #x18 2 12 (lambda (mem regs)
                                  (let ([pc (get-reg regs 'pc)])
                                   (values mem (set-reg regs 'pc (+ pc (get-mem mem (+ pc 1)))))))) ; jr *
    (make-instruction #x19 1 11 (lambda (mem regs) (values mem (set-reg regs 'hl (+ (get-reg regs 'hl) (get-reg regs 'de)))))) ; add hl, de
    (make-instruction #x1A 1 7 (lambda (mem regs) (values mem (set-reg regs 'a (get-mem mem (get-reg regs 'de)))))) ; ld a, (de)
    (make-instruction #x1B 1 6 (lambda (mem regs) (values mem (set-reg regs 'de (- (get-reg regs 'de) 1))))) ; dec de
    (make-instruction #x1C 1 4 (lambda (mem regs) (values mem (set-reg regs 'e (+ (get-reg regs 'e) 1))))) ; inc e
    (make-instruction #x1D 1 4 (lambda (mem regs) (values mem (set-reg regs 'e (- (get-reg regs 'e) 1))))) ; dec e
    (make-z80-ld-8-from-mem-ins #x1E 'e) ; ld e, *
    (make-instruction #x1F 1 4 (lambda (mem regs)
                                 (let* ([a (get-reg regs 'a)]
                                        [set (bit-set? a 0)]
                                        [carry-set (get-carry regs)]
                                        [shift (arithmetic-shift a -1)]
                                        [regs (if set (set-carry regs) regs)])
                                   (values mem (set-reg regs 'a (if carry-set (bitwise-ior shift #b10000000) shift)))))) ; rra
    (make-instruction #x20 2 12 (lambda (mem regs)
                                  (values mem
                                          (if (not (get-zero regs))
                                            (let ([pc (get-reg regs 'pc)])
                                             (set-reg regs 'pc (+ pc (get-mem (+ pc 1)))))
                                            regs)))) ; jr nz, *
                                  (values (set-mem mem (+ (arithmetic-shift (get-mem mem (+ pc 2)) 8) (get-mem mem (+ pc 1))) (get-reg regs 'l)) regs))) ; ld (**), hl
    (make-instruction #x23 1 6 (lambda (mem regs) (values mem (set-reg regs 'hl (+ (get-reg regs 'hl) 1))))) ; inc hl
    (make-instruction #x24 1 4 (lambda (mem regs) (values mem (set-reg regs 'h (+ (get-reg regs 'h) 1))))) ; inc h
    (make-instruction #x25 1 4 (lambda (mem regs) (values mem (set-reg regs 'h (- (get-reg regs 'h) 1))))) ; dec h
    (make-z80-ld-8-from-mem-ins #x26 'h) ; ld h, *
    (make-instruction #x27 1 4 (lambda (mem regs) (values mem regs))) ; daa TODO
    (make-instruction #x28 2 12 (lambda (mem regs)
                                  (values mem
                                          (if (get-zero regs)
                                            (let ([pc (get-reg regs 'pc)])
                                             (set-reg regs 'pc (+ pc (get-mem (+ pc 1)))))
                                            regs)))) ; jr z, *
    (make-instruction #x29 1 11 (lambda (mem regs) (values mem (set-reg regs 'hl (* (get-reg regs 'hl) 2))))) ; add hl, hl
    (make-instruction #x2A 3 16 (lambda (mem regs)
                                  (let* ([pc (get-reg 'pc)])
                                   (values mem (set-reg regs 'l (get-mem mem (+ (arithmetic-shift (get-mem mem (+ pc 1)) 8) (get-mem mem (+ pc 2)))))))))
    (make-instruction #x2B 1 6 (lambda (mem regs) (values mem (set-reg regs 'hl (- (get-reg regs 'hl) 1))))) ; dec hl
    (make-instruction #x2C 1 4 (lambda (mem regs) (values mem (set-reg regs 'l (+ (get-reg regs 'l) 1))))) ; inc l
    (make-instruction #x2D 1 4 (lambda (mem regs) (values mem (set-reg regs 'l (- (get-reg regs 'l) 1))))) ; dec l
    (make-z80-ld-8-from-mem-ins #x2E 'l) ; ld l, *
    (make-instruction #x2F 1 4 (lambda (mem regs) (values mem (set-reg regs 'a (bitwise-xor (get-reg regs 'a) #b11111111))))) ; cpl
    (make-instruction #x30 2 12 (lambda (mem regs)
                                  (values mem
                                          (if (not (get-carry regs))
                                            (let ([pc (get-reg regs 'pc)])
                                             (set-reg regs 'pc (+ pc (get-mem (+ pc 1)))))
                                            regs)))) ; jr nc, *
    (make-z80-ld-16-from-mem-ins #x31 'sp) ; ld sp **
    (make-instructuon #x32 3 16 (lambda (mem regs)
                                  (values (set-mem mem (+ (arithmetic-shift (get-mem mem (+ pc 2)) 8) (get-mem mem (+ pc 1))) (get-reg regs 'a)) regs))) ; ld (**), a
    (make-instruction #x33 1 6 (lambda (mem regs) (values mem (set-reg regs 'hl (+ (get-reg regs 'hl) 1))))) ; inc hl
    (make-instruction #x34 1 4 (lambda (mem regs) (values mem (set-reg regs 'h (+ (get-reg regs 'h) 1))))) ; inc h
    (make-instruction #x35 1 4 (lambda (mem regs) (values mem (set-reg regs 'h (- (get-reg regs 'h) 1))))) ; dec h
    (make-z80-ld-8-from-mem-ins #x36 'h) ; ld h, *
    (make-instruction #x37 1 4 (lambda (mem regs) (values mem regs))) ; daa TODO
    (make-instruction #x38 2 12 (lambda (mem regs)
                                  (values mem
                                          (if (get-zero regs)
                                            (let ([pc (get-reg regs 'pc)])
                                             (set-reg regs 'pc (+ pc (get-mem (+ pc 1)))))
                                            regs)))) ; jr z, *
    (make-instruction #x39 1 11 (lambda (mem regs) (values mem (set-reg regs 'hl (* (get-reg regs 'hl) 2))))) ; add hl, hl
    (make-instruction #x3A 3 16 (lambda (mem regs)
                                  (let* ([pc (get-reg 'pc)])
                                   (values mem (set-reg regs 'l (get-mem mem (+ (arithmetic-shift (get-mem mem (+ pc 1)) 8) (get-mem mem (+ pc 2)))))))))
    (make-instruction #x3B 1 6 (lambda (mem regs) (values mem (set-reg regs 'hl (- (get-reg regs 'hl) 1))))) ; dec hl
    (make-instruction #x3C 1 4 (lambda (mem regs) (values mem (set-reg regs 'l (+ (get-reg regs 'l) 1))))) ; inc l
    (make-instruction #x3D 1 4 (lambda (mem regs) (values mem (set-reg regs 'l (- (get-reg regs 'l) 1))))) ; dec l
    (make-z80-ld-8-from-mem-ins #x3E 'l) ; ld l, *
    (make-instruction #x3F 1 4 (lambda (mem regs) (values mem (set-reg regs 'a (bitwise-xor (get-reg regs 'a) #b11111111)))))
    ))
