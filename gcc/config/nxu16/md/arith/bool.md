
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
; and
(define_insn_and_split "andqi3"
  [(set (match_operand:QI 0 "register_operand"       "=r,r")
        (and:QI (match_operand:QI 1 "register_operand" "%0,0")
                (match_operand:QI 2 "nonmemory_operand" "r,i")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (and:QI (match_dup 1)
                           (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*andqi3"
  [(set (match_operand:QI 0 "register_operand"       "=r,r")
        (and:QI (match_operand:QI 1 "register_operand" "%0,0")
                (match_operand:QI 2 "nonmemory_operand" "r,i")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "and %0,%2"
  [(set_attr "length" "1")])
  
  
(define_insn_and_split "andhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
        (and:HI (match_operand:HI 1 "register_operand" "%0,0")
                (match_operand:HI 2 "nonmemory_operand" "r,i")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (and:HI (match_dup 1)
                           (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*andhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
        (and:HI (match_operand:HI 1 "register_operand" "%0,0")
                (match_operand:HI 2 "nonmemory_operand" "r,i")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
  and %A0,%A2\;and %B0,%B2
  and %A0,lo8(%2)\;and %B0,hi8(%2)"
  [(set_attr "length" "2")])

(define_insn_and_split "andpsi3"
  [(set (match_operand:PSI 0 "register_operand" "=r,r")
        (and:PSI (match_operand:PSI 1 "register_operand" "%0,0")
                 (match_operand:PSI 2 "nonmemory_operand" "r,i")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (and:PSI (match_dup 1)
                            (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*andpsi3"
  [(set (match_operand:PSI 0 "register_operand" "=r,r")
        (and:PSI (match_operand:PSI 1 "register_operand" "%0,0")
                 (match_operand:PSI 2 "nonmemory_operand" "r,i")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
  and %A0,%A2\;and %B0,%B2\;and %C0,%C2
  and %A0,lo8(%2)\;and %B0,hi8(%2)\;and %C0,hlo8(%2)"
  [(set_attr "length" "3")])

(define_insn_and_split "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (and:SI (match_operand:SI 1 "register_operand" "%0,0")
                (match_operand:SI 2 "nonmemory_operand" "r,i")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (and:SI (match_dup 1)
                           (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (and:SI (match_operand:SI 1 "register_operand" "%0,0")
                (match_operand:SI 2 "nonmemory_operand" "r,i")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
  and %A0,%A2\;and %B0,%B2\;and %C0,%C2\;and %D0,%D2
  and %A0,lo8(%2)\;and %B0,hi8(%2)\;and %C0,hlo8(%2)\;and %D0,hhi8(%2)"
  [(set_attr "length" "4")])

;;|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;; ior
(define_insn_and_split "iorqi3"
  [(set (match_operand:QI 0 "register_operand" "=r,r")
        (ior:QI (match_operand:QI 1 "register_operand" "%0,0")
                (match_operand:QI 2 "nonmemory_operand" "r,i")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ior:QI (match_dup 1)
                           (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*iorqi3"
  [(set (match_operand:QI 0 "register_operand" "=r,r")
        (ior:QI (match_operand:QI 1 "register_operand" "%0,0")
                (match_operand:QI 2 "nonmemory_operand" "r,i")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
   or %0,%2
   or %0,lo8(%2)"
  [(set_attr "length" "1")])

(define_insn_and_split "iorhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
        (ior:HI (match_operand:HI 1 "register_operand" "%0,0")
                (match_operand:HI 2 "nonmemory_operand" "r,i")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ior:HI (match_dup 1)
                           (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*iorhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
        (ior:HI (match_operand:HI 1 "register_operand" "%0,0")
                (match_operand:HI 2 "nonmemory_operand" "r,i")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
   or %A0,%A2\;or %B0,%B2
   or %A0,lo8(%2)\;or %B0,hi8(%2)"
  [(set_attr "length" "2")])

(define_insn_and_split "iorpsi3"
  [(set (match_operand:PSI 0 "register_operand" "=r,r")
        (ior:PSI (match_operand:PSI 1 "register_operand" "%0,0")
                (match_operand:PSI 2 "nonmemory_operand" "r,i")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ior:PSI (match_dup 1)
                           (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*iorpsi3"
  [(set (match_operand:PSI 0 "register_operand" "=r,r")
        (ior:PSI (match_operand:PSI 1 "register_operand" "%0,0")
                (match_operand:PSI 2 "nonmemory_operand" "r,i")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
   or %A0,%A2\;or %B0,%B2\;or %C0,%C2
   or %A0,lo8(%2)\;or %B0,hi8(%2)\;or %C0,hlo8(%2)"
  [(set_attr "length" "3")])

(define_insn_and_split "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (ior:SI (match_operand:SI 1 "register_operand" "%0,0")
                (match_operand:SI 2 "nonmemory_operand" "r,i")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ior:SI (match_dup 1)
                           (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (ior:SI (match_operand:SI 1 "register_operand" "%0,0")
                (match_operand:SI 2 "nonmemory_operand" "r,i")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
   or %A0,%A2\;or %B0,%B2\;or %C0,%C2\;or %D0,%D2
   or %A0,lo8(%2)\;or %B0,hi8(%2)\;or %C0,hlo8(%2)\;or %D0,hhi8(%2)"
  [(set_attr "length" "4")])
;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; xor

(define_insn_and_split "xorqi3"
  [(set (match_operand:QI 0 "register_operand" "=r,r")
        (xor:QI (match_operand:QI 1 "register_operand" "%0,0")
                (match_operand:QI 2 "nonmemory_operand" "r,i")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (xor:QI (match_dup 1)
                           (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*xorqi3"
  [(set (match_operand:QI 0 "register_operand" "=r,r")
        (xor:QI (match_operand:QI 1 "register_operand" "%0,0")
                (match_operand:QI 2 "nonmemory_operand" "r,i")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "xor %0,%2"
  [(set_attr "length" "1")])

(define_insn_and_split "xorhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
        (xor:HI (match_operand:HI 1 "register_operand" "%0,0")
                (match_operand:HI 2 "nonmemory_operand" "r,i")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (xor:HI (match_dup 1)
                           (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*xorhi3"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
        (xor:HI (match_operand:HI 1 "register_operand" "%0,0")
                (match_operand:HI 2 "nonmemory_operand" "r,i")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
   xor %A0,%A2\;xor %B0,%B2
   xor %A0,lo8(%2)\;xor %B0,hi8(%2)"
  [(set_attr "length" "2")])

(define_insn_and_split "xorpsi3"
  [(set (match_operand:PSI 0 "register_operand" "=r,r")
        (xor:PSI (match_operand:PSI 1 "register_operand" "%0,0")
                (match_operand:PSI 2 "nonmemory_operand" "r,i")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (xor:PSI (match_dup 1)
                           (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*xorpsi3"
  [(set (match_operand:PSI 0 "register_operand" "=r,r")
        (xor:PSI (match_operand:PSI 1 "register_operand" "%0,0")
                (match_operand:PSI 2 "nonmemory_operand" "r,i")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
   xor %A0,%A2\;xor %B0,%B2\;xor %C0,%C2
   xor %A0,lo8(%2)\;xor %B0,hi8(%2)\;xor %C0,hlo8(%2)"
  [(set_attr "length" "3")])

(define_insn_and_split "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (xor:SI (match_operand:SI 1 "register_operand" "%0,0")
                (match_operand:SI 2 "nonmemory_operand" "r,i")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (xor:SI (match_dup 1)
                           (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (xor:SI (match_operand:SI 1 "register_operand" "%0,0")
                (match_operand:SI 2 "nonmemory_operand" "r,i")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
   xor %A0,%A2\;xor %B0,%B2\;xor %C0,%C2\;xor %D0,%D2
   xor %A0,lo8(%2)\;xor %B0,hi8(%2)\;xor %C0,hlo8(%2)\;xor %D0,hhi8(%2)"
  [(set_attr "length" "4")])
  
  
  
  
  
  (define_split
  [(parallel [(set (match_operand:HISI 0 "register_operand")
                   (bitop:HISI (match_dup 0)
                               (match_operand:HISI 1 "register_operand")))
              (clobber (scratch:QI))
              (clobber (reg:CC REG_CC))])]
  "optimize
   && reload_completed"
  [(const_int 1)]
  {
    for (int i = 0; i < <SIZE>; i++)
      {
        rtx dst = simplify_gen_subreg (QImode, operands[0], <MODE>mode, i);
        rtx src = simplify_gen_subreg (QImode, operands[1], <MODE>mode, i);
        emit_insn (gen_<code>qi3 (dst, dst, src));
      }
    DONE;
  })

