(define_insn "pop<mode>1"
  [(set (match_operand:QIDI 0 "register_operand" "=YAL")
        (mem:QIDI (pre_inc:HI (reg:HI REG_SP))))]
  ""
  "pop %0"
  [])

(define_insn "push<mode>1"
  [(set (mem:QIDI (post_dec:HI (reg:HI REG_SP)))
        (match_operand:QIDI 0 "register_operand" "YAL"))]
  ""
  "push %0"
 [])
