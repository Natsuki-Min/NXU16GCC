(define_constraint "S"
  "A constant integer or symbolic operand "
  (match_code "const_int,symbol_ref,const")
)
(define_constraint "K"
  "Integer constant 2."
  (and (match_code "const_int")
       (match_test "ival >= 0 && ival <= 32768")))
(define_constraint "J"
  "Integer constant in the range -63 @dots{} 0."
  (and (match_code "const_int")
       (match_test "ival <= 0 && ival >= -63")))
(define_register_constraint "z" "ZERO_REGS"
  "Register zero")
