
(define_predicate "call_insn_operand"
  (and (match_code "mem")
       (ior (match_test "register_operand (XEXP (op, 0), mode)")
            (match_test "CONSTANT_ADDRESS_P (XEXP (op, 0))"))))
(define_predicate "general_register_operand"(and (match_code "reg")
  (match_test "REG_P (op) && (REGNO_REG_CLASS (REGNO (op))==GENERAL_REGS)")))
  (define_predicate "stack_register_operand"(and (match_code "reg")
  (match_test "REG_P (op) && ((REGNO (op))==STACK_POINTER_REGNUM)")))
;; Const int, const fixed, or const double operand
(define_predicate "nonmemory_or_const_operand"
  (ior (match_code "const_fixed")
       (match_code "const_double")
       (match_operand 0 "nonmemory_operand")))
;; Returns true when OP is a SYMBOL_REF, CONST or CONST_INT that is
;; a multiple of 256, i.e. lo8(OP) = 0.
(define_predicate "const_0mod256_operand"
  (ior (and (match_code "symbol_ref")
            (match_test "SYMBOL_REF_DECL (op)
                         && DECL_P (SYMBOL_REF_DECL (op))
                         && DECL_ALIGN (SYMBOL_REF_DECL (op)) >= 8 * 256"))
       (and (match_code "const")
            (match_test "GET_CODE (XEXP (op, 0)) == PLUS")
            (match_test "const_0mod256_operand (XEXP (XEXP (op, 0), 0), HImode)")
            (match_test "const_0mod256_operand (XEXP (XEXP (op, 0), 1), HImode)"))
       (and (match_code "const_int")
            (match_test "INTVAL (op) % 256 == 0"))))
            (define_predicate "pseudo_register_operand"
  (and (match_operand 0 "register_operand")
       (not (and (match_code "reg")
                 (match_test "HARD_REGISTER_P (op)")))))
(define_predicate "combine_pseudo_register_operand"
  (ior (match_operand 0 "pseudo_register_operand")
       (and (match_operand 0 "register_operand")
            (match_test "reload_completed || reload_in_progress"))))
            (define_predicate "const0_operand"
  (and (match_code "const_int,const_fixed,const_double")
       (match_test "op == CONST0_RTX (mode)")))
(define_predicate "reg_or_0_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const0_operand")))
