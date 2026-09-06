(define_register_constraint "YAL" "ALL_REGS"
  "ALL Registers.")
(define_register_constraint "YSI" "SIBCALL_REGS"
  "The call-clobbered XR0 register used for an indirect sibling target.")
(define_constraint "YSP"
  "The stack or eliminable frame pointer"
  (and (match_code "reg")
       (match_test "REGNO (op) == REG_SP
                    || REGNO (op) == FRAME_POINTER_REGNUM")))

;;---------------const ints--------------------
(define_constraint "Cim7i"
  "A negative constant integer in imm7."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -64, 63)")))
(define_constraint "Clo80"
  "A constant integer or symbolic operand that is at least .p2align 8."
  (and (match_code "const_int,symbol_ref,const")
       (match_test "const_0mod256_operand (op, HImode)")))
(define_constraint "Cim00"
  "Constant integer 0."
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "Cim01"
  "Constant integer 1."
  (and (match_code "const_int")
       (match_test "ival == 1")))

(define_constraint "Cim02"
  "Constant integer 2."
  (and (match_code "const_int")
       (match_test "ival == 2")))

(define_constraint "Cim03"
  "Constant integer 3."
  (and (match_code "const_int")
       (match_test "ival == 3")))

(define_constraint "Cim04"
  "Constant integer 4."
  (and (match_code "const_int")
       (match_test "ival == 4")))

(define_constraint "Cim05"
  "Constant integer 5."
  (and (match_code "const_int")
       (match_test "ival == 5")))

(define_constraint "Cim06"
  "Constant integer 6."
  (and (match_code "const_int")
       (match_test "ival == 6")))

(define_constraint "Cim07"
  "Constant integer 7."
  (and (match_code "const_int")
       (match_test "ival == 7")))
