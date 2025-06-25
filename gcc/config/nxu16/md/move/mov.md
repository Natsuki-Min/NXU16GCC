
(define_split
  [(set (match_operand:SPLIT34 0 "register_operand")
        (match_operand:SPLIT34 1 "register_operand"))]
  "optimize
   && reload_completed"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  {
    machine_mode mode_hi = <SIZE> == 4 ? HImode : QImode;
    bool lo_first = REGNO (operands[0]) < REGNO (operands[1]);
    rtx dst_lo = simplify_gen_subreg (HImode, operands[0], <MODE>mode, 0);
    rtx src_lo = simplify_gen_subreg (HImode, operands[1], <MODE>mode, 0);
    rtx dst_hi = simplify_gen_subreg (mode_hi, operands[0], <MODE>mode, 2);
    rtx src_hi = simplify_gen_subreg (mode_hi, operands[1], <MODE>mode, 2);

    operands[2] = lo_first ? dst_lo : dst_hi;
    operands[3] = lo_first ? src_lo : src_hi;
    operands[4] = lo_first ? dst_hi : dst_lo;
    operands[5] = lo_first ? src_hi : src_lo;
  })

(define_split
  [(set (match_operand:HI 0 "register_operand")
        (match_operand:HI 1 "reg_or_0_operand"))]
  "optimize
   && reload_completed
   &&  REGNO_REG_CLASS(REGNO(operands[0]))==GENERAL_REGS
   && (operands[1] == const0_rtx || REGNO_REG_CLASS(REGNO(operands[1]))==GENERAL_REGS)
   && ( const0_rtx == operands[1])"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  {
    operands[2] = simplify_gen_subreg (QImode, operands[0], HImode, 1);
    operands[3] = simplify_gen_subreg (QImode, operands[1], HImode, 1);
    operands[4] = simplify_gen_subreg (QImode, operands[0], HImode, 0);
    operands[5] = simplify_gen_subreg (QImode, operands[1], HImode, 0);
  })
