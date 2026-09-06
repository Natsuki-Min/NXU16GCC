
(define_expand "movpsi"
  [(set (match_operand:PSI 0 "nonimmediate_operand")
        (match_operand:PSI 1 "general_operand"))]
  ""
  {
    if (MEM_P (operands[0]) && MEM_P (operands[1]))
      operands[1] = force_reg (PSImode, operands[1]);
  })

(define_insn "*movpsi_insn"
  [(set (match_operand:PSI 0 "nonimmediate_operand" "=r,r,r,m,YSP,r")
        (match_operand:PSI 1 "general_operand"      "r,i,m,r,r,YSP"))]
  ""
  { return nxu16_output_movpsi (operands); })

(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand")
        (match_operand:SI 1 "general_operand"))]
  ""
  {
    if (MEM_P (operands[0]) && MEM_P (operands[1]))
      operands[1] = force_reg (SImode, operands[1]);
  })

(define_insn "*movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,m,YSP,r")
        (match_operand:SI 1 "general_operand"      "r,i,m,r,r,YSP"))]
  ""
  { return nxu16_output_movsi (operands); })

(define_split
  [(set (match_operand:SPLIT34 0 "register_operand")
        (match_operand:SPLIT34 1 "register_operand"))]
  "optimize
   && reload_completed
   && REGNO (operands[0]) != REG_SP
   && REGNO (operands[1]) != REG_SP
   && REGNO (operands[0]) != FRAME_POINTER_REGNUM
   && REGNO (operands[1]) != FRAME_POINTER_REGNUM
   && REGNO (operands[0]) != ARG_POINTER_REGNUM
   && REGNO (operands[1]) != ARG_POINTER_REGNUM"
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
