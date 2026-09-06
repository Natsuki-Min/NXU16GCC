;; Machine description for nxu16
;; Copyright (C) 2009-2022 Free Software Foundation, Inc.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; -------------------------------------------------------------------------
;; nxu16 specific constraints, predicates and attributes
;; -------------------------------------------------------------------------

(include "md/definitions.md")



;; -------------------------------------------------------------------------
;; nop instruction
;; -------------------------------------------------------------------------

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

;; -------------------------------------------------------------------------
;; Arithmetic instructions
;; -------------------------------------------------------------------------




;; The hardware MUL is an unsigned widening operation: ERn = Rn * Rm.
;; Do not advertise it as a general QI or HI multiply, since that used to
;; silently miscompile 16-bit multiplication and also printed an invalid
;; `mul ERn,ERm' second operand.
(define_insn "umulqihi3"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(mult:HI
	  (zero_extend:HI (match_operand:QI 1 "register_operand" "0"))
	  (zero_extend:HI (match_operand:QI 2 "register_operand" "r"))))
   (clobber (reg:CC REG_CC))]
  ""
  "mul %0,%2")

;; DIV is likewise not a general same-width division: it divides ERn by Rm
;; and overwrites Rm with the remainder.  Keep generic division on the
;; correctly specified libgcc path until both results and the overlap are
;; represented by a constrained udivmod pattern.

;; Shift instructions (ASHL/ASHR)
(define_expand "ashlpsi3"
  [(set (match_operand:PSI 0 "register_operand")
        (ashift:PSI
          (match_operand:PSI 1 "register_operand")
          (match_operand:QI 2 "general_operand")))]
  ""
  {
    /* Jump-table indices are scaled by their two-byte near entries.  Keep
       that common pointer operation native instead of calling an unavailable
       partial-integer libgcc helper.  */
    if (CONST_INT_P (operands[2]) && INTVAL (operands[2]) == 1)
      {
        emit_insn (gen_addpsi3 (operands[0], operands[1], operands[1]));
        DONE;
      }
    FAIL;
  })

(define_insn_and_split "ashlqi3"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (ashift:QI
          (match_operand:QI 1 "register_operand" "0")
          (match_operand:QI 2 "register_operand" "r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashift:QI (match_dup 1) (match_dup 2)))
              (clobber (reg:CC REG_CC))])]
)

(define_insn "ashlqi3_clobber"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (ashift:QI
          (match_operand:QI 1 "register_operand" "0")
          (match_operand:QI 2 "register_operand" "r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "sll %0,%2"
  [])

(define_insn_and_split "ashrqi3"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (ashiftrt:QI
          (match_operand:QI 1 "register_operand" "0")
          (match_operand:QI 2 "register_operand" "r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashiftrt:QI (match_dup 1) (match_dup 2)))
              (clobber (reg:CC REG_CC))])]
)

(define_insn "ashrqi3_clobber"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (ashiftrt:QI
          (match_operand:QI 1 "register_operand" "0")
          (match_operand:QI 2 "register_operand" "r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "srl %0,%2"
  [])

;; -------------------------------------------------------------------------
;; Unary arithmetic instructions
;; -------------------------------------------------------------------------

(define_insn "one_cmplqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(not:QI (match_operand:QI 1 "register_operand" "0")))]
  ""
  "xor %0,255")

(define_insn "one_cmplhi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(not:HI (match_operand:HI 1 "register_operand" "0")))]
  ""
  "xor %A0,255\;xor %B0,255")

;; -------------------------------------------------------------------------
;; Logical operators
;; -------------------------------------------------------------------------



;; -------------------------------------------------------------------------
;; Move instructions
;; -------------------------------------------------------------------------





(define_expand "mov<mode>"
  [(set (match_operand:QIHI 0 "nonimmediate_operand" "")
        (match_operand:QIHI 1 "general_operand" ""))]
  ""
  {
    rtx dest = operands[0];
    rtx src  = operands[1];

    if (<MODE>mode == HImode
	&& register_operand (dest, HImode)
	&& (SYMBOL_REF_P (src) || LABEL_REF_P (src) || GET_CODE (src) == CONST))
    {
      emit_insn (gen_movhi_symbol (dest, src));
      DONE;
    }

    if (MEM_P (dest) && MEM_P (src))
    {
      rtx reg = gen_reg_rtx (<MODE>mode);
      emit_move_insn (reg, src);
      emit_move_insn (dest, reg);
      DONE;
    }
    //if (register_operand (dest, <MODE>mode)){
    //  DONE;
    //}
  })

;; There is no word-sized absolute-immediate move.  Materialize an address
;; through the two byte-immediate instructions and let GAS/BFD carry the
;; byte-selection relocations.
(define_insn "movhi_symbol"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(match_operand:HI 1 "immediate_operand" "i"))]
  ""
  "mov %A0,lo8(%1)\;mov %B0,hi8(%1)")

;; Pointer tests and byte extraction can expose a byte subreg of a symbolic
;; far address before reload.  Keep the selector visible to GAS instead of
;; requiring the generic move patterns to treat that subreg as an integer.
(define_insn "*movqi_symbol_byte0"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(subreg:QI (match_operand:SI 1 "immediate_operand" "i") 0))]
  ""
  "mov %0,lo8(%1)")

(define_insn "*movqi_symbol_byte1"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(subreg:QI (match_operand:SI 1 "immediate_operand" "i") 1))]
  ""
  "mov %0,hi8(%1)")

(define_insn "*movqi_symbol_byte2"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(subreg:QI (match_operand:SI 1 "immediate_operand" "i") 2))]
  ""
  "mov %0,hlo8(%1)")

(define_insn "*movqi_symbol_byte3"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(subreg:QI (match_operand:SI 1 "immediate_operand" "i") 3))]
  ""
  "mov %0,hhi8(%1)")

	  (define_insn_and_split "mov<mode>_insn_split"
	[(set (match_operand:QIHI 0 "nonimmediate_operand"    "=YAL,r,r,m")
	      (match_operand:QIHI 1 "general_operand"          "YAL,i,m,r"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
              (clobber (reg:CC REG_CC))])])

;;TODO: to track register to realize mov er inst(avoid mov hi,0 to many times)
(define_insn "mov<mode>_insn"
[(set (match_operand:QIHI 0 "nonimmediate_operand"    "=YAL,r,r,m")
      (match_operand:QIHI 1 "general_operand"          "YAL,i,m,r"));;FIXME:may not have move between sp and cc
      (clobber (reg:CC REG_CC))]
"reload_completed"
{
  if (<MODE>mode == HImode
      && (SYMBOL_REF_P (operands[1])
	  || LABEL_REF_P (operands[1])
	  || GET_CODE (operands[1]) == CONST
	  || (CONST_INT_P (operands[1])
	      && !IN_RANGE (INTVAL (operands[1]), -64, 63))))
    return "mov %A0,lo8(%1)\;mov %B0,hi8(%1)";

  switch (which_alternative)
    {
    case 0:
    case 1:
      return "mov %0,%1";
    case 2:
    case 3:
      return nxu16_output_move (operands);
    default:
      gcc_unreachable ();
    }
}
)




;; -------------------------------------------------------------------------
;; Call and Jump instructions
;; -------------------------------------------------------------------------


;; call

;; Operand 1 not used on the AVR.
;; Operand 2 is 1 for tail-call, 0 otherwise.
(define_expand "call"
  [(parallel[(call (match_operand:SI 0 "call_insn_operand" "")
                   (match_operand:SI 1 "general_operand" ""))
             (use (const_int 0))])])

;; Operand 1 not used on the AVR.
;; Operand 2 is 1 for tail-call, 0 otherwise.TODO:treat leaf!!! 

(define_expand "sibcall"
  [(parallel[(call (match_operand:SI 0 "call_insn_operand" "")
                   (match_operand:SI 1 "general_operand" ""))
             (use (const_int 1))])])

;; call value

;; Operand 2 not used on the AVR.
;; Operand 3 is 1 for tail-call, 0 otherwise.
(define_expand "call_value"
  [(parallel[(set (match_operand 0 "register_operand" "")
                  (call (match_operand:SI 1 "call_insn_operand" "")
                        (match_operand:SI 2 "general_operand" "")))
             (use (const_int 0))])])

;; Operand 2 not used on the AVR.
;; Operand 3 is 1 for tail-call, 0 otherwise.
(define_expand "sibcall_value"
  [(parallel[(set (match_operand 0 "register_operand" "")
                  (call (match_operand:SI 1 "call_insn_operand" "")
                        (match_operand:SI 2 "general_operand" "")))
             (use (const_int 1))])])


(define_insn "call_value_insn"
  [(parallel[(set (match_operand 0 "register_operand"                    "=r,r")
                  (call (mem:SI (match_operand:SI 1 "nonmemory_operand" "ri,YSI i"))
                        (match_operand:SI 2 "general_operand"            "g,g")))
             (use (match_operand:SI 3 "const_int_operand"                "Cim00,Cim01"))])]
  ;; Operand 2 not used on the AVR.
  ;; Operand 3 is 1 for tail-call, 0 otherwise.
  ""
  { 
   return nxu16_output_far_transfer (operands[1], INTVAL (operands[3]) == 0);
  }
  )

(define_insn "call_insn"
  [(parallel[(call (mem:SI (match_operand:SI 0 "nonmemory_operand" "ri,YSI i"))
                   (match_operand:SI 1 "general_operand"            "g,g"))
             (use (match_operand:SI 2 "const_int_operand"            "Cim00,Cim01"))])]
  ;; Operand 1 not used on the AVR.
  ;; Operand 2 is 1 for tail-call, 0 otherwise.
  ""
  { 
   return nxu16_output_far_transfer (operands[0], INTVAL (operands[2]) == 0);
  }
  )



(define_insn "indirect_jump"
    [(set (pc) (match_operand:SI 0 "nonimmediate_operand" "r"))]
    ""
    { return nxu16_output_far_transfer (operands[0], false); })

(define_insn "tablejump"
    [(set (pc) (match_operand:HI 0 "register_operand" "r"))
     (use (label_ref (match_operand 1 "" "")))]
    ""
    "b %0")

(define_insn "jump"
    [(set (pc) (label_ref (match_operand 0 "" "")))]
    ""
    "b %l0")

;; -------------------------------------------------------------------------
;; Compare instructions
;; -------------------------------------------------------------------------



(define_expand "cbranch<mode>4"
  [(set (pc)
        (if_then_else (match_operator 0 "comparison_operator"
                       [(match_operand:QIHI 1 "general_operand" "")
                        (match_operand:QIHI 2 "general_operand" "")])
                      (label_ref (match_operand 3 "" ""))
                      (pc)))
   (clobber (reg:CC REG_CC))]
  ""
  {
  /* Force the compare operands into registers.  */
  if (!register_operand (operands[1], <MODE>mode))
	operands[1] = force_reg (<MODE>mode, operands[1]);
  if (!register_operand (operands[2], <MODE>mode))
	operands[2] = force_reg (<MODE>mode, operands[2]);

  /* Loads and moves update PSW on nX-U16.  Keep CMP adjacent to its branch
     so reload cannot insert a pointer-half load between the two.  */
  emit_jump_insn (gen_cbranch<mode>4_internal (operands[0], operands[1],
					       operands[2], operands[3]));
  DONE;
  })

(define_insn "cbranch<mode>4_internal"
  [(set (pc)
        (if_then_else (match_operator 0 "comparison_operator"
                       [(match_operand:QIHI 1 "register_operand" "r")
                        (match_operand:QIHI 2 "register_operand" "r")])
                      (label_ref (match_operand 3 "" ""))
                      (pc)))
   (clobber (reg:CC REG_CC))]
  ""
  "cmp %1,%2\;b%J0 %l3")

(define_insn "*cmpqi"
  [(set (reg:CC REG_CC)
      (compare:CC
	 (match_operand:QI 0 "register_operand" "r,r")
	 (match_operand:QI 1 "nonmemory_operand" "r,i")))]
  ""
  "cmp %0,%1")

(define_insn "*cmphi"
  [(set (reg:CC REG_CC)
      (compare:CC
	 (match_operand:HI 0 "register_operand" "r")
	 (match_operand:HI 1 "register_operand" "r")))]
  ""
  "cmp %0,%1")

;; -------------------------------------------------------------------------
;; Branch instructions
;; -------------------------------------------------------------------------

(define_code_iterator cond [ne eq lt ltu gt gtu ge le geu leu])
;; The bare LT/GE/GT/LE mnemonics are the ISA's unsigned conditions; signed
;; comparisons carry an S suffix.
(define_code_attr CC [(ne "ne") (eq "eq") (lt "lts") (ltu "lt")
		      (gt "gts") (gtu "gt") (ge "ges") (le "les")
		      (geu "ge") (leu "le") ])

(define_insn "*b<cond:code>"
  [(set (pc)
	(if_then_else (cond (reg:CC REG_CC)
			    (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
{
  return "b<CC> %l0";
})

;; Elimination of an incoming stack argument can require a temporary base
;; because SP is not an ISA load/store base.  Saving that temporary changes
;; the final argument offset, and LRA can represent the correction as an ADD
;; followed by a still-displaced access.  Fold both constants back into the
;; memory operand after reload.  This also exposes ER12/ER14 disp6 forms to
;; GAS instead of emitting an avoidable pointer adjustment.
(define_peephole2
  [(parallel
     [(set (match_operand:HI 0 "register_operand" "")
           (plus:HI (match_dup 0)
                    (match_operand:HI 1 "const_int_operand" "")))
      (clobber (reg:CC REG_CC))])
   (parallel
     [(set (match_operand:QIHI 2 "register_operand" "")
           (match_operand:QIHI 3 "memory_operand" ""))
      (clobber (reg:CC REG_CC))])]
  "GET_CODE (XEXP (operands[3], 0)) == PLUS
   && rtx_equal_p (XEXP (XEXP (operands[3], 0), 0), operands[0])
   && CONST_INT_P (XEXP (XEXP (operands[3], 0), 1))
   && IN_RANGE (INTVAL (operands[1])
                + INTVAL (XEXP (XEXP (operands[3], 0), 1)),
                -32768, 32767)
   && (reg_overlap_mentioned_p (operands[0], operands[2])
       || peep2_reg_dead_p (2, operands[0]))"
  [(parallel
     [(set (match_dup 2) (match_dup 4))
      (clobber (reg:CC REG_CC))])]
  {
    HOST_WIDE_INT displacement
      = INTVAL (operands[1])
        + INTVAL (XEXP (XEXP (operands[3], 0), 1));
    rtx address = plus_constant (HImode, operands[0], displacement);
    operands[4] = replace_equiv_address_nv (operands[3], address);
  })

;; -------------------------------------------------------------------------
;; Prologue & Epilogue
;; -------------------------------------------------------------------------

(define_expand "prologue"
  [(const_int 1)]
  ""
{
  nxu16_expand_prologue ();
  DONE;
})

(define_expand "epilogue"
  [(const_int 2)]
  ""
{
  nxu16_expand_epilogue (false);
  DONE;
})

(define_expand "sibcall_epilogue"
  [(const_int 2)]
  ""
{
  nxu16_expand_epilogue (true);
  DONE;
})

(define_insn "ret"
    [(return)]
    "reload_completed"
    {
    if(crtl->is_leaf)
     return "rt";
    else
     return "pop pc";
    }
    )

;; -------------------------------------------------------------------------
;; Interrupt related instructions
;; -------------------------------------------------------------------------

(define_c_enum "unspecv" [
  UNSPECV_IRET
  UNSPECV_STI
  UNSPECV_CLI
  ])

(define_insn "iret"
    [(return)
    (unspec_volatile [(const_int 0)] UNSPECV_IRET)]
    ""
    "rti")

(define_insn "sti"
    [(unspec_volatile [(const_int 0)] UNSPECV_STI)]
    ""
    "ei")

(define_insn "cli"
    [(unspec_volatile [(const_int 0)] UNSPECV_CLI)]
    ""
    "di")

;; Local Variables:
;; mode: lisp
;; indent-tabs-mode: t
;; End:
