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
  "")

;; -------------------------------------------------------------------------
;; Arithmetic instructions
;; -------------------------------------------------------------------------




;; MUL instruction with split
(define_insn_and_split "mul<mode>3"
  [(set (match_operand:QIHI 0 "register_operand" "=r")
        (mult:QIHI
          (match_operand:QIHI 1 "register_operand" "0")
          (match_operand:QIHI 2 "register_operand" "r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (mult:QIHI (match_dup 1) (match_dup 2)))
              (clobber (reg:CC REG_CC))])]
)

(define_insn "mul<mode>3_clobber"
  [(set (match_operand:QIHI 0 "register_operand" "=r")
        (mult:QIHI
          (match_operand:QIHI 1 "register_operand" "0")
          (match_operand:QIHI 2 "register_operand" "r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "mul %0 %2"
  [])

;; DIV instruction with split
(define_insn_and_split "div<mode>3"
  [(set (match_operand:QIHI 0 "register_operand" "=r")
        (div:QIHI
          (match_operand:QIHI 1 "register_operand" "0")
          (match_operand:QIHI 2 "register_operand" "r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (div:QIHI (match_dup 1) (match_dup 2)))
              (clobber (reg:CC REG_CC))])]
)

(define_insn "div<mode>3_clobber"
  [(set (match_operand:QIHI 0 "register_operand" "=r")
        (div:QIHI
          (match_operand:QIHI 1 "register_operand" "0")
          (match_operand:QIHI 2 "register_operand" "r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "div %0 %2"
  [])

;; Shift instructions (ASHL/ASHR)
(define_insn_and_split "ashl<mode>3"
  [(set (match_operand:QIHI 0 "register_operand" "=r")
        (ashift:QIHI
          (match_operand:QIHI 1 "register_operand" "0")
          (match_operand:QIHI 2 "register_operand" "r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashift:QIHI (match_dup 1) (match_dup 2)))
              (clobber (reg:CC REG_CC))])]
)

(define_insn "ashl<mode>3_clobber"
  [(set (match_operand:QIHI 0 "register_operand" "=r")
        (ashift:QIHI
          (match_operand:QIHI 1 "register_operand" "0")
          (match_operand:QIHI 2 "register_operand" "r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "sll %0 %2"
  [])

(define_insn_and_split "ashr<mode>3"
  [(set (match_operand:QIHI 0 "register_operand" "=r")
        (ashiftrt:QIHI
          (match_operand:QIHI 1 "register_operand" "0")
          (match_operand:QIHI 2 "register_operand" "r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashiftrt:QIHI (match_dup 1) (match_dup 2)))
              (clobber (reg:CC REG_CC))])]
)

(define_insn "ashr<mode>3_clobber"
  [(set (match_operand:QIHI 0 "register_operand" "=r")
        (ashiftrt:QIHI
          (match_operand:QIHI 1 "register_operand" "0")
          (match_operand:QIHI 2 "register_operand" "r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "srl %0 %2"
  [])

;; -------------------------------------------------------------------------
;; Unary arithmetic instructions
;; -------------------------------------------------------------------------

(define_insn "one_cmplhi2"
    [(set (match_operand:HI 0 "register_operand" "=r")
	  (not:HI (match_operand:HI 1 "register_operand" "0")))]
          ""
          "not %0")

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

    if (MEM_P (dest) && MEM_P (src))
    {
      rtx reg = gen_reg_rtx (<MODE>mode);
      emit_move_insn (reg, src);
      emit_move_insn (dest, reg);
      DONE;
    }
    //debug_rtx(dest);
    //debug_rtx(src); 

    //if (register_operand (dest, <MODE>mode)){
    //  DONE;
    //}
  })
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
"@
mov %0 ,%1
mov %0 ,%1
ld %0 ,%1
st %1 ,%0"
)




;; -------------------------------------------------------------------------
;; Call and Jump instructions
;; -------------------------------------------------------------------------


;; call

;; Operand 1 not used on the AVR.
;; Operand 2 is 1 for tail-call, 0 otherwise.
(define_expand "call"
  [(parallel[(call (match_operand:HI 0 "call_insn_operand" "")
                   (match_operand:HI 1 "general_operand" ""))
             (use (const_int 0))])])

;; Operand 1 not used on the AVR.
;; Operand 2 is 1 for tail-call, 0 otherwise.TODO:treat leaf!!! 

(define_expand "sibcall"
  [(parallel[(call (match_operand:HI 0 "call_insn_operand" "")
                   (match_operand:HI 1 "general_operand" ""))
             (use (const_int 1))])])

;; call value

;; Operand 2 not used on the AVR.
;; Operand 3 is 1 for tail-call, 0 otherwise.
(define_expand "call_value"
  [(parallel[(set (match_operand 0 "register_operand" "")
                  (call (match_operand:HI 1 "call_insn_operand" "")
                        (match_operand:HI 2 "general_operand" "")))
             (use (const_int 0))])])

;; Operand 2 not used on the AVR.
;; Operand 3 is 1 for tail-call, 0 otherwise.
(define_expand "sibcall_value"
  [(parallel[(set (match_operand 0 "register_operand" "")
                  (call (match_operand:HI 1 "call_insn_operand" "")
                        (match_operand:HI 2 "general_operand" "")))
             (use (const_int 1))])])


(define_insn "call_value_insn"
  [(parallel[(set (match_operand 0 "register_operand"                   )
                  (call (mem:HI (match_operand:HI 1 "nonmemory_operand"  ))
                        (match_operand:HI 2 "general_operand"           )))
             (use (match_operand:HI 3 "const_int_operand"               ))])];;todo:register jump is for assembler to complete
  ;; Operand 2 not used on the AVR.
  ;; Operand 3 is 1 for tail-call, 0 otherwise.
  ""
  { 
   if (INTVAL(operands[3]) == 1) 
    {
       return "B %1";
    }
    else
    {
      return "BL %1";
    }
  }
  )

(define_insn "call_insn"
  [(parallel[(call (mem:HI (match_operand:HI 0 "nonmemory_operand" ))
                   (match_operand:HI 1 "general_operand"           ))
             (use (match_operand:HI 2 "const_int_operand"          ))])]
  ;; Operand 1 not used on the AVR.
  ;; Operand 2 is 1 for tail-call, 0 otherwise.
  ""
  { 
   if (INTVAL(operands[2]) == 1) 
    {
       return "B %0";
    }
    else
    {
      return "BL %0";
    }
  }
  )



(define_insn "indirect_jump"
    [(set (pc) (match_operand:HI 0 "nonimmediate_operand" "r"))]
    ""
    "jmpr %0")

(define_insn "jump"
    [(set (pc) (label_ref (match_operand 0 "" "")))]
    ""
    "jmp %l0")

;; -------------------------------------------------------------------------
;; Compare instructions
;; -------------------------------------------------------------------------



(define_expand "cbranch<mode>4"
  [(set (reg:CC REG_CC)
        (compare:CC
         (match_operand:ALL1234 1 "general_operand" "")
         (match_operand:ALL1234 2 "general_operand" "")))
   (set (pc)
        (if_then_else (match_operator 0 "comparison_operator"
                       [(reg:CC REG_CC) (const_int 0)])
                      (label_ref (match_operand 3 "" ""))
                      (pc)))]
  ""
  "
  /* Force the compare operands into registers.  */
  if (GET_CODE (operands[1]) != REG)
	operands[1] = force_reg (GET_MODE(operands[1]), operands[1]);
  ")

(define_insn "*cmp<mode>"
  [(set (reg:CC REG_CC)
	(compare
	 (match_operand:ALL1234 0 "register_operand" "r,r")
	 (match_operand:ALL1234 1 "nonmemory_operand"	"r,i")))]
  ""
  "cmp %0 %1")

;; -------------------------------------------------------------------------
;; Branch instructions
;; -------------------------------------------------------------------------

(define_code_iterator cond [ne eq lt ltu gt gtu ge le geu leu])
(define_code_attr CC [(ne "ne") (eq "eq") (lt "lt") (ltu "ltu")
		      (gt "gt") (gtu "gtu") (ge "ge") (le "le")
		      (geu "geu") (leu "leu") ])

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
  nxu16_expand_epilogue ();
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
    "iret")

(define_insn "sti"
    [(unspec_volatile [(const_int 0)] UNSPECV_STI)]
    ""
    "sti")

(define_insn "cli"
    [(unspec_volatile [(const_int 0)] UNSPECV_CLI)]
    ""
    "cli")

;; Local Variables:
;; mode: lisp
;; indent-tabs-mode: t
;; End:
