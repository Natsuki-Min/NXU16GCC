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

;;(include "constraints.md")
;;(include "predicates.md")
(define_constants
  [(REG_0   0)   (REG_1   1)   (REG_2   2)	
   (REG_8   8)   (REG_9   9)   (REG_10 10)   (REG_11 11)
   (REG_12 12)   (REG_13 13)   (REG_14 14)   (REG_15 15)
   (REG_16 16)   (REG_17 17)   (REG_18 18)   (REG_19 19)
   ])

(define_constants
  [
   (REG_SP      REG_8)
   (REG_CC      REG_9)
   ])
(define_mode_iterator QIHI  [QI HI])


; Most instructions are two bytes long.
(define_attr "length" "" (const_int 2))

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

(define_insn "addhi3"
    [(set (match_operand:HI 0 "register_operand" "=r,r")
	  (plus:HI
	   (match_operand:HI 1 "register_operand" "0,0")
	   (match_operand:HI 2 "general_operand" "r,i")))]
           "reload_completed"
           "add %0 %2"
	   [])

(define_insn "addqi3"
    [(set (match_operand:QI 0 "register_operand" "=r,r")
	  (plus:QI
	   (match_operand:QI 1 "register_operand" "0,0")
	   (match_operand:QI 2 "general_operand" "r,i")))]
           "reload_completed"
           "add %0 %2"
	   [])
	   
(define_insn "subhi3"
    [(set (match_operand:HI 0 "register_operand" "=r,r")
	  (minus:HI
	   (match_operand:HI 1 "register_operand" "0,0")
	   (match_operand:HI 2 "general_operand" "r,i")))]
           ""
           "sub %0 %2"
	   [])
(define_insn "subqi3"
    [(set (match_operand:QI 0 "register_operand" "=r,r")
	  (minus:QI
	   (match_operand:QI 1 "register_operand" "0,0")
	   (match_operand:QI 2 "general_operand" "r,i")))]
           "reload_completed"
           "sub %0 %2"
	   [])

(define_insn "mulhi3"
    [(set (match_operand:HI 0 "register_operand" "=r")
	  (mult:HI
	   (match_operand:HI 1 "register_operand" "0")
	   (match_operand:HI 2 "register_operand" "r")))]
	   ""
	   "mul %0 %2")

(define_insn "divhi3"
    [(set (match_operand:HI 0 "register_operand" "=r")
	  (div:HI
	   (match_operand:HI 1 "register_operand" "0")
	   (match_operand:HI 2 "register_operand" "r")))]
	   ""
	   "div %0 %2")

(define_insn "ashlhi3"
    [(set (match_operand:HI 0 "register_operand" "=r")
	  (ashift:HI
	   (match_operand:HI 1 "register_operand" "0")
	   (match_operand:HI 2 "register_operand" "r")))]
           ""
           "sll %0 %2")

(define_insn "ashrhi3"
    [(set (match_operand:HI 0 "register_operand" "=r")
	  (ashiftrt:HI
	   (match_operand:HI 1 "register_operand" "0")
	   (match_operand:HI 2 "register_operand" "r")))]
           ""
           "srl %0 %2")

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

(define_insn "andhi3"
    [(set (match_operand:HI 0 "register_operand" "=r")
	  (and:HI (match_operand:HI 1 "register_operand" "0")
		  (match_operand:HI 2 "register_operand" "r")))]
                  ""
                  {
                  return "and %0 %2";
                  })

(define_insn "xorhi3"
    [(set (match_operand:HI 0 "register_operand" "=r")
	  (xor:HI (match_operand:HI 1 "register_operand" "0")
		  (match_operand:HI 2 "register_operand" "r")))]
                  ""
                  {
                  return "xor %0 %2";
                  })

(define_insn "iorhi3"
    [(set (match_operand:HI 0 "register_operand" "=r")
	  (ior:HI (match_operand:HI 1 "register_operand" "0")
		  (match_operand:HI 2 "register_operand" "r")))]
                  ""
                  {
                  return "or %0 %2";
                  })

;; -------------------------------------------------------------------------
;; Move instructions
;; -------------------------------------------------------------------------

(define_insn "movhi_push"
    [(set (mem:HI (pre_dec:HI (reg:HI 1)))
  	  (match_operand:HI 0 "register_operand" "r"))]
          ""
          "push %0")

(define_insn "movhi_pop"
    [(set (match_operand:HI 1 "register_operand" "=r")
  	  (mem:HI (post_inc:HI (match_operand:HI 0 "register_operand" "r"))))]
          ""
          "pop %1")

(define_expand "mov<mode>"
  [(set (match_operand:QIHI 0 "nonimmediate_operand" "")
        (match_operand:QIHI 1 "general_operand" ""))]
  ""
  {
    rtx dest = operands[0];
    rtx src  = operands[1];


    debug_rtx(dest);
    debug_rtx(src); 

    //if (register_operand (dest, <MODE>mode)){
    //  DONE;
    //}
  })
	  (define_insn_and_split "mov<mode>_insn_split"
	[(set (match_operand:QIHI 0 "nonimmediate_operand"    "=r,r,r,m")
	      (match_operand:QIHI 1 "general_operand"          "r,i,m,r"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (match_dup 1))
              (clobber (reg:CC REG_CC))])])


(define_insn "movhi_insn"
[(set (match_operand:HI 0 "nonimmediate_operand"    "=r,r,r,m")
      (match_operand:HI 1 "general_operand"          "r,i,m,r"))
      (clobber (reg:CC REG_CC))]
"reload_completed"
"@
mov %0 ,%1
mov %0 ,%1
ld %0 ,%1
st %1 ,%0"
)
(define_insn "movqi_insn"
[(set (match_operand:QI 0 "nonimmediate_operand"    "=r,r,r,m")
      (match_operand:QI 1 "general_operand"          "r,i,m,r"))
      (clobber (reg:CC REG_CC))]
"reload_completed"	
"@
movqi %0 ,%1
movqi %0 ,%1
ldqi %0 ,%1
stqi %1 ,%0"
)

(define_split
    [(set (match_operand:HI 0 "nonimmediate_operand" "")
	  (match_operand:HI 1 "general_operand" ""))]
	  "(
          ((GET_CODE (operands[0]) == MEM) && (GET_CODE (XEXP (operands[0], 0)) == SYMBOL_REF)) ||
          ((GET_CODE (operands[1]) == MEM) && (GET_CODE (XEXP (operands[1], 0)) == SYMBOL_REF)) ||
          ((GET_CODE (operands[0]) == MEM) && (GET_CODE (XEXP (operands[0], 0)) == CONST)) ||
          ((GET_CODE (operands[1]) == MEM) && (GET_CODE (XEXP (operands[1], 0)) == CONST)))&&!reload_completed
          "
	  [(const_int 0)]
	  {
	    //nxu16_split_symbolic_move (operands[0], operands[1]);
	    DONE;
	  })
	  

;; -------------------------------------------------------------------------
;; Call and Jump instructions
;; -------------------------------------------------------------------------

(define_expand "call"
    [(call (match_operand:HI 0 "memory_operand" "")
	   (match_operand 1 "general_operand" ""))]
           ""
           {
           gcc_assert (MEM_P (operands[0]));
           })

(define_insn "*call"
    [(call (mem:HI (match_operand:HI
		    0 "nonmemory_operand" "i,r"))
	   (match_operand 1 "" ""))]
           ""
           "@
           call %0
           callr %0"
           [(set_attr "length"	"4,2")])

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



(define_expand "cbranchhi4"
  [(set (reg:CC REG_CC)
        (compare:CC
         (match_operand:HI 1 "general_operand" "")
         (match_operand:HI 2 "general_operand" "")))
   (set (pc)
        (if_then_else (match_operator 0 "comparison_operator"
                       [(reg:CC REG_CC) (const_int 0)])
                      (label_ref (match_operand 3 "" ""))
                      (pc)))]
  ""
  "
  /* Force the compare operands into registers.  */
  if (GET_CODE (operands[1]) != REG)
	operands[1] = force_reg (HImode, operands[1]);
  if (GET_CODE (operands[2]) != REG)
	operands[2] = force_reg (HImode, operands[2]);
  ")

(define_insn "*cmphi"
  [(set (reg:CC REG_CC)
	(compare
	 (match_operand:HI 0 "register_operand" "r")
	 (match_operand:HI 1 "register_operand"	"r")))]
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
    [(clobber (const_int 0))]
    ""
    "
{
  nxu16_expand_prologue ();
  DONE;
}
")

(define_expand "epilogue"
    [(return)]
    ""
    "
{
  nxu16_expand_epilogue ();
  DONE;
}
")

(define_insn "ret"
    [(return)]
    "reload_completed"
    "ret")

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
