(include "constraints.md")
(include "predicates.md")
(define_constants
  [(REG_0   0)   (REG_1   1)   (REG_2   2)	
   (REG_8   8)   (REG_9   9)   (REG_10 10)   (REG_11 11)
   (REG_12 12)   (REG_13 13)   (REG_14 14)   (REG_15 15)
   (REG_16 16)   (REG_17 17)   (REG_18 18)   (REG_19 19)
   ])

(define_constants
  [
   (REG_SP      REG_18)
   (REG_CC      REG_19)
   ])



(define_insn "nop"
[(const_int 0)]
""
"nop")

;;-----------------------------------


(define_insn "addQI2"
[(set (match_operand:QI 0 "register_operand" "=r,r")
(plus:QI
(match_operand:QI 1 "register_operand" "0,0")
(match_operand:QI 2 "nonmemory_operand" "i,r")))]
""
"add %0 ,%2"
[])

(define_insn "subQI2"
[(set (match_operand:QI 0 "register_operand" "=r,r")
(minus:QI
(match_operand:QI 1 "register_operand" "0,0")
(match_operand:QI 2 "nonmemory_operand" "i,r")))]
""
"sub %0 ,%2"
[])

(define_insn "ashlQI2"
[(set (match_operand:QI 0 "register_operand" "=r")
(ashift:QI
(match_operand:QI 1 "register_operand" "0")
(match_operand:QI 2 "register_operand" "r")))]
""
"sll %0 ,%2")

(define_insn "ashrQI2"
[(set (match_operand:QI 0 "register_operand" "=r")
(ashiftrt:QI
(match_operand:QI 1 "register_operand" "0")
(match_operand:QI 2 "register_operand" "r")))]
""
"srl %0 ,%2")

(define_insn "one_cmplQI"
[(set (match_operand:QI 0 "register_operand" "=r")
(not:QI (match_operand:QI 1 "register_operand" "0")))]
""
"not %0")


(define_insn "andQI2"
[(set (match_operand:QI 0 "register_operand" "=r")
(and:QI (match_operand:QI 1 "register_operand" "0")
(match_operand:QI 2 "register_operand" "r")))]
""
"and %0 ,%2")

(define_insn "xorQI2"
[(set (match_operand:QI 0 "register_operand" "=r")
(xor:QI (match_operand:QI 1 "register_operand" "0")
(match_operand:QI 2 "register_operand" "r")))]
""
"xor %0 ,%2")

(define_insn "iorQI2"
[(set (match_operand:QI 0 "register_operand" "=r")
(ior:QI (match_operand:QI 1 "register_operand" "0")
(match_operand:QI 2 "register_operand" "r")))]
""
"or %0 ,%2")

;; ---------------------------

(define_insn "pushQI"
  [(set (mem:QI (post_dec:QI (reg:QI REG_SP)))
        (match_operand:QI 0 "nonimmediate_operand" "r"))]
  ""
  "push %0"
)

(define_insn "popQI"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r")
  	(mem:QI (post_inc:QI (reg:QI REG_SP))))]
  ""
  "push %0"
)


(define_insn "movQI"
[(set (match_operand:QI 0 "nonimmediate_operand"    "=r,r,r,m")
      (match_operand:QI 1 "general_operand"          "r,i,m,r"))]
""
"@
mov %0 ,%1
mov %0 ,%1
ld %0 ,%1
st %0 ,%1"
)




;; -------------------------------------------------------------------------





(define_expand "call"
[(call (match_operand:QI 0 "memory_operand" "")
(match_operand 1 "general_operand" ""))]
""
{
gcc_assert (MEM_P (operands[0]));
})

(define_insn "*call"
[(call (mem:QI (match_operand:QI
0 "nonmemory_operand" "i,r"))
(match_operand 1 "" ""))]
""
"@
call\t%0
callr\t%0"
[])

(define_insn "indirect_jump"
[(set (pc) (match_operand:QI 0 "nonimmediate_operand" "r"))]
""
"jmpr\t%0")

(define_insn "jump"
[(set (pc) (label_ref (match_operand 0 "" "")))]
""
"jmp\t%l0")


;; -------------------------------------------------------------------------


(define_expand "cbranchQI4"
[(set (reg:CC REG_CC)
(compare:CC
(match_operand:QI 1 "general_operand" "")
(match_operand:QI 2 "general_operand" "")))
(set (pc)
(if_then_else (match_operator 0 "comparison_operator"
[(reg:CC REG_CC) (const_int 0)])
(label_ref (match_operand 3 "" ""))
(pc)))]
""
{
/* Force the compare operands into registers.  */
if (GET_CODE (operands[1]) != REG)
operands[1] = force_reg (QImode, operands[1]);
if (GET_CODE (operands[2]) != REG)
operands[2] = force_reg (QImode, operands[2]);
})


(define_insn "*cmpQI"
[(set (reg:CC REG_CC)
(compare
(match_operand:QI 0 "register_operand" "r")
(match_operand:QI 1 "register_operand" "r")))]
""
"cmp %0 ,%1")

;; -------------------------------------------------------------------------


(define_code_iterator cond [ne eq lt ltu gt gtu ge le geu leu])

(define_code_attr CC [(ne "ne") (eq "eq") (lt "lt") (ltu "ltu")
(gt "gt") (gtu "gtu") (ge "ge") (le "le")
(geu "geu") (leu "leu") ])

(define_insn "*bcond:code"
[(set (pc)
(if_then_else (cond (reg:CC REG_CC)
(const_int 0))
(label_ref (match_operand 0 "" ""))
(pc)))]
""
{
return "b %l0";
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
