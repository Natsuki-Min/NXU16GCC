(include "constraints.md")
(include "predicates.md")



(define_insn "nop"
[(const_int 0)]
""
"nop")

;;-----------------------------------


(define_insn "addHI2"
[(set (match_operand:HI 0 "register_operand" "=r,r")
(plus:HI
(match_operand:HI 1 "register_operand" "0,0")
(match_operand:HI 2 "nonmemory_operand" "i,r")))]
""
"add %0 ,%2"
[])

(define_insn "subHI2"
[(set (match_operand:HI 0 "register_operand" "=r,r")
(minus:HI
(match_operand:HI 1 "register_operand" "0,0")
(match_operand:HI 2 "nonmemory_operand" "i,r")))]
""
"sub %0 ,%2"
[])

(define_insn "ashlHI2"
[(set (match_operand:HI 0 "register_operand" "=r")
(ashift:HI
(match_operand:HI 1 "register_operand" "0")
(match_operand:HI 2 "register_operand" "r")))]
""
"sll %0 ,%2")

(define_insn "ashrHI2"
[(set (match_operand:HI 0 "register_operand" "=r")
(ashiftrt:HI
(match_operand:HI 1 "register_operand" "0")
(match_operand:HI 2 "register_operand" "r")))]
""
"srl %0 ,%2")

(define_insn "one_cmplHI"
[(set (match_operand:HI 0 "register_operand" "=r")
(not:HI (match_operand:HI 1 "register_operand" "0")))]
""
"not %0")


(define_insn "andHI2"
[(set (match_operand:HI 0 "register_operand" "=r")
(and:HI (match_operand:HI 1 "register_operand" "0")
(match_operand:HI 2 "register_operand" "r")))]
""
"and %0 ,%2")

(define_insn "xorHI2"
[(set (match_operand:HI 0 "register_operand" "=r")
(xor:HI (match_operand:HI 1 "register_operand" "0")
(match_operand:HI 2 "register_operand" "r")))]
""
"xor %0 ,%2")

(define_insn "iorHI2"
[(set (match_operand:HI 0 "register_operand" "=r")
(ior:HI (match_operand:HI 1 "register_operand" "0")
(match_operand:HI 2 "register_operand" "r")))]
""
"or %0 ,%2")

;; ---------------------------

(define_insn "pushHI"
  [(set (mem:HI (post_dec:HI (reg:HI SP_REG)))
        (match_operand:HI 0 "nonimmediate_operand" "r"))]
  ""
  "push %0"
)

(define_insn "popHI"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r")
  	(mem:HI (post_inc:HI (reg:HI SP_REG))))]
  ""
  "push %0"
)


(define_insn "movHI"
[(set (match_operand:HI 0 "nonimmediate_operand"    "=r,r,r,m")
      (match_operand:HI 1 "general_operand"          "r,i,m,r"))]
""
"@
mov %0 ,%1
mov %0 ,%1
ld %0 ,%1
st %0 ,%1"
)




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
call\t%0
callr\t%0"
[])

(define_insn "indirect_jump"
[(set (pc) (match_operand:HI 0 "nonimmediate_operand" "r"))]
""
"jmpr\t%0")

(define_insn "jump"
[(set (pc) (label_ref (match_operand 0 "" "")))]
""
"jmp\t%l0")


;; -------------------------------------------------------------------------


(define_expand "cbranchHI4"
[(set (reg:CC CC_REG)
(compare:CC
(match_operand:HI 1 "general_operand" "")
(match_operand:HI 2 "general_operand" "")))
(set (pc)
(if_then_else (match_operator 0 "comparison_operator"
[(reg:CC CC_REG) (const_int 0)])
(label_ref (match_operand 3 "" ""))
(pc)))]
""
{
/* Force the compare operands into registers.  */
if (GET_CODE (operands[1]) != REG)
operands[1] = force_reg (HImode, operands[1]);
if (GET_CODE (operands[2]) != REG)
operands[2] = force_reg (HImode, operands[2]);
})


(define_insn "*cmpHI"
[(set (reg:CC CC_REG)
(compare
(match_operand:HI 0 "register_operand" "r")
(match_operand:HI 1 "register_operand" "r")))]
""
"cmp %0 ,%1")

;; -------------------------------------------------------------------------


(define_code_iterator cond [ne eq lt ltu gt gtu ge le geu leu])

(define_code_attr CC [(ne "ne") (eq "eq") (lt "lt") (ltu "ltu")
(gt "gt") (gtu "gtu") (ge "ge") (le "le")
(geu "geu") (leu "leu") ])

(define_insn "*bcond:code"
[(set (pc)
(if_then_else (cond (reg:CC CC_REG)
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
