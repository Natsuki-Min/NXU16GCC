;;TODO:should we really need fix mode?
;; "addqi3"
;; "addqq3" "adduqq3"
(define_insn_and_split "add<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"            "=r,r")
        (plus:ALL1 (match_operand:ALL1 1 "register_operand" "%0,0")
                   (match_operand:ALL1 2 "nonmemory_operand" "r,i")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:ALL1 (match_dup 1)
                              (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*add<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"            "=r,r")
        (plus:ALL1 (match_operand:ALL1 1 "register_operand" "%0,0")
                   (match_operand:ALL1 2 "nonmemory_operand" "r,i")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "add %0,%2"
  [(set_attr "length" "1")])

;; "addhi3"
;; "addhq3" "adduhq3"
;; "addha3" "adduha3"
(define_expand "add<mode>3"
  [(set (match_operand:ALL2 0 "register_operand" "")
        (plus:ALL2 (match_operand:ALL2 1 "register_operand" "")
                   (match_operand:ALL2 2 "nonmemory_operand" "")))]
  ""
  {
printf("add~~~~~~~~~\n");
debug_rtx(operands[1]);
debug_rtx(operands[2]);

// Check if operand[1] is either:
// 1. The physical stack pointer (SP), or
// 2. A virtual stack register (like virtual-stack-vars)
if ((REG_P(operands[1]) && 
    ((REGNO(operands[1]) == STACK_POINTER_REGNUM) || 
     (REGNO(stack_pointer_rtx )==REGNO(operands[1]))))) {
     printf("add~~~~~sp~~~~\n");
    emit_insn(gen_addhi3_noclobber(operands[0], operands[1], operands[2]));
    debug_rtx(gen_addhi3_noclobber(operands[0], operands[1], operands[2]));
    DONE;
} else {
    emit_insn(gen_add<mode>3_clobber(operands[0], operands[1], operands[2]));
    DONE;
}})


(define_insn_and_split "*addhi3_zero_extend_split"
  [(set (match_operand:HI 0 "register_operand"                         "=r,*?r")
        (plus:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "r  ,0"))
                 (match_operand:HI 2 "register_operand"                 "0  ,r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:HI (zero_extend:HI (match_dup 1))
                            (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*addhi3_zero_extend"
  [(set (match_operand:HI 0 "register_operand"                         "=r,*?r")
        (plus:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "r  ,0"))
                 (match_operand:HI 2 "register_operand"                 "0  ,r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
	add %A0,%1\;adc %B0,0
	add %A0,%A2\;mov %B0,%B2\;adc %B0,0"
  [(set_attr "length" "2,3")])

(define_insn_and_split "*addhi3_zero_extend1_split"
  [(set (match_operand:HI 0 "register_operand"                         "=r")
        (plus:HI (match_operand:HI 1 "register_operand"                 "0")
                 (zero_extend:HI (match_operand:QI 2 "register_operand" "r"))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:HI (match_dup 1)
                            (zero_extend:HI (match_dup 2))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*addhi3_zero_extend1"
  [(set (match_operand:HI 0 "register_operand"                         "=r")
        (plus:HI (match_operand:HI 1 "register_operand"                 "0")
                 (zero_extend:HI (match_operand:QI 2 "register_operand" "r"))))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "add %A0,%2\;adc %B0,0"
  [(set_attr "length" "2")])


;; Adding symbols that are aligned to 256 bytes can
;; save up to two instructions.
(define_insn "*aligned_add_symbol"
  [(set (match_operand:HI 0 "register_operand"                         "=r")
        (plus:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "r"))
                 (match_operand:HI 2 "const_0mod256_operand"            "Clo80")))]
  ""
 "mov %B0,hi8(%2)"
  [(set_attr "length" "1")])

;; Occurs when computing offsets into 16-bit arrays.
;; save one reg(otherwise mov sllc sll add )
(define_insn_and_split "*addhi3_zero_extend.ashift1.split"
  [(set (match_operand:HI 0 "register_operand"                                    "=r")
        (plus:HI (ashift:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "r"))
                            (const_int 1))
                 (match_operand:HI 2 "register_operand"                            "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:HI (ashift:HI (zero_extend:HI (match_dup 1))
                                       (const_int 1))
                            (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*addhi3_zero_extend.ashift1"
  [(set (match_operand:HI 0 "register_operand"                                    "=r")
        (plus:HI (ashift:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "r"))
                            (const_int 1))
                 (match_operand:HI 2 "register_operand"                            "0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
	"add %A0,%1\;adc %B0,0\;add %A0,%1\;adc %B0,0"
  [(set_attr "length" "4")])


(define_insn_and_split "*usum_widenqihi3_split"
  [(set (match_operand:HI 0 "register_operand"                          "=r")
        (plus:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "%0"))
                 (zero_extend:HI (match_operand:QI 2 "register_operand"  "r"))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:HI (zero_extend:HI (match_dup 1))
                            (zero_extend:HI (match_dup 2))))
              (clobber (reg:CC REG_CC))])])


(define_insn "*usum_widenqihi3"
  [(set (match_operand:HI 0 "register_operand"                          "=r")
        (plus:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "%0"))
                 (zero_extend:HI (match_operand:QI 2 "register_operand"  "r"))))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "add %A0,%2\;mov %B0,0\;addc %B0,0"
  [(set_attr "length" "3")])

(define_insn_and_split "*udiff_widenqihi3_split"
  [(set (match_operand:HI 0 "register_operand"                           "=r")
        (minus:HI (zero_extend:HI (match_operand:QI 1 "register_operand"  "0"))
                  (zero_extend:HI (match_operand:QI 2 "register_operand"  "r"))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (minus:HI (zero_extend:HI (match_dup 1))
                             (zero_extend:HI (match_dup 2))))
              (clobber (reg:CC REG_CC))])])

(define_insn "*udiff_widenqihi3"
  [(set (match_operand:HI 0 "register_operand"                           "=r")
        (minus:HI (zero_extend:HI (match_operand:QI 1 "register_operand"  "0"))
                  (zero_extend:HI (match_operand:QI 2 "register_operand"  "r"))))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "sub %A0,%2\;subc %B0,%B0"
  [(set_attr "length" "2")])




;; "*addhi3"
;; "*addhq3"  "*adduhq3"
;; "*addha3"  "*adduha3"
(define_insn_and_split "add<mode>3_clobber"
  [(set (match_operand:ALL2 0 "register_operand"                   "=r,r,r")
        (plus:ALL2 (match_operand:ALL2 1 "register_operand"          "%0,0,0")
                   (match_operand:ALL2 2 "nonmemory_operand" "r,Cim7i,??i")))]
  "((REGNO (operands[0]))!=STACK_POINTER_REGNUM)"
  "#"
  "&& reload_completed
  &&((REGNO_REG_CLASS (REGNO (operands[0]))==GENERAL_REGS))"
  [(parallel [(set (match_dup 0)
                   (plus:ALL2 (match_dup 1)
                              (match_dup 2)))
              (clobber (reg:CC REG_CC))])])
(define_insn "*add<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"                   "=r,r,r")
        (plus:ALL2 (match_operand:ALL2 1 "register_operand"          "%0,0,0")
                   (match_operand:ALL2 2 "nonmemory_operand" "r,Cim7i,??i")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
  add %0,%2
  add %0,%2
  add %A0,lo8(%2)\;addc %B0,hi8(%2)"
  [(set_attr "length" "1,1,2")])




;; "addhi3_clobber"
;; "addhq3_clobber" "adduhq3_clobber"
;; "addha3_clobber" "adduha3_clobber"
(define_insn "addhi3_noclobber"
  [(set (match_operand:HI 0 "stack_register_operand"            "=YSP")
        (plus:HI (match_operand:HI 1 "stack_register_operand"  "%0  ")
                   (match_operand:HI 2 "const_int_operand"     "i")))]
  "reload_completed"
  {debug_rtx(operands[0]);
 return "add %0,%2";}
  [(set_attr "length" "1")])






;; "addsi3"
;; "addsq3" "addusq3"
;; "addsa3" "addusa3"
(define_insn_and_split "add<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"          "=r,r")
        (plus:ALL4 (match_operand:ALL4 1 "register_operand" "%0,0")
                   (match_operand:ALL4 2 "nonmemory_operand" "r,i")))
  ]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:ALL4 (match_dup 1)
                              (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*add<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"          "=r,r")
        (plus:ALL4 (match_operand:ALL4 1 "register_operand" "%0,0")
                   (match_operand:ALL4 2 "nonmemory_operand" "r,i")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
  add e%A0,e%A2\;addc %C0,%C2\;addc %D0,%D2
  add %A0,lo8(%2)\;addc %B0,hi8(%2)\;addc %C0,hlo8(%2)\;addc %D0,hhi8(%2)"
  [(set_attr "length" "3,4")
   ])
;;todo:add ern ,imm

;;----------------extend_add

;;                                  "*addhi3.sign_extend.qi_split"
;; "*addpsi3.zero_extend.qi_split"  "*addpsi3.sign_extend.qi_split"
;; "*addpsi3.zero_extend.hi_split"  "*addpsi3.sign_extend.hi_split"
;; "*addsi3.zero_extend.qi_split"   "*addsi3.sign_extend.qi_split"
;; "*addsi3.zero_extend.hi_split"   "*addsi3.sign_extend.hi_split"
;; "*addsi3.zero_extend.psi_split"  "*addsi3.sign_extend.psi_split"
;; The zero_extend:HI(QI) case is treated in an own insn as it can
;; more than just "r,r,0".
(define_insn_and_split "*add<HISI:mode>3.zero_extend.<QIPSI:mode>_split"
  [(set (match_operand:HISI 0 "register_operand"                             "=r")
        (plus:HISI (zero_extend:HISI (match_operand:QIPSI 1 "register_operand" "r"))
                   (match_operand:HISI 2 "register_operand"                   "0")))]
  "<HISI:SIZE> > <QIPSI:SIZE>
   && (<HISI:SIZE> > 2 )"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:HISI (zero_extend:HISI (match_dup 1))
                              (match_dup 2)))
              (clobber (reg:CC REG_CC))])])
(define_insn_and_split "*add<HISI:mode>3.sign_extend.<QIPSI:mode>_split"
  [(set (match_operand:HISI 0 "register_operand"                             "=r")
        (plus:HISI (sign_extend:HISI (match_operand:QIPSI 1 "register_operand" "r"))
                   (match_operand:HISI 2 "register_operand"                   "0")))
                   (clobber (match_scratch:QI 3                             "=&r"))]
  "<HISI:SIZE> > <QIPSI:SIZE>"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:HISI (sign_extend:HISI (match_dup 1))
                              (match_dup 2)))
               (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])

;;                            "*addhi3.sign_extend.qi"
;; "*addpsi3.zero_extend.qi"  "*addpsi3.sign_extend.qi"
;; "*addpsi3.zero_extend.hi"  "*addpsi3.sign_extend.hi"
;; "*addsi3.zero_extend.qi"   "*addsi3.sign_extend.qi"
;; "*addsi3.zero_extend.hi"   "*addsi3.sign_extend.hi"
;; "*addsi3.zero_extend.psi"  "*addsi3.sign_extend.psi"
(define_insn "*add<HISI:mode>3.zero_extend.<QIPSI:mode>"
  [(set (match_operand:HISI 0 "register_operand"                             "=r")
        (plus:HISI (zero_extend:HISI (match_operand:QIPSI 1 "register_operand" "r"))
                   (match_operand:HISI 2 "register_operand"                   "0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed
   && <HISI:SIZE> > <QIPSI:SIZE>
   && (<HISI:SIZE> > 2)"
  {
    int bytes = <HISI:SIZE>;
    int src_bytes = <QIPSI:SIZE>;
    char buf[20];
    if (src_bytes>=2){
    output_asm_insn("add e%A0,e%A1", operands);
    
    for (int i = 2; i < src_bytes; i++) {
        sprintf(buf, "addc %%%c0,%%%c1", 'A' + i, 'A' + i);
        output_asm_insn(buf, operands);
    }}else{output_asm_insn("add %A0,%A1", operands);
    
    for (int i = 1; i < src_bytes; i++) {
        sprintf(buf, "addc %%%c0,%%%c1", 'A' + i, 'A' + i);
        output_asm_insn(buf, operands);
    }}
    
    for (int i = src_bytes; i < bytes; i++) {
        sprintf(buf, "addc %%%c0,0", 'A' + i);
        output_asm_insn(buf, operands);
    }

    return "";
}
  [(set (attr "length")
        (symbol_ref "<HISI:SIZE> "))])
(define_insn "*add<HISI:mode>3.sign_extend.<QIPSI:mode>"
  [(set (match_operand:HISI 0 "register_operand"                             "=r")
        (plus:HISI (sign_extend:HISI (match_operand:QIPSI 1 "register_operand" "r"))
                   (match_operand:HISI 2 "register_operand"                   "0")))
                   (clobber (match_scratch:QI 3                             "=&r"))
   (clobber (reg:CC REG_CC))]
  "reload_completed
   && <HISI:SIZE> > <QIPSI:SIZE>
   "
{
    int bytes = <HISI:SIZE>;
    int src_bytes = <QIPSI:SIZE>;
    char buf[32];  // Buffer for formatted strings
    sprintf(buf, "mov %%3,%%%c1", 'A' + src_bytes - 1);
    output_asm_insn(buf, operands);
    
    output_asm_insn("sra %3,7", operands);
    if (src_bytes>=2){
    output_asm_insn("add e%A0,e%A1", operands);
    
    for (int i = 2; i < src_bytes; i++) {
        sprintf(buf, "addc %%%c0,%%%c1", 'A' + i, 'A' + i);
        output_asm_insn(buf, operands);
    }}else{output_asm_insn("add %A0,%A1", operands);
    
    for (int i = 1; i < src_bytes; i++) {
        sprintf(buf, "addc %%%c0,%%%c1", 'A' + i, 'A' + i);
        output_asm_insn(buf, operands);
    }}


    
    for (int i = src_bytes; i < bytes; i++) {
        sprintf(buf, "addc %%%c0,%%3", 'A' + i);
        output_asm_insn(buf, operands);
    }

    return "";
}
  [(set (attr "length")
        (symbol_ref "<HISI:SIZE> + 2"))])

;; "*subhi3.zero_extend.qi_split"   "*subhi3.sign_extend.qi_split"
;; "*subpsi3.zero_extend.qi_split"  "*subpsi3.sign_extend.qi_split"
;; "*subpsi3.zero_extend.hi_split"  "*subpsi3.sign_extend.hi_split"
;; "*subsi3.zero_extend.qi_split"   "*subsi3.sign_extend.qi_split"
;; "*subsi3.zero_extend.hi_split"   "*subsi3.sign_extend.hi_split"
;; "*subsi3.zero_extend.psi_split"  "*subsi3.sign_extend.psi_split"
(define_insn_and_split "*sub<HISI:mode>3.zero_extend.<QIPSI:mode>_split"
  [(set (match_operand:HISI 0 "register_operand"                              "=r")
        (minus:HISI (match_operand:HISI 1 "register_operand"                   "0")
                    (zero_extend:HISI (match_operand:QIPSI 2 "register_operand" "r"))))]
  "<HISI:SIZE> > <QIPSI:SIZE> && (<HISI:SIZE> > 2 )"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (minus:HISI (match_dup 1)
                               (zero_extend:HISI (match_dup 2))))
              (clobber (reg:CC REG_CC))])])

(define_insn_and_split "*sub<HISI:mode>3.sign_extend.<QIPSI:mode>_split"
  [(set (match_operand:HISI 0 "register_operand"                              "=r")
        (minus:HISI (match_operand:HISI 1 "register_operand"                   "0")
                    (sign_extend:HISI (match_operand:QIPSI 2 "register_operand" "r"))))
   (clobber (match_scratch:QI 3                                              "=&r"))]
  "<HISI:SIZE> > <QIPSI:SIZE>"
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (minus:HISI (match_dup 1)
                               (sign_extend:HISI (match_dup 2))))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])

;; "*subhi3.zero_extend.qi"   "*subhi3.sign_extend.qi"
;; "*subpsi3.zero_extend.qi"  "*subpsi3.sign_extend.qi"
;; "*subpsi3.zero_extend.hi"  "*subpsi3.sign_extend.hi"
;; "*subsi3.zero_extend.qi"   "*subsi3.sign_extend.qi"
;; "*subsi3.zero_extend.hi"   "*subsi3.sign_extend.hi"
;; "*subsi3.zero_extend.psi"  "*subsi3.sign_extend.psi"
(define_insn "*sub<HISI:mode>3.zero_extend.<QIPSI:mode>"
  [(set (match_operand:HISI 0 "register_operand"                              "=r")
        (minus:HISI (match_operand:HISI 1 "register_operand"                   "0")
                    (zero_extend:HISI (match_operand:QIPSI 2 "register_operand" "r"))))
   (clobber (reg:CC REG_CC))]
  "reload_completed
   && <HISI:SIZE> > <QIPSI:SIZE>
   && (<HISI:SIZE> > 2)"
{
    int bytes = <HISI:SIZE>;
    int src_bytes = <QIPSI:SIZE>;
    char buf[32];  // Buffer for formatted assembly instructions

    output_asm_insn("sub %A0,%A2", operands);
    
    for (int i = 1; i < src_bytes; i++) {
        sprintf(buf, "subc %%%c0,%%%c2", 'A' + i, 'A' + i);
        output_asm_insn(buf, operands);
    }
    
    for (int i = src_bytes; i < bytes; i++) {
        sprintf(buf, "subc %%%c0,0", 'A' + i);
        output_asm_insn(buf, operands);
    }

    return "";
}
  [(set (attr "length")
        (symbol_ref "<HISI:SIZE>"))])

(define_insn "*sub<HISI:mode>3.sign_extend.<QIPSI:mode>"
  [(set (match_operand:HISI 0 "register_operand"                              "=r")
        (minus:HISI (match_operand:HISI 1 "register_operand"                   "0")
                    (sign_extend:HISI (match_operand:QIPSI 2 "register_operand" "r"))))
   (clobber (match_scratch:QI 3                                              "=&r"))
   (clobber (reg:CC REG_CC))]
  "reload_completed
   && <HISI:SIZE> > <QIPSI:SIZE>"
{
    int bytes = <HISI:SIZE>;
    int src_bytes = <QIPSI:SIZE>;
    char buf[32];  // Buffer for formatted instructions
    sprintf(buf, "mov %%3,%%%c2", 'A' + src_bytes - 1);
    output_asm_insn(buf, operands);
    output_asm_insn("sra %3,7", operands);
    // Initial subtraction of least significant byte
    output_asm_insn("sub %A0,%A2", operands);

    // Subtract middle bytes with carry
    for (int i = 1; i < src_bytes; i++) {
        sprintf(buf, "subc %%%c0,%%%c2", 'A' + i, 'A' + i);
        output_asm_insn(buf, operands);
    }

    // Prepare sign extension


    // Subtract high bytes with sign-extended carry
    for (int i = src_bytes; i < bytes; i++) {
        sprintf(buf, "subc %%%c0,%%3", 'A' + i);
        output_asm_insn(buf, operands);
    }

    return "";
}
  [(set (attr "length")
        (symbol_ref "<HISI:SIZE> + 2"))])


;;----------------psi_linear

(define_insn_and_split "addpsi3"
  [(set (match_operand:PSI 0 "register_operand"         "=r,r ")
        (plus:PSI (match_operand:PSI 1 "register_operand" "%0,0")
                  (match_operand:PSI 2 "nonmemory_operand" "r,i")))
   ]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (plus:PSI (match_dup 1)
                             (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*addpsi3"
  [(set (match_operand:PSI 0 "register_operand"         "=r,r ")
        (plus:PSI (match_operand:PSI 1 "register_operand" "%0,0")
                  (match_operand:PSI 2 "nonmemory_operand" "r,i")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
  add %A0,%A2\;addc %B0,%B2\;addc %C0,%C2
  add %A0,lo8(%2)\;addc %B0,hi8(%2)\;addc %C0,hlo8(%2)"
  [(set_attr "length" "3")
   ])

(define_insn_and_split "subpsi3"
  [(set (match_operand:PSI 0 "register_operand"           "=r")
        (minus:PSI (match_operand:PSI 1 "register_operand" "0")
                   (match_operand:PSI 2 "register_operand" "r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (minus:PSI (match_dup 1)
                              (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*subpsi3"
  [(set (match_operand:PSI 0 "register_operand"           "=r")
        (minus:PSI (match_operand:PSI 1 "register_operand" "0")
                   (match_operand:PSI 2 "register_operand" "r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "sub %A0,%A2\;subc %B0,%B2\;subc %C0,%C2"
  [(set_attr "length" "3")])

