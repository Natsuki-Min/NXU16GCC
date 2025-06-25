
;;<< << << << << << << << << << << << << << << << << << << << << << << << << <<
;; arithmetic shift left

;; "ashlqi3"
;; "ashlqq3"  "ashluqq3"
(define_expand "ashl<mode>3"
  [(set (match_operand:ALL1 0 "register_operand" "")
        (ashift:ALL1 (match_operand:ALL1 1 "register_operand" "")
                     (match_operand:QI 2 "general_operand" "")))])




;; "*ashlqi3"
;; "*ashlqq3"  "*ashluqq3"
(define_insn_and_split "*ashl<mode>3_split"
  [(set (match_operand:ALL1 0 "register_operand"              "=r,r ")
        (ashift:ALL1 (match_operand:ALL1 1 "register_operand"  "0,0")
                     (match_operand:QI 2 "general_operand" "r,i")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashift:ALL1 (match_dup 1)
                                (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*ashl<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"              "=r,r ")
        (ashift:ALL1 (match_operand:ALL1 1 "register_operand"  "0,0")
                     (match_operand:QI 2 "general_operand" "r,i")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
"sll %0,%1"
  [(set_attr "length" "1")])

;; "ashlhi3"
;; "ashlhq3"  "ashluhq3"
;; "ashlha3"  "ashluha3"
(define_insn_and_split "ashl<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"              "=r,r  ,r        ,r  ,r,r")
        (ashift:ALL2 (match_operand:ALL2 1 "register_operand"  "0,0  ,r        ,r  ,0,0")
                     (match_operand:QI 2 "nop_general_operand" "r,LPK,O C7c C15,C2l,n,Qm")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashift:ALL2 (match_dup 1)
                                (match_dup 2)))
              (clobber (reg:CC REG_CC))])]
  ""
  [(set_attr "isa" "*,*,*,3op,*,*")])

;; "*ashlhi3"
;; "*ashlhq3"  "*ashluhq3"
;; "*ashlha3"  "*ashluha3"
(define_insn "*ashl<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"              "=r,r  ,r        ,r  ,r,r")
        (ashift:ALL2 (match_operand:ALL2 1 "register_operand"  "0,0  ,r        ,r  ,0,0")
                     (match_operand:QI 2 "nop_general_operand" "r,LPK,O C7c C15,C2l,n,Qm")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return ashlhi3_out (insn, operands, NULL);
  }
  [(set_attr "isa" "*,*,*,3op,*,*")
   (set_attr "length" "10")
   (set_attr "adjust_len" "ashlhi")])


;; Insns like the following are generated when (implicitly) extending 8-bit shifts
;; like char1 = char2 << char3.  Only the low-byte is needed in that situation.

;; "*ashluqihiqi3"
;; "*ashlsqihiqi3"
(define_insn_and_split "*ashl<extend_su>qihiqi3"
  [(set (match_operand:QI 0 "register_operand"                                     "=r")
        (subreg:QI (ashift:HI (any_extend:HI (match_operand:QI 1 "register_operand" "0"))
                              (match_operand:QI 2 "register_operand"                "r"))
                   0))]
  ""
  "#"
  ""
  [(set (match_dup 0)
        (ashift:QI (match_dup 1)
                   (match_dup 2)))])

;; ??? Combiner does not recognize that it could split the following insn;
;;     presumably because he has no register handy?

;; "*ashluqihiqi3.mem"
;; "*ashlsqihiqi3.mem"
(define_insn_and_split "*ashl<extend_su>qihiqi3.mem"
  [(set (match_operand:QI 0 "memory_operand" "=m")
        (subreg:QI (ashift:HI (any_extend:HI (match_operand:QI 1 "register_operand" "r"))
                              (match_operand:QI 2 "register_operand" "r"))
                   0))]
  "!reload_completed"
  { gcc_unreachable(); }
  "&& 1"
  [(set (match_dup 3)
        (ashift:QI (match_dup 1)
                   (match_dup 2)))
   (set (match_dup 0)
        (match_dup 3))]
  {
    operands[3] = gen_reg_rtx (QImode);
  })

;; Similar.

(define_insn_and_split "*ashlhiqi3"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r")
        (subreg:QI (ashift:HI (match_operand:HI 1 "register_operand" "0")
                              (match_operand:QI 2 "register_operand" "r")) 0))]
  "!reload_completed"
  { gcc_unreachable(); }
  "&& 1"
  [(set (match_dup 4)
        (ashift:QI (match_dup 3)
                   (match_dup 2)))
   (set (match_dup 0)
        (match_dup 4))]
  {
    operands[3] = simplify_gen_subreg (QImode, operands[1], HImode, 0);
    operands[4] = gen_reg_rtx (QImode);
  })

;; High part of 16-bit shift is unused after the instruction:
;; No need to compute it, map to 8-bit shift.

(define_peephole2
  [(parallel [(set (match_operand:HI 0 "register_operand" "")
                   (ashift:HI (match_dup 0)
                              (match_operand:QI 1 "register_operand" "")))
              (clobber (reg:CC REG_CC))])]
  ""
  [(parallel [(set (match_dup 2)
                   (ashift:QI (match_dup 2)
                              (match_dup 1)))
              (clobber (reg:CC REG_CC))])
   (clobber (match_dup 3))]
  {
    operands[3] = simplify_gen_subreg (QImode, operands[0], HImode, 1);

    if (!peep2_reg_dead_p (1, operands[3]))
      FAIL;

    operands[2] = simplify_gen_subreg (QImode, operands[0], HImode, 0);
  })


;; "ashlsi3"
;; "ashlsq3"  "ashlusq3"
;; "ashlsa3"  "ashlusa3"
(define_insn_and_split "ashl<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"                "=r,r  ,r    ,r  ,r,r")
        (ashift:ALL4 (match_operand:ALL4 1 "register_operand"    "0,0  ,r    ,r  ,0,0")
                     (match_operand:QI 2 "nop_general_operand"   "r,LPK,O C4L,C4l,n,Qm")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashift:ALL4 (match_dup 1)
                                (match_dup 2)))
              (clobber (reg:CC REG_CC))])]
  ""
  [(set_attr "isa" "*,*,*,3op,*,*")])

(define_insn "*ashl<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"                "=r,r  ,r    ,r  ,r,r")
        (ashift:ALL4 (match_operand:ALL4 1 "register_operand"    "0,0  ,r    ,r  ,0,0")
                     (match_operand:QI 2 "nop_general_operand"   "r,LPK,O C4L,C4l,n,Qm")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return ashlsi3_out (insn, operands, NULL);
  }
  [(set_attr "isa" "*,*,*,3op,*,*")
   (set_attr "length" "12")
   (set_attr "adjust_len" "ashlsi")])

;; Optimize if a scratch register from LD_REGS happens to be available.

(define_peephole2 ; ashlqi3_l_const4
  [(parallel [(set (match_operand:ALL1 0 "l_register_operand" "")
                   (ashift:ALL1 (match_dup 0)
                                (const_int 4)))
              (clobber (reg:CC REG_CC))])
   (match_scratch:QI 1 "d")]
  ""
  [(parallel [(set (match_dup 2) (rotate:QI (match_dup 2) (const_int 4)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 1) (const_int -16))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 2) (and:QI (match_dup 2) (match_dup 1)))
              (clobber (reg:CC REG_CC))])]
  {
    operands[2] = avr_to_int_mode (operands[0]);
  })

(define_peephole2 ; ashlqi3_l_const5
  [(parallel [(set (match_operand:ALL1 0 "l_register_operand" "")
                   (ashift:ALL1 (match_dup 0)
                                (const_int 5)))
              (clobber (reg:CC REG_CC))])
   (match_scratch:QI 1 "d")]
  ""
  [(parallel [(set (match_dup 2) (rotate:QI (match_dup 2) (const_int 4)))
                   (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 2) (ashift:QI (match_dup 2) (const_int 1)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 1) (const_int -32))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 2) (and:QI (match_dup 2) (match_dup 1)))
              (clobber (reg:CC REG_CC))])]
  {
    operands[2] = avr_to_int_mode (operands[0]);
  })

(define_peephole2 ; ashlqi3_l_const6
  [(parallel [(set (match_operand:ALL1 0 "l_register_operand" "")
                   (ashift:ALL1 (match_dup 0)
                                (const_int 6)))
              (clobber (reg:CC REG_CC))])
   (match_scratch:QI 1 "d")]
  ""
  [(parallel [(set (match_dup 2) (rotate:QI (match_dup 2) (const_int 4)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 2) (ashift:QI (match_dup 2) (const_int 2)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 1) (const_int -64))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 2) (and:QI (match_dup 2) (match_dup 1)))
              (clobber (reg:CC REG_CC))])]
  {
    operands[2] = avr_to_int_mode (operands[0]);
  })

;; Endow 2-byte shift with a scratch if available.
(define_peephole2 ; *ashlhi3_const  *ashrhi3_const  *lshrhi3_const
  [(match_scratch:QI 3 "d")
   (parallel [(set (match_operand:ALL2 0 "register_operand")
                   (any_shift:ALL2 (match_operand:ALL2 1 "register_operand")
                                   (match_operand:QI 2 "const_int_operand")))
              (clobber (reg:CC REG_CC))])
   ;; Don't allow $3 to overlap with $0.
   (match_dup 3)]
  ""
  [(parallel [(set (match_dup 0)
                   (any_shift:ALL2 (match_dup 1)
                                   (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])

;; "*ashlhi3_const"
;; "*ashlhq3_const"  "*ashluhq3_const"
;; "*ashlha3_const"  "*ashluha3_const"
(define_insn "*ashl<mode>3_const"
  [(set (match_operand:ALL2 0 "register_operand"              "=r  ,r        ,r      ,r  ,r")
        (ashift:ALL2 (match_operand:ALL2 1 "register_operand"  "0  ,r        ,r      ,r  ,0")
                     (match_operand:QI 2 "const_int_operand"   "LPK,O C7c C15,C05 C06,C2l,n")))
   (clobber (match_scratch:QI 3                               "=X  ,X        ,&d     ,&d ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return ashlhi3_out (insn, operands, NULL);
  }
  [(set_attr "isa" "*,*,mul,3op,*")
   (set_attr "length" "10")
   (set_attr "adjust_len" "ashlhi")])

(define_code_attr constr_split_suffix
  [(ashift "l")
   (ashiftrt "a")
   (lshiftrt "r")])

;; Split shift into a byte shift and a residual bit shift (without scratch)
(define_split
  [(parallel [(set (match_operand:ALL234 0 "register_operand")
                   (any_shift:ALL234 (match_operand:ALL234 1 "register_operand")
                                     (match_operand:QI 2 "const_int_operand")))
              (clobber (reg:CC REG_CC))])]
  "avropt_split_bit_shift
   && n_avr_fuse_add_executed >= 1
   && satisfies_constraint_C<SIZE><constr_split_suffix> (operands[2])"
  [(parallel [(set (match_dup 0)
                   (any_shift:ALL234 (match_dup 1)
                                     (match_dup 3)))
              (clobber (scratch:QI))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 0)
                   (any_shift:ALL234 (match_dup 0)
                                     (match_dup 4)))
              (clobber (reg:CC REG_CC))])]
  {
    int offset = INTVAL (operands[2]);
    if (avr_split_shift (operands, NULL_RTX, <CODE>))
      DONE;
    else if (offset <= 8)
      FAIL;
    operands[3] = GEN_INT (offset & ~7);
    operands[4] = GEN_INT (offset & 7);
  })

;; Split shift into a byte shift and a residual bit shift (with scratch)
(define_split
  [(parallel [(set (match_operand:ALL234 0 "register_operand")
                   (any_shift:ALL234 (match_operand:ALL234 1 "register_operand")
                                     (match_operand:QI 2 "const_int_operand")))
              (clobber (match_operand:QI 3 "scratch_or_dreg_operand"))
              (clobber (reg:CC REG_CC))])]
  "avropt_split_bit_shift
   && n_avr_fuse_add_executed >= 1
   && satisfies_constraint_C<SIZE><constr_split_suffix> (operands[2])"
  [(parallel [(set (match_dup 0)
                   (any_shift:ALL234 (match_dup 1)
                                     (match_dup 4)))
              (clobber (scratch:QI))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 0)
                   (any_shift:ALL234 (match_dup 0)
                                     (match_dup 5)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])]
  {
    int offset = INTVAL (operands[2]);
    if (avr_split_shift (operands, operands[3], <CODE>))
      DONE;
    else if (offset <= 8)
      FAIL;
    operands[4] = GEN_INT (offset & ~7);
    operands[5] = GEN_INT (offset & 7);
  })


;; Endow 4-byte shift with a scratch if available.
(define_peephole2 ; *ashrsi3_const  *lshrsi3_const  *ashlsi3_const
  [(match_scratch:QI 3 "d")
   (parallel [(set (match_operand:ALL4 0 "register_operand")
                   (any_shift:ALL4 (match_operand:ALL4 1 "register_operand")
                                   (match_operand:QI 2 "const_int_operand")))
              (clobber (reg:CC REG_CC))])
   ;; $3 must not overlap with the output of the insn above.
   (match_dup 3)]
  ""
  [(parallel [(set (match_dup 0)
                   (any_shift:ALL4 (match_dup 1)
                                   (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])])


;; "*ashlsi3_const"
;; "*ashlsq3_const"  "*ashlusq3_const"
;; "*ashlsa3_const"  "*ashlusa3_const"
(define_insn "*ashl<mode>3_const"
  [(set (match_operand:ALL4 0 "register_operand"             "=r ,r    ,r  ,r")
        (ashift:ALL4 (match_operand:ALL4 1 "register_operand" "0 ,r    ,r  ,0")
                     (match_operand:QI 2 "const_int_operand"  "LP,O C4L,C4l,n")))
   (clobber (match_operand:QI 3 "scratch_or_dreg_operand"    "=X ,X    ,&d ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return ashlsi3_out (insn, operands, NULL);
  }
  [(set_attr "isa" "*,*,3op,*")
   (set_attr "length" "10")
   (set_attr "adjust_len" "ashlsi")])

(define_expand "ashlpsi3"
  [(parallel [(set (match_operand:PSI 0 "register_operand"             "")
                   (ashift:PSI (match_operand:PSI 1 "register_operand" "")
                               (match_operand:QI 2 "nonmemory_operand" "")))
              (clobber (scratch:QI))])]
  ""
  {
    if (AVR_HAVE_MUL
        && CONST_INT_P (operands[2]))
      {
        if (IN_RANGE (INTVAL (operands[2]), 3, 6))
          {
            rtx xoffset = force_reg (QImode, gen_int_mode (1 << INTVAL (operands[2]), QImode));
            emit_insn (gen_mulsqipsi3 (operands[0], xoffset, operands[1]));
            DONE;
          }
        else if (optimize_insn_for_speed_p ()
                 && INTVAL (operands[2]) != 16
                 && IN_RANGE (INTVAL (operands[2]), 9, 22))
          {
            rtx xoffset = force_reg (PSImode, gen_int_mode (1 << INTVAL (operands[2]), PSImode));
            emit_insn (gen_mulpsi3 (operands[0], operands[1], xoffset));
            DONE;
          }
      }
  })

(define_insn_and_split "*ashlpsi3_split"
  [(set (match_operand:PSI 0 "register_operand"             "=r,r,r        ,r  ,r")
        (ashift:PSI (match_operand:PSI 1 "register_operand"  "0,0,r        ,r  ,0")
                    (match_operand:QI 2 "nonmemory_operand"  "r,P,O C15 C23,C3l,n")))
   (clobber (match_scratch:QI 3                             "=X,X,X        ,&d ,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashift:PSI (match_dup 1)
                               (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])]
  ""
  [(set_attr "isa" "*,*,*,3op,*")])

(define_insn "*ashlpsi3"
  [(set (match_operand:PSI 0 "register_operand"             "=r,r,r        ,r  ,r")
        (ashift:PSI (match_operand:PSI 1 "register_operand"  "0,0,r        ,r  ,0")
                    (match_operand:QI 2 "nonmemory_operand"  "r,P,O C15 C23,C3l,n")))
   ; "X&d" since the insn may be a split of a 4-byte shift without scratch.
   (clobber (match_scratch:QI 3                             "=X,X,X        ,X&d,X&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_ashlpsi3 (insn, operands, NULL);
  }
  [(set_attr "isa" "*,*,*,3op,*")
   (set_attr "adjust_len" "ashlpsi")])

;; Seen in PSI loads from __flashx tables.
(define_insn_and_split "*ashlqi.1.zextpsi_split"
  [(set (match_operand:PSI 0 "register_operand"  "=r")
        (zero_extend:PSI
         (ashift:HI (zero_extend:HI (match_operand:QI 1 "register_operand" "0"))
                    (const_int 1))))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 2)
                   (const_int 0))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 3)
                   (const_int 0))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 4)
                   (ashift:HI (match_dup 4)
                              (const_int 1)))
              (clobber (reg:CC REG_CC))])]
  {
    operands[2] = avr_byte (operands[0], 2);
    operands[3] = avr_byte (operands[0], 1);
    operands[4] = avr_word (operands[0], 0);
  })

;; >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >>
;; arithmetic shift right

;; "ashrqi3"
;; "ashrqq3"  "ashruqq3"
(define_insn_and_split "ashr<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"                  "=r,r              ,r      ,r")
        (ashiftrt:ALL1 (match_operand:ALL1 1 "register_operand"    "0,0              ,r      ,0")
                       (match_operand:QI 2 "nop_general_operand"   "r,LPK C03 C04 C05,C06 C07,Qm")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashiftrt:ALL1 (match_dup 1)
                                  (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*ashr<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"                  "=r,r              ,r      ,r")
        (ashiftrt:ALL1 (match_operand:ALL1 1 "register_operand"    "0,0              ,r      ,0")
                       (match_operand:QI 2 "nop_general_operand"   "r,LPK C03 C04 C05,C06 C07,Qm")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return ashrqi3_out (insn, operands, NULL);
  }
  [(set_attr "length" "9")
   (set_attr "adjust_len" "ashrqi")])

;; "ashrhi3"
;; "ashrhq3"  "ashruhq3"
;; "ashrha3"  "ashruha3"
(define_insn_and_split "ashr<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"                "=r,r  ,r            ,r  ,r,r")
        (ashiftrt:ALL2 (match_operand:ALL2 1 "register_operand"  "0,0  ,r            ,r  ,0,0")
                       (match_operand:QI 2 "nop_general_operand" "r,LPK,O C07 C14 C15,C2a,n,Qm")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashiftrt:ALL2 (match_dup 1)
                                  (match_dup 2)))
              (clobber (reg:CC REG_CC))])]
   ""
   [(set_attr "isa" "*,*,*,3op,*,*")])

;; "*ashrhi3"
;; "*ashrhq3"  "*ashruhq3"
;; "*ashrha3"  "*ashruha3"
(define_insn "*ashr<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"                "=r,r  ,r            ,r  ,r,r")
        (ashiftrt:ALL2 (match_operand:ALL2 1 "register_operand"  "0,0  ,r            ,r  ,0,0")
                       (match_operand:QI 2 "nop_general_operand" "r,LPK,O C07 C14 C15,C2a,n,Qm")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return ashrhi3_out (insn, operands, NULL);
  }
  [(set_attr "isa" "*,*,*,3op,*,*")
   (set_attr "length" "10")
   (set_attr "adjust_len" "ashrhi")])

(define_insn_and_split "ashrpsi3"
  [(set (match_operand:PSI 0 "register_operand"               "=r,r ,r            ,r  ,r")
        (ashiftrt:PSI (match_operand:PSI 1 "register_operand"  "0,0 ,r            ,r  ,0")
                      (match_operand:QI 2 "nonmemory_operand"  "r,PK,O C15 C22 C23,C3a,n")))
   (clobber (match_scratch:QI 3                               "=X,X ,X            ,&d ,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashiftrt:PSI (match_dup 1)
                                 (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])]
  ""
  [(set_attr "isa" "*,*,*,3op,*")])

(define_insn "*ashrpsi3"
  [(set (match_operand:PSI 0 "register_operand"                 "=r,r ,r            ,r  ,r")
        (ashiftrt:PSI (match_operand:PSI 1 "register_operand"    "0,0 ,r            ,r  ,0")
                      (match_operand:QI 2 "nonmemory_operand"    "r,PK,O C15 C22 C23,C3a,n")))
   ; "X&d" since the insn may be a split of a 4-byte shift without scratch.
   (clobber (match_scratch:QI 3                                 "=X,X ,X            ,X&d,X&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_ashrpsi3 (insn, operands, NULL);
  }
  [(set_attr "isa" "*,*,*,3op,*")
   (set_attr "adjust_len" "ashrpsi")])

;; "ashrsi3"
;; "ashrsq3"  "ashrusq3"
;; "ashrsa3"  "ashrusa3"
(define_insn_and_split "ashr<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"                  "=r,r  ,r            ,r  ,r,r")
        (ashiftrt:ALL4 (match_operand:ALL4 1 "register_operand"    "0,0  ,r            ,r  ,0,0")
                       (match_operand:QI 2 "nop_general_operand"   "r,LPK,O C15 C30 C31,C4a,n,Qm")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (ashiftrt:ALL4 (match_dup 1)
                                  (match_dup 2)))
              (clobber (reg:CC REG_CC))])]
  ""
  [(set_attr "isa" "*,*,*,3op,*,*")])

(define_insn "*ashr<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"                  "=r,r  ,r            ,r  ,r,r")
        (ashiftrt:ALL4 (match_operand:ALL4 1 "register_operand"    "0,0  ,r            ,r  ,0,0")
                       (match_operand:QI 2 "nop_general_operand"   "r,LPK,O C15 C30 C31,C4a,n,Qm")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return ashrsi3_out (insn, operands, NULL);
  }
  [(set_attr "isa" "*,*,*,3op,*,*")
   (set_attr "length" "12")
   (set_attr "adjust_len" "ashrsi")])


;; "*ashrhi3_const"
;; "*ashrhq3_const"  "*ashruhq3_const"
;; "*ashrha3_const"  "*ashruha3_const"
(define_insn "*ashr<mode>3_const"
  [(set (match_operand:ALL2 0 "register_operand"                "=r  ,r            ,r  ,r")
        (ashiftrt:ALL2 (match_operand:ALL2 1 "register_operand"  "0  ,r            ,r  ,0")
                       (match_operand:QI 2 "const_int_operand"   "LPK,O C07 C14 C15,C2a,n")))
   (clobber (match_scratch:QI 3                                 "=X  ,X            ,&d ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return ashrhi3_out (insn, operands, NULL);
  }
  [(set_attr "isa" "*,*,3op,*")
   (set_attr "length" "10")
   (set_attr "adjust_len" "ashrhi")])


;; "*ashrsi3_const"
;; "*ashrsq3_const"  "*ashrusq3_const"
;; "*ashrsa3_const"  "*ashrusa3_const"
(define_insn "*ashr<mode>3_const"
  [(set (match_operand:ALL4 0 "register_operand"                "=r ,r            ,r  ,r")
        (ashiftrt:ALL4 (match_operand:ALL4 1 "register_operand"  "0 ,r            ,r  ,0")
                       (match_operand:QI 2 "const_int_operand"   "LP,O C15 C30 C31,C4a,n")))
   (clobber (match_operand:QI 3 "scratch_or_dreg_operand"       "=X ,X            ,&d ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return ashrsi3_out (insn, operands, NULL);
  }
  [(set_attr "isa" "*,*,3op,*")
   (set_attr "length" "10")
   (set_attr "adjust_len" "ashrsi")])

;; >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >> >>
;; logical shift right

;; "lshrqi3"
;; "lshrqq3" "lshruqq3"
(define_expand "lshr<mode>3"
  [(set (match_operand:ALL1 0 "register_operand" "")
        (lshiftrt:ALL1 (match_operand:ALL1 1 "register_operand" "")
                       (match_operand:QI 2 "nop_general_operand" "")))])

(define_split ; lshrqi3_const4
  [(set (match_operand:ALL1 0 "d_register_operand" "")
        (lshiftrt:ALL1 (match_dup 0)
                       (const_int 4)))]
  ""
  [(set (match_dup 1)
        (rotate:QI (match_dup 1)
                   (const_int 4)))
   (set (match_dup 1)
        (and:QI (match_dup 1)
                (const_int 15)))]
  {
    operands[1] = avr_to_int_mode (operands[0]);
  })

(define_split ; lshrqi3_const5
  [(set (match_operand:ALL1 0 "d_register_operand" "")
        (lshiftrt:ALL1 (match_dup 0)
                       (const_int 5)))]
  ""
  [(set (match_dup 1) (rotate:QI (match_dup 1) (const_int 4)))
   (set (match_dup 1) (lshiftrt:QI (match_dup 1) (const_int 1)))
   (set (match_dup 1) (and:QI (match_dup 1) (const_int 7)))]
  {
    operands[1] = avr_to_int_mode (operands[0]);
  })

(define_split ; lshrqi3_const6
  [(set (match_operand:QI 0 "d_register_operand" "")
        (lshiftrt:QI (match_dup 0)
                     (const_int 6)))]
  ""
  [(set (match_dup 1) (rotate:QI (match_dup 1) (const_int 4)))
   (set (match_dup 1) (lshiftrt:QI (match_dup 1) (const_int 2)))
   (set (match_dup 1) (and:QI (match_dup 1) (const_int 3)))]
  {
    operands[1] = avr_to_int_mode (operands[0]);
  })

;; "*lshrqi3"
;; "*lshrqq3"
;; "*lshruqq3"
(define_insn_and_split "*lshr<mode>3_split"
  [(set (match_operand:ALL1 0 "register_operand"                  "=r,r  ,r      ,r,r")
        (lshiftrt:ALL1 (match_operand:ALL1 1 "register_operand"    "0,0  ,r      ,0,0")
                       (match_operand:QI 2 "nop_general_operand"   "r,LPK,C07 C06,n,Qm")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (lshiftrt:ALL1 (match_dup 1)
                                  (match_dup 2)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*lshr<mode>3"
  [(set (match_operand:ALL1 0 "register_operand"                  "=r,r  ,r      ,r,r")
        (lshiftrt:ALL1 (match_operand:ALL1 1 "register_operand"    "0,0  ,r      ,0,0")
                       (match_operand:QI 2 "nop_general_operand"   "r,LPK,C07 C06,n,Qm")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return lshrqi3_out (insn, operands, NULL);
  }
  [(set_attr "adjust_len" "lshrqi")])

;; "lshrhi3"
;; "lshrhq3"  "lshruhq3"
;; "lshrha3"  "lshruha3"
(define_insn_and_split "lshr<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"                "=r,r  ,r        ,r  ,r,r")
        (lshiftrt:ALL2 (match_operand:ALL2 1 "register_operand"  "0,0  ,r        ,r  ,0,0")
                       (match_operand:QI 2 "nop_general_operand" "r,LPK,O C7c C15,C2r,n,Qm")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (lshiftrt:ALL2 (match_dup 1)
                                  (match_dup 2)))
              (clobber (reg:CC REG_CC))])]
  ""
  [(set_attr "isa" "*,*,*,3op,*,*")])

(define_insn "*lshr<mode>3"
  [(set (match_operand:ALL2 0 "register_operand"                "=r,r  ,r        ,r  ,r,r")
        (lshiftrt:ALL2 (match_operand:ALL2 1 "register_operand"  "0,0  ,r        ,r  ,0,0")
                       (match_operand:QI 2 "nop_general_operand" "r,LPK,O C7c C15,C2r,n,Qm")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return lshrhi3_out (insn, operands, NULL);
  }
  [(set_attr "isa" "*,*,*,3op,*,*")
   (set_attr "adjust_len" "lshrhi")])

(define_insn_and_split "lshrpsi3"
  [(set (match_operand:PSI 0 "register_operand"                 "=r,r ,r        ,r  ,r")
        (lshiftrt:PSI (match_operand:PSI 1 "register_operand"    "0,0 ,r        ,r  ,0")
                      (match_operand:QI 2 "nonmemory_operand"    "r,PK,O C15 C23,C3r,n")))
   (clobber (match_scratch:QI 3                                 "=X,X ,X        ,&d ,&d"))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (lshiftrt:PSI (match_dup 1)
                                 (match_dup 2)))
              (clobber (match_dup 3))
              (clobber (reg:CC REG_CC))])]
  ""
  [(set_attr "isa" "*,*,*,3op,*")])

(define_insn "*lshrpsi3"
  [(set (match_operand:PSI 0 "register_operand"                 "=r,r ,r        ,r  ,r")
        (lshiftrt:PSI (match_operand:PSI 1 "register_operand"    "0,0 ,r        ,r  ,0")
                      (match_operand:QI 2 "nonmemory_operand"    "r,PK,O C15 C23,C3r,n")))
   ; "X&d" since the insn may be a split of a 4-byte shift without scratch.
   (clobber (match_scratch:QI 3                                 "=X,X ,X        ,X&d,X&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return avr_out_lshrpsi3 (insn, operands, NULL);
  }
  [(set_attr "isa" "*,*,*,3op,*")
   (set_attr "adjust_len" "lshrpsi")])

;; "lshrsi3"
;; "lshrsq3"  "lshrusq3"
;; "lshrsa3"  "lshrusa3"
(define_insn_and_split "lshr<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"                  "=r,r  ,r    ,r  ,r,r")
        (lshiftrt:ALL4 (match_operand:ALL4 1 "register_operand"    "0,0  ,r    ,r  ,0,0")
                       (match_operand:QI 2 "nop_general_operand"   "r,LPK,O C4R,C4r,n,Qm")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (lshiftrt:ALL4 (match_dup 1)
                                  (match_dup 2)))
              (clobber (reg:CC REG_CC))])]
  ""
  [(set_attr "isa" "*,*,*,3op,*,*")])

(define_insn "*lshr<mode>3"
  [(set (match_operand:ALL4 0 "register_operand"                  "=r,r  ,r    ,r  ,r,r")
        (lshiftrt:ALL4 (match_operand:ALL4 1 "register_operand"    "0,0  ,r    ,r  ,0,0")
                       (match_operand:QI 2 "nop_general_operand"   "r,LPK,O C4R,C4r,n,Qm")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return lshrsi3_out (insn, operands, NULL);
  }
  [(set_attr "isa" "*,*,*,3op,*,*")
   (set_attr "adjust_len" "lshrsi")])

;; Optimize if a scratch register from LD_REGS happens to be available.

(define_peephole2 ; lshrqi3_l_const4
  [(parallel [(set (match_operand:ALL1 0 "l_register_operand" "")
                   (lshiftrt:ALL1 (match_dup 0)
                                  (const_int 4)))
              (clobber (reg:CC REG_CC))])
   (match_scratch:QI 1 "d")]
  ""
  [(parallel [(set (match_dup 2) (rotate:QI (match_dup 2) (const_int 4)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 1) (const_int 15))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 2) (and:QI (match_dup 2) (match_dup 1)))
              (clobber (reg:CC REG_CC))])]
  {
    operands[2] = avr_to_int_mode (operands[0]);
  })

(define_peephole2 ; lshrqi3_l_const5
  [(parallel [(set (match_operand:ALL1 0 "l_register_operand" "")
                   (lshiftrt:ALL1 (match_dup 0)
                                  (const_int 5)))
              (clobber (reg:CC REG_CC))])
   (match_scratch:QI 1 "d")]
  ""
  [(parallel [(set (match_dup 2) (rotate:QI (match_dup 2) (const_int 4)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 2) (lshiftrt:QI (match_dup 2) (const_int 1)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 1) (const_int 7))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 2) (and:QI (match_dup 2) (match_dup 1)))
              (clobber (reg:CC REG_CC))])]
  {
    operands[2] = avr_to_int_mode (operands[0]);
  })

(define_peephole2 ; lshrqi3_l_const6
  [(parallel [(set (match_operand:ALL1 0 "l_register_operand" "")
                   (lshiftrt:ALL1 (match_dup 0)
                                  (const_int 6)))
              (clobber (reg:CC REG_CC))])
   (match_scratch:QI 1 "d")]
  ""
  [(parallel [(set (match_dup 2) (rotate:QI (match_dup 2) (const_int 4)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 2) (lshiftrt:QI (match_dup 2) (const_int 2)))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 1) (const_int 3))
              (clobber (reg:CC REG_CC))])
   (parallel [(set (match_dup 2) (and:QI (match_dup 2) (match_dup 1)))
              (clobber (reg:CC REG_CC))])]
  {
    operands[2] = avr_to_int_mode (operands[0]);
  })


;; "*lshrhi3_const"
;; "*lshrhq3_const"  "*lshruhq3_const"
;; "*lshrha3_const"  "*lshruha3_const"
(define_insn "*lshr<mode>3_const"
  [(set (match_operand:ALL2 0 "register_operand"                "=r  ,r        ,r  ,r")
        (lshiftrt:ALL2 (match_operand:ALL2 1 "register_operand"  "0  ,r        ,r  ,0")
                       (match_operand:QI 2 "const_int_operand"   "LPK,O C7c C15,C2r,n")))
   (clobber (match_scratch:QI 3                                 "=X  ,X        ,&d ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return lshrhi3_out (insn, operands, NULL);
  }
  [(set_attr "isa" "*,*,3op,*")
   (set_attr "adjust_len" "lshrhi")])


;; "*lshrsi3_const"
;; "*lshrsq3_const"  "*lshrusq3_const"
;; "*lshrsa3_const"  "*lshrusa3_const"
(define_insn "*lshr<mode>3_const"
  [(set (match_operand:ALL4 0 "register_operand"               "=r ,r    ,r  ,r")
        (lshiftrt:ALL4 (match_operand:ALL4 1 "register_operand" "0 ,r    ,r  ,0")
                       (match_operand:QI 2 "const_int_operand"  "LP,O C4R,C4r,n")))
   (clobber (match_operand:QI 3 "scratch_or_dreg_operand"      "=X ,X    ,&d ,&d"))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  {
    return lshrsi3_out (insn, operands, NULL);
  }
  [(set_attr "isa" "*,*,3op,*")
   (set_attr "adjust_len" "lshrsi")])

;; abs(x) abs(x) abs(x) abs(x) abs(x) abs(x) abs(x) abs(x) abs(x) abs(x) abs(x)
;; abs

(define_insn_and_split "absqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (abs:QI (match_operand:QI 1 "register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (abs:QI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*absqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (abs:QI (match_operand:QI 1 "register_operand" "0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "sbrc %0,7
	neg %0"
  [(set_attr "length" "2")])


(define_insn_and_split "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=d,r")
        (abs:SF (match_operand:SF 1 "register_operand" "0,0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (abs:SF (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*abssf2"
  [(set (match_operand:SF 0 "register_operand" "=d,r")
        (abs:SF (match_operand:SF 1 "register_operand" "0,0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
	andi %D0,0x7f
	clt\;bld %D0,7"
  [(set_attr "length" "1,2")])
