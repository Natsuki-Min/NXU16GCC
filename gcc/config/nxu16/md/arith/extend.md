; xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x
;; sign extend

;; We keep combiner from inserting hard registers into the input of sign- and
;; zero-extends.  A hard register in the input operand is not wanted because
;; 32-bit multiply patterns clobber some hard registers and extends with a
;; hard register that overlaps these clobbers won't be combined to a widening
;; multiplication.  There is no need for combine to propagate hard registers,
;; register allocation can do it just as well.

;;todo:whether the allocation is really such newbee?? 

(define_insn_and_split "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
        (sign_extend:HI (match_operand:QI 1 "combine_pseudo_register_operand" "0,*r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (sign_extend:HI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
        (sign_extend:HI (match_operand:QI 1 "combine_pseudo_register_operand" "0,*r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
  extbw e%A0
  mov %A0,%A1\;extbw %0"
  [(set_attr "length" "1,2")])

(define_insn_and_split "extendqipsi2"
  [(set (match_operand:PSI 0 "register_operand" "=r,r")
        (sign_extend:PSI (match_operand:QI 1 "combine_pseudo_register_operand" "0,*r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (sign_extend:PSI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*extendqipsi2"
  [(set (match_operand:PSI 0 "register_operand" "=r,r")
        (sign_extend:PSI (match_operand:QI 1 "combine_pseudo_register_operand" "0,*r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
  "@
  extbw e%A0\;mov %C0,%B0
  mov %A0,%A1\;extbw e%A0\;mov %C0,%B0"
  [(set_attr "length" "2,3")])

(define_insn_and_split "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (sign_extend:SI (match_operand:QI 1 "combine_pseudo_register_operand" "0,*r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (sign_extend:SI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (sign_extend:SI (match_operand:QI 1 "combine_pseudo_register_operand" "0,*r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
"@
  extbw e%A0\;mov %C0,%B0\;mov %D0,%B0
  mov %A0,%A1\;extbw e%A0\;mov %C0,%B0\;mov %D0,%B0"
  [(set_attr "length" "3,4")])

(define_insn_and_split "extendhipsi2"
  [(set (match_operand:PSI 0 "register_operand"                               "=r,r")
        (sign_extend:PSI (match_operand:HI 1 "combine_pseudo_register_operand" "0,*r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (sign_extend:PSI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*extendhipsi2"
  [(set (match_operand:PSI 0 "register_operand"                               "=r,r")
        (sign_extend:PSI (match_operand:HI 1 "combine_pseudo_register_operand" "0,*r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
"@
  mov %C0,%B0\;sra %C0,7
  mov e%A0,%1\;mov %C0,%B0\;sra %C0,7"
  [(set_attr "length" "2,3")])

(define_insn_and_split "extendhisi2"
  [(set (match_operand:SI 0 "register_operand"                               "=r,r")
        (sign_extend:SI (match_operand:HI 1 "combine_pseudo_register_operand" "0,*r")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (sign_extend:SI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*extendhisi2"
  [(set (match_operand:SI 0 "register_operand"                               "=r,r")
        (sign_extend:SI (match_operand:HI 1 "combine_pseudo_register_operand" "0,*r")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
"@
  mov %C0,%B0\;sra %C0,7\;mov %D0,%C0
  mov e%A0,%1\;mov %C0,%B0\;sra %C0,7\;mov %D0,%C0"
  [(set_attr "length" "3,4")])

(define_insn_and_split "extendpsisi2"
  [(set (match_operand:SI 0 "register_operand"                                "=r")
        (sign_extend:SI (match_operand:PSI 1 "combine_pseudo_register_operand" "0")))]
  ""
  "#"
  "&& reload_completed"
  [(parallel [(set (match_dup 0)
                   (sign_extend:SI (match_dup 1)))
              (clobber (reg:CC REG_CC))])])

(define_insn "*extendpsisi2"
  [(set (match_operand:SI 0 "register_operand"                                "=r")
        (sign_extend:SI (match_operand:PSI 1 "combine_pseudo_register_operand" "0")))
   (clobber (reg:CC REG_CC))]
  "reload_completed"
"mov %D0,%C0\;sra %D0,7"
  [(set_attr "length" "2")])

;; xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x xx<---x
;; zero extend

(define_insn_and_split "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (zero_extend:HI (match_operand:QI 1 "combine_pseudo_register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 3) (const_int 0))]
  {
    unsigned int low_off = subreg_lowpart_offset (QImode, HImode);
    unsigned int high_off = subreg_highpart_offset (QImode, HImode);

    operands[2] = simplify_gen_subreg (QImode, operands[0], HImode, low_off);
    operands[3] = simplify_gen_subreg (QImode, operands[0], HImode, high_off);
  })

  
(define_split
  [(set (match_operand:HI 0 "register_operand" "")
        (zero_extend:HI (match_operand:QI 1 "register_operand" "")))]
  "reload_completed"
  [(set (strict_low_part (match_dup 2)) (match_dup 1))
   (set (match_dup 3) (const_int 0))]
  {
    operands[2] = gen_lowpart (QImode, operands[0]);
    operands[3] = gen_highpart (QImode, operands[0]);
  })

(define_insn_and_split "zero_extendqipsi2"
  [(set (match_operand:PSI 0 "register_operand" "=r")
        (zero_extend:PSI (match_operand:QI 1 "combine_pseudo_register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 3) (const_int 0))
   (set (match_dup 4) (const_int 0))]
  {
    operands[2] = simplify_gen_subreg (QImode, operands[0], PSImode, 0);
    operands[3] = simplify_gen_subreg (QImode, operands[0], PSImode, 1);
    operands[4] = simplify_gen_subreg (QImode, operands[0], PSImode, 2);
  })

(define_insn_and_split "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI (match_operand:QI 1 "combine_pseudo_register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (zero_extend:HI (match_dup 1)))
   (set (match_dup 3) (const_int 0))]
  {
    unsigned int low_off = subreg_lowpart_offset (HImode, SImode);
    unsigned int high_off = subreg_highpart_offset (HImode, SImode);

    operands[2] = simplify_gen_subreg (HImode, operands[0], SImode, low_off);
    operands[3] = simplify_gen_subreg (HImode, operands[0], SImode, high_off);
  })

(define_insn_and_split "zero_extendhipsi2"
  [(set (match_operand:PSI 0 "register_operand"                               "=r")
        (zero_extend:PSI (match_operand:HI 1 "combine_pseudo_register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 3) (const_int 0))]
  {
    operands[2] = simplify_gen_subreg (HImode, operands[0], PSImode, 0);
    operands[3] = simplify_gen_subreg (QImode, operands[0], PSImode, 2);
  })

(define_insn_and_split "n_extendhipsi2"
  [(set (match_operand:PSI 0 "register_operand"            "=r")
        (lo_sum:PSI (match_operand:QI 1 "const_int_operand" "n")
                    (match_operand:HI 2 "register_operand"  "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 3) (match_dup 2))
   (set (match_dup 4) (match_dup 1))]
  {
    operands[3] = simplify_gen_subreg (HImode, operands[0], PSImode, 0);
    operands[4] = simplify_gen_subreg (QImode, operands[0], PSImode, 2);
  })

(define_insn_and_split "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand"                               "=r")
        (zero_extend:SI (match_operand:HI 1 "combine_pseudo_register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 3) (const_int 0))]
  {
    unsigned int low_off = subreg_lowpart_offset (HImode, SImode);
    unsigned int high_off = subreg_highpart_offset (HImode, SImode);

    operands[2] = simplify_gen_subreg (HImode, operands[0], SImode, low_off);
    operands[3] = simplify_gen_subreg (HImode, operands[0], SImode, high_off);
  })

(define_insn_and_split "zero_extendpsisi2"
  [(set (match_operand:SI 0 "register_operand"                                "=r")
        (zero_extend:SI (match_operand:PSI 1 "combine_pseudo_register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 3) (const_int 0))]
  {
    operands[2] = simplify_gen_subreg (PSImode, operands[0], SImode, 0);
    operands[3] = simplify_gen_subreg (QImode, operands[0], SImode, 3);
  })

(define_insn_and_split "zero_extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (zero_extend:DI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (zero_extend:SI (match_dup 1)))
   (set (match_dup 3) (const_int 0))]
  {
    unsigned int low_off = subreg_lowpart_offset (SImode, DImode);
    unsigned int high_off = subreg_highpart_offset (SImode, DImode);

    operands[2] = simplify_gen_subreg (SImode, operands[0], DImode, low_off);
    operands[3] = simplify_gen_subreg (SImode, operands[0], DImode, high_off);
  })

(define_insn_and_split "zero_extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (zero_extend:DI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (zero_extend:SI (match_dup 1)))
   (set (match_dup 3) (const_int 0))]
  {
    unsigned int low_off = subreg_lowpart_offset (SImode, DImode);
    unsigned int high_off = subreg_highpart_offset (SImode, DImode);

    operands[2] = simplify_gen_subreg (SImode, operands[0], DImode, low_off);
    operands[3] = simplify_gen_subreg (SImode, operands[0], DImode, high_off);
  })

(define_insn_and_split "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (zero_extend:DI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "#"
  "reload_completed"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 3) (const_int 0))]
  {
    unsigned int low_off = subreg_lowpart_offset (SImode, DImode);
    unsigned int high_off = subreg_highpart_offset (SImode, DImode);

    operands[2] = simplify_gen_subreg (SImode, operands[0], DImode, low_off);
    operands[3] = simplify_gen_subreg (SImode, operands[0], DImode, high_off);
  })
