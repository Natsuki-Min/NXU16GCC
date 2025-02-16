;;- Machine description for ARM for GNU compiler
;;  Copyright 1991, 1993, 1994, 1995, 1996, 1996, 1997, 1998, 1999, 2000, 2001
;;  Free Software Foundation, Inc.
;;  Contributed by Pieter `Tiggr' Schoenmakers (rcpieter@win.tue.nl)
;;  and Martin Simmons (@harleqn.co.uk).
;;  More major hacks by Richard Earnshaw (rearnsha@arm.com).

;; This file is part of GNU CC.

;; GNU CC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU CC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU CC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

;; There are patterns in this file to support XFmode arithmetic.
;; Unfortunately RISC iX doesn't work well with these so they are disabled.
;; (See arm.h)

;;---------------------------------------------------------------------------
;; Constants

;; Register numbers
(define_constants
  [(IP_REGNUM	    12)		; Scratch register
   (SP_REGNUM	    13)		; Stack pointer
   (LR_REGNUM       14)		; Return address register
   (PC_REGNUM	    15)		; Program counter
   (CC_REGNUM       24)		; Condition code pseudo register
   (LAST_ARM_REGNUM 15)
  ]
)



;; UNSPEC_VOLATILE Usage:

(define_constants
  [(VUNSPEC_BLOCKAGE 0) ; `blockage' insn to prevent scheduling across an
			;   insn in the code.
   (VUNSPEC_EPILOGUE 1) ; `epilogue' insn, used to represent any part of the
			;   instruction epilogue sequence that isn't expanded
			;   into normal RTL.  Used for both normal and sibcall
			;   epilogues.
   (VUNSPEC_ALIGN    2) ; `align' insn.  Used at the head of a minipool table 
			;   for inlined constants.
   (VUNSPEC_POOL_END 3) ; `end-of-table'.  Used to mark the end of a minipool
			;   table.
   (VUNSPEC_POOL_1   4) ; `pool-entry(1)'.  An entry in the constant pool for
			;   an 8-bit object.
   (VUNSPEC_POOL_2   5) ; `pool-entry(2)'.  An entry in the constant pool for
			;   a 16-bit object.
   (VUNSPEC_POOL_4   6) ; `pool-entry(4)'.  An entry in the constant pool for
			;   a 32-bit object.
   (VUNSPEC_POOL_8   7) ; `pool-entry(8)'.  An entry in the constant pool for
			;   a 64-bit object.
   (VUNSPEC_PREFETCH 8) ; `pld' insn to prefetch a cache line:
			;   operand 0 is the address to fetch.
  ]
)

;;---------------------------------------------------------------------------
;; Attributes

; IS_THUMB is set to 'yes' when we are generating Thumb code, and 'no' when
; generating ARM code.  This is used to control the length of some insn
; patterns that share the same RTL in both ARM and Thumb code.
(define_attr "is_thumb" "no,yes" (const (symbol_ref "thumb_code")))

; PROG_MODE attribute is used to determine whether condition codes are
; clobbered by a call insn: they are if in prog32 mode.  This is controlled
; by the -mapcs-{32,26} flag, and possibly the -mcpu=... option.
(define_attr "prog_mode" "prog26,prog32" (const (symbol_ref "arm_prog_mode")))

; IS_STRONGARM is set to 'yes' when compiling for StrongARM, it affects
; scheduling decisions for the load unit and the multiplier.
(define_attr "is_strongarm" "no,yes" (const (symbol_ref "arm_is_strong")))

;; Operand number of an input operand that is shifted.  Zero if the
;; given instruction does not shift one of its input operands.
(define_attr "is_xscale" "no,yes" (const (symbol_ref "arm_is_xscale")))
(define_attr "shift" "" (const_int 0))

; Floating Point Unit.  If we only have floating point emulation, then there
; is no point in scheduling the floating point insns.  (Well, for best
; performance we should try and group them together).
(define_attr "fpu" "fpa,fpe2,fpe3" (const (symbol_ref "arm_fpu_attr")))

; LENGTH of an instruction (in bytes)
(define_attr "length" "" (const_int 4))

; POOL_RANGE is how far away from a constant pool entry that this insn
; can be placed.  If the distance is zero, then this insn will never
; reference the pool.
; NEG_POOL_RANGE is nonzero for insns that can reference a constant pool entry
; before its address.
(define_attr "pool_range" "" (const_int 0))
(define_attr "neg_pool_range" "" (const_int 0))

; An assembler sequence may clobber the condition codes without us knowing.
(define_asm_attributes
 [(set_attr "conds" "clob")])

; TYPE attribute is used to detect floating point instructions which, if
; running on a co-processor can run in parallel with other, basic instructions
; If write-buffer scheduling is enabled then it can also be used in the
; scheduling of writes.

; Classification of each insn
; normal	any data instruction that doesn't hit memory or fp regs
; mult		a multiply instruction
; block		blockage insn, this blocks all functional units
; float		a floating point arithmetic operation (subject to expansion)
; fdivx		XFmode floating point division
; fdivd		DFmode floating point division
; fdivs		SFmode floating point division
; fmul		Floating point multiply
; ffmul		Fast floating point multiply
; farith	Floating point arithmetic (4 cycle)
; ffarith	Fast floating point arithmetic (2 cycle)
; float_em	a floating point arithmetic operation that is normally emulated
;		even on a machine with an fpa.
; f_load	a floating point load from memory
; f_store	a floating point store to memory
; f_mem_r	a transfer of a floating point register to a real reg via mem
; r_mem_f	the reverse of f_mem_r
; f_2_r		fast transfer float to arm (no memory needed)
; r_2_f		fast transfer arm to float
; call		a subroutine call
; load		any load from memory
; store1	store 1 word to memory from arm registers
; store2	store 2 words
; store3	store 3 words
; store4	store 4 words
;
(define_attr "type"
	"normal,mult,block,float,fdivx,fdivd,fdivs,fmul,ffmul,farith,ffarith,float_em,f_load,f_store,f_mem_r,r_mem_f,f_2_r,r_2_f,call,load,store1,store2,store3,store4" 
	(const_string "normal"))

; Load scheduling, set from the arm_ld_sched variable
; initialised by arm_override_options() 
(define_attr "ldsched" "no,yes" (const (symbol_ref "arm_ld_sched")))

; condition codes: this one is used by final_prescan_insn to speed up
; conditionalizing instructions.  It saves having to scan the rtl to see if
; it uses or alters the condition codes.
; 
; USE means that the condition codes are used by the insn in the process of
;   outputting code, this means (at present) that we can't use the insn in
;   inlined branches
;
; SET means that the purpose of the insn is to set the condition codes in a
;   well defined manner.
;
; CLOB means that the condition codes are altered in an undefined manner, if
;   they are altered at all
;
; JUMP_CLOB is used when the condition cannot be represented by a single
;   instruction (UNEQ and LTGT).  These cannot be predicated.
;
; NOCOND means that the condition codes are neither altered nor affect the
;   output of this insn

(define_attr "conds" "use,set,clob,jump_clob,nocond"
	(if_then_else (eq_attr "type" "call")
	 (if_then_else (eq_attr "prog_mode" "prog32")
	  (const_string "clob") (const_string "nocond"))
	 (const_string "nocond")))

; Predicable means that the insn can be conditionally executed based on
; an automatically added predicate (additional patterns are generated by 
; gen...).  We default to 'no' because no Thumb patterns match this rule
; and not all ARM patterns do.
(define_attr "predicable" "no,yes" (const_string "no"))

; Only model the write buffer for ARM6 and ARM7.  Earlier processors don't
; have one.  Later ones, such as StrongARM, have write-back caches, so don't
; suffer blockages enough to warrent modelling this (and it can adversely
; affect the schedule).
(define_attr "model_wbuf" "no,yes" (const (symbol_ref "arm_is_6_or_7")))

; WRITE_CONFLICT implies that a read following an unrelated write is likely
; to stall the processor.  Used with model_wbuf above.
(define_attr "write_conflict" "no,yes"
  (if_then_else (eq_attr "type"
		 "block,float_em,f_load,f_store,f_mem_r,r_mem_f,call,load")
		(const_string "yes")
		(const_string "no")))

; Classify the insns into those that take one cycle and those that take more
; than one on the main cpu execution unit.
(define_attr "core_cycles" "single,multi"
  (if_then_else (eq_attr "type"
		 "normal,float,fdivx,fdivd,fdivs,fmul,ffmul,farith,ffarith")
		(const_string "single")
	        (const_string "multi")))

;; FAR_JUMP is "yes" if a BL instruction is used to generate a branch to a
;; distant label.  Only applicable to Thumb code.
(define_attr "far_jump" "yes,no" (const_string "no"))

;; (define_function_unit {name} {num-units} {n-users} {test}
;;                       {ready-delay} {issue-delay} [{conflict-list}])




;;---------------------------------------------------------------------------
;; Insn patterns
;;
;; Addition insns.

;; Note: For DImode insns, there is normally no reason why operands should
;; not be in the same register, what we don't want is for something being
;; written to partially overlap something that is an input.

(define_expand "adddi3"
 [(parallel
   [(set (match_operand:DI           0 "register_operand" "")
	  (plus:DI (match_operand:DI 1 "register_operand" "")
	           (match_operand:DI 2 "register_operand" "")))
    (clobber (reg:CC CC_REGNUM))])]
  ""
  "
    {
      if (GET_CODE (operands[1]) != REG)
        operands[1] = force_reg (SImode, operands[1]);
      if (GET_CODE (operands[2]) != REG)
        operands[2] = force_reg (SImode, operands[2]);
     }
  "
)

(define_insn "*thumb_adddi3"
  [(set (match_operand:DI          0 "register_operand" "=r")
	(plus:DI (match_operand:DI 1 "register_operand" "%0")
		 (match_operand:DI 2 "register_operand" "r")))
   (clobber (reg:CC CC_REGNUM))
  ]
  "TARGET_THUMB"
  "add\\t%Q0, %Q0, %Q2\;adc\\t%R0, %R0, %R2"
 
)







;; Register group 'k' is a single register group containing only the stack
;; register.  Trying to reload it will always fail catastrophically,
;; so never allow those alternatives to match if reloading is needed.

(define_insn "*thumb_addsi3"
  [(set (match_operand:SI          0 "register_operand" "=r,r,r,*r,*h,r,!k")
	(plus:SI (match_operand:SI 1 "register_operand" "%0,0,l,*0,*0,!k,!k")
		 (match_operand:SI 2 "nonmemory_operand" "I,J,lL,*h,*r,!M,!O")))]
  "TARGET_THUMB"
  "*
   static const char * const asms[] = 
   {
     \"add\\t%0, %0, %2\",
     \"sub\\t%0, %0, #%n2\",
     \"add\\t%0, %1, %2\",
     \"add\\t%0, %0, %2\",
     \"add\\t%0, %0, %2\",
     \"add\\t%0, %1, %2\",
     \"add\\t%0, %1, %2\"
   };
   if ((which_alternative == 2 || which_alternative == 6)
       && GET_CODE (operands[2]) == CONST_INT
       && INTVAL (operands[2]) < 0)
     return \"sub\\t%0, %1, #%n2\";
   return asms[which_alternative];
  "
 
)

;; Reloading and elimination of the frame pointer can
;; sometimes cause this optimization to be missed.
(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "=l")
	(match_operand:SI 1 "const_int_operand" "M"))
   (set (match_dup 0)
	(plus:SI (match_dup 0) (match_operand:SI 2 "register_operand" "k")))]
  "TARGET_THUMB
   && REGNO (operands[2]) == STACK_POINTER_REGNUM 
   && (unsigned HOST_WIDE_INT) (INTVAL (operands[1])) < 1024
   && (INTVAL (operands[1]) & 3) == 0"
  [(set (match_dup 0) (plus:SI (match_dup 2) (match_dup 1)))]
  ""
)



;; The next four insns work because they compare the result with one of
;; the operands, and we know that the use of the condition code is
;; either GEU or LTU, so we can use the carry flag from the addition
;; instead of doing the compare a second time.


(define_expand "subdi3"
 [(parallel
   [(set (match_operand:DI            0 "s_register_operand" "")
	  (minus:DI (match_operand:DI 1 "s_register_operand" "")
	            (match_operand:DI 2 "s_register_operand" "")))
    (clobber (reg:CC CC_REGNUM))])]
  "TARGET_EITHER"
  "
  if (TARGET_THUMB)
    {
      if (GET_CODE (operands[1]) != REG)
        operands[1] = force_reg (SImode, operands[1]);
      if (GET_CODE (operands[2]) != REG)
        operands[2] = force_reg (SImode, operands[2]);
     }	
  "
)



(define_insn "*thumb_subdi3"
  [(set (match_operand:DI           0 "register_operand" "=l")
	(minus:DI (match_operand:DI 1 "register_operand"  "0")
		  (match_operand:DI 2 "register_operand"  "l")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB"
  "sub\\t%Q0, %Q0, %Q2\;sbc\\t%R0, %R0, %R2"
 
)




(define_expand "subsi3"
  [(set (match_operand:SI           0 "s_register_operand" "")
	(minus:SI (match_operand:SI 1 "reg_or_int_operand" "")
		  (match_operand:SI 2 "s_register_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[1]) == CONST_INT)
    {
        operands[1] = force_reg (SImode, operands[1]);
    }
  "
)

(define_insn "*thumb_subsi3_insn"
  [(set (match_operand:SI           0 "register_operand" "=l")
	(minus:SI (match_operand:SI 1 "register_operand" "l")
		  (match_operand:SI 2 "register_operand" "l")))]
  "TARGET_THUMB"
  "sub\\t%0, %1, %2"
 
)



;; Multiplication insns

(define_expand "mulsi3"
  [(set (match_operand:SI          0 "s_register_operand" "")
	(mult:SI (match_operand:SI 2 "s_register_operand" "")
		 (match_operand:SI 1 "s_register_operand" "")))]
  "TARGET_EITHER"
  ""
)



; Unfortunately with the Thumb the '&'/'0' trick can fails when operands 
; 1 and 2; are the same, because reload will make operand 0 match 
; operand 1 without realizing that this conflicts with operand 2.  We fix 
; this by adding another alternative to match this case, and then `reload' 
; it ourselves.  This alternative must come first.
(define_insn "*thumb_mulsi3"
  [(set (match_operand:SI          0 "register_operand" "=&l,&l,&l")
	(mult:SI (match_operand:SI 1 "register_operand" "%l,*h,0")
		 (match_operand:SI 2 "register_operand" "l,l,l")))]
  "TARGET_THUMB"
  "*
  if (which_alternative < 2)
    return \"mov\\t%0, %1\;mul\\t%0, %0, %2\";
  else
    return \"mul\\t%0, %0, %2\";
  "

)





;; Unnamed templates to match MLA instruction.

;; Unnamed template to match long long multiply-accumlate (smlal)

;; Unnamed template to match long long unsigned multiply-accumlate (umlal)

;; Division insns

;; Modulo insns

;; Boolean and,ior,xor insns

;; Split up double word logical operations

;; Split up simple DImode logical operations.  Simply perform the logical
;; operation on the upper and lower halves of the registers.


;; The zero extend of operand 2 means we can just copy the high part of
;; operand1 into operand0.


;; The zero extend of operand 2 means we can just copy the high part of
;; operand1 into operand0.



(define_expand "andsi3"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(and:SI (match_operand:SI 1 "s_register_operand" "")
		(match_operand:SI 2 "reg_or_int_operand" "")))]
  "TARGET_EITHER"
  "

      if (GET_CODE (operands[2]) != CONST_INT)
        operands[2] = force_reg (SImode, operands[2]);
      else
        {
          int i;
	  
          if (((unsigned HOST_WIDE_INT) ~INTVAL (operands[2])) < 256)
  	    {
	      operands[2] = force_reg (SImode,
				       GEN_INT (~INTVAL (operands[2])));
	      
	      emit_insn (gen_bicsi3 (operands[0], operands[2], operands[1]));
	      
	      DONE;
	    }

          for (i = 9; i <= 31; i++)
	    {
	      if ((((HOST_WIDE_INT) 1) << i) - 1 == INTVAL (operands[2]))
	        {
	          emit_insn (gen_extzv (operands[0], operands[1], GEN_INT (i),
			 	        const0_rtx));
	          DONE;
	        }
	      else if ((((HOST_WIDE_INT) 1) << i) - 1
		       == ~INTVAL (operands[2]))
	        {
	          rtx shift = GEN_INT (i);
	          rtx reg = gen_reg_rtx (SImode);
		
	          emit_insn (gen_lshrsi3 (reg, operands[1], shift));
	          emit_insn (gen_ashlsi3 (operands[0], reg, shift));
		  
	          DONE;
	        }
	    }

          operands[2] = force_reg (SImode, operands[2]);
        }
    
  "
)



(define_insn "*thumb_andsi3_insn"
  [(set (match_operand:SI         0 "register_operand" "=l")
	(and:SI (match_operand:SI 1 "register_operand" "%0")
		(match_operand:SI 2 "register_operand" "l")))]
  "TARGET_THUMB"
  "and\\t%0, %0, %2"
 
)



(define_insn "bicsi3"
  [(set (match_operand:SI                 0 "register_operand" "=l")
	(and:SI (not:SI (match_operand:SI 1 "register_operand" "l"))
		(match_operand:SI         2 "register_operand" "0")))]
  "TARGET_THUMB"
  "bic\\t%0, %0, %1"
 
)


(define_expand "iorsi3"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(ior:SI (match_operand:SI 1 "s_register_operand" "")
		(match_operand:SI 2 "reg_or_int_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[2]) == CONST_INT)
    {
	operands [2] = force_reg (SImode, operands [2]);
    }
  "
)



(define_insn "*thumb_iorsi3"
  [(set (match_operand:SI         0 "register_operand" "=l")
	(ior:SI (match_operand:SI 1 "register_operand" "%0")
		(match_operand:SI 2 "register_operand" "l")))]
  "TARGET_THUMB"
  "orr\\t%0, %0, %2"
 
)


(define_expand "xorsi3"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(xor:SI (match_operand:SI 1 "s_register_operand" "")
		(match_operand:SI 2 "arm_rhs_operand"  "")))]
  "TARGET_EITHER"
  "if (TARGET_THUMB)
     if (GET_CODE (operands[2]) == CONST_INT)
       operands[2] = force_reg (SImode, operands[2]);
  "
)


(define_insn "*thumb_xorsi3"
  [(set (match_operand:SI         0 "register_operand" "=l")
	(xor:SI (match_operand:SI 1 "register_operand" "%0")
		(match_operand:SI 2 "register_operand" "l")))]
  "TARGET_THUMB"
  "eor\\t%0, %0, %2"
 
)


; By splitting (IOR (AND (NOT A) (NOT B)) C) as D = AND (IOR A B) (NOT C), 
; (NOT D) we can sometimes merge the final NOT into one of the following
; insns.



;; Minimum and maximum insns



; Reject the frame pointer in operand[1], since reloading this after
; it has been eliminated can cause carnage.

;; Shift and rotation insns

(define_expand "ashlsi3"
  [(set (match_operand:SI            0 "s_register_operand" "")
	(ashift:SI (match_operand:SI 1 "s_register_operand" "")
		   (match_operand:SI 2 "arm_rhs_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[2]) == CONST_INT
      && ((unsigned HOST_WIDE_INT) INTVAL (operands[2])) > 31)
    {
      emit_insn (gen_movsi (operands[0], const0_rtx));
      DONE;
    }
  "
)

(define_insn "*thumb_ashlsi3"
  [(set (match_operand:SI            0 "register_operand" "=l,l")
	(ashift:SI (match_operand:SI 1 "register_operand" "l,0")
		   (match_operand:SI 2 "nonmemory_operand" "N,l")))]
  "TARGET_THUMB"
  "lsl\\t%0, %1, %2"
 
)

(define_expand "ashrsi3"
  [(set (match_operand:SI              0 "s_register_operand" "")
	(ashiftrt:SI (match_operand:SI 1 "s_register_operand" "")
		     (match_operand:SI 2 "arm_rhs_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[2]) == CONST_INT
      && ((unsigned HOST_WIDE_INT) INTVAL (operands[2])) > 31)
    operands[2] = GEN_INT (31);
  "
)

(define_insn "*thumb_ashrsi3"
  [(set (match_operand:SI              0 "register_operand" "=l,l")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "l,0")
		     (match_operand:SI 2 "nonmemory_operand" "N,l")))]
  "TARGET_THUMB"
  "asr\\t%0, %1, %2"
 
)

(define_expand "lshrsi3"
  [(set (match_operand:SI              0 "s_register_operand" "")
	(lshiftrt:SI (match_operand:SI 1 "s_register_operand" "")
		     (match_operand:SI 2 "arm_rhs_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[2]) == CONST_INT
      && ((unsigned HOST_WIDE_INT) INTVAL (operands[2])) > 31)
    {
      emit_insn (gen_movsi (operands[0], const0_rtx));
      DONE;
    }
  "
)

(define_insn "*thumb_lshrsi3"
  [(set (match_operand:SI              0 "register_operand" "=l,l")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "l,0")
		     (match_operand:SI 2 "nonmemory_operand" "N,l")))]
  "TARGET_THUMB"
  "lsr\\t%0, %1, %2"
 
)



(define_expand "rotrsi3"
  [(set (match_operand:SI              0 "s_register_operand" "")
	(rotatert:SI (match_operand:SI 1 "s_register_operand" "")
		     (match_operand:SI 2 "arm_rhs_operand" "")))]
  "TARGET_EITHER"
  "

      if (GET_CODE (operands [2]) == CONST_INT)
        operands [2] = force_reg (SImode, operands[2]);
    
  "
)

(define_insn "*thumb_rotrsi3"
  [(set (match_operand:SI              0 "register_operand" "=l")
	(rotatert:SI (match_operand:SI 1 "register_operand" "0")
		     (match_operand:SI 2 "register_operand" "l")))]
  "TARGET_THUMB"
  "ror\\t%0, %0, %2"
 
)



;; We don't really have extzv, but defining this using shifts helps
;; to reduce register pressure later on.

(define_expand "extzv"
  [(set (match_dup 4)
	(ashift:SI (match_operand:SI   1 "register_operand" "")
		   (match_operand:SI   2 "const_int_operand" "")))
   (set (match_operand:SI              0 "register_operand" "")
	(lshiftrt:SI (match_dup 4)
		     (match_operand:SI 3 "const_int_operand" "")))]
  "TARGET_THUMB"
  "
  {
    HOST_WIDE_INT lshift = 32 - INTVAL (operands[2]) - INTVAL (operands[3]);
    HOST_WIDE_INT rshift = 32 - INTVAL (operands[2]);
    
    operands[3] = GEN_INT (rshift);
    
    if (lshift == 0)
      {
        emit_insn (gen_lshrsi3 (operands[0], operands[1], operands[3]));
        DONE;
      }
      
    operands[2] = GEN_INT (lshift);
    operands[4] = gen_reg_rtx (SImode);
  }"
)


;; Unary arithmetic insns

(define_expand "negdi2"
 [(parallel
   [(set (match_operand:DI          0 "s_register_operand" "")
	  (neg:DI (match_operand:DI 1 "s_register_operand" "")))
    (clobber (reg:CC CC_REGNUM))])]
  "TARGET_EITHER"
  "
  if (TARGET_THUMB)
    {
      if (GET_CODE (operands[1]) != REG)
        operands[1] = force_reg (SImode, operands[1]);
     }
  "
)

;; The constraints here are to prevent a *partial* overlap (where %Q0 == %R1).
;; The second alternative is to allow the common case of a *full* overlap.


(define_insn "*thumb_negdi2"
  [(set (match_operand:DI         0 "register_operand" "=&l")
	(neg:DI (match_operand:DI 1 "register_operand"   "l")))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_THUMB"
  "mov\\t%R0, #0\;neg\\t%Q0, %Q1\;sbc\\t%R0, %R1"
  
)

(define_expand "negsi2"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(neg:SI (match_operand:SI 1 "s_register_operand" "")))]
  "TARGET_EITHER"
  ""
)


(define_insn "*thumb_negsi2"
  [(set (match_operand:SI         0 "register_operand" "=l")
	(neg:SI (match_operand:SI 1 "register_operand" "l")))]
  "TARGET_THUMB"
  "neg\\t%0, %1"
 
)



;; abssi2 doesn't really clobber the condition codes if a different register
;; is being set.  To keep things simple, assume during rtl manipulations that
;; it does, but tell the final scan operator the truth.  Similarly for
;; (neg (abs...))





(define_expand "one_cmplsi2"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(not:SI (match_operand:SI 1 "s_register_operand" "")))]
  "TARGET_EITHER"
  ""
)

(define_insn "*thumb_one_cmplsi2"
  [(set (match_operand:SI         0 "register_operand" "=l")
	(not:SI (match_operand:SI 1 "register_operand"  "l")))]
  "TARGET_THUMB"
  "mvn\\t%0, %1"
 
)


;; Truncation insns

;; Zero and sign extension instructions.


(define_expand "zero_extendhisi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:HI 1 "nonimmediate_operand" "")
		   (const_int 16)))
   (set (match_operand:SI 0 "s_register_operand" "")
	(lshiftrt:SI (match_dup 2) (const_int 16)))]
  "TARGET_EITHER"
  "
  {
      
        if (GET_CODE (operands[1]) == MEM)
	  {
	    rtx tmp;

	    tmp = gen_rtx_ZERO_EXTEND (SImode, operands[1]);
	    tmp = gen_rtx_SET (VOIDmode, operands[0], tmp);
	    emit_insn (tmp);
	  }
	else
	  {
	    rtx ops[3];
	    
	    if (!s_register_operand (operands[1], HImode))
	      operands[1] = copy_to_mode_reg (HImode, operands[1]);
	    operands[1] = gen_lowpart (SImode, operands[1]);
	    operands[2] = gen_reg_rtx (SImode);
	    
	    ops[0] = operands[2];
	    ops[1] = operands[1];
	    ops[2] = GEN_INT (16);
	    
	    emit_insn (gen_rtx_SET (VOIDmode, ops[0],
				    gen_rtx_ASHIFT (SImode, ops[1], ops[2])));

	    ops[0] = operands[0];
	    ops[1] = operands[2];
	    ops[2] = GEN_INT (16);

	    emit_insn (gen_rtx_SET (VOIDmode, ops[0],
				    gen_rtx_LSHIFTRT (SImode, ops[1],
						      ops[2])));
	  }
	DONE; 
      
  }"
)

(define_insn "*thumb_zero_extendhisi2"
  [(set (match_operand:SI                 0 "register_operand" "=l")
	(zero_extend:SI (match_operand:HI 1 "memory_operand"    "m")))]
  "TARGET_THUMB"
  "*
  rtx mem = XEXP (operands[1], 0);

  if (GET_CODE (mem) == CONST)
    mem = XEXP (mem, 0);
    
  if (GET_CODE (mem) == LABEL_REF)
    return \"ldr\\t%0, %1\";
    
  if (GET_CODE (mem) == PLUS)
    {
      rtx a = XEXP (mem, 0);
      rtx b = XEXP (mem, 1);

      /* This can happen due to bugs in reload.  */
      if (GET_CODE (a) == REG && REGNO (a) == SP_REGNUM)
        {
          rtx ops[2];
          ops[0] = operands[0];
          ops[1] = a;
      
          output_asm_insn (\"mov	%0, %1\", ops);

          XEXP (mem, 0) = operands[0];
       }

      else if (   GET_CODE (a) == LABEL_REF
	       && GET_CODE (b) == CONST_INT)
        return \"ldr\\t%0, %1\";
    }
    
  return \"ldrh\\t%0, %1\";
  "

)


(define_expand "zero_extendqisi2"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[1]) != MEM)
        {
          rtx temp = gen_reg_rtx (SImode);
	  rtx ops[3];
	  
          operands[1] = copy_to_mode_reg (QImode, operands[1]);
          operands[1] = gen_lowpart (SImode, operands[1]);

	  ops[0] = temp;
	  ops[1] = operands[1];
	  ops[2] = GEN_INT (24);

	  emit_insn (gen_rtx_SET (VOIDmode, ops[0],
				  gen_rtx_ASHIFT (SImode, ops[1], ops[2])));
	  
          ops[0] = operands[0];
	  ops[1] = temp;
	  ops[2] = GEN_INT (24);

	  emit_insn (gen_rtx_SET (VOIDmode, ops[0],
				  gen_rtx_LSHIFTRT (SImode, ops[1], ops[2])));
	}
      DONE;
    }
  "
)

(define_insn "*thumb_zero_extendqisi2"
  [(set (match_operand:SI                 0 "register_operand" "=l")
	(zero_extend:SI (match_operand:QI 1 "memory_operand"    "m")))]
  "TARGET_THUMB"
  "ldrb\\t%0, %1"

)


(define_expand "extendhisi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:HI 1 "nonimmediate_operand" "")
		   (const_int 16)))
   (set (match_operand:SI 0 "s_register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 16)))]
  "TARGET_EITHER"
  "
  {
    if (!s_register_operand (operands[1], HImode))
      operands[1] = copy_to_mode_reg (HImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[2] = gen_reg_rtx (SImode);

    if (TARGET_THUMB)
      {
	rtx ops[3];
	
	ops[0] = operands[2];
	ops[1] = operands[1];
	ops[2] = GEN_INT (16);
	
        emit_insn (gen_rtx_SET (VOIDmode, ops[0],
				gen_rtx_ASHIFT (SImode, ops[1], ops[2])));
	    
	ops[0] = operands[0];
	ops[1] = operands[2];
	ops[2] = GEN_INT (16);
	
        emit_insn (gen_rtx_SET (VOIDmode, ops[0],
				gen_rtx_ASHIFTRT (SImode, ops[1], ops[2])));
	
	DONE;
      }
  }"
)

(define_insn "*thumb_extendhisi2_insn"
  [(set (match_operand:SI                 0 "register_operand" "=l")
	(sign_extend:SI (match_operand:HI 1 "memory_operand"    "m")))
   (clobber (match_scratch:SI             2                   "=&l"))]
  "TARGET_THUMB"
  "*
  {
    rtx ops[4];
    rtx mem = XEXP (operands[1], 0);

    /* This code used to try to use 'V', and fix the address only if it was
       offsettable, but this fails for e.g. REG+48 because 48 is outside the
       range of QImode offsets, and offsettable_address_p does a QImode
       address check.  */
       
    if (GET_CODE (mem) == CONST)
      mem = XEXP (mem, 0);
    
    if (GET_CODE (mem) == LABEL_REF)
      return \"ldr\\t%0, %1\";
    
    if (GET_CODE (mem) == PLUS)
      {
        rtx a = XEXP (mem, 0);
        rtx b = XEXP (mem, 1);

        if (GET_CODE (a) == LABEL_REF
	    && GET_CODE (b) == CONST_INT)
          return \"ldr\\t%0, %1\";

        if (GET_CODE (b) == REG)
          return \"ldrsh\\t%0, %1\";
	  
        ops[1] = a;
        ops[2] = b;
      }
    else
      {
        ops[1] = mem;
        ops[2] = const0_rtx;
      }
      
    if (GET_CODE (ops[1]) != REG)
      {
        debug_rtx (ops[1]);
        abort ();
      }

    ops[0] = operands[0];
    ops[3] = operands[2];
    output_asm_insn (\"mov\\t%3, %2\;ldrsh\\t%0, [%1, %3]\", ops);
    return \"\";
  }"

)


(define_expand "extendqisi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:QI 1 "general_operand" "")
		   (const_int 24)))
   (set (match_operand:SI 0 "s_register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 24)))]
  "TARGET_EITHER"
  "
  {
    if (!s_register_operand (operands[1], QImode))
      operands[1] = copy_to_mode_reg (QImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[2] = gen_reg_rtx (SImode);
    
    if (TARGET_THUMB)
      {
	rtx ops[3];
	
	ops[0] = operands[2];
	ops[1] = operands[1];
	ops[2] = GEN_INT (24);
	
        emit_insn (gen_rtx_SET (VOIDmode, ops[0],
		   gen_rtx_ASHIFT (SImode, ops[1], ops[2])));

	ops[0] = operands[0];
	ops[1] = operands[2];
	ops[2] = GEN_INT (24);
	
        emit_insn (gen_rtx_SET (VOIDmode, ops[0],
		   gen_rtx_ASHIFTRT (SImode, ops[1], ops[2])));
	
	DONE;
      }
  }"
)

; Rather than restricting all byte accesses to memory addresses that ldrsb
; can handle, we fix up the ones that ldrsb can't grok with a split.


(define_insn "*thumb_extendqisi2_insn"
  [(set (match_operand:SI                 0 "register_operand" "=l,l")
	(sign_extend:SI (match_operand:QI 1 "memory_operand"    "V,m")))]
  "TARGET_THUMB"
  "*
  {
    rtx ops[3];
    rtx mem = XEXP (operands[1], 0);
    
    if (GET_CODE (mem) == CONST)
      mem = XEXP (mem, 0);
    
    if (GET_CODE (mem) == LABEL_REF)
      return \"ldr\\t%0, %1\";

    if (GET_CODE (mem) == PLUS
        && GET_CODE (XEXP (mem, 0)) == LABEL_REF)
      return \"ldr\\t%0, %1\";
      
    if (which_alternative == 0)
      return \"ldrsb\\t%0, %1\";
      
    ops[0] = operands[0];
    
    if (GET_CODE (mem) == PLUS)
      {
        rtx a = XEXP (mem, 0);
	rtx b = XEXP (mem, 1);
	
        ops[1] = a;
        ops[2] = b;

        if (GET_CODE (a) == REG)
	  {
	    if (GET_CODE (b) == REG)
              output_asm_insn (\"ldrsb\\t%0, [%1, %2]\", ops);
            else if (REGNO (a) == REGNO (ops[0]))
	      {
                output_asm_insn (\"ldrb\\t%0, [%1, %2]\", ops);
		output_asm_insn (\"lsl\\t%0, %0, #24\", ops);
		output_asm_insn (\"asr\\t%0, %0, #24\", ops);
	      }
	    else
              output_asm_insn (\"mov\\t%0, %2\;ldrsb\\t%0, [%1, %0]\", ops);
	  }
        else if (GET_CODE (b) != REG)
	  abort ();
	else
          {
            if (REGNO (b) == REGNO (ops[0]))
	      {
                output_asm_insn (\"ldrb\\t%0, [%2, %1]\", ops);
		output_asm_insn (\"lsl\\t%0, %0, #24\", ops);
		output_asm_insn (\"asr\\t%0, %0, #24\", ops);
	      }
	    else
              output_asm_insn (\"mov\\t%0, %2\;ldrsb\\t%0, [%1, %0]\", ops);
          }
      }
    else if (GET_CODE (mem) == REG && REGNO (ops[0]) == REGNO (mem))
      {
        output_asm_insn (\"ldrb\\t%0, [%0, #0]\", ops);
	output_asm_insn (\"lsl\\t%0, %0, #24\", ops);
	output_asm_insn (\"asr\\t%0, %0, #24\", ops);
      }
    else
      {
        ops[1] = mem;
        ops[2] = const0_rtx;
	
        output_asm_insn (\"mov\\t%0, %2\;ldrsb\\t%0, [%1, %0]\", ops);
      }
    return \"\";
  }"

)



;; Move insns (including loads and stores)

;; XXX Just some ideas about movti.
;; I don't think these are a good idea on the arm, there just aren't enough
;; registers
;;(define_expand "loadti"
;;  [(set (match_operand:TI 0 "s_register_operand" "")
;;	(mem:TI (match_operand:SI 1 "address_operand" "")))]
;;  "" "")

;;(define_expand "storeti"
;;  [(set (mem:TI (match_operand:TI 0 "address_operand" ""))
;;	(match_operand:TI 1 "s_register_operand" ""))]
;;  "" "")

;;(define_expand "movti"
;;  [(set (match_operand:TI 0 "general_operand" "")
;;	(match_operand:TI 1 "general_operand" ""))]
;;  ""
;;  "
;;{
;;  rtx insn;
;;
;;  if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == MEM)
;;    operands[1] = copy_to_reg (operands[1]);
;;  if (GET_CODE (operands[0]) == MEM)
;;    insn = gen_storeti (XEXP (operands[0], 0), operands[1]);
;;  else if (GET_CODE (operands[1]) == MEM)
;;    insn = gen_loadti (operands[0], XEXP (operands[1], 0));
;;  else
;;    FAIL;
;;
;;  emit_insn (insn);
;;  DONE;
;;}")

;; Recognise garbage generated above.

;;(define_insn ""
;;  [(set (match_operand:TI 0 "general_operand" "=r,r,r,<,>,m")
;;	(match_operand:TI 1 "general_operand" "<,>,m,r,r,r"))]
;;  ""
;;  "*
;;  {
;;    register mem = (which_alternative < 3);
;;    register const char *template;
;;
;;    operands[mem] = XEXP (operands[mem], 0);
;;    switch (which_alternative)
;;      {
;;      case 0: template = \"ldmdb\\t%1!, %M0\"; break;
;;      case 1: template = \"ldmia\\t%1!, %M0\"; break;
;;      case 2: template = \"ldmia\\t%1, %M0\"; break;
;;      case 3: template = \"stmdb\\t%0!, %M1\"; break;
;;      case 4: template = \"stmia\\t%0!, %M1\"; break;
;;      case 5: template = \"stmia\\t%0, %M1\"; break;
;;      }
;;    output_asm_insn (template, operands);
;;    return \"\";
;;  }")

(define_expand "movdi"
  [(set (match_operand:DI 0 "general_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
  if (TARGET_THUMB)
    {
      if (!no_new_pseudos)
        {
          if (GET_CODE (operands[0]) != REG)
	    operands[1] = force_reg (DImode, operands[1]);
        }
    }
  "
)


;;; ??? This should have alternatives for constants.
;;; ??? This was originally identical to the movdf_insn pattern.
;;; ??? The 'i' constraint looks funny, but it should always be replaced by
;;; thumb_reorg with a memory reference.
(define_insn "*thumb_movdi_insn"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=l,l,l,l,>,l, m,*r")
	(match_operand:DI 1 "general_operand"      "l, I,J,>,l,mi,l,*r"))]
  "TARGET_THUMB
   && (   register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode))"
  "*
  {
  switch (which_alternative)
    {
    default:
    case 0:
      if (REGNO (operands[1]) == REGNO (operands[0]) + 1)
	return \"add\\t%0,  %1,  #0\;add\\t%H0, %H1, #0\";
      return   \"add\\t%H0, %H1, #0\;add\\t%0,  %1,  #0\";
    case 1:
      return \"mov\\t%Q0, %1\;mov\\t%R0, #0\";
    case 2:
      operands[1] = GEN_INT (- INTVAL (operands[1]));
      return \"mov\\t%Q0, %1\;neg\\t%Q0, %Q0\;asr\\t%R0, %Q0, #31\";
    case 3:
      return \"ldmia\\t%1, {%0, %H0}\";
    case 4:
      return \"stmia\\t%0, {%1, %H1}\";
    case 5:
      return thumb_load_double_from_address (operands);
    case 6:
      operands[2] = gen_rtx (MEM, SImode,
			     plus_constant (XEXP (operands[0], 0), 4));
      output_asm_insn (\"str\\t%1, %0\;str\\t%H1, %2\", operands);
      return \"\";
    case 7:
      if (REGNO (operands[1]) == REGNO (operands[0]) + 1)
	return \"mov\\t%0, %1\;mov\\t%H0, %H1\";
      return \"mov\\t%H0, %H1\;mov\\t%0, %1\";
    }
  }"

)

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
        (match_operand:SI 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
 /* TARGET_THUMB.... */
    {
      if (!no_new_pseudos)
        {
          if (GET_CODE (operands[0]) != REG)
	    operands[1] = force_reg (SImode, operands[1]);
        }
    }
    
  if (flag_pic
      && (CONSTANT_P (operands[1])
	 || symbol_mentioned_p (operands[1])
	 || label_mentioned_p (operands[1])))
    operands[1] = legitimize_pic_address (operands[1], SImode,
					  (no_new_pseudos ? operands[0] : 0));
  "
)


(define_insn "*thumb_movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=l,l,l,l,l,>,l, m,*lh")
	(match_operand:SI 1 "general_operand"      "l, I,J,K,>,l,mi,l,*lh"))]
  "TARGET_THUMB
   && (   register_operand (operands[0], SImode) 
       || register_operand (operands[1], SImode))"
  "@
   mov	%0, %1
   mov	%0, %1
   #
   #
   ldmia\\t%1, {%0}
   stmia\\t%0, {%1}
   ldr\\t%0, %1
   str\\t%1, %0
   mov\\t%0, %1"

)

(define_split 
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))]
  "TARGET_THUMB && CONST_OK_FOR_THUMB_LETTER (INTVAL (operands[1]), 'J')"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 0) (neg:SI (match_dup 0)))]
  "operands[1] = GEN_INT (- INTVAL (operands[1]));"
)

(define_split 
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "const_int_operand" ""))]
  "TARGET_THUMB && CONST_OK_FOR_THUMB_LETTER (INTVAL (operands[1]), 'K')"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 0) (ashift:SI (match_dup 0) (match_dup 2)))]
  "
  {
    unsigned HOST_WIDE_INT val = INTVAL (operands[1]);
    unsigned HOST_WIDE_INT mask = 0xff;
    int i;
    
    for (i = 0; i < 25; i++)
      if ((val & (mask << i)) == val)
        break;

    if (i == 0)
      FAIL;

    operands[1] = GEN_INT (val >> i);
    operands[2] = GEN_INT (i);
  }"
)


;; When generating pic, we need to load the symbol offset into a register.
;; So that the optimizer does not confuse this with a normal symbol load
;; we use an unspec.  The offset will be loaded from a constant pool entry,
;; since that is the only type of relocation we can use.

;; The rather odd constraints on the following are to force reload to leave
;; the insn alone, and to force the minipool generation pass to then move
;; the GOT symbol to memory.

(define_insn "pic_load_addr_thumb"
  [(set (match_operand:SI 0 "s_register_operand" "=l")
	(unspec:SI [(match_operand:SI 1 "" "mX")] UNSPEC_PIC_SYM))]
  "TARGET_THUMB && flag_pic"
  "ldr\\t%0, %1"

)

;; This variant is used for AOF assembly, since it needs to mention the
;; pic register in the rtl.


(define_insn "*pic_load_addr_based_insn"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(unspec:SI [(match_operand 1 "" "")
		    (match_operand 2 "s_register_operand" "r")]
		   UNSPEC_PIC_SYM))]
  "TARGET_EITHER && flag_pic && operands[2] == pic_offset_table_rtx"
  "*
#ifdef AOF_ASSEMBLER
  operands[1] = aof_pic_entry (operands[1]);
#endif
  output_asm_insn (\"ldr%?\\t%0, %a1\", operands);
  return \"\";
  "

)

(define_insn "pic_add_dot_plus_four"
  [(set (match_operand:SI 0 "register_operand" "+r")
	(plus:SI (match_dup 0) (const (plus:SI (pc) (const_int 4)))))
   (use (label_ref (match_operand 1 "" "")))]
  "TARGET_THUMB && flag_pic"
  "*
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, \"L\",
			     CODE_LABEL_NUMBER (operands[1]));
  return \"add\\t%0, %|pc\";
  "
 
)


(define_expand "builtin_setjmp_receiver"
  [(label_ref (match_operand 0 "" ""))]
  "flag_pic"
  "
{
  arm_finalize_pic (0);
  DONE;
}")

;; If copying one reg to another we can set the condition codes according to
;; its value.  Such a move is common after a return from subroutine and the
;; result is being tested against zero.


;; Subroutine to store a half word from a register into memory.
;; Operand 0 is the source register (HImode)
;; Operand 1 is the destination address in a register (SImode)

;; In both this routine and the next, we must be careful not to spill
;; a memory address of reg+large_const into a separate PLUS insn, since this
;; can generate unrecognizable rtl.

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
 
    {
      if (!no_new_pseudos)
        {
          if (GET_CODE (operands[0]) != REG)
	    operands[1] = force_reg (HImode, operands[1]);

          /* ??? We shouldn't really get invalid addresses here, but this can
	     happen if we are passed a SP (never OK for HImode/QImode) or 
	     virtual register (rejected by GO_IF_LEGITIMATE_ADDRESS for 
	     HImode/QImode) relative address.  */
          /* ??? This should perhaps be fixed elsewhere, for instance, in
	     fixup_stack_1, by checking for other kinds of invalid addresses,
	     e.g. a bare reference to a virtual register.  This may confuse the
	     alpha though, which must handle this case differently.  */
          if (GET_CODE (operands[0]) == MEM
	      && !memory_address_p (GET_MODE (operands[0]),
				    XEXP (operands[0], 0)))
	    {
	      rtx temp = copy_to_reg (XEXP (operands[0], 0));
	      operands[0] = change_address (operands[0], VOIDmode, temp);
	    }
   
          if (GET_CODE (operands[1]) == MEM
	      && !memory_address_p (GET_MODE (operands[1]),
				    XEXP (operands[1], 0)))
	    {
	      rtx temp = copy_to_reg (XEXP (operands[1], 0));
	      operands[1] = change_address (operands[1], VOIDmode, temp);
	    }
        }
      /* Handle loading a large integer during reload */
      else if (GET_CODE (operands[1]) == CONST_INT
	        && !CONST_OK_FOR_THUMB_LETTER (INTVAL (operands[1]), 'I'))
        {
          /* Writing a constant to memory needs a scratch, which should
	     be handled with SECONDARY_RELOADs.  */
          if (GET_CODE (operands[0]) != REG)
	    abort ();

          operands[0] = gen_rtx (SUBREG, SImode, operands[0], 0);
          emit_insn (gen_movsi (operands[0], operands[1]));
          DONE;
        }
    }
  "
)

(define_insn "*thumb_movhi_insn"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=l,l, m,*r,*h,l")
	(match_operand:HI 1 "general_operand"       "l,mn,l,*h,*r,I"))]
  "TARGET_THUMB
   && (   register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode))"
  "*
  switch (which_alternative)
    {
    case 0: return \"add	%0, %1, #0\";
    case 2: return \"strh	%1, %0\";
    case 3: return \"mov	%0, %1\";
    case 4: return \"mov	%0, %1\";
    case 5: return \"mov	%0, %1\";
    default: abort ();
    case 1:
      /* The stack pointer can end up being taken as an index register.
          Catch this case here and deal with it.  */
      if (GET_CODE (XEXP (operands[1], 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (operands[1], 0), 0)) == REG
	  && REGNO    (XEXP (XEXP (operands[1], 0), 0)) == SP_REGNUM)
        {
	  rtx ops[2];
          ops[0] = operands[0];
          ops[1] = XEXP (XEXP (operands[1], 0), 0);
      
          output_asm_insn (\"mov	%0, %1\", ops);

          XEXP (XEXP (operands[1], 0), 0) = operands[0];
    
	}
      return \"ldrh	%0, %1\";
    }"

)



(define_insn "thumb_movhi_clobber"
  [(set (match_operand:HI     0 "memory_operand"   "=m")
	(match_operand:HI     1 "register_operand" "l"))
   (clobber (match_operand:SI 2 "register_operand" "=&l"))]
  "TARGET_THUMB"
  "*
  abort ();"
)
	
;; We use a DImode scratch because we may occasionally need an additional
;; temporary if the address isn't offsettable -- push_reload doesn't seem
;; to take any notice of the "o" constraints on reload_memory_operand operand.
;;(define_expand "reload_outhi"
;;  [(parallel [(match_operand:HI 0 "arm_reload_memory_operand" "=o")
;;	      (match_operand:HI 1 "s_register_operand"        "r")
;;	      (match_operand:DI 2 "s_register_operand"        "=&l")])]
;;  "TARGET_EITHER"
;;  "if (TARGET_ARM)
;;     arm_reload_out_hi (operands);
;;   else
;;     thumb_reload_out_hi (operands);
;;  DONE;
;;  "
;;)



(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
        (match_operand:QI 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
    {
      if (!no_new_pseudos)
        {
          if (GET_CODE (operands[0]) != REG)
	    operands[1] = force_reg (QImode, operands[1]);

          /* ??? We shouldn't really get invalid addresses here, but this can
	     happen if we are passed a SP (never OK for HImode/QImode) or
	     virtual register (rejected by GO_IF_LEGITIMATE_ADDRESS for
	     HImode/QImode) relative address.  */
          /* ??? This should perhaps be fixed elsewhere, for instance, in
	     fixup_stack_1, by checking for other kinds of invalid addresses,
	     e.g. a bare reference to a virtual register.  This may confuse the
	     alpha though, which must handle this case differently.  */
          if (GET_CODE (operands[0]) == MEM
	      && !memory_address_p (GET_MODE (operands[0]),
		  		     XEXP (operands[0], 0)))
	    {
	      rtx temp = copy_to_reg (XEXP (operands[0], 0));
	      operands[0] = change_address (operands[0], VOIDmode, temp);
	    }
          if (GET_CODE (operands[1]) == MEM
	      && !memory_address_p (GET_MODE (operands[1]),
				    XEXP (operands[1], 0)))
	    {
	       rtx temp = copy_to_reg (XEXP (operands[1], 0));
	       operands[1] = change_address (operands[1], VOIDmode, temp);
	    }
        }
      /* Handle loading a large integer during reload */
      else if (GET_CODE (operands[1]) == CONST_INT
	       && !CONST_OK_FOR_LETTER_P (INTVAL (operands[1]), 'I'))
        {
          /* Writing a constant to memory needs a scratch, which should
	     be handled with SECONDARY_RELOADs.  */
          if (GET_CODE (operands[0]) != REG)
	    abort ();

          operands[0] = gen_rtx (SUBREG, SImode, operands[0], 0);
          emit_insn (gen_movsi (operands[0], operands[1]));
          DONE;
       }
    }
  "
)



(define_insn "*thumb_movqi_insn"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=l,l,m,*r,*h,l")
	(match_operand:QI 1 "general_operand"      "l, m,l,*h,*r,I"))]
  "TARGET_THUMB
   && (   register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode))"
  "@
   add\\t%0, %1, #0
   ldrb\\t%0, %1
   strb\\t%1, %0
   mov\\t%0, %1
   mov\\t%0, %1
   mov\\t%0, %1"

)

(define_expand "movsf"
  [(set (match_operand:SF 0 "general_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
    {
      if (!no_new_pseudos)
        {
           if (GET_CODE (operands[0]) != REG)
	     operands[1] = force_reg (SFmode, operands[1]);
        }
    }
  "
)



;;; ??? This should have alternatives for constants.
(define_insn "*thumb_movsf_insn"
  [(set (match_operand:SF     0 "nonimmediate_operand" "=l,l,>,l, m,*r,*h")
	(match_operand:SF     1 "general_operand"      "l, >,l,mF,l,*h,*r"))]
  "TARGET_THUMB
   && (   register_operand (operands[0], SFmode) 
       || register_operand (operands[1], SFmode))"
  "@
   add\\t%0, %1, #0
   ldmia\\t%1, {%0}
   stmia\\t%0, {%1}
   ldr\\t%0, %1
   str\\t%1, %0
   mov\\t%0, %1
   mov\\t%0, %1"

)

(define_expand "movdf"
  [(set (match_operand:DF 0 "general_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
    {
      if (!no_new_pseudos)
        {
          if (GET_CODE (operands[0]) != REG)
	    operands[1] = force_reg (DFmode, operands[1]);
        }
    }
  "
)

;; Reloading a df mode value stored in integer regs to memory can require a
;; scratch reg.

;; Software floating point version.  This is essentially the same as movdi.
;; Do not use `f' as a constraint to prevent reload from ever trying to use
;; an `f' reg.

;;; ??? This should have alternatives for constants.
;;; ??? This was originally identical to the movdi_insn pattern.
;;; ??? The 'F' constraint looks funny, but it should always be replaced by
;;; thumb_reorg with a memory reference.
(define_insn "*thumb_movdf_insn"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=l,l,>,l, m,*r")
	(match_operand:DF 1 "general_operand"      "l, >,l,mF,l,*r"))]
  "TARGET_THUMB
   && (   register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  "*
  switch (which_alternative)
    {
    default:
    case 0:
      if (REGNO (operands[1]) == REGNO (operands[0]) + 1)
	return \"add\\t%0, %1, #0\;add\\t%H0, %H1, #0\";
      return \"add\\t%H0, %H1, #0\;add\\t%0, %1, #0\";
    case 1:
      return \"ldmia\\t%1, {%0, %H0}\";
    case 2:
      return \"stmia\\t%0, {%1, %H1}\";
    case 3:
      return thumb_load_double_from_address (operands);
    case 4:
      operands[2] = gen_rtx (MEM, SImode,
			     plus_constant (XEXP (operands[0], 0), 4));
      output_asm_insn (\"str\\t%1, %0\;str\\t%H1, %2\", operands);
      return \"\";
    case 5:
      if (REGNO (operands[1]) == REGNO (operands[0]) + 1)
	return \"mov\\t%0, %1\;mov\\t%H0, %H1\";
      return \"mov\\t%H0, %H1\;mov\\t%0, %1\";
    }
  "

)


;; Even when the XFmode patterns aren't enabled, we enable this after
;; reloading so that we can push floating point registers in the prologue.


;; load- and store-multiple insns
;; The arm can load/store any set of registers, provided that they are in
;; ascending order; but that is beyond GCC so stick with what it knows.

;; Load multiple with write-back


;; Ordinary load multiple


;; Move a block of memory if it is word aligned and MORE than 2 words long.
;; We could let this apply for blocks of less than this, but it clobbers so
;; many registers that there is then probably a better way.

(define_expand "movstrqi"
  [(match_operand:BLK 0 "general_operand" "")
   (match_operand:BLK 1 "general_operand" "")
   (match_operand:SI 2 "const_int_operand" "")
   (match_operand:SI 3 "const_int_operand" "")]
  "TARGET_EITHER"
  "
    {
      if (   INTVAL (operands[3]) != 4
          || INTVAL (operands[2]) > 48)
        FAIL;

      thumb_expand_movstrqi (operands);
      DONE;
    }
  "
)

;; Thumb block-move insns

(define_insn "movmem12b"
  [(set (mem:SI (match_operand:SI 2 "register_operand" "0"))
	(mem:SI (match_operand:SI 3 "register_operand" "1")))
   (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	(mem:SI (plus:SI (match_dup 3) (const_int 4))))
   (set (mem:SI (plus:SI (match_dup 2) (const_int 8)))
	(mem:SI (plus:SI (match_dup 3) (const_int 8))))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 2) (const_int 12)))
   (set (match_operand:SI 1 "register_operand" "=r")
	(plus:SI (match_dup 3) (const_int 12)))
   (clobber (match_scratch:SI 4 "=&l"))
   (clobber (match_scratch:SI 5 "=&l"))
   (clobber (match_scratch:SI 6 "=&l"))]
  "TARGET_THUMB"
  "* return thumb_output_move_mem_multiple (3, operands);"

)

(define_insn "movmem8b"
  [(set (mem:SI (match_operand:SI 2 "register_operand" "0"))
	(mem:SI (match_operand:SI 3 "register_operand" "1")))
   (set (mem:SI (plus:SI (match_dup 2) (const_int 4)))
	(mem:SI (plus:SI (match_dup 3) (const_int 4))))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 2) (const_int 8)))
   (set (match_operand:SI 1 "register_operand" "=r")
	(plus:SI (match_dup 3) (const_int 8)))
   (clobber (match_scratch:SI 4 "=&l"))
   (clobber (match_scratch:SI 5 "=&l"))]
  "TARGET_THUMB"
  "* return thumb_output_move_mem_multiple (2, operands);"

)


;; Compare & branch insns
;; The range calcualations are based as follows:
;; For forward branches, the address calculation returns the address of
;; the next instruction.  This is 2 beyond the branch instruction.
;; For backward branches, the address calculation returns the address of
;; the first instruction in this pattern (cmp).  This is 2 before the branch
;; instruction for the shortest sequence, and 4 before the branch instruction
;; if we have to jump around an unconditional branch.
;; To the basic branch range the PC offset must be added (this is +4).
;; So for forward branches we have 
;;   (pos_range - pos_base_offs + pc_offs) = (pos_range - 2 + 4).
;; And for backward branches we have 
;;   (neg_range - neg_base_offs + pc_offs) = (neg_range - (-2 or -4) + 4).
;;
;; For a 'b'       pos_range = 2046, neg_range = -2048 giving (-2040->2048).
;; For a 'b<cond>' pos_range = 254,  neg_range = -256  giving (-250 ->256).

(define_insn "cbranchsi4"
  [(set (pc)
	(if_then_else
	    (match_operator                    0 "arm_comparison_operator"
	                    [(match_operand:SI 1 "register_operand"   "l,r")
			     (match_operand:SI 2 "nonmemory_operand" "rI,r")])
	    (label_ref       (match_operand    3 "" ""))
	    (pc)))]
  "TARGET_THUMB"
  "*
  output_asm_insn (\"cmp\\t%1, %2\", operands);
  switch (get_attr_length (insn))
    {
    case 4:  return \"b%d0\\t%l3\";
    case 6:  return \"b%D0\\t.LCB%=\;b\\t%l3\\t%@long jump\\n.LCB%=:\";
    default: return \"b%D0\\t.LCB%=\;bl\\t%l3\\t%@far jump\\n.LCB%=:\";
    }
  "
  [(set (attr "far_jump")
        (if_then_else
	    (eq_attr "length" "8")
	    (const_string "yes")
            (const_string "no")))
   (set (attr "length") 
        (if_then_else
	    (and (ge (minus (match_dup 3) (pc)) (const_int -250))
	         (le (minus (match_dup 3) (pc)) (const_int 256)))
	    (const_int 4)
	    (if_then_else
	        (and (ge (minus (match_dup 3) (pc)) (const_int -2040))
		     (le (minus (match_dup 3) (pc)) (const_int 2048)))
		(const_int 6)
		(const_int 8))))]
)

(define_insn "*negated_cbranchsi4"
  [(set (pc)
	(if_then_else
	 (match_operator             0 "arm_comparison_operator"
	  [(match_operand:SI         1 "register_operand"  "l")
	   (neg:SI (match_operand:SI 2 "nonmemory_operand" "l"))])
	 (label_ref (match_operand   3 "" ""))
	 (pc)))]
  "TARGET_THUMB"
  "*
  output_asm_insn (\"cmn\\t%1, %2\", operands);
  switch (get_attr_length (insn))
    {
    case 4:  return \"b%d0\\t%l3\";
    case 6:  return \"b%D0\\t.LCB%=\;b\\t%l3\\t%@long jump\\n.LCB%=:\";
    default: return \"b%D0\\t.LCB%=\;bl\\t%l3\\t%@far jump\\n.LCB%=:\";
    }
  "
  [(set (attr "far_jump")
        (if_then_else
	    (eq_attr "length" "8")
	    (const_string "yes")
            (const_string "no")))
   (set (attr "length") 
        (if_then_else
	    (and (ge (minus (match_dup 3) (pc)) (const_int -250))
	         (le (minus (match_dup 3) (pc)) (const_int 256)))
	    (const_int 4)
	    (if_then_else
	        (and (ge (minus (match_dup 3) (pc)) (const_int -2040))
		     (le (minus (match_dup 3) (pc)) (const_int 2048)))
		(const_int 6)
		(const_int 8))))]
)


;; Comparison and test insns




;; Jump and linkage insns

(define_expand "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  "TARGET_EITHER"
  ""
)

(define_insn "*thumb_jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  "TARGET_THUMB"
  "*
  if (get_attr_length (insn) == 2)
    return \"b\\t%l0\";
  return \"bl\\t%l0\\t%@ far jump\";
  "
  [(set (attr "far_jump")
        (if_then_else
	    (eq_attr "length" "4")
	    (const_string "yes")
	    (const_string "no")))
   (set (attr "length") 
        (if_then_else
	    (and (ge (minus (match_dup 0) (pc)) (const_int -2048))
		 (le (minus (match_dup 0) (pc)) (const_int 2044)))
  	    (const_int 2)
	    (const_int 4)))]
)

(define_expand "call"
  [(parallel [(call (match_operand 0 "memory_operand" "")
	            (match_operand 1 "general_operand" ""))
	      (use (match_operand 2 "" ""))
	      (clobber (reg:SI LR_REGNUM))])]
  "TARGET_EITHER"
  "
  {
    rtx callee;
    
    /* In an untyped call, we can get NULL for operand 2.  */
    if (operands[2] == NULL_RTX)
      operands[2] = const0_rtx;
      
    /* This is to decide if we should generate indirect calls by loading the
       32 bit address of the callee into a register before performing the
       branch and link.  operand[2] encodes the long_call/short_call
       attribute of the function being called.  This attribute is set whenever
       __attribute__((long_call/short_call)) or #pragma long_call/no_long_call
       is used, and the short_call attribute can also be set if function is
       declared as static or if it has already been defined in the current
       compilation unit.  See arm.c and arm.h for info about this.  The third
       parameter to arm_is_longcall_p is used to tell it which pattern
       invoked it.  */
    callee  = XEXP (operands[0], 0);
    
    if (GET_CODE (callee) != REG
       && arm_is_longcall_p (operands[0], INTVAL (operands[2]), 0))
      XEXP (operands[0], 0) = force_reg (Pmode, callee);
  }"
)


(define_insn "*call_indirect"
  [(call (mem:SI (match_operand:SI 0 "register_operand" "l*r"))
	 (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB"
  "*
  {
    if (TARGET_CALLER_INTERWORKING)
      return \"bl\\t%__interwork_call_via_%0\";
    else
      return \"bl\\t%__call_via_%0\";
  }"

)

(define_insn "*call_value_indirect"
  [(set (match_operand 0 "" "=l")
	(call (mem:SI (match_operand:SI 1 "register_operand" "l*r"))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB"
  "*
  {
    if (TARGET_CALLER_INTERWORKING)
      return \"bl\\t%__interwork_call_via_%1\";
    else
      return \"bl\\t%__call_via_%1\";
  }"

)

(define_expand "call_value"
  [(parallel [(set (match_operand       0 "" "")
	           (call (match_operand 1 "memory_operand" "")
		         (match_operand 2 "general_operand" "")))
	      (use (match_operand 3 "" ""))
	      (clobber (reg:SI LR_REGNUM))])]
  "TARGET_EITHER"
  "
  {
    rtx callee = XEXP (operands[1], 0);
    
    /* In an untyped call, we can get NULL for operand 2.  */
    if (operands[3] == 0)
      operands[3] = const0_rtx;
      
    /* See the comment in define_expand \"call\".  */
    if (GET_CODE (callee) != REG
	&& arm_is_longcall_p (operands[1], INTVAL (operands[3]), 0))
      XEXP (operands[1], 0) = force_reg (Pmode, callee);
  }"
)




(define_insn "*call_insn"
  [(call (mem:SI (match_operand:SI 0 "" "X"))
	 (match_operand:SI 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB
   && operands[2] == const0_rtx && (GET_CODE (operands[0]) == SYMBOL_REF)"
  "bl\\t%a0"

)

(define_insn "*call_value_insn"
  [(set (match_operand 0 "register_operand" "=l")
	(call (mem:SI (match_operand 1 "" "X"))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  "TARGET_THUMB
   && operands[3] == const0_rtx && (GET_CODE (operands[1]) == SYMBOL_REF)"
  "bl\\t%a1"

)


;; UNSPEC_VOLATILE is considered to use and clobber all hard registers and
;; all of memory.  This blocks insns from being moved across this point.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] VUNSPEC_BLOCKAGE)]
  "TARGET_EITHER"
  ""

)

(define_expand "indirect_jump"
  [(set (pc)
	(match_operand:SI 0 "s_register_operand" ""))]
  "TARGET_EITHER"
  ""
)


;; Although not supported by the define_expand above,
;; cse/combine may generate this form.


(define_insn "*thumb_indirect_jump"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "l*r"))]
  "TARGET_THUMB"
  "mov\\tpc, %0"

)


;; Misc insns

(define_insn "nop"
  [(const_int 0)]
  "TARGET_EITHER"
  "nop"
  [(set (attr "length")
	(if_then_else (eq_attr "is_thumb" "yes")
		      (const_int 2)
		      (const_int 4)))]
)


;; Patterns to allow combination of arithmetic, cond code and shifts



;; These variants of the above insns can occur if the first operand is the
;; frame pointer and we eliminate that.  This is a kludge, but there doesn't
;; seem to be a way around it.  Most of the predicates have to be null
;; because the format can be generated part way through reload, so
;; if we don't match it as soon as it becomes available, reload doesn't know
;; how to reload pseudos that haven't got hard registers; the constraints will
;; sort everything out.


;;(define_expand "prologue"
;;  [(clobber (const_int 0))]
;;  "TARGET_EITHER"
;;  "if (TARGET_ARM)
;;     arm_expand_prologue ();
;;   else
;;     thumb_expand_prologue ();
;;  DONE;
;;  "
;;)

(define_expand "epilogue"
  [(unspec_volatile [(return)] VUNSPEC_EPILOGUE)]
  "TARGET_EITHER"
  "
  if (TARGET_THUMB)
    thumb_expand_epilogue ();
  else if (USE_RETURN_INSN (FALSE))
    {
      emit_jump_insn (gen_return ());
      DONE;
    }
  emit_jump_insn (gen_rtx_UNSPEC_VOLATILE (VOIDmode,
	gen_rtvec (1,
		gen_rtx_RETURN (VOIDmode)),
	VUNSPEC_EPILOGUE));
  DONE;
  "
)


(define_insn "*epilogue_insns"
  [(unspec_volatile [(return)] VUNSPEC_EPILOGUE)]
  "TARGET_EITHER"
  "*
    return thumb_unexpanded_epilogue ();
  "
  ; Length is absolute worst case

)

(define_expand "eh_epilogue"
  [(use (match_operand:SI 0 "register_operand" "r"))
   (use (match_operand:SI 1 "register_operand" "r"))
   (use (match_operand:SI 2 "register_operand" "r"))]
  "TARGET_EITHER"
  "
  {
    cfun->machine->eh_epilogue_sp_ofs = operands[1];
    if (GET_CODE (operands[2]) != REG || REGNO (operands[2]) != 2)
      {
	rtx ra = gen_rtx_REG (Pmode, 2);

	emit_move_insn (ra, operands[2]);
	operands[2] = ra;
      }
    /* This is a hack -- we may have crystalized the function type too
       early.  */
    cfun->machine->func_type = 0;
  }"
)

;; This split is only used during output to reduce the number of patterns
;; that need assembler instructions adding to them.  We allowed the setting
;; of the conditions to be implicit during rtl generation so that
;; the conditional compare patterns would work.  However this conflicts to
;; some extent with the conditional data operations, so we have to split them
;; up again here.


;; Similarly for the floating point registers

;; Special patterns for dealing with the constant pool

(define_insn "align_4"
  [(unspec_volatile [(const_int 0)] VUNSPEC_ALIGN)]
  "TARGET_EITHER"
  "*
  assemble_align (32);
  return \"\";
  "
)

(define_insn "consttable_end"
  [(unspec_volatile [(const_int 0)] VUNSPEC_POOL_END)]
  "TARGET_EITHER"
  "*
  making_const_table = FALSE;
  return \"\";
  "
)

(define_insn "consttable_1"
  [(unspec_volatile [(match_operand 0 "" "")] VUNSPEC_POOL_1)]
  "TARGET_THUMB"
  "*
  making_const_table = TRUE;
  assemble_integer (operands[0], 1, 1);
  assemble_zeros (3);
  return \"\";
  "
 
)

(define_insn "consttable_2"
  [(unspec_volatile [(match_operand 0 "" "")] VUNSPEC_POOL_2)]
  "TARGET_THUMB"
  "*
  making_const_table = TRUE;
  assemble_integer (operands[0], 2, 1);
  assemble_zeros (2);
  return \"\";
  "
 
)

(define_insn "consttable_4"
  [(unspec_volatile [(match_operand 0 "" "")] VUNSPEC_POOL_4)]
  "TARGET_EITHER"
  "*
  {
    making_const_table = TRUE;
    switch (GET_MODE_CLASS (GET_MODE (operands[0])))
      {
      case MODE_FLOAT:
      {
        union real_extract u;
        memcpy (&u, &CONST_DOUBLE_LOW (operands[0]), sizeof u);
        assemble_real (u.d, GET_MODE (operands[0]));
        break;
      }
      default:
        assemble_integer (operands[0], 4, 1);
        break;
      }
    return \"\";
  }"
 
)

(define_insn "consttable_8"
  [(unspec_volatile [(match_operand 0 "" "")] VUNSPEC_POOL_8)]
  "TARGET_EITHER"
  "*
  {
    making_const_table = TRUE;
    switch (GET_MODE_CLASS (GET_MODE (operands[0])))
      {
       case MODE_FLOAT:
        {
          union real_extract u;
          memcpy (&u, &CONST_DOUBLE_LOW (operands[0]), sizeof u);
          assemble_real (u.d, GET_MODE (operands[0]));
          break;
        }
      default:
        assemble_integer (operands[0], 8, 1);
        break;
      }
    return \"\";
  }"

)

;; Miscellaneous Thumb patterns

(define_insn "tablejump"
  [(set (pc) (match_operand:SI 0 "register_operand" "l*r"))
   (use (label_ref (match_operand 1 "" "")))]
  "TARGET_THUMB"
  "mov	pc, %0"
 
)

;; V5 Instructions,

;; General predication pattern


