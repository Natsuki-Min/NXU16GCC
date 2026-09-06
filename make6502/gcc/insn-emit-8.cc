/* Generated automatically by the program `genemit'
from the machine description file `md'.  */

#define IN_TARGET_CODE 1
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "predict.h"
#include "tree.h"
#include "rtl.h"
#include "alias.h"
#include "varasm.h"
#include "stor-layout.h"
#include "calls.h"
#include "memmodel.h"
#include "tm_p.h"
#include "flags.h"
#include "insn-config.h"
#include "expmed.h"
#include "dojump.h"
#include "explow.h"
#include "emit-rtl.h"
#include "stmt.h"
#include "expr.h"
#include "insn-codes.h"
#include "optabs.h"
#include "dfp.h"
#include "output.h"
#include "recog.h"
#include "df.h"
#include "resource.h"
#include "reload.h"
#include "diagnostic-core.h"
#include "regs.h"
#include "tm-constrs.h"
#include "ggc.h"
#include "target.h"

/* ../../gcc/config/6502/6502.md:2430 */
rtx
gen_call_indirect (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_CALL (VOIDmode,
	gen_rtx_MEM (QImode,
	operand0),
	operand1),
		gen_rtx_USE (VOIDmode,
	const0_rtx)));
}

/* ../../gcc/config/6502/6502.md:2452 */
rtx
gen_call_value_sym (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	gen_rtx_CALL (VOIDmode,
	gen_rtx_MEM (QImode,
	operand1),
	operand2)),
		gen_rtx_USE (VOIDmode,
	const0_rtx)));
}

/* ../../gcc/config/6502/6502.md:2461 */
rtx
gen_call_indirect_sym (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	gen_rtx_CALL (VOIDmode,
	gen_rtx_MEM (QImode,
	operand1),
	operand2)),
		gen_rtx_USE (VOIDmode,
	const0_rtx)));
}

/* ../../gcc/config/6502/6502.md:2478 */
rtx
gen_m65x_return (void)
{
  return ret_rtx;
}

/* ../../gcc/config/6502/6502.md:2500 */
rtx
gen_m65x_savestack_s7s0_sp (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED)
{
  return gen_rtx_UNSPEC_VOLATILE (VOIDmode,
	gen_rtvec (2,
		operand0,
		operand1),
	0);
}

/* ../../gcc/config/6502/6502.md:2508 */
rtx
gen_m65x_savestack_s7s0 (void)
{
  return gen_rtx_UNSPEC_VOLATILE (VOIDmode,
	gen_rtvec (1,
		const0_rtx),
	2);
}

/* ../../gcc/config/6502/6502.md:2514 */
rtx
gen_m65x_restorestack_s7s0_sp_rts (void)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_UNSPEC_VOLATILE (VOIDmode,
	gen_rtvec (1,
		const0_rtx),
	1),
		ret_rtx));
}

/* ../../gcc/config/6502/6502.md:2521 */
rtx
gen_m65x_restorestack_s7s0_rts (void)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_UNSPEC_VOLATILE (VOIDmode,
	gen_rtvec (1,
		const0_rtx),
	3),
		ret_rtx));
}

/* ../../gcc/config/6502/6502.md:107 */
rtx
gen_movhi (rtx operand0,
	rtx operand1)
{
  rtx_insn *_val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 111 "../../gcc/config/6502/6502.md"
{
  int modesize, i;
  bool need_clobber = false;
  rtx seq;
  
  /*dump_value_slim (stderr, operands[0], 0);
  fprintf (stderr, " <-- ");
  dump_value_slim (stderr, operands[1], 0);
  fprintf (stderr, "\n");*/
  
  if (HImode == HImode && can_create_pseudo_p ()
      && sym_const_operand (operands[1], HImode))
    {
      rtx op0lo = gen_lowpart (QImode, operands[0]), dstlo;
      rtx op0hi = gen_highpart_mode (QImode, HImode, operands[0]), dsthi;

      dstlo = op0lo;
      dsthi = op0hi;

      if (!hard_reg_operand (dstlo, QImode))
        dstlo = gen_reg_rtx (QImode);
      if (!hard_reg_operand (dsthi, QImode))
        dsthi = gen_reg_rtx (QImode);

      emit_move_insn (dstlo, m65x_gen_subreg (QImode, operands[1], HImode, 0));
      emit_move_insn (dsthi, m65x_gen_subreg (QImode, operands[1], HImode, 1));

      if (dstlo != op0lo)
        emit_move_insn (op0lo, dstlo);
      if (dsthi != op0hi)
        emit_move_insn (op0hi, dsthi);

      DONE;
    }
  else if (!lra_in_progress && can_create_pseudo_p ())
    {
      rtx dest = operands[0];
      modesize = GET_MODE_SIZE (HImode);

      /* We can only load symbol_ref/label_ref to an actual register.  */
      if (HImode == HImode
	  && !hard_reg_operand (operands[0], HImode)
	  && sym_const_operand (operands[1], HImode))
	operands[1] = force_reg (HImode, operands[1]);

      /* The below code is adapted from expr.c:emit_move_multi_word.  */

      if (reload_in_progress && MEM_P (operands[0]))
	{
	  rtx inner = find_replacement (&XEXP (operands[0], 0));
	  if (inner != XEXP (operands[0], 0))
            operands[0] = replace_equiv_address_nv (operands[0], inner);
	}

      if (reload_in_progress && MEM_P (operands[1]))
	{
	  rtx inner = find_replacement (&XEXP (operands[1], 0));
	  if (inner != XEXP (operands[1], 0))
            operands[1] = replace_equiv_address_nv (operands[1], inner);
	}

      if (!rtx_equal_p (operands[0], operands[1])
	  && REG_P (operands[0])
	  && reg_overlap_mentioned_p (operands[0], operands[1]))
	dest = gen_reg_rtx (HImode);

      start_sequence ();

      for (i = 0; i < modesize; i++)
	{
	  rtx xpart = operand_subword (dest, i, 1, HImode);
	  rtx ypart;

	 /* if (undefined_operand_subword_p (operands[1], i))
            continue;*/

	  ypart = operand_subword (operands[1], i, 1, HImode);

	  if (ypart == 0 && CONSTANT_P (operands[1]))
            {
	      operands[1] = use_anchored_address (force_const_mem (HImode,
						  operands[1]));
	      ypart = operand_subword (operands[1], i, 1, HImode);
	    }
	  else if (ypart == 0)
            ypart = operand_subword_force (operands[1], i, HImode);

	  gcc_assert (xpart && ypart);

	  need_clobber |= (GET_CODE (xpart) == SUBREG);

	  emit_move_insn (xpart, ypart);
	}

      seq = get_insns ();
      end_sequence ();

      if (dest != operands[1]
	  && ! (lra_in_progress || reload_in_progress || reload_completed)
	  && need_clobber)
	emit_clobber (dest);

      emit_insn (seq);

      if (dest != operands[0])
        emit_move_insn (operands[0], dest);

      DONE;
    }

  if (memory_operand (operands[0], HImode)
      && memory_operand (operands[1], HImode))
    operands[1] = force_reg (HImode, operands[1]);
}
#undef DONE
#undef FAIL
    operand0 = operands[0];
    (void) operand0;
    operand1 = operands[1];
    (void) operand1;
  }
  emit_insn (gen_rtx_SET (operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:107 */
rtx
gen_movsi (rtx operand0,
	rtx operand1)
{
  rtx_insn *_val = 0;
  start_sequence ();
  {
    rtx operands[2];
    operands[0] = operand0;
    operands[1] = operand1;
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 111 "../../gcc/config/6502/6502.md"
{
  int modesize, i;
  bool need_clobber = false;
  rtx seq;
  
  /*dump_value_slim (stderr, operands[0], 0);
  fprintf (stderr, " <-- ");
  dump_value_slim (stderr, operands[1], 0);
  fprintf (stderr, "\n");*/
  
  if (SImode == HImode && can_create_pseudo_p ()
      && sym_const_operand (operands[1], HImode))
    {
      rtx op0lo = gen_lowpart (QImode, operands[0]), dstlo;
      rtx op0hi = gen_highpart_mode (QImode, HImode, operands[0]), dsthi;

      dstlo = op0lo;
      dsthi = op0hi;

      if (!hard_reg_operand (dstlo, QImode))
        dstlo = gen_reg_rtx (QImode);
      if (!hard_reg_operand (dsthi, QImode))
        dsthi = gen_reg_rtx (QImode);

      emit_move_insn (dstlo, m65x_gen_subreg (QImode, operands[1], HImode, 0));
      emit_move_insn (dsthi, m65x_gen_subreg (QImode, operands[1], HImode, 1));

      if (dstlo != op0lo)
        emit_move_insn (op0lo, dstlo);
      if (dsthi != op0hi)
        emit_move_insn (op0hi, dsthi);

      DONE;
    }
  else if (!lra_in_progress && can_create_pseudo_p ())
    {
      rtx dest = operands[0];
      modesize = GET_MODE_SIZE (SImode);

      /* We can only load symbol_ref/label_ref to an actual register.  */
      if (SImode == HImode
	  && !hard_reg_operand (operands[0], SImode)
	  && sym_const_operand (operands[1], SImode))
	operands[1] = force_reg (SImode, operands[1]);

      /* The below code is adapted from expr.c:emit_move_multi_word.  */

      if (reload_in_progress && MEM_P (operands[0]))
	{
	  rtx inner = find_replacement (&XEXP (operands[0], 0));
	  if (inner != XEXP (operands[0], 0))
            operands[0] = replace_equiv_address_nv (operands[0], inner);
	}

      if (reload_in_progress && MEM_P (operands[1]))
	{
	  rtx inner = find_replacement (&XEXP (operands[1], 0));
	  if (inner != XEXP (operands[1], 0))
            operands[1] = replace_equiv_address_nv (operands[1], inner);
	}

      if (!rtx_equal_p (operands[0], operands[1])
	  && REG_P (operands[0])
	  && reg_overlap_mentioned_p (operands[0], operands[1]))
	dest = gen_reg_rtx (SImode);

      start_sequence ();

      for (i = 0; i < modesize; i++)
	{
	  rtx xpart = operand_subword (dest, i, 1, SImode);
	  rtx ypart;

	 /* if (undefined_operand_subword_p (operands[1], i))
            continue;*/

	  ypart = operand_subword (operands[1], i, 1, SImode);

	  if (ypart == 0 && CONSTANT_P (operands[1]))
            {
	      operands[1] = use_anchored_address (force_const_mem (SImode,
						  operands[1]));
	      ypart = operand_subword (operands[1], i, 1, SImode);
	    }
	  else if (ypart == 0)
            ypart = operand_subword_force (operands[1], i, SImode);

	  gcc_assert (xpart && ypart);

	  need_clobber |= (GET_CODE (xpart) == SUBREG);

	  emit_move_insn (xpart, ypart);
	}

      seq = get_insns ();
      end_sequence ();

      if (dest != operands[1]
	  && ! (lra_in_progress || reload_in_progress || reload_completed)
	  && need_clobber)
	emit_clobber (dest);

      emit_insn (seq);

      if (dest != operands[0])
        emit_move_insn (operands[0], dest);

      DONE;
    }

  if (memory_operand (operands[0], SImode)
      && memory_operand (operands[1], SImode))
    operands[1] = force_reg (SImode, operands[1]);
}
#undef DONE
#undef FAIL
    operand0 = operands[0];
    (void) operand0;
    operand1 = operands[1];
    (void) operand1;
  }
  emit_insn (gen_rtx_SET (operand0,
	operand1));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:230 */
extern rtx_insn *gen_split_1 (rtx_insn *, rtx *);
rtx_insn *
gen_split_1 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_split_1 (6502.md:230)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 239 "../../gcc/config/6502/6502.md"
{
  rtx acc = gen_rtx_REG (QImode, ACC_REGNUM);
  rtx yreg = gen_rtx_REG (QImode, Y_REGNUM);
  int i, modesize = GET_MODE_SIZE (HImode);

  if (rtx_equal_p (operands[0], operands[1]))
    DONE;

  emit_insn (m65x_push (QImode, acc));

  if (indirect_mem_operand (operands[1], HImode)
      || indirect_offset_mem_operand (operands[1], HImode))
    {
      rtx addr = XEXP (operands[1], 0);
      HOST_WIDE_INT offset;
      rtx base;

      if (REG_P (addr))
        {
	  base = addr;
	  offset = 0;
	}
      else if (GET_CODE (addr) == PLUS)
        {
	  base = XEXP (addr, 0);
	  offset = INTVAL (XEXP (addr, 1));
	}
      else
        gcc_unreachable ();

      if (TARGET_PHX)
	emit_insn (m65x_push (QImode, yreg));
      else
        {
	  emit_move_insn (acc, yreg);
	  emit_insn (m65x_push (QImode, acc));
	}
      emit_move_insn (yreg, gen_int_mode (offset, QImode));

      for (i = 0; i < modesize; i++)
        {
	  rtx dstpart = simplify_gen_subreg (QImode, operands[0],
					     HImode, i);
	  emit_insn (gen_loadqi_indy (acc, yreg, base));
	  emit_move_insn (dstpart, acc);
	  if (i != modesize - 1)
	    emit_insn (gen_incdecqi3 (yreg, yreg, const1_rtx));
	}

      if (TARGET_PHX)
	emit_insn (m65x_pop (QImode, yreg));
      else
        {
	  emit_insn (m65x_pop (QImode, acc));
	  emit_move_insn (yreg, acc);
	}
    }
  else if (indirect_mem_operand (operands[0], HImode)
	   || indirect_offset_mem_operand (operands[0], HImode))
    {
      rtx addr = XEXP (operands[0], 0);
      HOST_WIDE_INT offset;
      rtx base;

      if (REG_P (addr))
        {
	  base = addr;
	  offset = 0;
	}
      else if (GET_CODE (addr) == PLUS)
        {
	  base = XEXP (addr, 0);
	  offset = INTVAL (XEXP (addr, 1));
	}
      else
        gcc_unreachable ();

      if (TARGET_PHX)
	emit_insn (m65x_push (QImode, yreg));
      else
        {
	  emit_move_insn (acc, yreg);
	  emit_insn (m65x_push (QImode, acc));
	}

      emit_move_insn (yreg, gen_int_mode (offset, QImode));
      
      for (i = 0; i < modesize; i++)
        {
	  rtx srcpart = simplify_gen_subreg (QImode, operands[1],
					     HImode, i);
	  emit_move_insn (acc, srcpart);
	  emit_insn (gen_storeqi_indy (yreg, base, acc));
	  if (i != modesize - 1)
	    emit_insn (gen_incdecqi3 (yreg, yreg, const1_rtx));
	}
      
      if (TARGET_PHX)
	emit_insn (m65x_pop (QImode, yreg));
      else
        {
	  emit_insn (m65x_pop (QImode, acc));
	  emit_move_insn (yreg, acc);
	}
    }
  else
    {
      bool backwards = false;

      if (REG_P (operands[0]) && operands[0] != operands[1]
	  && reg_overlap_mentioned_p (operands[0], operands[1]))
	{
	  gcc_assert (REG_P (operands[0])
		      && IS_ZP_REGNUM (REGNO (operands[0])));
	  gcc_assert (REG_P (operands[1])
		      && IS_ZP_REGNUM (REGNO (operands[1])));
	  if (REGNO (operands[0]) < REGNO (operands[1]))
	    backwards = true;
	}

      if (backwards)
	for (i = modesize - 1; i >= 0; i--)
	  {
	    rtx dstpart = simplify_gen_subreg (QImode, operands[0],
					       HImode, i);
	    rtx srcpart = simplify_gen_subreg (QImode, operands[1],
					       HImode, i);
	    emit_move_insn (acc, srcpart);
	    emit_move_insn (dstpart, acc);
	  }
      else
	for (i = 0; i < modesize; i++)
	  {
	    rtx dstpart = simplify_gen_subreg (QImode, operands[0],
					       HImode, i);
	    rtx srcpart = simplify_gen_subreg (QImode, operands[1],
					       HImode, i);
	    emit_move_insn (acc, srcpart);
	    emit_move_insn (dstpart, acc);
	  }
    }
  
  emit_insn (m65x_pop (QImode, acc));
  emit_use (gen_rtx_REG (HImode, HARDSP_REGNUM));
  
  DONE;
}
#undef DONE
#undef FAIL
  emit_insn (pc_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:230 */
extern rtx_insn *gen_split_2 (rtx_insn *, rtx *);
rtx_insn *
gen_split_2 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_split_2 (6502.md:230)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 239 "../../gcc/config/6502/6502.md"
{
  rtx acc = gen_rtx_REG (QImode, ACC_REGNUM);
  rtx yreg = gen_rtx_REG (QImode, Y_REGNUM);
  int i, modesize = GET_MODE_SIZE (SImode);

  if (rtx_equal_p (operands[0], operands[1]))
    DONE;

  emit_insn (m65x_push (QImode, acc));

  if (indirect_mem_operand (operands[1], SImode)
      || indirect_offset_mem_operand (operands[1], SImode))
    {
      rtx addr = XEXP (operands[1], 0);
      HOST_WIDE_INT offset;
      rtx base;

      if (REG_P (addr))
        {
	  base = addr;
	  offset = 0;
	}
      else if (GET_CODE (addr) == PLUS)
        {
	  base = XEXP (addr, 0);
	  offset = INTVAL (XEXP (addr, 1));
	}
      else
        gcc_unreachable ();

      if (TARGET_PHX)
	emit_insn (m65x_push (QImode, yreg));
      else
        {
	  emit_move_insn (acc, yreg);
	  emit_insn (m65x_push (QImode, acc));
	}
      emit_move_insn (yreg, gen_int_mode (offset, QImode));

      for (i = 0; i < modesize; i++)
        {
	  rtx dstpart = simplify_gen_subreg (QImode, operands[0],
					     SImode, i);
	  emit_insn (gen_loadqi_indy (acc, yreg, base));
	  emit_move_insn (dstpart, acc);
	  if (i != modesize - 1)
	    emit_insn (gen_incdecqi3 (yreg, yreg, const1_rtx));
	}

      if (TARGET_PHX)
	emit_insn (m65x_pop (QImode, yreg));
      else
        {
	  emit_insn (m65x_pop (QImode, acc));
	  emit_move_insn (yreg, acc);
	}
    }
  else if (indirect_mem_operand (operands[0], SImode)
	   || indirect_offset_mem_operand (operands[0], SImode))
    {
      rtx addr = XEXP (operands[0], 0);
      HOST_WIDE_INT offset;
      rtx base;

      if (REG_P (addr))
        {
	  base = addr;
	  offset = 0;
	}
      else if (GET_CODE (addr) == PLUS)
        {
	  base = XEXP (addr, 0);
	  offset = INTVAL (XEXP (addr, 1));
	}
      else
        gcc_unreachable ();

      if (TARGET_PHX)
	emit_insn (m65x_push (QImode, yreg));
      else
        {
	  emit_move_insn (acc, yreg);
	  emit_insn (m65x_push (QImode, acc));
	}

      emit_move_insn (yreg, gen_int_mode (offset, QImode));
      
      for (i = 0; i < modesize; i++)
        {
	  rtx srcpart = simplify_gen_subreg (QImode, operands[1],
					     SImode, i);
	  emit_move_insn (acc, srcpart);
	  emit_insn (gen_storeqi_indy (yreg, base, acc));
	  if (i != modesize - 1)
	    emit_insn (gen_incdecqi3 (yreg, yreg, const1_rtx));
	}
      
      if (TARGET_PHX)
	emit_insn (m65x_pop (QImode, yreg));
      else
        {
	  emit_insn (m65x_pop (QImode, acc));
	  emit_move_insn (yreg, acc);
	}
    }
  else
    {
      bool backwards = false;

      if (REG_P (operands[0]) && operands[0] != operands[1]
	  && reg_overlap_mentioned_p (operands[0], operands[1]))
	{
	  gcc_assert (REG_P (operands[0])
		      && IS_ZP_REGNUM (REGNO (operands[0])));
	  gcc_assert (REG_P (operands[1])
		      && IS_ZP_REGNUM (REGNO (operands[1])));
	  if (REGNO (operands[0]) < REGNO (operands[1]))
	    backwards = true;
	}

      if (backwards)
	for (i = modesize - 1; i >= 0; i--)
	  {
	    rtx dstpart = simplify_gen_subreg (QImode, operands[0],
					       SImode, i);
	    rtx srcpart = simplify_gen_subreg (QImode, operands[1],
					       SImode, i);
	    emit_move_insn (acc, srcpart);
	    emit_move_insn (dstpart, acc);
	  }
      else
	for (i = 0; i < modesize; i++)
	  {
	    rtx dstpart = simplify_gen_subreg (QImode, operands[0],
					       SImode, i);
	    rtx srcpart = simplify_gen_subreg (QImode, operands[1],
					       SImode, i);
	    emit_move_insn (acc, srcpart);
	    emit_move_insn (dstpart, acc);
	  }
    }
  
  emit_insn (m65x_pop (QImode, acc));
  emit_use (gen_rtx_REG (HImode, HARDSP_REGNUM));
  
  DONE;
}
#undef DONE
#undef FAIL
  emit_insn (pc_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:387 */
rtx
gen_addhi3 (rtx operand0,
	rtx operand1,
	rtx operand2)
{
  rtx_insn *_val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 395 "../../gcc/config/6502/6502.md"
{
#if 0
  /* Eagerly splitting up adds used to calculate addresses is
     counterproductive, because it inhibits middle-end passes which deal with
     addressing modes, etc.  FIXME.  */
  if ((REG_P (operands[1]) && REG_POINTER (operands[1]))
      || (REG_P (operands[2]) && REG_POINTER (operands[2]))
      || sym_const_operand (operands[1], HImode)
      || sym_const_operand (operands[2], HImode))
    {
      emit_insn (gen_addhi3_noclob (operands[0], operands[1], operands[2]));
      DONE;
    }
#endif

  if (can_create_pseudo_p ())
    {
#if 0
      /* This doesn't really work either, because it creates RTL that's hard to
	 deal with later.  */
      rtx base, index;
      bool op1ptr, op2ptr;

      op1ptr = REG_P (operands[1]) && REG_POINTER (operands[1]);
      op2ptr = REG_P (operands[2]) && REG_POINTER (operands[2]);

      if (op1ptr && !op2ptr)
	{
	  base = operands[1];
	  index = operands[2];
	}
      else
	{
	  base = operands[2];
	  index = operands[1];
	}

      if (!op1ptr ^ !op2ptr)
	{
	  rtx idx_lo, idx_hi, tmp_hi;
	  rtx tmp = gen_reg_rtx (HImode);

	  idx_lo = operand_subword (index, 0, 1, HImode);
	  idx_hi = operand_subword (index, 1, 1, HImode);
	  tmp_hi = operand_subword (tmp, 1, 1, HImode);

	  emit_move_insn (tmp, base);
	  emit_insn (gen_addqi3 (tmp_hi, tmp_hi, idx_hi));

	  rtx x = gen_rtx_PLUS (HImode,
		   gen_rtx_ZERO_EXTEND (HImode, force_reg (QImode, idx_lo)),
					tmp);
	  emit_move_insn (operands[0], x);
	}
      else
#endif
	m65x_expand_addsub (HImode, true, operands);
      DONE;
    }
}
#undef DONE
#undef FAIL
    operand0 = operands[0];
    (void) operand0;
    operand1 = operands[1];
    (void) operand1;
    operand2 = operands[2];
    (void) operand2;
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (operand0,
	gen_rtx_PLUS (HImode,
	operand1,
	operand2)),
		gen_hard_reg_clobber (CC_NZmode, 68),
		gen_hard_reg_clobber (CC_Cmode, 72),
		gen_hard_reg_clobber (CC_Vmode, 76))), false);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:387 */
rtx
gen_addsi3 (rtx operand0,
	rtx operand1,
	rtx operand2)
{
  rtx_insn *_val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 395 "../../gcc/config/6502/6502.md"
{
#if 0
  /* Eagerly splitting up adds used to calculate addresses is
     counterproductive, because it inhibits middle-end passes which deal with
     addressing modes, etc.  FIXME.  */
  if ((REG_P (operands[1]) && REG_POINTER (operands[1]))
      || (REG_P (operands[2]) && REG_POINTER (operands[2]))
      || sym_const_operand (operands[1], SImode)
      || sym_const_operand (operands[2], SImode))
    {
      emit_insn (gen_addhi3_noclob (operands[0], operands[1], operands[2]));
      DONE;
    }
#endif

  if (can_create_pseudo_p ())
    {
#if 0
      /* This doesn't really work either, because it creates RTL that's hard to
	 deal with later.  */
      rtx base, index;
      bool op1ptr, op2ptr;

      op1ptr = REG_P (operands[1]) && REG_POINTER (operands[1]);
      op2ptr = REG_P (operands[2]) && REG_POINTER (operands[2]);

      if (op1ptr && !op2ptr)
	{
	  base = operands[1];
	  index = operands[2];
	}
      else
	{
	  base = operands[2];
	  index = operands[1];
	}

      if (!op1ptr ^ !op2ptr)
	{
	  rtx idx_lo, idx_hi, tmp_hi;
	  rtx tmp = gen_reg_rtx (HImode);

	  idx_lo = operand_subword (index, 0, 1, HImode);
	  idx_hi = operand_subword (index, 1, 1, HImode);
	  tmp_hi = operand_subword (tmp, 1, 1, HImode);

	  emit_move_insn (tmp, base);
	  emit_insn (gen_addqi3 (tmp_hi, tmp_hi, idx_hi));

	  rtx x = gen_rtx_PLUS (HImode,
		   gen_rtx_ZERO_EXTEND (HImode, force_reg (QImode, idx_lo)),
					tmp);
	  emit_move_insn (operands[0], x);
	}
      else
#endif
	m65x_expand_addsub (SImode, true, operands);
      DONE;
    }
}
#undef DONE
#undef FAIL
    operand0 = operands[0];
    (void) operand0;
    operand1 = operands[1];
    (void) operand1;
    operand2 = operands[2];
    (void) operand2;
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (operand0,
	gen_rtx_PLUS (SImode,
	operand1,
	operand2)),
		gen_hard_reg_clobber (CC_NZmode, 68),
		gen_hard_reg_clobber (CC_Cmode, 72),
		gen_hard_reg_clobber (CC_Vmode, 76))), false);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:504 */
rtx
gen_subhi3 (rtx operand0,
	rtx operand1,
	rtx operand2)
{
  rtx_insn *_val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 509 "../../gcc/config/6502/6502.md"
{
  m65x_expand_addsub (HImode, false, operands);
  DONE;
}
#undef DONE
#undef FAIL
    operand0 = operands[0];
    (void) operand0;
    operand1 = operands[1];
    (void) operand1;
    operand2 = operands[2];
    (void) operand2;
  }
  emit_insn (gen_rtx_SET (operand0,
	gen_rtx_MINUS (HImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:504 */
rtx
gen_subsi3 (rtx operand0,
	rtx operand1,
	rtx operand2)
{
  rtx_insn *_val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 509 "../../gcc/config/6502/6502.md"
{
  m65x_expand_addsub (SImode, false, operands);
  DONE;
}
#undef DONE
#undef FAIL
    operand0 = operands[0];
    (void) operand0;
    operand1 = operands[1];
    (void) operand1;
    operand2 = operands[2];
    (void) operand2;
  }
  emit_insn (gen_rtx_SET (operand0,
	gen_rtx_MINUS (SImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:514 */
rtx
gen_lshrhi3 (rtx operand0,
	rtx operand1,
	rtx operand2)
{
  rtx_insn *_val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 519 "../../gcc/config/6502/6502.md"
{
  HOST_WIDE_INT shiftamt = INTVAL (operands[2]);

  switch (shiftamt)
    {
    case 1:
    case 2:
    case 3:
    case 4:
      {
        rtx tmp = gen_reg_rtx (HImode);
        int i;

	emit_move_insn (tmp, operands[1]);

	rtx tmphi = gen_highpart_mode (QImode, HImode, tmp);
	rtx tmplo = gen_lowpart (QImode, tmp);

	for (i = 1; i <= shiftamt; i++)
	  {
	    emit_insn (gen_lshrqi3_insn (tmphi, tmphi, const1_rtx));
	    emit_insn (gen_m65x_rorqi3 (tmplo, tmplo));
	  }

	emit_move_insn (operands[0], tmp);
      }
      break;

    case 5:
    case 6:
      {
	int i;
        rtx tmp = gen_reg_rtx (QImode);
	rtx tmp2 = gen_reg_rtx (HImode), tmplo, tmphi;

	/* lshr #5			  lshr #6
	   -------			  -------
	   hhhhhhhh llllllll ->           hhhhhhhh llllllll ->
	   00000000 hhhhhhhh llllllll ->  00000000 hhhhhhhh llllllll->
	   00000hhh hhhhhlll lllll000 ->  000000hh hhhhhhll llllll00 ->
	   00000hhh hhhhhlll		  000000hh hhhhhhll.  */

	tmplo = gen_lowpart (QImode, tmp2);
	tmphi = gen_highpart_mode (QImode, HImode, tmp2);

        emit_move_insn (tmp, gen_lowpart (QImode, operands[1]));
	emit_move_insn (tmplo, gen_highpart_mode (QImode, HImode, operands[1]));
	emit_move_insn (tmphi, const0_rtx);

	for (i = 8; i > shiftamt; i--)
	  {
	    emit_insn (gen_ashlqi3_insn (tmp, tmp, const1_rtx));
	    emit_insn (gen_m65x_rolqi3 (tmplo, tmplo));
	    emit_insn (gen_m65x_rolqi3 (tmphi, tmphi));
	  }

	emit_move_insn (operands[0], tmp2);
      }
      break;

    case 7:
      {
	rtx tmp = gen_reg_rtx (HImode);
	rtx tmp2 = gen_reg_rtx (QImode);
	/* hhhhhhhh llllllll ->
           0000000h hhhhhhhl.  */
	emit_insn (gen_ashlqi3_insn (tmp2, gen_lowpart (QImode, operands[1]),
				     const1_rtx));
	emit_insn (gen_m65x_rolqi3 (gen_lowpart (QImode, tmp),
				    gen_highpart_mode (QImode, HImode,
						       operands[1])));
	emit_insn (gen_m65x_rolqi3 (gen_highpart_mode (QImode, HImode, tmp),
				    force_reg (QImode, const0_rtx)));
	emit_move_insn (operands[0], tmp);
      }
      break;

    case 8:
      {
	emit_move_insn (gen_lowpart (QImode, operands[0]),
			gen_highpart_mode (QImode, HImode, operands[1]));
	emit_move_insn (gen_highpart_mode (QImode, HImode, operands[0]),
			const0_rtx);
      }
      break;

    /* Shifts of 9-16 are handled reasonably well by generic expanders using
       lshrqi3, so no need to duplicate that here.  */

    default:
      FAIL;
    }

  DONE;
}
#undef DONE
#undef FAIL
    operand0 = operands[0];
    (void) operand0;
    operand1 = operands[1];
    (void) operand1;
    operand2 = operands[2];
    (void) operand2;
  }
  emit_insn (gen_rtx_SET (operand0,
	gen_rtx_LSHIFTRT (HImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:615 */
rtx
gen_ashlhi3 (rtx operand0,
	rtx operand1,
	rtx operand2)
{
  rtx_insn *_val = 0;
  start_sequence ();
  {
    rtx operands[3];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 620 "../../gcc/config/6502/6502.md"
{
  HOST_WIDE_INT shiftamt = INTVAL (operands[2]);

  switch (shiftamt)
    {
    case 1:
    case 2:
    case 3:
    case 4:
      {
	rtx tmp = gen_reg_rtx (HImode);
	int i;

	emit_move_insn (tmp, operands[1]);

	rtx tmphi = gen_highpart_mode (QImode, HImode, tmp);
	rtx tmplo = gen_lowpart (QImode, tmp);

	for (i = 1; i <= shiftamt; i++)
	  {
	    emit_insn (gen_ashlqi3_insn (tmplo, tmplo, const1_rtx));
	    emit_insn (gen_m65x_rolqi3 (tmphi, tmphi));
	  }

	emit_move_insn (operands[0], tmp);
      }
      break;

    case 5:
    case 6:
      {
	/* ashl #5			  ashl #6
	   -------			  -------
	            hhhhhhhh llllllll ->   hhhhhhhh llllllll ->
           hhhhhhhh llllllll 00000000 ->   hhhhhhhh llllllll 00000000 ->
           ...hhhhh hhhlllll lll00000	   ..hhhhhh hhllllll ll000000.  */
	rtx tmp = gen_reg_rtx (QImode);
	rtx tmp2 = gen_reg_rtx (HImode), tmplo, tmphi;
	int i;

	tmplo = gen_lowpart (QImode, tmp2);
	tmphi = gen_highpart_mode (QImode, HImode, tmp2);

	emit_move_insn (tmp, gen_highpart_mode (QImode, HImode, operands[1]));
	emit_move_insn (tmphi, gen_lowpart (QImode, operands[1]));
	emit_move_insn (tmplo, const0_rtx);

	for (i = 8; i > shiftamt; i--)
	  {
            emit_insn (gen_lshrqi3_insn (tmp, tmp, const1_rtx));
	    emit_insn (gen_m65x_rorqi3 (tmphi, tmphi));
	    emit_insn (gen_m65x_rorqi3 (tmplo, tmplo));
	  }

	emit_move_insn (operands[0], tmp2);
      }
      break;

    case 7:
      {
	rtx tmp = gen_reg_rtx (HImode);
	rtx tmp2 = gen_reg_rtx (QImode);

        /* hhhhhhhh llllllll ->
	   hlllllll l0000000.  */

	emit_insn (gen_lshrqi3_insn (tmp2, gen_highpart_mode (QImode,
							      HImode,
							      operands[1]),
				     const1_rtx));
	emit_insn (gen_m65x_rorqi3 (gen_highpart_mode (QImode, HImode, tmp),
				    gen_lowpart (QImode, operands[1])));
	emit_insn (gen_m65x_rorqi3 (gen_lowpart (QImode, tmp),
				    force_reg (QImode, const0_rtx)));
	emit_move_insn (operands[0], tmp);
      }
      break;

    default:
      FAIL;
    }

  DONE;
}
#undef DONE
#undef FAIL
    operand0 = operands[0];
    (void) operand0;
    operand1 = operands[1];
    (void) operand1;
    operand2 = operands[2];
    (void) operand2;
  }
  emit_insn (gen_rtx_SET (operand0,
	gen_rtx_ASHIFT (HImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:780 */
rtx
gen_cbranchhi4 (rtx operand0,
	rtx operand1,
	rtx operand2,
	rtx operand3)
{
  rtx_insn *_val = 0;
  start_sequence ();
  {
    rtx operands[5];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 790 "../../gcc/config/6502/6502.md"
{
  int force_reg_operand;

  switch (GET_CODE (operands[0]))
    {
    case GTU:
    case LEU:
    case GT:
    case LE:
      force_reg_operand = 2;
      break;

    case EQ:
    case NE:
    case LTU:
    case GEU:
    case LT:
    case GE:
      force_reg_operand = 1;
      break;

    default:
      gcc_unreachable ();
    }

  if (can_create_pseudo_p () &&
      !register_operand (operands[force_reg_operand], HImode))
    operands[force_reg_operand] = force_reg (HImode,
					     operands[force_reg_operand]);
}
#undef DONE
#undef FAIL
    operand0 = operands[0];
    (void) operand0;
    operand1 = operands[1];
    (void) operand1;
    operand2 = operands[2];
    (void) operand2;
    operand3 = operands[3];
    (void) operand3;
  }
  emit_jump_insn (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_fmt_ee (GET_CODE (operand0), VOIDmode,
		operand1,
		operand2),
	gen_rtx_LABEL_REF (VOIDmode,
	operand3),
	pc_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (QImode)))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:821 */
extern rtx_insn *gen_split_3 (rtx_insn *, rtx *);
rtx_insn *
gen_split_3 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_split_3 (6502.md:821)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 832 "../../gcc/config/6502/6502.md"
{
  m65x_emit_himode_comparison (EQ, operands[0], operands[1], operands[2],
			       operands[3]);
  DONE;
}
#undef DONE
#undef FAIL
  emit_insn (pc_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:838 */
extern rtx_insn *gen_split_4 (rtx_insn *, rtx *);
rtx_insn *
gen_split_4 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_split_4 (6502.md:838)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 849 "../../gcc/config/6502/6502.md"
{
  m65x_emit_himode_comparison (NE, operands[0], operands[1], operands[2],
			       operands[3]);
  DONE;
}
#undef DONE
#undef FAIL
  emit_insn (pc_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:855 */
extern rtx_insn *gen_split_5 (rtx_insn *, rtx *);
rtx_insn *
gen_split_5 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_split_5 (6502.md:855)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 866 "../../gcc/config/6502/6502.md"
{
  m65x_emit_himode_comparison (LTU, operands[0], operands[1], operands[2],
			       operands[3]);
  DONE;
}
#undef DONE
#undef FAIL
  emit_insn (pc_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:872 */
extern rtx_insn *gen_split_6 (rtx_insn *, rtx *);
rtx_insn *
gen_split_6 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_split_6 (6502.md:872)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 883 "../../gcc/config/6502/6502.md"
{
  m65x_emit_himode_comparison (LTU, operands[1], operands[0], operands[2],
			       operands[3]);
  DONE;
}
#undef DONE
#undef FAIL
  emit_insn (pc_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

