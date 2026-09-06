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

/* ../../gcc/config/6502/6502.md:889 */
extern rtx_insn *gen_split_7 (rtx_insn *, rtx *);
rtx_insn *
gen_split_7 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_split_7 (6502.md:889)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 900 "../../gcc/config/6502/6502.md"
{
  m65x_emit_himode_comparison (GEU, operands[0], operands[1], operands[2],
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

/* ../../gcc/config/6502/6502.md:906 */
extern rtx_insn *gen_split_8 (rtx_insn *, rtx *);
rtx_insn *
gen_split_8 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_split_8 (6502.md:906)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 917 "../../gcc/config/6502/6502.md"
{
  m65x_emit_himode_comparison (GEU, operands[1], operands[0], operands[2],
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

/* ../../gcc/config/6502/6502.md:923 */
extern rtx_insn *gen_split_9 (rtx_insn *, rtx *);
rtx_insn *
gen_split_9 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_split_9 (6502.md:923)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 934 "../../gcc/config/6502/6502.md"
{
  m65x_emit_himode_comparison (LT, operands[0], operands[1], operands[2],
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

/* ../../gcc/config/6502/6502.md:940 */
extern rtx_insn *gen_split_10 (rtx_insn *, rtx *);
rtx_insn *
gen_split_10 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_split_10 (6502.md:940)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 951 "../../gcc/config/6502/6502.md"
{
  m65x_emit_himode_comparison (LT, operands[1], operands[0], operands[2],
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

/* ../../gcc/config/6502/6502.md:957 */
extern rtx_insn *gen_split_11 (rtx_insn *, rtx *);
rtx_insn *
gen_split_11 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_split_11 (6502.md:957)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 968 "../../gcc/config/6502/6502.md"
{
  m65x_emit_himode_comparison (GE, operands[0], operands[1], operands[2],
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

/* ../../gcc/config/6502/6502.md:974 */
extern rtx_insn *gen_split_12 (rtx_insn *, rtx *);
rtx_insn *
gen_split_12 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_split_12 (6502.md:974)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 985 "../../gcc/config/6502/6502.md"
{
  m65x_emit_himode_comparison (GE, operands[1], operands[0], operands[2],
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

/* ../../gcc/config/6502/6502.md:991 */
rtx
gen_extendhisi2 (rtx operand0,
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
#line 995 "../../gcc/config/6502/6502.md"
{
  rtx acc = gen_reg_rtx (QImode);
  rtx cmp, new_label = gen_label_rtx ();
  
  emit_move_insn (simplify_gen_subreg (QImode, operands[0], SImode, 0),
		  simplify_gen_subreg (QImode, operands[1], HImode, 0));
  emit_move_insn (simplify_gen_subreg (QImode, operands[0], SImode, 1),
		  simplify_gen_subreg (QImode, operands[1], HImode, 1));

  emit_move_insn (acc, simplify_gen_subreg (QImode, operands[1], HImode, 1));
  emit_insn (gen_iorqi3_nz (acc, acc, gen_int_mode (0x7f, QImode)));
  cmp = gen_rtx_LT (VOIDmode, gen_rtx_REG (CC_NZmode, NZ_REGNUM), const0_rtx);
  emit_jump_insn (gen_rtx_SET (pc_rtx,
			       gen_rtx_IF_THEN_ELSE (VOIDmode, cmp,
				 gen_rtx_LABEL_REF (Pmode, new_label),
				 pc_rtx)));
  emit_move_insn (acc, const0_rtx);
  emit_label (new_label);

  emit_move_insn (simplify_gen_subreg (QImode, operands[0], SImode, 2), acc);
  emit_move_insn (simplify_gen_subreg (QImode, operands[0], SImode, 3), acc);
  
  DONE;
}
#undef DONE
#undef FAIL
    operand0 = operands[0];
    (void) operand0;
    operand1 = operands[1];
    (void) operand1;
  }
  emit_insn (gen_rtx_SET (operand0,
	gen_rtx_SIGN_EXTEND (SImode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1020 */
rtx
gen_neghi2 (rtx operand0,
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
#line 1024 "../../gcc/config/6502/6502.md"
{
  rtx zero = force_reg (HImode, const0_rtx);
  emit_insn (gen_subhi3 (operands[0], zero, operands[1]));
  DONE;
}
#undef DONE
#undef FAIL
    operand0 = operands[0];
    (void) operand0;
    operand1 = operands[1];
    (void) operand1;
  }
  emit_insn (gen_rtx_SET (operand0,
	gen_rtx_NEG (HImode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1032 */
rtx
gen_movqi (rtx operand0,
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
#line 1036 "../../gcc/config/6502/6502.md"
{
  if (can_create_pseudo_p ())
    {
      if (MEM_P (operands[0])
	  && MEM_ADDR_SPACE (operands[0]) == ADDR_SPACE_GENERIC
	  && ((MEM_P (operands[1])
	       && MEM_ADDR_SPACE (operands[1]) == ADDR_SPACE_GENERIC)
	      || (GET_CODE (operands[1]) == CONST_INT
		  && (INTVAL (operands[1]) != 0
		      || !CONSTANT_ADDRESS_P (XEXP (operands[0], 0))))
	      || (!CONSTANT_ADDRESS_P (XEXP (operands[0], 0))
		  && REG_P (operands[1])
		  && REGNO (operands[1]) != ACC_REGNUM)
	      || !REG_P (operands[1])))
	operands[1] = force_reg (QImode, operands[1]);
      else if (strict_ptr_reg_operand (operands[0], QImode)
	       && immediate_operand (operands[1], QImode)
	       && !(CONST_INT_P (operands[1]) && INTVAL (operands[1]) == 0
		    && TARGET_STZ))
	operands[1] = force_reg (QImode, operands[1]);
      else if (((MEM_P (operands[0])
		 && MEM_ADDR_SPACE (operands[0]) == ADDR_SPACE_GENERIC)
		|| strict_ptr_reg_operand (operands[0], QImode)
		|| reg_mentioned_p (frame_pointer_rtx, operands[0])
		|| reg_mentioned_p (virtual_stack_vars_rtx, operands[0]))
	       && ((MEM_P (operands[1])
		    && MEM_ADDR_SPACE (operands[1]) == ADDR_SPACE_GENERIC)
		   || strict_ptr_reg_operand (operands[1], QImode)
		   || reg_mentioned_p (frame_pointer_rtx, operands[1])
		   || reg_mentioned_p (virtual_stack_vars_rtx, operands[1])))
	operands[1] = copy_to_mode_reg (QImode, operands[1]);
      else if ((MEM_P (operands[0])
		&& MEM_ADDR_SPACE (operands[0]) == ADDR_SPACE_ZP
		&& !hard_reg_operand (operands[1], QImode))
	       || (MEM_P (operands[1])
		   && MEM_ADDR_SPACE (operands[1]) == ADDR_SPACE_ZP
		   && !hard_reg_operand (operands[0], QImode)))
	operands[1] = copy_to_mode_reg (QImode, operands[1]);
    }

  if (operands[0] == operands[1])
    DONE;
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

/* ../../gcc/config/6502/6502.md:1139 */
extern rtx_insn *gen_peephole2_1 (rtx_insn *, rtx *);
rtx_insn *
gen_peephole2_1 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_peephole2_1 (6502.md:1139)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 1145 "../../gcc/config/6502/6502.md"
{
  if (m65x_peep_load (operands[0], operands[1], NULL_RTX, true))
    DONE;
  else
    FAIL;
}
#undef DONE
#undef FAIL
  emit_insn (pc_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1152 */
extern rtx_insn *gen_peephole2_2 (rtx_insn *, rtx *);
rtx_insn *
gen_peephole2_2 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_peephole2_2 (6502.md:1152)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 1157 "../../gcc/config/6502/6502.md"
{
  if (m65x_peep_load (operands[0], operands[1], NULL_RTX, false))
    DONE;
  else
    FAIL;
}
#undef DONE
#undef FAIL
  emit_insn (pc_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1166 */
extern rtx_insn *gen_peephole2_3 (rtx_insn *, rtx *);
rtx_insn *
gen_peephole2_3 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_peephole2_3 (6502.md:1166)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 1175 "../../gcc/config/6502/6502.md"
{
  if (m65x_peep_load (operands[0], operands[2], operands[1], true))
    DONE;
  else
    FAIL;
}
#undef DONE
#undef FAIL
  emit_insn (pc_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1182 */
extern rtx_insn *gen_peephole2_4 (rtx_insn *, rtx *);
rtx_insn *
gen_peephole2_4 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_peephole2_4 (6502.md:1182)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 1189 "../../gcc/config/6502/6502.md"
{
  if (m65x_peep_load (operands[0], operands[2], operands[1], false))
    DONE;
  else
    FAIL;
}
#undef DONE
#undef FAIL
  emit_insn (pc_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1198 */
extern rtx_insn *gen_peephole2_5 (rtx_insn *, rtx *);
rtx_insn *
gen_peephole2_5 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_peephole2_5 (6502.md:1198)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 1204 "../../gcc/config/6502/6502.md"
{
  if (m65x_peep_store (operands[0], NULL_RTX, operands[1], true))
    DONE;
  else
    FAIL;
}
#undef DONE
#undef FAIL
  emit_insn (pc_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1211 */
extern rtx_insn *gen_peephole2_6 (rtx_insn *, rtx *);
rtx_insn *
gen_peephole2_6 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_peephole2_6 (6502.md:1211)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 1216 "../../gcc/config/6502/6502.md"
{
  if (m65x_peep_store (operands[0], NULL_RTX, operands[1], false))
    DONE;
  else
    FAIL;
}
#undef DONE
#undef FAIL
  emit_insn (pc_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1225 */
extern rtx_insn *gen_peephole2_7 (rtx_insn *, rtx *);
rtx_insn *
gen_peephole2_7 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_peephole2_7 (6502.md:1225)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 1234 "../../gcc/config/6502/6502.md"
{
  if (m65x_peep_store (operands[1], operands[0], operands[2], true))
    DONE;
  else
    FAIL;
}
#undef DONE
#undef FAIL
  emit_insn (pc_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1241 */
extern rtx_insn *gen_peephole2_8 (rtx_insn *, rtx *);
rtx_insn *
gen_peephole2_8 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_peephole2_8 (6502.md:1241)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 1248 "../../gcc/config/6502/6502.md"
{
  if (m65x_peep_store (operands[1], operands[0], operands[2], false))
    DONE;
  else
    FAIL;
}
#undef DONE
#undef FAIL
  emit_insn (pc_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1257 */
extern rtx_insn *gen_peephole2_9 (rtx_insn *, rtx *);
rtx_insn *
gen_peephole2_9 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_peephole2_9 (6502.md:1257)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 1263 "../../gcc/config/6502/6502.md"
{
  if (m65x_peep_load (operands[0], operands[1], NULL_RTX, true))
    DONE;
  else
    FAIL;
}
#undef DONE
#undef FAIL
  emit_insn (pc_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1270 */
extern rtx_insn *gen_peephole2_10 (rtx_insn *, rtx *);
rtx_insn *
gen_peephole2_10 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_peephole2_10 (6502.md:1270)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 1275 "../../gcc/config/6502/6502.md"
{
  if (m65x_peep_load (operands[0], operands[1], NULL_RTX, false))
    DONE;
  else
    FAIL;
}
#undef DONE
#undef FAIL
  emit_insn (pc_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1284 */
extern rtx_insn *gen_peephole2_11 (rtx_insn *, rtx *);
rtx_insn *
gen_peephole2_11 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_peephole2_11 (6502.md:1284)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 1290 "../../gcc/config/6502/6502.md"
{
  if (m65x_peep_store (operands[0], NULL_RTX, operands[1], true))
    DONE;
  else
    FAIL;
}
#undef DONE
#undef FAIL
  emit_insn (pc_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1297 */
extern rtx_insn *gen_peephole2_12 (rtx_insn *, rtx *);
rtx_insn *
gen_peephole2_12 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands ATTRIBUTE_UNUSED)
{
  rtx_insn *_val = NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_peephole2_12 (6502.md:1297)\n");
  start_sequence ();
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 1302 "../../gcc/config/6502/6502.md"
{
  if (m65x_peep_store (operands[0], NULL_RTX, operands[1], false))
    DONE;
  else
    FAIL;
}
#undef DONE
#undef FAIL
  emit_insn (pc_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1311 */
extern rtx_insn *gen_peephole2_13 (rtx_insn *, rtx *);
rtx_insn *
gen_peephole2_13 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands)
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx_insn *_val = NULL;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[2] = peep2_find_free_register (1, 1, "hq", QImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_peephole2_13 (6502.md:1311)\n");
  start_sequence ();
  operand0 = operands[0];
  (void) operand0;
  operand1 = operands[1];
  (void) operand1;
  operand2 = operands[2];
  (void) operand2;
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand2,
	operand1),
		gen_hard_reg_clobber (CC_NZmode, 68))), true);
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	copy_rtx (operand2)),
		gen_hard_reg_clobber (CC_NZmode, 68))), false);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1323 */
extern rtx_insn *gen_peephole2_14 (rtx_insn *, rtx *);
rtx_insn *
gen_peephole2_14 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands)
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx_insn *_val = NULL;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[2] = peep2_find_free_register (1, 1, "hq", QImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_peephole2_14 (6502.md:1323)\n");
  start_sequence ();
  operand0 = operands[0];
  (void) operand0;
  operand1 = operands[1];
  (void) operand1;
  operand2 = operands[2];
  (void) operand2;
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand2,
	operand1),
		gen_hard_reg_clobber (CC_NZmode, 68))), true);
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	copy_rtx (operand2)),
		gen_hard_reg_clobber (CC_NZmode, 68))), false);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

