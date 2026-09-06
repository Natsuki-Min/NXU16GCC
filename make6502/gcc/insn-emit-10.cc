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

/* ../../gcc/config/6502/6502.md:1336 */
extern rtx_insn *gen_peephole2_15 (rtx_insn *, rtx *);
rtx_insn *
gen_peephole2_15 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands)
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx_insn *_val = NULL;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[3] = peep2_find_free_register (0, 0, "hq", QImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_peephole2_15 (6502.md:1336)\n");
  start_sequence ();
  operand0 = operands[0];
  (void) operand0;
  operand1 = operands[1];
  (void) operand1;
  operand2 = operands[2];
  (void) operand2;
  operand3 = operands[3];
  (void) operand3;
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand3,
	gen_rtx_MEM (QImode,
	gen_rtx_PLUS (HImode,
	gen_rtx_ZERO_EXTEND (HImode,
	operand1),
	operand2))),
		gen_hard_reg_clobber (CC_NZmode, 68))), true);
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	copy_rtx (operand3)),
		gen_hard_reg_clobber (CC_NZmode, 68))), false);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1353 */
extern rtx_insn *gen_peephole2_16 (rtx_insn *, rtx *);
rtx_insn *
gen_peephole2_16 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands)
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx_insn *_val = NULL;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[3] = peep2_find_free_register (0, 0, "hq", QImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_peephole2_16 (6502.md:1353)\n");
  start_sequence ();
  operand0 = operands[0];
  (void) operand0;
  operand1 = operands[1];
  (void) operand1;
  operand2 = operands[2];
  (void) operand2;
  operand3 = operands[3];
  (void) operand3;
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand3,
	gen_rtx_MEM (QImode,
	gen_rtx_PLUS (HImode,
	gen_rtx_ZERO_EXTEND (HImode,
	operand1),
	operand2))),
		gen_hard_reg_clobber (CC_NZmode, 68))), true);
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	copy_rtx (operand3)),
		gen_hard_reg_clobber (CC_NZmode, 68))), false);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1370 */
extern rtx_insn *gen_peephole2_17 (rtx_insn *, rtx *);
rtx_insn *
gen_peephole2_17 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands)
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx_insn *_val = NULL;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[3] = peep2_find_free_register (1, 1, "Aq", QImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_peephole2_17 (6502.md:1370)\n");
  start_sequence ();
  operand0 = operands[0];
  (void) operand0;
  operand1 = operands[1];
  (void) operand1;
  operand2 = operands[2];
  (void) operand2;
  operand3 = operands[3];
  (void) operand3;
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand3,
	operand2),
		gen_hard_reg_clobber (CC_NZmode, 68))), true);
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (gen_rtx_MEM (QImode,
	gen_rtx_PLUS (HImode,
	gen_rtx_ZERO_EXTEND (HImode,
	operand0),
	operand1)),
	copy_rtx (operand3)),
		gen_hard_reg_clobber (CC_NZmode, 68))), false);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1387 */
extern rtx_insn *gen_peephole2_18 (rtx_insn *, rtx *);
rtx_insn *
gen_peephole2_18 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands)
{
  rtx operand0;
  rtx operand1;
  rtx operand2;
  rtx operand3;
  rtx_insn *_val = NULL;
  HARD_REG_SET _regs_allocated;
  CLEAR_HARD_REG_SET (_regs_allocated);
  if ((operands[3] = peep2_find_free_register (1, 1, "Aq", QImode, &_regs_allocated)) == NULL_RTX)
    return NULL;
  if (dump_file)
    fprintf (dump_file, "Splitting with gen_peephole2_18 (6502.md:1387)\n");
  start_sequence ();
  operand0 = operands[0];
  (void) operand0;
  operand1 = operands[1];
  (void) operand1;
  operand2 = operands[2];
  (void) operand2;
  operand3 = operands[3];
  (void) operand3;
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand3,
	operand2),
		gen_hard_reg_clobber (CC_NZmode, 68))), true);
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (gen_rtx_MEM (QImode,
	gen_rtx_PLUS (HImode,
	gen_rtx_ZERO_EXTEND (HImode,
	operand0),
	operand1)),
	copy_rtx (operand3)),
		gen_hard_reg_clobber (CC_NZmode, 68))), false);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1404 */
extern rtx_insn *gen_peephole2_19 (rtx_insn *, rtx *);
rtx_insn *
gen_peephole2_19 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands)
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
    fprintf (dump_file, "Splitting with gen_peephole2_19 (6502.md:1404)\n");
  start_sequence ();
  operand0 = operands[0];
  (void) operand0;
  operand1 = operands[1];
  (void) operand1;
  operand2 = operands[2];
  (void) operand2;
  emit_insn (gen_rtx_SET (operand2,
	gen_rtx_MEM (QImode,
	operand1)));
  emit_insn (gen_rtx_SET (operand0,
	copy_rtx (operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1415 */
extern rtx_insn *gen_peephole2_20 (rtx_insn *, rtx *);
rtx_insn *
gen_peephole2_20 (rtx_insn *curr_insn ATTRIBUTE_UNUSED, rtx *operands)
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
    fprintf (dump_file, "Splitting with gen_peephole2_20 (6502.md:1415)\n");
  start_sequence ();
  operand0 = operands[0];
  (void) operand0;
  operand1 = operands[1];
  (void) operand1;
  operand2 = operands[2];
  (void) operand2;
  emit_insn (gen_rtx_SET (operand2,
	operand1));
  emit_insn (gen_rtx_SET (gen_rtx_MEM (QImode,
	operand0),
	copy_rtx (operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1424 */
rtx
gen_loadqi_indy (rtx operand0,
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
#line 1430 "../../gcc/config/6502/6502.md"
{}
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
	gen_rtx_MEM (QImode,
	gen_rtx_PLUS (HImode,
	gen_rtx_ZERO_EXTEND (HImode,
	operand1),
	operand2))));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1432 */
rtx
gen_loadqi_indy_nz (rtx operand0,
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
#line 1440 "../../gcc/config/6502/6502.md"
{}
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
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	gen_rtx_MEM (QImode,
	gen_rtx_PLUS (HImode,
	gen_rtx_ZERO_EXTEND (HImode,
	operand1),
	operand2))),
		gen_hard_reg_clobber (CC_NZmode, 68))), false);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1461 */
rtx
gen_storeqi_indy (rtx operand0,
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
#line 1467 "../../gcc/config/6502/6502.md"
{}
#undef DONE
#undef FAIL
    operand0 = operands[0];
    (void) operand0;
    operand1 = operands[1];
    (void) operand1;
    operand2 = operands[2];
    (void) operand2;
  }
  emit_insn (gen_rtx_SET (gen_rtx_MEM (QImode,
	gen_rtx_PLUS (HImode,
	gen_rtx_ZERO_EXTEND (HImode,
	operand0),
	operand1)),
	operand2));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1599 */
rtx
gen_pushqi1 (rtx operand0,
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
#line 1604 "../../gcc/config/6502/6502.md"
{}
#undef DONE
#undef FAIL
    operand0 = operands[0];
    (void) operand0;
    operand1 = operands[1];
    (void) operand1;
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	operand1),
		gen_hard_reg_clobber (CC_NZmode, 68))), false);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1606 */
rtx
gen_popqi1 (rtx operand0,
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
#line 1611 "../../gcc/config/6502/6502.md"
{}
#undef DONE
#undef FAIL
    operand0 = operands[0];
    (void) operand0;
    operand1 = operands[1];
    (void) operand1;
  }
  emit (gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	operand1),
		gen_hard_reg_clobber (CC_NZmode, 68))), false);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1634 */
rtx
gen_reload_inoutqi_zp (rtx operand0,
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
#line 1639 "../../gcc/config/6502/6502.md"
{
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
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	operand1),
		gen_rtx_CLOBBER (VOIDmode,
	operand2))), false);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1928 */
rtx
gen_negqi2 (rtx operand0,
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
#line 1932 "../../gcc/config/6502/6502.md"
{
  rtx tmp = gen_reg_rtx (QImode);
  
  emit_insn (gen_xorqi3 (tmp, operands[1], gen_int_mode (0xff, QImode)));
  emit_insn (gen_addqi3 (operands[0], tmp, const1_rtx));
  
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
	gen_rtx_NEG (QImode,
	operand1)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:1951 */
rtx
gen_andqi3 (rtx operand0,
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
#line 1957 "../../gcc/config/6502/6502.md"
{
  if (can_create_pseudo_p () && TARGET_TRB_TSB
      && zp_reg_or_const_mem_operand (operands[0], QImode)
      && CONST_INT_P (operands[2]))
    {
      /* This doesn't really work, it gets folded away into an andqi3 again
         (even when that means loading and storing the accumulator).  Maybe
	 setting RTX costs appropriately would help that.  */
      rtx tmp = force_reg (QImode,
			   gen_int_mode (~INTVAL (operands[2]), QImode));
      emit_insn (gen_m65x_trbqi3 (operands[0], tmp, operands[1]));
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
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	gen_rtx_AND (QImode,
	operand1,
	operand2)),
		gen_hard_reg_clobber (CC_NZmode, 68))), false);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:2079 */
rtx
gen_ashlqi3 (rtx operand0,
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
#line 2084 "../../gcc/config/6502/6502.md"
{
  int i;
  rtx tmp;

  if (!CONST_INT_P (operands[2]))
    FAIL;
  
  switch (INTVAL (operands[2]))
    {
    case 1:
      emit_insn (gen_ashlqi3_insn (operands[0], operands[1], operands[2]));
      break;
    
    case 2:
    case 3:
    case 4:
    case 5:
      tmp = gen_reg_rtx (QImode);
      emit_move_insn (tmp, operands[1]);

      for (i = 0; i < INTVAL (operands[2]); i++)
        emit_insn (gen_ashlqi3_insn (tmp, tmp, const1_rtx));

      emit_move_insn (operands[0], tmp);
      break;
    
    case 6:
      tmp = gen_reg_rtx (QImode);
      emit_insn (gen_m65x_rorqi3 (tmp, operands[1]));
      emit_insn (gen_m65x_rorqi3 (tmp, tmp));
      emit_insn (gen_m65x_rorqi3 (tmp, tmp));
      emit_insn (gen_andqi3 (operands[0], tmp, gen_int_mode (0xc0, QImode)));
      break;

    case 7:
      tmp = gen_reg_rtx (QImode);
      emit_insn (gen_m65x_rorqi3 (tmp, operands[1]));
      emit_insn (gen_m65x_rorqi3 (tmp, tmp));
      emit_insn (gen_andqi3 (operands[0], tmp, gen_int_mode (0x80, QImode)));
      break;

    case 8:
      emit_move_insn (operands[0], const0_rtx);
      break;

    default:
      gcc_unreachable ();
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
	gen_rtx_ASHIFT (QImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:2189 */
rtx
gen_lshrqi3 (rtx operand0,
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
#line 2194 "../../gcc/config/6502/6502.md"
{
  int i;
  rtx tmp;

  if (!CONST_INT_P (operands[2]))
    FAIL;
  
  switch (INTVAL (operands[2]))
    {
    case 1:
      emit_insn (gen_lshrqi3_insn (operands[0], operands[1], operands[2]));
      break;
    
    case 2:
    case 3:
    case 4:
    case 5:
      tmp = gen_reg_rtx (QImode);
      emit_move_insn (tmp, operands[1]);

      for (i = 0; i < INTVAL (operands[2]); i++)
        emit_insn (gen_lshrqi3_insn (tmp, tmp, const1_rtx));

      emit_move_insn (operands[0], tmp);
      break;
    
    case 6:
      tmp = gen_reg_rtx (QImode);
      emit_insn (gen_m65x_rolqi3 (tmp, operands[1]));
      emit_insn (gen_m65x_rolqi3 (tmp, tmp));
      emit_insn (gen_m65x_rolqi3 (tmp, tmp));
      emit_insn (gen_andqi3 (operands[0], tmp, gen_int_mode (0x03, QImode)));
      break;

    case 7:
      tmp = gen_reg_rtx (QImode);
      emit_insn (gen_m65x_rolqi3 (tmp, operands[1]));
      emit_insn (gen_m65x_rolqi3 (tmp, tmp));
      emit_insn (gen_andqi3 (operands[0], tmp, gen_int_mode (0x01, QImode)));
      break;

    case 8:
      emit_move_insn (operands[0], const0_rtx);
      break;

    default:
      gcc_unreachable ();
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
	gen_rtx_LSHIFTRT (QImode,
	operand1,
	operand2)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:2367 */
rtx
gen_cbranchqi4 (rtx operand0,
	rtx operand1,
	rtx operand2,
	rtx operand3)
{
  rtx_insn *_val = 0;
  start_sequence ();
  {
    rtx operands[4];
    operands[0] = operand0;
    operands[1] = operand1;
    operands[2] = operand2;
    operands[3] = operand3;
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 2375 "../../gcc/config/6502/6502.md"
{
  switch (GET_CODE (operands[0]))
    {
    case EQ:
    case NE:
    case LTU:
    case GEU:
    case LT:
    case GE:
      m65x_emit_qimode_comparison (GET_CODE (operands[0]), operands[1],
				   operands[2], operands[3]);
      break;

    case LEU:
      m65x_emit_qimode_comparison (GEU, operands[2], operands[1], operands[3]);
      break;

    case GTU:
      m65x_emit_qimode_comparison (LTU, operands[2], operands[1], operands[3]);
      break;

    case LE:
      m65x_emit_qimode_comparison (GE, operands[2], operands[1], operands[3]);
      break;

    case GT:
      m65x_emit_qimode_comparison (LT, operands[2], operands[1], operands[3]);
      break;
    
    default:
      gcc_unreachable ();
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
    operand3 = operands[3];
    (void) operand3;
  }
  emit_jump_insn (gen_rtx_SET (pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_fmt_ee (GET_CODE (operand0), VOIDmode,
		operand1,
		operand2),
	gen_rtx_LABEL_REF (VOIDmode,
	operand3),
	pc_rtx)));
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:2417 */
rtx
gen_call (rtx operand0,
	rtx operand1)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_CALL (VOIDmode,
	operand0,
	operand1),
		gen_rtx_USE (VOIDmode,
	const0_rtx)));
}

/* ../../gcc/config/6502/6502.md:2446 */
rtx
gen_call_value (rtx operand0,
	rtx operand1,
	rtx operand2)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	gen_rtx_CALL (VOIDmode,
	operand1,
	operand2)),
		gen_rtx_USE (VOIDmode,
	const0_rtx)));
}

/* ../../gcc/config/6502/6502.md:2484 */
rtx
gen_prologue (void)
{
  rtx_insn *_val = 0;
  start_sequence ();
  {
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 2487 "../../gcc/config/6502/6502.md"
{
  m65x_expand_prologue ();
  DONE;
}
#undef DONE
#undef FAIL
  }
  emit_insn (pc_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}

/* ../../gcc/config/6502/6502.md:2492 */
rtx
gen_epilogue (void)
{
  rtx_insn *_val = 0;
  start_sequence ();
  {
#define FAIL return (end_sequence (), _val)
#define DONE return (_val = get_insns (), end_sequence (), _val)
#line 2495 "../../gcc/config/6502/6502.md"
{
  m65x_expand_epilogue ();
  DONE;
}
#undef DONE
#undef FAIL
  }
  emit_insn (pc_rtx);
  _val = get_insns ();
  end_sequence ();
  return _val;
}



void
add_clobbers (rtx pattern ATTRIBUTE_UNUSED, int insn_code_number)
{
  switch (insn_code_number)
    {
    case 50:
      XVECEXP (pattern, 0, 1) = gen_hard_reg_clobber (CC_Cmode, 72);
      XVECEXP (pattern, 0, 2) = gen_hard_reg_clobber (CC_NZmode, 68);
      break;

    case 49:
    case 48:
    case 47:
    case 46:
      XVECEXP (pattern, 0, 2) = gen_hard_reg_clobber (CC_NZmode, 68);
      break;

    case 38:
      XVECEXP (pattern, 0, 3) = gen_hard_reg_clobber (CC_Cmode, 72);
      break;

    case 37:
    case 35:
      XVECEXP (pattern, 0, 2) = gen_hard_reg_clobber (CC_NZmode, 68);
      XVECEXP (pattern, 0, 3) = gen_hard_reg_clobber (CC_Vmode, 76);
      break;

    case 23:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (QImode));
      XVECEXP (pattern, 0, 2) = gen_hard_reg_clobber (CC_NZmode, 68);
      break;

    case 51:
    case 44:
    case 42:
    case 41:
    case 40:
    case 27:
    case 21:
    case 19:
    case 17:
    case 16:
      XVECEXP (pattern, 0, 1) = gen_hard_reg_clobber (CC_NZmode, 68);
      break;

    case 26:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 8:
    case 7:
    case 6:
      XVECEXP (pattern, 0, 1) = gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (QImode));
      break;

    case 4:
      XVECEXP (pattern, 0, 1) = gen_hard_reg_clobber (CC_NZmode, 68);
      XVECEXP (pattern, 0, 2) = gen_hard_reg_clobber (CC_Cmode, 72);
      XVECEXP (pattern, 0, 3) = gen_hard_reg_clobber (CC_Vmode, 76);
      XVECEXP (pattern, 0, 4) = gen_hard_reg_clobber (QImode, 0);
      break;

    case 36:
    case 34:
    case 31:
    case 29:
    case 3:
      XVECEXP (pattern, 0, 1) = gen_hard_reg_clobber (CC_NZmode, 68);
      XVECEXP (pattern, 0, 2) = gen_hard_reg_clobber (CC_Cmode, 72);
      XVECEXP (pattern, 0, 3) = gen_hard_reg_clobber (CC_Vmode, 76);
      break;

    default:
      gcc_unreachable ();
    }
}


bool
added_clobbers_hard_reg_p (int insn_code_number)
{
  switch (insn_code_number)
    {
    case 26:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 8:
    case 7:
    case 6:
      return false;

    case 50:
    case 49:
    case 48:
    case 47:
    case 46:
    case 38:
    case 37:
    case 35:
    case 23:
    case 51:
    case 44:
    case 42:
    case 41:
    case 40:
    case 27:
    case 21:
    case 19:
    case 17:
    case 16:
    case 4:
    case 36:
    case 34:
    case 31:
    case 29:
    case 3:
      return true;

    default:
      gcc_unreachable ();
    }
}
