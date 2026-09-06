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

/* ../../gcc/config/6502/6502.md:1903 */
rtx
gen_sbcqi3_nzv (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (operand0,
	gen_rtx_MINUS (QImode,
	operand1,
	gen_rtx_MINUS (QImode,
	operand2,
	gen_rtx_LTU (QImode,
	gen_rtx_REG (CC_Cmode,
	72),
	const0_rtx)))),
		gen_rtx_SET (gen_rtx_REG (CC_NZmode,
	68),
	gen_rtx_UNSPEC (CC_NZmode,
	gen_rtvec (3,
		copy_rtx (operand1),
		copy_rtx (operand2),
		gen_rtx_REG (CC_Cmode,
	72)),
	0)),
		gen_rtx_SET (gen_rtx_REG (CC_Vmode,
	76),
	gen_rtx_UNSPEC (CC_Vmode,
	gen_rtvec (3,
		copy_rtx (operand1),
		copy_rtx (operand2),
		gen_rtx_REG (CC_Cmode,
	72)),
	1)),
		gen_hard_reg_clobber (CC_Cmode, 72)));
}

/* ../../gcc/config/6502/6502.md:1941 */
rtx
gen_negate_highbit (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	gen_rtx_XOR (QImode,
	operand1,
	GEN_INT (128L))),
		gen_rtx_SET (gen_rtx_REG (CC_NZmode,
	68),
	gen_rtx_UNSPEC (CC_NZmode,
	gen_rtvec (1,
		copy_rtx (operand1)),
	2))));
}

/* ../../gcc/config/6502/6502.md:1972 */
rtx
gen_andqi3_insn (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	gen_rtx_AND (QImode,
	operand1,
	operand2)),
		gen_hard_reg_clobber (CC_NZmode, 68)));
}

/* ../../gcc/config/6502/6502.md:1991 */
rtx
gen_m65x_trbqi3 (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	gen_rtx_AND (QImode,
	gen_rtx_NOT (QImode,
	operand1),
	operand2)),
		gen_hard_reg_clobber (CC_NZmode, 68)));
}

/* ../../gcc/config/6502/6502.md:2001 */
rtx
gen_iorqi3 (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	gen_rtx_IOR (QImode,
	operand1,
	operand2)),
		gen_hard_reg_clobber (CC_NZmode, 68)));
}

/* ../../gcc/config/6502/6502.md:2023 */
rtx
gen_iorqi3_nz (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	gen_rtx_IOR (QImode,
	operand1,
	operand2)),
		gen_rtx_SET (gen_rtx_REG (CC_NZmode,
	68),
	gen_rtx_UNSPEC (CC_NZmode,
	gen_rtvec (2,
		copy_rtx (operand1),
		copy_rtx (operand2)),
	3))));
}

/* ../../gcc/config/6502/6502.md:2043 */
rtx
gen_xorqi3 (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	gen_rtx_XOR (QImode,
	operand1,
	operand2)),
		gen_hard_reg_clobber (CC_NZmode, 68)));
}

/* ../../gcc/config/6502/6502.md:2062 */
rtx
gen_loadqi_nz (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	operand1),
		gen_rtx_SET (gen_rtx_REG (CC_NZmode,
	68),
	gen_rtx_UNSPEC (CC_NZmode,
	gen_rtvec (1,
		copy_rtx (operand1)),
	9))));
}

/* ../../gcc/config/6502/6502.md:2136 */
rtx
gen_ashlqi3_insn (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (operand0,
	gen_rtx_ASHIFT (QImode,
	operand1,
	operand2)),
		gen_rtx_SET (gen_rtx_REG (CC_Cmode,
	72),
	gen_rtx_UNSPEC (CC_Cmode,
	gen_rtvec (2,
		copy_rtx (operand1),
		copy_rtx (operand2)),
	6)),
		gen_hard_reg_clobber (CC_NZmode, 68)));
}

/* ../../gcc/config/6502/6502.md:2153 */
rtx
gen_m65x_rorqi3 (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (operand0,
	gen_rtx_UNSPEC (QImode,
	gen_rtvec (2,
		operand1,
		gen_rtx_REG (CC_Cmode,
	72)),
	10)),
		gen_rtx_SET (gen_rtx_REG (CC_Cmode,
	72),
	gen_rtx_UNSPEC (CC_Cmode,
	gen_rtvec (2,
		copy_rtx (operand1),
		gen_rtx_REG (CC_Cmode,
	72)),
	10)),
		gen_hard_reg_clobber (CC_NZmode, 68)));
}

/* ../../gcc/config/6502/6502.md:2171 */
rtx
gen_m65x_rolqi3 (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (operand0,
	gen_rtx_UNSPEC (QImode,
	gen_rtvec (2,
		operand1,
		gen_rtx_REG (CC_Cmode,
	72)),
	11)),
		gen_rtx_SET (gen_rtx_REG (CC_Cmode,
	72),
	gen_rtx_UNSPEC (CC_Cmode,
	gen_rtvec (2,
		copy_rtx (operand1),
		gen_rtx_REG (CC_Cmode,
	72)),
	11)),
		gen_hard_reg_clobber (CC_NZmode, 68)));
}

/* ../../gcc/config/6502/6502.md:2246 */
rtx
gen_lshrqi3_insn (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (operand0,
	gen_rtx_LSHIFTRT (QImode,
	operand1,
	operand2)),
		gen_rtx_SET (gen_rtx_REG (CC_Cmode,
	72),
	gen_rtx_UNSPEC (CC_Cmode,
	gen_rtvec (2,
		copy_rtx (operand1),
		copy_rtx (operand2)),
	7)),
		gen_hard_reg_clobber (CC_NZmode, 68)));
}

/* ../../gcc/config/6502/6502.md:2263 */
rtx
gen_ashrqi3 (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (operand0,
	gen_rtx_ASHIFTRT (QImode,
	operand1,
	operand2)),
		gen_hard_reg_clobber (CC_Cmode, 72),
		gen_hard_reg_clobber (CC_NZmode, 68)));
}

/* ../../gcc/config/6502/6502.md:2296 */
rtx
gen_one_cmplqi2 (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	gen_rtx_NOT (QImode,
	operand1)),
		gen_hard_reg_clobber (CC_NZmode, 68)));
}

/* ../../gcc/config/6502/6502.md:2306 */
rtx
gen_compareqi (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (gen_rtx_REG (CC_Cmode,
	72),
	gen_rtx_COMPARE (CC_Cmode,
	operand0,
	operand1)),
		gen_rtx_SET (gen_rtx_REG (CC_NZmode,
	68),
	gen_rtx_COMPARE (CC_NZmode,
	copy_rtx (operand0),
	copy_rtx (operand1)))));
}

/* ../../gcc/config/6502/6502.md:2335 */
rtx
gen_condbranchqi_cc_c (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED)
{
  return gen_rtx_SET (pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_fmt_e (GET_CODE (operand0), VOIDmode,
		gen_rtx_REG (CC_Cmode,
	0)),
	gen_rtx_LABEL_REF (VOIDmode,
	operand1),
	pc_rtx));
}

/* ../../gcc/config/6502/6502.md:2335 */
rtx
gen_condbranchqi_cc_ui (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED)
{
  return gen_rtx_SET (pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_fmt_e (GET_CODE (operand0), VOIDmode,
		gen_rtx_REG (CC_UImode,
	0)),
	gen_rtx_LABEL_REF (VOIDmode,
	operand1),
	pc_rtx));
}

/* ../../gcc/config/6502/6502.md:2335 */
rtx
gen_condbranchqi_cc_v (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED)
{
  return gen_rtx_SET (pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_fmt_e (GET_CODE (operand0), VOIDmode,
		gen_rtx_REG (CC_Vmode,
	0)),
	gen_rtx_LABEL_REF (VOIDmode,
	operand1),
	pc_rtx));
}

/* ../../gcc/config/6502/6502.md:2335 */
rtx
gen_condbranchqi_cc_nz (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED)
{
  return gen_rtx_SET (pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_fmt_e (GET_CODE (operand0), VOIDmode,
		gen_rtx_REG (CC_NZmode,
	0)),
	gen_rtx_LABEL_REF (VOIDmode,
	operand1),
	pc_rtx));
}

/* ../../gcc/config/6502/6502.md:2353 */
rtx
gen_indirect_jump (rtx operand0 ATTRIBUTE_UNUSED)
{
  return gen_rtx_SET (pc_rtx,
	operand0);
}

/* ../../gcc/config/6502/6502.md:2360 */
rtx
gen_jump (rtx operand0 ATTRIBUTE_UNUSED)
{
  return gen_rtx_SET (pc_rtx,
	gen_rtx_LABEL_REF (VOIDmode,
	operand0));
}

/* ../../gcc/config/6502/6502.md:2411 */
rtx
gen_nop (void)
{
  return const0_rtx;
}

/* ../../gcc/config/6502/6502.md:2422 */
rtx
gen_call_sym (rtx operand0 ATTRIBUTE_UNUSED,
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

