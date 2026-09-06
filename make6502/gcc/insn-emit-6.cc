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

/* ../../gcc/config/6502/6502.md:974 */
rtx
gen_cbrhi_le (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_LE (VOIDmode,
	operand0,
	operand1),
	gen_rtx_LABEL_REF (VOIDmode,
	operand2),
	pc_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (QImode))));
}

/* ../../gcc/config/6502/6502.md:1080 */
rtx
gen_loadqi_zp (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	operand1),
		gen_hard_reg_clobber (CC_NZmode, 68)));
}

/* ../../gcc/config/6502/6502.md:1088 */
rtx
gen_storeqi_zp (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	operand1),
		gen_hard_reg_clobber (CC_NZmode, 68)));
}

/* ../../gcc/config/6502/6502.md:1096 */
rtx
gen_movqi_noclob (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED)
{
  return gen_rtx_SET (operand0,
	operand1);
}

/* ../../gcc/config/6502/6502.md:1113 */
rtx
gen_movqi_insn (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	operand1),
		gen_hard_reg_clobber (CC_NZmode, 68)));
}

/* ../../gcc/config/6502/6502.md:1442 */
rtx
gen_loadqi_xind (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_SET (operand0,
	gen_rtx_MEM (QImode,
	gen_rtx_UNSPEC (QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	20)));
}

/* ../../gcc/config/6502/6502.md:1451 */
rtx
gen_loadqi_xind_nz (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	gen_rtx_MEM (QImode,
	gen_rtx_UNSPEC (QImode,
	gen_rtvec (2,
		operand1,
		operand2),
	20))),
		gen_hard_reg_clobber (CC_NZmode, 68)));
}

/* ../../gcc/config/6502/6502.md:1469 */
rtx
gen_storeqi_xind (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_SET (gen_rtx_MEM (QImode,
	gen_rtx_UNSPEC (QImode,
	gen_rtvec (2,
		operand0,
		operand1),
	20)),
	operand2);
}

/* ../../gcc/config/6502/6502.md:1478 */
rtx
gen_zp_clob_hardreg_movqi (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (operand0,
	operand1),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (QImode)),
		gen_hard_reg_clobber (CC_NZmode, 68)));
}

/* ../../gcc/config/6502/6502.md:1613 */
rtx
gen_pushflags (rtx operand0 ATTRIBUTE_UNUSED)
{
  return gen_rtx_SET (operand0,
	gen_rtx_UNSPEC_VOLATILE (CCmode,
	gen_rtvec (3,
		gen_rtx_REG (CC_NZmode,
	68),
		gen_rtx_REG (CC_Cmode,
	72),
		gen_rtx_REG (CC_Vmode,
	76)),
	16));
}

/* ../../gcc/config/6502/6502.md:1622 */
rtx
gen_popflags (rtx operand0 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (3,
		gen_rtx_SET (gen_rtx_REG (CC_NZmode,
	68),
	gen_rtx_UNSPEC_VOLATILE (CC_NZmode,
	gen_rtvec (1,
		operand0),
	17)),
		gen_rtx_SET (gen_rtx_REG (CC_Cmode,
	72),
	gen_rtx_UNSPEC_VOLATILE (CC_Cmode,
	gen_rtvec (1,
		const0_rtx),
	18)),
		gen_rtx_SET (gen_rtx_REG (CC_Vmode,
	76),
	gen_rtx_UNSPEC_VOLATILE (CC_Vmode,
	gen_rtvec (1,
		const0_rtx),
	19))));
}

/* ../../gcc/config/6502/6502.md:1642 */
rtx
gen_movqi_loadstore_zp (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	operand1),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (QImode))));
}

/* ../../gcc/config/6502/6502.md:1670 */
rtx
gen_incdecqi3 (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	gen_rtx_PLUS (QImode,
	operand1,
	operand2)),
		gen_hard_reg_clobber (CC_NZmode, 68)));
}

/* ../../gcc/config/6502/6502.md:1670 */
rtx
gen_incdecqi3_nz (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (operand0,
	gen_rtx_PLUS (QImode,
	operand1,
	operand2)),
		gen_rtx_SET (gen_rtx_REG (CC_NZmode,
	68),
	gen_rtx_UNSPEC (CC_NZmode,
	gen_rtvec (2,
		copy_rtx (operand1),
		copy_rtx (operand2)),
	8))));
}

/* ../../gcc/config/6502/6502.md:1718 */
rtx
gen_addqi3 (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (operand0,
	gen_rtx_PLUS (QImode,
	operand1,
	operand2)),
		gen_hard_reg_clobber (CC_NZmode, 68),
		gen_hard_reg_clobber (CC_Cmode, 72),
		gen_hard_reg_clobber (CC_Vmode, 76)));
}

/* ../../gcc/config/6502/6502.md:1718 */
rtx
gen_addqi3_noclob (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_SET (operand0,
	gen_rtx_PLUS (QImode,
	operand1,
	operand2));
}

/* ../../gcc/config/6502/6502.md:1786 */
rtx
gen_subqi3 (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (operand0,
	gen_rtx_MINUS (QImode,
	operand1,
	operand2)),
		gen_hard_reg_clobber (CC_NZmode, 68),
		gen_hard_reg_clobber (CC_Cmode, 72),
		gen_hard_reg_clobber (CC_Vmode, 76)));
}

/* ../../gcc/config/6502/6502.md:1802 */
rtx
gen_clc (void)
{
  return gen_rtx_SET (gen_rtx_REG (CC_Cmode,
	72),
	gen_rtx_UNSPEC (CC_Cmode,
	gen_rtvec (1,
		const0_rtx),
	21));
}

/* ../../gcc/config/6502/6502.md:1808 */
rtx
gen_sec (void)
{
  return gen_rtx_SET (gen_rtx_REG (CC_Cmode,
	72),
	gen_rtx_UNSPEC (CC_Cmode,
	gen_rtvec (1,
		const0_rtx),
	22));
}

/* ../../gcc/config/6502/6502.md:1837 */
rtx
gen_adcqi3 (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (operand0,
	gen_rtx_PLUS (QImode,
	gen_rtx_PLUS (QImode,
	operand1,
	operand2),
	gen_rtx_NE (QImode,
	gen_rtx_REG (CC_Cmode,
	72),
	const0_rtx))),
		gen_hard_reg_clobber (CC_NZmode, 68),
		gen_hard_reg_clobber (CC_Cmode, 72),
		gen_hard_reg_clobber (CC_Vmode, 76)));
}

/* ../../gcc/config/6502/6502.md:1837 */
rtx
gen_adcqi3_c (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (operand0,
	gen_rtx_PLUS (QImode,
	gen_rtx_PLUS (QImode,
	operand1,
	operand2),
	gen_rtx_NE (QImode,
	gen_rtx_REG (CC_Cmode,
	72),
	const0_rtx))),
		gen_rtx_SET (gen_rtx_REG (CC_Cmode,
	72),
	gen_rtx_UNSPEC (CC_Cmode,
	gen_rtvec (3,
		copy_rtx (operand1),
		copy_rtx (operand2),
		gen_rtx_REG (CC_Cmode,
	72)),
	4)),
		gen_hard_reg_clobber (CC_NZmode, 68),
		gen_hard_reg_clobber (CC_Vmode, 76)));
}

/* ../../gcc/config/6502/6502.md:1882 */
rtx
gen_sbcqi3 (rtx operand0 ATTRIBUTE_UNUSED,
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
	gen_rtx_EQ (QImode,
	gen_rtx_REG (CC_Cmode,
	72),
	const0_rtx)))),
		gen_hard_reg_clobber (CC_NZmode, 68),
		gen_hard_reg_clobber (CC_Cmode, 72),
		gen_hard_reg_clobber (CC_Vmode, 76)));
}

/* ../../gcc/config/6502/6502.md:1882 */
rtx
gen_sbcqi3_c (rtx operand0 ATTRIBUTE_UNUSED,
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
	gen_rtx_EQ (QImode,
	gen_rtx_REG (CC_Cmode,
	72),
	const0_rtx)))),
		gen_rtx_SET (gen_rtx_REG (CC_Cmode,
	72),
	gen_rtx_UNSPEC (CC_Cmode,
	gen_rtvec (3,
		copy_rtx (operand1),
		copy_rtx (operand2),
		gen_rtx_REG (CC_Cmode,
	72)),
	5)),
		gen_hard_reg_clobber (CC_NZmode, 68),
		gen_hard_reg_clobber (CC_Vmode, 76)));
}

