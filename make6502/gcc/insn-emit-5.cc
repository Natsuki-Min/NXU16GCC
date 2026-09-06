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

/* ../../gcc/config/6502/6502.md:230 */
rtx
gen_movhi_noclob (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED)
{
  return gen_rtx_SET (operand0,
	operand1);
}

/* ../../gcc/config/6502/6502.md:230 */
rtx
gen_movsi_noclob (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED)
{
  return gen_rtx_SET (operand0,
	operand1);
}

/* ../../gcc/config/6502/6502.md:460 */
rtx
gen_addhi3_noclobacc (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (4,
		gen_rtx_SET (operand0,
	gen_rtx_PLUS (HImode,
	operand1,
	operand2)),
		gen_hard_reg_clobber (CC_NZmode, 68),
		gen_hard_reg_clobber (CC_Cmode, 72),
		gen_hard_reg_clobber (CC_Vmode, 76)));
}

/* ../../gcc/config/6502/6502.md:476 */
rtx
gen_addhi3_clobacc (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (5,
		gen_rtx_SET (operand0,
	gen_rtx_PLUS (HImode,
	operand1,
	operand2)),
		gen_hard_reg_clobber (CC_NZmode, 68),
		gen_hard_reg_clobber (CC_Cmode, 72),
		gen_hard_reg_clobber (CC_Vmode, 76),
		gen_hard_reg_clobber (QImode, 0)));
}

/* ../../gcc/config/6502/6502.md:494 */
rtx
gen_addhi3_noclob (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_SET (operand0,
	gen_rtx_PLUS (HImode,
	operand1,
	operand2));
}

/* ../../gcc/config/6502/6502.md:821 */
rtx
gen_cbrhi_eq (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_EQ (VOIDmode,
	operand0,
	operand1),
	gen_rtx_LABEL_REF (VOIDmode,
	operand2),
	pc_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (QImode))));
}

/* ../../gcc/config/6502/6502.md:838 */
rtx
gen_cbrhi_ne (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_NE (VOIDmode,
	operand0,
	operand1),
	gen_rtx_LABEL_REF (VOIDmode,
	operand2),
	pc_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (QImode))));
}

/* ../../gcc/config/6502/6502.md:855 */
rtx
gen_cbrhi_ltu (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_LTU (VOIDmode,
	operand0,
	operand1),
	gen_rtx_LABEL_REF (VOIDmode,
	operand2),
	pc_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (QImode))));
}

/* ../../gcc/config/6502/6502.md:872 */
rtx
gen_cbrhi_gtu (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_GTU (VOIDmode,
	operand0,
	operand1),
	gen_rtx_LABEL_REF (VOIDmode,
	operand2),
	pc_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (QImode))));
}

/* ../../gcc/config/6502/6502.md:889 */
rtx
gen_cbrhi_geu (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_GEU (VOIDmode,
	operand0,
	operand1),
	gen_rtx_LABEL_REF (VOIDmode,
	operand2),
	pc_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (QImode))));
}

/* ../../gcc/config/6502/6502.md:906 */
rtx
gen_cbrhi_leu (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_LEU (VOIDmode,
	operand0,
	operand1),
	gen_rtx_LABEL_REF (VOIDmode,
	operand2),
	pc_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (QImode))));
}

/* ../../gcc/config/6502/6502.md:923 */
rtx
gen_cbrhi_lt (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_LT (VOIDmode,
	operand0,
	operand1),
	gen_rtx_LABEL_REF (VOIDmode,
	operand2),
	pc_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (QImode))));
}

/* ../../gcc/config/6502/6502.md:940 */
rtx
gen_cbrhi_gt (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_GT (VOIDmode,
	operand0,
	operand1),
	gen_rtx_LABEL_REF (VOIDmode,
	operand2),
	pc_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (QImode))));
}

/* ../../gcc/config/6502/6502.md:957 */
rtx
gen_cbrhi_ge (rtx operand0 ATTRIBUTE_UNUSED,
	rtx operand1 ATTRIBUTE_UNUSED,
	rtx operand2 ATTRIBUTE_UNUSED)
{
  return gen_rtx_PARALLEL (VOIDmode,
	gen_rtvec (2,
		gen_rtx_SET (pc_rtx,
	gen_rtx_IF_THEN_ELSE (VOIDmode,
	gen_rtx_GE (VOIDmode,
	operand0,
	operand1),
	gen_rtx_LABEL_REF (VOIDmode,
	operand2),
	pc_rtx)),
		gen_rtx_CLOBBER (VOIDmode,
	gen_rtx_SCRATCH (QImode))));
}

