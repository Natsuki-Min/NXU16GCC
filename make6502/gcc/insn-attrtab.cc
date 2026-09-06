/* Generated automatically by the program `genattrtab'
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
#include "options.h"
#include "varasm.h"
#include "stor-layout.h"
#include "calls.h"
#include "insn-attr.h"
#include "memmodel.h"
#include "tm_p.h"
#include "insn-config.h"
#include "recog.h"
#include "regs.h"
#include "real.h"
#include "output.h"
#include "toplev.h"
#include "flags.h"
#include "emit-rtl.h"

#define operands recog_data.operand

int
insn_current_length (rtx_insn *insn ATTRIBUTE_UNUSED)
{
  switch (recog_memoized (insn))
    {
    case 56:  /* condbranchqi_cc_nz */
      extract_insn_cached (insn);
      if ((((INSN_ADDRESSES_SET_P () ? INSN_ADDRESSES (INSN_UID (GET_CODE (operands[1]) == LABEL_REF ? XEXP (operands[1], 0) : operands[1])) : 0) - (insn_current_reference_address (insn))) >= (-110)) && (((INSN_ADDRESSES_SET_P () ? INSN_ADDRESSES (INSN_UID (GET_CODE (operands[1]) == LABEL_REF ? XEXP (operands[1], 0) : operands[1])) : 0) - (insn_current_reference_address (insn))) < (110)))
        {
	  return 2;
        }
      else
        {
	  return 5;
        }

    case 55:  /* condbranchqi_cc_v */
      extract_insn_cached (insn);
      if ((((INSN_ADDRESSES_SET_P () ? INSN_ADDRESSES (INSN_UID (GET_CODE (operands[1]) == LABEL_REF ? XEXP (operands[1], 0) : operands[1])) : 0) - (insn_current_reference_address (insn))) >= (-110)) && (((INSN_ADDRESSES_SET_P () ? INSN_ADDRESSES (INSN_UID (GET_CODE (operands[1]) == LABEL_REF ? XEXP (operands[1], 0) : operands[1])) : 0) - (insn_current_reference_address (insn))) < (110)))
        {
	  return 2;
        }
      else
        {
	  return 5;
        }

    case 54:  /* condbranchqi_cc_ui */
      extract_insn_cached (insn);
      if ((((INSN_ADDRESSES_SET_P () ? INSN_ADDRESSES (INSN_UID (GET_CODE (operands[1]) == LABEL_REF ? XEXP (operands[1], 0) : operands[1])) : 0) - (insn_current_reference_address (insn))) >= (-110)) && (((INSN_ADDRESSES_SET_P () ? INSN_ADDRESSES (INSN_UID (GET_CODE (operands[1]) == LABEL_REF ? XEXP (operands[1], 0) : operands[1])) : 0) - (insn_current_reference_address (insn))) < (110)))
        {
	  return 2;
        }
      else
        {
	  return 5;
        }

    case 53:  /* condbranchqi_cc_c */
      extract_insn_cached (insn);
      if ((((INSN_ADDRESSES_SET_P () ? INSN_ADDRESSES (INSN_UID (GET_CODE (operands[1]) == LABEL_REF ? XEXP (operands[1], 0) : operands[1])) : 0) - (insn_current_reference_address (insn))) >= (-110)) && (((INSN_ADDRESSES_SET_P () ? INSN_ADDRESSES (INSN_UID (GET_CODE (operands[1]) == LABEL_REF ? XEXP (operands[1], 0) : operands[1])) : 0) - (insn_current_reference_address (insn))) < (110)))
        {
	  return 2;
        }
      else
        {
	  return 5;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
      /* FALLTHRU */
    default:
      return 0;

    }
}

int
insn_variable_length_p (rtx_insn *insn ATTRIBUTE_UNUSED)
{
  switch (recog_memoized (insn))
    {
    case 56:  /* condbranchqi_cc_nz */
    case 55:  /* condbranchqi_cc_v */
    case 54:  /* condbranchqi_cc_ui */
    case 53:  /* condbranchqi_cc_c */
      return 1;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
      /* FALLTHRU */
    default:
      return 0;

    }
}

int
insn_min_length (rtx_insn *insn ATTRIBUTE_UNUSED)
{
  switch (recog_memoized (insn))
    {
    case 52:  /* compareqi */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0x3ULL))
        {
	  return 2;
        }
      else if (which_alternative == 2)
        {
	  return 3;
        }
      else if (((1ULL << which_alternative) & 0x38ULL))
        {
	  return 2;
        }
      else
        {
	  return 3;
        }

    case 42:  /* iorqi3 */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0xfULL))
        {
	  return 2;
        }
      else if (which_alternative == 4)
        {
	  return 3;
        }
      else if (which_alternative == 5)
        {
	  return 2;
        }
      else if (which_alternative == 6)
        {
	  return 3;
        }
      else if (which_alternative == 7)
        {
	  return 2;
        }
      else
        {
	  return 3;
        }

    case 45:  /* loadqi_nz */
    case 44:  /* xorqi3 */
    case 43:  /* iorqi3_nz */
    case 40:  /* andqi3_insn */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0xfULL))
        {
	  return 2;
        }
      else if (which_alternative == 4)
        {
	  return 3;
        }
      else if (which_alternative == 5)
        {
	  return 2;
        }
      else
        {
	  return 3;
        }

    case 38:  /* sbcqi3_nzv */
    case 37:  /* sbcqi3_c */
    case 36:  /* sbcqi3 */
    case 35:  /* adcqi3_c */
    case 34:  /* adcqi3 */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0xfULL))
        {
	  return 2;
        }
      else
        {
	  return 3;
        }

    case 31:  /* subqi3 */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0x7ULL))
        {
	  return 3;
        }
      else
        {
	  return 4;
        }

    case 28:  /* incdecqi3_nz */
    case 27:  /* incdecqi3 */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0x3fULL))
        {
	  return 1;
        }
      else if (((1ULL << which_alternative) & 0xc0ULL))
        {
	  return 2;
        }
      else if (((1ULL << which_alternative) & 0xf00ULL))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 23:  /* zp_clob_hardreg_movqi */
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 4;
        }
      else if (((1ULL << which_alternative) & 0x6ULL))
        {
	  return 5;
        }
      else
        {
	  return 6;
        }

    case 19:  /* movqi_insn */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0x7ULL))
        {
	  return 2;
        }
      else if (((1ULL << which_alternative) & 0x18ULL))
        {
	  return 3;
        }
      else if (which_alternative == 5)
        {
	  return 4;
        }
      else if (((1ULL << which_alternative) & 0x3c0ULL))
        {
	  return 1;
        }
      else if (((1ULL << which_alternative) & 0xc00ULL))
        {
	  return 8;
        }
      else if (which_alternative == 12)
        {
	  return 2;
        }
      else if (which_alternative == 13)
        {
	  return 3;
        }
      else if (which_alternative == 14)
        {
	  return 9;
        }
      else if (which_alternative == 15)
        {
	  return 13 /* 0xd */;
        }
      else if (((1ULL << which_alternative) & 0x30000ULL))
        {
	  return 14 /* 0xe */;
        }
      else
        {
	  return 0;
        }

    case 18:  /* movqi_noclob */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0x7ULL))
        {
	  return 4;
        }
      else if (((1ULL << which_alternative) & 0x18ULL))
        {
	  return 5;
        }
      else if (which_alternative == 5)
        {
	  return 6;
        }
      else if (((1ULL << which_alternative) & 0x3c0ULL))
        {
	  return 3;
        }
      else if (((1ULL << which_alternative) & 0xc00ULL))
        {
	  return 10 /* 0xa */;
        }
      else if (which_alternative == 12)
        {
	  return 4;
        }
      else if (which_alternative == 13)
        {
	  return 5;
        }
      else if (which_alternative == 14)
        {
	  return 11 /* 0xb */;
        }
      else if (which_alternative == 15)
        {
	  return 15 /* 0xf */;
        }
      else if (((1ULL << which_alternative) & 0x30000ULL))
        {
	  return 16 /* 0x10 */;
        }
      else
        {
	  return 0;
        }

    case 68:  /* m65x_restorestack_s7s0_rts */
    case 67:  /* m65x_restorestack_s7s0_sp_rts */
    case 66:  /* m65x_savestack_s7s0 */
    case 65:  /* m65x_savestack_s7s0_sp */
    case 62:  /* call_value_sym */
    case 60:  /* call_sym */
    case 58:  /* jump */
      return 3;

    case 64:  /* m65x_return */
    case 33:  /* sec */
    case 32:  /* clc */
    case 25:  /* popflags */
    case 24:  /* pushflags */
      return 1;

    case 63:  /* call_indirect_sym */
    case 61:  /* call_indirect */
      return 9;

    case 50:  /* ashrqi3 */
      extract_constrain_insn_cached (insn);
      if (
#line 2286 "../../gcc/config/6502/6502.md"
(INTVAL (operands[2]) == 1))
        {
	  return 3;
        }
      else if (
#line 2287 "../../gcc/config/6502/6502.md"
(INTVAL (operands[2]) == 2))
        {
	  return 6;
        }
      else if (
#line 2288 "../../gcc/config/6502/6502.md"
(INTVAL (operands[2]) == 3))
        {
	  return 9;
        }
      else if (
#line 2289 "../../gcc/config/6502/6502.md"
(INTVAL (operands[2]) == 4))
        {
	  return 10 /* 0xa */;
        }
      else if (
#line 2290 "../../gcc/config/6502/6502.md"
(INTVAL (operands[2]) == 5))
        {
	  return 11 /* 0xb */;
        }
      else if (
#line 2291 "../../gcc/config/6502/6502.md"
(INTVAL (operands[2]) == 6))
        {
	  return 11 /* 0xb */;
        }
      else if (
#line 2292 "../../gcc/config/6502/6502.md"
(INTVAL (operands[2]) == 7))
        {
	  return 6;
        }
      else if (
#line 2293 "../../gcc/config/6502/6502.md"
(INTVAL (operands[2]) == 8))
        {
	  return 6;
        }
      else
        {
	  return 0;
        }

    case 49:  /* lshrqi3_insn */
    case 48:  /* m65x_rolqi3 */
    case 47:  /* m65x_rorqi3 */
    case 46:  /* ashlqi3_insn */
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 1;
        }
      else if (which_alternative == 1)
        {
	  return 2;
        }
      else if (which_alternative == 2)
        {
	  return 3;
        }
      else if (which_alternative == 3)
        {
	  return 2;
        }
      else
        {
	  return 3;
        }

    case 41:  /* m65x_trbqi3 */
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 2;
        }
      else
        {
	  return 3;
        }

    case 30:  /* addqi3_noclob */
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 5;
        }
      else if (which_alternative == 1)
        {
	  return 5;
        }
      else if (which_alternative == 2)
        {
	  return 5;
        }
      else if (which_alternative == 3)
        {
	  return 6;
        }
      else if (which_alternative == 4)
        {
	  return 6;
        }
      else if (which_alternative == 5)
        {
	  return 3;
        }
      else if (which_alternative == 6)
        {
	  return 3;
        }
      else if (which_alternative == 7)
        {
	  return 5;
        }
      else if (which_alternative == 8)
        {
	  return 3;
        }
      else if (which_alternative == 9)
        {
	  return 3;
        }
      else if (which_alternative == 10)
        {
	  return 4;
        }
      else if (which_alternative == 11)
        {
	  return 5;
        }
      else if (which_alternative == 12)
        {
	  return 5;
        }
      else if (which_alternative == 13)
        {
	  return 4;
        }
      else if (which_alternative == 14)
        {
	  return 5;
        }
      else if (which_alternative == 15)
        {
	  return 3;
        }
      else if (which_alternative == 16)
        {
	  return 3;
        }
      else if (which_alternative == 17)
        {
	  return 4;
        }
      else if (which_alternative == 18)
        {
	  return 5;
        }
      else if (which_alternative == 19)
        {
	  return 5;
        }
      else
        {
	  return 4;
        }

    case 29:  /* addqi3 */
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 3;
        }
      else if (which_alternative == 1)
        {
	  return 3;
        }
      else if (which_alternative == 2)
        {
	  return 3;
        }
      else if (which_alternative == 3)
        {
	  return 4;
        }
      else if (which_alternative == 4)
        {
	  return 4;
        }
      else if (which_alternative == 5)
        {
	  return 1;
        }
      else if (which_alternative == 6)
        {
	  return 1;
        }
      else if (which_alternative == 7)
        {
	  return 3;
        }
      else if (which_alternative == 8)
        {
	  return 1;
        }
      else if (which_alternative == 9)
        {
	  return 1;
        }
      else if (which_alternative == 10)
        {
	  return 2;
        }
      else if (which_alternative == 11)
        {
	  return 3;
        }
      else if (which_alternative == 12)
        {
	  return 3;
        }
      else if (which_alternative == 13)
        {
	  return 2;
        }
      else if (which_alternative == 14)
        {
	  return 3;
        }
      else if (which_alternative == 15)
        {
	  return 1;
        }
      else if (which_alternative == 16)
        {
	  return 1;
        }
      else if (which_alternative == 17)
        {
	  return 2;
        }
      else if (which_alternative == 18)
        {
	  return 3;
        }
      else if (which_alternative == 19)
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 26:  /* movqi_loadstore_zp */
    case 20:  /* loadqi_xind */
      return 4;

    case 5:  /* addhi3_noclob */
      return 19 /* 0x13 */;

    case 4:  /* addhi3_clobacc */
      return 13 /* 0xd */;

    case 3:  /* addhi3_noclobacc */
      return 17 /* 0x11 */;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
      /* FALLTHRU */
    default:
      return 2;

    }
}

int
insn_default_length (rtx_insn *insn ATTRIBUTE_UNUSED)
{
  switch (recog_memoized (insn))
    {
    case 52:  /* compareqi */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0x3ULL))
        {
	  return 2;
        }
      else if (which_alternative == 2)
        {
	  return 3;
        }
      else if (((1ULL << which_alternative) & 0x38ULL))
        {
	  return 2;
        }
      else
        {
	  return 3;
        }

    case 42:  /* iorqi3 */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0xfULL))
        {
	  return 2;
        }
      else if (which_alternative == 4)
        {
	  return 3;
        }
      else if (which_alternative == 5)
        {
	  return 2;
        }
      else if (which_alternative == 6)
        {
	  return 3;
        }
      else if (which_alternative == 7)
        {
	  return 2;
        }
      else
        {
	  return 3;
        }

    case 45:  /* loadqi_nz */
    case 44:  /* xorqi3 */
    case 43:  /* iorqi3_nz */
    case 40:  /* andqi3_insn */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0xfULL))
        {
	  return 2;
        }
      else if (which_alternative == 4)
        {
	  return 3;
        }
      else if (which_alternative == 5)
        {
	  return 2;
        }
      else
        {
	  return 3;
        }

    case 38:  /* sbcqi3_nzv */
    case 37:  /* sbcqi3_c */
    case 36:  /* sbcqi3 */
    case 35:  /* adcqi3_c */
    case 34:  /* adcqi3 */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0xfULL))
        {
	  return 2;
        }
      else
        {
	  return 3;
        }

    case 31:  /* subqi3 */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0x7ULL))
        {
	  return 3;
        }
      else
        {
	  return 4;
        }

    case 28:  /* incdecqi3_nz */
    case 27:  /* incdecqi3 */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0x3fULL))
        {
	  return 1;
        }
      else if (((1ULL << which_alternative) & 0xc0ULL))
        {
	  return 2;
        }
      else if (((1ULL << which_alternative) & 0xf00ULL))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 23:  /* zp_clob_hardreg_movqi */
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 4;
        }
      else if (((1ULL << which_alternative) & 0x6ULL))
        {
	  return 5;
        }
      else
        {
	  return 6;
        }

    case 19:  /* movqi_insn */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0x7ULL))
        {
	  return 2;
        }
      else if (((1ULL << which_alternative) & 0x18ULL))
        {
	  return 3;
        }
      else if (which_alternative == 5)
        {
	  return 4;
        }
      else if (((1ULL << which_alternative) & 0x3c0ULL))
        {
	  return 1;
        }
      else if (((1ULL << which_alternative) & 0xc00ULL))
        {
	  return 8;
        }
      else if (which_alternative == 12)
        {
	  return 2;
        }
      else if (which_alternative == 13)
        {
	  return 3;
        }
      else if (which_alternative == 14)
        {
	  return 9;
        }
      else if (which_alternative == 15)
        {
	  return 13 /* 0xd */;
        }
      else if (((1ULL << which_alternative) & 0x30000ULL))
        {
	  return 14 /* 0xe */;
        }
      else
        {
	  return 0;
        }

    case 18:  /* movqi_noclob */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0x7ULL))
        {
	  return 4;
        }
      else if (((1ULL << which_alternative) & 0x18ULL))
        {
	  return 5;
        }
      else if (which_alternative == 5)
        {
	  return 6;
        }
      else if (((1ULL << which_alternative) & 0x3c0ULL))
        {
	  return 3;
        }
      else if (((1ULL << which_alternative) & 0xc00ULL))
        {
	  return 10 /* 0xa */;
        }
      else if (which_alternative == 12)
        {
	  return 4;
        }
      else if (which_alternative == 13)
        {
	  return 5;
        }
      else if (which_alternative == 14)
        {
	  return 11 /* 0xb */;
        }
      else if (which_alternative == 15)
        {
	  return 15 /* 0xf */;
        }
      else if (((1ULL << which_alternative) & 0x30000ULL))
        {
	  return 16 /* 0x10 */;
        }
      else
        {
	  return 0;
        }

    case 68:  /* m65x_restorestack_s7s0_rts */
    case 67:  /* m65x_restorestack_s7s0_sp_rts */
    case 66:  /* m65x_savestack_s7s0 */
    case 65:  /* m65x_savestack_s7s0_sp */
    case 62:  /* call_value_sym */
    case 60:  /* call_sym */
    case 58:  /* jump */
      return 3;

    case 64:  /* m65x_return */
    case 33:  /* sec */
    case 32:  /* clc */
    case 25:  /* popflags */
    case 24:  /* pushflags */
      return 1;

    case 63:  /* call_indirect_sym */
    case 61:  /* call_indirect */
      return 9;

    case 56:  /* condbranchqi_cc_nz */
    case 55:  /* condbranchqi_cc_v */
    case 54:  /* condbranchqi_cc_ui */
    case 53:  /* condbranchqi_cc_c */
      return 5;

    case 50:  /* ashrqi3 */
      extract_constrain_insn_cached (insn);
      if (
#line 2286 "../../gcc/config/6502/6502.md"
(INTVAL (operands[2]) == 1))
        {
	  return 3;
        }
      else if (
#line 2287 "../../gcc/config/6502/6502.md"
(INTVAL (operands[2]) == 2))
        {
	  return 6;
        }
      else if (
#line 2288 "../../gcc/config/6502/6502.md"
(INTVAL (operands[2]) == 3))
        {
	  return 9;
        }
      else if (
#line 2289 "../../gcc/config/6502/6502.md"
(INTVAL (operands[2]) == 4))
        {
	  return 10 /* 0xa */;
        }
      else if (
#line 2290 "../../gcc/config/6502/6502.md"
(INTVAL (operands[2]) == 5))
        {
	  return 11 /* 0xb */;
        }
      else if (
#line 2291 "../../gcc/config/6502/6502.md"
(INTVAL (operands[2]) == 6))
        {
	  return 11 /* 0xb */;
        }
      else if (
#line 2292 "../../gcc/config/6502/6502.md"
(INTVAL (operands[2]) == 7))
        {
	  return 6;
        }
      else if (
#line 2293 "../../gcc/config/6502/6502.md"
(INTVAL (operands[2]) == 8))
        {
	  return 6;
        }
      else
        {
	  return 0;
        }

    case 49:  /* lshrqi3_insn */
    case 48:  /* m65x_rolqi3 */
    case 47:  /* m65x_rorqi3 */
    case 46:  /* ashlqi3_insn */
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 1;
        }
      else if (which_alternative == 1)
        {
	  return 2;
        }
      else if (which_alternative == 2)
        {
	  return 3;
        }
      else if (which_alternative == 3)
        {
	  return 2;
        }
      else
        {
	  return 3;
        }

    case 41:  /* m65x_trbqi3 */
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 2;
        }
      else
        {
	  return 3;
        }

    case 30:  /* addqi3_noclob */
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 5;
        }
      else if (which_alternative == 1)
        {
	  return 5;
        }
      else if (which_alternative == 2)
        {
	  return 5;
        }
      else if (which_alternative == 3)
        {
	  return 6;
        }
      else if (which_alternative == 4)
        {
	  return 6;
        }
      else if (which_alternative == 5)
        {
	  return 3;
        }
      else if (which_alternative == 6)
        {
	  return 3;
        }
      else if (which_alternative == 7)
        {
	  return 5;
        }
      else if (which_alternative == 8)
        {
	  return 3;
        }
      else if (which_alternative == 9)
        {
	  return 3;
        }
      else if (which_alternative == 10)
        {
	  return 4;
        }
      else if (which_alternative == 11)
        {
	  return 5;
        }
      else if (which_alternative == 12)
        {
	  return 5;
        }
      else if (which_alternative == 13)
        {
	  return 4;
        }
      else if (which_alternative == 14)
        {
	  return 5;
        }
      else if (which_alternative == 15)
        {
	  return 3;
        }
      else if (which_alternative == 16)
        {
	  return 3;
        }
      else if (which_alternative == 17)
        {
	  return 4;
        }
      else if (which_alternative == 18)
        {
	  return 5;
        }
      else if (which_alternative == 19)
        {
	  return 5;
        }
      else
        {
	  return 4;
        }

    case 29:  /* addqi3 */
      extract_constrain_insn_cached (insn);
      if (which_alternative == 0)
        {
	  return 3;
        }
      else if (which_alternative == 1)
        {
	  return 3;
        }
      else if (which_alternative == 2)
        {
	  return 3;
        }
      else if (which_alternative == 3)
        {
	  return 4;
        }
      else if (which_alternative == 4)
        {
	  return 4;
        }
      else if (which_alternative == 5)
        {
	  return 1;
        }
      else if (which_alternative == 6)
        {
	  return 1;
        }
      else if (which_alternative == 7)
        {
	  return 3;
        }
      else if (which_alternative == 8)
        {
	  return 1;
        }
      else if (which_alternative == 9)
        {
	  return 1;
        }
      else if (which_alternative == 10)
        {
	  return 2;
        }
      else if (which_alternative == 11)
        {
	  return 3;
        }
      else if (which_alternative == 12)
        {
	  return 3;
        }
      else if (which_alternative == 13)
        {
	  return 2;
        }
      else if (which_alternative == 14)
        {
	  return 3;
        }
      else if (which_alternative == 15)
        {
	  return 1;
        }
      else if (which_alternative == 16)
        {
	  return 1;
        }
      else if (which_alternative == 17)
        {
	  return 2;
        }
      else if (which_alternative == 18)
        {
	  return 3;
        }
      else if (which_alternative == 19)
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 26:  /* movqi_loadstore_zp */
    case 20:  /* loadqi_xind */
      return 4;

    case 5:  /* addhi3_noclob */
      return 19 /* 0x13 */;

    case 4:  /* addhi3_clobacc */
      return 13 /* 0xd */;

    case 3:  /* addhi3_noclobacc */
      return 17 /* 0x11 */;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
      /* FALLTHRU */
    default:
      return 2;

    }
}

int
num_delay_slots (rtx_insn *insn ATTRIBUTE_UNUSED)
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
      /* FALLTHRU */
    default:
      return 0;

    }
}

attr_adcqi3
get_attr_adcqi3 (rtx_insn *insn ATTRIBUTE_UNUSED)
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
      /* FALLTHRU */
    default:
      return ADCQI3_NO;

    }
}

attr_addqi3
get_attr_addqi3 (rtx_insn *insn ATTRIBUTE_UNUSED)
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
      /* FALLTHRU */
    default:
      return ADDQI3_NO;

    }
}

attr_arch_enabled
get_attr_arch_enabled (rtx_insn *insn ATTRIBUTE_UNUSED)
{
  attr_arch cached_arch ATTRIBUTE_UNUSED;

  switch (recog_memoized (insn))
    {
    case 52:  /* compareqi */
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 4) || ((which_alternative == 4) && (
#line 34 "../../gcc/config/6502/6502.md"
(TARGET_CMOS))))
        {
	  return ARCH_ENABLED_YES;
        }
      else
        {
	  return ARCH_ENABLED_NO;
        }

    case 42:  /* iorqi3 */
      extract_constrain_insn_cached (insn);
      if ((((1ULL << which_alternative) & 0x77ULL)) || ((!((1ULL << which_alternative) & 0x77ULL)) && (
#line 34 "../../gcc/config/6502/6502.md"
(TARGET_CMOS))))
        {
	  return ARCH_ENABLED_YES;
        }
      else
        {
	  return ARCH_ENABLED_NO;
        }

    case 41:  /* m65x_trbqi3 */
      extract_constrain_insn_cached (insn);
      if (
#line 34 "../../gcc/config/6502/6502.md"
(TARGET_CMOS))
        {
	  return ARCH_ENABLED_YES;
        }
      else
        {
	  return ARCH_ENABLED_NO;
        }

    case 45:  /* loadqi_nz */
    case 44:  /* xorqi3 */
    case 43:  /* iorqi3_nz */
    case 40:  /* andqi3_insn */
    case 38:  /* sbcqi3_nzv */
    case 37:  /* sbcqi3_c */
    case 36:  /* sbcqi3 */
    case 35:  /* adcqi3_c */
    case 34:  /* adcqi3 */
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 3) || ((which_alternative == 3) && (
#line 34 "../../gcc/config/6502/6502.md"
(TARGET_CMOS))))
        {
	  return ARCH_ENABLED_YES;
        }
      else
        {
	  return ARCH_ENABLED_NO;
        }

    case 31:  /* subqi3 */
      extract_constrain_insn_cached (insn);
      if ((which_alternative != 2) || ((which_alternative == 2) && (
#line 34 "../../gcc/config/6502/6502.md"
(TARGET_CMOS))))
        {
	  return ARCH_ENABLED_YES;
        }
      else
        {
	  return ARCH_ENABLED_NO;
        }

    case 30:  /* addqi3_noclob */
    case 29:  /* addqi3 */
      extract_constrain_insn_cached (insn);
      if ((!((1ULL << which_alternative) & 0x64ULL)) || ((((1ULL << which_alternative) & 0x64ULL)) && (
#line 34 "../../gcc/config/6502/6502.md"
(TARGET_CMOS))))
        {
	  return ARCH_ENABLED_YES;
        }
      else
        {
	  return ARCH_ENABLED_NO;
        }

    case 28:  /* incdecqi3_nz */
    case 27:  /* incdecqi3 */
      extract_constrain_insn_cached (insn);
      if ((!((1ULL << which_alternative) & 0x3ULL)) || ((((1ULL << which_alternative) & 0x3ULL)) && (
#line 34 "../../gcc/config/6502/6502.md"
(TARGET_CMOS))))
        {
	  return ARCH_ENABLED_YES;
        }
      else
        {
	  return ARCH_ENABLED_NO;
        }

    case 19:  /* movqi_insn */
    case 18:  /* movqi_noclob */
      extract_constrain_insn_cached (insn);
      if ((!((1ULL << which_alternative) & 0x33c0ULL)) || ((((1ULL << which_alternative) & 0x3000ULL)) && (
#line 34 "../../gcc/config/6502/6502.md"
(TARGET_CMOS))) || ((((1ULL << which_alternative) & 0xc0ULL)) && (
#line 38 "../../gcc/config/6502/6502.md"
(TARGET_PHX))) || ((((1ULL << which_alternative) & 0x300ULL)) && (
#line 42 "../../gcc/config/6502/6502.md"
(!TARGET_PHX))))
        {
	  return ARCH_ENABLED_YES;
        }
      else
        {
	  return ARCH_ENABLED_NO;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
      /* FALLTHRU */
      extract_constrain_insn_cached (insn);
      if ((cached_arch = get_attr_arch (insn)) == ARCH_ANY)
        {
	  return ARCH_ENABLED_YES;
        }
      else if ((cached_arch == ARCH_N02) && (
#line 10 "../../gcc/config/6502/6502.md"
(TARGET_6502)))
        {
	  return ARCH_ENABLED_YES;
        }
      else if ((cached_arch == ARCH_N02X) && (
#line 14 "../../gcc/config/6502/6502.md"
(TARGET_6502X)))
        {
	  return ARCH_ENABLED_YES;
        }
      else if ((cached_arch == ARCH_NMOS) && (
#line 18 "../../gcc/config/6502/6502.md"
(TARGET_NMOS)))
        {
	  return ARCH_ENABLED_YES;
        }
      else if ((cached_arch == ARCH_WC02) && (
#line 22 "../../gcc/config/6502/6502.md"
(TARGET_65C02)))
        {
	  return ARCH_ENABLED_YES;
        }
      else if ((cached_arch == ARCH_WSC02) && (
#line 26 "../../gcc/config/6502/6502.md"
(TARGET_65SC02)))
        {
	  return ARCH_ENABLED_YES;
        }
      else if ((cached_arch == ARCH_HUC) && (
#line 30 "../../gcc/config/6502/6502.md"
(TARGET_HUC6280)))
        {
	  return ARCH_ENABLED_YES;
        }
      else if ((cached_arch == ARCH_CMOS) && (
#line 34 "../../gcc/config/6502/6502.md"
(TARGET_CMOS)))
        {
	  return ARCH_ENABLED_YES;
        }
      else if ((cached_arch == ARCH_PHX) && (
#line 38 "../../gcc/config/6502/6502.md"
(TARGET_PHX)))
        {
	  return ARCH_ENABLED_YES;
        }
      else if ((cached_arch == ARCH_NO_PHX) && (
#line 42 "../../gcc/config/6502/6502.md"
(!TARGET_PHX)))
        {
	  return ARCH_ENABLED_YES;
        }
      else
        {
	  return ARCH_ENABLED_NO;
        }

    default:
      return ARCH_ENABLED_YES;

    }
}

attr_arch
get_attr_arch (rtx_insn *insn ATTRIBUTE_UNUSED)
{
  switch (recog_memoized (insn))
    {
    case 52:  /* compareqi */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0xfULL))
        {
	  return ARCH_ANY;
        }
      else if (which_alternative == 4)
        {
	  return ARCH_CMOS;
        }
      else
        {
	  return ARCH_ANY;
        }

    case 42:  /* iorqi3 */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0x7ULL))
        {
	  return ARCH_ANY;
        }
      else if (which_alternative == 3)
        {
	  return ARCH_CMOS;
        }
      else if (((1ULL << which_alternative) & 0x70ULL))
        {
	  return ARCH_ANY;
        }
      else
        {
	  return ARCH_CMOS;
        }

    case 45:  /* loadqi_nz */
    case 44:  /* xorqi3 */
    case 43:  /* iorqi3_nz */
    case 40:  /* andqi3_insn */
    case 38:  /* sbcqi3_nzv */
    case 37:  /* sbcqi3_c */
    case 36:  /* sbcqi3 */
    case 35:  /* adcqi3_c */
    case 34:  /* adcqi3 */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0x7ULL))
        {
	  return ARCH_ANY;
        }
      else if (which_alternative == 3)
        {
	  return ARCH_CMOS;
        }
      else
        {
	  return ARCH_ANY;
        }

    case 31:  /* subqi3 */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0x3ULL))
        {
	  return ARCH_ANY;
        }
      else if (which_alternative == 2)
        {
	  return ARCH_CMOS;
        }
      else
        {
	  return ARCH_ANY;
        }

    case 30:  /* addqi3_noclob */
    case 29:  /* addqi3 */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0x3ULL))
        {
	  return ARCH_ANY;
        }
      else if (which_alternative == 2)
        {
	  return ARCH_CMOS;
        }
      else if (((1ULL << which_alternative) & 0x18ULL))
        {
	  return ARCH_ANY;
        }
      else if (((1ULL << which_alternative) & 0x60ULL))
        {
	  return ARCH_CMOS;
        }
      else
        {
	  return ARCH_ANY;
        }

    case 28:  /* incdecqi3_nz */
    case 27:  /* incdecqi3 */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0x3ULL))
        {
	  return ARCH_CMOS;
        }
      else
        {
	  return ARCH_ANY;
        }

    case 19:  /* movqi_insn */
    case 18:  /* movqi_noclob */
      extract_constrain_insn_cached (insn);
      if (((1ULL << which_alternative) & 0x3fULL))
        {
	  return ARCH_ANY;
        }
      else if (((1ULL << which_alternative) & 0xc0ULL))
        {
	  return ARCH_PHX;
        }
      else if (((1ULL << which_alternative) & 0x300ULL))
        {
	  return ARCH_NO_PHX;
        }
      else if (((1ULL << which_alternative) & 0xc00ULL))
        {
	  return ARCH_ANY;
        }
      else if (((1ULL << which_alternative) & 0x3000ULL))
        {
	  return ARCH_CMOS;
        }
      else
        {
	  return ARCH_ANY;
        }

    case 41:  /* m65x_trbqi3 */
      return ARCH_CMOS;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
      /* FALLTHRU */
    default:
      return ARCH_ANY;

    }
}

attr_enabled
get_attr_enabled (rtx_insn *insn ATTRIBUTE_UNUSED)
{
  switch (recog_memoized (insn))
    {
    case 52:  /* compareqi */
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 4) && (! (
#line 34 "../../gcc/config/6502/6502.md"
(TARGET_CMOS))))
        {
	  return ENABLED_NO;
        }
      else
        {
	  return ENABLED_YES;
        }

    case 42:  /* iorqi3 */
      extract_constrain_insn_cached (insn);
      if ((!((1ULL << which_alternative) & 0x77ULL)) && (! (
#line 34 "../../gcc/config/6502/6502.md"
(TARGET_CMOS))))
        {
	  return ENABLED_NO;
        }
      else
        {
	  return ENABLED_YES;
        }

    case 41:  /* m65x_trbqi3 */
      extract_constrain_insn_cached (insn);
      if (! (
#line 34 "../../gcc/config/6502/6502.md"
(TARGET_CMOS)))
        {
	  return ENABLED_NO;
        }
      else
        {
	  return ENABLED_YES;
        }

    case 45:  /* loadqi_nz */
    case 44:  /* xorqi3 */
    case 43:  /* iorqi3_nz */
    case 40:  /* andqi3_insn */
    case 38:  /* sbcqi3_nzv */
    case 37:  /* sbcqi3_c */
    case 36:  /* sbcqi3 */
    case 35:  /* adcqi3_c */
    case 34:  /* adcqi3 */
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 3) && (! (
#line 34 "../../gcc/config/6502/6502.md"
(TARGET_CMOS))))
        {
	  return ENABLED_NO;
        }
      else
        {
	  return ENABLED_YES;
        }

    case 31:  /* subqi3 */
      extract_constrain_insn_cached (insn);
      if ((which_alternative == 2) && (! (
#line 34 "../../gcc/config/6502/6502.md"
(TARGET_CMOS))))
        {
	  return ENABLED_NO;
        }
      else
        {
	  return ENABLED_YES;
        }

    case 30:  /* addqi3_noclob */
    case 29:  /* addqi3 */
      extract_constrain_insn_cached (insn);
      if ((((1ULL << which_alternative) & 0x64ULL)) && (! (
#line 34 "../../gcc/config/6502/6502.md"
(TARGET_CMOS))))
        {
	  return ENABLED_NO;
        }
      else
        {
	  return ENABLED_YES;
        }

    case 28:  /* incdecqi3_nz */
    case 27:  /* incdecqi3 */
      extract_constrain_insn_cached (insn);
      if ((((1ULL << which_alternative) & 0x3ULL)) && (! (
#line 34 "../../gcc/config/6502/6502.md"
(TARGET_CMOS))))
        {
	  return ENABLED_NO;
        }
      else
        {
	  return ENABLED_YES;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
      /* FALLTHRU */
    case 18:  /* movqi_noclob */
    case 19:  /* movqi_insn */
      if (get_attr_arch_enabled (insn) == ARCH_ENABLED_NO)
        {
	  return ENABLED_NO;
        }
      else
        {
	  return ENABLED_YES;
        }

    default:
      return ENABLED_YES;

    }
}

attr_incdecqi3
get_attr_incdecqi3 (rtx_insn *insn ATTRIBUTE_UNUSED)
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
      /* FALLTHRU */
    default:
      return INCDECQI3_NO;

    }
}

attr_sbcqi3
get_attr_sbcqi3 (rtx_insn *insn ATTRIBUTE_UNUSED)
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
      /* FALLTHRU */
    default:
      return SBCQI3_NO;

    }
}

int
eligible_for_delay (rtx_insn *delay_insn ATTRIBUTE_UNUSED, int slot, 
		   rtx_insn *candidate_insn, int flags ATTRIBUTE_UNUSED)
{
  rtx_insn *insn ATTRIBUTE_UNUSED;

  if (num_delay_slots (delay_insn) == 0)
    return 0;
  gcc_assert (slot < 0);

  if (!INSN_P (candidate_insn))
    return 0;

  insn = candidate_insn;
  switch (slot)
    {
    default:
      gcc_unreachable ();
    }
}

int
eligible_for_annul_true (rtx_insn *delay_insn ATTRIBUTE_UNUSED,
    int slot ATTRIBUTE_UNUSED,
    rtx_insn *candidate_insn ATTRIBUTE_UNUSED,
    int flags ATTRIBUTE_UNUSED)
{
  return 0;
}

int
eligible_for_annul_false (rtx_insn *delay_insn ATTRIBUTE_UNUSED,
    int slot ATTRIBUTE_UNUSED,
    rtx_insn *candidate_insn ATTRIBUTE_UNUSED,
    int flags ATTRIBUTE_UNUSED)
{
  return 0;
}

int
const_num_delay_slots (rtx_insn *insn)
{
  switch (recog_memoized (insn))
    {
    default:
      return 1;
    }
}

EXPORTED_CONST int length_unit_log = 0;
