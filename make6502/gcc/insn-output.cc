/* Generated automatically by the program `genoutput'
   from the machine description file `md'.  */

#define IN_TARGET_CODE 1
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "predict.h"
#include "tree.h"
#include "rtl.h"
#include "flags.h"
#include "alias.h"
#include "varasm.h"
#include "stor-layout.h"
#include "calls.h"
#include "insn-config.h"
#include "expmed.h"
#include "dojump.h"
#include "explow.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "stmt.h"
#include "expr.h"
#include "insn-codes.h"
#include "tm_p.h"
#include "regs.h"
#include "conditions.h"
#include "insn-attr.h"

#include "recog.h"

#include "diagnostic-core.h"
#include "output.h"
#include "target.h"
#include "tm-constrs.h"

static const char * const output_3[] = {
  "sta _tmp0\n\tlda %1\n\tclc\n\tadc %2\n\tsta %0\n\tlda %h1\n\tadc %h2\n\tsta %h0\n\tlda _tmp0",
  "sta _tmp0\n\tlda %1\n\tclc\n\tadc #%L2\n\tsta %0\n\tlda %h1\n\tadc #%H2\n\tsta %h0\n\tlda _tmp0",
};

static const char * const output_4[] = {
  "lda %1\n\tclc\n\tadc %2\n\tsta %0\n\tlda %h1\n\tadc %h2\n\tsta %h0",
  "lda %1\n\tclc\n\tadc #%L2\n\tsta %0\n\tlda %h1\n\tadc #%H2\n\tsta %h0",
};

static const char * const output_5[] = {
  "php\n\tsta _tmp0\n\tlda %1\n\tclc\n\tadc %2\n\tsta %0\n\tlda %h1\n\tadc %h2\n\tsta %h0\n\tlda _tmp0\n\tplp",
  "php\n\tsta _tmp0\n\tlda %1\n\tclc\n\tadc #%L2\n\tsta %0\n\tlda %h1\n\tadc #%H2\n\tsta %h0\n\tlda _tmp0\n\tplp",
};

static const char *
output_18 (rtx *operands ATTRIBUTE_UNUSED, rtx_insn *insn ATTRIBUTE_UNUSED)
{
#line 1102 "../../gcc/config/6502/6502.md"
{
  return m65x_print_movqi (which_alternative, operands, true);
}
}

static const char *
output_19 (rtx *operands ATTRIBUTE_UNUSED, rtx_insn *insn ATTRIBUTE_UNUSED)
{
#line 1120 "../../gcc/config/6502/6502.md"
{
  return m65x_print_movqi (which_alternative, operands, false);
}
}

static const char * const output_27[] = {
  "inc a",
  "dec a",
  "inx",
  "dex",
  "iny",
  "dey",
  "inc %0",
  "dec %0",
  "inc %0",
  "dec %0",
  "inc %0",
  "dec %0",
  "inc %0",
  "dec %0",
};

static const char * const output_28[] = {
  "inc a",
  "dec a",
  "inx",
  "dex",
  "iny",
  "dey",
  "inc %0",
  "dec %0",
  "inc %0",
  "dec %0",
  "inc %0",
  "dec %0",
  "inc %0",
  "dec %0",
};

static const char *
output_29 (rtx *operands ATTRIBUTE_UNUSED, rtx_insn *insn ATTRIBUTE_UNUSED)
{
#line 1729 "../../gcc/config/6502/6502.md"
{
  const char *insns = NULL;

  switch (which_alternative)
    {
    case 0: insns = "clc\n\tadc %2"; break;
    case 1: insns = "clc\n\tadc %2"; break;
    case 2: insns = "clc\n\tadc %2"; break;
    case 3: insns = "clc\n\tadc %2"; break;
    case 4: insns = "clc\n\tadc %2"; break;
    case 5: insns = "inc a"; break;
    case 6: insns = "dec a"; break;
    case 7: insns = "clc\n\tadc #%2"; break;
    case 8: insns = "inx"; break;
    case 9: insns = "iny"; break;
    case 10: insns = "inc %0"; break;
    case 11: insns = "inc %0"; break;
    case 12: insns = "inc %0"; break;
    case 13: insns = "inc %0"; break;
    case 14: insns = "sec\n\tsbc #%m2"; break;
    case 15: insns = "dex"; break;
    case 16: insns = "dey"; break;
    case 17: insns = "dec %0"; break;
    case 18: insns = "dec %0"; break;
    case 19: insns = "dec %0"; break;
    case 20: insns = "dec %0"; break;
    default: gcc_unreachable ();
    }

  if (false)
    {
      fprintf (asm_out_file, "\tphp\n");
      output_asm_insn (insns, operands);
      fprintf (asm_out_file, "\tplp\n");
    }
  else
    output_asm_insn (insns, operands);

  return "";
}
}

static const char *
output_30 (rtx *operands ATTRIBUTE_UNUSED, rtx_insn *insn ATTRIBUTE_UNUSED)
{
#line 1729 "../../gcc/config/6502/6502.md"
{
  const char *insns = NULL;

  switch (which_alternative)
    {
    case 0: insns = "clc\n\tadc %2"; break;
    case 1: insns = "clc\n\tadc %2"; break;
    case 2: insns = "clc\n\tadc %2"; break;
    case 3: insns = "clc\n\tadc %2"; break;
    case 4: insns = "clc\n\tadc %2"; break;
    case 5: insns = "inc a"; break;
    case 6: insns = "dec a"; break;
    case 7: insns = "clc\n\tadc #%2"; break;
    case 8: insns = "inx"; break;
    case 9: insns = "iny"; break;
    case 10: insns = "inc %0"; break;
    case 11: insns = "inc %0"; break;
    case 12: insns = "inc %0"; break;
    case 13: insns = "inc %0"; break;
    case 14: insns = "sec\n\tsbc #%m2"; break;
    case 15: insns = "dex"; break;
    case 16: insns = "dey"; break;
    case 17: insns = "dec %0"; break;
    case 18: insns = "dec %0"; break;
    case 19: insns = "dec %0"; break;
    case 20: insns = "dec %0"; break;
    default: gcc_unreachable ();
    }

  if (true)
    {
      fprintf (asm_out_file, "\tphp\n");
      output_asm_insn (insns, operands);
      fprintf (asm_out_file, "\tplp\n");
    }
  else
    output_asm_insn (insns, operands);

  return "";
}
}

static const char * const output_31[] = {
  "sec\n\tsbc %2",
  "sec\n\tsbc %2",
  "sec\n\tsbc %2",
  "sec\n\tsbc %2",
};

static const char * const output_34[] = {
  "adc %2",
  "adc #%2",
  "adc %2",
  "adc %2",
  "adc %2",
  "adc %2",
};

static const char * const output_35[] = {
  "adc %2",
  "adc #%2",
  "adc %2",
  "adc %2",
  "adc %2",
  "adc %2",
};

static const char * const output_36[] = {
  "sbc %2",
  "sbc #%2",
  "sbc %2",
  "sbc %2",
  "sbc %2",
  "sbc %2",
};

static const char * const output_37[] = {
  "sbc %2",
  "sbc #%2",
  "sbc %2",
  "sbc %2",
  "sbc %2",
  "sbc %2",
};

static const char * const output_38[] = {
  "sbc %2",
  "sbc #%2",
  "sbc %2",
  "sbc %2",
  "sbc %2",
  "sbc %2",
};

static const char * const output_40[] = {
  "and %2",
  "and #%2",
  "and %2",
  "and %2",
  "and %2",
  "and %2",
  "and %2",
};

static const char * const output_42[] = {
  "ora %2",
  "ora #%2",
  "ora %2",
  "ora %2",
  "ora %2",
  "ora %2",
  "ora %2",
  "tsb %0",
  "tsb %0",
};

static const char * const output_43[] = {
  "ora %2",
  "ora #%2",
  "ora %2",
  "ora %2",
  "ora %2",
  "ora %2",
  "ora %2",
};

static const char * const output_44[] = {
  "eor %2",
  "eor #%2",
  "eor %2",
  "eor %2",
  "eor %2",
  "eor %2",
  "eor %2",
};

static const char * const output_45[] = {
  "ld%R0 %1",
  "ld%R0 #%1",
  "lda %1",
  "ld%R0 %1",
  "ld%R0 %1",
  "ld%R0 %1",
  "ld%R0 %1",
};

static const char * const output_46[] = {
  "asl a",
  "asl %0",
  "asl %0",
  "asl %0",
  "asl %0",
};

static const char * const output_47[] = {
  "ror a",
  "ror %0",
  "ror %0",
  "ror %0",
  "ror %0",
};

static const char * const output_48[] = {
  "rol a",
  "rol %0",
  "rol %0",
  "rol %0",
  "rol %0",
};

static const char * const output_49[] = {
  "lsr a",
  "lsr %0",
  "lsr %0",
  "lsr %0",
  "lsr %0",
};

static const char *
output_50 (rtx *operands ATTRIBUTE_UNUSED, rtx_insn *insn ATTRIBUTE_UNUSED)
{
#line 2270 "../../gcc/config/6502/6502.md"
{
  switch (INTVAL (operands[2]))
    {
    case 1: return "cmp #$80\n\tror a";
    case 2: return "cmp #$80\n\tror a\n\tcmp #$80\n\tror a";
    case 3: return "cmp #$80\n\tror a\n\tcmp #$80\n\tror a\n\tcmp #$80\n\tror a";
    case 4: return "lsr a\n\tlsr a\n\tlsr a\n\tlsr a\n\tcmp #8\n\tbcc :+\n\tora #$f0\n\t:";
    case 5: return "lsr a\n\tlsr a\n\tlsr a\n\tlsr a\n\tlsr a\n\tcmp #4\n\tbcc :+\n\t"
		   "ora #$f8\n\t:";
    case 6: return "rol a\n\trol a\n\trol a\n\tand #3\n\tcmp #2\n\tbcc :+\n\tora #$fc\n\t:";
    case 7:
    case 8: return "ora #$7f\n\tbmi :+\n\tlda #0\n\t:";
    default: gcc_unreachable ();
    }
}
}

static const char * const output_52[] = {
  "%C0 %1",
  "%C0 #%1",
  "%C0 %1",
  "%C0 %1",
  "%C0 %1",
  "%C0 %1",
  "%C0 %1",
};

static const char *
output_53 (rtx *operands ATTRIBUTE_UNUSED, rtx_insn *insn ATTRIBUTE_UNUSED)
{
#line 2342 "../../gcc/config/6502/6502.md"
{
  m65x_print_branch (CC_Cmode, operands[0], operands[1],
		     get_attr_length (insn) > 2);
  return "";
}
}

static const char *
output_54 (rtx *operands ATTRIBUTE_UNUSED, rtx_insn *insn ATTRIBUTE_UNUSED)
{
#line 2342 "../../gcc/config/6502/6502.md"
{
  m65x_print_branch (CC_UImode, operands[0], operands[1],
		     get_attr_length (insn) > 2);
  return "";
}
}

static const char *
output_55 (rtx *operands ATTRIBUTE_UNUSED, rtx_insn *insn ATTRIBUTE_UNUSED)
{
#line 2342 "../../gcc/config/6502/6502.md"
{
  m65x_print_branch (CC_Vmode, operands[0], operands[1],
		     get_attr_length (insn) > 2);
  return "";
}
}

static const char *
output_56 (rtx *operands ATTRIBUTE_UNUSED, rtx_insn *insn ATTRIBUTE_UNUSED)
{
#line 2342 "../../gcc/config/6502/6502.md"
{
  m65x_print_branch (CC_NZmode, operands[0], operands[1],
		     get_attr_length (insn) > 2);
  return "";
}
}

static const char *
output_61 (rtx *operands ATTRIBUTE_UNUSED, rtx_insn *insn ATTRIBUTE_UNUSED)
{
#line 2435 "../../gcc/config/6502/6502.md"
{
  output_asm_insn ("lda #>(_call_ind$%=-1)\n\t"
		   "pha\n\t"
		   "lda #<(_call_ind$%=-1)\n\t"
		   "pha\n\t"
		   "jmp (%0)\n\t"
		   "_call_ind$%=:", operands);
  return "";
}
}

static const char *
output_63 (rtx *operands ATTRIBUTE_UNUSED, rtx_insn *insn ATTRIBUTE_UNUSED)
{
#line 2467 "../../gcc/config/6502/6502.md"
{
  output_asm_insn ("lda #<(_call_ind$%=-1)\n\t"
		   "pha\n\t"
		   "lda #>(_call_ind$%=-1)\n\t"
		   "pha\n\t"
		   "jmp (%0)\n\t"
		   "_call_ind$%=:", operands);
  return "";
}
}



static const struct insn_operand_data operand_data[] = 
{
  {
    0,
    "",
    E_VOIDmode,
    0,
    0,
    0,
    0
  },
  {
    nonimmediate_operand,
    "=&r,m,r,r",
    E_HImode,
    0,
    0,
    1,
    1
  },
  {
    general_operand,
    "r,r,m,i",
    E_HImode,
    0,
    0,
    1,
    1
  },
  {
    nonimmediate_operand,
    "=&r,m,r,r",
    E_SImode,
    0,
    0,
    1,
    1
  },
  {
    general_operand,
    "r,r,m,i",
    E_SImode,
    0,
    0,
    1,
    1
  },
  {
    register_operand,
    "=&r,r",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    register_operand,
    "r,r",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    nonmemory_operand,
    "r,i",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    register_operand,
    "=&r,r",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    register_operand,
    "%r,r",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    nonmemory_operand,
    "r,i",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    zp_reg_operand,
    "r,r",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    zp_reg_or_imm_operand,
    "r,i",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    0,
    "",
    E_VOIDmode,
    0,
    0,
    1,
    0
  },
  {
    scratch_operand,
    "=hq,hq",
    E_QImode,
    0,
    0,
    0,
    0
  },
  {
    zp_reg_or_imm_operand,
    "r,i",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    zp_reg_operand,
    "r,r",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    0,
    "",
    E_VOIDmode,
    0,
    0,
    1,
    0
  },
  {
    scratch_operand,
    "=hq,hq",
    E_QImode,
    0,
    0,
    0,
    0
  },
  {
    zp_reg_operand,
    "r,r",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    zp_reg_or_imm_operand,
    "r,i",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    0,
    "",
    E_VOIDmode,
    0,
    0,
    1,
    0
  },
  {
    scratch_operand,
    "=Aq,Aq",
    E_QImode,
    0,
    0,
    0,
    0
  },
  {
    zp_reg_or_imm_operand,
    "r,i",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    zp_reg_operand,
    "r,r",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    0,
    "",
    E_VOIDmode,
    0,
    0,
    1,
    0
  },
  {
    scratch_operand,
    "=Aq,Aq",
    E_QImode,
    0,
    0,
    0,
    0
  },
  {
    hard_reg_operand,
    "=hq",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    zp_memory_operand,
    "m",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    zp_memory_operand,
    "=m",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    hard_reg_operand,
    "hq",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    nonimmediate_operand,
    "=hq,hq,r,hq,Uc,hq,<,hq,<,Aq,Aq,m,r,Uc,?hq,?m,?r,?m,rhq",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    general_operand,
    "i,r,hq,Uc,hq,hq,hq,>,Aq,>,m,Aq,z,z,?m,?hq,?m,?r,0",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    accumulator_operand,
    "=Aq",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    zp_reg_operand,
    "r",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    x_reg_operand,
    "xq",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    accumulator_operand,
    "Aq",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    zp_reg_or_const_mem_operand,
    "=r,Uc,r,Uc",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    zp_reg_or_const_mem_operand,
    "r,r,Uc,Uc",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    scratch_operand,
    "=hq,hq,hq,hq",
    E_QImode,
    0,
    0,
    0,
    0
  },
  {
    hard_sp_operand,
    "=<",
    E_CCmode,
    0,
    0,
    1,
    1
  },
  {
    hard_sp_operand,
    ">",
    E_CCmode,
    0,
    0,
    1,
    1
  },
  {
    reload_zpreg_operand,
    "=r",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    reload_zpreg_operand,
    "r",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    scratch_operand,
    "=hq",
    E_QImode,
    0,
    0,
    0,
    0
  },
  {
    incdec_operand,
    "=Aq,Aq,xq,xq,yq,yq,r,r,UX,UX,Uc,Uc,ZX,ZX",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    incdec_operand,
    "%0,0,0,0,0,0,0,0,0,0,0,0,0,0",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    one_or_minus_one_amount,
    "K,L,K,L,K,L,K,L,K,L,K,L,K,L",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    incdec_operand,
    "=Aq,Aq,Aq,Aq,Aq,Aq,Aq,Aq,xq,yq,r,UX,Uc,ZX,Aq,xq,yq,r,UX,Uc,ZX",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    incdec_operand,
    "%0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    general_operand,
    "r,Uy,Ur,Uc,Uj,K,L,I,K,K,K,K,K,K,M,L,L,L,L,L,L",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    accumulator_operand,
    "=Aq,Aq,Aq,Aq",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    accumulator_operand,
    "0,0,0,0",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    memory_operand,
    "r,Uy,Ur,Uc",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    accumulator_operand,
    "=Aq,Aq,Aq,Aq,Aq,Aq",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    accumulator_operand,
    "%0,0,0,0,0,0",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    qimode_src_operand,
    "r,i,Uy,Ur,Uc,Uj",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    accumulator_operand,
    "=Aq,Aq,Aq,Aq,Aq,Aq",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    accumulator_operand,
    "0,0,0,0,0,0",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    qimode_src_operand,
    "r,i,Uy,Ur,Uc,Uj",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    accumulator_operand,
    "=Aq",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    accumulator_operand,
    "0",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    accumulator_operand,
    "=Aq,Aq,Aq,Aq,Aq,Aq,Aq",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    accumulator_operand,
    "%0,0,0,0,0,0,0",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    qimode_src_operand,
    "r,i,Uy,Ur,Uc,ZX,Uj",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    zp_reg_or_const_mem_operand,
    "=r,Uc",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    accumulator_operand,
    "Aq,Aq",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    zp_reg_or_const_mem_operand,
    "0,0",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    qimode_ior_dst_operand,
    "=Aq,Aq,Aq,Aq,Aq,Aq,Aq,r,Uc",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    qimode_ior_dst_operand,
    "%0,0,0,0,0,0,0,0,0",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    qimode_ior_src_operand,
    "r,i,Uy,Ur,Uc,ZX,Uj,Aq,Aq",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    hard_reg_operand,
    "=&hq,hq,Aq,hq,hq,hq,hq",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    qimode_src_operand,
    "r,i,Uy,Ur,Uc,Zj,Uj",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    reg_or_const_mem_operand,
    "=Aq,r,Uc,ZX,UX",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    reg_or_const_mem_operand,
    "0,0,0,0,0",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    const_one_amount,
    "K,K,K,K,K",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    accumulator_operand,
    "=Aq",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    accumulator_operand,
    "0",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    shiftqi_amount,
    "NB",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    hard_reg_operand,
    "hq,hq,hq,Aq,Aq,Aq,Aq",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    compareqi_src_operand,
    "r,i,Uc,Uy,Ur,ZX,Uj",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    qimode_c_comparison,
    "",
    E_VOIDmode,
    0,
    1,
    0,
    0
  },
  {
    0,
    "",
    E_VOIDmode,
    0,
    0,
    1,
    0
  },
  {
    qimode_ui_comparison,
    "",
    E_VOIDmode,
    0,
    1,
    0,
    0
  },
  {
    0,
    "",
    E_VOIDmode,
    0,
    0,
    1,
    0
  },
  {
    qimode_v_comparison,
    "",
    E_VOIDmode,
    0,
    1,
    0,
    0
  },
  {
    0,
    "",
    E_VOIDmode,
    0,
    0,
    1,
    0
  },
  {
    qimode_nz_comparison,
    "",
    E_VOIDmode,
    0,
    1,
    0,
    0
  },
  {
    0,
    "",
    E_VOIDmode,
    0,
    0,
    1,
    0
  },
  {
    0,
    "",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    0,
    "",
    E_VOIDmode,
    0,
    0,
    1,
    0
  },
  {
    register_operand,
    "r",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    0,
    "",
    E_VOIDmode,
    0,
    0,
    1,
    0
  },
  {
    0,
    "",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    general_operand,
    "",
    E_VOIDmode,
    0,
    0,
    1,
    1
  },
  {
    0,
    "",
    E_VOIDmode,
    0,
    0,
    1,
    0
  },
  {
    zp_reg_operand,
    "r",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    general_operand,
    "",
    E_VOIDmode,
    0,
    0,
    1,
    1
  },
  {
    x_reg_operand,
    "xq",
    E_VOIDmode,
    0,
    0,
    1,
    0
  },
  {
    y_reg_operand,
    "yq",
    E_VOIDmode,
    0,
    0,
    1,
    0
  },
  {
    nonimmediate_operand,
    "",
    E_HImode,
    0,
    0,
    1,
    1
  },
  {
    general_operand,
    "",
    E_HImode,
    0,
    0,
    1,
    1
  },
  {
    nonimmediate_operand,
    "",
    E_SImode,
    0,
    0,
    1,
    1
  },
  {
    general_operand,
    "",
    E_SImode,
    0,
    0,
    1,
    1
  },
  {
    nonimmediate_operand,
    "",
    E_HImode,
    0,
    0,
    1,
    1
  },
  {
    general_operand,
    "",
    E_HImode,
    0,
    0,
    1,
    1
  },
  {
    general_operand,
    "",
    E_HImode,
    0,
    0,
    1,
    1
  },
  {
    nonimmediate_operand,
    "",
    E_SImode,
    0,
    0,
    1,
    1
  },
  {
    general_operand,
    "",
    E_SImode,
    0,
    0,
    1,
    1
  },
  {
    general_operand,
    "",
    E_SImode,
    0,
    0,
    1,
    1
  },
  {
    register_operand,
    "",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    register_operand,
    "",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    reg_or_const_mem_operand,
    "",
    E_HImode,
    0,
    0,
    1,
    1
  },
  {
    register_operand,
    "",
    E_SImode,
    0,
    0,
    1,
    0
  },
  {
    register_operand,
    "",
    E_SImode,
    0,
    0,
    1,
    0
  },
  {
    reg_or_const_mem_operand,
    "",
    E_SImode,
    0,
    0,
    1,
    1
  },
  {
    reg_or_const_mem_operand,
    "",
    E_HImode,
    0,
    0,
    1,
    1
  },
  {
    reg_or_const_mem_operand,
    "",
    E_HImode,
    0,
    0,
    1,
    1
  },
  {
    shifthi_amount,
    "",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    m65x_comparison,
    "",
    E_VOIDmode,
    0,
    1,
    0,
    0
  },
  {
    register_operand,
    "",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    nonmemory_operand,
    "",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    0,
    "",
    E_VOIDmode,
    0,
    0,
    1,
    0
  },
  {
    scratch_operand,
    "",
    E_QImode,
    0,
    0,
    0,
    0
  },
  {
    zp_reg_operand,
    "",
    E_SImode,
    0,
    0,
    1,
    0
  },
  {
    zp_reg_operand,
    "",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    nonimmediate_operand,
    "",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    general_operand,
    "",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    accumulator_operand,
    "",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    y_reg_operand,
    "",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    zp_reg_operand,
    "",
    E_HImode,
    0,
    0,
    1,
    0
  },
  {
    accumulator_operand,
    "",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    hard_sp_operand,
    "",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    hard_reg_operand,
    "",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    hard_sp_operand,
    "",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    reload_zpreg_operand,
    "=r",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    reload_zpreg_operand,
    "r",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    hard_reg_operand,
    "=hq",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    register_operand,
    "",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    register_operand,
    "",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    qimode_ior_dst_operand,
    "",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    qimode_ior_dst_operand,
    "",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    qimode_ior_src_operand,
    "",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    reg_or_const_mem_operand,
    "",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    reg_or_const_mem_operand,
    "",
    E_QImode,
    0,
    0,
    1,
    1
  },
  {
    shiftqi_amount,
    "",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    m65x_comparison,
    "",
    E_VOIDmode,
    0,
    1,
    0,
    0
  },
  {
    register_operand,
    "",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    nonmemory_operand,
    "",
    E_QImode,
    0,
    0,
    1,
    0
  },
  {
    0,
    "",
    E_VOIDmode,
    0,
    0,
    1,
    0
  },
  {
    memory_operand,
    "",
    E_VOIDmode,
    0,
    0,
    1,
    1
  },
  {
    general_operand,
    "",
    E_VOIDmode,
    0,
    0,
    1,
    1
  },
};


#if GCC_VERSION >= 2007
__extension__
#endif

const struct insn_data_d insn_data[] = 
{
  /* <internal>:0 */
  {
    "*placeholder_for_nothing",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { 0 },
    &operand_data[0],
    0,
    0,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:230 */
  {
    "movhi_noclob",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "#",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_movhi_noclob },
    &operand_data[1],
    2,
    2,
    0,
    4,
    1
  },
  /* ../../gcc/config/6502/6502.md:230 */
  {
    "movsi_noclob",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "#",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_movsi_noclob },
    &operand_data[3],
    2,
    2,
    0,
    4,
    1
  },
  /* ../../gcc/config/6502/6502.md:460 */
  {
    "addhi3_noclobacc",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_3 },
#else
    { 0, output_3, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_addhi3_noclobacc },
    &operand_data[5],
    3,
    3,
    0,
    2,
    2
  },
  /* ../../gcc/config/6502/6502.md:476 */
  {
    "addhi3_clobacc",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_4 },
#else
    { 0, output_4, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_addhi3_clobacc },
    &operand_data[5],
    3,
    3,
    0,
    2,
    2
  },
  /* ../../gcc/config/6502/6502.md:494 */
  {
    "addhi3_noclob",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_5 },
#else
    { 0, output_5, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_addhi3_noclob },
    &operand_data[8],
    3,
    3,
    0,
    2,
    2
  },
  /* ../../gcc/config/6502/6502.md:821 */
  {
    "cbrhi_eq",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "#",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_cbrhi_eq },
    &operand_data[11],
    3,
    4,
    0,
    2,
    1
  },
  /* ../../gcc/config/6502/6502.md:838 */
  {
    "cbrhi_ne",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "#",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_cbrhi_ne },
    &operand_data[11],
    3,
    4,
    0,
    2,
    1
  },
  /* ../../gcc/config/6502/6502.md:855 */
  {
    "cbrhi_ltu",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "#",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_cbrhi_ltu },
    &operand_data[11],
    3,
    4,
    0,
    2,
    1
  },
  /* ../../gcc/config/6502/6502.md:872 */
  {
    "cbrhi_gtu",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "#",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_cbrhi_gtu },
    &operand_data[15],
    3,
    4,
    0,
    2,
    1
  },
  /* ../../gcc/config/6502/6502.md:889 */
  {
    "cbrhi_geu",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "#",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_cbrhi_geu },
    &operand_data[11],
    3,
    4,
    0,
    2,
    1
  },
  /* ../../gcc/config/6502/6502.md:906 */
  {
    "cbrhi_leu",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "#",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_cbrhi_leu },
    &operand_data[15],
    3,
    4,
    0,
    2,
    1
  },
  /* ../../gcc/config/6502/6502.md:923 */
  {
    "cbrhi_lt",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "#",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_cbrhi_lt },
    &operand_data[19],
    3,
    4,
    0,
    2,
    1
  },
  /* ../../gcc/config/6502/6502.md:940 */
  {
    "cbrhi_gt",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "#",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_cbrhi_gt },
    &operand_data[23],
    3,
    4,
    0,
    2,
    1
  },
  /* ../../gcc/config/6502/6502.md:957 */
  {
    "cbrhi_ge",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "#",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_cbrhi_ge },
    &operand_data[19],
    3,
    4,
    0,
    2,
    1
  },
  /* ../../gcc/config/6502/6502.md:974 */
  {
    "cbrhi_le",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "#",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_cbrhi_le },
    &operand_data[23],
    3,
    4,
    0,
    2,
    1
  },
  /* ../../gcc/config/6502/6502.md:1080 */
  {
    "loadqi_zp",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "ld%R0 %1",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_loadqi_zp },
    &operand_data[27],
    2,
    2,
    0,
    1,
    1
  },
  /* ../../gcc/config/6502/6502.md:1088 */
  {
    "storeqi_zp",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "st%R1 %0",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_storeqi_zp },
    &operand_data[29],
    2,
    2,
    0,
    1,
    1
  },
  /* ../../gcc/config/6502/6502.md:1096 */
  {
    "movqi_noclob",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .function = output_18 },
#else
    { 0, 0, output_18 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_movqi_noclob },
    &operand_data[31],
    2,
    2,
    0,
    19,
    3
  },
  /* ../../gcc/config/6502/6502.md:1113 */
  {
    "movqi_insn",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .function = output_19 },
#else
    { 0, 0, output_19 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_movqi_insn },
    &operand_data[31],
    2,
    2,
    0,
    19,
    3
  },
  /* ../../gcc/config/6502/6502.md:1442 */
  {
    "loadqi_xind",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "php\n\tlda (%1,x)\n\tplp",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_loadqi_xind },
    &operand_data[33],
    3,
    3,
    0,
    1,
    1
  },
  /* ../../gcc/config/6502/6502.md:1451 */
  {
    "loadqi_xind_nz",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "lda (%1,x)",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_loadqi_xind_nz },
    &operand_data[33],
    3,
    3,
    0,
    1,
    1
  },
  /* ../../gcc/config/6502/6502.md:1469 */
  {
    "storeqi_xind",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "sta (%0,x)",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_storeqi_xind },
    &operand_data[34],
    3,
    3,
    0,
    1,
    1
  },
  /* ../../gcc/config/6502/6502.md:1478 */
  {
    "zp_clob_hardreg_movqi",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "ld%R2 %1\n\tst%R2 %0",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_zp_clob_hardreg_movqi },
    &operand_data[37],
    2,
    3,
    0,
    4,
    1
  },
  /* ../../gcc/config/6502/6502.md:1613 */
  {
    "pushflags",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "php",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_pushflags },
    &operand_data[40],
    1,
    1,
    0,
    1,
    1
  },
  /* ../../gcc/config/6502/6502.md:1622 */
  {
    "popflags",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "plp",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_popflags },
    &operand_data[41],
    1,
    1,
    0,
    1,
    1
  },
  /* ../../gcc/config/6502/6502.md:1642 */
  {
    "movqi_loadstore_zp",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "ld%R2 %1\n\tst%R2 %0",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_movqi_loadstore_zp },
    &operand_data[42],
    2,
    3,
    0,
    1,
    1
  },
  /* ../../gcc/config/6502/6502.md:1670 */
  {
    "incdecqi3",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_27 },
#else
    { 0, output_27, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_incdecqi3 },
    &operand_data[45],
    3,
    3,
    0,
    14,
    2
  },
  /* ../../gcc/config/6502/6502.md:1670 */
  {
    "incdecqi3_nz",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_28 },
#else
    { 0, output_28, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_incdecqi3_nz },
    &operand_data[45],
    3,
    3,
    2,
    14,
    2
  },
  /* ../../gcc/config/6502/6502.md:1718 */
  {
    "addqi3",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .function = output_29 },
#else
    { 0, 0, output_29 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_addqi3 },
    &operand_data[48],
    3,
    3,
    0,
    21,
    3
  },
  /* ../../gcc/config/6502/6502.md:1718 */
  {
    "addqi3_noclob",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .function = output_30 },
#else
    { 0, 0, output_30 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_addqi3_noclob },
    &operand_data[48],
    3,
    3,
    0,
    21,
    3
  },
  /* ../../gcc/config/6502/6502.md:1786 */
  {
    "subqi3",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_31 },
#else
    { 0, output_31, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_subqi3 },
    &operand_data[51],
    3,
    3,
    0,
    4,
    2
  },
  /* ../../gcc/config/6502/6502.md:1802 */
  {
    "clc",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "clc",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_clc },
    &operand_data[0],
    0,
    0,
    0,
    0,
    1
  },
  /* ../../gcc/config/6502/6502.md:1808 */
  {
    "sec",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "sec",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_sec },
    &operand_data[0],
    0,
    0,
    0,
    0,
    1
  },
  /* ../../gcc/config/6502/6502.md:1837 */
  {
    "adcqi3",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_34 },
#else
    { 0, output_34, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_adcqi3 },
    &operand_data[54],
    3,
    3,
    0,
    6,
    2
  },
  /* ../../gcc/config/6502/6502.md:1837 */
  {
    "adcqi3_c",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_35 },
#else
    { 0, output_35, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_adcqi3_c },
    &operand_data[54],
    3,
    3,
    2,
    6,
    2
  },
  /* ../../gcc/config/6502/6502.md:1882 */
  {
    "sbcqi3",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_36 },
#else
    { 0, output_36, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_sbcqi3 },
    &operand_data[57],
    3,
    3,
    0,
    6,
    2
  },
  /* ../../gcc/config/6502/6502.md:1882 */
  {
    "sbcqi3_c",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_37 },
#else
    { 0, output_37, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_sbcqi3_c },
    &operand_data[57],
    3,
    3,
    2,
    6,
    2
  },
  /* ../../gcc/config/6502/6502.md:1903 */
  {
    "sbcqi3_nzv",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_38 },
#else
    { 0, output_38, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_sbcqi3_nzv },
    &operand_data[57],
    3,
    3,
    4,
    6,
    2
  },
  /* ../../gcc/config/6502/6502.md:1941 */
  {
    "negate_highbit",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "eor #$80\t; negate top bit",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_negate_highbit },
    &operand_data[60],
    2,
    2,
    1,
    1,
    1
  },
  /* ../../gcc/config/6502/6502.md:1972 */
  {
    "andqi3_insn",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_40 },
#else
    { 0, output_40, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_andqi3_insn },
    &operand_data[62],
    3,
    3,
    0,
    7,
    2
  },
  /* ../../gcc/config/6502/6502.md:1991 */
  {
    "m65x_trbqi3",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "trb %0",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_m65x_trbqi3 },
    &operand_data[65],
    3,
    3,
    0,
    2,
    1
  },
  /* ../../gcc/config/6502/6502.md:2001 */
  {
    "iorqi3",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_42 },
#else
    { 0, output_42, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_iorqi3 },
    &operand_data[68],
    3,
    3,
    0,
    9,
    2
  },
  /* ../../gcc/config/6502/6502.md:2023 */
  {
    "iorqi3_nz",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_43 },
#else
    { 0, output_43, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_iorqi3_nz },
    &operand_data[62],
    3,
    3,
    2,
    7,
    2
  },
  /* ../../gcc/config/6502/6502.md:2043 */
  {
    "xorqi3",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_44 },
#else
    { 0, output_44, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_xorqi3 },
    &operand_data[62],
    3,
    3,
    0,
    7,
    2
  },
  /* ../../gcc/config/6502/6502.md:2062 */
  {
    "loadqi_nz",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_45 },
#else
    { 0, output_45, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_loadqi_nz },
    &operand_data[71],
    2,
    2,
    1,
    7,
    2
  },
  /* ../../gcc/config/6502/6502.md:2136 */
  {
    "ashlqi3_insn",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_46 },
#else
    { 0, output_46, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_ashlqi3_insn },
    &operand_data[73],
    3,
    3,
    2,
    5,
    2
  },
  /* ../../gcc/config/6502/6502.md:2153 */
  {
    "m65x_rorqi3",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_47 },
#else
    { 0, output_47, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_m65x_rorqi3 },
    &operand_data[73],
    2,
    2,
    1,
    5,
    2
  },
  /* ../../gcc/config/6502/6502.md:2171 */
  {
    "m65x_rolqi3",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_48 },
#else
    { 0, output_48, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_m65x_rolqi3 },
    &operand_data[73],
    2,
    2,
    1,
    5,
    2
  },
  /* ../../gcc/config/6502/6502.md:2246 */
  {
    "lshrqi3_insn",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_49 },
#else
    { 0, output_49, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_lshrqi3_insn },
    &operand_data[73],
    3,
    3,
    2,
    5,
    2
  },
  /* ../../gcc/config/6502/6502.md:2263 */
  {
    "ashrqi3",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .function = output_50 },
#else
    { 0, 0, output_50 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_ashrqi3 },
    &operand_data[76],
    3,
    3,
    0,
    1,
    3
  },
  /* ../../gcc/config/6502/6502.md:2296 */
  {
    "one_cmplqi2",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "eor #$ff",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_one_cmplqi2 },
    &operand_data[60],
    2,
    2,
    0,
    1,
    1
  },
  /* ../../gcc/config/6502/6502.md:2306 */
  {
    "compareqi",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .multi = output_52 },
#else
    { 0, output_52, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_compareqi },
    &operand_data[79],
    2,
    2,
    2,
    7,
    2
  },
  /* ../../gcc/config/6502/6502.md:2335 */
  {
    "condbranchqi_cc_c",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .function = output_53 },
#else
    { 0, 0, output_53 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_condbranchqi_cc_c },
    &operand_data[81],
    2,
    2,
    0,
    0,
    3
  },
  /* ../../gcc/config/6502/6502.md:2335 */
  {
    "condbranchqi_cc_ui",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .function = output_54 },
#else
    { 0, 0, output_54 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_condbranchqi_cc_ui },
    &operand_data[83],
    2,
    2,
    0,
    0,
    3
  },
  /* ../../gcc/config/6502/6502.md:2335 */
  {
    "condbranchqi_cc_v",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .function = output_55 },
#else
    { 0, 0, output_55 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_condbranchqi_cc_v },
    &operand_data[85],
    2,
    2,
    0,
    0,
    3
  },
  /* ../../gcc/config/6502/6502.md:2335 */
  {
    "condbranchqi_cc_nz",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .function = output_56 },
#else
    { 0, 0, output_56 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_condbranchqi_cc_nz },
    &operand_data[87],
    2,
    2,
    0,
    0,
    3
  },
  /* ../../gcc/config/6502/6502.md:2353 */
  {
    "indirect_jump",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "jmp (%0)",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_indirect_jump },
    &operand_data[34],
    1,
    1,
    0,
    1,
    1
  },
  /* ../../gcc/config/6502/6502.md:2360 */
  {
    "jump",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "jmp %0",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_jump },
    &operand_data[13],
    1,
    1,
    0,
    0,
    1
  },
  /* ../../gcc/config/6502/6502.md:2411 */
  {
    "nop",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "nop",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_nop },
    &operand_data[0],
    0,
    0,
    0,
    0,
    1
  },
  /* ../../gcc/config/6502/6502.md:2422 */
  {
    "call_sym",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "jsr %0",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_call_sym },
    &operand_data[89],
    2,
    2,
    0,
    0,
    1
  },
  /* ../../gcc/config/6502/6502.md:2430 */
  {
    "call_indirect",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .function = output_61 },
#else
    { 0, 0, output_61 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_call_indirect },
    &operand_data[91],
    2,
    2,
    0,
    1,
    3
  },
  /* ../../gcc/config/6502/6502.md:2452 */
  {
    "call_value_sym",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "jsr %1",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_call_value_sym },
    &operand_data[92],
    3,
    3,
    0,
    0,
    1
  },
  /* ../../gcc/config/6502/6502.md:2461 */
  {
    "call_indirect_sym",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .function = output_63 },
#else
    { 0, 0, output_63 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_call_indirect_sym },
    &operand_data[95],
    3,
    3,
    0,
    1,
    3
  },
  /* ../../gcc/config/6502/6502.md:2478 */
  {
    "m65x_return",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "rts",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_m65x_return },
    &operand_data[0],
    0,
    0,
    0,
    0,
    1
  },
  /* ../../gcc/config/6502/6502.md:2500 */
  {
    "m65x_savestack_s7s0_sp",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "jsr __m65x_savestack_s7s0_sp",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_m65x_savestack_s7s0_sp },
    &operand_data[98],
    2,
    2,
    0,
    1,
    1
  },
  /* ../../gcc/config/6502/6502.md:2508 */
  {
    "m65x_savestack_s7s0",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "jsr __m65x_savestack_s7s0",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_m65x_savestack_s7s0 },
    &operand_data[0],
    0,
    0,
    0,
    0,
    1
  },
  /* ../../gcc/config/6502/6502.md:2514 */
  {
    "m65x_restorestack_s7s0_sp_rts",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "jmp __m65x_restorestack_s7s0_sp_rts",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_m65x_restorestack_s7s0_sp_rts },
    &operand_data[0],
    0,
    0,
    0,
    0,
    1
  },
  /* ../../gcc/config/6502/6502.md:2521 */
  {
    "m65x_restorestack_s7s0_rts",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { .single =
#else
    {
#endif
    "jmp __m65x_restorestack_s7s0_rts",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    },
#else
    0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_m65x_restorestack_s7s0_rts },
    &operand_data[0],
    0,
    0,
    0,
    0,
    1
  },
  /* ../../gcc/config/6502/6502.md:107 */
  {
    "movhi",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_movhi },
    &operand_data[100],
    2,
    2,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:107 */
  {
    "movsi",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_movsi },
    &operand_data[102],
    2,
    2,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:387 */
  {
    "addhi3",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_addhi3 },
    &operand_data[104],
    3,
    3,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:387 */
  {
    "addsi3",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_addsi3 },
    &operand_data[107],
    3,
    3,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:504 */
  {
    "subhi3",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_subhi3 },
    &operand_data[110],
    3,
    3,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:504 */
  {
    "subsi3",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_subsi3 },
    &operand_data[113],
    3,
    3,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:514 */
  {
    "lshrhi3",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_lshrhi3 },
    &operand_data[116],
    3,
    3,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:615 */
  {
    "ashlhi3",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_ashlhi3 },
    &operand_data[116],
    3,
    3,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:780 */
  {
    "cbranchhi4",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_cbranchhi4 },
    &operand_data[119],
    4,
    5,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:991 */
  {
    "extendhisi2",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_extendhisi2 },
    &operand_data[124],
    2,
    2,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:1020 */
  {
    "neghi2",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_neghi2 },
    &operand_data[116],
    2,
    2,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:1032 */
  {
    "movqi",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_movqi },
    &operand_data[126],
    2,
    2,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:1424 */
  {
    "loadqi_indy",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_loadqi_indy },
    &operand_data[128],
    3,
    3,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:1432 */
  {
    "loadqi_indy_nz",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_loadqi_indy_nz },
    &operand_data[128],
    3,
    3,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:1461 */
  {
    "storeqi_indy",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_storeqi_indy },
    &operand_data[129],
    3,
    3,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:1599 */
  {
    "pushqi1",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_pushqi1 },
    &operand_data[132],
    2,
    2,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:1606 */
  {
    "popqi1",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_popqi1 },
    &operand_data[133],
    2,
    2,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:1634 */
  {
    "reload_inoutqi_zp",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_reload_inoutqi_zp },
    &operand_data[135],
    3,
    3,
    0,
    1,
    0
  },
  /* ../../gcc/config/6502/6502.md:1928 */
  {
    "negqi2",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_negqi2 },
    &operand_data[138],
    2,
    2,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:1951 */
  {
    "andqi3",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_andqi3 },
    &operand_data[140],
    3,
    3,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:2079 */
  {
    "ashlqi3",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_ashlqi3 },
    &operand_data[143],
    3,
    3,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:2189 */
  {
    "lshrqi3",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_lshrqi3 },
    &operand_data[143],
    3,
    3,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:2367 */
  {
    "cbranchqi4",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_cbranchqi4 },
    &operand_data[146],
    4,
    4,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:2417 */
  {
    "call",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_call },
    &operand_data[150],
    2,
    2,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:2446 */
  {
    "call_value",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_call_value },
    &operand_data[149],
    3,
    3,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:2484 */
  {
    "prologue",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_prologue },
    &operand_data[0],
    0,
    0,
    0,
    0,
    0
  },
  /* ../../gcc/config/6502/6502.md:2492 */
  {
    "epilogue",
#if HAVE_DESIGNATED_UNION_INITIALIZERS
    { 0 },
#else
    { 0, 0, 0 },
#endif
    { (insn_gen_fn::stored_funcptr) gen_epilogue },
    &operand_data[0],
    0,
    0,
    0,
    0,
    0
  },
};


const char *
get_insn_name (int code)
{
  if (code == NOOP_MOVE_INSN_CODE)
    return "NOOP_MOVE";
  else
    return insn_data[code].name;
}
