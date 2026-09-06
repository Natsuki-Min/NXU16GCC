/* Generated automatically by the program 'build/genpreds'
   from the machine description file '../../gcc/config/6502/6502.md'.  */

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
#include "insn-config.h"
#include "recog.h"
#include "output.h"
#include "flags.h"
#include "df.h"
#include "resource.h"
#include "diagnostic-core.h"
#include "reload.h"
#include "regs.h"
#include "emit-rtl.h"
#include "tm-constrs.h"
#include "target.h"


struct target_constraints default_target_constraints;
#if SWITCHABLE_TARGET
struct target_constraints *this_target_constraints = &default_target_constraints;
#endif
static inline bool
aligned_register_operand_1 (rtx op ATTRIBUTE_UNUSED, machine_mode mode ATTRIBUTE_UNUSED)
#line 25 "../../gcc/common.md"
{
  /* Require the offset in a non-paradoxical subreg to be naturally aligned.
     For example, if we have a subreg of something that is double the size of
     this operand, the offset must select the first or second half of it.  */
  if (SUBREG_P (op)
      && multiple_p (SUBREG_BYTE (op), GET_MODE_SIZE (GET_MODE (op))))
    op = SUBREG_REG (op);
  if (!REG_P (op))
    return false;

  if (HARD_REGISTER_P (op))
    {
      if (!in_hard_reg_set_p (operand_reg_set, GET_MODE (op), REGNO (op)))
	return false;

      /* Reject hard registers that would need reloading, so that the reload
	 is visible to IRA and to pre-RA optimizers.  */
      if (REGNO (op) % REG_NREGS (op) != 0)
	return false;
    }
  return true;
}

bool
aligned_register_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case REG:
    case SUBREG:
      break;
    default:
      return false;
    }
  return (
(aligned_register_operand_1 (op, mode))) && (
(mode == VOIDmode || GET_MODE (op) == mode));
}

static inline bool
accumulator_operand_1 (rtx op ATTRIBUTE_UNUSED, machine_mode mode ATTRIBUTE_UNUSED)
#line 3 "../../gcc/config/6502/predicates.md"
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  
  return REG_P (op)
	 && (REGNO (op) == ACC_REGNUM || REGNO (op) >= FIRST_PSEUDO_REGISTER);
}

bool
accumulator_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case REG:
    case SUBREG:
      break;
    default:
      return false;
    }
  return (
(accumulator_operand_1 (op, mode))) && (
(mode == VOIDmode || GET_MODE (op) == mode));
}

static inline bool
x_reg_operand_1 (rtx op ATTRIBUTE_UNUSED, machine_mode mode ATTRIBUTE_UNUSED)
#line 13 "../../gcc/config/6502/predicates.md"
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  
  return REG_P (op)
	 && (REGNO (op) == X_REGNUM || REGNO (op) >= FIRST_PSEUDO_REGISTER);
}

bool
x_reg_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case REG:
    case SUBREG:
      break;
    default:
      return false;
    }
  return (
(x_reg_operand_1 (op, mode))) && (
(mode == VOIDmode || GET_MODE (op) == mode));
}

static inline bool
y_reg_operand_1 (rtx op ATTRIBUTE_UNUSED, machine_mode mode ATTRIBUTE_UNUSED)
#line 23 "../../gcc/config/6502/predicates.md"
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  
  return REG_P (op)
	 && (REGNO (op) == Y_REGNUM || REGNO (op) >= FIRST_PSEUDO_REGISTER);
}

bool
y_reg_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case REG:
    case SUBREG:
      break;
    default:
      return false;
    }
  return (
(y_reg_operand_1 (op, mode))) && (
(mode == VOIDmode || GET_MODE (op) == mode));
}

bool
index_reg_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (x_reg_operand (op, mode)) || (y_reg_operand (op, mode));
}

static inline bool
hard_reg_operand_1 (rtx op ATTRIBUTE_UNUSED, machine_mode mode ATTRIBUTE_UNUSED)
#line 37 "../../gcc/config/6502/predicates.md"
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  
  return REG_P (op)
	 && (REGNO (op) == ACC_REGNUM || REGNO (op) == X_REGNUM
	     || REGNO (op) == Y_REGNUM || REGNO (op) >= FIRST_PSEUDO_REGISTER);
}

bool
hard_reg_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case REG:
    case SUBREG:
      break;
    default:
      return false;
    }
  return (
(hard_reg_operand_1 (op, mode))) && (
(mode == VOIDmode || GET_MODE (op) == mode));
}

static inline bool
mov_dst_operand_1 (rtx op ATTRIBUTE_UNUSED, machine_mode mode ATTRIBUTE_UNUSED)
#line 48 "../../gcc/config/6502/predicates.md"
{
  if (memory_operand (op, mode))
    return true;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  
  if (!REG_P (op))
    return false;
  
  return true;
}

bool
mov_dst_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case REG:
    case SUBREG:
    case MEM:
      break;
    default:
      return false;
    }
  return (
(mov_dst_operand_1 (op, mode))) && (
(mode == VOIDmode || GET_MODE (op) == mode));
}

static inline bool
sym_const_operand_1 (rtx op ATTRIBUTE_UNUSED, machine_mode mode ATTRIBUTE_UNUSED)
#line 63 "../../gcc/config/6502/predicates.md"
{
  if (GET_CODE (op) == PLUS || GET_CODE (op) == SUBREG)
    return sym_const_operand (XEXP (op, 0), mode);
  
  return true;
}

bool
sym_const_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case SUBREG:
    case LABEL_REF:
    case SYMBOL_REF:
    case PLUS:
    case CONST:
      break;
    default:
      return false;
    }
  return (
(sym_const_operand_1 (op, mode))) && (
(mode == VOIDmode || GET_MODE (op) == mode || GET_MODE (op) == VOIDmode));
}

bool
mov_src_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (general_operand (op, mode)) || (sym_const_operand (op, mode));
}

bool
const_mem_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return ((GET_CODE (op) == MEM) && (
#line 76 "../../gcc/config/6502/predicates.md"
(CONSTANT_ADDRESS_P (XEXP (op, 0))))) && (
(mode == VOIDmode || GET_MODE (op) == mode));
}

bool
indirect_mem_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return ((GET_CODE (op) == MEM) && (
#line 80 "../../gcc/config/6502/predicates.md"
(REG_P (XEXP (op, 0))))) && (
(mode == VOIDmode || GET_MODE (op) == mode));
}

bool
indirect_offset_mem_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return ((GET_CODE (op) == MEM) && (
#line 84 "../../gcc/config/6502/predicates.md"
(m65x_indirect_offset_addr_p (mode, XEXP (op, 0), false)))) && (
(mode == VOIDmode || GET_MODE (op) == mode));
}

bool
ind_y_mem_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return ((GET_CODE (op) == MEM) && (
#line 88 "../../gcc/config/6502/predicates.md"
(GET_CODE (XEXP (op, 0)) == PLUS
		    && GET_CODE (XEXP (XEXP (op, 0), 0)) == ZERO_EXTEND
		    && REG_P (XEXP (XEXP (XEXP (op, 0), 0), 0))
		    && REG_P (XEXP (XEXP (op, 0), 1))))) && (
(mode == VOIDmode || GET_MODE (op) == mode));
}

bool
abs_x_mem_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return ((GET_CODE (op) == MEM) && (
#line 95 "../../gcc/config/6502/predicates.md"
(m65x_absolute_x_addr_p (mode, XEXP (op, 0), false)))) && (
(mode == VOIDmode || GET_MODE (op) == mode));
}

bool
abs_y_mem_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return ((GET_CODE (op) == MEM) && (
#line 99 "../../gcc/config/6502/predicates.md"
(m65x_absolute_y_addr_p (mode, XEXP (op, 0), false)))) && (
(mode == VOIDmode || GET_MODE (op) == mode));
}

bool
incdec_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (const_mem_operand (op, mode)) || ((abs_x_mem_operand (op, mode)) || (register_operand (op, mode)));
}

bool
hard_sp_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return ((GET_CODE (op) == MEM) && (
#line 108 "../../gcc/config/6502/predicates.md"
((GET_CODE (XEXP (op, 0)) == POST_DEC
		     || GET_CODE (XEXP (op, 0)) == PRE_INC)
		    && REG_P (XEXP (XEXP (op, 0), 0))
		    && REGNO (XEXP (XEXP (op, 0), 0)) == HARDSP_REGNUM))) && (
(mode == VOIDmode || GET_MODE (op) == mode));
}

static inline bool
strict_zp_reg_operand_1 (rtx op ATTRIBUTE_UNUSED, machine_mode mode ATTRIBUTE_UNUSED)
#line 115 "../../gcc/config/6502/predicates.md"
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  
  return REG_P (op) && IS_ZP_REGNUM (REGNO (op));
}

bool
strict_zp_reg_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case SUBREG:
    case REG:
      break;
    default:
      return false;
    }
  return (
(strict_zp_reg_operand_1 (op, mode))) && (
(mode == VOIDmode || GET_MODE (op) == mode));
}

static inline bool
zp_reg_operand_1 (rtx op ATTRIBUTE_UNUSED, machine_mode mode ATTRIBUTE_UNUSED)
#line 124 "../../gcc/config/6502/predicates.md"
{
  int regno;
  
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  
  if (!REG_P (op))
    return false;
  
  regno = REGNO (op);
  
  return IS_ZP_REGNUM (regno) || regno >= FIRST_PSEUDO_REGISTER;
}

bool
zp_reg_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case REG:
    case SUBREG:
      break;
    default:
      return false;
    }
  return (
(zp_reg_operand_1 (op, mode))) && (
(mode == VOIDmode || GET_MODE (op) == mode));
}

static inline bool
ptr_reg_operand_1 (rtx op ATTRIBUTE_UNUSED, machine_mode mode ATTRIBUTE_UNUSED)
#line 140 "../../gcc/config/6502/predicates.md"
{
  int regno;
  
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  
  if (!REG_P (op))
    return false;
  
  regno = REGNO (op);
  
  return IS_ZP_REGNUM (regno) || regno == ARG_POINTER_REGNUM
	 || regno == FRAME_POINTER_REGNUM || regno >= FIRST_PSEUDO_REGISTER;
}

bool
ptr_reg_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case REG:
    case SUBREG:
      break;
    default:
      return false;
    }
  return (
(ptr_reg_operand_1 (op, mode))) && (
(mode == VOIDmode || GET_MODE (op) == mode));
}

static inline bool
strict_ptr_reg_operand_1 (rtx op ATTRIBUTE_UNUSED, machine_mode mode ATTRIBUTE_UNUSED)
#line 157 "../../gcc/config/6502/predicates.md"
{
  int regno;
  
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  
  if (!REG_P (op))
    return false;
  
  regno = REGNO (op);
  
  return IS_ZP_REGNUM (regno) || regno == ARG_POINTER_REGNUM
	 || regno == FRAME_POINTER_REGNUM;
}

bool
strict_ptr_reg_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case REG:
    case SUBREG:
      break;
    default:
      return false;
    }
  return (
(strict_ptr_reg_operand_1 (op, mode))) && (
(mode == VOIDmode || GET_MODE (op) == mode));
}

static inline bool
reload_zpreg_operand_1 (rtx op ATTRIBUTE_UNUSED, machine_mode mode ATTRIBUTE_UNUSED)
#line 174 "../../gcc/config/6502/predicates.md"
{
  int regno;
  
  if (reload_in_progress)
    {
      regno = true_regnum (op);

      if (regno == -1)
	return false;
    }
  else
    {
      if (GET_CODE (op) == SUBREG)
	op = SUBREG_REG (op);

      if (!REG_P (op))
        return false;

      regno = REGNO (op);
    }
  
  return IS_ZP_REGNUM (regno);
}

bool
reload_zpreg_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case REG:
    case SUBREG:
      break;
    default:
      return false;
    }
  return (
(reload_zpreg_operand_1 (op, mode))) && (
(mode == VOIDmode || GET_MODE (op) == mode));
}

bool
zp_reg_or_acc_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (accumulator_operand (op, mode)) || (zp_reg_operand (op, mode));
}

bool
zp_or_hard_reg_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (hard_reg_operand (op, mode)) || (zp_reg_operand (op, mode));
}

bool
zp_or_index_reg_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (index_reg_operand (op, mode)) || (zp_reg_operand (op, mode));
}

bool
zp_reg_or_const_mem_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (zp_reg_operand (op, mode)) || (const_mem_operand (op, mode));
}

bool
qimode_ior_dst_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (accumulator_operand (op, mode)) || ((
#line 216 "../../gcc/config/6502/predicates.md"
(TARGET_TRB_TSB)) && (zp_reg_or_const_mem_operand (op, mode)));
}

bool
zp_reg_or_ind_y_mem_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (zp_reg_operand (op, mode)) || (ind_y_mem_operand (op, mode));
}

bool
zp_reg_or_ind_y_mem_imm_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (zp_reg_operand (op, mode)) || ((ind_y_mem_operand (op, mode)) || (immediate_operand (op, mode)));
}

bool
reg_or_const_mem_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (register_operand (op, mode)) || (const_mem_operand (op, mode));
}

bool
zp_memory_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (memory_operand (op, mode)) && (
#line 234 "../../gcc/config/6502/predicates.md"
(MEM_ADDR_SPACE (op) == ADDR_SPACE_ZP));
}

bool
symlab_ref_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case LABEL_REF:
    case SYMBOL_REF:
      break;
    default:
      return false;
    }
  return 
(mode == VOIDmode || GET_MODE (op) == mode || GET_MODE (op) == VOIDmode);
}

bool
zp_reg_or_imm_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (immediate_operand (op, mode)) || (zp_reg_operand (op, mode));
}

bool
ptr_reg_or_int_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (const_int_operand (op, mode)) || (ptr_reg_operand (op, mode));
}

bool
zp_acc_imm_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (accumulator_operand (op, mode)) || ((zp_reg_operand (op, mode)) || (immediate_operand (op, mode)));
}

bool
addhi_op2 (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (zp_acc_imm_operand (op, mode)) || (((GET_CODE (op) == ZERO_EXTEND) && (
#line 255 "../../gcc/config/6502/predicates.md"
(REG_P (XEXP (op, 0))
			 && REGNO (XEXP (op, 0)) == Y_REGNUM))) && (
(mode == VOIDmode || GET_MODE (op) == mode || GET_MODE (op) == VOIDmode)));
}

bool
const_byte_amount (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (GET_CODE (op) == CONST_INT) && (
#line 260 "../../gcc/config/6502/predicates.md"
(INTVAL (op) >= 0 && INTVAL (op) < 256));
}

bool
shifthi_amount (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (GET_CODE (op) == CONST_INT) && (
#line 264 "../../gcc/config/6502/predicates.md"
(INTVAL (op) >= 1 && INTVAL (op) <= 16));
}

bool
shifthi_rt_byteswap (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (GET_CODE (op) == CONST_INT) && (
#line 268 "../../gcc/config/6502/predicates.md"
(INTVAL (op) == 5 || INTVAL (op) == 6));
}

bool
shiftqi_amount (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (GET_CODE (op) == CONST_INT) && (
#line 272 "../../gcc/config/6502/predicates.md"
(INTVAL (op) >= 1 && INTVAL (op) <= 8));
}

bool
const_one_amount (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (GET_CODE (op) == CONST_INT) && (
#line 276 "../../gcc/config/6502/predicates.md"
(INTVAL (op) == 1));
}

bool
one_or_minus_one_amount (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (GET_CODE (op) == CONST_INT) && (
#line 280 "../../gcc/config/6502/predicates.md"
(INTVAL (op) == 1 || INTVAL (op) == -1));
}

bool
m65x_comparison (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case EQ:
    case NE:
    case GTU:
    case LTU:
    case GEU:
    case LEU:
    case GT:
    case LT:
    case GE:
    case LE:
      return true;
    default:
      break;
    }
  return false;
}

bool
qimode_ui_comparison (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case LTU:
    case GEU:
      return true;
    default:
      break;
    }
  return false;
}

bool
qimode_v_comparison (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case LTU:
    case GEU:
      return true;
    default:
      break;
    }
  return false;
}

bool
qimode_nz_comparison (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case EQ:
    case NE:
    case LT:
    case GE:
      return true;
    default:
      break;
    }
  return false;
}

bool
qimode_c_comparison (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case EQ:
    case NE:
      return true;
    default:
      break;
    }
  return false;
}

static inline bool
compareqi_src_operand_1 (rtx op ATTRIBUTE_UNUSED, machine_mode mode ATTRIBUTE_UNUSED)
#line 299 "../../gcc/config/6502/predicates.md"
{
  int regno;
  
  if (memory_operand (op, mode))
    return true;
  
  if (CONSTANT_P (op))
    return true;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  
  if (GET_CODE (op) == LABEL_REF || GET_CODE (op) == SYMBOL_REF
      || GET_CODE (op) == CONST)
    return true;
  
  if (!REG_P (op))
    return false;
  
  regno = REGNO (op);
  
  return IS_ZP_REGNUM (regno)
	 || regno == FRAME_POINTER_REGNUM
	 || regno == ARG_POINTER_REGNUM
	 || regno >= FIRST_PSEUDO_REGISTER;
}

bool
compareqi_src_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (op))
    {
    case REG:
    case SUBREG:
    case CONST_INT:
    case MEM:
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST:
      break;
    default:
      return false;
    }
  return (
(compareqi_src_operand_1 (op, mode))) && (
(mode == VOIDmode || GET_MODE (op) == mode || GET_MODE (op) == VOIDmode));
}

bool
qimode_src_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return compareqi_src_operand (op, mode);
}

bool
qimode_ior_src_operand (rtx op, machine_mode mode ATTRIBUTE_UNUSED)
{
  return (qimode_src_operand (op, mode)) || ((
#line 331 "../../gcc/config/6502/predicates.md"
(TARGET_TRB_TSB)) && (accumulator_operand (op, mode)));
}

enum constraint_num
lookup_constraint_1 (const char *str)
{
  switch (str[0])
    {
    case '<':
      return CONSTRAINT__l;
    case '>':
      return CONSTRAINT__g;
    case 'A':
      if (str[1] == 'q')
        return CONSTRAINT_Aq;
      break;
    case 'E':
      return CONSTRAINT_E;
    case 'F':
      return CONSTRAINT_F;
    case 'I':
      return CONSTRAINT_I;
    case 'J':
      return CONSTRAINT_J;
    case 'K':
      return CONSTRAINT_K;
    case 'L':
      return CONSTRAINT_L;
    case 'M':
      return CONSTRAINT_M;
    case 'N':
      if (str[1] == 'r')
        return CONSTRAINT_Nr;
      if (str[1] == 'c')
        return CONSTRAINT_Nc;
      if (str[1] == 'l')
        return CONSTRAINT_Nl;
      if (str[1] == 'x')
        return CONSTRAINT_Nx;
      if (str[1] == 'a')
        return CONSTRAINT_Na;
      if (str[1] == 'b')
        return CONSTRAINT_Nb;
      if (str[1] == 'B')
        return CONSTRAINT_NB;
      if (str[1] == 's')
        return CONSTRAINT_Ns;
      break;
    case 'S':
      return CONSTRAINT_S;
    case 'U':
      if (str[1] == 'o')
        return CONSTRAINT_Uo;
      if (str[1] == 'y')
        return CONSTRAINT_Uy;
      if (str[1] == 'j')
        return CONSTRAINT_Uj;
      if (str[1] == 'X')
        return CONSTRAINT_UX;
      if (str[1] == 'Y')
        return CONSTRAINT_UY;
      if (str[1] == 'r')
        return CONSTRAINT_Ur;
      if (str[1] == 'c')
        return CONSTRAINT_Uc;
      break;
    case 'V':
      return CONSTRAINT_V;
    case 'X':
      return CONSTRAINT_X;
    case 'Z':
      if (str[1] == 'Y')
        return CONSTRAINT_ZY;
      if (str[1] == 'j')
        return CONSTRAINT_Zj;
      if (str[1] == 'X')
        return CONSTRAINT_ZX;
      break;
    case 'h':
      if (str[1] == 'q')
        return CONSTRAINT_hq;
      break;
    case 'i':
      return CONSTRAINT_i;
    case 'j':
      if (str[1] == 'q')
        return CONSTRAINT_jq;
      break;
    case 'm':
      return CONSTRAINT_m;
    case 'n':
      return CONSTRAINT_n;
    case 'o':
      return CONSTRAINT_o;
    case 'p':
      return CONSTRAINT_p;
    case 'r':
      return CONSTRAINT_r;
    case 's':
      return CONSTRAINT_s;
    case 'x':
      if (str[1] == 'q')
        return CONSTRAINT_xq;
      break;
    case 'y':
      if (str[1] == 'q')
        return CONSTRAINT_yq;
      break;
    case 'z':
      return CONSTRAINT_z;
    default: break;
    }
  return CONSTRAINT__UNKNOWN;
}

const unsigned char lookup_constraint_array[] = {
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  MIN ((int) CONSTRAINT__l, (int) UCHAR_MAX),
  CONSTRAINT__UNKNOWN,
  MIN ((int) CONSTRAINT__g, (int) UCHAR_MAX),
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  UCHAR_MAX,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  MIN ((int) CONSTRAINT_E, (int) UCHAR_MAX),
  MIN ((int) CONSTRAINT_F, (int) UCHAR_MAX),
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  MIN ((int) CONSTRAINT_I, (int) UCHAR_MAX),
  MIN ((int) CONSTRAINT_J, (int) UCHAR_MAX),
  MIN ((int) CONSTRAINT_K, (int) UCHAR_MAX),
  MIN ((int) CONSTRAINT_L, (int) UCHAR_MAX),
  MIN ((int) CONSTRAINT_M, (int) UCHAR_MAX),
  UCHAR_MAX,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  MIN ((int) CONSTRAINT_S, (int) UCHAR_MAX),
  CONSTRAINT__UNKNOWN,
  UCHAR_MAX,
  MIN ((int) CONSTRAINT_V, (int) UCHAR_MAX),
  CONSTRAINT__UNKNOWN,
  MIN ((int) CONSTRAINT_X, (int) UCHAR_MAX),
  CONSTRAINT__UNKNOWN,
  UCHAR_MAX,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  UCHAR_MAX,
  MIN ((int) CONSTRAINT_i, (int) UCHAR_MAX),
  UCHAR_MAX,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  MIN ((int) CONSTRAINT_m, (int) UCHAR_MAX),
  MIN ((int) CONSTRAINT_n, (int) UCHAR_MAX),
  MIN ((int) CONSTRAINT_o, (int) UCHAR_MAX),
  MIN ((int) CONSTRAINT_p, (int) UCHAR_MAX),
  CONSTRAINT__UNKNOWN,
  MIN ((int) CONSTRAINT_r, (int) UCHAR_MAX),
  MIN ((int) CONSTRAINT_s, (int) UCHAR_MAX),
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  UCHAR_MAX,
  UCHAR_MAX,
  MIN ((int) CONSTRAINT_z, (int) UCHAR_MAX),
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN,
  CONSTRAINT__UNKNOWN
};

enum reg_class
reg_class_for_constraint_1 (enum constraint_num c)
{
  switch (c)
    {
    case CONSTRAINT_r: return GENERAL_REGS;
    case CONSTRAINT_Aq: return HARD_ACCUM_REG;
    case CONSTRAINT_xq: return HARD_X_REG;
    case CONSTRAINT_yq: return HARD_Y_REG;
    case CONSTRAINT_jq: return HARD_INDEX_REGS;
    case CONSTRAINT_hq: return ACTUALLY_HARD_REGS;
    default: break;
    }
  return NO_REGS;
}

bool (*constraint_satisfied_p_array[]) (rtx) = {
  satisfies_constraint_I,
  satisfies_constraint_J,
  satisfies_constraint_K,
  satisfies_constraint_L,
  satisfies_constraint_M,
  satisfies_constraint_Ns,
  satisfies_constraint_Nr,
  satisfies_constraint_Nc,
  satisfies_constraint_Nl,
  satisfies_constraint_Nx,
  satisfies_constraint_Na,
  satisfies_constraint_Nb,
  satisfies_constraint_NB,
  satisfies_constraint_m,
  satisfies_constraint_o,
  satisfies_constraint_Ur,
  satisfies_constraint_p,
  satisfies_constraint_z,
  satisfies_constraint_S,
  satisfies_constraint_V,
  satisfies_constraint__l,
  satisfies_constraint__g,
  satisfies_constraint_Uc,
  satisfies_constraint_Uo,
  satisfies_constraint_Uy,
  satisfies_constraint_Uj,
  satisfies_constraint_UX,
  satisfies_constraint_UY,
  satisfies_constraint_ZX,
  satisfies_constraint_ZY,
  satisfies_constraint_Zj,
  satisfies_constraint_i,
  satisfies_constraint_s,
  satisfies_constraint_n,
  satisfies_constraint_E,
  satisfies_constraint_F,
  satisfies_constraint_X
};

bool
insn_const_int_ok_for_constraint (HOST_WIDE_INT ival, enum constraint_num c)
{
  switch (c)
    {
    case CONSTRAINT_I:
      return 
#line 24 "../../gcc/config/6502/constraints.md"
(ival >= 0 && ival <= 255);

    case CONSTRAINT_J:
      return 
#line 29 "../../gcc/config/6502/constraints.md"
(ival >= 0 && ival <= 65535);

    case CONSTRAINT_K:
      return 
#line 34 "../../gcc/config/6502/constraints.md"
(ival == 1);

    case CONSTRAINT_L:
      return 
#line 39 "../../gcc/config/6502/constraints.md"
(ival == -1);

    case CONSTRAINT_M:
      return 
#line 49 "../../gcc/config/6502/constraints.md"
(ival >= -255 && ival <= 0);

    case CONSTRAINT_Ns:
      return 
#line 54 "../../gcc/config/6502/constraints.md"
(ival >= 1 && ival <= 16);

    case CONSTRAINT_Nr:
      return 
#line 59 "../../gcc/config/6502/constraints.md"
(ival >= 1 && ival <= 16 && ival != 5 && ival != 6);

    case CONSTRAINT_Nc:
      return 
#line 64 "../../gcc/config/6502/constraints.md"
(ival == 5 || ival == 6);

    case CONSTRAINT_Nl:
      return 
#line 69 "../../gcc/config/6502/constraints.md"
(ival >= 1 && ival <= 16 && ival != 6);

    case CONSTRAINT_Nx:
      return 
#line 74 "../../gcc/config/6502/constraints.md"
(ival == 6);

    case CONSTRAINT_Na:
      return 
#line 79 "../../gcc/config/6502/constraints.md"
(ival >= 1 && ival <= 14);

    case CONSTRAINT_Nb:
      return 
#line 84 "../../gcc/config/6502/constraints.md"
(ival == 15 || ival == 16);

    case CONSTRAINT_NB:
      return 
#line 89 "../../gcc/config/6502/constraints.md"
(ival >= 1 && ival <= 8);

    default: break;
    }
  return false;
}


void
init_reg_class_start_regs ()
{
}
