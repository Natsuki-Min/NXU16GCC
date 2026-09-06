/* Generated automatically by the program `genrecog' from the target
   machine description file.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "predict.h"
#include "rtl.h"
#include "memmodel.h"
#include "tm_p.h"
#include "emit-rtl.h"
#include "insn-config.h"
#include "recog.h"
#include "output.h"
#include "flags.h"
#include "df.h"
#include "resource.h"
#include "diagnostic-core.h"
#include "reload.h"
#include "regs.h"
#include "tm-constrs.h"



/* `recog' contains a decision tree that recognizes whether the rtx
   X0 is a valid instruction.

   recog returns -1 if the rtx is not valid.  If the rtx is valid, recog
   returns a nonnegative number which is the insn code number for the
   pattern that matched.  This is the same as the order in the machine
   description of the entry that matched.  This number can be used as an
   index into `insn_data' and other tables.

   The third parameter to recog is an optional pointer to an int.  If
   present, recog will accept a pattern if it matches except for missing
   CLOBBER expressions at the end.  In that case, the value pointed to by
   the optional pointer will be set to the number of CLOBBERs that need
   to be added (it should be initialized to zero by the caller).  If it
   is set nonzero, the caller should allocate a PARALLEL of the
   appropriate size, copy the initial entries, and call add_clobbers
   (found in insn-emit.cc) to fill in the CLOBBERs.


   The function split_insns returns 0 if the rtl could not
   be split or the split rtl as an INSN list if it can be.

   The function peephole2_insns returns 0 if the rtl could not
   be matched. If there was a match, the new rtl is returned in an INSN list,
   and LAST_INSN will point to the last recognized insn in the old sequence.
*/


extern rtx_insn *gen_split_1 (rtx_insn *, rtx *);
extern rtx_insn *gen_split_2 (rtx_insn *, rtx *);
extern rtx_insn *gen_split_3 (rtx_insn *, rtx *);
extern rtx_insn *gen_split_4 (rtx_insn *, rtx *);
extern rtx_insn *gen_split_5 (rtx_insn *, rtx *);
extern rtx_insn *gen_split_6 (rtx_insn *, rtx *);
extern rtx_insn *gen_split_7 (rtx_insn *, rtx *);
extern rtx_insn *gen_split_8 (rtx_insn *, rtx *);
extern rtx_insn *gen_split_9 (rtx_insn *, rtx *);
extern rtx_insn *gen_split_10 (rtx_insn *, rtx *);
extern rtx_insn *gen_split_11 (rtx_insn *, rtx *);
extern rtx_insn *gen_split_12 (rtx_insn *, rtx *);
extern rtx_insn *gen_peephole2_1 (rtx_insn *, rtx *);
extern rtx_insn *gen_peephole2_2 (rtx_insn *, rtx *);
extern rtx_insn *gen_peephole2_3 (rtx_insn *, rtx *);
extern rtx_insn *gen_peephole2_4 (rtx_insn *, rtx *);
extern rtx_insn *gen_peephole2_5 (rtx_insn *, rtx *);
extern rtx_insn *gen_peephole2_6 (rtx_insn *, rtx *);
extern rtx_insn *gen_peephole2_7 (rtx_insn *, rtx *);
extern rtx_insn *gen_peephole2_8 (rtx_insn *, rtx *);
extern rtx_insn *gen_peephole2_9 (rtx_insn *, rtx *);
extern rtx_insn *gen_peephole2_10 (rtx_insn *, rtx *);
extern rtx_insn *gen_peephole2_11 (rtx_insn *, rtx *);
extern rtx_insn *gen_peephole2_12 (rtx_insn *, rtx *);
extern rtx_insn *gen_peephole2_13 (rtx_insn *, rtx *);
extern rtx_insn *gen_peephole2_14 (rtx_insn *, rtx *);
extern rtx_insn *gen_peephole2_15 (rtx_insn *, rtx *);
extern rtx_insn *gen_peephole2_16 (rtx_insn *, rtx *);
extern rtx_insn *gen_peephole2_17 (rtx_insn *, rtx *);
extern rtx_insn *gen_peephole2_18 (rtx_insn *, rtx *);
extern rtx_insn *gen_peephole2_19 (rtx_insn *, rtx *);
extern rtx_insn *gen_peephole2_20 (rtx_insn *, rtx *);




static int
pattern0 (rtx x1, int *pnum_clobbers)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4;
  int res ATTRIBUTE_UNUSED;
  if (pnum_clobbers == NULL)
    return -1;
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) != E_QImode)
    return -1;
  x3 = XEXP (x1, 0);
  operands[0] = x3;
  if (!accumulator_operand (operands[0], E_QImode))
    return -1;
  x4 = XEXP (x2, 0);
  operands[1] = x4;
  if (!accumulator_operand (operands[1], E_QImode))
    return -1;
  return 0;
}

static int
pattern1 (rtx x1, int *pnum_clobbers)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5;
  int res ATTRIBUTE_UNUSED;
  if (pnum_clobbers == NULL)
    return -1;
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) != E_QImode)
    return -1;
  x3 = XEXP (x1, 0);
  operands[0] = x3;
  if (!accumulator_operand (operands[0], E_QImode))
    return -1;
  x4 = XEXP (x2, 0);
  operands[1] = x4;
  if (!accumulator_operand (operands[1], E_QImode))
    return -1;
  x5 = XEXP (x2, 1);
  operands[2] = x5;
  return 0;
}

static int
pattern2 (rtx x1)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2;
  int res ATTRIBUTE_UNUSED;
  if (GET_CODE (x1) != CLOBBER)
    return -1;
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) != REG
      || REGNO (x2) != 68
      || GET_MODE (x2) != E_CC_NZmode)
    return -1;
  return 0;
}

static int
pattern3 (rtx x1, machine_mode i1, unsigned int i2)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2;
  int res ATTRIBUTE_UNUSED;
  if (GET_CODE (x1) != CLOBBER)
    return -1;
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) != REG
      || REGNO (x2) != i2
      || GET_MODE (x2) != i1)
    return -1;
  return 0;
}

static int
pattern4 (rtx x1, machine_mode i1, unsigned int i2, machine_mode i3, unsigned int i4)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4;
  int res ATTRIBUTE_UNUSED;
  x2 = XVECEXP (x1, 0, 1);
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) != REG
      || REGNO (x3) != i4
      || GET_MODE (x3) != i3)
    return -1;
  x4 = XVECEXP (x1, 0, 2);
  return pattern3 (x4, i1, i2); /* [-1, 0] */
}

static int
pattern5 (rtx x1, machine_mode i1, unsigned int i2, machine_mode i3, unsigned int i4, machine_mode i5)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4;
  int res ATTRIBUTE_UNUSED;
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) != i5)
    return -1;
  x4 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x4) != CLOBBER
      || pattern4 (x1, i1, i2, i3, i4) != 0)
    return -1;
  return 0;
}

static int
pattern6 (rtx x1)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5, x6, x7, x8, x9;
  rtx x10, x11, x12;
  int res ATTRIBUTE_UNUSED;
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 1);
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) != LABEL_REF)
    return -1;
  x5 = XEXP (x3, 2);
  if (GET_CODE (x5) != PC)
    return -1;
  x6 = XEXP (x2, 0);
  if (GET_CODE (x6) != PC)
    return -1;
  x7 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x7) != CLOBBER)
    return -1;
  x8 = XEXP (x4, 0);
  operands[2] = x8;
  x9 = XEXP (x7, 0);
  operands[3] = x9;
  if (!scratch_operand (operands[3], E_QImode))
    return -1;
  x10 = XEXP (x3, 0);
  switch (GET_CODE (x10))
    {
    case EQ:
      x11 = XEXP (x10, 0);
      operands[0] = x11;
      if (!zp_reg_operand (operands[0], E_HImode))
        return -1;
      x12 = XEXP (x10, 1);
      operands[1] = x12;
      if (!zp_reg_or_imm_operand (operands[1], E_HImode))
        return -1;
      return 0;

    case NE:
      x11 = XEXP (x10, 0);
      operands[0] = x11;
      if (!zp_reg_operand (operands[0], E_HImode))
        return -1;
      x12 = XEXP (x10, 1);
      operands[1] = x12;
      if (!zp_reg_or_imm_operand (operands[1], E_HImode))
        return -1;
      return 1;

    case LTU:
      x11 = XEXP (x10, 0);
      operands[0] = x11;
      if (!zp_reg_operand (operands[0], E_HImode))
        return -1;
      x12 = XEXP (x10, 1);
      operands[1] = x12;
      if (!zp_reg_or_imm_operand (operands[1], E_HImode))
        return -1;
      return 2;

    case GTU:
      x11 = XEXP (x10, 0);
      operands[0] = x11;
      if (!zp_reg_or_imm_operand (operands[0], E_HImode))
        return -1;
      x12 = XEXP (x10, 1);
      operands[1] = x12;
      if (!zp_reg_operand (operands[1], E_HImode))
        return -1;
      return 3;

    case GEU:
      x11 = XEXP (x10, 0);
      operands[0] = x11;
      if (!zp_reg_operand (operands[0], E_HImode))
        return -1;
      x12 = XEXP (x10, 1);
      operands[1] = x12;
      if (!zp_reg_or_imm_operand (operands[1], E_HImode))
        return -1;
      return 4;

    case LEU:
      x11 = XEXP (x10, 0);
      operands[0] = x11;
      if (!zp_reg_or_imm_operand (operands[0], E_HImode))
        return -1;
      x12 = XEXP (x10, 1);
      operands[1] = x12;
      if (!zp_reg_operand (operands[1], E_HImode))
        return -1;
      return 5;

    case LT:
      x11 = XEXP (x10, 0);
      operands[0] = x11;
      if (!zp_reg_operand (operands[0], E_HImode))
        return -1;
      x12 = XEXP (x10, 1);
      operands[1] = x12;
      if (!zp_reg_or_imm_operand (operands[1], E_HImode))
        return -1;
      return 6;

    case GT:
      x11 = XEXP (x10, 0);
      operands[0] = x11;
      if (!zp_reg_or_imm_operand (operands[0], E_HImode))
        return -1;
      x12 = XEXP (x10, 1);
      operands[1] = x12;
      if (!zp_reg_operand (operands[1], E_HImode))
        return -1;
      return 7;

    case GE:
      x11 = XEXP (x10, 0);
      operands[0] = x11;
      if (!zp_reg_operand (operands[0], E_HImode))
        return -1;
      x12 = XEXP (x10, 1);
      operands[1] = x12;
      if (!zp_reg_or_imm_operand (operands[1], E_HImode))
        return -1;
      return 8;

    case LE:
      x11 = XEXP (x10, 0);
      operands[0] = x11;
      if (!zp_reg_or_imm_operand (operands[0], E_HImode))
        return -1;
      x12 = XEXP (x10, 1);
      operands[1] = x12;
      if (!zp_reg_operand (operands[1], E_HImode))
        return -1;
      return 9;

    default:
      return -1;
    }
}

static int
pattern7 (rtx x1, int *pnum_clobbers, rtx_code i1)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3;
  int res ATTRIBUTE_UNUSED;
  if (pnum_clobbers == NULL
      || GET_MODE (x1) != E_QImode)
    return -1;
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) != MINUS
      || GET_MODE (x2) != E_QImode)
    return -1;
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) != i1
      || GET_MODE (x3) != E_QImode)
    return -1;
  return 0;
}

static int
pattern8 (rtx x1)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5, x6, x7, x8, x9;
  rtx x10;
  int res ATTRIBUTE_UNUSED;
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  operands[0] = x3;
  if (!reg_or_const_mem_operand (operands[0], E_QImode))
    return -1;
  x4 = XEXP (x2, 1);
  x5 = XEXP (x4, 0);
  operands[1] = x5;
  if (!reg_or_const_mem_operand (operands[1], E_QImode))
    return -1;
  x6 = XEXP (x4, 1);
  operands[2] = x6;
  if (!const_one_amount (operands[2], E_QImode))
    return -1;
  x7 = XVECEXP (x1, 0, 1);
  x8 = XEXP (x7, 1);
  x9 = XVECEXP (x8, 0, 0);
  if (!rtx_equal_p (x9, operands[1]))
    return -1;
  x10 = XVECEXP (x8, 0, 1);
  if (!rtx_equal_p (x10, operands[2]))
    return -1;
  return 0;
}

static int
pattern9 (rtx x1, int *pnum_clobbers, int i1)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5, x6;
  int res ATTRIBUTE_UNUSED;
  if (pnum_clobbers == NULL)
    return -1;
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) != E_QImode)
    return -1;
  x4 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x4) != SET)
    return -1;
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) != UNSPEC
      || XVECLEN (x5, 0) != 2
      || XINT (x5, 1) != i1
      || GET_MODE (x5) != E_CC_Cmode)
    return -1;
  x6 = XEXP (x4, 0);
  if (GET_CODE (x6) != REG
      || REGNO (x6) != 72
      || GET_MODE (x6) != E_CC_Cmode
      || pattern8 (x1) != 0)
    return -1;
  return 0;
}

static int
pattern10 (rtx x1, int i1)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5, x6, x7, x8;
  int res ATTRIBUTE_UNUSED;
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) != E_QImode)
    return -1;
  x4 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x4) != SET)
    return -1;
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) != UNSPEC
      || XVECLEN (x5, 0) != 2
      || XINT (x5, 1) != i1
      || GET_MODE (x5) != E_CC_Cmode)
    return -1;
  x6 = XEXP (x4, 0);
  if (GET_CODE (x6) != REG
      || REGNO (x6) != 72
      || GET_MODE (x6) != E_CC_Cmode)
    return -1;
  x7 = XVECEXP (x1, 0, 2);
  if (GET_CODE (x7) != CLOBBER)
    return -1;
  x8 = XEXP (x7, 0);
  if (GET_CODE (x8) != REG
      || REGNO (x8) != 68
      || GET_MODE (x8) != E_CC_NZmode
      || pattern8 (x1) != 0)
    return -1;
  return 0;
}

static int
pattern11 (rtx x1)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5, x6, x7;
  int res ATTRIBUTE_UNUSED;
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 1);
  if (XVECLEN (x3, 0) != 2)
    return -1;
  x4 = XVECEXP (x3, 0, 1);
  if (GET_CODE (x4) != REG
      || REGNO (x4) != 72
      || GET_MODE (x4) != E_CC_Cmode)
    return -1;
  x5 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x5) != SET)
    return -1;
  x6 = XEXP (x5, 1);
  if (GET_CODE (x6) != UNSPEC
      || XVECLEN (x6, 0) != 2
      || GET_MODE (x6) != E_CC_Cmode)
    return -1;
  x7 = XVECEXP (x6, 0, 1);
  if (GET_CODE (x7) != REG
      || REGNO (x7) != 72
      || GET_MODE (x7) != E_CC_Cmode)
    return -1;
  return 0;
}

static int
pattern12 (rtx x1)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5;
  int res ATTRIBUTE_UNUSED;
  x2 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x2) != CLOBBER)
    return -1;
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) != REG
      || REGNO (x3) != 68
      || GET_MODE (x3) != E_CC_NZmode)
    return -1;
  x4 = XVECEXP (x1, 0, 0);
  x5 = XEXP (x4, 0);
  operands[0] = x5;
  return 0;
}

static int
pattern13 (rtx x1)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5;
  int res ATTRIBUTE_UNUSED;
  x2 = XVECEXP (x1, 0, 2);
  if (GET_CODE (x2) != CLOBBER)
    return -1;
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) != REG)
    return -1;
  x4 = XVECEXP (x1, 0, 3);
  x5 = XEXP (x4, 0);
  if (REGNO (x5) != 76
      || GET_MODE (x5) != E_CC_Vmode)
    return -1;
  return 0;
}

static int
pattern14 (rtx x1, machine_mode i1, unsigned int i2)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3;
  int res ATTRIBUTE_UNUSED;
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) != i1)
    return -1;
  x3 = XEXP (x1, 0);
  if (GET_CODE (x3) != REG
      || REGNO (x3) != i2
      || GET_MODE (x3) != i1)
    return -1;
  return 0;
}

static int
pattern15 (rtx x1, int i1, int i2, machine_mode i3, unsigned int i4)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2;
  int res ATTRIBUTE_UNUSED;
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) != UNSPEC
      || XVECLEN (x2, 0) != i2
      || XINT (x2, 1) != i1
      || pattern14 (x1, i3, i4) != 0)
    return -1;
  return 0;
}

static int
pattern16 (rtx x1)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5;
  int res ATTRIBUTE_UNUSED;
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != E_QImode)
    return -1;
  x3 = XEXP (x1, 1);
  if (GET_CODE (x3) != NE
      || GET_MODE (x3) != E_QImode)
    return -1;
  x4 = XEXP (x3, 0);
  if (GET_CODE (x4) != REG
      || REGNO (x4) != 72
      || GET_MODE (x4) != E_CC_Cmode)
    return -1;
  x5 = XEXP (x3, 1);
  if (x5 != const_int_rtx[MAX_SAVED_CONST_INT + 0])
    return -1;
  return 0;
}

static int
pattern17 (rtx x1, machine_mode i1, int i2, int i3, rtx_code i4)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  int res ATTRIBUTE_UNUSED;
  if (GET_CODE (x1) != i4
      || XVECLEN (x1, 0) != i3
      || XINT (x1, 1) != i2
      || GET_MODE (x1) != i1)
    return -1;
  return 0;
}

static int
pattern18 (rtx x1)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3;
  int res ATTRIBUTE_UNUSED;
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) != REG
      || REGNO (x2) != 72
      || GET_MODE (x2) != E_CC_Cmode)
    return -1;
  x3 = XEXP (x1, 1);
  if (x3 != const_int_rtx[MAX_SAVED_CONST_INT + 0])
    return -1;
  return 0;
}

static int
pattern19 (rtx x1)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5, x6, x7, x8, x9;
  rtx x10, x11, x12, x13, x14;
  int res ATTRIBUTE_UNUSED;
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 1);
  x4 = XEXP (x3, 1);
  x5 = XEXP (x4, 1);
  x6 = XEXP (x5, 0);
  if (GET_CODE (x6) != REG
      || REGNO (x6) != 72
      || GET_MODE (x6) != E_CC_Cmode)
    return -1;
  x7 = XEXP (x5, 1);
  if (x7 != const_int_rtx[MAX_SAVED_CONST_INT + 0])
    return -1;
  x8 = XVECEXP (x1, 0, 1);
  if (GET_CODE (x8) != SET)
    return -1;
  x9 = XEXP (x8, 1);
  if (GET_CODE (x9) != UNSPEC
      || XVECLEN (x9, 0) != 3
      || XINT (x9, 1) != 0
      || GET_MODE (x9) != E_CC_NZmode)
    return -1;
  x10 = XVECEXP (x9, 0, 2);
  if (GET_CODE (x10) != REG
      || REGNO (x10) != 72
      || GET_MODE (x10) != E_CC_Cmode)
    return -1;
  x11 = XEXP (x8, 0);
  if (GET_CODE (x11) != REG
      || REGNO (x11) != 68
      || GET_MODE (x11) != E_CC_NZmode)
    return -1;
  x12 = XVECEXP (x1, 0, 2);
  if (GET_CODE (x12) != SET)
    return -1;
  x13 = XEXP (x12, 1);
  if (GET_CODE (x13) != UNSPEC
      || XVECLEN (x13, 0) != 3
      || XINT (x13, 1) != 1
      || GET_MODE (x13) != E_CC_Vmode)
    return -1;
  x14 = XVECEXP (x13, 0, 2);
  if (GET_CODE (x14) != REG
      || REGNO (x14) != 72
      || GET_MODE (x14) != E_CC_Cmode)
    return -1;
  return 0;
}

static int
pattern20 (rtx x1, machine_mode i1, unsigned int i2)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  int res ATTRIBUTE_UNUSED;
  if (GET_CODE (x1) != REG
      || REGNO (x1) != i2
      || GET_MODE (x1) != i1
      || !accumulator_operand (operands[0], E_QImode))
    return -1;
  return 0;
}

static int
pattern21 (rtx x1)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5;
  int res ATTRIBUTE_UNUSED;
  x2 = XVECEXP (x1, 0, 1);
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) != REG
      || REGNO (x3) != 68
      || GET_MODE (x3) != E_CC_NZmode)
    return -1;
  x4 = XVECEXP (x1, 0, 2);
  x5 = XEXP (x4, 0);
  if (REGNO (x5) != 72
      || GET_MODE (x5) != E_CC_Cmode)
    return -1;
  return 0;
}

static int
pattern22 (rtx x1, int i1)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3;
  int res ATTRIBUTE_UNUSED;
  if (GET_CODE (x1) != SET)
    return -1;
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) != UNSPEC
      || XVECLEN (x2, 0) != 3
      || XINT (x2, 1) != i1
      || GET_MODE (x2) != E_CC_Cmode)
    return -1;
  x3 = XVECEXP (x2, 0, 2);
  if (GET_CODE (x3) != REG
      || REGNO (x3) != 72
      || GET_MODE (x3) != E_CC_Cmode)
    return -1;
  return 0;
}

static int
pattern23 (rtx x1, rtx_code i1, machine_mode i2, unsigned int i3)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4;
  int res ATTRIBUTE_UNUSED;
  x2 = XVECEXP (x1, 0, 1);
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) != REG
      || REGNO (x3) != i3
      || GET_MODE (x3) != i2)
    return -1;
  x4 = XVECEXP (x1, 0, 2);
  if (GET_CODE (x4) != i1)
    return -1;
  return 0;
}

static int
pattern24 (rtx x1, machine_mode i1, unsigned int i2, machine_mode i3, unsigned int i4)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5;
  int res ATTRIBUTE_UNUSED;
  x2 = XVECEXP (x1, 0, 2);
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) != REG
      || REGNO (x3) != i4
      || GET_MODE (x3) != i3)
    return -1;
  x4 = XVECEXP (x1, 0, 3);
  x5 = XEXP (x4, 0);
  if (REGNO (x5) != i2
      || GET_MODE (x5) != i1)
    return -1;
  return 0;
}

static int
pattern25 (rtx x1)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5, x6, x7, x8;
  int res ATTRIBUTE_UNUSED;
  x2 = XVECEXP (x1, 0, 0);
  x3 = XEXP (x2, 0);
  operands[0] = x3;
  if (!reg_or_const_mem_operand (operands[0], E_QImode))
    return -1;
  x4 = XEXP (x2, 1);
  if (GET_MODE (x4) != E_QImode)
    return -1;
  x5 = XVECEXP (x4, 0, 0);
  operands[1] = x5;
  if (!reg_or_const_mem_operand (operands[1], E_QImode))
    return -1;
  x6 = XVECEXP (x1, 0, 1);
  x7 = XEXP (x6, 1);
  x8 = XVECEXP (x7, 0, 0);
  if (!rtx_equal_p (x8, operands[1]))
    return -1;
  switch (XINT (x4, 1))
    {
    case 10:
      if (XINT (x7, 1) != 10)
        return -1;
      return 0;

    case 11:
      if (XINT (x7, 1) != 11)
        return -1;
      return 1;

    default:
      return -1;
    }
}

static int
pattern26 (rtx x1, int i1)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5, x6, x7, x8, x9;
  int res ATTRIBUTE_UNUSED;
  x2 = XVECEXP (x1, 0, 1);
  switch (GET_CODE (x2))
    {
    case CLOBBER:
      return pattern21 (x1); /* [-1, 0] */

    case SET:
      x3 = XEXP (x2, 1);
      if (GET_CODE (x3) != UNSPEC
          || XVECLEN (x3, 0) != 3
          || XINT (x3, 1) != i1
          || GET_MODE (x3) != E_CC_Cmode)
        return -1;
      x4 = XVECEXP (x3, 0, 2);
      if (GET_CODE (x4) != REG
          || REGNO (x4) != 72
          || GET_MODE (x4) != E_CC_Cmode)
        return -1;
      x5 = XEXP (x2, 0);
      if (GET_CODE (x5) != REG
          || REGNO (x5) != 72
          || GET_MODE (x5) != E_CC_Cmode)
        return -1;
      x6 = XVECEXP (x1, 0, 2);
      x7 = XEXP (x6, 0);
      if (REGNO (x7) != 68
          || GET_MODE (x7) != E_CC_NZmode)
        return -1;
      x8 = XVECEXP (x3, 0, 0);
      if (!rtx_equal_p (x8, operands[1]))
        return -1;
      x9 = XVECEXP (x3, 0, 1);
      if (!rtx_equal_p (x9, operands[2]))
        return -1;
      return 1;

    default:
      return -1;
    }
}

static int
pattern27 (rtx x1)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5, x6, x7, x8, x9;
  int res ATTRIBUTE_UNUSED;
  x2 = XVECEXP (x1, 0, 1);
  x3 = XEXP (x2, 1);
  x4 = XVECEXP (x3, 0, 0);
  if (!rtx_equal_p (x4, operands[1]))
    return -1;
  x5 = XVECEXP (x3, 0, 1);
  if (!rtx_equal_p (x5, operands[2]))
    return -1;
  x6 = XVECEXP (x1, 0, 2);
  x7 = XEXP (x6, 1);
  x8 = XVECEXP (x7, 0, 0);
  if (!rtx_equal_p (x8, operands[1]))
    return -1;
  x9 = XVECEXP (x7, 0, 1);
  if (!rtx_equal_p (x9, operands[2]))
    return -1;
  return 0;
}

static int
recog_1 (rtx x1 ATTRIBUTE_UNUSED,
	rtx_insn *insn ATTRIBUTE_UNUSED,
	int *pnum_clobbers ATTRIBUTE_UNUSED)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2;
  int res ATTRIBUTE_UNUSED;
  x2 = XEXP (x1, 1);
  operands[1] = x2;
  switch (GET_MODE (operands[0]))
    {
    case E_HImode:
      if (!nonimmediate_operand (operands[0], E_HImode)
          || !general_operand (operands[1], E_HImode)
          || !
#line 233 "../../gcc/config/6502/6502.md"
((register_operand (operands[0], HImode)
    || register_operand (operands[1], HImode))
   && (lra_in_progress || reload_completed)))
        return -1;
      return 1; /* movhi_noclob */

    case E_SImode:
      if (!nonimmediate_operand (operands[0], E_SImode)
          || !general_operand (operands[1], E_SImode)
          || !
#line 233 "../../gcc/config/6502/6502.md"
((register_operand (operands[0], SImode)
    || register_operand (operands[1], SImode))
   && (lra_in_progress || reload_completed)))
        return -1;
      return 2; /* movsi_noclob */

    case E_QImode:
      if (pnum_clobbers != NULL)
        {
          if (hard_reg_operand (operands[0], E_QImode)
              && zp_memory_operand (operands[1], E_QImode))
            {
              *pnum_clobbers = 1;
              return 16; /* loadqi_zp */
            }
          if (zp_memory_operand (operands[0], E_QImode)
              && hard_reg_operand (operands[1], E_QImode))
            {
              *pnum_clobbers = 1;
              return 17; /* storeqi_zp */
            }
        }
      if (!nonimmediate_operand (operands[0], E_QImode)
          || !general_operand (operands[1], E_QImode))
        return -1;
      if (
#line 1101 "../../gcc/config/6502/6502.md"
(m65x_valid_mov_operands (QImode, operands, true)))
        return 18; /* movqi_noclob */
      if (pnum_clobbers == NULL
          || !
#line 1119 "../../gcc/config/6502/6502.md"
(m65x_valid_mov_operands (QImode, operands, true)))
        return -1;
      *pnum_clobbers = 1;
      return 19; /* movqi_insn */

    default:
      return -1;
    }
}

static int
recog_2 (rtx x1 ATTRIBUTE_UNUSED,
	rtx_insn *insn ATTRIBUTE_UNUSED,
	int *pnum_clobbers ATTRIBUTE_UNUSED)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5, x6, x7;
  int res ATTRIBUTE_UNUSED;
  if (pnum_clobbers != NULL)
    {
      x2 = XEXP (x1, 1);
      x3 = XEXP (x2, 0);
      x4 = XEXP (x3, 0);
      operands[0] = x4;
      if (zp_reg_operand (operands[0], E_HImode))
        {
          x5 = XEXP (x3, 1);
          operands[1] = x5;
          if (zp_reg_or_imm_operand (operands[1], E_HImode))
            {
              x6 = XEXP (x2, 1);
              x7 = XEXP (x6, 0);
              operands[2] = x7;
              switch (GET_CODE (x3))
                {
                case EQ:
                  *pnum_clobbers = 1;
                  return 6; /* cbrhi_eq */

                case NE:
                  *pnum_clobbers = 1;
                  return 7; /* cbrhi_ne */

                default:
                  break;
                }
            }
        }
    }
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 0);
  operands[0] = x3;
  if (!qimode_c_comparison (operands[0], E_VOIDmode))
    return -1;
  x4 = XEXP (x3, 0);
  if (GET_CODE (x4) != REG
      || REGNO (x4) != 0
      || GET_MODE (x4) != E_CC_Cmode)
    return -1;
  x6 = XEXP (x2, 1);
  x7 = XEXP (x6, 0);
  operands[1] = x7;
  return 53; /* condbranchqi_cc_c */
}

static int
recog_3 (rtx x1 ATTRIBUTE_UNUSED,
	rtx_insn *insn ATTRIBUTE_UNUSED,
	int *pnum_clobbers ATTRIBUTE_UNUSED)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5, x6, x7, x8, x9;
  rtx x10, x11, x12, x13, x14, x15, x16, x17;
  rtx x18, x19, x20, x21;
  int res ATTRIBUTE_UNUSED;
  x2 = XVECEXP (x1, 0, 0);
  switch (GET_CODE (x2))
    {
    case SET:
      x3 = XEXP (x2, 1);
      switch (GET_CODE (x3))
        {
        case IF_THEN_ELSE:
          switch (pattern6 (x1))
            {
            case 0:
              return 6; /* cbrhi_eq */

            case 1:
              return 7; /* cbrhi_ne */

            case 2:
              return 8; /* cbrhi_ltu */

            case 3:
              return 9; /* cbrhi_gtu */

            case 4:
              return 10; /* cbrhi_geu */

            case 5:
              return 11; /* cbrhi_leu */

            case 6:
              return 12; /* cbrhi_lt */

            case 7:
              return 13; /* cbrhi_gt */

            case 8:
              return 14; /* cbrhi_ge */

            case 9:
              return 15; /* cbrhi_le */

            default:
              return -1;
            }

        case CONST_INT:
        case CONST_WIDE_INT:
        case CONST_POLY_INT:
        case CONST_FIXED:
        case CONST_DOUBLE:
        case CONST_VECTOR:
        case CONST:
        case REG:
        case SUBREG:
        case MEM:
        case LABEL_REF:
        case SYMBOL_REF:
        case HIGH:
          operands[1] = x3;
          x4 = XEXP (x2, 0);
          operands[0] = x4;
          x5 = XVECEXP (x1, 0, 1);
          switch (GET_CODE (x5))
            {
            case CLOBBER:
              x6 = XEXP (x5, 0);
              if (GET_CODE (x6) == REG
                  && REGNO (x6) == 68
                  && GET_MODE (x6) == E_CC_NZmode)
                {
                  if (hard_reg_operand (operands[0], E_QImode)
                      && zp_memory_operand (operands[1], E_QImode))
                    return 16; /* loadqi_zp */
                  if (zp_memory_operand (operands[0], E_QImode)
                      && hard_reg_operand (operands[1], E_QImode))
                    return 17; /* storeqi_zp */
                  if (nonimmediate_operand (operands[0], E_QImode)
                      && general_operand (operands[1], E_QImode)
                      && 
#line 1119 "../../gcc/config/6502/6502.md"
(m65x_valid_mov_operands (QImode, operands, true)))
                    return 19; /* movqi_insn */
                }
              if (reload_zpreg_operand (operands[0], E_QImode)
                  && reload_zpreg_operand (operands[1], E_QImode))
                {
                  operands[2] = x6;
                  if (scratch_operand (operands[2], E_QImode))
                    return 26; /* movqi_loadstore_zp */
                }
              break;

            case SET:
              if (pattern15 (x5, 9, 1, E_CC_NZmode, 68) == 0
                  && hard_reg_operand (operands[0], E_QImode)
                  && qimode_src_operand (operands[1], E_QImode))
                {
                  x7 = XEXP (x5, 1);
                  x8 = XVECEXP (x7, 0, 0);
                  if (rtx_equal_p (x8, operands[1]))
                    return 45; /* loadqi_nz */
                }
              break;

            default:
              break;
            }
          if (GET_CODE (x3) != MEM
              || GET_MODE (x3) != E_QImode)
            return -1;
          x9 = XEXP (x3, 0);
          if (GET_CODE (x9) != UNSPEC
              || XVECLEN (x9, 0) != 2
              || XINT (x9, 1) != 20
              || GET_MODE (x9) != E_QImode
              || pattern12 (x1) != 0
              || !accumulator_operand (operands[0], E_QImode))
            return -1;
          x10 = XVECEXP (x9, 0, 0);
          operands[1] = x10;
          if (!zp_reg_operand (operands[1], E_HImode))
            return -1;
          x11 = XVECEXP (x9, 0, 1);
          operands[2] = x11;
          if (!x_reg_operand (operands[2], E_QImode))
            return -1;
          return 21; /* loadqi_xind_nz */

        case PLUS:
          if (GET_MODE (x3) != E_QImode)
            return -1;
          x4 = XEXP (x2, 0);
          operands[0] = x4;
          x9 = XEXP (x3, 0);
          switch (GET_CODE (x9))
            {
            case REG:
            case SUBREG:
            case MEM:
              operands[1] = x9;
              if (!incdec_operand (operands[1], E_QImode)
                  || !incdec_operand (operands[0], E_QImode))
                return -1;
              x12 = XEXP (x3, 1);
              operands[2] = x12;
              if (!one_or_minus_one_amount (operands[2], E_QImode))
                return -1;
              x5 = XVECEXP (x1, 0, 1);
              switch (GET_CODE (x5))
                {
                case CLOBBER:
                  x6 = XEXP (x5, 0);
                  if (GET_CODE (x6) != REG
                      || REGNO (x6) != 68
                      || GET_MODE (x6) != E_CC_NZmode)
                    return -1;
                  return 27; /* incdecqi3 */

                case SET:
                  if (pattern15 (x5, 8, 2, E_CC_NZmode, 68) != 0)
                    return -1;
                  x7 = XEXP (x5, 1);
                  x8 = XVECEXP (x7, 0, 0);
                  if (!rtx_equal_p (x8, operands[1]))
                    return -1;
                  x13 = XVECEXP (x7, 0, 1);
                  if (!rtx_equal_p (x13, operands[2]))
                    return -1;
                  return 28; /* incdecqi3_nz */

                default:
                  return -1;
                }

            case PLUS:
              if (pnum_clobbers == NULL
                  || pattern16 (x3) != 0)
                return -1;
              x5 = XVECEXP (x1, 0, 1);
              if (pattern22 (x5, 4) != 0)
                return -1;
              x6 = XEXP (x5, 0);
              if (pattern20 (x6, E_CC_Cmode, 72) != 0)
                return -1;
              x14 = XEXP (x9, 0);
              operands[1] = x14;
              if (!accumulator_operand (operands[1], E_QImode))
                return -1;
              x15 = XEXP (x9, 1);
              operands[2] = x15;
              if (!qimode_src_operand (operands[2], E_QImode))
                return -1;
              x7 = XEXP (x5, 1);
              x8 = XVECEXP (x7, 0, 0);
              if (!rtx_equal_p (x8, operands[1]))
                return -1;
              x13 = XVECEXP (x7, 0, 1);
              if (!rtx_equal_p (x13, operands[2]))
                return -1;
              *pnum_clobbers = 2;
              return 35; /* adcqi3_c */

            default:
              return -1;
            }

        case MINUS:
          if (pattern7 (x3, pnum_clobbers, EQ) != 0)
            return -1;
          x12 = XEXP (x3, 1);
          x16 = XEXP (x12, 1);
          if (pattern18 (x16) != 0)
            return -1;
          x5 = XVECEXP (x1, 0, 1);
          if (pattern22 (x5, 5) != 0)
            return -1;
          x6 = XEXP (x5, 0);
          if (GET_CODE (x6) != REG
              || REGNO (x6) != 72
              || GET_MODE (x6) != E_CC_Cmode)
            return -1;
          x4 = XEXP (x2, 0);
          operands[0] = x4;
          if (!accumulator_operand (operands[0], E_QImode))
            return -1;
          x9 = XEXP (x3, 0);
          operands[1] = x9;
          if (!accumulator_operand (operands[1], E_QImode))
            return -1;
          x17 = XEXP (x12, 0);
          operands[2] = x17;
          if (!qimode_src_operand (operands[2], E_QImode))
            return -1;
          x7 = XEXP (x5, 1);
          x8 = XVECEXP (x7, 0, 0);
          if (!rtx_equal_p (x8, operands[1]))
            return -1;
          x13 = XVECEXP (x7, 0, 1);
          if (!rtx_equal_p (x13, operands[2]))
            return -1;
          *pnum_clobbers = 2;
          return 37; /* sbcqi3_c */

        case XOR:
          if (GET_MODE (x3) != E_QImode)
            return -1;
          x4 = XEXP (x2, 0);
          operands[0] = x4;
          if (!accumulator_operand (operands[0], E_QImode))
            return -1;
          x9 = XEXP (x3, 0);
          operands[1] = x9;
          if (!accumulator_operand (operands[1], E_QImode))
            return -1;
          x12 = XEXP (x3, 1);
          if (GET_CODE (x12) == CONST_INT
              && XWINT (x12, 0) == 128L)
            {
              x5 = XVECEXP (x1, 0, 1);
              if (GET_CODE (x5) == SET
                  && pattern15 (x5, 2, 1, E_CC_NZmode, 68) == 0)
                {
                  x7 = XEXP (x5, 1);
                  x8 = XVECEXP (x7, 0, 0);
                  if (rtx_equal_p (x8, operands[1]))
                    return 39; /* negate_highbit */
                }
            }
          x5 = XVECEXP (x1, 0, 1);
          if (GET_CODE (x5) != CLOBBER)
            return -1;
          x6 = XEXP (x5, 0);
          if (GET_CODE (x6) != REG
              || REGNO (x6) != 68
              || GET_MODE (x6) != E_CC_NZmode)
            return -1;
          operands[2] = x12;
          if (!qimode_src_operand (operands[2], E_QImode))
            return -1;
          return 44; /* xorqi3 */

        case AND:
          if (GET_MODE (x3) != E_QImode
              || pattern12 (x1) != 0)
            return -1;
          x12 = XEXP (x3, 1);
          operands[2] = x12;
          x9 = XEXP (x3, 0);
          switch (GET_CODE (x9))
            {
            case REG:
            case SUBREG:
              operands[1] = x9;
              if (!accumulator_operand (operands[1], E_QImode)
                  || !accumulator_operand (operands[0], E_QImode)
                  || !qimode_src_operand (operands[2], E_QImode))
                return -1;
              return 40; /* andqi3_insn */

            case NOT:
              if (GET_MODE (x9) != E_QImode
                  || !zp_reg_or_const_mem_operand (operands[0], E_QImode))
                return -1;
              x14 = XEXP (x9, 0);
              operands[1] = x14;
              if (!accumulator_operand (operands[1], E_QImode)
                  || !zp_reg_or_const_mem_operand (operands[2], E_QImode)
                  || !
#line 1996 "../../gcc/config/6502/6502.md"
(TARGET_TRB_TSB))
                return -1;
              return 41; /* m65x_trbqi3 */

            default:
              return -1;
            }

        case IOR:
          if (GET_MODE (x3) != E_QImode)
            return -1;
          x4 = XEXP (x2, 0);
          operands[0] = x4;
          x9 = XEXP (x3, 0);
          operands[1] = x9;
          x12 = XEXP (x3, 1);
          operands[2] = x12;
          x5 = XVECEXP (x1, 0, 1);
          switch (GET_CODE (x5))
            {
            case CLOBBER:
              x6 = XEXP (x5, 0);
              if (GET_CODE (x6) != REG
                  || REGNO (x6) != 68
                  || GET_MODE (x6) != E_CC_NZmode
                  || !qimode_ior_dst_operand (operands[0], E_QImode)
                  || !qimode_ior_dst_operand (operands[1], E_QImode)
                  || !qimode_ior_src_operand (operands[2], E_QImode))
                return -1;
              return 42; /* iorqi3 */

            case SET:
              x7 = XEXP (x5, 1);
              if (pattern17 (x7, E_CC_NZmode, 3, 2, UNSPEC) != 0)
                return -1;
              x6 = XEXP (x5, 0);
              if (pattern20 (x6, E_CC_NZmode, 68) != 0
                  || !accumulator_operand (operands[1], E_QImode)
                  || !qimode_src_operand (operands[2], E_QImode))
                return -1;
              x8 = XVECEXP (x7, 0, 0);
              if (!rtx_equal_p (x8, operands[1]))
                return -1;
              x13 = XVECEXP (x7, 0, 1);
              if (!rtx_equal_p (x13, operands[2]))
                return -1;
              return 43; /* iorqi3_nz */

            default:
              return -1;
            }

        case ASHIFT:
          if (pattern9 (x1, pnum_clobbers, 6) != 0)
            return -1;
          *pnum_clobbers = 1;
          return 46; /* ashlqi3_insn */

        case UNSPEC:
          if (pnum_clobbers == NULL
              || pattern11 (x1) != 0)
            return -1;
          x5 = XVECEXP (x1, 0, 1);
          x6 = XEXP (x5, 0);
          if (GET_CODE (x6) != REG
              || REGNO (x6) != 72
              || GET_MODE (x6) != E_CC_Cmode)
            return -1;
          switch (pattern25 (x1))
            {
            case 0:
              *pnum_clobbers = 1;
              return 47; /* m65x_rorqi3 */

            case 1:
              *pnum_clobbers = 1;
              return 48; /* m65x_rolqi3 */

            default:
              return -1;
            }

        case LSHIFTRT:
          if (pattern9 (x1, pnum_clobbers, 7) != 0)
            return -1;
          *pnum_clobbers = 1;
          return 49; /* lshrqi3_insn */

        case NOT:
          if (GET_MODE (x3) != E_QImode)
            return -1;
          x5 = XVECEXP (x1, 0, 1);
          if (pattern2 (x5) != 0)
            return -1;
          x4 = XEXP (x2, 0);
          operands[0] = x4;
          if (!accumulator_operand (operands[0], E_QImode))
            return -1;
          x9 = XEXP (x3, 0);
          operands[1] = x9;
          if (!accumulator_operand (operands[1], E_QImode))
            return -1;
          return 51; /* one_cmplqi2 */

        case COMPARE:
          if (GET_MODE (x3) != E_CC_Cmode)
            return -1;
          x4 = XEXP (x2, 0);
          if (GET_CODE (x4) != REG
              || REGNO (x4) != 72
              || GET_MODE (x4) != E_CC_Cmode)
            return -1;
          x5 = XVECEXP (x1, 0, 1);
          if (GET_CODE (x5) != SET)
            return -1;
          x7 = XEXP (x5, 1);
          if (GET_CODE (x7) != COMPARE
              || pattern14 (x5, E_CC_NZmode, 68) != 0)
            return -1;
          x9 = XEXP (x3, 0);
          operands[0] = x9;
          if (!hard_reg_operand (operands[0], E_QImode))
            return -1;
          x12 = XEXP (x3, 1);
          operands[1] = x12;
          if (!compareqi_src_operand (operands[1], E_QImode))
            return -1;
          x18 = XEXP (x7, 0);
          if (!rtx_equal_p (x18, operands[0]))
            return -1;
          x19 = XEXP (x7, 1);
          if (!rtx_equal_p (x19, operands[1]))
            return -1;
          return 52; /* compareqi */

        case CALL:
          x9 = XEXP (x3, 0);
          if (GET_CODE (x9) != MEM
              || GET_MODE (x9) != E_QImode)
            return -1;
          x5 = XVECEXP (x1, 0, 1);
          if (GET_CODE (x5) != USE)
            return -1;
          x6 = XEXP (x5, 0);
          if (x6 != const_int_rtx[MAX_SAVED_CONST_INT + 0])
            return -1;
          x4 = XEXP (x2, 0);
          operands[0] = x4;
          x14 = XEXP (x9, 0);
          operands[1] = x14;
          x12 = XEXP (x3, 1);
          operands[2] = x12;
          if (!general_operand (operands[2], E_VOIDmode))
            return -1;
          if (
#line 2457 "../../gcc/config/6502/6502.md"
(GET_CODE (operands[1]) == SYMBOL_REF))
            return 62; /* call_value_sym */
          if (!zp_reg_operand (operands[1], E_HImode))
            return -1;
          return 63; /* call_indirect_sym */

        default:
          return -1;
        }

    case CALL:
      x4 = XEXP (x2, 0);
      if (GET_CODE (x4) != MEM
          || GET_MODE (x4) != E_QImode)
        return -1;
      x5 = XVECEXP (x1, 0, 1);
      if (GET_CODE (x5) != USE)
        return -1;
      x6 = XEXP (x5, 0);
      if (x6 != const_int_rtx[MAX_SAVED_CONST_INT + 0])
        return -1;
      x20 = XEXP (x4, 0);
      operands[0] = x20;
      x3 = XEXP (x2, 1);
      operands[1] = x3;
      if (
#line 2426 "../../gcc/config/6502/6502.md"
(GET_CODE (operands[0]) == SYMBOL_REF))
        return 60; /* call_sym */
      if (!register_operand (operands[0], E_HImode))
        return -1;
      return 61; /* call_indirect */

    case UNSPEC_VOLATILE:
      if (XVECLEN (x2, 0) != 1)
        return -1;
      x21 = XVECEXP (x2, 0, 0);
      if (x21 != const_int_rtx[MAX_SAVED_CONST_INT + 0])
        return -1;
      x5 = XVECEXP (x1, 0, 1);
      if (GET_CODE (x5) != RETURN)
        return -1;
      switch (XINT (x2, 1))
        {
        case 1:
          return 67; /* m65x_restorestack_s7s0_sp_rts */

        case 3:
          return 68; /* m65x_restorestack_s7s0_rts */

        default:
          return -1;
        }

    default:
      return -1;
    }
}

static int
recog_4 (rtx x1 ATTRIBUTE_UNUSED,
	rtx_insn *insn ATTRIBUTE_UNUSED,
	int *pnum_clobbers ATTRIBUTE_UNUSED)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5, x6, x7, x8, x9;
  rtx x10, x11, x12, x13, x14, x15, x16, x17;
  rtx x18, x19, x20;
  int res ATTRIBUTE_UNUSED;
  x2 = XEXP (x1, 1);
  switch (GET_CODE (x2))
    {
    case CONST_INT:
    case CONST_WIDE_INT:
    case CONST_POLY_INT:
    case CONST_FIXED:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case CONST:
    case REG:
    case SUBREG:
    case MEM:
    case LABEL_REF:
    case SYMBOL_REF:
    case HIGH:
      x3 = XEXP (x1, 0);
      switch (GET_CODE (x3))
        {
        case REG:
        case SUBREG:
        case MEM:
          operands[0] = x3;
          res = recog_1 (x1, insn, pnum_clobbers);
          if (res >= 0)
            return res;
          if (GET_CODE (x3) == MEM
              && GET_MODE (x3) == E_QImode)
            {
              x4 = XEXP (x3, 0);
              if (GET_CODE (x4) == UNSPEC
                  && XVECLEN (x4, 0) == 2
                  && XINT (x4, 1) == 20
                  && GET_MODE (x4) == E_QImode)
                {
                  x5 = XVECEXP (x4, 0, 0);
                  operands[0] = x5;
                  if (zp_reg_operand (operands[0], E_HImode))
                    {
                      x6 = XVECEXP (x4, 0, 1);
                      operands[1] = x6;
                      if (x_reg_operand (operands[1], E_QImode))
                        {
                          operands[2] = x2;
                          if (accumulator_operand (operands[2], E_QImode))
                            return 22; /* storeqi_xind */
                        }
                    }
                }
            }
          break;

        case PC:
          operands[0] = x2;
          if (zp_reg_operand (operands[0], E_HImode))
            return 57; /* indirect_jump */
          break;

        default:
          break;
        }
      switch (GET_CODE (x2))
        {
        case REG:
        case SUBREG:
        case MEM:
          operands[0] = x3;
          if (accumulator_operand (operands[0], E_QImode)
              && GET_CODE (x2) == MEM
              && GET_MODE (x2) == E_QImode)
            {
              x7 = XEXP (x2, 0);
              if (GET_CODE (x7) == UNSPEC
                  && XVECLEN (x7, 0) == 2
                  && XINT (x7, 1) == 20
                  && GET_MODE (x7) == E_QImode)
                {
                  x8 = XVECEXP (x7, 0, 0);
                  operands[1] = x8;
                  if (zp_reg_operand (operands[1], E_HImode))
                    {
                      x9 = XVECEXP (x7, 0, 1);
                      operands[2] = x9;
                      if (x_reg_operand (operands[2], E_QImode))
                        return 20; /* loadqi_xind */
                    }
                }
            }
          if (pnum_clobbers == NULL)
            return -1;
          operands[1] = x2;
          if (zp_reg_or_const_mem_operand (operands[1], E_QImode)
              && zp_reg_or_const_mem_operand (operands[0], E_QImode))
            {
              *pnum_clobbers = 2;
              return 23; /* zp_clob_hardreg_movqi */
            }
          if (!reload_zpreg_operand (operands[1], E_QImode)
              || !reload_zpreg_operand (operands[0], E_QImode))
            return -1;
          *pnum_clobbers = 1;
          return 26; /* movqi_loadstore_zp */

        case LABEL_REF:
          if (GET_CODE (x3) != PC)
            return -1;
          x7 = XEXP (x2, 0);
          operands[0] = x7;
          return 58; /* jump */

        default:
          return -1;
        }

    case PLUS:
      x3 = XEXP (x1, 0);
      operands[0] = x3;
      x7 = XEXP (x2, 0);
      switch (GET_CODE (x7))
        {
        case REG:
        case SUBREG:
        case MEM:
          operands[1] = x7;
          x10 = XEXP (x2, 1);
          operands[2] = x10;
          switch (GET_MODE (operands[0]))
            {
            case E_HImode:
              if (!register_operand (operands[0], E_HImode)
                  || GET_MODE (x2) != E_HImode
                  || !register_operand (operands[1], E_HImode)
                  || !nonmemory_operand (operands[2], E_HImode))
                return -1;
              if (pnum_clobbers != NULL
                  && 
#line 467 "../../gcc/config/6502/6502.md"
(reload_completed))
                {
                  *pnum_clobbers = 3;
                  return 3; /* addhi3_noclobacc */
                }
              return 5; /* addhi3_noclob */

            case E_QImode:
              if (!incdec_operand (operands[0], E_QImode)
                  || GET_MODE (x2) != E_QImode
                  || !incdec_operand (operands[1], E_QImode))
                return -1;
              if (pnum_clobbers != NULL
                  && one_or_minus_one_amount (operands[2], E_QImode))
                {
                  *pnum_clobbers = 1;
                  return 27; /* incdecqi3 */
                }
              if (!general_operand (operands[2], E_QImode))
                return -1;
              if (pnum_clobbers != NULL)
                {
                  *pnum_clobbers = 3;
                  return 29; /* addqi3 */
                }
              return 30; /* addqi3_noclob */

            default:
              return -1;
            }

        case PLUS:
          if (pnum_clobbers == NULL
              || GET_MODE (x7) != E_QImode)
            return -1;
          x10 = XEXP (x2, 1);
          if (GET_CODE (x10) != NE
              || GET_MODE (x10) != E_QImode)
            return -1;
          x11 = XEXP (x10, 0);
          if (GET_CODE (x11) != REG
              || REGNO (x11) != 72
              || GET_MODE (x11) != E_CC_Cmode)
            return -1;
          x12 = XEXP (x10, 1);
          if (x12 != const_int_rtx[MAX_SAVED_CONST_INT + 0]
              || !accumulator_operand (operands[0], E_QImode)
              || GET_MODE (x2) != E_QImode)
            return -1;
          x13 = XEXP (x7, 0);
          operands[1] = x13;
          if (!accumulator_operand (operands[1], E_QImode))
            return -1;
          x14 = XEXP (x7, 1);
          operands[2] = x14;
          if (!qimode_src_operand (operands[2], E_QImode))
            return -1;
          *pnum_clobbers = 3;
          return 34; /* adcqi3 */

        default:
          return -1;
        }

    case IF_THEN_ELSE:
      x10 = XEXP (x2, 1);
      if (GET_CODE (x10) != LABEL_REF)
        return -1;
      x15 = XEXP (x2, 2);
      if (GET_CODE (x15) != PC)
        return -1;
      x3 = XEXP (x1, 0);
      if (GET_CODE (x3) != PC)
        return -1;
      x7 = XEXP (x2, 0);
      switch (GET_CODE (x7))
        {
        case GTU:
          if (pnum_clobbers == NULL)
            return -1;
          x13 = XEXP (x7, 0);
          operands[0] = x13;
          if (!zp_reg_or_imm_operand (operands[0], E_HImode))
            return -1;
          x14 = XEXP (x7, 1);
          operands[1] = x14;
          if (!zp_reg_operand (operands[1], E_HImode))
            return -1;
          x11 = XEXP (x10, 0);
          operands[2] = x11;
          *pnum_clobbers = 1;
          return 9; /* cbrhi_gtu */

        case LEU:
          if (pnum_clobbers == NULL)
            return -1;
          x13 = XEXP (x7, 0);
          operands[0] = x13;
          if (!zp_reg_or_imm_operand (operands[0], E_HImode))
            return -1;
          x14 = XEXP (x7, 1);
          operands[1] = x14;
          if (!zp_reg_operand (operands[1], E_HImode))
            return -1;
          x11 = XEXP (x10, 0);
          operands[2] = x11;
          *pnum_clobbers = 1;
          return 11; /* cbrhi_leu */

        case GT:
          if (pnum_clobbers == NULL)
            return -1;
          x13 = XEXP (x7, 0);
          operands[0] = x13;
          if (!zp_reg_or_imm_operand (operands[0], E_HImode))
            return -1;
          x14 = XEXP (x7, 1);
          operands[1] = x14;
          if (!zp_reg_operand (operands[1], E_HImode))
            return -1;
          x11 = XEXP (x10, 0);
          operands[2] = x11;
          *pnum_clobbers = 1;
          return 13; /* cbrhi_gt */

        case LE:
          if (pnum_clobbers == NULL)
            return -1;
          x13 = XEXP (x7, 0);
          operands[0] = x13;
          if (!zp_reg_or_imm_operand (operands[0], E_HImode))
            return -1;
          x14 = XEXP (x7, 1);
          operands[1] = x14;
          if (!zp_reg_operand (operands[1], E_HImode))
            return -1;
          x11 = XEXP (x10, 0);
          operands[2] = x11;
          *pnum_clobbers = 1;
          return 15; /* cbrhi_le */

        case GEU:
        case LTU:
          if (pnum_clobbers != NULL)
            {
              x13 = XEXP (x7, 0);
              operands[0] = x13;
              if (zp_reg_operand (operands[0], E_HImode))
                {
                  x14 = XEXP (x7, 1);
                  operands[1] = x14;
                  if (zp_reg_or_imm_operand (operands[1], E_HImode))
                    {
                      x11 = XEXP (x10, 0);
                      operands[2] = x11;
                      switch (GET_CODE (x7))
                        {
                        case LTU:
                          *pnum_clobbers = 1;
                          return 8; /* cbrhi_ltu */

                        case GEU:
                          *pnum_clobbers = 1;
                          return 10; /* cbrhi_geu */

                        default:
                          break;
                        }
                    }
                }
            }
          operands[0] = x7;
          x13 = XEXP (x7, 0);
          if (GET_CODE (x13) != REG
              || REGNO (x13) != 0)
            return -1;
          if (GET_MODE (x13) == E_CC_UImode
              && qimode_ui_comparison (operands[0], E_VOIDmode))
            {
              x11 = XEXP (x10, 0);
              operands[1] = x11;
              return 54; /* condbranchqi_cc_ui */
            }
          if (GET_MODE (x13) != E_CC_Vmode
              || !qimode_v_comparison (operands[0], E_VOIDmode))
            return -1;
          x11 = XEXP (x10, 0);
          operands[1] = x11;
          return 55; /* condbranchqi_cc_v */

        case NE:
        case EQ:
        case GE:
        case LT:
          switch (GET_CODE (x7))
            {
            case LT:
              if (pnum_clobbers != NULL)
                {
                  x13 = XEXP (x7, 0);
                  operands[0] = x13;
                  if (zp_reg_operand (operands[0], E_HImode))
                    {
                      x14 = XEXP (x7, 1);
                      operands[1] = x14;
                      if (zp_reg_or_imm_operand (operands[1], E_HImode))
                        {
                          x11 = XEXP (x10, 0);
                          operands[2] = x11;
                          *pnum_clobbers = 1;
                          return 12; /* cbrhi_lt */
                        }
                    }
                }
              break;

            case GE:
              if (pnum_clobbers != NULL)
                {
                  x13 = XEXP (x7, 0);
                  operands[0] = x13;
                  if (zp_reg_operand (operands[0], E_HImode))
                    {
                      x14 = XEXP (x7, 1);
                      operands[1] = x14;
                      if (zp_reg_or_imm_operand (operands[1], E_HImode))
                        {
                          x11 = XEXP (x10, 0);
                          operands[2] = x11;
                          *pnum_clobbers = 1;
                          return 14; /* cbrhi_ge */
                        }
                    }
                }
              break;

            case NE:
            case EQ:
              res = recog_2 (x1, insn, pnum_clobbers);
              if (res >= 0)
                return res;
              break;

            default:
              break;
            }
          operands[0] = x7;
          if (!qimode_nz_comparison (operands[0], E_VOIDmode))
            return -1;
          x13 = XEXP (x7, 0);
          if (GET_CODE (x13) != REG
              || REGNO (x13) != 0
              || GET_MODE (x13) != E_CC_NZmode)
            return -1;
          x11 = XEXP (x10, 0);
          operands[1] = x11;
          return 56; /* condbranchqi_cc_nz */

        default:
          return -1;
        }

    case UNSPEC_VOLATILE:
      if (XVECLEN (x2, 0) != 3
          || XINT (x2, 1) != 16
          || GET_MODE (x2) != E_CCmode)
        return -1;
      x16 = XVECEXP (x2, 0, 0);
      if (GET_CODE (x16) != REG
          || REGNO (x16) != 68
          || GET_MODE (x16) != E_CC_NZmode)
        return -1;
      x17 = XVECEXP (x2, 0, 1);
      if (GET_CODE (x17) != REG
          || REGNO (x17) != 72
          || GET_MODE (x17) != E_CC_Cmode)
        return -1;
      x18 = XVECEXP (x2, 0, 2);
      if (GET_CODE (x18) != REG
          || REGNO (x18) != 76
          || GET_MODE (x18) != E_CC_Vmode)
        return -1;
      x3 = XEXP (x1, 0);
      operands[0] = x3;
      if (!hard_sp_operand (operands[0], E_CCmode))
        return -1;
      return 24; /* pushflags */

    case MINUS:
      if (pattern0 (x1, pnum_clobbers) != 0)
        return -1;
      x10 = XEXP (x2, 1);
      switch (GET_CODE (x10))
        {
        case SUBREG:
        case MEM:
          operands[2] = x10;
          if (!memory_operand (operands[2], E_QImode))
            return -1;
          *pnum_clobbers = 3;
          return 31; /* subqi3 */

        case MINUS:
          if (GET_MODE (x10) != E_QImode)
            return -1;
          x12 = XEXP (x10, 1);
          if (GET_CODE (x12) != EQ
              || GET_MODE (x12) != E_QImode)
            return -1;
          x19 = XEXP (x12, 0);
          if (GET_CODE (x19) != REG
              || REGNO (x19) != 72
              || GET_MODE (x19) != E_CC_Cmode)
            return -1;
          x20 = XEXP (x12, 1);
          if (x20 != const_int_rtx[MAX_SAVED_CONST_INT + 0])
            return -1;
          x11 = XEXP (x10, 0);
          operands[2] = x11;
          if (!qimode_src_operand (operands[2], E_QImode))
            return -1;
          *pnum_clobbers = 3;
          return 36; /* sbcqi3 */

        default:
          return -1;
        }

    case UNSPEC:
      if (XVECLEN (x2, 0) != 1)
        return -1;
      x16 = XVECEXP (x2, 0, 0);
      if (x16 != const_int_rtx[MAX_SAVED_CONST_INT + 0])
        return -1;
      x3 = XEXP (x1, 0);
      if (GET_CODE (x3) != REG
          || REGNO (x3) != 72
          || GET_MODE (x3) != E_CC_Cmode
          || GET_MODE (x2) != E_CC_Cmode)
        return -1;
      switch (XINT (x2, 1))
        {
        case 21:
          return 32; /* clc */

        case 22:
          return 33; /* sec */

        default:
          return -1;
        }

    case AND:
      if (pnum_clobbers == NULL
          || GET_MODE (x2) != E_QImode)
        return -1;
      x3 = XEXP (x1, 0);
      operands[0] = x3;
      x10 = XEXP (x2, 1);
      operands[2] = x10;
      x7 = XEXP (x2, 0);
      switch (GET_CODE (x7))
        {
        case REG:
        case SUBREG:
          operands[1] = x7;
          if (!accumulator_operand (operands[1], E_QImode)
              || !accumulator_operand (operands[0], E_QImode)
              || !qimode_src_operand (operands[2], E_QImode))
            return -1;
          *pnum_clobbers = 1;
          return 40; /* andqi3_insn */

        case NOT:
          if (GET_MODE (x7) != E_QImode
              || !zp_reg_or_const_mem_operand (operands[0], E_QImode))
            return -1;
          x13 = XEXP (x7, 0);
          operands[1] = x13;
          if (!accumulator_operand (operands[1], E_QImode)
              || !zp_reg_or_const_mem_operand (operands[2], E_QImode)
              || !
#line 1996 "../../gcc/config/6502/6502.md"
(TARGET_TRB_TSB))
            return -1;
          *pnum_clobbers = 1;
          return 41; /* m65x_trbqi3 */

        default:
          return -1;
        }

    case IOR:
      if (pnum_clobbers == NULL
          || GET_MODE (x2) != E_QImode)
        return -1;
      x3 = XEXP (x1, 0);
      operands[0] = x3;
      if (!qimode_ior_dst_operand (operands[0], E_QImode))
        return -1;
      x7 = XEXP (x2, 0);
      operands[1] = x7;
      if (!qimode_ior_dst_operand (operands[1], E_QImode))
        return -1;
      x10 = XEXP (x2, 1);
      operands[2] = x10;
      if (!qimode_ior_src_operand (operands[2], E_QImode))
        return -1;
      *pnum_clobbers = 1;
      return 42; /* iorqi3 */

    case XOR:
      if (pattern1 (x1, pnum_clobbers) != 0
          || !qimode_src_operand (operands[2], E_QImode))
        return -1;
      *pnum_clobbers = 1;
      return 44; /* xorqi3 */

    case ASHIFTRT:
      if (pattern1 (x1, pnum_clobbers) != 0
          || !shiftqi_amount (operands[2], E_QImode))
        return -1;
      *pnum_clobbers = 2;
      return 50; /* ashrqi3 */

    case NOT:
      if (pattern0 (x1, pnum_clobbers) != 0)
        return -1;
      *pnum_clobbers = 1;
      return 51; /* one_cmplqi2 */

    default:
      return -1;
    }
}

int
recog (rtx x1 ATTRIBUTE_UNUSED,
	rtx_insn *insn ATTRIBUTE_UNUSED,
	int *pnum_clobbers ATTRIBUTE_UNUSED)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5, x6, x7, x8, x9;
  rtx x10, x11, x12, x13, x14, x15, x16, x17;
  rtx x18, x19, x20, x21, x22, x23;
  int res ATTRIBUTE_UNUSED;
  recog_data.insn = NULL;
  switch (GET_CODE (x1))
    {
    case SET:
      return recog_4 (x1, insn, pnum_clobbers);

    case PARALLEL:
      switch (XVECLEN (x1, 0))
        {
        case 4:
          x2 = XVECEXP (x1, 0, 0);
          if (GET_CODE (x2) != SET)
            return -1;
          x3 = XVECEXP (x1, 0, 3);
          if (GET_CODE (x3) != CLOBBER)
            return -1;
          x4 = XEXP (x3, 0);
          if (GET_CODE (x4) != REG)
            return -1;
          x5 = XEXP (x2, 0);
          operands[0] = x5;
          x6 = XEXP (x2, 1);
          switch (GET_CODE (x6))
            {
            case PLUS:
              if (pattern13 (x1) != 0)
                return -1;
              x7 = XEXP (x6, 0);
              switch (GET_CODE (x7))
                {
                case REG:
                case SUBREG:
                case MEM:
                  operands[1] = x7;
                  x8 = XVECEXP (x1, 0, 1);
                  if (GET_CODE (x8) != CLOBBER
                      || pattern21 (x1) != 0)
                    return -1;
                  x9 = XEXP (x6, 1);
                  operands[2] = x9;
                  switch (GET_MODE (operands[0]))
                    {
                    case E_HImode:
                      if (!register_operand (operands[0], E_HImode)
                          || GET_MODE (x6) != E_HImode
                          || !register_operand (operands[1], E_HImode)
                          || !nonmemory_operand (operands[2], E_HImode)
                          || !
#line 467 "../../gcc/config/6502/6502.md"
(reload_completed))
                        return -1;
                      return 3; /* addhi3_noclobacc */

                    case E_QImode:
                      if (!incdec_operand (operands[0], E_QImode)
                          || GET_MODE (x6) != E_QImode
                          || !incdec_operand (operands[1], E_QImode)
                          || !general_operand (operands[2], E_QImode))
                        return -1;
                      return 29; /* addqi3 */

                    default:
                      return -1;
                    }

                case PLUS:
                  if (pattern16 (x6) != 0
                      || !accumulator_operand (operands[0], E_QImode)
                      || GET_MODE (x6) != E_QImode)
                    return -1;
                  x10 = XEXP (x7, 0);
                  operands[1] = x10;
                  if (!accumulator_operand (operands[1], E_QImode))
                    return -1;
                  x11 = XEXP (x7, 1);
                  operands[2] = x11;
                  if (!qimode_src_operand (operands[2], E_QImode))
                    return -1;
                  switch (pattern26 (x1, 4))
                    {
                    case 0:
                      return 34; /* adcqi3 */

                    case 1:
                      return 35; /* adcqi3_c */

                    default:
                      return -1;
                    }

                default:
                  return -1;
                }

            case MINUS:
              if (GET_MODE (x6) != E_QImode
                  || !accumulator_operand (operands[0], E_QImode))
                return -1;
              x7 = XEXP (x6, 0);
              operands[1] = x7;
              if (!accumulator_operand (operands[1], E_QImode))
                return -1;
              x9 = XEXP (x6, 1);
              switch (GET_CODE (x9))
                {
                case SUBREG:
                case MEM:
                  operands[2] = x9;
                  if (!memory_operand (operands[2], E_QImode))
                    return -1;
                  x8 = XVECEXP (x1, 0, 1);
                  if (GET_CODE (x8) != CLOBBER
                      || pattern23 (x1, CLOBBER, E_CC_NZmode, 68) != 0
                      || pattern24 (x1, E_CC_Vmode, 76, E_CC_Cmode, 72) != 0)
                    return -1;
                  return 31; /* subqi3 */

                case MINUS:
                  if (GET_MODE (x9) != E_QImode)
                    return -1;
                  x12 = XEXP (x9, 0);
                  operands[2] = x12;
                  if (!qimode_src_operand (operands[2], E_QImode))
                    return -1;
                  x13 = XEXP (x9, 1);
                  if (GET_MODE (x13) != E_QImode)
                    return -1;
                  switch (GET_CODE (x13))
                    {
                    case EQ:
                      if (pattern18 (x13) != 0
                          || pattern13 (x1) != 0)
                        return -1;
                      switch (pattern26 (x1, 5))
                        {
                        case 0:
                          return 36; /* sbcqi3 */

                        case 1:
                          return 37; /* sbcqi3_c */

                        default:
                          return -1;
                        }

                    case LTU:
                      if (pattern19 (x1) != 0
                          || pattern24 (x1, E_CC_Cmode, 72, E_CC_Vmode, 76) != 0
                          || pattern27 (x1) != 0)
                        return -1;
                      return 38; /* sbcqi3_nzv */

                    default:
                      return -1;
                    }

                default:
                  return -1;
                }

            default:
              return -1;
            }

        case 5:
          x2 = XVECEXP (x1, 0, 0);
          if (GET_CODE (x2) != SET)
            return -1;
          x6 = XEXP (x2, 1);
          if (GET_CODE (x6) != PLUS
              || pattern5 (x1, E_CC_Cmode, 72, E_CC_NZmode, 68, E_HImode) != 0)
            return -1;
          x3 = XVECEXP (x1, 0, 3);
          if (GET_CODE (x3) != CLOBBER)
            return -1;
          x4 = XEXP (x3, 0);
          if (GET_CODE (x4) != REG
              || REGNO (x4) != 76
              || GET_MODE (x4) != E_CC_Vmode)
            return -1;
          x14 = XVECEXP (x1, 0, 4);
          if (GET_CODE (x14) != CLOBBER)
            return -1;
          x15 = XEXP (x14, 0);
          if (GET_CODE (x15) != REG
              || REGNO (x15) != 0
              || GET_MODE (x15) != E_QImode)
            return -1;
          x5 = XEXP (x2, 0);
          operands[0] = x5;
          if (!register_operand (operands[0], E_HImode))
            return -1;
          x7 = XEXP (x6, 0);
          operands[1] = x7;
          if (!register_operand (operands[1], E_HImode))
            return -1;
          x9 = XEXP (x6, 1);
          operands[2] = x9;
          if (!nonmemory_operand (operands[2], E_HImode)
              || !
#line 484 "../../gcc/config/6502/6502.md"
(reload_completed))
            return -1;
          return 4; /* addhi3_clobacc */

        case 2:
          return recog_3 (x1, insn, pnum_clobbers);

        case 3:
          x2 = XVECEXP (x1, 0, 0);
          if (GET_CODE (x2) != SET)
            return -1;
          x6 = XEXP (x2, 1);
          switch (GET_CODE (x6))
            {
            case REG:
            case SUBREG:
            case MEM:
              operands[1] = x6;
              if (!zp_reg_or_const_mem_operand (operands[1], E_QImode))
                return -1;
              x8 = XVECEXP (x1, 0, 1);
              if (GET_CODE (x8) != CLOBBER)
                return -1;
              x16 = XVECEXP (x1, 0, 2);
              if (pattern3 (x16, E_CC_NZmode, 68) != 0)
                return -1;
              x5 = XEXP (x2, 0);
              operands[0] = x5;
              if (!zp_reg_or_const_mem_operand (operands[0], E_QImode))
                return -1;
              x17 = XEXP (x8, 0);
              operands[2] = x17;
              if (!scratch_operand (operands[2], E_QImode))
                return -1;
              return 23; /* zp_clob_hardreg_movqi */

            case UNSPEC_VOLATILE:
              if (XVECLEN (x6, 0) != 1
                  || XINT (x6, 1) != 17
                  || GET_MODE (x6) != E_CC_NZmode)
                return -1;
              x5 = XEXP (x2, 0);
              if (GET_CODE (x5) != REG
                  || REGNO (x5) != 68
                  || GET_MODE (x5) != E_CC_NZmode)
                return -1;
              x8 = XVECEXP (x1, 0, 1);
              if (GET_CODE (x8) != SET)
                return -1;
              x18 = XEXP (x8, 1);
              if (pattern17 (x18, E_CC_Cmode, 18, 1, UNSPEC_VOLATILE) != 0)
                return -1;
              x19 = XVECEXP (x18, 0, 0);
              if (x19 != const_int_rtx[MAX_SAVED_CONST_INT + 0]
                  || pattern23 (x1, SET, E_CC_Cmode, 72) != 0)
                return -1;
              x16 = XVECEXP (x1, 0, 2);
              x20 = XEXP (x16, 1);
              if (GET_CODE (x20) != UNSPEC_VOLATILE
                  || XVECLEN (x20, 0) != 1
                  || XINT (x20, 1) != 19
                  || GET_MODE (x20) != E_CC_Vmode)
                return -1;
              x21 = XVECEXP (x20, 0, 0);
              if (x21 != const_int_rtx[MAX_SAVED_CONST_INT + 0])
                return -1;
              x22 = XEXP (x16, 0);
              if (GET_CODE (x22) != REG
                  || REGNO (x22) != 76
                  || GET_MODE (x22) != E_CC_Vmode)
                return -1;
              x23 = XVECEXP (x6, 0, 0);
              operands[0] = x23;
              if (!hard_sp_operand (operands[0], E_CCmode))
                return -1;
              return 25; /* popflags */

            case MINUS:
              if (pattern7 (x6, pnum_clobbers, LTU) != 0
                  || pattern19 (x1) != 0)
                return -1;
              x16 = XVECEXP (x1, 0, 2);
              x22 = XEXP (x16, 0);
              if (GET_CODE (x22) != REG
                  || REGNO (x22) != 76
                  || GET_MODE (x22) != E_CC_Vmode)
                return -1;
              x5 = XEXP (x2, 0);
              operands[0] = x5;
              if (!accumulator_operand (operands[0], E_QImode))
                return -1;
              x7 = XEXP (x6, 0);
              operands[1] = x7;
              if (!accumulator_operand (operands[1], E_QImode))
                return -1;
              x9 = XEXP (x6, 1);
              x12 = XEXP (x9, 0);
              operands[2] = x12;
              if (!qimode_src_operand (operands[2], E_QImode)
                  || pattern27 (x1) != 0)
                return -1;
              *pnum_clobbers = 1;
              return 38; /* sbcqi3_nzv */

            case ASHIFT:
              if (pattern10 (x1, 6) != 0)
                return -1;
              return 46; /* ashlqi3_insn */

            case UNSPEC:
              if (pattern11 (x1) != 0
                  || pattern4 (x1, E_CC_NZmode, 68, E_CC_Cmode, 72) != 0)
                return -1;
              switch (pattern25 (x1))
                {
                case 0:
                  return 47; /* m65x_rorqi3 */

                case 1:
                  return 48; /* m65x_rolqi3 */

                default:
                  return -1;
                }

            case LSHIFTRT:
              if (pattern10 (x1, 7) != 0)
                return -1;
              return 49; /* lshrqi3_insn */

            case ASHIFTRT:
              if (pattern5 (x1, E_CC_NZmode, 68, E_CC_Cmode, 72, E_QImode) != 0)
                return -1;
              x5 = XEXP (x2, 0);
              operands[0] = x5;
              if (!accumulator_operand (operands[0], E_QImode))
                return -1;
              x7 = XEXP (x6, 0);
              operands[1] = x7;
              if (!accumulator_operand (operands[1], E_QImode))
                return -1;
              x9 = XEXP (x6, 1);
              operands[2] = x9;
              if (!shiftqi_amount (operands[2], E_QImode))
                return -1;
              return 50; /* ashrqi3 */

            default:
              return -1;
            }

        default:
          return -1;
        }

    case CONST_INT:
      if (XWINT (x1, 0) != 0L)
        return -1;
      return 59; /* nop */

    case RETURN:
      return 64; /* m65x_return */

    case UNSPEC_VOLATILE:
      switch (XVECLEN (x1, 0))
        {
        case 2:
          if (XINT (x1, 1) != 0)
            return -1;
          x2 = XVECEXP (x1, 0, 0);
          operands[0] = x2;
          if (!x_reg_operand (operands[0], E_VOIDmode))
            return -1;
          x8 = XVECEXP (x1, 0, 1);
          operands[1] = x8;
          if (!y_reg_operand (operands[1], E_VOIDmode))
            return -1;
          return 65; /* m65x_savestack_s7s0_sp */

        case 1:
          if (XINT (x1, 1) != 2)
            return -1;
          x2 = XVECEXP (x1, 0, 0);
          if (x2 != const_int_rtx[MAX_SAVED_CONST_INT + 0])
            return -1;
          return 66; /* m65x_savestack_s7s0 */

        default:
          return -1;
        }

    default:
      return -1;
    }
}

rtx_insn *
split_insns (rtx x1 ATTRIBUTE_UNUSED, rtx_insn *insn ATTRIBUTE_UNUSED)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5;
  rtx_insn *res ATTRIBUTE_UNUSED;
  recog_data.insn = NULL;
  switch (GET_CODE (x1))
    {
    case SET:
      x2 = XEXP (x1, 0);
      operands[0] = x2;
      x3 = XEXP (x1, 1);
      operands[1] = x3;
      switch (GET_MODE (operands[0]))
        {
        case E_HImode:
          if (!nonimmediate_operand (operands[0], E_HImode)
              || !general_operand (operands[1], E_HImode)
              || !(
#line 233 "../../gcc/config/6502/6502.md"
((register_operand (operands[0], HImode)
    || register_operand (operands[1], HImode))
   && (lra_in_progress || reload_completed)) && 
#line 237 "../../gcc/config/6502/6502.md"
( reload_completed)))
            return NULL;
          return gen_split_1 (insn, operands);

        case E_SImode:
          if (!nonimmediate_operand (operands[0], E_SImode)
              || !general_operand (operands[1], E_SImode)
              || !(
#line 233 "../../gcc/config/6502/6502.md"
((register_operand (operands[0], SImode)
    || register_operand (operands[1], SImode))
   && (lra_in_progress || reload_completed)) && 
#line 237 "../../gcc/config/6502/6502.md"
( reload_completed)))
            return NULL;
          return gen_split_2 (insn, operands);

        default:
          return NULL;
        }

    case PARALLEL:
      if (XVECLEN (x1, 0) != 2)
        return NULL;
      x4 = XVECEXP (x1, 0, 0);
      if (GET_CODE (x4) != SET)
        return NULL;
      x5 = XEXP (x4, 1);
      if (GET_CODE (x5) != IF_THEN_ELSE)
        return NULL;
      switch (pattern6 (x1))
        {
        case 0:
          if (!
#line 830 "../../gcc/config/6502/6502.md"
(reload_completed))
            return NULL;
          return gen_split_3 (insn, operands);

        case 1:
          if (!
#line 847 "../../gcc/config/6502/6502.md"
(reload_completed))
            return NULL;
          return gen_split_4 (insn, operands);

        case 2:
          if (!
#line 864 "../../gcc/config/6502/6502.md"
(reload_completed))
            return NULL;
          return gen_split_5 (insn, operands);

        case 3:
          if (!
#line 881 "../../gcc/config/6502/6502.md"
(reload_completed))
            return NULL;
          return gen_split_6 (insn, operands);

        case 4:
          if (!
#line 898 "../../gcc/config/6502/6502.md"
(reload_completed))
            return NULL;
          return gen_split_7 (insn, operands);

        case 5:
          if (!
#line 915 "../../gcc/config/6502/6502.md"
(reload_completed))
            return NULL;
          return gen_split_8 (insn, operands);

        case 6:
          if (!
#line 932 "../../gcc/config/6502/6502.md"
(reload_completed))
            return NULL;
          return gen_split_9 (insn, operands);

        case 7:
          if (!
#line 949 "../../gcc/config/6502/6502.md"
(reload_completed))
            return NULL;
          return gen_split_10 (insn, operands);

        case 8:
          if (!
#line 966 "../../gcc/config/6502/6502.md"
(reload_completed))
            return NULL;
          return gen_split_11 (insn, operands);

        case 9:
          if (!
#line 983 "../../gcc/config/6502/6502.md"
(reload_completed))
            return NULL;
          return gen_split_12 (insn, operands);

        default:
          return NULL;
        }

    default:
      return NULL;
    }
}

static rtx_insn *
peephole2_1 (rtx x1 ATTRIBUTE_UNUSED,
	rtx_insn *insn ATTRIBUTE_UNUSED,
	int *pmatch_len_ ATTRIBUTE_UNUSED)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5, x6, x7;
  rtx_insn *res ATTRIBUTE_UNUSED;
  x2 = XEXP (x1, 0);
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) != E_HImode)
    return NULL;
  x4 = XEXP (x3, 0);
  if (GET_CODE (x4) != ZERO_EXTEND
      || GET_MODE (x4) != E_HImode)
    return NULL;
  x5 = XEXP (x4, 0);
  operands[0] = x5;
  x6 = XEXP (x3, 1);
  operands[1] = x6;
  if (zp_reg_operand (operands[1], E_HImode)
      && y_reg_operand (operands[0], E_QImode))
    {
      x7 = XEXP (x1, 1);
      operands[2] = x7;
      if (zp_or_index_reg_operand (operands[2], E_QImode))
        {
          *pmatch_len_ = 0;
          res = gen_peephole2_8 (insn, operands);
          if (res != NULL_RTX)
            return res;
        }
    }
  if (!sym_const_operand (operands[1], E_HImode)
      || !index_reg_operand (operands[0], E_QImode))
    return NULL;
  x7 = XEXP (x1, 1);
  operands[2] = x7;
  if (!zp_reg_operand (operands[2], E_QImode)
      || !
#line 1393 "../../gcc/config/6502/6502.md"
(peep2_regno_dead_p (1, NZ_REGNUM)))
    return NULL;
  *pmatch_len_ = 0;
  return gen_peephole2_18 (insn, operands);
}

rtx_insn *
peephole2_insns (rtx x1 ATTRIBUTE_UNUSED,
	rtx_insn *insn ATTRIBUTE_UNUSED,
	int *pmatch_len_ ATTRIBUTE_UNUSED)
{
  rtx * const operands ATTRIBUTE_UNUSED = &recog_data.operand[0];
  rtx x2, x3, x4, x5, x6, x7, x8, x9;
  rtx x10, x11, x12, x13, x14, x15, x16, x17;
  rtx x18, x19, x20;
  rtx_insn *res ATTRIBUTE_UNUSED;
  recog_data.insn = NULL;
  switch (GET_CODE (x1))
    {
    case PARALLEL:
      if (XVECLEN (x1, 0) != 2)
        return NULL;
      x2 = XVECEXP (x1, 0, 0);
      if (GET_CODE (x2) != SET)
        return NULL;
      x3 = XVECEXP (x1, 0, 1);
      if (pattern2 (x3) != 0)
        return NULL;
      x4 = XEXP (x2, 1);
      switch (GET_CODE (x4))
        {
        case MEM:
          if (GET_MODE (x4) != E_QImode)
            return NULL;
          x5 = XEXP (x2, 0);
          operands[0] = x5;
          x6 = XEXP (x4, 0);
          switch (GET_CODE (x6))
            {
            case REG:
            case SUBREG:
              operands[1] = x6;
              if (!zp_reg_operand (operands[1], E_HImode))
                return NULL;
              if (zp_or_index_reg_operand (operands[0], E_QImode))
                {
                  *pmatch_len_ = 0;
                  res = gen_peephole2_1 (insn, operands);
                  if (res != NULL_RTX)
                    return res;
                }
              if (!accumulator_operand (operands[0], E_QImode)
                  || !
#line 1261 "../../gcc/config/6502/6502.md"
(!TARGET_ZPIND))
                return NULL;
              *pmatch_len_ = 0;
              return gen_peephole2_9 (insn, operands);

            case PLUS:
              if (GET_MODE (x6) != E_HImode)
                return NULL;
              x7 = XEXP (x6, 0);
              if (GET_CODE (x7) != ZERO_EXTEND
                  || GET_MODE (x7) != E_HImode)
                return NULL;
              x8 = XEXP (x7, 0);
              operands[1] = x8;
              if (y_reg_operand (operands[1], E_QImode)
                  && zp_or_index_reg_operand (operands[0], E_QImode))
                {
                  x9 = XEXP (x6, 1);
                  operands[2] = x9;
                  if (zp_reg_operand (operands[2], E_HImode))
                    {
                      *pmatch_len_ = 0;
                      res = gen_peephole2_3 (insn, operands);
                      if (res != NULL_RTX)
                        return res;
                    }
                }
              if (!index_reg_operand (operands[1], E_QImode)
                  || !zp_reg_operand (operands[0], E_QImode))
                return NULL;
              x9 = XEXP (x6, 1);
              operands[2] = x9;
              if (!sym_const_operand (operands[2], E_HImode))
                return NULL;
              *pmatch_len_ = 0;
              return gen_peephole2_15 (insn, operands);

            default:
              return NULL;
            }

        case REG:
        case SUBREG:
          x5 = XEXP (x2, 0);
          switch (GET_CODE (x5))
            {
            case MEM:
              if (GET_MODE (x5) != E_QImode)
                return NULL;
              x10 = XEXP (x5, 0);
              switch (GET_CODE (x10))
                {
                case REG:
                case SUBREG:
                  operands[0] = x10;
                  if (!zp_reg_operand (operands[0], E_HImode))
                    return NULL;
                  operands[1] = x4;
                  if (zp_or_index_reg_operand (operands[1], E_QImode))
                    {
                      *pmatch_len_ = 0;
                      res = gen_peephole2_5 (insn, operands);
                      if (res != NULL_RTX)
                        return res;
                    }
                  if (!accumulator_operand (operands[1], E_QImode)
                      || !
#line 1288 "../../gcc/config/6502/6502.md"
(!TARGET_ZPIND))
                    return NULL;
                  *pmatch_len_ = 0;
                  return gen_peephole2_11 (insn, operands);

                case PLUS:
                  if (GET_MODE (x10) != E_HImode)
                    return NULL;
                  x11 = XEXP (x10, 0);
                  if (GET_CODE (x11) != ZERO_EXTEND
                      || GET_MODE (x11) != E_HImode)
                    return NULL;
                  x12 = XEXP (x11, 0);
                  operands[0] = x12;
                  x13 = XEXP (x10, 1);
                  operands[1] = x13;
                  if (zp_reg_operand (operands[1], E_HImode)
                      && y_reg_operand (operands[0], E_QImode))
                    {
                      operands[2] = x4;
                      if (zp_or_index_reg_operand (operands[2], E_QImode))
                        {
                          *pmatch_len_ = 0;
                          res = gen_peephole2_7 (insn, operands);
                          if (res != NULL_RTX)
                            return res;
                        }
                    }
                  if (!sym_const_operand (operands[1], E_HImode)
                      || !index_reg_operand (operands[0], E_QImode))
                    return NULL;
                  operands[2] = x4;
                  if (!zp_reg_operand (operands[2], E_QImode))
                    return NULL;
                  *pmatch_len_ = 0;
                  return gen_peephole2_17 (insn, operands);

                default:
                  return NULL;
                }

            case REG:
            case SUBREG:
              operands[0] = x5;
              if (!zp_reg_operand (operands[0], E_QImode))
                return NULL;
              operands[1] = x4;
              if (!zp_reg_operand (operands[1], E_QImode))
                return NULL;
              *pmatch_len_ = 0;
              return gen_peephole2_13 (insn, operands);

            default:
              return NULL;
            }

        default:
          return NULL;
        }

    case SET:
      x14 = XEXP (x1, 1);
      switch (GET_CODE (x14))
        {
        case MEM:
          if (GET_MODE (x14) != E_QImode)
            return NULL;
          x15 = XEXP (x1, 0);
          operands[0] = x15;
          x16 = XEXP (x14, 0);
          switch (GET_CODE (x16))
            {
            case REG:
            case SUBREG:
              operands[1] = x16;
              if (zp_reg_operand (operands[1], E_HImode))
                {
                  if (zp_or_index_reg_operand (operands[0], E_QImode))
                    {
                      *pmatch_len_ = 0;
                      res = gen_peephole2_2 (insn, operands);
                      if (res != NULL_RTX)
                        return res;
                    }
                  if (accumulator_operand (operands[0], E_QImode)
                      && 
#line 1273 "../../gcc/config/6502/6502.md"
(!TARGET_ZPIND))
                    {
                      *pmatch_len_ = 0;
                      res = gen_peephole2_10 (insn, operands);
                      if (res != NULL_RTX)
                        return res;
                    }
                }
              break;

            case PLUS:
              if (GET_MODE (x16) == E_HImode)
                {
                  x17 = XEXP (x16, 0);
                  if (GET_CODE (x17) == ZERO_EXTEND
                      && GET_MODE (x17) == E_HImode)
                    {
                      x18 = XEXP (x17, 0);
                      operands[1] = x18;
                      if (y_reg_operand (operands[1], E_QImode)
                          && zp_or_index_reg_operand (operands[0], E_QImode))
                        {
                          x19 = XEXP (x16, 1);
                          operands[2] = x19;
                          if (zp_reg_operand (operands[2], E_HImode))
                            {
                              *pmatch_len_ = 0;
                              res = gen_peephole2_4 (insn, operands);
                              if (res != NULL_RTX)
                                return res;
                            }
                        }
                      if (index_reg_operand (operands[1], E_QImode)
                          && zp_reg_operand (operands[0], E_QImode))
                        {
                          x19 = XEXP (x16, 1);
                          operands[2] = x19;
                          if (sym_const_operand (operands[2], E_HImode)
                              && 
#line 1359 "../../gcc/config/6502/6502.md"
(peep2_regno_dead_p (1, NZ_REGNUM)))
                            {
                              *pmatch_len_ = 0;
                              res = gen_peephole2_16 (insn, operands);
                              if (res != NULL_RTX)
                                return res;
                            }
                        }
                    }
                }
              break;

            default:
              break;
            }
          if (!zp_reg_operand (operands[0], E_QImode))
            return NULL;
          operands[1] = x16;
          if (!sym_const_operand (operands[1], E_HImode))
            return NULL;
          *pmatch_len_ = 0;
          return gen_peephole2_19 (insn, operands);

        case REG:
        case SUBREG:
          x15 = XEXP (x1, 0);
          switch (GET_CODE (x15))
            {
            case MEM:
              if (GET_MODE (x15) != E_QImode)
                return NULL;
              x20 = XEXP (x15, 0);
              switch (GET_CODE (x20))
                {
                case REG:
                case SUBREG:
                  operands[0] = x20;
                  if (zp_reg_operand (operands[0], E_HImode))
                    {
                      operands[1] = x14;
                      if (zp_or_index_reg_operand (operands[1], E_QImode))
                        {
                          *pmatch_len_ = 0;
                          res = gen_peephole2_6 (insn, operands);
                          if (res != NULL_RTX)
                            return res;
                        }
                      if (accumulator_operand (operands[1], E_QImode)
                          && 
#line 1300 "../../gcc/config/6502/6502.md"
(!TARGET_ZPIND))
                        {
                          *pmatch_len_ = 0;
                          res = gen_peephole2_12 (insn, operands);
                          if (res != NULL_RTX)
                            return res;
                        }
                    }
                  break;

                case PLUS:
                  res = peephole2_1 (x1, insn, pmatch_len_);
                  if (res != NULL_RTX)
                    return res;
                  break;

                default:
                  break;
                }
              operands[0] = x20;
              if (!sym_const_operand (operands[0], E_HImode))
                return NULL;
              operands[1] = x14;
              if (!zp_reg_operand (operands[1], E_QImode))
                return NULL;
              *pmatch_len_ = 0;
              return gen_peephole2_20 (insn, operands);

            case REG:
            case SUBREG:
              operands[0] = x15;
              if (!zp_reg_operand (operands[0], E_QImode))
                return NULL;
              operands[1] = x14;
              if (!zp_reg_operand (operands[1], E_QImode)
                  || !
#line 1327 "../../gcc/config/6502/6502.md"
(peep2_regno_dead_p (1, NZ_REGNUM)))
                return NULL;
              *pmatch_len_ = 0;
              return gen_peephole2_14 (insn, operands);

            default:
              return NULL;
            }

        default:
          return NULL;
        }

    default:
      return NULL;
    }
}
