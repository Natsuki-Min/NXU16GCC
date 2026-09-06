/* Generated automatically by the program 'build/genpreds'
   from the machine description file '../../gcc/config/6502/6502.md'.  */

#ifndef GCC_TM_CONSTRS_H
#define GCC_TM_CONSTRS_H

static inline bool
satisfies_constraint_m (rtx op)
{
  return (GET_CODE (op) == MEM) && (
#line 54 "../../gcc/common.md"
(memory_address_addr_space_p (GET_MODE (op), XEXP (op, 0),
						 MEM_ADDR_SPACE (op))));
}
static inline bool
satisfies_constraint_o (rtx op)
{
  return (GET_CODE (op) == MEM) && (
#line 60 "../../gcc/common.md"
(offsettable_nonstrict_memref_p (op)));
}
static inline bool
satisfies_constraint_V (rtx op)
{
  return (GET_CODE (op) == MEM) && ((
#line 69 "../../gcc/common.md"
(memory_address_addr_space_p (GET_MODE (op), XEXP (op, 0),
						 MEM_ADDR_SPACE (op)))) && (!(
#line 71 "../../gcc/common.md"
(offsettable_nonstrict_memref_p (op)))));
}
static inline bool
satisfies_constraint__l (rtx op)
{
  return (GET_CODE (op) == MEM) && ((
#line 78 "../../gcc/common.md"
(GET_CODE (XEXP (op, 0)) == PRE_DEC)) || (
#line 79 "../../gcc/common.md"
(GET_CODE (XEXP (op, 0)) == POST_DEC)));
}
static inline bool
satisfies_constraint__g (rtx op)
{
  return (GET_CODE (op) == MEM) && ((
#line 85 "../../gcc/common.md"
(GET_CODE (XEXP (op, 0)) == PRE_INC)) || (
#line 86 "../../gcc/common.md"
(GET_CODE (XEXP (op, 0)) == POST_INC)));
}
static inline bool
satisfies_constraint_p (rtx ARG_UNUSED (op))
{
  return 
#line 90 "../../gcc/common.md"
(address_operand (op, VOIDmode));
}
static inline bool
satisfies_constraint_i (rtx op)
{
  return (
#line 94 "../../gcc/common.md"
(CONSTANT_P (op))) && (
#line 95 "../../gcc/common.md"
(!flag_pic || LEGITIMATE_PIC_OPERAND_P (op)));
}
static inline bool
satisfies_constraint_s (rtx op)
{
  return (
#line 99 "../../gcc/common.md"
(CONSTANT_P (op))) && ((
#line 100 "../../gcc/common.md"
(!CONST_SCALAR_INT_P (op))) && (
#line 101 "../../gcc/common.md"
(!flag_pic || LEGITIMATE_PIC_OPERAND_P (op))));
}
static inline bool
satisfies_constraint_n (rtx op)
{
  return (
#line 105 "../../gcc/common.md"
(CONST_SCALAR_INT_P (op))) && (
#line 106 "../../gcc/common.md"
(!flag_pic || LEGITIMATE_PIC_OPERAND_P (op)));
}
static inline bool
satisfies_constraint_E (rtx op)
{
  return (
#line 110 "../../gcc/common.md"
(CONST_DOUBLE_AS_FLOAT_P (op))) || (
#line 111 "../../gcc/common.md"
(GET_CODE (op) == CONST_VECTOR
		    && GET_MODE_CLASS (GET_MODE (op)) == MODE_VECTOR_FLOAT));
}
static inline bool
satisfies_constraint_F (rtx op)
{
  return (
#line 117 "../../gcc/common.md"
(CONST_DOUBLE_AS_FLOAT_P (op))) || (
#line 118 "../../gcc/common.md"
(GET_CODE (op) == CONST_VECTOR
		    && GET_MODE_CLASS (GET_MODE (op)) == MODE_VECTOR_FLOAT));
}
static inline bool
satisfies_constraint_X (rtx ARG_UNUSED (op))
{
  return 
#line 123 "../../gcc/common.md"
(true);
}
static inline bool
satisfies_constraint_I (rtx op)
{
  HOST_WIDE_INT ival = 0;
  if (CONST_INT_P (op))
    ival = INTVAL (op);
  return (GET_CODE (op) == CONST_INT) && (
#line 24 "../../gcc/config/6502/constraints.md"
(ival >= 0 && ival <= 255));
}
static inline bool
satisfies_constraint_J (rtx op)
{
  HOST_WIDE_INT ival = 0;
  if (CONST_INT_P (op))
    ival = INTVAL (op);
  return (GET_CODE (op) == CONST_INT) && (
#line 29 "../../gcc/config/6502/constraints.md"
(ival >= 0 && ival <= 65535));
}
static inline bool
satisfies_constraint_K (rtx op)
{
  HOST_WIDE_INT ival = 0;
  if (CONST_INT_P (op))
    ival = INTVAL (op);
  return (GET_CODE (op) == CONST_INT) && (
#line 34 "../../gcc/config/6502/constraints.md"
(ival == 1));
}
static inline bool
satisfies_constraint_L (rtx op)
{
  HOST_WIDE_INT ival = 0;
  if (CONST_INT_P (op))
    ival = INTVAL (op);
  return (GET_CODE (op) == CONST_INT) && (
#line 39 "../../gcc/config/6502/constraints.md"
(ival == -1));
}
static inline bool
satisfies_constraint_z (rtx op)
{
  HOST_WIDE_INT ival = 0;
  if (CONST_INT_P (op))
    ival = INTVAL (op);
  return (GET_CODE (op) == CONST_INT) && (
#line 44 "../../gcc/config/6502/constraints.md"
(ival == 0));
}
static inline bool
satisfies_constraint_M (rtx op)
{
  HOST_WIDE_INT ival = 0;
  if (CONST_INT_P (op))
    ival = INTVAL (op);
  return (GET_CODE (op) == CONST_INT) && (
#line 49 "../../gcc/config/6502/constraints.md"
(ival >= -255 && ival <= 0));
}
static inline bool
satisfies_constraint_Ns (rtx op)
{
  HOST_WIDE_INT ival = 0;
  if (CONST_INT_P (op))
    ival = INTVAL (op);
  return (GET_CODE (op) == CONST_INT) && (
#line 54 "../../gcc/config/6502/constraints.md"
(ival >= 1 && ival <= 16));
}
static inline bool
satisfies_constraint_Nr (rtx op)
{
  HOST_WIDE_INT ival = 0;
  if (CONST_INT_P (op))
    ival = INTVAL (op);
  return (GET_CODE (op) == CONST_INT) && (
#line 59 "../../gcc/config/6502/constraints.md"
(ival >= 1 && ival <= 16 && ival != 5 && ival != 6));
}
static inline bool
satisfies_constraint_Nc (rtx op)
{
  HOST_WIDE_INT ival = 0;
  if (CONST_INT_P (op))
    ival = INTVAL (op);
  return (GET_CODE (op) == CONST_INT) && (
#line 64 "../../gcc/config/6502/constraints.md"
(ival == 5 || ival == 6));
}
static inline bool
satisfies_constraint_Nl (rtx op)
{
  HOST_WIDE_INT ival = 0;
  if (CONST_INT_P (op))
    ival = INTVAL (op);
  return (GET_CODE (op) == CONST_INT) && (
#line 69 "../../gcc/config/6502/constraints.md"
(ival >= 1 && ival <= 16 && ival != 6));
}
static inline bool
satisfies_constraint_Nx (rtx op)
{
  HOST_WIDE_INT ival = 0;
  if (CONST_INT_P (op))
    ival = INTVAL (op);
  return (GET_CODE (op) == CONST_INT) && (
#line 74 "../../gcc/config/6502/constraints.md"
(ival == 6));
}
static inline bool
satisfies_constraint_Na (rtx op)
{
  HOST_WIDE_INT ival = 0;
  if (CONST_INT_P (op))
    ival = INTVAL (op);
  return (GET_CODE (op) == CONST_INT) && (
#line 79 "../../gcc/config/6502/constraints.md"
(ival >= 1 && ival <= 14));
}
static inline bool
satisfies_constraint_Nb (rtx op)
{
  HOST_WIDE_INT ival = 0;
  if (CONST_INT_P (op))
    ival = INTVAL (op);
  return (GET_CODE (op) == CONST_INT) && (
#line 84 "../../gcc/config/6502/constraints.md"
(ival == 15 || ival == 16));
}
static inline bool
satisfies_constraint_NB (rtx op)
{
  HOST_WIDE_INT ival = 0;
  if (CONST_INT_P (op))
    ival = INTVAL (op);
  return (GET_CODE (op) == CONST_INT) && (
#line 89 "../../gcc/config/6502/constraints.md"
(ival >= 1 && ival <= 8));
}
static inline bool
satisfies_constraint_S (rtx op)
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
    case SUBREG:
      break;
    default:
      return false;
    }
  return 
#line 94 "../../gcc/config/6502/constraints.md"
(GET_CODE (op) != SUBREG
		    || GET_CODE (XEXP (op, 0)) == SYMBOL_REF
		    || GET_CODE (XEXP (op, 0)) == LABEL_REF);
}
static inline bool
satisfies_constraint_Uc (rtx op)
{
  return (GET_CODE (op) == MEM) && (
#line 101 "../../gcc/config/6502/constraints.md"
(CONSTANT_ADDRESS_P (XEXP (op, 0))));
}
static inline bool
satisfies_constraint_Uo (rtx op)
{
  machine_mode mode = GET_MODE (op);
  return (GET_CODE (op) == MEM) && (
#line 106 "../../gcc/config/6502/constraints.md"
(m65x_indirect_offset_addr_p (mode, XEXP (op, 0), true)));
}
static inline bool
satisfies_constraint_Uy (rtx op)
{
  machine_mode mode = GET_MODE (op);
  return (GET_CODE (op) == MEM) && (
#line 111 "../../gcc/config/6502/constraints.md"
(m65x_indirect_indexed_addr_p (mode, XEXP (op, 0), true)));
}
static inline bool
satisfies_constraint_Uj (rtx op)
{
  machine_mode mode = GET_MODE (op);
  return (GET_CODE (op) == MEM) && (
#line 116 "../../gcc/config/6502/constraints.md"
(m65x_absolute_indexed_addr_p (mode, XEXP (op, 0), true)));
}
static inline bool
satisfies_constraint_UX (rtx op)
{
  machine_mode mode = GET_MODE (op);
  return (GET_CODE (op) == MEM) && (
#line 121 "../../gcc/config/6502/constraints.md"
(m65x_absolute_x_addr_p (mode, XEXP (op, 0), true)));
}
static inline bool
satisfies_constraint_UY (rtx op)
{
  machine_mode mode = GET_MODE (op);
  return (GET_CODE (op) == MEM) && (
#line 126 "../../gcc/config/6502/constraints.md"
(m65x_absolute_y_addr_p (mode, XEXP (op, 0), true)));
}
static inline bool
satisfies_constraint_Ur (rtx op)
{
  return (GET_CODE (op) == MEM) && (
#line 131 "../../gcc/config/6502/constraints.md"
(REG_P (XEXP (op, 0))));
}
static inline bool
satisfies_constraint_ZX (rtx op)
{
  machine_mode mode = GET_MODE (op);
  return (GET_CODE (op) == MEM) && (
#line 136 "../../gcc/config/6502/constraints.md"
(MEM_ADDR_SPACE (op) == ADDR_SPACE_ZP
		    && m65x_zeropage_x_addr_p (mode, XEXP (op, 0), true)));
}
static inline bool
satisfies_constraint_ZY (rtx op)
{
  machine_mode mode = GET_MODE (op);
  return (GET_CODE (op) == MEM) && (
#line 142 "../../gcc/config/6502/constraints.md"
(MEM_ADDR_SPACE (op) == ADDR_SPACE_ZP
		    && m65x_zeropage_y_addr_p (mode, XEXP (op, 0), true)));
}
static inline bool
satisfies_constraint_Zj (rtx op)
{
  machine_mode mode = GET_MODE (op);
  return (GET_CODE (op) == MEM) && (
#line 148 "../../gcc/config/6502/constraints.md"
(MEM_ADDR_SPACE (op) == ADDR_SPACE_ZP
		    && m65x_zeropage_indexed_addr_p (mode, XEXP (op, 0),
						     true)));
}
#endif /* tm-constrs.h */
