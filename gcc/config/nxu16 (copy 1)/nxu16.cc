#include "config.h"
#include "system.h"
#include "intl.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "cgraph.h"
#include "c-family/c-common.h"
#include "cfghooks.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "conditions.h"
#include "insn-attr.h"
#include "reload.h"
#include "varasm.h"
#include "calls.h"
#include "stor-layout.h"
#include "output.h"
#include "explow.h"
#include "expr.h"
#include "langhooks.h"
#include "builtins.h"
#include "tree-pass.h"
#include "context.h"
#include "pass_manager.h"
#include "rtl-iter.h"

/* This file should be included last.  */
#include "target-def.h"

#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND nxu16_print_operand
#undef  TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS nxu16_print_operand_address
#undef  TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P
#define TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P  \
  nxu16_addr_space_legitimate_address_p
#undef  TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE nxu16_function_value
#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG         nxu16_function_arg
#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE nxu16_function_arg_advance
//-------------------------------------------------------------------
#define LOSE_AND_RETURN(msgid, x)		\
  do						\
    {						\
      mrmr16_operand_lossage (msgid, x);          \
      return;					\
    } while (0)

/* Emit an error message when we're in an asm, and a fatal error for
   "normal" insns.  Formatted output isn't easily implemented, since we
   use output_operand_lossage to output the actual message and handle the
   categorization of the error.  */
static void
mrmr16_operand_lossage (const char *msgid, rtx op)
{
  debug_rtx (op);
  output_operand_lossage ("%s", msgid);
}

reg_class
nxu16_regno_reg_class (int r)
{
  static const reg_class reg_class_tab[] =
    {
      /* r0 - r15 */
      GENERAL_REGS,GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
      GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
      GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
      GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
/* ea lr sp psw
...*/ 
      ALL_REGS,ALL_REGS, REG_SP,REG_CC
    };

  if (r <= 20)
    return reg_class_tab[r];

  return ALL_REGS;
}

/* Access some RTX as INT_MODE.  If X is a CONST_FIXED we can get
   the bit representation of X by "casting" it to CONST_INT.  */

rtx
nxu16_to_int_mode (rtx x)
{
  machine_mode mode = GET_MODE (x);

  return VOIDmode == mode
    ? x
    : simplify_gen_subreg (int_mode_for_mode (mode).require (), x, mode, 0);
}


/* Worker function for `INITIAL_ELIMINATION_OFFSET'.  */
/* Compute offset between arg_pointer and frame_pointer.  */

poly_int64
nxu16_initial_elimination_offset (int from, int to)
{
  if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return 0;
  else
    {
      int offset = frame_pointer_needed ? 2 : 0;
      int pc_size = 4;

      // If FROM is ARG_POINTER_REGNUM, we are not in an ISR as ISRs
      // might not have arguments.  Hence the following is not affected
      // by gasisr prologues.
      offset += 12;//regs_to_save (NULL);
      return ((get_frame_size () )+ //nxu16_outgoing_args_size()
	      + pc_size + 1 + offset);
    }
}
static const char *
ptrreg_to_str (int regno)
{
  /*switch (regno)
    {
   case REG_X: return "X";
    case REG_Y: return "Y";
    case REG_Z: return "Z";
    default:
      output_operand_lossage ("address operand requires constraint for"
			      " X, Y, or Z register");
    }*/
    static char buf[32]={0};
    int i=30;
    for(;regno&&i;--i,regno/=10){
    buf[i]="0123456789"[regno%10];}
    return &buf[i+1];
  return NULL;
}
static void
nxu16_print_operand_address (FILE *file, machine_mode /*mode*/, rtx x)
{
  switch (GET_CODE (x))
    {
    case REG:
      fprintf (file, "%s", reg_names[REGNO (x)]);
      break;
    default:
      output_addr_const (file, x);
      break;
    }
}
void
nxu16_print_operand (FILE *file, rtx x, int code)
{
  rtx operand = x;

  /* New code entries should just be added to the switch below.  If
     handling is finished, just return.  If handling was just a
     modification of the operand, the modified operand should be put in
     "operand", and then do a break to let default handling
     (zero-modifier) output the operand.  */

  switch (code)
    {
    case 0:
      /* No code, print as usual.  */
      break;

    default:
      //return;
      LOSE_AND_RETURN ("invalid operand modifier letter", x);
    }

  /* Print an operand as without a modifier letter.  */
  switch (GET_CODE (operand))
    {
    case REG:
      if (REGNO (operand) > FRAME_POINTER_REGNUM)
	internal_error ("internal error: bad register: %d", REGNO (operand));
      fprintf (file, "%s", reg_names[REGNO (operand)]);
      return;

    case MEM:
      output_address (GET_MODE (XEXP (operand, 0)), XEXP (operand, 0));
      return;

    default:
      /* No need to handle all strange variants, let output_addr_const
	 do it for us.  */
      if (CONSTANT_P (operand))
	{
	  output_addr_const (file, operand);
	  return;
	}
	//return;
      LOSE_AND_RETURN ("unexpected operand", x);
    }

}



bool
nxu16_legitimate_address_p (enum machine_mode mode, rtx x, bool strict)
{
  /* A big lie!  */
  return true;
}

bool
nxu16_mode_dependent_address_p (rtx x)
{
  /* Another big lie!  */
  return false;
}

static bool
nxu16_addr_space_legitimate_address_p (machine_mode mode, rtx x, bool strict,
				     addr_space_t as, code_helper = ERROR_MARK){
return nxu16_legitimate_address_p (mode, x, strict);				 
}
static rtx
nxu16_function_value (const_tree type, const_tree /*fn_decl_or_type*/,
		    bool /*outgoing*/)
{
  /*if (TYPE_MODE (type) != BLKmode)
    return nxu16_libcall_value (TYPE_MODE (type), NULL_RTX);*/

  unsigned int offs = int_size_in_bytes (type);
  if (offs < 2)
    offs = 2;
  if (offs > 2 && offs < GET_MODE_SIZE (SImode))
    offs = GET_MODE_SIZE (SImode);
  else if (offs > GET_MODE_SIZE (SImode) && offs < GET_MODE_SIZE (DImode))
    offs = GET_MODE_SIZE (DImode);

  return gen_rtx_REG (BLKmode, 0 + 2 - offs);
}

void
nxu16_expand_prologue (void)
{
//TODO:realize
}

void
nxu16_expand_epilogue (void)
{
  //TODO:realize
}

static rtx
nxu16_function_arg (cumulative_args_t cum_v, const function_arg_info &arg)
{
  return NULL_RTX;//another lie
}
static void
nxu16_function_arg_advance (cumulative_args_t cum_v,
                           const function_arg_info &arg)
{
  return;
}


gcc_target targetm = TARGET_INITIALIZER;
