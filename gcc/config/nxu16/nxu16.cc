/* Target Code for nxu16
   Copyright (C) 2008-2021 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"

#include "system.h"

#include "coretypes.h"

#include "backend.h"

#include "target.h"

#include "rtl.h"

#include "tree.h"

#include "stringpool.h"

#include "attribs.h"

#include "df.h"

#include "regs.h"

#include "memmodel.h"

#include "emit-rtl.h"

#include "diagnostic-core.h"

#include "output.h"

#include "stor-layout.h"

#include "varasm.h"

#include "calls.h"

#include "expr.h"

#include "builtins.h"
 //#include "dbxout.h"
#include "ggc.h"

/* This file should be included last.  */
#include "target-def.h"

/* Per-function machine data.  */
struct GTY(()) machine_function {
  /* Number of bytes saved on the stack for callee saved registers.  */
  int callee_saved_reg_size;

  /* Number of bytes saved on the stack for local variables.  */
  int local_vars_size;

  /* The sum of 2 sizes: locals vars and padding byte for saving the
   * registers.  Used in expand_prologue () and expand_epilogue().  */
  int size_for_adjusting_sp;

  /* True if current function is an interrupt function.  */
  bool interrupt_handler_p;

  /* True if attributes on current function have been checked.  */
  bool attributes_checked_p;
  
  bool sibcall_fails;
};

/* Zero initialization is OK for all current fields.  */
static struct machine_function *
  nxu16_init_machine_status(void) {
  machine_function*  init_machine_status=ggc_cleared_alloc < machine_function > ();
  init_machine_status->sibcall_fails=false;
    return init_machine_status;
  }

/* The TARGET_OPTION_OVERRIDE worker.  */
static void
nxu16_option_override(void) {
  /* Set the per-function-data initializer.  */
  init_machine_status = nxu16_init_machine_status;
}

static tree nxu16_handle_type_attribute(tree * , tree, tree, int, bool * );

/* Defining target-specific uses of __attribute__.  */
/* Defining target-specific uses of __attribute__.  */
TARGET_GNU_ATTRIBUTES(nxu16_attribute_table, {
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  {
    "interrupt",0,
    1,
    false,
    true,
    true,
    false,
    nxu16_handle_type_attribute,
    NULL
  }
});

/* Compute the size of the local area and the size to be adjusted by the
 * prologue and epilogue.  */
static void
nxu16_compute_frame(void) {
  /* For aligning the local variables.  */
  int stack_alignment = STACK_BOUNDARY / BITS_PER_UNIT;
  int padding_locals;
  int regno;

  /* Padding needed for each element of the frame.  */
  cfun -> machine -> local_vars_size = get_frame_size();

  /* Align to the stack alignment.  */
  padding_locals = cfun -> machine -> local_vars_size % stack_alignment;
  if (padding_locals)
    padding_locals = stack_alignment - padding_locals;

  cfun -> machine -> local_vars_size += padding_locals;

  cfun -> machine -> callee_saved_reg_size = 0;

  /* Save callee-saved registers.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (df_regs_ever_live_p(regno) && (!call_used_or_fixed_reg_p(regno)))
      cfun -> machine -> callee_saved_reg_size += 1;

  cfun -> machine -> size_for_adjusting_sp =
    crtl -> args.pretend_args_size +
    cfun -> machine -> local_vars_size +
    (ACCUMULATE_OUTGOING_ARGS ?
      (HOST_WIDE_INT) crtl -> outgoing_args_size : 0);//TODO:will work in multi calls with different arg size?
}
static bool
mips_cfun_call_saved_reg_p (unsigned int regno)
{
  /* If the user makes an ordinarily-call-saved register global,
     that register is no longer call-saved.  */
  if (global_regs[regno])
    return false;



  /* call_insns preserve $28 unless they explicitly say otherwise,
     so call_used_regs[] treats $28 as call-saved.  However,
     we want the ABI property rather than the default call_insn
     property here.  */
  return ( !call_used_regs[regno]);
}
void
nxu16_expand_prologue(void) {
  printf("expand_prologue\n"); //todo:leaf function
const char *mips_reg_names[]=REGISTER_NAMES;
    // Iterate over registers
    for (int regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++) {
        // Check if register is live and not call-used or fixed
        if (df_regs_ever_live_p(regno)) {
            // Log the register name
            fprintf(stdout, "PRO~~~~~~~~~~ %s is live , %s for save\n", mips_reg_names[regno],mips_cfun_call_saved_reg_p(regno)?"true":"false");
        }
    }
  int regno;
  rtx insn;

  nxu16_compute_frame();
  if(!crtl->is_leaf){      insn = emit_insn(gen_pushqi1(gen_rtx_REG(QImode, nxu16_LR))); //todo:use correct mode
      RTX_FRAME_RELATED_P(insn) = 1;}
  /*
    insn = emit_insn (gen_movhi_push (hard_frame_pointer_rtx));
    RTX_FRAME_RELATED_P (insn) = 1;*/

  if (flag_stack_usage_info)
    current_function_static_stack_size = cfun -> machine -> size_for_adjusting_sp;

  /* Save callee-saved registers.  */
  for (regno = FIRST_PSEUDO_REGISTER - 1; regno >= 0; regno--) //FIXME: may use er12 r11 er8 like this 
  {
    if (df_regs_ever_live_p(regno) &&
      !call_used_or_fixed_reg_p(regno)) {
      printf("regno:%d live:%s call:%s",regno,df_regs_ever_live_p(regno)? "true" : "false",call_used_or_fixed_reg_p(regno)? "true" : "false");
      insn = emit_insn(gen_pushqi1(gen_rtx_REG(QImode, regno))); //todo:use peephole to combine them
      RTX_FRAME_RELATED_P(insn) = 1;
    }
  }
  if (frame_pointer_needed) {//FIXME:may change sp but not use fp?idk
    insn = emit_move_insn(hard_frame_pointer_rtx, stack_pointer_rtx);
    RTX_FRAME_RELATED_P(insn) = 1;
  }
  if (cfun -> machine -> size_for_adjusting_sp > 0) {
    int i = cfun -> machine -> size_for_adjusting_sp;
    if (i <= 4 * 128) {

      while (i > 128) {
        insn = emit_insn(gen_addhi3(stack_pointer_rtx,
          stack_pointer_rtx,
          GEN_INT(-128)));
        RTX_FRAME_RELATED_P(insn) = 1;
        i -= 128;
      }
      insn = emit_insn(gen_addhi3(stack_pointer_rtx,
        stack_pointer_rtx,
        GEN_INT(-i)));
      RTX_FRAME_RELATED_P(insn) = 1;
    } else {
      rtx reg = gen_rtx_REG(HImode, nxu16_ER12); //FIXME:will have  fp and stack mem use but donot use er12? use TARGET_EXTRA_LIVE_ON_ENTRY bitmap_set_bit (regs, MIPS16_PIC_TEMP_REGNUM);
      
      insn = emit_move_insn(reg, stack_pointer_rtx);
      RTX_FRAME_RELATED_P(insn) = 1;
      insn = emit_insn(gen_addhi3(reg,
        reg,
        GEN_INT(-i)));
      RTX_FRAME_RELATED_P(insn) = 1;
      insn = emit_move_insn(stack_pointer_rtx, reg);
      RTX_FRAME_RELATED_P(insn) = 1;

    }

  }

}
bool
nxu16_epilogue_uses (unsigned int regno)
{
return(reload_completed
	  && cfun->machine
	  && (cfun->machine->interrupt_handler_p && regno==nxu16_LR ));
}
static void
nxu16_asm_out_ctor (rtx symbol, int priority)
{
  fputs ("\t.global __do_global_ctors\n", asm_out_file);
  default_ctor_section_asm_out_constructor (symbol, priority);
}


/* Worker function for `TARGET_ASM_DESTRUCTOR'.  */

static void
nxu16_asm_out_dtor (rtx symbol, int priority)
{
  fputs ("\t.global __do_global_dtors\n", asm_out_file);
  default_dtor_section_asm_out_destructor (symbol, priority);
}

void
nxu16_expand_epilogue(void) {
  printf("expand_epilogue\n"); 

  int regno;
  rtx insn;

  if (frame_pointer_needed) {
    insn = emit_move_insn(stack_pointer_rtx, hard_frame_pointer_rtx);
    RTX_FRAME_RELATED_P(insn) = 1;
  }
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++) {
    if (df_regs_ever_live_p(regno) && !call_used_or_fixed_reg_p(regno)) {
      insn = emit_insn(gen_popqi1(gen_rtx_REG(QImode, regno))); 
      RTX_FRAME_RELATED_P(insn) = 1;
    }
  }

  if (cfun->machine->interrupt_handler_p)
    emit_jump_insn (gen_iret ());
  else
    emit_jump_insn (gen_ret ());
}



static unsigned int
nxu16_hard_regno_nregs(unsigned int regno, machine_mode mode) {
  if (regno == REG_CC && GET_MODE_CLASS(mode) == MODE_CC)
    return 1;
  if (IN_RANGE(regno,nxu16_SP,nxu16_BP) )
    return 1;
  return CEIL(GET_MODE_SIZE(mode), UNITS_PER_WORD);
}

static bool
nxu16_hard_regno_mode_ok(unsigned int regno, machine_mode mode) {
  if (regno == REG_CC)
    return GET_MODE_CLASS(mode) == MODE_CC;
  if (regno == REG_SP)
    return mode == HImode;
  if (regno == nxu16_LR)
    return true;
  if (GET_MODE_SIZE(mode) == 1) //todo:EA
    return true;
  if (GET_MODE_SIZE(mode) >= 2) //todo:ea has a larger boundary
    return !(regno & 1);
  return false;
}

/* Implements the macro INITIAL_ELIMINATION_OFFSET, return the OFFSET.  */

int
nxu16_initial_elimination_offset(int from, int to)
{
  int i = cfun -> machine -> callee_saved_reg_size, regno;
  /*for (regno = FIRST_PSEUDO_REGISTER - 1; regno >= 0; regno--) {
    if (df_regs_ever_live_p(regno) &&
      !call_used_or_fixed_reg_p(regno)) {
      i++;
    }
  }*/
  if (i % 2)
    i++;

  switch (from) {
  case ARG_POINTER_REGNUM:
    if (to == STACK_POINTER_REGNUM) {

      return (get_frame_size() + (ACCUMULATE_OUTGOING_ARGS ?
          (HOST_WIDE_INT) crtl -> outgoing_args_size :
          0) +
        i + 4);
    } else if (to == HARD_FRAME_POINTER_REGNUM) {
      printf("ARG to FP: %d %d\n", i, ACCUMULATE_OUTGOING_ARGS ? (HOST_WIDE_INT) crtl -> outgoing_args_size : 0); 
      return (i + 4 + (ACCUMULATE_OUTGOING_ARGS ?
        (HOST_WIDE_INT) crtl -> outgoing_args_size :
        0));
    }
    break;

  case FRAME_POINTER_REGNUM:
    if (to == STACK_POINTER_REGNUM) {
      return (get_frame_size() +
        0);
    } else if (to == HARD_FRAME_POINTER_REGNUM) {

      return 0;
    }
    break;

  default:
    gcc_unreachable();
  }

  gcc_unreachable();
  return 0;

}



bool
nxu16_function_arg_regno_p (int r)
{
  return IN_RANGE (r, nxu16_R0, nxu16_R3);//TODO:Can i have all regs to pass?try pass in infixed regs
}
void
nxu16_init_cumulative_args (CUMULATIVE_ARGS *cum, tree fntype, rtx libname,
			  tree /*fndecl*/)
{
  cum->nregs =  0;
  for(int i=0;i<FIRST_PSEUDO_REGISTER;i++){
  	cum->regno[i] = nxu16_function_arg_regno_p(i);
  	cum->nregs += cum->regno[i]?1:0;
  	}
  cum->has_stack_args = false;
  if (!libname && stdarg_p (fntype))
    cum->nregs = 0;

  /* Assume the calle may be tail called */

  cfun->machine->sibcall_fails = false;
}


/* Worker function for TARGET_RETURN_IN_MEMORY.  */
static bool
nxu16_return_in_memory(const_tree type, const_tree fntype ATTRIBUTE_UNUSED) //todo: use as many regs as possible
{
  const HOST_WIDE_INT size = int_size_in_bytes(type);
  return (size == -1 || size > 4 * UNITS_PER_WORD);
}

/* Return non-zero if the function argument described by ARG is to be
   passed by reference.  */
static bool
nxu16_pass_by_reference(cumulative_args_t,
  const function_arg_info & arg) {
return targetm.calls.must_pass_in_stack (arg);
}

static bool
nxu16_frame_pointer_required (void)
{
  /* If the function contains dynamic stack allocations, we need to
     use the frame pointer to access the static parts of the frame.  */
  if (cfun->calls_alloca)
    return true;

  /* In MIPS16 mode, we need a frame pointer for a large frame; otherwise,
     reload may be unable to compute the address of a local variable,
     since there is no way to add a large constant to the stack pointer
     without using a second temporary register.  */

  return false;
}
static int
nxu16_num_arg_regs (machine_mode mode, const_tree type)
{
  int size = (mode == BLKmode
	      ? int_size_in_bytes (type)
	      : GET_MODE_SIZE (mode));
  return size ;
}
int nxu16_find_best_reg_index(bool regno[], int bytes) {
    int i, best_index = -1;
    int best_block_size = -1;
    
    
    for (i = 0; i < FIRST_PSEUDO_REGISTER - bytes ; ++i) {
        if (regno[i] == true) {  
 
    int j;
            int block_size = 0;
    if(bytes>1)
      i=i+i%2;

            for (j = i; j < FIRST_PSEUDO_REGISTER; ++j) {
             
 if (regno[ j] != true) { 
                    break;
                }
                block_size++;  
            }
        
            if ((block_size >= bytes)) {
                    if (best_index == -1 || block_size < best_block_size) {
                        best_index = i;
                        best_block_size = block_size;
                }
            }
i+=block_size;
        }
    }
    
    return best_index;
}


/* Return the next register to be used to hold a function argument or
   NULL_RTX if there's no more space.  */
static rtx
nxu16_function_arg(cumulative_args_t cum_v,
  const function_arg_info & arg) {
    CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int bytes = nxu16_num_arg_regs (arg.mode, arg.type);
if(nxu16_find_best_reg_index(cum->regno,bytes)!=-1)
    return gen_rtx_REG (arg.mode, nxu16_find_best_reg_index(cum->regno,bytes));

  cum->has_stack_args = true;

  return NULL_RTX;
}//TODO:support stdarg.h
static void
nxu16_function_arg_advance(cumulative_args_t cum_v,
  const function_arg_info & arg) {//TODO,use not call used regs to pass args but also support sibcall
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int bytes = nxu16_num_arg_regs (arg.mode, arg.type);
int index=nxu16_find_best_reg_index(cum->regno,bytes);
for(int i=0;i<bytes;i++)
cum->regno[index+i]=false;
  cum->nregs -= bytes;
}

int
nxu16_regno_mode_ok_for_base_p (int regno, machine_mode mode,
			       bool strict_p)
{ 
  if (!HARD_REGISTER_NUM_P (regno))
    {
      if (!strict_p)
	return true;
      regno = reg_renumber[regno];
    }

  /* These fake registers will be eliminated to either the stack or
     hard frame pointer, both of which are usually valid base registers.
     Reload deals with the cases where the eliminated form isn't valid.  */
  if (regno == ARG_POINTER_REGNUM || regno == FRAME_POINTER_REGNUM)
    return true;


  return REGNO_REG_CLASS(regno)==GENERAL_REGS;
}


/* Worker function for TARGET_LEGITIMATE_ADDRESS_P.  */
static bool//TODO:reject mov bp to 
nxu16_legitimate_address_p(machine_mode mode ATTRIBUTE_UNUSED,
  rtx x, bool strict_p,
  addr_space_t as, code_helper = ERROR_MARK) {
  gcc_assert(ADDR_SPACE_GENERIC_P(as));
printf("ADDRESS_REG:%d\n",REGNO(x));
  if (GET_CODE(x) == PLUS)
    /* && REG_P (XEXP (x, 0)) */
    /* && nxu16_reg_ok_for_base_p (XEXP (x, 0), strict_p) */
    /* && CONST_INT_P (XEXP (x, 1)) */
    /* && IN_RANGE (INTVAL (XEXP (x, 1)), -32768, 32767)) */
    return true;
  if (REG_P(x) && nxu16_regno_mode_ok_for_base_p (REGNO(x),Pmode, strict_p))
    return true;
  if (GET_CODE(x) == SYMBOL_REF ||
    GET_CODE(x) == LABEL_REF ||
    GET_CODE(x) == CONST)
    return true;
  return false;
}



/* Return the fixed registers used for condition codes.  */
static bool
nxu16_fixed_condition_code_regs(unsigned int * p1, unsigned int * p2) {
  * p1 = REG_CC;
  * p2 = INVALID_REGNUM;
  return true;
}

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its
   FUNCTION_DECL; otherwise, FUNC is 0.
   We always return values in register 0 for nxu16.  */
static rtx
nxu16_function_value(const_tree valtype,
  const_tree fntype_or_decl ATTRIBUTE_UNUSED,
  bool outgoing ATTRIBUTE_UNUSED) {
  return gen_rtx_REG(TYPE_MODE(valtype), nxu16_R0);
}

/* Define how to find the value returned by a library function.
   We always return values in register 0 for nxu16.  */
static rtx
nxu16_libcall_value(machine_mode mode,
  const_rtx fun ATTRIBUTE_UNUSED) {
  return gen_rtx_REG(mode, nxu16_R0);
}//todo:modify this

/* Handle TARGET_FUNCTION_VALUE_REGNO_P.
   We always return values in register 0 for nxu16.  */
static bool
nxu16_function_value_regno_p(const unsigned int regno) {
  return (regno == nxu16_ER0);
}



/* Return true for memory offset addresses between -32768 and 32767.  */
bool
nxu16_offset_address_p(rtx x) {
  x = XEXP(x, 0);

  if (GET_CODE(x) == PLUS) {
    x = XEXP(x, 1);
    if (GET_CODE(x) == CONST_INT) {
      unsigned int v = INTVAL(x) & 0xFFFF8000;
      return (v == 0xFFFF8000 || v == 0x00000000);
    }
  }
  return 0;
}

void
nxu16_split_symbolic_move(rtx dst, rtx src) {
  if ((GET_CODE(dst) == MEM) &&
    ((GET_CODE(XEXP(dst, 0)) == SYMBOL_REF) ||
      (GET_CODE(XEXP(dst, 0)) == CONST))) {
    rtx temp = gen_rtx_REG(HImode, nxu16_ER0);
    emit_move_insn(temp, XEXP(dst, 0));
    emit_move_insn(gen_rtx_MEM(HImode, temp), src);
  }

  if ((GET_CODE(src) == MEM) &&
    ((GET_CODE(XEXP(src, 0)) == SYMBOL_REF) ||
      (GET_CODE(XEXP(src, 0)) == CONST))) {
    emit_move_insn(dst, XEXP(src, 0));
    emit_move_insn(dst, gen_rtx_MEM(HImode, dst));
  }
}

/* Verify type based attributes.  NODE is the what the attribute is being
   applied to.  NAME is the attribute name.  ARGS are the attribute args.
   FLAGS gives info about the context.  NO_ADD_ATTRS should be set to true if
   the attribute should be ignored.  */
static tree
nxu16_handle_type_attribute(tree * node ATTRIBUTE_UNUSED, tree name, tree args,
  int flags ATTRIBUTE_UNUSED, bool * no_add_attrs) {
  return NULL_TREE;
}

/* Return true if function TYPE is an interrupt function.  */
static bool
nxu16_interrupt_type_p(tree type) {
  return lookup_attribute("interrupt", TYPE_ATTRIBUTES(type)) != NULL;
}

/* Implement `TARGET_SET_CURRENT_FUNCTION'.  */
/* Sanity checking for function attributes.  */
static void
nxu16_set_current_function(tree decl) {
  if (decl == NULL_TREE ||
    current_function_decl == NULL_TREE ||
    current_function_decl == error_mark_node ||
    !cfun -> machine ||
    cfun -> machine -> attributes_checked_p)
    return;

  cfun -> machine -> interrupt_handler_p = nxu16_interrupt_type_p(TREE_TYPE(decl));

  if (cfun -> machine -> interrupt_handler_p) {
    tree ret = TREE_TYPE(TREE_TYPE(decl));
    tree args = TYPE_ARG_TYPES(TREE_TYPE(decl));

    if (TREE_CODE(ret) != VOID_TYPE)
      error("%qs function cannot return a value", "interrupt");

    if (args && TREE_CODE(TREE_VALUE(args)) != VOID_TYPE)
      error("%qs function cannot have arguments", "interrupt");
  }

  /* Don't print the above diagnostics more than once.  */
  cfun -> machine -> attributes_checked_p = 1;
}

#define LOSE_AND_RETURN(msgid, x) \
do{ \
  nxu16_operand_lossage(msgid, x);\
  return;\
} while (0)

/* Emit an error message when we're in an asm, and a fatal error for
   "normal" insns.  Formatted output isn't easily implemented, since we
   use output_operand_lossage to output the actual message and handle the
   categorization of the error.  */
static void
nxu16_operand_lossage(const char * msgid, rtx op) {
  debug_rtx(op);
  output_operand_lossage("%s", msgid);
}

/* The PRINT_OPERAND_ADDRESS worker.  */
static void
nxu16_print_operand_address(FILE * file, machine_mode mode, rtx x) {
  printf(" PRINT_OPERAND_ADDRESS\n");

  switch (GET_CODE(x)) {
  case REG:
    fprintf(file, "[e%s]", reg_names[REGNO(x)]);
    break;

  case PLUS:
    if (GET_CODE(XEXP(x, 0)) == REG && CONST_INT_P(XEXP(x, 1))) {
      fprintf(file, HOST_WIDE_INT_PRINT_DEC, INTVAL(XEXP(x, 1)));
      fprintf(file, "[e%s]", reg_names[REGNO(XEXP(x, 0))]);

    } else {
      output_addr_const(file, x);
    }
    break;

  default:
    output_addr_const(file, x);
    break;
  }
}

/* The PRINT_OPERAND worker.  */
static void
nxu16_print_operand(FILE * file, rtx x, int code) {
  printf("PRINT_OPERAND\n");
  rtx operand = x;

  /* New code entries should just be added to the switch below.  If
     handling is finished, just return.  If handling was just a
     modification of the operand, the modified operand should be put in
     "operand", and then do a break to let default handling
     (zero-modifier) output the operand.  */

  switch (code) {
  case 0:
    /* No code, print as usual.  */
    break;

  default:
    LOSE_AND_RETURN("invalid operand modifier letter", x);
  }

  /* Print an operand as without a modifier letter.  */
  switch (GET_CODE(operand)) {

  case REG:
    if (REGNO(operand) >= FIRST_PSEUDO_REGISTER)
      internal_error("internal error: bad register: %d", REGNO(operand));
    fprintf(file, "%s%s", (GET_MODE(operand) == HImode && REGNO(operand) < nxu16_SP) ? "e" : "", reg_names[REGNO(operand)]);
    return;

  case MEM:
    output_address(GET_MODE(XEXP(operand, 0)), XEXP(operand, 0));
    return;

  default:
    /* No need to handle all strange variants, let output_addr_const
	 do it for us.  */
    if (CONSTANT_P(operand)) {
      output_addr_const(file, operand);
      return;
    }

    LOSE_AND_RETURN("unexpected operand", x);
  }
}


/* The Global `targetm' Variable.  */

/* Initialize the GCC target structure.  */
static bool
nxu16_use_lra_p ()
{
  return true;
}
#undef TARGET_LRA_P
#define TARGET_LRA_P nxu16_use_lra_p

/******************************************************************************
 * Run-time Target
 ******************************************************************************/

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE nxu16_option_override

/******************************************************************************
 * Storage Layout
 ******************************************************************************/
/*
#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT constant_alignment_word_strings*///TODO:realize it

/******************************************************************************
 * Stack and Calling
 ******************************************************************************/


/* Elimination */




/* Register Arguments */

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG nxu16_function_arg

/*#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE nxu16_pass_by_reference*/
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE nxu16_function_arg_advance

/* Scalar Return */

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE nxu16_function_value
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE nxu16_libcall_value
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P nxu16_function_value_regno_p

/* Aggregate Return */

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY nxu16_return_in_memory



/******************************************************************************
 * Addressing Modes
 ******************************************************************************/

/* #undef  TARGET_LEGITIMATE_ADDRESS_P */
/* #define TARGET_LEGITIMATE_ADDRESS_P nxu16_legitimate_address_p */
#undef TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P
#define TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P nxu16_legitimate_address_p

/******************************************************************************
 * Condition Codes
 ******************************************************************************/

#undef TARGET_FIXED_CONDITION_CODE_REGS
#define TARGET_FIXED_CONDITION_CODE_REGS nxu16_fixed_condition_code_regs

/******************************************************************************
 * Assembler Format
 ******************************************************************************/

#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND nxu16_print_operand
#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS nxu16_print_operand_address

/* These are for printing type pseudo-ops, e.g. '.byte', '.short' etc. */
/* #undef TARGET_ASM_BYTE_OP */
/* #define TARGET_ASM_BYTE_OP "\t" */
/* #undef TARGET_ASM_ALIGNED_HI_OP */
/* #define TARGET_ASM_ALIGNED_HI_OP "\t" */
/* #undef TARGET_ASM_ALIGNED_SI_OP */
/* #define TARGET_ASM_ALIGNED_SI_OP "\t" */
/* #undef TARGET_ASM_ALIGNED_DI_OP */
/* #define TARGET_ASM_ALIGNED_DI_OP "\t" */

/******************************************************************************
 * Sections
 ******************************************************************************/

#undef TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION default_elf_select_section

/******************************************************************************
 * Target Attributes
 ******************************************************************************/

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE nxu16_attribute_table

/******************************************************************************
 * Misc.
 ******************************************************************************/

#undef TARGET_SET_CURRENT_FUNCTION
#define TARGET_SET_CURRENT_FUNCTION nxu16_set_current_function

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS nxu16_hard_regno_nregs
#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK nxu16_hard_regno_mode_ok

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-nxu16.h"
