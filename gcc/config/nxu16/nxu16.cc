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
  flag_function_sections = 1;
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

  /* The prologue saves one contiguous register group selected by the
     lowest live call-saved byte register.  Account for every byte that the
     grouped PUSH writes, including otherwise-unused registers in the group.
     The argument-pointer elimination offset must describe the physical
     frame, not merely the number of live registers.  */
  int lowest_saved_reg = 16;
  for (regno = 15; regno >= 4; --regno)
    if (df_regs_ever_live_p (regno) && !call_used_or_fixed_reg_p (regno))
      lowest_saved_reg = regno;
  /* The hard frame pointer is introduced during prologue expansion, after
     df_regs_ever_live_p has been computed.  Preserve its ER pair explicitly
     whenever it is needed.  */
  if (frame_pointer_needed && lowest_saved_reg > HARD_FRAME_POINTER_REGNUM)
    lowest_saved_reg = HARD_FRAME_POINTER_REGNUM;
  switch (lowest_saved_reg)
    {
    case 4: case 5:   cfun->machine->callee_saved_reg_size = 12; break;
    case 6: case 7:   cfun->machine->callee_saved_reg_size = 10; break;
    case 8: case 9:   cfun->machine->callee_saved_reg_size = 8; break;
    case 10: case 11: cfun->machine->callee_saved_reg_size = 6; break;
    case 12: case 13: cfun->machine->callee_saved_reg_size = 4; break;
    case 14: case 15: cfun->machine->callee_saved_reg_size = 2; break;
    default: break;
    }

  cfun -> machine -> size_for_adjusting_sp =
    crtl -> args.pretend_args_size +
    cfun -> machine -> local_vars_size +
    (ACCUMULATE_OUTGOING_ARGS ?
      (HOST_WIDE_INT) crtl -> outgoing_args_size : 0);//TODO:will work in multi calls with different arg size?
}
void
nxu16_expand_prologue(void) {
  int regno;
  rtx insn;
  rtx physical_sp = gen_rtx_REG (HImode, nxu16_SP);

  nxu16_compute_frame();
  if(!crtl->is_leaf){      insn = emit_insn(gen_pushsi1(gen_rtx_REG(SImode, nxu16_LR))); 
      RTX_FRAME_RELATED_P(insn) = 1;}
  /*
    insn = emit_insn (gen_movhi_push (hard_frame_pointer_rtx));
    RTX_FRAME_RELATED_P (insn) = 1;*/

  if (flag_stack_usage_info)
    current_function_static_stack_size = cfun -> machine -> size_for_adjusting_sp;
/* Save callee-saved registers - hardcoded version */
int lowest_reg = 16;
for (regno = FIRST_PSEUDO_REGISTER - 1; regno >= 0; regno--) {
    if (df_regs_ever_live_p(regno) && !call_used_or_fixed_reg_p(regno)) {
        lowest_reg = regno;
    }
}///FIXME: may use er12 r11 er8 like this ,also,may use non general reg
if (frame_pointer_needed && lowest_reg > HARD_FRAME_POINTER_REGNUM)
  lowest_reg = HARD_FRAME_POINTER_REGNUM;

if (lowest_reg < 16) {
    switch (lowest_reg) {

            
        case 4: case 5:
                    /* Push DI for r8-r15 if needed */
                insn = emit_insn(gen_pushdi1(gen_rtx_REG(DImode, 8)));
                RTX_FRAME_RELATED_P(insn) = 1;
            /* Push SI for r4-r7 */
            insn = emit_insn(gen_pushsi1(gen_rtx_REG(SImode, 4)));
            RTX_FRAME_RELATED_P(insn) = 1;

            
            break;
            
        case 6: case 7:
                    /* Push DI for r8-r15 if needed */
                insn = emit_insn(gen_pushdi1(gen_rtx_REG(DImode, 8)));
                RTX_FRAME_RELATED_P(insn) = 1;
            /* Push HI for r6-r7 */
            insn = emit_insn(gen_pushhi1(gen_rtx_REG(HImode, 6)));
            RTX_FRAME_RELATED_P(insn) = 1;

            
            break;
            
        case 8: case 9:
            /* Push DI for r8-r15 */
            insn = emit_insn(gen_pushdi1(gen_rtx_REG(DImode, 8)));
            RTX_FRAME_RELATED_P(insn) = 1;
            break;
            
        case 10: case 11:
                    /* Push SI for r12-r15 */
            insn = emit_insn(gen_pushsi1(gen_rtx_REG(SImode, 12)));
            RTX_FRAME_RELATED_P(insn) = 1;
            /* Push HI for r10-r11 */
            insn = emit_insn(gen_pushhi1(gen_rtx_REG(HImode, 10)));
            RTX_FRAME_RELATED_P(insn) = 1;

            break;
            
        case 12: case 13:
            /* Push SI for r12-r15 */
            insn = emit_insn(gen_pushsi1(gen_rtx_REG(SImode, 12)));
            RTX_FRAME_RELATED_P(insn) = 1;
            break;
            
        case 14: case 15:
            /* Push HI for r14-r15 */
            insn = emit_insn(gen_pushhi1(gen_rtx_REG(HImode, 14)));
            RTX_FRAME_RELATED_P(insn) = 1;
            break;
            
         default:
         gcc_unreachable();
         break;
    }
}
  if (frame_pointer_needed) {//FIXME:may change sp but not use fp?idk
    rtx physical_fp = gen_rtx_REG (HImode, HARD_FRAME_POINTER_REGNUM);
    insn = emit_move_insn (physical_fp, physical_sp);
    RTX_FRAME_RELATED_P(insn) = 1;
  }
  if (cfun -> machine -> size_for_adjusting_sp > 0) {
    int i = cfun -> machine -> size_for_adjusting_sp;
    if (i <= 4 * 128) {

      while (i > 128) {
        insn = emit_insn(gen_addhi3(physical_sp,
          physical_sp,
          GEN_INT(-128)));
        RTX_FRAME_RELATED_P(insn) = 1;
        i -= 128;
      }
      insn = emit_insn(gen_addhi3(physical_sp,
        physical_sp,
        GEN_INT(-i)));
      RTX_FRAME_RELATED_P(insn) = 1;
    } else {
      rtx reg = gen_rtx_REG(HImode, nxu16_ER12); //FIXME:will have  fp and stack mem use but donot use er12? use TARGET_EXTRA_LIVE_ON_ENTRY bitmap_set_bit (regs, MIPS16_PIC_TEMP_REGNUM);
      
      insn = emit_move_insn(reg, physical_sp);
      RTX_FRAME_RELATED_P(insn) = 1;
      insn = emit_insn(gen_addhi3(reg,
        reg,
        GEN_INT(-i)));
      RTX_FRAME_RELATED_P(insn) = 1;
      insn = emit_move_insn(physical_sp, reg);
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
nxu16_expand_epilogue (bool sibcall_p) {
  int regno;
  rtx insn;
  rtx physical_sp = gen_rtx_REG (HImode, nxu16_SP);

  if (frame_pointer_needed) {
    rtx physical_fp = gen_rtx_REG (HImode, HARD_FRAME_POINTER_REGNUM);
    insn = emit_move_insn(physical_sp, physical_fp);
    RTX_FRAME_RELATED_P(insn) = 1;
  }
int lowest_reg = 16;
for (regno = FIRST_PSEUDO_REGISTER-1; regno >=0; regno--) {
    if (df_regs_ever_live_p(regno) && !call_used_or_fixed_reg_p(regno)) {
        lowest_reg = regno;
    }
}
if (frame_pointer_needed && lowest_reg > HARD_FRAME_POINTER_REGNUM)
  lowest_reg = HARD_FRAME_POINTER_REGNUM;

if (lowest_reg <16) {
    switch (lowest_reg) {

            
        case 4: case 5:
                    /* Pop SI for r4-r7 */
            insn = emit_insn(gen_popsi1(gen_rtx_REG(SImode, 4)));
            RTX_FRAME_RELATED_P(insn) = 1;
            /* Pop DI for r8-r15 if needed */

                insn = emit_insn(gen_popdi1(gen_rtx_REG(DImode, 8)));
                RTX_FRAME_RELATED_P(insn) = 1;
            

            break;
            
        case 6: case 7:
                    /* Pop HI for r6-r7 */
            insn = emit_insn(gen_pophi1(gen_rtx_REG(HImode, 6)));
            RTX_FRAME_RELATED_P(insn) = 1;
            /* Pop DI for r8-r15 if needed */
                insn = emit_insn(gen_popdi1(gen_rtx_REG(DImode, 8)));
                RTX_FRAME_RELATED_P(insn) = 1;
            

            break;
            
        case 8: case 9:
            /* Pop DI for r8-r15 */
            insn = emit_insn(gen_popdi1(gen_rtx_REG(DImode, 8)));
            RTX_FRAME_RELATED_P(insn) = 1;
            break;
            
        case 10: case 11:
                    /* Pop HI for r10-r11 */
            insn = emit_insn(gen_pophi1(gen_rtx_REG(HImode, 10)));
            RTX_FRAME_RELATED_P(insn) = 1;
            /* Pop SI for r12-r15 */
            insn = emit_insn(gen_popsi1(gen_rtx_REG(SImode, 12)));
            RTX_FRAME_RELATED_P(insn) = 1;

            break;
            
        case 12: case 13:
            /* Pop SI for r12-r15 */
            insn = emit_insn(gen_popsi1(gen_rtx_REG(SImode, 12)));
            RTX_FRAME_RELATED_P(insn) = 1;
            break;
            
        case 14: case 15:
            /* Pop HI for r14-r15 */
            insn = emit_insn(gen_pophi1(gen_rtx_REG(HImode, 14)));
            RTX_FRAME_RELATED_P(insn) = 1;
            break;
           default:
           gcc_unreachable();
    }
}

  /* A normal non-leaf return consumes the four-byte LR:LCSR record with
     POP PC.  A sibling call has no return instruction of its own, so restore
     that record explicitly before jumping to the sibling.  */
  if (sibcall_p && !crtl->is_leaf)
    {
      insn = emit_insn (gen_popsi1 (gen_rtx_REG (SImode, nxu16_LR)));
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (!sibcall_p)
    {
      if (cfun->machine->interrupt_handler_p)
	emit_jump_insn (gen_iret ());
      else
	emit_jump_insn (gen_ret ());
    }
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
  unsigned int size = GET_MODE_SIZE(mode);

  if (regno == REG_CC)
    return GET_MODE_CLASS(mode) == MODE_CC;
  if (regno == REG_SP)
    return mode == HImode || mode == SImode;
  if (regno == nxu16_LR)
    return true;
  if (regno < nxu16_SP) {
    if (size == 1)
      return true;

    /* Multi-byte values use consecutive byte registers.  Reject starts
       which would run through r15 into the fixed SP/PSW register numbers;
       advertising those as allocatable makes LRA retry forever.  */
    return !(regno & 1) && regno + size <= nxu16_SP;
  }
  return false;
}

/* Implements the macro INITIAL_ELIMINATION_OFFSET, return the OFFSET.  */
static bool
nxu16_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  /* Prefer SP for frameless functions.  When a stable frame is required,
     LRA must retain the hard-frame-pointer route; allowing SP elimination in
     that case makes incoming stack arguments move with the local frame.  */
  return (to == HARD_FRAME_POINTER_REGNUM
	  || (to == STACK_POINTER_REGNUM && !frame_pointer_needed));
}
int
nxu16_initial_elimination_offset(int from, int to)
{
  /* Elimination is computed before prologue expansion, so do not depend on
     a previous caller having populated the per-function frame data.  */
  nxu16_compute_frame ();
  int i = cfun -> machine -> callee_saved_reg_size;
  /*for (regno = FIRST_PSEUDO_REGISTER - 1; regno >= 0; regno--) {
    if (df_regs_ever_live_p(regno) &&
      !call_used_or_fixed_reg_p(regno)) {
      i++;
    }
  }*/
  if (i % 2)
    i++;
if(!crtl->is_leaf)
i+=4;
  switch (from) {
  case ARG_POINTER_REGNUM:
    if (to == STACK_POINTER_REGNUM) {

      return (get_frame_size() + (ACCUMULATE_OUTGOING_ARGS ?
          (HOST_WIDE_INT) crtl -> outgoing_args_size :
          0) +
        i );
    } else if (to == HARD_FRAME_POINTER_REGNUM) {
      /* The hard frame pointer is established immediately after register
         saves.  Outgoing arguments and locals are allocated below it, so
         they must not move incoming arguments relative to this base.  */
      return i;
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
nxu16_frame_pointer_required(void)
{
    nxu16_compute_frame();  // Ensure frame info is computed first
    
    if (cfun->calls_alloca) {
        return true;
    }

    if (get_frame_size() > 0) {
        return true;
    }
    
    if (cfun->machine->size_for_adjusting_sp > 0) {
        return true;
    }
    
    return false;
}

/* The ISA has both direct and register-indirect branches, so a sibling call
   does not need a link register.  The generic call machinery has already
   checked argument compatibility before consulting this hook.  An interrupt
   handler is the exception: it must retain its IRET epilogue.  */
static bool
nxu16_function_ok_for_sibcall (tree decl, tree exp)
{
  if (cfun == NULL
      || cfun->machine == NULL
      || cfun->machine->interrupt_handler_p)
    return false;

  /* A symbolic target does not consume an argument register.  An indirect
     sibling target must occupy the whole call-clobbered XR0 slot, so it is
     currently safe only when there are no outgoing register arguments.  */
  return (decl != NULL_TREE
	  || (exp != NULL_TREE
	      && TREE_CODE (exp) == CALL_EXPR
	      && call_expr_nargs (exp) == 0));
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
  int index = nxu16_find_best_reg_index (cum->regno, bytes);
  if (index < 0)
    {
      cum->has_stack_args = true;
      return;
    }
  for (int i = 0; i < bytes; i++)
    cum->regno[index + i] = false;
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


  if (regno == STACK_POINTER_REGNUM)
    return true;
  return (REGNO_REG_CLASS(regno)==GENERAL_REGS
          && nxu16_hard_regno_mode_ok (regno, mode));
}

static bool//TODO:reject mov bp to 
nxu16_legitimate_address_p(machine_mode mode ATTRIBUTE_UNUSED,
  rtx x, bool strict_p,
  code_helper = ERROR_MARK) {
  if (REG_P(x)
      && (GET_MODE (x) == HImode || GET_MODE (x) == SImode)
      && nxu16_regno_mode_ok_for_base_p (REGNO(x), GET_MODE (x),
                                                   strict_p))
    return true;
  if (GET_CODE (x) == PLUS
      && REG_P (XEXP (x, 0))
      && (GET_MODE (XEXP (x, 0)) == HImode
          || GET_MODE (XEXP (x, 0)) == SImode)
      && nxu16_regno_mode_ok_for_base_p (REGNO (XEXP (x, 0)),
                                         GET_MODE (XEXP (x, 0)),
					 strict_p)
      && CONST_INT_P (XEXP (x, 1))
      && IN_RANGE (INTVAL (XEXP (x, 1)), -32768, 32767))
    return true;
  return false;
}

/* Data pointers use the generic 16-bit RAM address space.  Function types
   live in the far address space so their pointers retain the full 24-bit
   code address in a naturally aligned 32-bit slot.  */
static void
nxu16_set_default_type_attributes (tree type)
{
  if (FUNC_OR_METHOD_TYPE_P (type))
    TYPE_ADDR_SPACE (type) = ADDR_SPACE_FAR;
}
/* Worker function for TARGET_LEGITIMATE_ADDRESS_P.  */
static bool//TODO:reject mov bp to 
nxu16_legitimate_addr_space_address_p(machine_mode mode ATTRIBUTE_UNUSED,
  rtx x, bool strict_p,
  addr_space_t as, code_helper = ERROR_MARK) {
  if (as == ADDR_SPACE_FAR)
    return nxu16_legitimate_address_p (mode, x, strict_p);

  gcc_assert (as == ADDR_SPACE_NEAR);
  if (GET_CODE (x) == PLUS)
    return (REG_P (XEXP (x, 0)) && CONST_INT_P (XEXP (x, 1)));
  if (REG_P (x))
    return nxu16_regno_mode_ok_for_base_p (REGNO (x), HImode, strict_p);
  return (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == LABEL_REF
          || GET_CODE (x) == CONST);
}

static scalar_int_mode
nxu16_addr_space_pointer_mode (addr_space_t addrspace)
{
  switch (addrspace)
    {
    case ADDR_SPACE_FAR:
      return SImode;
    case ADDR_SPACE_NEAR:
      return HImode;
    default:
      gcc_unreachable ();
    }
}

static bool
nxu16_addr_space_subset_p (addr_space_t subset, addr_space_t superset)
{
  return subset == superset
    || (subset == ADDR_SPACE_NEAR && superset == ADDR_SPACE_FAR);
}

static rtx
nxu16_addr_space_convert (rtx op, tree from_type, tree to_type)
{
  addr_space_t from_as = TYPE_ADDR_SPACE (TREE_TYPE (from_type));
  addr_space_t to_as = TYPE_ADDR_SPACE (TREE_TYPE (to_type));

  if (from_as == ADDR_SPACE_NEAR && to_as == ADDR_SPACE_FAR)
    {
      rtx result = gen_reg_rtx (SImode);
      emit_insn (gen_zero_extendhisi2 (result, op));
      return result;
    }
  if (from_as == ADDR_SPACE_FAR && to_as == ADDR_SPACE_NEAR)
    {
      rtx result = gen_reg_rtx (HImode);
      emit_move_insn (result, gen_lowpart (HImode, op));
      return result;
    }
  gcc_unreachable ();
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
nxu16_operand_lossage(const char * msgid, rtx op ATTRIBUTE_UNUSED) {
  output_operand_lossage("%s", msgid);
}

/* The PRINT_OPERAND_ADDRESS worker.  */
static void
nxu16_print_operand_address(FILE * file, machine_mode mode ATTRIBUTE_UNUSED,
			    rtx x) {
  switch (GET_CODE(x)) {
  case REG:
    if (GET_MODE (x) == SImode && REGNO (x) < nxu16_SP)
      fprintf (file, "r%d:[er%d]", REGNO (x) + 2, REGNO (x));
    else
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
  rtx operand = x;

  /* New code entries should just be added to the switch below.  If
     handling is finished, just return.  If handling was just a
     modification of the operand, the modified operand should be put in
     "operand", and then do a break to let default handling
     (zero-modifier) output the operand.  */
  int abcd = 0;

  if (code == 'J')
    {
      const char *suffix;
      switch (GET_CODE (x))
	{
	case NE:  suffix = "ne";  break;
	case EQ:  suffix = "eq";  break;
	case LT:  suffix = "lts"; break;
	case LTU: suffix = "lt";  break;
	case GT:  suffix = "gts"; break;
	case GTU: suffix = "gt";  break;
	case GE:  suffix = "ges"; break;
	case LE:  suffix = "les"; break;
	case GEU: suffix = "ge";  break;
	case LEU: suffix = "le";  break;
	default:
	  LOSE_AND_RETURN ("invalid branch condition", x);
	}
      fputs (suffix, file);
      return;
    }

  if (code == 'S')
    {
      rtx addr = MEM_P (x) ? XEXP (x, 0) : x;
      if (GET_CODE (addr) == PLUS)
	addr = XEXP (addr, 0);
      if (!REG_P (addr) || REGNO (addr) + 2 >= nxu16_SP)
	LOSE_AND_RETURN ("far address has no segment register", x);
      fprintf (file, "r%d", REGNO (addr) + 2);
      return;
    }

  if (code >= 'A' && code <= 'D')
    abcd = code - 'A' + 1;

  /* Print an operand as without a modifier letter.  */
  switch (GET_CODE(operand)) {

  case REG:
    if (REGNO(operand) >= FIRST_PSEUDO_REGISTER)
      internal_error("internal error: bad register: %d", REGNO(operand));
    if (abcd)
      fprintf(file, "%s", reg_names[REGNO(operand) + abcd - 1]);
    else
      fprintf(file, "%s%s",
	      ((REGNO_REG_CLASS(REGNO(operand)) == GENERAL_REGS)
	       ? ((GET_MODE(operand) == HImode) ? "e"
		  : (GET_MODE(operand) == SImode) ? "x"
		  : (GET_MODE(operand) == DImode) ? "q" : "")
	       : ""),
	      reg_names[REGNO(operand)]);
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

/* QI/HI memory operations use a register-selected DSR prefix for generic
   (far) addresses.  The compact Rn:[ERm] spelling covers zero displacement;
   displacement forms use a separate prefix in the same asm template.  */
const char *
nxu16_output_move (rtx *operands)
{
  if (MEM_P (operands[1]))
    {
      rtx addr = XEXP (operands[1], 0);
      if (GET_MODE (addr) == SImode && GET_CODE (addr) == PLUS)
	return "dsr %S1\n\tl %0,%1";
      return "l %0,%1";
    }
  if (MEM_P (operands[0]))
    {
      rtx addr = XEXP (operands[0], 0);
      if (GET_MODE (addr) == SImode && GET_CODE (addr) == PLUS)
	return "dsr %S0\n\tst %1,%0";
      return "st %1,%0";
    }
  return "mov %0,%1";
}

const char *
nxu16_output_movpsi (rtx *operands)
{
  rtx dst = operands[0];
  rtx src = operands[1];
  rtx pair[2];

  if (REG_P (dst) && REGNO (dst) == nxu16_SP)
    {
      pair[0] = gen_rtx_REG (HImode, nxu16_SP);
      pair[1] = simplify_gen_subreg (HImode, src, PSImode, 0);
      output_asm_insn ("mov %0,%1", pair);
      return "";
    }
  if (REG_P (src) && REGNO (src) == nxu16_SP)
    {
      pair[0] = simplify_gen_subreg (HImode, dst, PSImode, 0);
      pair[1] = gen_rtx_REG (HImode, nxu16_SP);
      output_asm_insn ("mov %0,%1", pair);
      pair[0] = simplify_gen_subreg (QImode, dst, PSImode, 2);
      pair[1] = const0_rtx;
      output_asm_insn ("mov %0,%1", pair);
      return "";
    }

  if (REG_P (dst) && CONSTANT_P (src))
    {
      pair[0] = simplify_gen_subreg (QImode, dst, PSImode, 0);
      pair[1] = src;
      output_asm_insn ("mov %0,lo8(%1)", pair);
      pair[0] = simplify_gen_subreg (QImode, dst, PSImode, 1);
      output_asm_insn ("mov %0,hi8(%1)", pair);
      pair[0] = simplify_gen_subreg (QImode, dst, PSImode, 2);
      output_asm_insn ("mov %0,hlo8(%1)", pair);
      return "";
    }

  if ((REG_P (dst) && REG_P (src))
      || (REG_P (dst) && MEM_P (src))
      || (MEM_P (dst) && REG_P (src)))
    {
      bool backwards = (REG_P (dst) && REG_P (src)
			&& REGNO (dst) > REGNO (src));
      int offsets[2] = { backwards ? 2 : 0, backwards ? 0 : 2 };
      for (int i = 0; i < 2; ++i)
	{
	  int offset = offsets[i];
	  machine_mode part_mode = offset == 0 ? HImode : QImode;
	  pair[0] = REG_P (dst)
	    ? simplify_gen_subreg (part_mode, dst, PSImode, offset)
	    : adjust_address_nv (dst, part_mode, offset);
	  pair[1] = REG_P (src)
	    ? simplify_gen_subreg (part_mode, src, PSImode, offset)
	    : adjust_address_nv (src, part_mode, offset);
	  output_asm_insn (nxu16_output_move (pair), pair);
	}
      return "";
    }
  gcc_unreachable ();
}

/* Repair the rare case where reload assigns a dying near or far address over
   the destination of the multi-instruction SI load.  */
static bool
nxu16_output_overlapping_movsi_load (rtx dst, rtx src)
{
  if (!REG_P (dst) || !MEM_P (src))
    return false;

  rtx addr = XEXP (src, 0);
  HOST_WIDE_INT displacement = 0;
  if (GET_CODE (addr) == PLUS)
    {
      gcc_assert (REG_P (XEXP (addr, 0)) && CONST_INT_P (XEXP (addr, 1)));
      displacement = INTVAL (XEXP (addr, 1));
      addr = XEXP (addr, 0);
    }
  if (!REG_P (addr))
    return false;
  unsigned int addr_nregs = GET_MODE_SIZE (GET_MODE (addr));
  gcc_assert (GET_MODE (addr) == HImode || GET_MODE (addr) == SImode);
  if (REGNO (dst) >= REGNO (addr) + addr_nregs
      || REGNO (addr) >= REGNO (dst) + GET_MODE_SIZE (SImode))
    return false;
  gcc_assert (REGNO (addr) < nxu16_SP);

  /* Borrow a disjoint SI group and preserve its live value on the stack.
     The overlapping source and destination ranges have a union of at most
     six registers, so an even-start four-register group is always free.  */
  unsigned int scratch_regno = nxu16_SP;
  unsigned int dst_first = REGNO (dst);
  unsigned int addr_first = REGNO (addr);
  for (unsigned int candidate = 0; candidate + 4 <= nxu16_SP;
       candidate += 2)
    if ((candidate + 4 <= dst_first || dst_first + 4 <= candidate)
	&& (candidate + 4 <= addr_first
	    || addr_first + addr_nregs <= candidate))
      {
	scratch_regno = candidate;
	break;
      }
  gcc_assert (scratch_regno < nxu16_SP);

  rtx scratch = gen_rtx_REG (SImode, scratch_regno);
  rtx out[4];
  if ((scratch_regno & 3) == 0)
    {
      out[0] = scratch;
      output_asm_insn ("push x%A0", out);
    }
  else
    {
      out[0] = simplify_gen_subreg (HImode, scratch, SImode, 2);
      output_asm_insn ("push %0", out);
      out[0] = simplify_gen_subreg (HImode, scratch, SImode, 0);
      output_asm_insn ("push %0", out);
    }

  if (GET_MODE (addr) == SImode)
    {
      for (int offset = 0; offset <= 2; offset += 2)
	{
	  out[0] = simplify_gen_subreg (HImode, scratch, SImode, offset);
	  out[1] = simplify_gen_subreg (HImode, addr, SImode, offset);
	  output_asm_insn ("mov %0,%1", out);
	}
    }
  else
    {
      out[0] = simplify_gen_subreg (HImode, scratch, SImode, 0);
      out[1] = addr;
      output_asm_insn ("mov %0,%1", out);
    }
  for (int offset = 2; offset >= 0; offset -= 2)
    {
      out[0] = simplify_gen_subreg (HImode, dst, SImode, offset);
      out[1] = simplify_gen_subreg (HImode, scratch, SImode, 0);
      if (GET_MODE (addr) == SImode)
	{
	  out[2] = simplify_gen_subreg (QImode, scratch, SImode, 2);
	  out[3] = GEN_INT (displacement + offset);
	  output_asm_insn ("dsr %2\n\tl %0,%3[%1]", out);
	}
      else
	{
	  out[2] = GEN_INT (displacement + offset);
	  output_asm_insn ("l %0,%2[%1]", out);
	}
    }

  if ((scratch_regno & 3) == 0)
    {
      out[0] = scratch;
      output_asm_insn ("pop x%A0", out);
    }
  else
    {
      out[0] = simplify_gen_subreg (HImode, scratch, SImode, 0);
      output_asm_insn ("pop %0", out);
      out[0] = simplify_gen_subreg (HImode, scratch, SImode, 2);
      output_asm_insn ("pop %0", out);
    }
  return true;
}

/* Move a four-byte value without relying on generic post-reload splitting.
   This is also the far-pointer container: bytes 0..1 are the offset, byte 2
   is the segment, and byte 3 is padding.  */
const char *
nxu16_output_movsi (rtx *operands)
{
  rtx dst = operands[0];
  rtx src = operands[1];
  rtx pair[2];

  if (REG_P (dst) && REGNO (dst) == nxu16_SP)
    {
      pair[0] = gen_rtx_REG (HImode, nxu16_SP);
      pair[1] = simplify_gen_subreg (HImode, src, SImode, 0);
      output_asm_insn ("mov %0,%1", pair);
      return "";
    }
  if (REG_P (src) && REGNO (src) == nxu16_SP)
    {
      pair[0] = simplify_gen_subreg (HImode, dst, SImode, 0);
      pair[1] = gen_rtx_REG (HImode, nxu16_SP);
      output_asm_insn ("mov %0,%1", pair);
      pair[0] = simplify_gen_subreg (HImode, dst, SImode, 2);
      pair[1] = const0_rtx;
      output_asm_insn ("mov %0,%1", pair);
      return "";
    }

  if (REG_P (dst) && CONSTANT_P (src))
    {
      static const char *const templates[4] =
	{ "mov %0,lo8(%1)", "mov %0,hi8(%1)",
	  "mov %0,hlo8(%1)", "mov %0,hhi8(%1)" };
      for (int offset = 0; offset < 4; ++offset)
	{
	  pair[0] = simplify_gen_subreg (QImode, dst, SImode, offset);
	  pair[1] = src;
	  output_asm_insn (templates[offset], pair);
	}
      return "";
    }

  if (nxu16_output_overlapping_movsi_load (dst, src))
    return "";

  if ((REG_P (dst) && REG_P (src))
      || (REG_P (dst) && MEM_P (src))
      || (MEM_P (dst) && REG_P (src)))
    {
      bool backwards = (REG_P (dst) && REG_P (src)
			&& REGNO (dst) > REGNO (src));
      int offsets[2] = { backwards ? 2 : 0, backwards ? 0 : 2 };
      for (int i = 0; i < 2; ++i)
	{
	  int offset = offsets[i];
	  pair[0] = REG_P (dst)
	    ? simplify_gen_subreg (HImode, dst, SImode, offset)
	    : adjust_address_nv (dst, HImode, offset);
	  pair[1] = REG_P (src)
	    ? simplify_gen_subreg (HImode, src, SImode, offset)
	    : adjust_address_nv (src, HImode, offset);
	  output_asm_insn (nxu16_output_move (pair), pair);
	}
      return "";
    }
  gcc_unreachable ();
}

/* Emit the padded control-address record expected by large-model POP PC.  */
const char *
nxu16_output_far_transfer (rtx target, bool call_p)
{
  if (CONSTANT_P (target))
    {
      rtx operands[1] = { target };
      output_asm_insn (call_p ? "bl %0" : "b %0", operands);
      return "";
    }

  gcc_assert (REG_P (target));
  rtx operands[1] = { target };
  if ((REGNO (target) & 3) == 0)
    output_asm_insn ("push x%A0", operands);
  else
    {
      output_asm_insn ("push %C0", operands);
      output_asm_insn ("push e%A0", operands);
    }
  return call_p ? "bl __nxu16_indirect_call" : "pop pc";
}

/* Preserve support for explicit packed three-byte constants.  The default
   pointer ABI uses a four-byte SI container for the 24-bit address.  */
static bool
nxu16_assemble_integer (rtx value, unsigned int size, int aligned_p)
{
  /* FUNCTION_MODE is wider than both Pmode and UNITS_PER_WORD.  The generic
     implementation deliberately refuses such symbolic constants rather than
     selecting the target's 32-bit relocation, so emit the supported form
     explicitly for far pointers and ordinary SI constants.  */
  if (size == 4)
    {
      assemble_integer_with_op ("\t.long\t", value);
      return true;
    }
  if (size != 3)
    return default_assemble_integer (value, size, aligned_p);

  fputs ("\t.byte\tlo8(", asm_out_file);
  output_addr_const (asm_out_file, value);
  fputs ("),hi8(", asm_out_file);
  output_addr_const (asm_out_file, value);
  fputs ("),hlo8(", asm_out_file);
  output_addr_const (asm_out_file, value);
  fputs (")\n", asm_out_file);
  return true;
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
#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE nxu16_can_eliminate



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

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL nxu16_function_ok_for_sibcall



/******************************************************************************
 * Addressing Modes
 ******************************************************************************/

#undef  TARGET_LEGITIMATE_ADDRESS_P 
#define TARGET_LEGITIMATE_ADDRESS_P nxu16_legitimate_address_p 
#undef TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P
#define TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P nxu16_legitimate_addr_space_address_p

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
#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER nxu16_assemble_integer

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
#undef  TARGET_ADDR_SPACE_POINTER_MODE
#define TARGET_ADDR_SPACE_POINTER_MODE nxu16_addr_space_pointer_mode
#undef  TARGET_ADDR_SPACE_ADDRESS_MODE
#define TARGET_ADDR_SPACE_ADDRESS_MODE nxu16_addr_space_pointer_mode
#undef TARGET_ADDR_SPACE_SUBSET_P
#define TARGET_ADDR_SPACE_SUBSET_P nxu16_addr_space_subset_p
#undef TARGET_ADDR_SPACE_CONVERT
#define TARGET_ADDR_SPACE_CONVERT nxu16_addr_space_convert
#undef TARGET_SET_DEFAULT_TYPE_ATTRIBUTES
#define TARGET_SET_DEFAULT_TYPE_ATTRIBUTES nxu16_set_default_type_attributes
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
#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED nxu16_frame_pointer_required
#undef TARGET_SET_CURRENT_FUNCTION
#define TARGET_SET_CURRENT_FUNCTION nxu16_set_current_function

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS nxu16_hard_regno_nregs
#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK nxu16_hard_regno_mode_ok

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-nxu16.h"
