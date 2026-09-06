#define IN_TARGET_CODE 1

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


struct GTY(()) machine_function
{
  /* 'true' - if current function is a naked function.  */
  int is_naked;

  /* 0 when no "interrupt" attribute is present.
     1 when an "interrupt" attribute without arguments is present (and
	    perhaps also "interrupt" attributes with argument(s)).
     -1 when "interrupt" attribute(s) with arguments are present but none
     without argument.  */
  int is_interrupt;

  
  /* Current function stack size.  */
  int stack_usage;

  /* 'true' if a callee might be tail called */
  int sibcall_fails;

  /* 'true' if the above is_foo predicates are sanity-checked to avoid
     multiple diagnose for the same function.  */
  int attributes_checked_p;

  /* 'true' - if current function shall not use '__gcc_isr' pseudo
     instructions as specified by the "no_gccisr" attribute.  */
  int is_no_gccisr;

  /* Used for `__gcc_isr' pseudo instruction handling of
     non-naked ISR prologue / epilogue(s).  */
  struct
  {
    /* 'true' if this function actually uses "*gasisr" insns. */
    int yes;
    /* 'true' if this function is allowed to use "*gasisr" insns. */
    int maybe;
    /* The register numer as printed by the Done chunk.  */
    int regno;
  } gasisr;


};
/*
static struct machine_function *
nxu16_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}*/
/* Initialize the GCC target structure.  */

#undef  TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.word\t"
#undef  TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.long\t"
#undef  TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP "\t.word\t"
#undef  TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP "\t.long\t"
//#undef  TARGET_ASM_INTEGER
//#define TARGET_ASM_INTEGER nxu16_assemble_integer
#undef  TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START nxu16_file_start
#undef  TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END nxu16_file_end


#undef  TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE nxu16_function_value
#undef  TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE nxu16_libcall_value
#undef  TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P nxu16_function_value_regno_p


#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE nxu16_attribute_table

#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG nxu16_function_arg
#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE nxu16_function_arg_advance

#undef  TARGET_SET_CURRENT_FUNCTION
#define TARGET_SET_CURRENT_FUNCTION nxu16_set_current_function

#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY nxu16_return_in_memory


#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE nxu16_option_override


#undef  TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P
#define TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P  nxu16_addr_space_legitimate_address_p

#undef  TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS
#define TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS nxu16_addr_space_legitimize_address



#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND nxu16_print_operand
#undef  TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS nxu16_print_operand_address
/* To track if code will use .bss, .data, .rodata.  */
bool nxu16_need_clear_bss_p = false;
bool nxu16_need_copy_data_p = false;
bool nxu16_has_rodata_p = false;

static tree
nxu16_handle_fndecl_attribute (tree *node, tree name, tree /*args*/,
			     int /*flags*/, bool *no_add_attrs)
{
//todo:realize it

  return NULL_TREE;
}

/* nxu16 attributes.  */
TARGET_GNU_ATTRIBUTES (nxu16_attribute_table,
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "interrupt", 0, -1, true,  false, false, false,
    nxu16_handle_fndecl_attribute, NULL },
});


void
nxu16_cpu_cpp_builtins (cpp_reader *pfile)
{
  builtin_define_std ("nxu16");

}
/* Implement `ACCUMULATE_OUTGOING_ARGS'.  */

int
nxu16_accumulate_outgoing_args (void)
{
#define TARGET_ACCUMULATE_OUTGOING_ARGS 1
  if (!cfun)
    return TARGET_ACCUMULATE_OUTGOING_ARGS;

  /* FIXME: For setjmp and in nxu16_builtin_setjmp_frame_value we don't know
	what offset is correct.  In some cases it is relative to
	virtual_outgoing_args_rtx and in others it is relative to
	virtual_stack_vars_rtx.  For example code see
	    gcc.c-torture/execute/built-in-setjmp.c
	    gcc.c-torture/execute/builtins/sprintf-chk.c   */

  return (TARGET_ACCUMULATE_OUTGOING_ARGS
	  && !(cfun->calls_setjmp
	       || cfun->has_nonlocal_label));
}



/* Implement `TARGET_ASM_FILE_START'.  */
/* Outputs some text at the start of each assembler file.  */

static void
nxu16_file_start (void)
{
  default_file_start ();
  fprintf (asm_out_file, ";start");
}


/* Implement `TARGET_ASM_FILE_END'.  */
/* Outputs to the stdio stream FILE some
   appropriate text to go at the end of an assembler file.  */

static void
nxu16_file_end (void)
{
fprintf (asm_out_file, ";start");
}
/* Returns register number for function return value.*/

static inline unsigned int
nxu16_ret_register (void)
{
  return 0;
}
/* Implement `TARGET_LIBCALL_VALUE'.  */
/* Create an RTX representing the place where a
   library function returns a value of mode MODE.  */

static rtx
nxu16_libcall_value (machine_mode mode, const_rtx /*func*/)
{
  int offs = GET_MODE_SIZE (mode);

  if (offs <= 4)
    offs = (offs + 1) & ~1;

  return gen_rtx_REG (mode, nxu16_ret_register () + 2 - offs);
}
/* Implement `TARGET_FUNCTION_VALUE'.  */
/* Create an RTX representing the place where a
   function returns a value of data type VALTYPE.  */

static rtx
nxu16_function_value (const_tree type, const_tree /*fn_decl_or_type*/,
		    bool /*outgoing*/)
{
  if (TYPE_MODE (type) != BLKmode)
    return nxu16_libcall_value (TYPE_MODE (type), NULL_RTX);

  unsigned int offs = int_size_in_bytes (type);
  if (offs < 2)
    offs = 2;
  if (offs > 2 && offs < GET_MODE_SIZE (SImode))
    offs = GET_MODE_SIZE (SImode);
  else if (offs > GET_MODE_SIZE (SImode) && offs < GET_MODE_SIZE (DImode))
    offs = GET_MODE_SIZE (DImode);

  return gen_rtx_REG (BLKmode, nxu16_ret_register () + 2 - offs);
}
/* Implement `TARGET_FUNCTION_VALUE_REGNO_P'.  */
static bool
nxu16_function_value_regno_p (const unsigned int regno)
{
  return regno == nxu16_ret_register ();
}

/* Returns the number of registers to allocate for a function argument.  */

static int
nxu16_num_arg_regs (machine_mode mode, const_tree type)
{
  int size = (mode == BLKmode
	      ? int_size_in_bytes (type)
	      : GET_MODE_SIZE (mode));

  /* Align all function arguments to start in even-numbered registers.
     Odd-sized arguments leave holes above them.  */

  return (size + 1) & ~1;
}

/* Implement `TARGET_FUNCTION_ARG'.  */
/* Controls whether a function argument is passed
   in a register, and which register.  */

static rtx
nxu16_function_arg (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int bytes = nxu16_num_arg_regs (arg.mode, arg.type);

  if (cum->nregs && bytes <= cum->nregs)
    return gen_rtx_REG (arg.mode, cum->regno - bytes);

  cum->has_stack_args = 1;

  return NULL_RTX;
}


/* Implement `TARGET_FUNCTION_ARG_ADVANCE'.  */
/* Update the summarizer variable CUM to advance past an argument
   in the argument list.  */

static void
nxu16_function_arg_advance (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int bytes = nxu16_num_arg_regs (arg.mode, arg.type);

  cum->nregs -= bytes;
  cum->regno -= bytes;

  /* A parameter is being passed in a call-saved register.  As the original
     contents of these regs has to be restored before leaving the function,
     a function must not pass arguments in call-saved regs in order to get
     tail-called.  */

  if (cum->regno <=3
      && cum->nregs >= 0
      && !call_used_or_fixed_reg_p (cum->regno))
    {
      /* FIXME: We ship info on failing tail-call in struct machine_function.
	 This uses internals of calls.cc:expand_call() and the way args_so_far
	 is used.  targetm.function_ok_for_sibcall() needs to be extended to
	 pass &args_so_far, too.  At present, CUMULATIVE_ARGS is target
	 dependent so that such an extension is not wanted.  */

      cfun->machine->sibcall_fails = 1;
    }

  /* Test if all registers needed by the ABI are actually available.  If the
     user has fixed a GPR needed to pass an argument, an (implicit) function
     call will clobber that fixed register.  See PR45099 for an example.  */

  if (cum->regno <=3
      && cum->nregs >= 0)
    {
      for (int regno = cum->regno; regno < cum->regno + bytes; regno++)
	if (fixed_regs[regno])
	  warning (0, "fixed register %s used to pass parameter to function",
		   reg_names[regno]);
    }

  if (cum->nregs <= 0)
    {
      cum->nregs = 0;
      cum->regno = 0;
    }
}

/* Implement `TARGET_SET_CURRENT_FUNCTION'.  */
/* Sanity cheching for above function attributes.  */

static void
nxu16_set_current_function (tree decl)
{
  if (decl == NULL_TREE
      || current_function_decl == NULL_TREE
      || current_function_decl == error_mark_node
      || ! cfun->machine
      || cfun->machine->attributes_checked_p)
    return;

  location_t loc = DECL_SOURCE_LOCATION (decl);

  cfun->machine->is_interrupt = 0;//nxu16_interrupt_function (decl);

  //todo:realize it

  cfun->machine->attributes_checked_p = 1;
}

static bool
nxu16_return_in_memory (const_tree type, const_tree /*fntype*/)
{
  HOST_WIDE_INT size = int_size_in_bytes (type);
  HOST_WIDE_INT ret_size_limit =4;

  /* In nxu16, there are 8 return registers. But, for Tiny Core
     (ATtiny4/5/9/10/20/40) devices, only 4 registers are available.
     Return true if size is unknown or greater than the limit.  */

  if (size == -1 || size > ret_size_limit)
    {
      return true;
    }
  else
    {
      return false;
    }
}


/* Implement `TARGET_OPTION_OVERRIDE'.  */
//todo:treat it serious
static void
nxu16_option_override (void)
{
  /* caller-save.cc looks for call-clobbered hard registers that are assigned
     to pseudos that cross calls and tries so save-restore them around calls
     in order to reduce the number of stack slots needed.

     This might lead to situations where reload is no more able to cope
     with the challenge of nxu16's very few address registers and fails to
     perform the requested spills.  */


  /* Unwind tables currently require a frame pointer for correctness,
     see toplev.cc:process_options().  */

  if ((flag_unwind_tables
       || flag_non_call_exceptions
       || flag_asynchronous_unwind_tables)
      && !ACCUMULATE_OUTGOING_ARGS)
    {
      flag_omit_frame_pointer = 0;
    }

  /* Disable flag_delete_null_pointer_checks if zero is a valid address. */
  if (targetm.addr_space.zero_address_valid (ADDR_SPACE_GENERIC))
    flag_delete_null_pointer_checks = 0;

  /* PR ipa/92606: Inter-procedural analysis optimizes data across
     address-spaces and PROGMEM.  As of v14, the PROGMEM part is
     still not fixed (and there is still no target hook as proposed
     in PR92932).  Just disable respective bogus optimization.  */
  flag_ipa_icf_variables = 0;

  if (flag_pic == 1)
    warning (OPT_fpic, "%<-fpic%> is not supported");
  if (flag_pic == 2)
    warning (OPT_fPIC, "%<-fPIC%> is not supported");
  if (flag_pie == 1)
    warning (OPT_fpie, "%<-fpie%> is not supported");
  if (flag_pie == 2)
    warning (OPT_fPIE, "%<-fPIE%> is not supported");

    return;

}

static bool
nxu16_legitimate_address_p (machine_mode mode, rtx x, bool strict)
{
  bool ok = CONSTANT_ADDRESS_P (x);

  //todo:realize it
  return ok;
}

/* Implement `TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P'.  */

static bool
nxu16_addr_space_legitimate_address_p (machine_mode mode, rtx x, bool strict,
				     addr_space_t as, code_helper = ERROR_MARK)
{
  bool ok = false;

  switch (as)
    {
    default:
      gcc_unreachable();

    case ADDR_SPACE_GENERIC:
      return nxu16_legitimate_address_p (mode, x, strict);

    }

  return ok;
}


/* Implement `TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS'.  */

static rtx
nxu16_addr_space_legitimize_address (rtx x, rtx old_x,
				   machine_mode mode, addr_space_t as)
{
 /* if (ADDR_SPACE_GENERIC_P (as))
    return nxu16_legitimize_address (x, old_x, mode);*/
  //todo:realize it
  return old_x;
}
static const char *
ptrreg_to_str (int regno)
{
    static char buf[32]={0};
    int i=30;
    for(;regno&&i;--i,regno/=10){
    buf[i]="0123456789"[regno%10];}
    return &buf[i+1];
}

/* Implement `TARGET_PRINT_OPERAND_ADDRESS'.  */
/* Output ADDR to FILE as address.  */

static void
nxu16_print_operand_address (FILE *file, machine_mode /*mode*/, rtx addr)
{

  switch (GET_CODE (addr))
    {
    case REG:
      fprintf (file, "%s", ptrreg_to_str (REGNO (addr)));
      break;

    /*case PRE_DEC:
      fprintf (file, "-%s", ptrreg_to_str (REGNO (XEXP (addr, 0))));
      break;

    case POST_INC:
      fprintf (file, "%s+", ptrreg_to_str (REGNO (XEXP (addr, 0))));
      break;*/

    default:
	output_addr_const (file, addr);
    }
}




/* Implement `TARGET_PRINT_OPERAND'.  */
/* Output X as assembler operand to file FILE.
   For a description of supported %-codes, see top of nxu16.md.  */

static void
nxu16_print_operand (FILE *file, rtx x, int code)
{
  int abcd = 0, ef = 0, ij = 0;

  if (code >= 'A' && code <= 'D')
    abcd = code - 'A';
  else if (code == 'E' || code == 'F')
    ef = code - 'E';
  else if (code == 'I' || code == 'J')
    ij = code - 'I';

  else if (code == 't'
	   || code == 'T')
    {
      static int t_regno = -1;
      static int t_nbits = -1;

      if (REG_P (x) && t_regno < 0 && code == 'T')
	{
	  t_regno = REGNO (x);
	  t_nbits = GET_MODE_BITSIZE (GET_MODE (x));
	}
      else if (CONST_INT_P (x) && t_regno >= 0
	       && IN_RANGE (INTVAL (x), 0, t_nbits - 1))
	{
	  int bpos = INTVAL (x);

	  fprintf (file, "%s", reg_names[t_regno + bpos / 8]);
	  if (code == 'T')
	    fprintf (file, ",%d", bpos % 8);

	  t_regno = -1;
	}
      else
	fatal_insn ("operands to %T/%t must be reg + const_int:", x);
    }
  else if (code == 'E' || code == 'F')
    {
      rtx op = XEXP (x, 0);
      fprintf (file, "%s", reg_names[REGNO (op) + ef]);
    }
  else if (code == 'I' || code == 'J')
    {
      rtx op = XEXP (XEXP (x, 0), 0);
      fprintf (file, "%s", reg_names[REGNO (op) + ij]);
    }
  else if (REG_P (x))
    {
      if (code == 'r' && REGNO (x) < 16)
	fprintf (file, "%d", (int) REGNO (x));
      else
	fprintf (file, "%s", reg_names[REGNO (x) + abcd]);
    }
  else if (CONST_INT_P (x))
    {
      HOST_WIDE_INT ival = INTVAL (x);
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, ival + abcd);
    }
  else if (MEM_P (x))
    {
      rtx addr = XEXP (x, 0);

      if (code == 'm')
	{
	  if (!CONSTANT_P (addr))
	    fatal_insn ("bad address, not a constant:", addr);

	  output_addr_const (file, addr);
	}
      else if (code == 'i')
	{
	  nxu16_print_operand (file, addr, 'i');
	}
      else if (code == 'o')
	{
	  if (GET_CODE (addr) != PLUS)
	    fatal_insn ("bad address, not (reg+disp):", addr);

	  nxu16_print_operand (file, XEXP (addr, 1), 0);
	}
      else if (code == 'b')
	{
	  if (GET_CODE (addr) != PLUS)
	    fatal_insn ("bad address, not (reg+disp):", addr);

	  nxu16_print_operand_address (file, VOIDmode, XEXP (addr, 0));
	}
      else if (code == 'p' || code == 'r')
	{
	  if (GET_CODE (addr) != POST_INC && GET_CODE (addr) != PRE_DEC)
	    fatal_insn ("bad address, not post_inc or pre_dec:", addr);

	  if (code == 'p')
	    /* X, Y, Z */
	    nxu16_print_operand_address (file, VOIDmode, XEXP (addr, 0));
	  else
	    nxu16_print_operand (file, XEXP (addr, 0), 0);  /* r26, r28, r30 */
	}
      else if (GET_CODE (addr) == PLUS)
	{
	  nxu16_print_operand_address (file, VOIDmode, XEXP (addr, 0));
	  fputc ('+', file);
	  nxu16_print_operand (file, XEXP (addr, 1), code);
	}
      else
	nxu16_print_operand_address (file, VOIDmode, addr);
    }
  else if (code == 'x')
    {
      output_addr_const (file, x);
    }
  else if (CONST_FIXED_P (x))
    {
      HOST_WIDE_INT ival = INTVAL (x);
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, ival);
    }
  else if (CONST_DOUBLE_P (x))
    {
      if (GET_MODE (x) == SFmode)
	{
	  long val;
	  REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (x), val);
	  fprintf (file, "0x%lx", val);
	}
      else if (GET_MODE (x) == DFmode)
	{
	  long l[2];
	  REAL_VALUE_TO_TARGET_DOUBLE (*CONST_DOUBLE_REAL_VALUE (x), l);
	  fprintf (file, "0x%lx%08lx", l[1] & 0xffffffff, l[0] & 0xffffffff);
	}
      else
	fatal_insn ("internal compiler error.  Unknown mode:", x);
    }
  else if (GET_CODE (x) == CONST_STRING)
    fputs (XSTR (x, 0), file);
  else
    nxu16_print_operand_address (file, VOIDmode, x);
}

void
nxu16_expand_prologue (void)
{
printf("nxu16_expand_prologue\n");
}

void
nxu16_expand_epilogue ()
{
  printf("nxu16_expand_epilogue\n");
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
      ALL_REGS,ALL_REGS, STACK_REG,CC_REG
    };

  if (r < 20)
    return reg_class_tab[r];

  return ALL_REGS;
}
/* Worker function for `INITIAL_ELIMINATION_OFFSET'.  */
/* Compute offset between arg_pointer and frame_pointer.  */

int
nxu16_initial_elimination_offset (int from, int to)
{
  /*if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return 0;
  else
    {
      int offset = frame_pointer_needed ? 2 : 0;
      int nxu16_pc_size = nxu16_HAVE_EIJMP_EICALL ? 3 : 2;

      // If FROM is ARG_POINTER_REGNUM, we are not in an ISR as ISRs
      // might not have arguments.  Hence the following is not affected
      // by gasisr prologues.
      offset += nxu16_regs_to_save (NULL);
      return (get_frame_size () + nxu16_outgoing_args_size()
	      + nxu16_pc_size + 1 + offset);
    }*/
    return 0;//todo:realize it
}
rtx
nxu16_return_addr_rtx (int count, rtx tem)
{
  rtx r;

  /* Can only return this function's return address. Others not supported.  */
  if (count)
    return NULL;

//todo:treat it serious
  r = gen_rtx_PLUS (Pmode, tem, r);
  r = gen_frame_mem (Pmode, memory_address (Pmode, r));
  r = gen_rtx_ROTATE (HImode, r, GEN_INT (8));
  return r;
}

void
nxu16_init_cumulative_args (CUMULATIVE_ARGS *cum, tree fntype, rtx libname,
			  tree /*fndecl*/)
{
  cum->nregs = 4;
  cum->regno = 0;
  cum->has_stack_args = 0;
  if (!libname && stdarg_p (fntype))
    cum->nregs = 0;

  /* Assume the calle may be tail called */

  cfun->machine->sibcall_fails = 0;
}

int
nxu16_function_arg_regno_p (int r)
{
  return IN_RANGE (r,0, 3);
}


/* Worker function for `EPILOGUE_USES'.  */

int
nxu16_epilogue_uses (int /*regno*/)
{
  if (reload_completed
      && cfun->machine
      && (cfun->machine->is_interrupt))
    return 1;
  return 0;
}


/* Worker function for `TARGET_ASM_CONSTRUCTOR'.  */
 void
nxu16_asm_out_ctor (rtx symbol, int priority)
{
  fputs ("\t.global __do_global_ctors\n", asm_out_file);
  default_ctor_section_asm_out_constructor (symbol, priority);
}


/* Worker function for `TARGET_ASM_DESTRUCTOR'.  */

 void
nxu16_asm_out_dtor (rtx symbol, int priority)
{
  fputs ("\t.global __do_global_dtors\n", asm_out_file);
  default_dtor_section_asm_out_destructor (symbol, priority);
}
/* Implement `ASM_OUTPUT_ALIGNED_DECL_LOCAL'.  */
/* Implement `ASM_OUTPUT_ALIGNED_DECL_COMMON'.  */
/* Track need of __do_clear_bss.  */

void
nxu16_asm_output_aligned_decl_common (FILE *stream, tree /* decl */,
				    const char *name,
				    unsigned HOST_WIDE_INT size,
				    unsigned int align, bool local_p)
{
  /* __gnu_lto_slim is just a marker for the linker injected by toplev.cc.
     There is no need to trigger __do_clear_bss code for them.  */

  if (!startswith (name, "__gnu_lto"))
    nxu16_need_clear_bss_p = true;

  if (local_p)
    ASM_OUTPUT_ALIGNED_LOCAL (stream, name, size, align);
  else
    ASM_OUTPUT_ALIGNED_COMMON (stream, name, size, align);
}


/* Implement `ASM_OUTPUT_ALIGNED_BSS'.  */

void
nxu16_asm_asm_output_aligned_bss (FILE *file, tree decl, const char *name,
				unsigned HOST_WIDE_INT size, int align,
				void (*default_func)
				  (FILE *, tree, const char *,
				   unsigned HOST_WIDE_INT, int))
{
  if (!startswith (name, "__gnu_lto"))
    nxu16_need_clear_bss_p = true;

  default_func (file, decl, name, size, align);
}

/* Implement `INIT_EXPANDERS'.  */
/* The function works like a singleton.  */

void
nxu16_init_expanders (void)
{
//maybe useless
}


rtx
nxu16_legitimize_reload_address (rtx *px, machine_mode mode, int opnum,
			       int type, int addr_type, int /*ind_levels*/,
			       rtx (*mk_memloc)(rtx,int))
{
  rtx x = *px;


  if (1 && (GET_CODE (x) == POST_INC
	    || GET_CODE (x) == PRE_DEC))
    {
      push_reload (XEXP (x, 0), XEXP (x, 0), &XEXP (x, 0), &XEXP (x, 0),
		   POINTER_REGS, GET_MODE (x), GET_MODE (x), 0, 0,
		   opnum, RELOAD_OTHER);


      return x;
    }

  if (GET_CODE (x) == PLUS
      && REG_P (XEXP (x, 0))
      && reg_equiv_constant (REGNO (XEXP (x, 0))) == 0
      && CONST_INT_P (XEXP (x, 1))
      && INTVAL (XEXP (x, 1)) >= 1)
    {
      bool fit = INTVAL (XEXP (x, 1)) <= (64 - (signed)GET_MODE_SIZE (mode));

      if (fit)
	{
	  if (reg_equiv_address (REGNO (XEXP (x, 0))) != 0)
	    {
	      int regno = REGNO (XEXP (x, 0));
	      rtx mem = mk_memloc (x, regno);

	      push_reload (XEXP (mem, 0), NULL_RTX, &XEXP (mem, 0), NULL,
			   POINTER_REGS, Pmode, VOIDmode, 0, 0,
			   1, (enum reload_type) addr_type);




	      return x;
	    }
	}
      else if (! (frame_pointer_needed
		  && XEXP (x, 0) == frame_pointer_rtx))
	{
	  push_reload (x, NULL_RTX, px, NULL,
		       POINTER_REGS, GET_MODE (x), VOIDmode, 0, 0,
		       opnum, (enum reload_type) type);
	  return x;
	}
    }

  return NULL_RTX;
}


























gcc_target targetm = TARGET_INITIALIZER;


#include "gt-nxu16.h"
