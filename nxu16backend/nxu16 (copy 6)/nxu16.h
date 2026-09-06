#define TARGET_CPU_CPP_BUILTINS()	nxu16_cpu_cpp_builtins (pfile)

#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 0
#define WORDS_BIG_ENDIAN 0

#define UNITS_PER_WORD 1//todo:be 4 after figure out whats this

#define MAX_FIXED_MODE_SIZE 32

//align,todo:correct it
#define PARM_BOUNDARY 8

#define FUNCTION_BOUNDARY 16

#define EMPTY_FIELD_BOUNDARY 8

#define BIGGEST_ALIGNMENT 16

#define TARGET_VTABLE_ENTRY_ALIGN 8

#define STRICT_ALIGNMENT 0

/////////type

#define INT_TYPE_SIZE (16)
#define SHORT_TYPE_SIZE (16)
#define LONG_TYPE_SIZE (32)
#define LONG_LONG_TYPE_SIZE (64)

#define LONG_LONG_ACCUM_TYPE_SIZE 64

#define DEFAULT_SIGNED_CHAR 1

#define SIZE_TYPE ("unsigned int")
#define PTRDIFF_TYPE ("int")

#define WCHAR_TYPE_SIZE 16

////////register


#define FIRST_PSEUDO_REGISTER 20
/* r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15
ea lr sp psw
...*/ 
#define FIXED_REGISTERS {\
  0,0,/* r0 r1 */\
  0,0,/* r2 r3 */\
  0,0,/* r4 r5 */\
  0,0,/* r6 r7 */\
  0,0,/* r8 r9 */\
  0,0,/* r10 r11 */\
  0,0,/* r12 r13 */\
  0,0,/* r14 r15 (fp) */\
  1,1,1,1/* ea lr sp psw */\
  }
#define CALL_USED_REGISTERS {\
  1,1,/* r0 r1 */\
  1,1,/* r2 r3 */\
  0,0,/* r4 r5 */\
  0,0,/* r6 r7 */\
  0,0,/* r8 r9 */\
  0,0,/* r10 r11 */\
  0,0,/* r12 r13 */\
  0,0,/* r14 r15 (fp) */\
  1,1,1,1/* ea lr sp psw */\
  }

enum reg_class {
  NO_REGS,
  STACK_REG,			
  POINTER_REGS,			
  GENERAL_REGS,			
  CC_REG,		
  ALL_REGS, 
  LIM_REG_CLASSES,
};

#define N_REG_CLASSES (int)LIM_REG_CLASSES

#define REG_CLASS_NAMES {					\
		   "NO_REGS",					\
		   "STACK_REG",	    			        \
		   "POINTER_REGS", 				\
		   "GENERAL_REGS", 				\
		   "CC_REG", 					\
		   "ALL_REGS" }

#define REG_CLASS_CONTENTS {			\
  {0x00000000},	/* NO_REGS */			\
  {0x00040000},	/* STACK_REG */			\
  {0x0000ffff},	                                \
  {0x0000ffff},	/* GENERAL_REGS */		\
  {0x00080000},	/* CC */			\
  {0x000fffff}	/* ALL_REGS */			\
}

#define REGNO_REG_CLASS(R) nxu16_regno_reg_class(R)

#define INDEX_REG_CLASS GENERAL_REGS

#define BASE_REG_CLASS GENERAL_REGS

#define REGNO_OK_FOR_BASE_P(NUM)  0<=NUM<=15

#define REGNO_OK_FOR_INDEX_P(NUM) 0<=NUM<=15

//////////////stack

#define STACK_PUSH_CODE POST_DEC

#define STACK_GROWS_DOWNWARD 1

#define STACK_POINTER_OFFSET 1

#define FIRST_PARM_OFFSET(FUNDECL) 0

#define STACK_BOUNDARY 8

#define STACK_POINTER_REGNUM 18

#define FRAME_POINTER_REGNUM 14

#define ARG_POINTER_REGNUM FRAME_POINTER_REGNUM //todo:evaluate it too

//#define STATIC_CHAIN_REGNUM 0 //pass in memeory now,todo:evalate whether need to define one

#define RELOAD_ELIMINABLE_REGS {				\
    { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM }}

#define ELIMINABLE_REGS						\
  {								\
    { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM }		\
  }

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
  OFFSET = nxu16_initial_elimination_offset (FROM, TO)

#define RETURN_ADDR_RTX(count, tem) nxu16_return_addr_rtx (count, tem)

/* Don't use Push rounding. expr.cc: emit_single_push_insn is broken 
   for POST_DEC targets (PR27386).  */
/*#define PUSH_ROUNDING(NPUSHED) (NPUSHED)*/

typedef struct nxu16_args
{
  /* # Registers available for passing */
  int nregs;

  /* Next available register number */
  int regno;

  /* Whether some of the arguments are passed on the stack,
     and hence an arg pointer is needed.  */
  int has_stack_args;
} CUMULATIVE_ARGS;

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
  nxu16_init_cumulative_args (&(CUM), FNTYPE, LIBNAME, FNDECL)

#define FUNCTION_ARG_REGNO_P(r) nxu16_function_arg_regno_p(r)

#define DEFAULT_PCC_STRUCT_RETURN 0

#define EPILOGUE_USES(REGNO) nxu16_epilogue_uses(REGNO)

#define MAX_REGS_PER_ADDRESS 3

#define LEGITIMIZE_RELOAD_ADDRESS(X,MODE,OPNUM,TYPE,IND_L,WIN)          \
  do {                                                                  \
    rtx new_x = nxu16_legitimize_reload_address (&(X), MODE, OPNUM, TYPE, \
                                               ADDR_TYPE (TYPE),        \
                                               IND_L, make_memloc);     \
    if (new_x)                                                          \
      {                                                                 \
        X = new_x;                                                      \
        goto WIN;                                                       \
      }                                                                 \
  } while (0)

/* We increase branch costs after reload in order to keep basic-block
   reordering from introducing out-of-line jumps and to prefer fall-through
   edges instead.  The default branch costs are 0, mainly because otherwise
   do_store_flag might come up with bloated code.  */
#define BRANCH_COST(speed_p, predictable_p)     \
  (0/*todo:setable*/ + (reload_completed ? 4 : 0))

#define SLOW_BYTE_ACCESS 0

#define NO_FUNCTION_CSE 1


////asm

#define TEXT_SECTION_ASM_OP "\t.text"

#define DATA_SECTION_ASM_OP "\t.data"

#define BSS_SECTION_ASM_OP "\t.section .bss"

#undef CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP "\t.section .ctors,\"a\",@progbits"

#undef DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP "\t.section .dtors,\"a\",@progbits"

#define TARGET_ASM_CONSTRUCTOR nxu16_asm_out_ctor

#define TARGET_ASM_DESTRUCTOR nxu16_asm_out_dtor

#define SUPPORTS_INIT_PRIORITY 0 //todo:not be lazy

/* We pretend jump tables are in text section because otherwise,
   final.cc will switch to .rodata before jump tables and thereby
   triggers __do_copy_data.  As we implement ASM_OUTPUT_ADDR_VEC,
   we still have full control over the jump tables themselves.  */
#define JUMP_TABLES_IN_TEXT_SECTION 1

#define ASM_COMMENT_START " ; "

#define ASM_APP_ON "/* #APP */\n"

#define ASM_APP_OFF "/* #NOAPP */\n"

#define IS_ASM_LOGICAL_LINE_SEPARATOR(C, STR) ((C) == '\n' || ((C) == '$'))

#define ASM_OUTPUT_ALIGNED_DECL_COMMON(STREAM, DECL, NAME, SIZE, ALIGN) \
  nxu16_asm_output_aligned_decl_common (STREAM, DECL, NAME, SIZE, ALIGN, false)

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  nxu16_asm_asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN, \
				  asm_output_aligned_bss)

#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(STREAM, DECL, NAME, SIZE, ALIGN)  \
  nxu16_asm_output_aligned_decl_common (STREAM, DECL, NAME, SIZE, ALIGN, true)

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP ".global\t"

#define SUPPORTS_WEAK 1

#define HAS_INIT_SECTION 1

#define REGISTER_NAMES {				\
  "r0","r1","r2","r3","r4","r5","r6","r7",		\
    "r8","r9","r10","r11","r12","r13","r14","r15",	\
    "ea","lr","sp","psw"}
/*
#define FINAL_PRESCAN_INSN(insn, operand, nop)  \
  nxu16_final_prescan_insn (insn, operand,nop)
*///maybe usable
/*
#define ASM_OUTPUT_REG_PUSH(STREAM, REGNO)	\
{						\
  gcc_assert (REGNO < 32);			\
  fprintf (STREAM, "\tpush\tr%d", REGNO);	\
}

#define ASM_OUTPUT_REG_POP(STREAM, REGNO)	\
{						\
  gcc_assert (REGNO < 32);			\
  fprintf (STREAM, "\tpop\tr%d", REGNO);	\
}

#define ASM_OUTPUT_ADDR_VEC(TLABEL, TDATA)      \
  nxu16_output_addr_vec (TLABEL, TDATA)
*/
#define ASM_OUTPUT_ALIGN(STREAM, POWER)                 \
  do {                                                  \
    if ((POWER) > 0)                                    \
      fprintf (STREAM, "\t.p2align\t%d\n", POWER);      \
  } while (0)

#define CASE_VECTOR_MODE HImode

#undef WORD_REGISTER_OPERATIONS

/* Can move only a single byte from memory to reg in a
   single instruction. */

#define MOVE_MAX 4



/* Set MOVE_RATIO to 3 to allow memory moves upto 4 bytes to happen
   by pieces when optimizing for speed, like it did when MOVE_MAX_PIECES
   was 4. When optimizing for size, allow memory moves upto 2 bytes. 
   Also see nxu16_use_by_pieces_infrastructure_p. */

#define MOVE_RATIO(speed) ((speed) ? 3 : 2)

#define Pmode HImode

#define FUNCTION_MODE HImode

#define DOLLARS_IN_IDENTIFIERS 0

#define TRAMPOLINE_SIZE 4

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
  fprintf (FILE, "/* profiler %d */", (LABELNO))



/* Driver self specs has lmited functionality w.r.t. '%s' for dynamic specs.
   Apply '%s' to a static string to inflate the file (directory) name which
   is used to diagnose problems with reading the specs file.  */


/* No libstdc++ for now.  Empty string doesn't work.  */
#define LIBSTDCXX "gcc"

/* This is the default without any -mmcu=* option.  */
//todo:swhats this?//#define MULTILIB_DEFAULTS { "mmcu=" nxu16_MMCU_DEFAULT }

#define TEST_HARD_REG_CLASS(CLASS, REGNO) \
  TEST_HARD_REG_BIT (reg_class_contents[ (int) (CLASS)], REGNO)

#define CR_TAB "\n\t"

#define DWARF2_ADDR_SIZE 4

/*#define INCOMING_RETURN_ADDR_RTX   nxu16_incoming_return_addr_rtx ()
#define INCOMING_FRAME_SP_OFFSET   (4)*///no need for debugging

/* The caller's stack pointer value immediately before the call
   is one byte below the first argument.  */
#define ARG_POINTER_CFA_OFFSET(FNDECL)  -2//todo:what's this ??


/* nxu16 does not round pushes, but the existence of this macro is
   required in order for pushes to be generated.  */
#define PUSH_ROUNDING(X)	(X)

/* Define prototype here to avoid build warning.  Some files using
   ACCUMULATE_OUTGOING_ARGS (directly or indirectly) include
   tm.h but not tm_p.h.  */
extern int nxu16_accumulate_outgoing_args (void);
#define ACCUMULATE_OUTGOING_ARGS nxu16_accumulate_outgoing_args()

#define INIT_EXPANDERS nxu16_init_expanders()































