#define TARGET_THUMB 1
#define TARGET_EITHER 1
#define CONST_OK_FOR_THUMB_LETTER(x,y) (1)

////////////////////


#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 0
#define WORDS_BIG_ENDIAN 0

#ifdef IN_LIBGCC2
/* This is to get correct SI and DI modes in libgcc2.c (32 and 64 bits).  */
#define UNITS_PER_WORD 4
#else
/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 1
#endif

#define POINTER_SIZE 16

#define MAX_FIXED_MODE_SIZE 32

#define PARM_BOUNDARY 8

#define FUNCTION_BOUNDARY 8

#define EMPTY_FIELD_BOUNDARY 8

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 8

#define TARGET_VTABLE_ENTRY_ALIGN 8

#define STRICT_ALIGNMENT 0

#define TARGET_INT8 0

#define INT_TYPE_SIZE (TARGET_INT8 ? 8 : 16)
#define SHORT_TYPE_SIZE (INT_TYPE_SIZE == 8 ? INT_TYPE_SIZE : 16)
#define LONG_TYPE_SIZE (INT_TYPE_SIZE == 8 ? 16 : 32)
#define LONG_LONG_TYPE_SIZE (INT_TYPE_SIZE == 8 ? 32 : 64)

#define LONG_LONG_ACCUM_TYPE_SIZE 64

#define DEFAULT_SIGNED_CHAR 1

#define SIZE_TYPE (INT_TYPE_SIZE == 8 ? "long unsigned int" : "unsigned int")
#define PTRDIFF_TYPE (INT_TYPE_SIZE == 8 ? "long int" :"int")

#define WCHAR_TYPE_SIZE 16

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
  REG_CC,	
  REG_SP,		
  ALL_REGS, 
  LIM_REG_CLASSES,
};

#define N_REG_CLASSES (int)LIM_REG_CLASSES

#define REG_CLASS_NAMES {					\
		   "NO_REGS",					\
		   "STACK_REG",	    			        \
		   "POINTER_REGS", 		\
		   "GENERAL_REGS", 		\
		   "REG_CC", 	\
		   "REG_SP",\
		   "ALL_REGS" }

#define REG_CLASS_CONTENTS {						\
  {0x00000000},	/* NO_REGS */				\
  {0x00040000},	/* STACK_REG */			\
  {0x0000ffff},	                                \
  {0x0000ffff},	/* GENERAL_REGS */		\
  {0x00080000},	/* CC */				\
  {0x00040000},/*sp*/                                \
  {0x000fffff}	/* ALL_REGS */				\
}

#define INDEX_REG_CLASS NO_REGS

#define REGNO_REG_CLASS(R) nxu16_regno_reg_class(R)

/* Dummy definition.  TODO:whats this?*/

typedef int CUMULATIVE_ARGS;

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
  do { (CUM) = 0; } while (0)

#define Pmode				HImode
#define FUNCTION_MODE			QImode
#define CASE_VECTOR_MODE		HImode

#define MOVE_MAX 2

#define MAX_REGS_PER_ADDRESS 1

#define STACK_POINTER_REGNUM 18

#define FRAME_POINTER_REGNUM 14

#define ARG_POINTER_REGNUM 16

#define FIRST_ARG_REGISTER 0
#define LAST_ARG_REGISTER 3

#define FUNCTION_ARG_REGNO_P(REGNO) \
  ((REGNO) >= FIRST_ARG_REGISTER && (REGNO) <= LAST_ARG_REGISTER)
  
#define STACK_PUSH_CODE POST_DEC

#define STACK_GROWS_DOWNWARD 1

#define STACK_POINTER_OFFSET 1

#define FIRST_PARM_OFFSET(FUNDECL) 0

#define STACK_BOUNDARY 8

#define BASE_REG_CLASS	GENERAL_REGS

#define REGNO_OK_FOR_BASE_P(NUM) \
  ((NUM) >= 0 && (NUM) <= 13)
  
#define TRAMPOLINE_SIZE 0

#define ELIMINABLE_REGS						\
  {								\
    { ARG_POINTER_REGNUM, STACK_POINTER_REGNUM },		\
    { ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM },		\
    { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM }		\
  }

#define TARGET_CPU_CPP_BUILTINS()		\
      builtin_define ("__nxu16__");	
      
    	
#define TEXT_SECTION_ASM_OP "\t.text"

#define DATA_SECTION_ASM_OP "\t.data"

#define BSS_SECTION_ASM_OP "\t.section .bss"

/* Define the pseudo-ops used to switch to the .ctors and .dtors sections.
   There are no shared libraries on this target, and these sections are
   placed in the read-only program memory, so they are not writable.  */

#undef CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP "\t.section .ctors,\"a\",@progbits"

#undef DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP "\t.section .dtors,\"a\",@progbits"
/*todo:realize this
#define TARGET_ASM_CONSTRUCTOR avr_asm_out_ctor

#define TARGET_ASM_DESTRUCTOR avr_asm_out_dtor
*/
#define SUPPORTS_INIT_PRIORITY 0

/* We pretend jump tables are in text section because otherwise,
   final.cc will switch to .rodata before jump tables and thereby
   triggers __do_copy_data.  As we implement ASM_OUTPUT_ADDR_VEC,
   we still have full control over the jump tables themselves.  */
#define JUMP_TABLES_IN_TEXT_SECTION 1

#define ASM_COMMENT_START " ; "

#define ASM_APP_ON "/* #APP */\n"

#define ASM_APP_OFF "/* #NOAPP */\n"

#define IS_ASM_LOGICAL_LINE_SEPARATOR(C, STR) ((C) == '\n' || ((C) == '$'))

#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM) \
  sprintf (LABEL, "%s@%u", PREFIX, (unsigned) (NUM))

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP ".global\t"

#define SUPPORTS_WEAK 1

#define HAS_INIT_SECTION 1

#define REGISTER_NAMES {				\
  "r0","r1","r2","r3","r4","r5","r6","r7",		\
    "r8","r9","r10","r11","r12","r13","r14","r15",	\
    "ea","lr","sp","psw"}

#define SLOW_BYTE_ACCESS 0

#define ASM_OUTPUT_ALIGN(STREAM, POWER)                 \
  do {                                                  \
    if ((POWER) > 0)                                    \
      fprintf (STREAM, "\t.p2align\t%d\n", POWER);      \
  } while (0)
#define FUNCTION_PROFILER(FILE, LABELNO)  \
  fprintf (FILE, "/* profiler %d */", (LABELNO))

#define REGNO_OK_FOR_INDEX_P(NUM) 0

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
  OFFSET = nxu16_initial_elimination_offset (FROM, TO)


/* Output of uninitialized variables.  */

#define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED)		\
  do {								\
    bool needs_dollar = strlen (NAME) == 1;			\
    fprintf ((STREAM), "\t.global ");				\
    assemble_name ((STREAM), (NAME));				\
    fprintf ((STREAM), "\n%s%s:\n\t.res %d\n", (NAME),		\
	     needs_dollar ? "$" : "", (int) (SIZE));		\
  } while (0)

#define ASM_OUTPUT_LOCAL(STREAM, NAME, SIZE, ROUNDED)		\
  do {								\
    assemble_name ((STREAM), (NAME));				\
    fprintf ((STREAM), ":\n");					\
    fprintf ((STREAM), "\t.res %d\n", (int) (SIZE));		\
  } while (0)

#define ASM_OUTPUT_SKIP(STREAM, NBYTES)				\
  do {								\
    fprintf ((STREAM), ".res %d,$00\n", (int) (NBYTES));	\
  } while (0)







