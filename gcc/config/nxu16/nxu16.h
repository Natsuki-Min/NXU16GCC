/* Target Definitions for nxu16.
   Copyright (C) 2008-2022 Free Software Foundation, Inc.

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

#ifndef GCC_nxu16_H
#define GCC_nxu16_H

/******************************************************************************
 * Storage Layout
 ******************************************************************************/

#define TARGET_LITTLE_ENDIAN 1
#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN (!TARGET_LITTLE_ENDIAN)
#define WORDS_BIG_ENDIAN (!TARGET_LITTLE_ENDIAN)

/* Number of storage units in a word; normally the size of a
   general-purpose register, a power of two from 1 or 8.  */
#define UNITS_PER_WORD 1//todo:then 4?

/* Width of a pointer, in bits. You must specify a value no wider than the width of Pmode.
   If it is not equal to the width of Pmode, you must define POINTERS_EXTEND_UNSIGNED.
   If you do not specify a value the default is BITS_PER_WORD. */
#define POINTER_SIZE 16

/* Normal alignment required for function parameters on the stack, in
   bits.  All stack parameters receive at least this much alignment
   regardless of data type.  */
#define PARM_BOUNDARY 16

/* Define this macro to the minimum alignment enforced by hardware
   for the stack pointer on this machine.  The definition is a C
   expression for the desired alignment (measured in bits).  */
#define STACK_BOUNDARY 16
//#define PUSH_ROUNDING(BYTES)	((BYTES+1)/(BYTES-BYTES+2)*(BYTES-BYTES+2)) //TODO:arm told me not define this
/* Alignment required for a function entry point, in bits.  */
#define FUNCTION_BOUNDARY 16

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 16

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 16


/* Every structures size must be a multiple of 8 bits.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* Look at the fundamental type that is used for a bit-field and use
   that to impose alignment on the enclosing structure.
   struct s {int a:8}; should have same alignment as "int", not "char".  */
#define	PCC_BITFIELD_TYPE_MATTERS 1

/* Largest integer machine mode for structures.  If undefined, the default
   is GET_MODE_SIZE(DImode).  */
#define MAX_FIXED_MODE_SIZE 16

/* Make arrays of chars word-aligned for the same reasons.  */
/*#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))*/

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 0


/******************************************************************************
 * Type Layout
 ******************************************************************************/





#define INT_TYPE_SIZE (16)
#define SHORT_TYPE_SIZE (INT_TYPE_SIZE == 8 ? INT_TYPE_SIZE : 16)
#define LONG_TYPE_SIZE (INT_TYPE_SIZE == 8 ? 16 : 32)
#define LONG_LONG_TYPE_SIZE (INT_TYPE_SIZE == 8 ? 32 : 64)

#define LONG_LONG_ACCUM_TYPE_SIZE 64

#define DEFAULT_SIGNED_CHAR 1

#define SIZE_TYPE (INT_TYPE_SIZE == 8 ? "long unsigned int" : "unsigned int")
#define PTRDIFF_TYPE (INT_TYPE_SIZE == 8 ? "long int" :"int")

#define WCHAR_TYPE_SIZE 16
/******************************************************************************
 * Registers
 ******************************************************************************/

/* Register Basics */

#define nxu16_ER0  0
#define nxu16_ER2  2
#define nxu16_ER4  4
#define nxu16_ER6  6
#define nxu16_ER8  8
#define nxu16_ER10  10
#define nxu16_ER12  12
#define nxu16_ER14 14
#define nxu16_R0 0
#define nxu16_R1 1
#define nxu16_R2 2
#define nxu16_R3 3
#define nxu16_R4 4
#define nxu16_R5 5
#define nxu16_R6 6
#define nxu16_R7 7
#define nxu16_R8 8
#define nxu16_R9 9
#define nxu16_R10 10
#define nxu16_R11 11
#define nxu16_R12 12
#define nxu16_R13 13
#define nxu16_R14 14
#define nxu16_R15 15
//#define nxu16_FP  14
#define nxu16_SP  16
#define nxu16_CC  17
#define nxu16_LR  18
#define nxu16_FP  19//fake reg
#define nxu16_BP  20


#define FIRST_PSEUDO_REGISTER 21

#define FIXED_REGISTERS                         \
    { 0, 0, 0, 0,  0,0,0,0,0,0,0,0,0,0,0,0     ,                        \
      1,1,1,1,1                               }

#define CALL_USED_REGISTERS                     \
    { 1, 1, 1, 1,0,0,0,0,0,0,0,0,0,0,0,0        ,                       \
      1,1,1,1 ,1                              }//todo:sp?

#define REG_ALLOC_ORDER {			\
    0,1,2,3,15,14,13,12,11,10,9,8,7,6,5,4,					\
    16,17,18,19,20				\
    }

/******************************************************************************
 * Register Classes
 ******************************************************************************/

enum reg_class
{
  NO_REGS,
  GENERAL_REGS,
  SPECIAL_REGS,
  CC_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES LIM_REG_CLASSES

#define REG_CLASS_NAMES                         \
    { "NO_REGS",                                \
      "GENERAL_REGS",                           \
      "SPECIAL_REGS",                           \
      "CC_REGS",                                \
      "ALL_REGS" }

#define REG_CLASS_CONTENTS \
{ { 0x00000000 }, /* Empty */			   \
  { 0x0000FFFF },       \
  { 0x00060000 }, 	                   \
  { 0x00020000 }, /* CC */                         \
  { 0x001FFFFF }  /* All registers */              \
}

/* A C expression whose value is a register class containing hard
   register REGNO.  */
#define REGNO_REG_CLASS(R) ((R < nxu16_SP) ? GENERAL_REGS :		\
                            (R == nxu16_CC ? CC_REGS : SPECIAL_REGS))



/* A macro whose definition is the name of the class to which a valid
   base register must belong.  A base register is one used in an
   address which is the register value plus a displacement.  */
#define BASE_REG_CLASS GENERAL_REGS
#define INDEX_REG_CLASS NO_REGS

#define REGNO_OK_FOR_INDEX_P(REGNO) 0
/* A C expression which is nonzero if register number NUM is suitable
   for use as a base register in operand addresses.  */
#ifdef REG_OK_STRICT
#define REGNO_MODE_OK_FOR_BASE_P(REGNO, MODE) \
  nxu16_regno_mode_ok_for_base_p (REGNO, MODE, 1)
#else
#define REGNO_MODE_OK_FOR_BASE_P(REGNO, MODE) \
  nxu16_regno_mode_ok_for_base_p (REGNO, MODE, 0)
#endif

/******************************************************************************
 * Stack and Calling
 ******************************************************************************/

/* Frame Layout */

/* Define this macro if pushing a word onto the stack moves the stack
   pointer to a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

/* Define this macro to nonzero value if the addresses of local variable slots
   are at negative offsets from the frame pointer.  */
#define FRAME_GROWS_DOWNWARD 1

/* Offset from the argument pointer register to the first argument's
   address.  On some machines it may depend on the data type of the
   function.  */
#define FIRST_PARM_OFFSET(F) 0

/* A C expression whose value is RTL representing the location of the
   incoming return address at the beginning of any function, before
   the prologue.  */
#define INCOMING_RETURN_ADDR_RTX					\
    gen_frame_mem (Pmode,                                               \
                   plus_constant (Pmode, stack_pointer_rtx, UNITS_PER_WORD))

/* Exception Handling */

/* Describe how we implement __builtin_eh_return.  */
#define EH_RETURN_DATA_REGNO(N)	((N) < 4 ? (N+2) : INVALID_REGNUM)

/* Store the return handler into the call frame.  */
#define EH_RETURN_HANDLER_RTX						\
    gen_frame_mem (Pmode,                                               \
                   plus_constant (Pmode, frame_pointer_rtx, UNITS_PER_WORD))

/* Frame Registers */
#define STATIC_CHAIN_REGNUM nxu16_ER0//TODO:must must treat serious
/* The register number of the stack pointer register, which must also
   be a fixed register according to `FIXED_REGISTERS'.  */
#define STACK_POINTER_REGNUM nxu16_SP

/* The register number of the frame pointer register, which is used to
   access automatic variables in the stack frame.  */
#define FRAME_POINTER_REGNUM nxu16_FP

/* The register number of the arg pointer register, which is used to
   access the function's argument list.  */
#define ARG_POINTER_REGNUM nxu16_BP


#define HARD_FRAME_POINTER_IS_FRAME_POINTER 0
#define HARD_FRAME_POINTER_IS_ARG_POINTER 0



#define HARD_FRAME_POINTER_REGNUM nxu16_ER14

/* Elimination */

#define ELIMINABLE_REGS							\
{{ ARG_POINTER_REGNUM,   STACK_POINTER_REGNUM},				\
 { ARG_POINTER_REGNUM,   HARD_FRAME_POINTER_REGNUM },				\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},				\
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM },}				\


/* This macro returns the initial difference between the specified pair
   of registers.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
  OFFSET = nxu16_initial_elimination_offset (FROM, TO)

/* Stack Arguments */

/* Define this if it is the responsibility of the caller to allocate
   the area reserved for arguments passed in registers.  */
//#define REG_PARM_STACK_SPACE(FNDECL) (6 * UNITS_PER_WORD)//todo:realize it

/* Define this if the above stack space is to be considered part of the
   space allocated by the caller.  */
//#define OUTGOING_REG_PARM_STACK_SPACE(FNTYPE) 1
//#define STACK_PARMS_IN_REG_PARM_AREA

/* Register Arguments */

typedef struct nxu16_args
{
  /* # Registers available for passing */
  int nregs;

  /* available register number */
  bool regno[FIRST_PSEUDO_REGISTER];

  /* Whether some of the arguments are passed on the stack,
     and hence an arg pointer is needed.  */
  bool has_stack_args;
} CUMULATIVE_ARGS;

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
  nxu16_init_cumulative_args (&(CUM), FNTYPE, LIBNAME, FNDECL)

/* If defined, the maximum amount of space required for outgoing arguments
   will be computed and placed into the variable
   `current_function_outgoing_args_size'.  No space will be pushed
   onto the stack for each call; instead, the function prologue should
   increase the stack frame size by this amount.  */
#define ACCUMULATE_OUTGOING_ARGS 1//I knew its not good for some emmm,but i just follow the document

#define FUNCTION_ARG_REGNO_P(r) nxu16_function_arg_regno_p(r)


/* Function Entry and Exit */

/* Define this macro as a C expression that is nonzero for registers that are
   used by the epilogue or the return pattern.  The stack and frame
   pointer registers are already assumed to be used as needed.  */
#define EPILOGUE_USES(REGNO) nxu16_epilogue_uses(REGNO)

/* Profiling */

/* Generating Code for Profiling */
#define FUNCTION_PROFILER(FILE, LABELNO)  \
  fprintf (FILE, "/* profiler %d */", (LABELNO))

/*ctor TODO:make global support*/
#undef CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP "\t.section .ctors,\"a\",@progbits"

#undef DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP "\t.section .dtors,\"a\",@progbits"

#define TARGET_ASM_CONSTRUCTOR nxu16_asm_out_ctor

#define TARGET_ASM_DESTRUCTOR nxu16_asm_out_dtor

#define SUPPORTS_INIT_PRIORITY 0
//TODO:jump talbe problem
/******************************************************************************
 * Trampolines
 ******************************************************************************/

/* Trampolines for Nested Functions.  */
#define TRAMPOLINE_SIZE 0

/* Alignment required for trampolines, in bits.  */
#define TRAMPOLINE_ALIGNMENT 0


/******************************************************************************
 * Addressing Modes
 ******************************************************************************/

#define HAVE_PRE_INCREMENT 0
#define HAVE_PRE_DECREMENT 0
#define HAVE_POST_INCREMENT 0
#define HAVE_POST_DECREMENT 0
#define HAVE_PRE_MODIFY_DISP 0
#define HAVE_POST_MODIFY_DISP 0
#define HAVE_PRE_MODIFY_REG 0
#define HAVE_POST_MODIFY_REG 0

/* A number, the maximum number of registers that can appear in a
   valid memory address.  */
#define MAX_REGS_PER_ADDRESS 1

#define SLOW_BYTE_ACCESS 0

#define NO_FUNCTION_CSE 1//not call reg except necessary
/******************************************************************************
 * Sections
 ******************************************************************************/

#define TEXT_SECTION_ASM_OP "\t.text"
#define DATA_SECTION_ASM_OP "\t.data"
//#define READONLY_DATA_SECTION_ASM_OP "\t.rodata"
#define BSS_SECTION_ASM_OP "\t.bss"


/******************************************************************************
 * Assembler Format
 ******************************************************************************/

/* File Framework */

#define ASM_COMMENT_START "//"
#define ASM_APP_ON ""
#define ASM_APP_OFF ""
#define FILE_ASM_OP "\t.file\n"

/* Label Output */

#define GLOBAL_ASM_OP ".global\t"

/* Instruction Output */

#define REGISTER_NAMES {                        \
        "r0","r1", "r2","r3","r4","r5", "r6","r7",                 \
        "r8","r9", "r10","r11 ","r12","r13","r14","r15", "sp",                 \
        "psw","lr","fp","bp" }

/* Alignment Output */

#define ASM_OUTPUT_ALIGN(STREAM,POWER)          \
    fprintf (STREAM, "\t.align\t%d\n", POWER);
/*
#define ASM_OUTPUT_SKIP(FILE,SIZE) {}*/

/* Following is an  C statement which is used  to store into
   the string `LABEL',  a label whose name is  made from the
   string `PREFIX' and the number `NUM'.

   This string, when output subsequently by `assemble_name',
   should        produce        the       output        that
   `ASM_OUTPUT_INTERNAL_LABEL'  would produce with  the same
   prefix and num. */
/*#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)   \
    sprintf (LABEL, ".%s%ld", PREFIX, (long) NUM)*/

/* This macro says  that how to output an  assembler line to
   define a local common symbol. */
/*#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGNED)		\
    ( fputs ("\t.comm\t", (FILE)),					\
      assemble_name ((FILE), (NAME)),					\
      fprintf ((FILE), "," HOST_WIDE_INT_PRINT_UNSIGNED",%u\n",	\
               (SIZE), ((ALIGNED) / BITS_PER_UNIT)))*/

/* This macro says  that how to output an  assembler line to
   define a global common symbol. */
#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)                    \
    ( fputs ("\t.comm\t", (FILE)),                                      \
      assemble_name ((FILE), (NAME)),                                   \
      fprintf ((FILE), "," HOST_WIDE_INT_PRINT_UNSIGNED"\n", (SIZE)))


/******************************************************************************
 * Run-time Target
 ******************************************************************************/

#define TARGET_CPU_CPP_BUILTINS() \
  { \
    builtin_define_std ("nxu16");			\
    builtin_define_std ("nxu16");			\
    if (TARGET_LITTLE_ENDIAN)				\
      builtin_define ("__nxu16_LITTLE_ENDIAN__");	\
    else						\
      builtin_define ("__nxu16_BIG_ENDIAN__");		\
  }


/******************************************************************************
 * Misc.
 ******************************************************************************/

#define HAS_LONG_UNCOND_BRANCH true

/* The maximum number of bytes that a single instruction can move
   quickly between memory and registers or between two memory
   locations.  */
#define MOVE_MAX 2

/* All load operations zero extend.  */
#define LOAD_EXTEND_OP(MEM) ZERO_EXTEND//todo:whats this??

/* An alias for the machine mode for pointers.  */
#define Pmode HImode

/* An alias for the machine mode used for memory references to
   functions being called, in `call' RTL expressions.  */
#define FUNCTION_MODE HImode//TODO:May PSI Mode ,make it could be chose

/* An alias for a machine mode name.  This is the machine mode that
   elements of a jump-table should have.  */
#define CASE_VECTOR_MODE HImode

#endif /* GCC_nxu16_H */
