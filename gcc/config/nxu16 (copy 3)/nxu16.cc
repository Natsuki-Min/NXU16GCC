/* Subroutines for insn-output.cc for nxu16 8-bit microcontrollers
   Copyright (C) 1998-2024 Free Software Foundation, Inc.
   Contributed by Denis Chertykov (chertykov@gmail.com)

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.
   
   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

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

/* Maximal allowed offset for an address in the LD command */
#define MAX_LD_OFFSET(MODE) (64 - (signed)GET_MODE_SIZE (MODE))

/* The 4 bits starting at SECTION_MACH_DEP are reserved to store the
   address space where data is to be located.
   As the only non-generic address spaces are all located in flash,
   this can be used to test if data shall go into some .progmem* section.
   This must be the rightmost field of machine dependent section flags.  */
#define nxu16_SECTION_PROGMEM (0xf * SECTION_MACH_DEP)

/* Similar 4-bit region for SYMBOL_REF_FLAGS.  */
#define nxu16_SYMBOL_FLAG_PROGMEM (0xf * SYMBOL_FLAG_MACH_DEP)

/* Similar 4-bit region in SYMBOL_REF_FLAGS:
   Set address-space AS in SYMBOL_REF_FLAGS of SYM  */
#define nxu16_SYMBOL_SET_ADDR_SPACE(SYM,AS)                       \
  do {                                                          \
    SYMBOL_REF_FLAGS (sym) &= ~nxu16_SYMBOL_FLAG_PROGMEM;         \
    SYMBOL_REF_FLAGS (sym) |= (AS) * SYMBOL_FLAG_MACH_DEP;      \
  } while (0)

/* Read address-space from SYMBOL_REF_FLAGS of SYM  */
#define nxu16_SYMBOL_GET_ADDR_SPACE(SYM)                          \
  ((SYMBOL_REF_FLAGS (sym) & nxu16_SYMBOL_FLAG_PROGMEM)           \
   / SYMBOL_FLAG_MACH_DEP)

/* (nxu16_TINY only): Symbol has attribute progmem */
#define nxu16_SYMBOL_FLAG_TINY_PM \
  (SYMBOL_FLAG_MACH_DEP << 7)

/* (nxu16_TINY only): Symbol has attribute absdata */
#define nxu16_SYMBOL_FLAG_TINY_ABSDATA \
  (SYMBOL_FLAG_MACH_DEP << 8)

#define TINY_ADIW(REG1, REG2, I)                                \
    "subi " #REG1 ",lo8(-(" #I "))" CR_TAB                      \
    "sbci " #REG2 ",hi8(-(" #I "))"

#define TINY_SBIW(REG1, REG2, I)                                \
    "subi " #REG1 ",lo8((" #I "))" CR_TAB                       \
    "sbci " #REG2 ",hi8((" #I "))"

#define nxu16_TMP_REGNO (nxu16_TINY ? TMP_REGNO_TINY : TMP_REGNO)
#define nxu16_ZERO_REGNO (nxu16_TINY ? ZERO_REGNO_TINY : ZERO_REGNO)

/* Known address spaces.  The order must be the same as in the respective
   enum from nxu16.h (or designated initialized must be used).  */
const nxu16_addrspace_t nxu16_addrspace[ADDR_SPACE_COUNT] =
{
  { ADDR_SPACE_RAM,  0, 2, "", 0, NULL },
  { ADDR_SPACE_FLASH,  1, 2, "__flash",   0, ".progmem.data" },
  { ADDR_SPACE_FLASH1, 1, 2, "__flash1",  1, ".progmem1.data" },
  { ADDR_SPACE_FLASH2, 1, 2, "__flash2",  2, ".progmem2.data" },
  { ADDR_SPACE_FLASH3, 1, 2, "__flash3",  3, ".progmem3.data" },
  { ADDR_SPACE_FLASH4, 1, 2, "__flash4",  4, ".progmem4.data" },
  { ADDR_SPACE_FLASH5, 1, 2, "__flash5",  5, ".progmem5.data" },
  { ADDR_SPACE_MEMX, 1, 3, "__memx",  0, ".progmemx.data" },
};


/* Holding RAM addresses of some SFRs used by the compiler and that
   are unique over all devices in an architecture like 'nxu164'.  */

typedef struct
{
  /* SREG: The processor status */
  int sreg;

  /* RAMPX, RAMPY, RAMPD and CCP of XMEGA */
  int ccp;
  int rampd;
  int rampx;
  int rampy;

  /* RAMPZ: The high byte of 24-bit address used with ELPM */
  int rampz;

  /* SP: The stack pointer and its low and high byte */
  int sp_l;
  int sp_h;
} nxu16_addr_t;

static nxu16_addr_t nxu16_addr;


/* Prototypes for local helper functions.  */

static const char *out_movqi_r_mr (rtx_insn *, rtx[], int *);
static const char *out_movhi_r_mr (rtx_insn *, rtx[], int *);
static const char *out_movsi_r_mr (rtx_insn *, rtx[], int *);
static const char *out_movqi_mr_r (rtx_insn *, rtx[], int *);
static const char *out_movhi_mr_r (rtx_insn *, rtx[], int *);
static const char *out_movsi_mr_r (rtx_insn *, rtx[], int *);


/* Prototypes for hook implementors if needed before their implementation.  */

static bool nxu16_rtx_costs (rtx, machine_mode, int, int, int *, bool);


/* Allocate registers from r25 to r8 for parameters for function calls
   resp. r25 to r20 for reduced Tiny.  */
#define FIRST_CUM_REG REG_26

/* Last call saved register */
#define LAST_CALLEE_SAVED_REG (nxu16_TINY ? REG_19 : REG_17)

/* Implicit target register of LPM instruction (R0) */
extern GTY(()) rtx lpm_reg_rtx;
rtx lpm_reg_rtx;

/* (Implicit) address register of LPM instruction (R31:R30 = Z) */
extern GTY(()) rtx lpm_addr_reg_rtx;
rtx lpm_addr_reg_rtx;

/* Temporary register RTX (reg:QI TMP_REGNO) */
extern GTY(()) rtx tmp_reg_rtx;
rtx tmp_reg_rtx;

/* Zeroed register RTX (reg:QI ZERO_REGNO) */
extern GTY(()) rtx zero_reg_rtx;
rtx zero_reg_rtx;

/* Condition Code register RTX (reg:CC REG_CC) */
extern GTY(()) rtx cc_reg_rtx;
extern GTY(()) rtx ccn_reg_rtx;
extern GTY(()) rtx cczn_reg_rtx;
rtx cc_reg_rtx;
rtx ccn_reg_rtx;
rtx cczn_reg_rtx;

/* RTXs for all general purpose registers as QImode */
extern GTY(()) rtx all_regs_rtx[REG_32];
rtx all_regs_rtx[REG_32];

/* SREG, the processor status */
extern GTY(()) rtx sreg_rtx;
rtx sreg_rtx;

/* RAMP* special function registers */
extern GTY(()) rtx rampd_rtx;
extern GTY(()) rtx rampx_rtx;
extern GTY(()) rtx rampy_rtx;
extern GTY(()) rtx rampz_rtx;
rtx rampd_rtx;
rtx rampx_rtx;
rtx rampy_rtx;
rtx rampz_rtx;

/* RTX containing the strings "" and "e", respectively */
static GTY(()) rtx xstring_empty;
static GTY(()) rtx xstring_e;

/* Current architecture.  */
const nxu16_arch_t *nxu16_arch;
nxu16_arch_id nxu16_arch_index;

/* Unnamed sections associated to __attribute__((progmem)) aka. PROGMEM
   or to address space __flash* or __memx.  Only used as singletons inside
   nxu16_asm_select_section, but it must not be local there because of GTY.  */
static GTY(()) section *progmem_section[ADDR_SPACE_COUNT];

/* Condition for insns/expanders from nxu16-dimode.md.  */
bool nxu16_have_dimode = true;

/* To track if code will use .bss, .data, .rodata.  */
bool nxu16_need_clear_bss_p = false;
bool nxu16_need_copy_data_p = false;
bool nxu16_has_rodata_p = false;


/* Transform UP into lowercase and write the result to LO.
   You must provide enough space for LO.  Return LO.  */

static char *
nxu16_tolower (char *lo, const char *up)
{
  char *lo0 = lo;

  for (; *up; up++, lo++)
    *lo = TOLOWER (*up);

  *lo = '\0';

  return lo0;
}


/* Return chunk of mode MODE of X as an rtx.  N specifies the subreg
   byte at which the chunk starts.  N must be an integral multiple
   of the mode size.  */

static rtx
nxu16_chunk (machine_mode mode, rtx x, int n)
{
  gcc_assert (n % GET_MODE_SIZE (mode) == 0);
  machine_mode xmode = GET_MODE (x) == VOIDmode ? DImode : GET_MODE (x);
  return simplify_gen_subreg (mode, x, xmode, n);
}


/* Return the N-th byte of X as an rtx.  */

static rtx
nxu16_byte (rtx x, int n)
{
  return nxu16_chunk (QImode, x, n);
}


/* Return the sub-word of X starting at byte number N.  */

static rtx
nxu16_word (rtx x, int n)
{
  return nxu16_chunk (HImode, x, n);
}


/* Return the N-th byte of compile-time constant X as an int8_t.  */

static int8_t
nxu16_int8 (rtx x, int n)
{
  gcc_assert (CONST_INT_P (x) || CONST_FIXED_P (x) || CONST_DOUBLE_P (x));

  return (int8_t) trunc_int_for_mode (INTVAL (nxu16_byte (x, n)), QImode);
}

/* Return the N-th byte of compile-time constant X as an uint8_t.  */

static uint8_t
nxu16_uint8 (rtx x, int n)
{
  return (uint8_t) nxu16_int8 (x, n);
}


/* Return the sub-word of compile-time constant X that starts
   at byte N as an int16_t.  */

static int16_t
nxu16_int16 (rtx x, int n)
{
  gcc_assert (CONST_INT_P (x) || CONST_FIXED_P (x) || CONST_DOUBLE_P (x));

  return (int16_t) trunc_int_for_mode (INTVAL (nxu16_word (x, n)), HImode);
}

/* Return the sub-word of compile-time constant X that starts
   at byte N as an uint16_t.  */

static uint16_t
nxu16_uint16 (rtx x, int n)
{
  return (uint16_t) nxu16_int16 (x, n);
}


/* Constraint helper function.  XVAL is a CONST_INT or a CONST_DOUBLE.
   Return true if the least significant N_BYTES bytes of XVAL all have a
   popcount in POP_MASK and false, otherwise.  POP_MASK represents a subset
   of integers which contains an integer N iff bit N of POP_MASK is set.  */

bool
nxu16_popcount_each_byte (rtx xval, int n_bytes, int pop_mask)
{
  for (int i = 0; i < n_bytes; i++)
    {
      unsigned int val8 = nxu16_uint8 (xval, i);

      if ((pop_mask & (1 << popcount_hwi (val8))) == 0)
	return false;
    }

  return true;
}


/* Constraint helper function.  XVAL is a CONST_INT.  Return true if we
   can perform XOR without a clobber reg, provided the operation is on
   a d-register.  This means each byte is in { 0, 0xff, 0x80 }.  */

bool
nxu16_xor_noclobber_dconst (rtx xval, int n_bytes)
{
  for (int i = 0; i < n_bytes; ++i)
    {
      unsigned int val8 = nxu16_uint8 (xval, i);

      if (val8 != 0 && val8 != 0xff && val8 != 0x80)
	return false;
    }

  return true;
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


/* Return true if hard register REG supports the ADIW and SBIW instructions.  */

bool
nxu16_adiw_reg_p (rtx reg)
{
  return (nxu16_HAVE_ADIW
	  && test_hard_reg_class (ADDW_REGS, reg));
}


static bool
ra_in_progress ()
{
  return nxu16_lra_p ? lra_in_progress : reload_in_progress;
}


/* Set `nxu16_arch' as specified by `-mmcu='.
   Return true on success.  */

static bool
nxu16_set_core_architecture (void)
{
  /* Search for mcu core architecture.  */

  if (!nxu16_mmcu)
    nxu16_mmcu = nxu16_MMCU_DEFAULT;

  nxu16_arch = &nxu16_arch_types[0];

  for (const nxu16_mcu_t *mcu = nxu16_mcu_types; ; mcu++)
    {
      if (mcu->name == NULL)
	{
	  /* Reached the end of `nxu16_mcu_types'.  This should actually never
	     happen as options are provided by device-specs.  It could be a
	     typo in a device-specs or calling the compiler proper directly
	     with -mmcu=<device>. */

	  error ("unknown core architecture %qs specified with %qs",
		 nxu16_mmcu, "-mmcu=");
	  nxu16_inform_core_architectures ();
	  break;
	}
      else if (strcmp (mcu->name, nxu16_mmcu) == 0
	       // Is this a proper architecture ?
	       && mcu->macro == NULL)
	{
	  nxu16_arch = &nxu16_arch_types[mcu->arch_id];
	  nxu16_arch_index = mcu->arch_id;
	  if (nxu16_n_flash < 0)
	    nxu16_n_flash = 1 + (mcu->flash_size - 1) / 0x10000;

	  return true;
	}
    }

  return false;
}


/* Implement `TARGET_OPTION_OVERRIDE'.  */

static void
nxu16_option_override (void)
{
  /* caller-save.cc looks for call-clobbered hard registers that are assigned
     to pseudos that cross calls and tries so save-restore them around calls
     in order to reduce the number of stack slots needed.

     This might lead to situations where reload is no more able to cope
     with the challenge of nxu16's very few address registers and fails to
     perform the requested spills.  */

  if (nxu16_strict_X)
    flag_caller_saves = 0;

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

#if !defined (HAVE_AS_nxu16_MGCCISR_OPTION)
  nxu16_gasisr_prologues = 0;
#endif

  if (!nxu16_set_core_architecture())
    return;

  /* Sould be set by nxu16-common.cc */
  gcc_assert (nxu16_long_double >= nxu16_double && nxu16_double >= 32);

  /* RAM addresses of some SFRs common to all devices in respective arch. */

  /* SREG: Status Register containing flags like I (global IRQ) */
  nxu16_addr.sreg = 0x3F + nxu16_arch->sfr_offset;

  /* RAMPZ: Address' high part when loading via ELPM */
  nxu16_addr.rampz = 0x3B + nxu16_arch->sfr_offset;

  nxu16_addr.rampy = 0x3A + nxu16_arch->sfr_offset;
  nxu16_addr.rampx = 0x39 + nxu16_arch->sfr_offset;
  nxu16_addr.rampd = 0x38 + nxu16_arch->sfr_offset;
  nxu16_addr.ccp = (nxu16_TINY ? 0x3C : 0x34) + nxu16_arch->sfr_offset;

  /* SP: Stack Pointer (SP_H:SP_L) */
  nxu16_addr.sp_l = 0x3D + nxu16_arch->sfr_offset;
  nxu16_addr.sp_h = nxu16_addr.sp_l + 1;

  init_machine_status = []()
  {
    return ggc_cleared_alloc<machine_function> ();
  };

  nxu16_log_set_nxu16_log();

  /* As long as peep2_rescan is not implemented, see
     http://gcc.gnu.org/ml/gcc-patches/2011-10/msg02819.html
     we add a second peephole2 run to get best results.  */
  {
    opt_pass *extra_peephole2
      = g->get_passes ()->get_pass_peephole2 ()->clone ();
    register_pass_info peep2_2_info
      = { extra_peephole2, "peephole2", 1, PASS_POS_INSERT_AFTER };

    register_pass (&peep2_2_info);
  }
}


/* Implement `INIT_EXPANDERS'.  */
/* The function works like a singleton.  */

void
nxu16_init_expanders (void)
{
  for (int regno = REG_0; regno < REG_32; regno ++)
    all_regs_rtx[regno] = gen_rtx_REG (QImode, regno);

  lpm_reg_rtx  = all_regs_rtx[LPM_REGNO];
  //tmp_reg_rtx  = all_regs_rtx[nxu16_TMP_REGNO];
  //zero_reg_rtx = all_regs_rtx[nxu16_ZERO_REGNO];

  cc_reg_rtx = gen_rtx_REG (CCmode, REG_CC);
  ccn_reg_rtx = gen_rtx_REG (CCNmode, REG_CC);
  cczn_reg_rtx = gen_rtx_REG (CCZNmode, REG_CC);

  lpm_addr_reg_rtx = gen_rtx_REG (HImode, REG_Z);

  sreg_rtx = gen_rtx_MEM (QImode, GEN_INT (nxu16_addr.sreg));
  rampd_rtx = gen_rtx_MEM (QImode, GEN_INT (nxu16_addr.rampd));
  rampx_rtx = gen_rtx_MEM (QImode, GEN_INT (nxu16_addr.rampx));
  rampy_rtx = gen_rtx_MEM (QImode, GEN_INT (nxu16_addr.rampy));
  rampz_rtx = gen_rtx_MEM (QImode, GEN_INT (nxu16_addr.rampz));

  xstring_empty = gen_rtx_CONST_STRING (VOIDmode, "");
  xstring_e = gen_rtx_CONST_STRING (VOIDmode, "e");

  /* TINY core does not have regs r10-r16, but nxu16-dimode.md expects them
     to be present */
  if (nxu16_TINY)
    nxu16_have_dimode = false;
}


/* Implement `REGNO_REG_CLASS'.  */
/* Return register class for register R.  */

reg_class
nxu16_regno_reg_class (int r)
{
  static const reg_class reg_class_tab[] =
    {
      R0_REG,
      /* r1 - r15 */
      NO_LD_REGS, NO_LD_REGS, NO_LD_REGS,
      NO_LD_REGS, NO_LD_REGS, NO_LD_REGS, NO_LD_REGS,
      NO_LD_REGS, NO_LD_REGS, NO_LD_REGS, NO_LD_REGS,
      NO_LD_REGS, NO_LD_REGS, NO_LD_REGS, NO_LD_REGS,
      /* r16 - r23 */
      SIMPLE_LD_REGS, SIMPLE_LD_REGS, SIMPLE_LD_REGS, SIMPLE_LD_REGS,
      SIMPLE_LD_REGS, SIMPLE_LD_REGS, SIMPLE_LD_REGS, SIMPLE_LD_REGS,
      /* r24, r25 */
      ADDW_REGS, ADDW_REGS,
      /* X: r26, 27 */
      POINTER_X_REGS, POINTER_X_REGS,
      /* Y: r28, r29 */
      POINTER_Y_REGS, POINTER_Y_REGS,
      /* Z: r30, r31 */
      POINTER_Z_REGS, POINTER_Z_REGS,
      /* SP: SPL, SPH */
      STACK_REG, STACK_REG
    };

  if (r <= 33)
    return reg_class_tab[r];

  if (r == REG_CC)
    return CC_REG;

  return ALL_REGS;
}


/* Implement `TARGET_SCALAR_MODE_SUPPORTED_P'.  */

static bool
nxu16_scalar_mode_supported_p (scalar_mode mode)
{
  if (ALL_FIXED_POINT_MODE_P (mode))
    return true;

  if (PSImode == mode)
    return true;

  return default_scalar_mode_supported_p (mode);
}


/* Return TRUE if DECL is a VAR_DECL located in flash and FALSE, otherwise.  */

static bool
nxu16_decl_flash_p (tree decl)
{
  if (TREE_CODE (decl) != VAR_DECL
      || TREE_TYPE (decl) == error_mark_node)
    {
      return false;
    }

  return !ADDR_SPACE_GENERIC_P (TYPE_ADDR_SPACE (TREE_TYPE (decl)));
}


/* Return TRUE if DECL is a VAR_DECL located in the 24-bit flash
   address space and FALSE, otherwise.  */

static bool
nxu16_decl_memx_p (tree decl)
{
  if (TREE_CODE (decl) != VAR_DECL
      || TREE_TYPE (decl) == error_mark_node)
    {
      return false;
    }

  return (ADDR_SPACE_MEMX == TYPE_ADDR_SPACE (TREE_TYPE (decl)));
}


/* Return TRUE if X is a MEM rtx located in flash and FALSE, otherwise.  */

bool
nxu16_mem_flash_p (rtx x)
{
  return (MEM_P (x)
	  && !ADDR_SPACE_GENERIC_P (MEM_ADDR_SPACE (x)));
}


/* Return TRUE if X is a MEM rtx located in the 24-bit flash
   address space and FALSE, otherwise.  */

bool
nxu16_mem_memx_p (rtx x)
{
  return (MEM_P (x)
	  && ADDR_SPACE_MEMX == MEM_ADDR_SPACE (x));
}


/* A helper for the subsequent function attribute used to dig for
   attribute 'name' in a FUNCTION_DECL or FUNCTION_TYPE */

static inline bool
nxu16_lookup_function_attribute1 (const_tree func, const char *name)
{
  if (FUNCTION_DECL == TREE_CODE (func))
    {
      if (NULL_TREE != lookup_attribute (name, DECL_ATTRIBUTES (func)))
	{
	  return true;
	}

      func = TREE_TYPE (func);
    }

  gcc_assert (FUNC_OR_METHOD_TYPE_P (func));

  return NULL_TREE != lookup_attribute (name, TYPE_ATTRIBUTES (func));
}


/* Call WORKER on all NAME attributes of function FUNC.  */

static void
nxu16_foreach_function_attribute (tree func, const char *name,
				void (*worker) (tree, tree, void *),
				void *cookie)
{
  tree attrs = NULL_TREE;

  if (TREE_CODE (func) == FUNCTION_DECL)
    attrs = DECL_ATTRIBUTES (func);
  else if (FUNC_OR_METHOD_TYPE_P (func))
    attrs = TYPE_ATTRIBUTES (TREE_TYPE (func));

  while (attrs)
    {
      attrs = lookup_attribute (name, attrs);
      if (attrs)
	{
	  worker (func, attrs, cookie);
	  attrs = TREE_CHAIN (attrs);
	}
    }
}


/* Return nonzero if FUNC is a naked function.  */

static bool
nxu16_naked_function_p (tree func)
{
  return nxu16_lookup_function_attribute1 (func, "naked");
}

/* Return nonzero if FUNC is a noblock function.  */

static bool
nxu16_noblock_function_p (tree func)
{
  return nxu16_lookup_function_attribute1 (func, "noblock");
}

/* Return 1 if FUNC is a function that has a "ATTR_NAME" attribute
   (and perhaps also "ATTR_NAME(num)" attributes.  Return -1 if FUNC has
   "ATTR_NAME(num)" attribute(s) but no "ATTR_NAME" attribute.
   When no form of ATTR_NAME is present, return 0.  */

static int
nxu16_interrupt_signal_function (tree func, const char *attr_name)
{
  int res = 0;

  nxu16_foreach_function_attribute (func, attr_name,
    [] (tree, tree attr, void *cookie)
    {
      int *pcook = (int *) cookie;

      *pcook = TREE_VALUE (attr)
	? *pcook ? *pcook : -1
	: 1;
    }, &res);

  return res;
}


/* Return 1 if FUNC is an interrupt function that has an "interrupt" attribute
   (and perhaps also "interrupt(num)" attributes.  Return -1 if FUNC has
   "interrupt(num)" attribute(s) but no "interrupt" attribute.  */

static int
nxu16_interrupt_function (tree func)
{
  return nxu16_interrupt_signal_function (func, "interrupt");
}

/* Return 1 if FUNC is a signal function that has a "signal" attribute
   (and perhaps also "signal(num)" attributes.  Return -1 if FUNC has
   "signal(num)" attribute(s) but no "signal" attribute.  */

static int
nxu16_signal_function (tree func)
{
  return nxu16_interrupt_signal_function (func, "signal");
}

/* Return nonzero if FUNC is an OS_task function.  */

static bool
nxu16_OS_task_function_p (tree func)
{
  return nxu16_lookup_function_attribute1 (func, "OS_task");
}

/* Return nonzero if FUNC is an OS_main function.  */

static bool
nxu16_OS_main_function_p (tree func)
{
  return nxu16_lookup_function_attribute1 (func, "OS_main");
}


/* Return nonzero if FUNC is a no_gccisr function as specified
   by the "no_gccisr" attribute.  */

static bool
nxu16_no_gccisr_function_p (tree func)
{
  return nxu16_lookup_function_attribute1 (func, "no_gccisr");
}


/* Implement `TARGET_CAN_INLINE_P'.  */
/* Some options like -mgas_isr_prologues depend on optimization level,
   and the inliner might think that due to different options, inlining
   is not permitted; see PR104327.  */

static bool
nxu16_can_inline_p (tree /* caller */, tree /* callee */)
{
  // No restrictions whatsoever.
  return true;
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

  cfun->machine->is_naked = nxu16_naked_function_p (decl);
  cfun->machine->is_signal = nxu16_signal_function (decl);
  cfun->machine->is_interrupt = nxu16_interrupt_function (decl);
  cfun->machine->is_noblock = nxu16_noblock_function_p (decl);
  cfun->machine->is_OS_task = nxu16_OS_task_function_p (decl);
  cfun->machine->is_OS_main = nxu16_OS_main_function_p (decl);
  cfun->machine->is_no_gccisr = nxu16_no_gccisr_function_p (decl);

  const char *isr = cfun->machine->is_interrupt ? "interrupt" : "signal";

  /* Too much attributes make no sense as they request conflicting features. */

  if (cfun->machine->is_OS_task
      && (cfun->machine->is_signal || cfun->machine->is_interrupt))
    error_at (loc, "function attributes %qs and %qs are mutually exclusive",
	      "OS_task", isr);

  if (cfun->machine->is_OS_main
      && (cfun->machine->is_signal || cfun->machine->is_interrupt))
    error_at (loc, "function attributes %qs and %qs are mutually exclusive",
	      "OS_main", isr);

  if (cfun->machine->is_interrupt || cfun->machine->is_signal)
    {
      tree args = TYPE_ARG_TYPES (TREE_TYPE (decl));
      tree ret = TREE_TYPE (TREE_TYPE (decl));
      const char *name;

      name = DECL_ASSEMBLER_NAME_SET_P (decl)
	? IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl))
	: IDENTIFIER_POINTER (DECL_NAME (decl));

      /* Skip a leading '*' that might still prefix the assembler name,
	 e.g. in non-LTO runs.  */

      name = default_strip_name_encoding (name);

      /* Interrupt handlers must be  void __vector (void)  functions.  */

      if (args && TREE_CODE (TREE_VALUE (args)) != VOID_TYPE)
	{
	  error_at (loc, "%qs function cannot have arguments", isr);
	  if (TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE)
	    inform (loc, "method %qs has an implicit %<this%> argument", name);
	}

      if (TREE_CODE (ret) != VOID_TYPE)
	error_at (loc, "%qs function cannot return a value", isr);

#if defined WITH_nxu16LIBC
      /* If the function has the 'signal' or 'interrupt' attribute, ensure
	 that the name of the function is "__vector_NN" so as to catch
	 when the user misspells the vector name.  This check is only
	 required when the "interrupt" resp. "signal" attribute does not
	 have an IRQ-number argument.  */

      if (!startswith (name, "__vector")
	  && (cfun->machine->is_interrupt == 1
	      || cfun->machine->is_signal == 1))
	warning_at (loc, OPT_Wmisspelled_isr, "%qs appears to be a misspelled "
		    "%qs handler, missing %<__vector%> prefix", name, isr);
#endif // nxu16-LibC naming conventions
    }
  else if (cfun->machine->is_noblock)
    {
      warning (OPT_Wattributes, "%qs attribute ignored on non-ISR function",
	       "noblock");
    }

#if defined WITH_nxu16LIBC
  // Common problem is using "ISR" without first including nxu16/interrupt.h.
  const char *name = IDENTIFIER_POINTER (DECL_NAME (decl));
  name = default_strip_name_encoding (name);
  if (strcmp ("ISR", name) == 0)
    {
      warning_at (loc, OPT_Wmisspelled_isr, "%qs is a reserved identifier"
		  " in nxu16-LibC.  Consider %<#include <nxu16/interrupt.h>%>"
		  " before using the %qs macro", name, name);
    }
  if (strcmp ("INTERRUPT", name) == 0
      || strcmp ("SIGNAL", name) == 0)
    {
      warning_at (loc, OPT_Wmisspelled_isr, "%qs is a deprecated identifier"
		  " in nxu16-LibC.  Consider %<#include <nxu16/interrupt.h>%>"
		  " or %<#include <compat/deprecated.h>%>"
		  " before using the %qs macro", name, name);
    }
#endif // nxu16-LibC naming conventions

  /* Don't print the above diagnostics more than once.  */

  cfun->machine->attributes_checked_p = 1;
}


/* Implement `ACCUMULATE_OUTGOING_ARGS'.  */

int
nxu16_accumulate_outgoing_args (void)
{
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


/* Report contribution of accumulated outgoing arguments to stack size.  */

static inline int
nxu16_outgoing_args_size (void)
{
  return (ACCUMULATE_OUTGOING_ARGS
	  ? (HOST_WIDE_INT) crtl->outgoing_args_size : 0);
}


/* Implement `TARGET_STARTING_FRAME_OFFSET'.  */
/* This is the offset from the frame pointer register to the first stack slot
   that contains a variable living in the frame.  */

static HOST_WIDE_INT
nxu16_starting_frame_offset (void)
{
  return 1 + nxu16_outgoing_args_size ();
}


/* Return the number of hard registers to push/pop in the prologue/epilogue
   of the current function, and optionally store these registers in SET.  */

static int
nxu16_regs_to_save (HARD_REG_SET *set)
{
  int count = 0;
  int int_or_sig_p = cfun->machine->is_interrupt || cfun->machine->is_signal;

  if (set)
    CLEAR_HARD_REG_SET (*set);

  /* No need to save any registers if the function never returns or
     has the "OS_task" or "OS_main" attribute.  */

  if (TREE_THIS_VOLATILE (current_function_decl)
      || cfun->machine->is_OS_task
      || cfun->machine->is_OS_main)
    return 0;

  for (int reg = REG_0; reg < REG_32; reg++)
    {
      /* Do not push/pop __tmp_reg__, __zero_reg__, as well as
	 any global register variables.  */

      if (fixed_regs[reg])
	continue;

      if ((int_or_sig_p && !crtl->is_leaf && call_used_or_fixed_reg_p (reg))
	  || (df_regs_ever_live_p (reg)
	      && (int_or_sig_p || !call_used_or_fixed_reg_p (reg))
	      /* Don't record frame pointer registers here.  They are treated
		 indivitually in prologue.  */
	      && !(frame_pointer_needed
		   && (reg == REG_Y || reg == REG_Y + 1))))
	{
	  if (set)
	    SET_HARD_REG_BIT (*set, reg);
	  count++;
	}
    }
  return count;
}






/* Implement `TARGET_WARN_FUNC_RETURN'.  */

static bool
nxu16_warn_func_return (tree decl)
{
  /* Naked functions are implemented entirely in assembly, including the
     return sequence, so suppress warnings about this.  */

  return !nxu16_naked_function_p (decl);
}


/* Worker function for `INITIAL_ELIMINATION_OFFSET'.  */
/* Compute offset between arg_pointer and frame_pointer.  */

int
nxu16_initial_elimination_offset (int from, int to)
{
  if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
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
    }
}


/* Helper for the function below.  */

static void
nxu16_adjust_type_node (tree *node, machine_mode mode, int sat_p)
{
  *node = make_node (FIXED_POINT_TYPE);
  TYPE_SATURATING (*node) = sat_p;
  TYPE_UNSIGNED (*node) = UNSIGNED_FIXED_POINT_MODE_P (mode);
  TYPE_IBIT (*node) = GET_MODE_IBIT (mode);
  TYPE_FBIT (*node) = GET_MODE_FBIT (mode);
  TYPE_PRECISION (*node) = GET_MODE_BITSIZE (mode);
  SET_TYPE_ALIGN (*node, 8);
  SET_TYPE_MODE (*node, mode);

  layout_type (*node);
}


/* Implement `TARGET_BUILD_BUILTIN_VA_LIST'.  */

static tree
nxu16_build_builtin_va_list (void)
{
  /* nxu16-modes.def adjusts [U]TA to be 64-bit modes with 48 fractional bits.
     This is more appropriate for the 8-bit machine nxu16 than 128-bit modes.
     The ADJUST_IBIT/FBIT are handled in toplev:init_adjust_machine_modes()
     which is auto-generated by genmodes, but the compiler assigns [U]DAmode
     to the long long accum modes instead of the desired [U]TAmode.

     Fix this now, right after node setup in tree.cc:build_common_tree_nodes().
     This must run before c-cppbuiltin.cc:builtin_define_fixed_point_constants()
     which built-in defines macros like __ULLACCUM_FBIT__ that are used by
     libgcc to detect IBIT and FBIT.  */

  nxu16_adjust_type_node (&ta_type_node, TAmode, 0);
  nxu16_adjust_type_node (&uta_type_node, UTAmode, 0);
  nxu16_adjust_type_node (&sat_ta_type_node, TAmode, 1);
  nxu16_adjust_type_node (&sat_uta_type_node, UTAmode, 1);

  unsigned_long_long_accum_type_node = uta_type_node;
  long_long_accum_type_node = ta_type_node;
  sat_unsigned_long_long_accum_type_node = sat_uta_type_node;
  sat_long_long_accum_type_node = sat_ta_type_node;

  /* Dispatch to the default handler.  */

  return std_build_builtin_va_list ();
}


/* Worker function for `INCOMING_RETURN_ADDR_RTX'.  */
/* Return contents of MEM at frame pointer + stack size + 1 (+2 if 3-byte PC).
   This is return address of function.  */

rtx
nxu16_return_addr_rtx (int count, rtx tem)
{
  rtx r;

  /* Can only return this function's return address. Others not supported.  */
  if (count)
    return NULL;

  if (nxu16_3_BYTE_PC)
    {
      r = gen_rtx_SYMBOL_REF (Pmode, ".L__stack_usage+2");
      warning (0, "%<builtin_return_address%> contains only 2 bytes"
	       " of address");
    }
  else
    r = gen_rtx_SYMBOL_REF (Pmode, ".L__stack_usage+1");

  cfun->machine->use_L__stack_usage = 1;

  r = gen_rtx_PLUS (Pmode, tem, r);
  r = gen_frame_mem (Pmode, memory_address (Pmode, r));
  r = gen_rtx_ROTATE (HImode, r, GEN_INT (8));
  return r;
}

/* Return 1 if the function epilogue is just a single "ret".  */

int
nxu16_simple_epilogue (void)
{
  return (! frame_pointer_needed
	  && get_frame_size () == 0
	  && nxu16_outgoing_args_size() == 0
	  && nxu16_regs_to_save (NULL) == 0
	  && ! cfun->machine->is_interrupt
	  && ! cfun->machine->is_signal
	  && ! cfun->machine->is_naked
	  && ! TREE_THIS_VOLATILE (current_function_decl));
}

/* This function checks sequence of live registers.  */

static int
sequent_regs_live (void)
{
  int live_seq = 0;
  int cur_seq = 0;

  for (int reg = 0; reg <= LAST_CALLEE_SAVED_REG; ++reg)
    {
      if (fixed_regs[reg])
	{
	  /* Don't recognize sequences that contain global register
	     variables.  */

	  if (live_seq != 0)
	    return 0;
	  else
	    continue;
	}

      if (!call_used_or_fixed_reg_p (reg))
	{
	  if (df_regs_ever_live_p (reg))
	    {
	      ++live_seq;
	      ++cur_seq;
	    }
	  else
	    cur_seq = 0;
	}
    }

  if (!frame_pointer_needed)
    {
      if (df_regs_ever_live_p (REG_Y))
	{
	  ++live_seq;
	  ++cur_seq;
	}
      else
	cur_seq = 0;

      if (df_regs_ever_live_p (REG_Y + 1))
	{
	  ++live_seq;
	  ++cur_seq;
	}
      else
	cur_seq = 0;
    }
  else
    {
      cur_seq += 2;
      live_seq += 2;
    }
  return (cur_seq == live_seq) ? live_seq : 0;
}


/* Obtain the length sequence of insns.  */

static int
get_sequence_length (rtx_insn *insns)
{
  int length = 0;

  for (rtx_insn *insn = insns; insn; insn = NEXT_INSN (insn))
    length += get_attr_length (insn);

  return length;
}


/*  Implement `INCOMING_RETURN_ADDR_RTX'.  */

rtx
nxu16_incoming_return_addr_rtx (void)
{
  /* The return address is at the top of the stack.  Note that the push
     was via post-decrement, which means the actual address is off by one.  */
  return gen_frame_mem (HImode, plus_constant (Pmode, stack_pointer_rtx, 1));
}


/* Unset a bit in *SET.  If successful, return the respective bit number.
   Otherwise, return -1 and *SET is unaltered.  */

static int
nxu16_hregs_split_reg (HARD_REG_SET *set)
{
  for (int regno = REG_0; regno < REG_32; regno++)
    if (TEST_HARD_REG_BIT (*set, regno))
      {
	// Don't remove a register from *SET which might indicate that
	// some RAMP* register might need ISR prologue / epilogue treatment.

	if (nxu16_HAVE_RAMPX
	    && (REG_X == regno || REG_X + 1 == regno)
	    && TEST_HARD_REG_BIT (*set, REG_X)
	    && TEST_HARD_REG_BIT (*set, REG_X + 1))
	  continue;

	if (nxu16_HAVE_RAMPY
	    && !frame_pointer_needed
	    && (REG_Y == regno || REG_Y + 1 == regno)
	    && TEST_HARD_REG_BIT (*set, REG_Y)
	    && TEST_HARD_REG_BIT (*set, REG_Y + 1))
	  continue;

	if (nxu16_HAVE_RAMPZ
	    && (REG_Z == regno || REG_Z + 1 == regno)
	    && TEST_HARD_REG_BIT (*set, REG_Z)
	    && TEST_HARD_REG_BIT (*set, REG_Z + 1))
	  continue;

	CLEAR_HARD_REG_BIT (*set, regno);
	return regno;
      }

  return -1;
}


/*  Helper for expand_prologue.  Emit a push of a byte register.  */

static void
emit_push_byte (unsigned regno, bool frame_related_p)
{
  rtx mem, reg;
  rtx_insn *insn;

  mem = gen_rtx_POST_DEC (HImode, stack_pointer_rtx);
  mem = gen_frame_mem (QImode, mem);
  reg = gen_rtx_REG (QImode, regno);

  insn = emit_insn (gen_rtx_SET (mem, reg));
  if (frame_related_p)
    RTX_FRAME_RELATED_P (insn) = 1;

  cfun->machine->stack_usage++;
}


/*  Helper for expand_prologue.  Emit a push of a SFR via register TREG.
    SFR is a MEM representing the memory location of the SFR.
    If CLR_P then clear the SFR after the push using zero_reg.  */

static void
emit_push_sfr (rtx sfr, bool frame_related_p, bool clr_p, int treg)
{
  gcc_assert (MEM_P (sfr));

  /* IN treg, IO(SFR) */
  rtx_insn *insn = emit_move_insn (all_regs_rtx[treg], sfr);
  if (frame_related_p)
    RTX_FRAME_RELATED_P (insn) = 1;

  /* PUSH treg */
  emit_push_byte (treg, frame_related_p);

  if (clr_p)
    {
      /* OUT IO(SFR), __zero_reg__ */
      insn = emit_move_insn (sfr, const0_rtx);
      if (frame_related_p)
	RTX_FRAME_RELATED_P (insn) = 1;
    }
}


/*  Output function prologue.  */

void
nxu16_expand_prologue (void)
{
printf("nx16_expand_epilogue\n");
}


/* Turn TVAL into an integer that represents an ISR number.  When no such
   conversion is possible, then return 0.  Unfortunately, we don't know
   how many IRQs the device actually has.  */

static int
nxu16_isr_number (tree tval)
{
  return (TREE_CODE (tval) == INTEGER_CST
	  && tree_fits_shwi_p (tval)
	  && tree_to_shwi (tval) > 0)
    ? (int) tree_to_shwi (tval)
    : 0;
}


struct nxu16_fun_cookie
{
  FILE *file;
  const char *name;
};

/* A helper for `nxu16_declare_function_name' below.  When the function has
   attributes like signal(N) or interrupt(N), then define __vector_N as
   a global alias for the function name.  */

static void
nxu16_asm_isr_alias (tree /*func*/, tree attr, void *pv)
{
  nxu16_fun_cookie *cookie = (nxu16_fun_cookie *) pv;

  for (tree v = TREE_VALUE (attr); v; v = TREE_CHAIN (v))
    {
      int ival = nxu16_isr_number (TREE_VALUE (v));

      if (ival)
	{
	  fprintf (cookie->file, ".global __vector_%d\n", ival);
	  fprintf (cookie->file, "__vector_%d = ", ival);
	  assemble_name (cookie->file, cookie->name);
	  fprintf (cookie->file, "\n");
	}
    }
}


/* Worker for `ASM_DECLARE_FUNCTION_NAME'.  */

void
nxu16_declare_function_name (FILE *file, const char *name, tree decl)
{
  // Default action from elfos.h.
  ASM_OUTPUT_TYPE_DIRECTIVE (file, name, "function");
  ASM_DECLARE_RESULT (file, DECL_RESULT (decl));
  ASM_OUTPUT_FUNCTION_LABEL (file, name, decl);

  nxu16_fun_cookie fc = { file, name };

  nxu16_foreach_function_attribute (decl, "signal", nxu16_asm_isr_alias, &fc);
  nxu16_foreach_function_attribute (decl, "interrupt", nxu16_asm_isr_alias, &fc);
}



/* Worker function for `EPILOGUE_USES'.  */

int
nxu16_epilogue_uses (int /*regno*/)
{
  if (reload_completed
      && cfun->machine
      && (cfun->machine->is_interrupt || cfun->machine->is_signal))
    return 1;
  return 0;
}

/*  Helper for nxu16_expand_epilogue.  Emit a pop of a byte register.  */

static void
emit_pop_byte (unsigned regno)
{
  rtx mem = gen_rtx_PRE_INC (HImode, stack_pointer_rtx);
  mem = gen_frame_mem (QImode, mem);
  rtx reg = gen_rtx_REG (QImode, regno);

  emit_insn (gen_rtx_SET (reg, mem));
}

/*  Output RTL epilogue.  */

void
nxu16_expand_epilogue ()
{
  printf("nxu16_expand_epilogue\n");
}




/* Implement `TARGET_CANNOT_MODITY_JUMPS_P'.  */

static bool
nxu16_cannot_modify_jumps_p (void)
{
  /* Naked Functions must not have any instructions after
     their epilogue, see PR42240 */

  return (reload_completed
	  && cfun->machine
	  && cfun->machine->is_naked);
}


/* Implement `TARGET_MODE_DEPENDENT_ADDRESS_P'.  */

static bool
nxu16_mode_dependent_address_p (const_rtx /*addr*/, addr_space_t as)
{
  /* FIXME:  Non-generic addresses are not mode-dependent in themselves.
       This hook just serves to hack around PR rtl-optimization/52543 by
       claiming that non-generic addresses were mode-dependent so that
       lower-subreg.cc will skip these addresses.  lower-subreg.cc sets up fake
       RTXes to probe SET and MEM costs and assumes that MEM is always in the
       generic address space which is not true.  */

  return !ADDR_SPACE_GENERIC_P (as);
}


/* Return true if rtx X is a CONST_INT, CONST or SYMBOL_REF
   address with the `absdata' variable attribute, i.e. respective
   data can be read / written by LDS / STS instruction.
   This is used only for nxu16_TINY.  */

static bool
nxu16_address_tiny_absdata_p (rtx x, machine_mode mode)
{
  if (CONST == GET_CODE (x))
    x = XEXP (XEXP (x, 0), 0);

  if (SYMBOL_REF_P (x))
    return SYMBOL_REF_FLAGS (x) & nxu16_SYMBOL_FLAG_TINY_ABSDATA;

  if (CONST_INT_P (x)
      && IN_RANGE (INTVAL (x), 0, 0xc0 - GET_MODE_SIZE (mode)))
    return true;

  return false;
}


/* Helper function for `nxu16_legitimate_address_p'.  */

static inline bool
nxu16_reg_ok_for_addr_p (rtx reg, addr_space_t as,
		       rtx_code outer_code, bool strict)
{
  return (REG_P (reg)
	  && (nxu16_regno_mode_code_ok_for_base_p (REGNO (reg), QImode,
						 as, outer_code, UNKNOWN)
	      || (!strict
		  && REGNO (reg) >= FIRST_PSEUDO_REGISTER)));
}


/* Return nonzero if X (an RTX) is a legitimate memory address on the target
   machine for a memory operand of mode MODE.  */

static bool
nxu16_legitimate_address_p (machine_mode mode, rtx x, bool strict)
{
  bool ok = CONSTANT_ADDRESS_P (x);

  switch (GET_CODE (x))
    {
    case REG:
      ok = nxu16_reg_ok_for_addr_p (x, ADDR_SPACE_GENERIC,
				  MEM, strict);

      if (strict
	  && GET_MODE_SIZE (mode) > 4
	  && REG_X == REGNO (x))
	{
	  ok = false;
	}
      break;

    case POST_INC:
    case PRE_DEC:
      ok = nxu16_reg_ok_for_addr_p (XEXP (x, 0), ADDR_SPACE_GENERIC,
				  GET_CODE (x), strict);
      break;

    case PLUS:
      {
	rtx reg = XEXP (x, 0);
	rtx op1 = XEXP (x, 1);

	if (REG_P (reg)
	    && CONST_INT_P (op1)
	    && INTVAL (op1) >= 0)
	  {
	    bool fit = (IN_RANGE (INTVAL (op1), 0, MAX_LD_OFFSET (mode))
			// Reduced Tiny does not support PLUS addressing
			// anyway, so we are not restricted to LD offset.
			|| nxu16_TINY);

	    if (fit)
	      {
		ok = (! strict
		      || nxu16_reg_ok_for_addr_p (reg, ADDR_SPACE_GENERIC,
						PLUS, strict));

		if (reg == frame_pointer_rtx
		    || reg == arg_pointer_rtx)
		  {
		    ok = true;
		  }
	      }
	    else if (frame_pointer_needed
		     && reg == frame_pointer_rtx)
	      {
		ok = true;
	      }
	  }
      }
      break;

    default:
      break;
    }

  if (nxu16_TINY
      && CONSTANT_ADDRESS_P (x))
    {
      /* nxu16tiny's load / store instructions only cover addresses 0..0xbf:
	 IN / OUT range is 0..0x3f and LDS / STS can access 0x40..0xbf.  */

      ok = nxu16_address_tiny_absdata_p (x, mode);
    }

  if (nxu16_log.legitimate_address_p)
    {
      nxu16_edump ("\n%?: ret=%d, mode=%m strict=%d "
		 "reload_completed=%d ra_in_progress=%d %s:",
		 ok, mode, strict, reload_completed, ra_in_progress (),
		 reg_renumber ? "(reg_renumber)" : "");

      if (GET_CODE (x) == PLUS
	  && REG_P (XEXP (x, 0))
	  && CONST_INT_P (XEXP (x, 1))
	  && IN_RANGE (INTVAL (XEXP (x, 1)), 0, MAX_LD_OFFSET (mode))
	  && reg_renumber)
	{
	  nxu16_edump ("(r%d ---> r%d)", REGNO (XEXP (x, 0)),
		     true_regnum (XEXP (x, 0)));
	}

      nxu16_edump ("\n%r\n", x);
    }

  return ok;
}


/* Former implementation of TARGET_LEGITIMIZE_ADDRESS,
   now only a helper for nxu16_addr_space_legitimize_address.  */
/* Attempts to replace X with a valid
   memory address for an operand of mode MODE  */

static rtx
nxu16_legitimize_address (rtx x, rtx oldx, machine_mode mode)
{
  bool big_offset_p = false;

  x = oldx;

  if (nxu16_TINY)
    {
      if (CONSTANT_ADDRESS_P (x)
	  && ! nxu16_address_tiny_absdata_p (x, mode))
	{
	  x = force_reg (Pmode, x);
	}
    }

  if (GET_CODE (oldx) == PLUS
      && REG_P (XEXP (oldx, 0)))
    {
      if (REG_P (XEXP (oldx, 1)))
	x = force_reg (GET_MODE (oldx), oldx);
      else if (CONST_INT_P (XEXP (oldx, 1)))
	{
	  int offs = INTVAL (XEXP (oldx, 1));
	  if (frame_pointer_rtx != XEXP (oldx, 0)
	      && offs > MAX_LD_OFFSET (mode))
	    {
	      big_offset_p = true;
	      x = force_reg (GET_MODE (oldx), oldx);
	    }
	}
    }

  if (nxu16_log.legitimize_address)
    {
      nxu16_edump ("\n%?: mode=%m\n %r\n", mode, oldx);

      if (x != oldx)
	nxu16_edump (" %s --> %r\n", big_offset_p ? "(big offset)" : "", x);
    }

  return x;
}


/* Implement `LEGITIMIZE_RELOAD_ADDRESS'.  */
/* This will allow register R26/27 to be used where it is no worse than normal
   base pointers R28/29 or R30/31.  For example, if base offset is greater
   than 63 bytes or for R++ or --R addressing.  */

rtx
nxu16_legitimize_reload_address (rtx *px, machine_mode mode, int opnum,
			       int type, int addr_type, int /*ind_levels*/,
			       rtx (*mk_memloc)(rtx,int))
{
  rtx x = *px;

  if (nxu16_log.legitimize_reload_address)
    nxu16_edump ("\n%?:%m %r\n", mode, x);

  if (1 && (GET_CODE (x) == POST_INC
	    || GET_CODE (x) == PRE_DEC))
    {
      push_reload (XEXP (x, 0), XEXP (x, 0), &XEXP (x, 0), &XEXP (x, 0),
		   POINTER_REGS, GET_MODE (x), GET_MODE (x), 0, 0,
		   opnum, RELOAD_OTHER);

      if (nxu16_log.legitimize_reload_address)
	nxu16_edump (" RCLASS.1 = %R\n IN = %r\n OUT = %r\n",
		   POINTER_REGS, XEXP (x, 0), XEXP (x, 0));

      return x;
    }

  if (GET_CODE (x) == PLUS
      && REG_P (XEXP (x, 0))
      && reg_equiv_constant (REGNO (XEXP (x, 0))) == 0
      && CONST_INT_P (XEXP (x, 1))
      && INTVAL (XEXP (x, 1)) >= 1)
    {
      bool fit = INTVAL (XEXP (x, 1)) <= MAX_LD_OFFSET (mode);

      if (fit)
	{
	  if (reg_equiv_address (REGNO (XEXP (x, 0))) != 0)
	    {
	      int regno = REGNO (XEXP (x, 0));
	      rtx mem = mk_memloc (x, regno);

	      push_reload (XEXP (mem, 0), NULL_RTX, &XEXP (mem, 0), NULL,
			   POINTER_REGS, Pmode, VOIDmode, 0, 0,
			   1, (enum reload_type) addr_type);

	      if (nxu16_log.legitimize_reload_address)
		nxu16_edump (" RCLASS.2 = %R\n IN = %r\n OUT = %r\n",
			   POINTER_REGS, XEXP (mem, 0), NULL_RTX);

	      push_reload (mem, NULL_RTX, &XEXP (x, 0), NULL,
			   BASE_POINTER_REGS, GET_MODE (x), VOIDmode, 0, 0,
			   opnum, (enum reload_type) type);

	      if (nxu16_log.legitimize_reload_address)
		nxu16_edump (" RCLASS.2 = %R\n IN = %r\n OUT = %r\n",
			   BASE_POINTER_REGS, mem, NULL_RTX);

	      return x;
	    }
	}
      else if (! (frame_pointer_needed
		  && XEXP (x, 0) == frame_pointer_rtx))
	{
	  push_reload (x, NULL_RTX, px, NULL,
		       POINTER_REGS, GET_MODE (x), VOIDmode, 0, 0,
		       opnum, (enum reload_type) type);

	  if (nxu16_log.legitimize_reload_address)
	    nxu16_edump (" RCLASS.3 = %R\n IN = %r\n OUT = %r\n",
		       POINTER_REGS, x, NULL_RTX);

	  return x;
	}
    }

  return NULL_RTX;
}


/* Helper function to print assembler resp. track instruction
   sequence lengths.  Always return "".

   If PLEN == NULL:
       Output assembler code from template TPL with operands supplied
       by OPERANDS.  This is just forwarding to output_asm_insn.

   If PLEN != NULL:
       If N_WORDS >= 0  Add N_WORDS to *PLEN.
       If N_WORDS < 0   Set *PLEN to -N_WORDS.
       Don't output anything.
*/

static const char *
nxu16_asm_len (const char *tpl, rtx *operands, int *plen, int n_words)
{
  if (plen == NULL)
    output_asm_insn (tpl, operands);
  else
    {
      if (n_words < 0)
	*plen = -n_words;
      else
	*plen += n_words;
    }

  return "";
}


/* Return a pointer register name as a string.  */

static const char *
ptrreg_to_str (int regno)
{
  switch (regno)
    {
    case REG_X: return "X";
    case REG_Y: return "Y";
    case REG_Z: return "Z";
    default:
      output_operand_lossage ("address operand requires constraint for"
			      " X, Y, or Z register");
    }
  return NULL;
}


/* Return the condition name as a string to be used in a BR** instruction.
   Used in conditional jump constructing.  */

static const char *
nxu16_cond_string (rtx_code code, bool cc_overflow_unusable)
{
  switch (code)
    {
    case NE:
      return "ne";
    case EQ:
      return "eq";
    case GE:
      return cc_overflow_unusable ? "pl" : "ge";
    case LT:
      return cc_overflow_unusable ? "mi" : "lt";
    case GEU:
      return "sh";
    case LTU:
      return "lo";
    default:
      gcc_unreachable ();
    }

  return "";
}


/* Return true if rtx X is a CONST or SYMBOL_REF with progmem.
   This must be used for nxu16_TINY only because on other cores
   the flash memory is not visible in the RAM address range and
   cannot be read by, say,  LD instruction.  */

static bool
nxu16_address_tiny_pm_p (rtx x)
{
  if (CONST == GET_CODE (x))
    x = XEXP (XEXP (x, 0), 0);

  if (SYMBOL_REF_P (x))
    return SYMBOL_REF_FLAGS (x) & nxu16_SYMBOL_FLAG_TINY_PM;

  return false;
}


/* Implement `TARGET_PRINT_OPERAND_ADDRESS'.  */
/* Output ADDR to FILE as address.  */

static void
nxu16_print_operand_address (FILE *file, machine_mode /*mode*/, rtx addr)
{
  if (nxu16_TINY
      && nxu16_address_tiny_pm_p (addr))
    {
      addr = plus_constant (Pmode, addr, nxu16_arch->flash_pm_offset);
    }

  switch (GET_CODE (addr))
    {
    case REG:
      fprintf (file, "%s", ptrreg_to_str (REGNO (addr)));
      break;

    case PRE_DEC:
      fprintf (file, "-%s", ptrreg_to_str (REGNO (XEXP (addr, 0))));
      break;

    case POST_INC:
      fprintf (file, "%s+", ptrreg_to_str (REGNO (XEXP (addr, 0))));
      break;

    default:
      if (CONSTANT_ADDRESS_P (addr)
	  && text_segment_operand (addr, VOIDmode))
	{
	  rtx x = addr;
	  if (GET_CODE (x) == CONST)
	    x = XEXP (x, 0);
	  if (GET_CODE (x) == PLUS && CONST_INT_P (XEXP (x, 1)))
	    {
	      /* Assembler gs() will implant word address.  Make offset
		 a byte offset inside gs() for assembler.  This is
		 needed because the more logical (constant+gs(sym)) is not
		 accepted by gas.  For 128K and smaller devices this is ok.
		 For large devices it will create a trampoline to offset
		 from symbol which may not be what the user really wanted.  */

	      fprintf (file, "gs(");
	      output_addr_const (file, XEXP (x, 0));
	      fprintf (file, "+" HOST_WIDE_INT_PRINT_DEC ")",
		       2 * INTVAL (XEXP (x, 1)));
	      if (nxu16_3_BYTE_PC)
		if (warning (0, "pointer offset from symbol maybe incorrect"))
		  {
		    output_addr_const (stderr, addr);
		    fprintf (stderr, "\n");
		  }
	    }
	  else
	    {
	      fprintf (file, "gs(");
	      output_addr_const (file, addr);
	      fprintf (file, ")");
	    }
	}
      else
	output_addr_const (file, addr);
    }
}


/* Implement `TARGET_PRINT_OPERAND_PUNCT_VALID_P'.  */

static bool
nxu16_print_operand_punct_valid_p (unsigned char code)
{
  return code == '~' || code == '!';
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

  if (code == '~')
    {
      if (!nxu16_HAVE_JMP_CALL)
	fputc ('r', file);
    }
  else if (code == '!')
    {
      if (nxu16_HAVE_EIJMP_EICALL)
	fputc ('e', file);
    }
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
      if (x == zero_reg_rtx)
	fprintf (file, "__zero_reg__");
      else if (code == 'r' && REGNO (x) < REG_32)
	fprintf (file, "%d", (int) REGNO (x));
      else
	fprintf (file, "%s", reg_names[REGNO (x) + abcd]);
    }
  else if (CONST_INT_P (x))
    {
      HOST_WIDE_INT ival = INTVAL (x);

      if ('i' != code)
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, ival + abcd);
      else if (low_io_address_operand (x, VOIDmode)
	       )//|| high_io_address_operand (x, VOIDmode))
	{
	  if (nxu16_HAVE_RAMPZ && ival == nxu16_addr.rampz)
	    fprintf (file, "__RAMPZ__");
	  else if (nxu16_HAVE_RAMPY && ival == nxu16_addr.rampy)
	    fprintf (file, "__RAMPY__");
	  else if (nxu16_HAVE_RAMPX && ival == nxu16_addr.rampx)
	    fprintf (file, "__RAMPX__");
	  else if (nxu16_HAVE_RAMPD && ival == nxu16_addr.rampd)
	    fprintf (file, "__RAMPD__");
	  else if ((nxu16_XMEGA || nxu16_TINY) && ival == nxu16_addr.ccp)
	    fprintf (file, "__CCP__");
	  else if (ival == nxu16_addr.sreg)   fprintf (file, "__SREG__");
	  else if (ival == nxu16_addr.sp_l)   fprintf (file, "__SP_L__");
	  else if (ival == nxu16_addr.sp_h)   fprintf (file, "__SP_H__");
	  else
	    {
	      fprintf (file, HOST_WIDE_INT_PRINT_HEX,
		       ival - nxu16_arch->sfr_offset);
	    }
	}
      else
	fatal_insn ("bad address, not an I/O address:", x);
    }
  else if (MEM_P (x))
    {
      rtx addr = XEXP (x, 0);

      if (code == 'm')
	{
	  if (!CONSTANT_P (addr))
	    fatal_insn ("bad address, not a constant:", addr);
	  /* Assembler template with m-code is data - not progmem section */
	  if (text_segment_operand (addr, VOIDmode))
	    if (warning (0, "accessing data memory with"
			 " program memory address"))
	      {
		output_addr_const (stderr, addr);
		fprintf(stderr,"\n");
	      }
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
	  if (REGNO (XEXP (addr, 0)) == REG_X)
	    fatal_insn ("internal compiler error.  Bad address:"
			,addr);
	  fputc ('+', file);
	  nxu16_print_operand (file, XEXP (addr, 1), code);
	}
      else
	nxu16_print_operand_address (file, VOIDmode, addr);
    }
  else if (code == 'i')
    {
      if (SYMBOL_REF_P (x) && (SYMBOL_REF_FLAGS (x) & SYMBOL_FLAG_IO))
	nxu16_print_operand_address
	  (file, VOIDmode, plus_constant (HImode, x, -nxu16_arch->sfr_offset));
      else
	fatal_insn ("bad address, not an I/O address:", x);
    }
  else if (code == 'x')
    {
      /* Constant progmem address - like used in jmp or call */
      if (text_segment_operand (x, VOIDmode) == 0)
	if (warning (0, "accessing program memory"
		     " with data memory address"))
	  {
	    output_addr_const (stderr, x);
	    fprintf (stderr, "\n");
	  }
      /* Use normal symbol for direct address no linker trampoline needed */
      output_addr_const (file, x);
    }
  else if (CONST_FIXED_P (x))
    {
      HOST_WIDE_INT ival = INTVAL (nxu16_to_int_mode (x));
      if (code != 0)
	output_operand_lossage ("Unsupported code '%c' for fixed-point:",
				code);
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
  else if (code == 'j' || code == 'L')
    fputs (nxu16_cond_string (GET_CODE (x), code == 'L'), file);
  else if (code == 'k' || code == 'K')
    fputs (nxu16_cond_string (reverse_condition (GET_CODE (x)), code == 'K'),
	   file);
  else
    nxu16_print_operand_address (file, VOIDmode, x);
}


/* Implement `TARGET_USE_BY_PIECES_INFRASTRUCTURE_P'.  */
/* Prefer sequence of loads/stores for moves of size upto
   two - two pairs of load/store instructions are always better
   than the 5 instruction sequence for a loop (1 instruction
   for loop counter setup, and 4 for the body of the loop). */

static bool
nxu16_use_by_pieces_infrastructure_p (unsigned HOST_WIDE_INT size,
				    unsigned int align,
				    by_pieces_operation op, bool speed_p)
{
  if (op != MOVE_BY_PIECES
      || (speed_p && size > MOVE_MAX_PIECES))
    return default_use_by_pieces_infrastructure_p (size, align, op, speed_p);

  return size <= MOVE_MAX_PIECES;
}


/* Choose mode for jump insn:
   1 - relative jump in range -63 <= x <= 62 ;
   2 - relative jump in range -2046 <= x <= 2045 ;
   3 - absolute jump (only when we have JMP / CALL).

   When jumping backwards, assume the jump offset is EXTRA words
   bigger than inferred from insn addresses.  */

int
nxu16_jump_mode (rtx x, rtx_insn *insn, int extra)
{
  int dest_addr = INSN_ADDRESSES (INSN_UID (GET_CODE (x) == LABEL_REF
					    ? XEXP (x, 0) : x));
  int cur_addr = INSN_ADDRESSES (INSN_UID (insn));
  int jump_distance = cur_addr - dest_addr;

  if (IN_RANGE (jump_distance, -63, 62 - extra))
    return 1;
  else if (IN_RANGE (jump_distance, -2046, 2045 - extra))
    return 2;
  else if (nxu16_HAVE_JMP_CALL)
    return 3;

  return 2;
}


/* Return the asm code for conditional branch INSN, where XOP[0] is the jump
   target label and XOP[1] is a comparison operator of REG_CC against 0.  */

const char *
nxu16_cond_branch (rtx_insn *insn, rtx *xop)
{
  machine_mode ccmode = GET_MODE (XEXP (xop[1], 0));
  rtx_code cond = GET_CODE (xop[1]);
  bool cc_overflow_unusable = ccmode != CCmode;
  int len = nxu16_jump_mode (xop[0], insn);

  if (ccmode == CCNmode)
    // The N flag can only do < 0 and >= 0.
    gcc_assert (cond == GE || cond == LT);

  switch (cond)
    {
    case GT:
      if (cc_overflow_unusable)
	return (len == 1 ? ("breq .+2" CR_TAB
			    "brpl %0") :
		len == 2 ? ("breq .+4" CR_TAB
			    "brmi .+2" CR_TAB
			    "rjmp %0") :
		("breq .+6" CR_TAB
		 "brmi .+4" CR_TAB
		 "jmp %0"));

      else
	return (len == 1 ? ("breq .+2" CR_TAB
			    "brge %0") :
		len == 2 ? ("breq .+4" CR_TAB
			    "brlt .+2" CR_TAB
			    "rjmp %0") :
		("breq .+6" CR_TAB
		 "brlt .+4" CR_TAB
		 "jmp %0"));
    case GTU:
      return (len == 1 ? ("breq .+2" CR_TAB
			  "brsh %0") :
	      len == 2 ? ("breq .+4" CR_TAB
			  "brlo .+2" CR_TAB
			  "rjmp %0") :
	      ("breq .+6" CR_TAB
	       "brlo .+4" CR_TAB
	       "jmp %0"));
    case LE:
      if (cc_overflow_unusable)
	return (len == 1 ? ("breq %0" CR_TAB
			    "brmi %0") :
		len == 2 ? ("breq .+2" CR_TAB
			    "brpl .+2" CR_TAB
			    "rjmp %0") :
		("breq .+2" CR_TAB
		 "brpl .+4" CR_TAB
		 "jmp %0"));
      else
	return (len == 1 ? ("breq %0" CR_TAB
			    "brlt %0") :
		len == 2 ? ("breq .+2" CR_TAB
			    "brge .+2" CR_TAB
			    "rjmp %0") :
		("breq .+2" CR_TAB
		 "brge .+4" CR_TAB
		 "jmp %0"));
    case LEU:
      return (len == 1 ? ("breq %0" CR_TAB
			  "brlo %0") :
	      len == 2 ? ("breq .+2" CR_TAB
			  "brsh .+2" CR_TAB
			  "rjmp %0") :
	      ("breq .+2" CR_TAB
	       "brsh .+4" CR_TAB
	       "jmp %0"));
    default:
      switch (len)
	{
	case 1:
	  return cc_overflow_unusable
	    ? "br%L1 %0"
	    : "br%j1 %0";
	case 2:
	  return cc_overflow_unusable
	    ? "br%K1 .+2" CR_TAB "rjmp %0"
	    : "br%k1 .+2" CR_TAB "rjmp %0";
	default:
	  return cc_overflow_unusable
	    ? "br%K1 .+4" CR_TAB "jmp %0"
	    : "br%k1 .+4" CR_TAB "jmp %0";
	}
    }
  return "";
}


/* Worker function for `FINAL_PRESCAN_INSN'.  */
/* Output insn cost for next insn.  */

void
nxu16_final_prescan_insn (rtx_insn *insn, rtx * /*operands*/,
			int /*num_operands*/)
{
  if (nxu16_log.rtx_costs)
    {
      rtx set = single_set (insn);

      if (set)
	fprintf (asm_out_file, "/* DEBUG: cost = %d.  */\n",
		 set_src_cost (SET_SRC (set), GET_MODE (SET_DEST (set)),
			       optimize_insn_for_speed_p ()));
      else
	fprintf (asm_out_file, "/* DEBUG: pattern-cost = %d.  */\n",
		 rtx_cost (PATTERN (insn), VOIDmode, INSN, 0,
			   optimize_insn_for_speed_p()));
    }

  if (nxu16_log.insn_addresses)
    fprintf (asm_out_file, ";; ADDR = %d\n",
	     (int) INSN_ADDRESSES (INSN_UID (insn)));
}





/* Worker function for `FUNCTION_ARG_REGNO_P'.  */
/* Returns nonzero if REGNO is the number of a hard
   register in which function arguments are sometimes passed.  */

int
nxu16_function_arg_regno_p (int r)
{
  return nxu16_TINY
    ? IN_RANGE (r, REG_20, REG_25)
    : IN_RANGE (r, REG_8, REG_25);
}


/* Worker function for `INIT_CUMULATIVE_ARGS'.  */
/* Initializing the variable cum for the state at the beginning
   of the argument list.  */

void
nxu16_init_cumulative_args (CUMULATIVE_ARGS *cum, tree fntype, rtx libname,
			  tree /*fndecl*/)
{
  cum->nregs = nxu16_TINY ? 1 + REG_25 - REG_20 : 1 + REG_25 - REG_8;
  cum->regno = FIRST_CUM_REG;
  cum->has_stack_args = 0;
  if (!libname && stdarg_p (fntype))
    cum->nregs = 0;

  /* Assume the calle may be tail called */

  cfun->machine->sibcall_fails = 0;
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

  if (cum->regno >= REG_8
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

  if (cum->regno >= REG_8
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
      cum->regno = FIRST_CUM_REG;
    }
}

/* Implement `TARGET_FUNCTION_OK_FOR_SIBCALL' */
/* Decide whether we can make a sibling call to a function.  DECL is the
   declaration of the function being targeted by the call and EXP is the
   CALL_EXPR representing the call.  */

static bool
nxu16_function_ok_for_sibcall (tree decl_callee, tree exp_callee)
{
  /* Tail-calling must fail if callee-saved regs are used to pass
     function args.  We must not tail-call when `epilogue_restores'
     is used.  Unfortunately, we cannot tell at this point if that
     actually will happen or not, and we cannot step back from
     tail-calling.  Thus, we inhibit tail-calling with -mcall-prologues.  */

  if (cfun->machine->sibcall_fails
      || TARGET_CALL_PROLOGUES)
    {
      return false;
    }

  tree fntype_callee = TREE_TYPE (CALL_EXPR_FN (exp_callee));

  if (decl_callee)
    {
      decl_callee = TREE_TYPE (decl_callee);
    }
  else
    {
      decl_callee = fntype_callee;

      while (FUNCTION_TYPE != TREE_CODE (decl_callee)
	     && METHOD_TYPE != TREE_CODE (decl_callee))
	{
	  decl_callee = TREE_TYPE (decl_callee);
	}
    }

  /* Ensure that caller and callee have compatible epilogues */

  if (cfun->machine->is_interrupt
      || cfun->machine->is_signal
      || cfun->machine->is_naked
      || nxu16_naked_function_p (decl_callee))
    {
      return false;
    }

  return true;
}

/***********************************************************************
  Functions for outputting various mov's for a various modes
************************************************************************/

/* Return true if a value of mode MODE is read from flash by
   __load_* function from libgcc.  */

bool
nxu16_load_libgcc_p (rtx op)
{
  machine_mode mode = GET_MODE (op);
  int n_bytes = GET_MODE_SIZE (mode);

  return (n_bytes > 2
	  && !nxu16_HAVE_LPMX
	  && nxu16_mem_flash_p (op));
}

/* Return true if a value of mode MODE is read by __xload_* function.  */

bool
nxu16_xload_libgcc_p (machine_mode mode)
{
  int n_bytes = GET_MODE_SIZE (mode);

  return (n_bytes > 1
	  || nxu16_n_flash > 1);
}


/* Return true when INSN has a REG_UNUSED note for hard reg REG.
   rtlanal.cc::find_reg_note() uses == to compare XEXP (link, 0)
   therefore use a custom function.  */

static bool
nxu16_insn_has_reg_unused_note_p (rtx_insn *insn, rtx reg)
{
  for (rtx link = REG_NOTES (insn); link; link = XEXP (link, 1))
    if (REG_NOTE_KIND (link) == REG_UNUSED
	&& REG_P (XEXP (link, 0))
	&& REGNO (reg) >= REGNO (XEXP (link, 0))
	&& END_REGNO (reg) <= END_REGNO (XEXP (link, 0)))
      return true;

  return false;
}


/* A helper for the next function.
   Return nonzero if REG is not used after INSN.
   We assume REG is a reload reg, and therefore does
   not live past labels.  It may live past calls or jumps though.  */

static bool
_reg_unused_after (rtx_insn *insn, rtx reg, bool look_at_insn)
{
  if (look_at_insn)
    {
      /* If the reg is set by this instruction, then it is safe for our
	 case.  Disregard the case where this is a store to memory, since
	 we are checking a register used in the store address.  */
      rtx set = single_set (insn);
      if (set && !MEM_P (SET_DEST (set))
	  && reg_overlap_mentioned_p (reg, SET_DEST (set)))
	return 1;

      /* This case occurs when fuse-add introduced a POST_INC addressing,
	 but the address register is unused after.  */
      if (set)
	{
	  rtx mem = MEM_P (SET_SRC (set)) ? SET_SRC (set) : SET_DEST (set);
	  if (MEM_P (mem)
	      && reg_overlap_mentioned_p (reg, XEXP (mem, 0))
	      && nxu16_insn_has_reg_unused_note_p (insn, reg))
	    return 1;
	}
    }

  while ((insn = NEXT_INSN (insn)))
    {
      rtx set;
      rtx_code code = GET_CODE (insn);

#if 0
      /* If this is a label that existed before reload, then the register
	 if dead here.  However, if this is a label added by reorg, then
	 the register may still be live here.  We can't tell the difference,
	 so we just ignore labels completely.  */
      if (code == CODE_LABEL)
	return 1;
      /* else */
#endif

      if (!INSN_P (insn))
	continue;

      if (code == JUMP_INSN)
	return 0;

      /* If this is a sequence, we must handle them all at once.
	 We could have for instance a call that sets the target register,
	 and an insn in a delay slot that uses the register.  In this case,
	 we must return 0.  */
      else if (code == INSN && GET_CODE (PATTERN (insn)) == SEQUENCE)
	{
	  rtx_sequence *seq = as_a <rtx_sequence *> (PATTERN (insn));
	  int retval = 0;

	  for (int i = 0; i < seq->len (); i++)
	    {
	      rtx_insn *this_insn = seq->insn (i);
	      rtx set = single_set (this_insn);

	      if (CALL_P (this_insn))
		code = CALL_INSN;
	      else if (JUMP_P (this_insn))
		{
		  if (INSN_ANNULLED_BRANCH_P (this_insn))
		    return 0;
		  code = JUMP_INSN;
		}

	      if (set && reg_overlap_mentioned_p (reg, SET_SRC (set)))
		return 0;
	      if (set && reg_overlap_mentioned_p (reg, SET_DEST (set)))
		{
		  if (!MEM_P (SET_DEST (set)))
		    retval = 1;
		  else
		    return 0;
		}
	      if (set == 0
		  && reg_overlap_mentioned_p (reg, PATTERN (this_insn)))
		return 0;
	    }
	  if (retval == 1)
	    return 1;
	  else if (code == JUMP_INSN)
	    return 0;
	}

      if (code == CALL_INSN)
	{
	  rtx tem;
	  for (tem = CALL_INSN_FUNCTION_USAGE (insn); tem; tem = XEXP (tem, 1))
	    if (GET_CODE (XEXP (tem, 0)) == USE
		&& REG_P (XEXP (XEXP (tem, 0), 0))
		&& reg_overlap_mentioned_p (reg, XEXP (XEXP (tem, 0), 0)))
	      return 0;
	  if (call_used_or_fixed_reg_p (REGNO (reg)))
	    return 1;
	}

      set = single_set (insn);

      if (set && reg_overlap_mentioned_p (reg, SET_SRC (set)))
	return 0;
      if (set && reg_overlap_mentioned_p (reg, SET_DEST (set)))
	return !MEM_P (SET_DEST (set));
      if (set == 0 && reg_overlap_mentioned_p (reg, PATTERN (insn)))
	return 0;
    }
  return 1;
}


/* Return nonzero if register REG dead after INSN.  */

int
reg_unused_after (rtx_insn *insn, rtx reg)
{
  return (dead_or_set_p (insn, reg)
	  || (REG_P (reg) && _reg_unused_after (insn, reg, true)));
}


/* Fixme: This is a hack because secondary reloads don't works as expected.

   Find an unused d-register to be used as scratch in INSN.
   EXCLUDE is either NULL_RTX or some register. In the case where EXCLUDE
   is a register, skip all possible return values that overlap EXCLUDE.
   The policy for the returned register is similar to that of
   `reg_unused_after', i.e. the returned register may overlap the SET_DEST
   of INSN.

   Return a QImode d-register or NULL_RTX if nothing found.  */

static rtx
nxu16_find_unused_d_reg (rtx_insn *insn, rtx exclude)
{
  bool isr_p = (nxu16_interrupt_function (current_function_decl)
		|| nxu16_signal_function (current_function_decl));

  for (int regno = REG_16; regno < REG_32; regno++)
    {
      rtx reg = all_regs_rtx[regno];

      if ((exclude
	   && reg_overlap_mentioned_p (exclude, reg))
	  || fixed_regs[regno])
	{
	  continue;
	}

      /* Try non-live register */

      if (!df_regs_ever_live_p (regno)
	  && (TREE_THIS_VOLATILE (current_function_decl)
	      || cfun->machine->is_OS_task
	      || cfun->machine->is_OS_main
	      || (!isr_p && call_used_or_fixed_reg_p (regno))))
	{
	  return reg;
	}

      /* Any live register can be used if it is unused after.
	 Prologue/epilogue will care for it as needed.  */

      if (df_regs_ever_live_p (regno)
	  && reg_unused_after (insn, reg))
	{
	  return reg;
	}
    }

  return NULL_RTX;
}


/* Helper function for the next function in the case where only restricted
   version of LPM instruction is available.  */

static const char *
nxu16_out_lpm_no_lpmx (rtx_insn *insn, rtx *xop, int *plen)
{
  rtx dest = xop[0];
  rtx addr = xop[1];
  int n_bytes = GET_MODE_SIZE (GET_MODE (dest));
  int regno_dest;

  regno_dest = REGNO (dest);

  /* The implicit target register of LPM.  */
  xop[3] = lpm_reg_rtx;

  switch (GET_CODE (addr))
    {
    default:
      gcc_unreachable();

    case REG:

      gcc_assert (REG_Z == REGNO (addr));

      switch (n_bytes)
	{
	default:
	  gcc_unreachable();

	case 1:
	  nxu16_asm_len ("%4lpm", xop, plen, 1);

	  if (regno_dest != LPM_REGNO)
	    nxu16_asm_len ("mov %0,%3", xop, plen, 1);

	  return "";

	case 2:
	  if (REGNO (dest) == REG_Z)
	    return nxu16_asm_len ("%4lpm"      CR_TAB
				"push %3"    CR_TAB
				"adiw %2,1"  CR_TAB
				"%4lpm"      CR_TAB
				"mov %B0,%3" CR_TAB
				"pop %A0", xop, plen, 6);

	  nxu16_asm_len ("%4lpm"      CR_TAB
		       "mov %A0,%3" CR_TAB
		       "adiw %2,1"  CR_TAB
		       "%4lpm"      CR_TAB
		       "mov %B0,%3", xop, plen, 5);

	  if (!reg_unused_after (insn, addr))
	    nxu16_asm_len ("sbiw %2,1", xop, plen, 1);

	  break; /* 2 */
	}

      break; /* REG */

    case POST_INC:

      gcc_assert (REG_Z == REGNO (XEXP (addr, 0))
		  && n_bytes <= 4);

      for (int i = 0; i < n_bytes; ++i)
	{
	  rtx reg = nxu16_byte (dest, i);

	  if (i > 0)
	    nxu16_asm_len ("adiw %2,1", xop, plen, 1);

	  nxu16_asm_len ("%4lpm", xop, plen, 1);

	  if (REGNO (reg) != LPM_REGNO)
	    nxu16_asm_len ("mov %0,r0", &reg, plen, 1);
	}

      if (! reg_unused_after (insn, xop[2]))
	nxu16_asm_len ("adiw %2,1", xop, plen, 1);

      break; /* POST_INC */

    } /* switch CODE (addr) */

  return "";
}


/* If PLEN == NULL: Ouput instructions to load a value from a memory location
   OP[1] in AS1 to register OP[0].
   If PLEN != 0 set *PLEN to the length in words of the instruction sequence.
   Return "".  */

const char *
nxu16_out_lpm (rtx_insn *insn, rtx *op, int *plen)
{
	return "";
  rtx xop[7];
  rtx dest = op[0];
  rtx src = SET_SRC (single_set (insn));
  int n_bytes = GET_MODE_SIZE (GET_MODE (dest));
  addr_space_t as = MEM_ADDR_SPACE (src);

  if (plen)
    *plen = 0;

  if (MEM_P (dest))
    {
      warning (0, "writing to address space %qs not supported",
	       nxu16_addrspace[MEM_ADDR_SPACE (dest)].name);

      return "";
    }

  rtx addr = XEXP (src, 0);
  rtx_code code = GET_CODE (addr);

  gcc_assert (REG_P (dest));
  gcc_assert (REG == code || POST_INC == code);

  xop[0] = dest;
  xop[1] = addr;
  xop[2] = lpm_addr_reg_rtx;
  xop[4] = xstring_empty;
  xop[5] = tmp_reg_rtx;
  xop[6] = XEXP (rampz_rtx, 0);

  int segment = nxu16_addrspace[as].segment;

  /* Set RAMPZ as needed.  */

  if (segment)
    {
      xop[4] = GEN_INT (segment);
      xop[3] = nxu16_find_unused_d_reg (insn, lpm_addr_reg_rtx);

      if (xop[3] != NULL_RTX)
	{
	  nxu16_asm_len ("ldi %3,%4" CR_TAB
		       "out %i6,%3", xop, plen, 2);
	}
      else if (segment == 1)
	{
	  nxu16_asm_len ("clr %5" CR_TAB
		       "inc %5" CR_TAB
		       "out %i6,%5", xop, plen, 3);
	}
      else
	{
	  nxu16_asm_len ("mov %5,%2"   CR_TAB
		       "ldi %2,%4"   CR_TAB
		       "out %i6,%2"  CR_TAB
		       "mov %2,%5", xop, plen, 4);
	}

      xop[4] = xstring_e;

      if (!nxu16_HAVE_ELPMX)
	return nxu16_out_lpm_no_lpmx (insn, xop, plen);
    }
  else if (!nxu16_HAVE_LPMX)
    {
      return nxu16_out_lpm_no_lpmx (insn, xop, plen);
    }

  /* We have [E]LPMX: Output reading from Flash the comfortable way.  */

  switch (GET_CODE (addr))
    {
    default:
      gcc_unreachable();

    case REG:

      gcc_assert (REG_Z == REGNO (addr));

      switch (n_bytes)
	{
	default:
	  gcc_unreachable();

	case 1:
	  nxu16_asm_len ("%4lpm %0,%a2", xop, plen, 1);
	  break;

	case 2:
	  if (REGNO (dest) == REG_Z)
	    nxu16_asm_len ("%4lpm %5,%a2+" CR_TAB
			 "%4lpm %B0,%a2" CR_TAB
			 "mov %A0,%5", xop, plen, 3);
	  else
	    {
	      nxu16_asm_len ("%4lpm %A0,%a2+" CR_TAB
			   "%4lpm %B0,%a2", xop, plen, 2);

	      if (!reg_unused_after (insn, addr))
		nxu16_asm_len ("sbiw %2,1", xop, plen, 1);
	    }

	  break; /* 2 */

	case 3:

	  nxu16_asm_len ("%4lpm %A0,%a2+" CR_TAB
		       "%4lpm %B0,%a2+" CR_TAB
		       "%4lpm %C0,%a2", xop, plen, 3);

	  if (!reg_unused_after (insn, addr))
	    nxu16_asm_len ("sbiw %2,2", xop, plen, 1);

	  break; /* 3 */

	case 4:

	  nxu16_asm_len ("%4lpm %A0,%a2+" CR_TAB
		       "%4lpm %B0,%a2+", xop, plen, 2);

	  if (REGNO (dest) == REG_Z - 2)
	    nxu16_asm_len ("%4lpm %5,%a2+" CR_TAB
			 "%4lpm %C0,%a2" CR_TAB
			 "mov %D0,%5", xop, plen, 3);
	  else
	    {
	      nxu16_asm_len ("%4lpm %C0,%a2+" CR_TAB
			   "%4lpm %D0,%a2", xop, plen, 2);

	      if (!reg_unused_after (insn, addr))
		nxu16_asm_len ("sbiw %2,3", xop, plen, 1);
	    }

	  break; /* 4 */
	} /* n_bytes */

      break; /* REG */

    case POST_INC:

      gcc_assert (REG_Z == REGNO (XEXP (addr, 0))
		  && n_bytes <= 4);

      nxu16_asm_len		    ("%4lpm %A0,%a2+", xop, plen, 1);
      if (n_bytes >= 2)  nxu16_asm_len ("%4lpm %B0,%a2+", xop, plen, 1);
      if (n_bytes >= 3)  nxu16_asm_len ("%4lpm %C0,%a2+", xop, plen, 1);
      if (n_bytes >= 4)  nxu16_asm_len ("%4lpm %D0,%a2+", xop, plen, 1);

      break; /* POST_INC */

    } /* switch CODE (addr) */

  if (xop[4] == xstring_e && nxu16_HAVE_RAMPD)
    {
      /* Reset RAMPZ to 0 so that EBI devices don't read garbage from RAM.  */

      xop[0] = zero_reg_rtx;
      nxu16_asm_len ("out %i6,%0", xop, plen, 1);
    }

  return "";
}


/* Worker function for xload_8 insn.  */

const char *
nxu16_out_xload (rtx_insn * /*insn*/, rtx *op, int *plen)
{
  rtx xop[4];

  xop[0] = op[0];
  xop[1] = op[1];
  xop[2] = lpm_addr_reg_rtx;
  xop[3] = nxu16_HAVE_LPMX ? op[0] : lpm_reg_rtx;

  if (plen)
    *plen = 0;

  if (reg_overlap_mentioned_p (xop[3], lpm_addr_reg_rtx))
    nxu16_asm_len ("sbrs %1,7", xop, plen, 1);

  nxu16_asm_len (nxu16_HAVE_LPMX ? "lpm %3,%a2" : "lpm", xop, plen, 1);

  nxu16_asm_len ("sbrc %1,7" CR_TAB
	       "ld %3,%a2", xop, plen, 2);

  if (REGNO (xop[0]) != REGNO (xop[3]))
    nxu16_asm_len ("mov %0,%3", xop, plen, 1);

  return "";
}


/* A helper for `output_reload_insisf' and `output_reload_inhi'.  */
/* Set register OP[0] to compile-time constant OP[1].
   CLOBBER_REG is a QI clobber register or NULL_RTX.
   LEN == NULL: output instructions.
   LEN != NULL: set *LEN to the length of the instruction sequence
		(in words) printed with LEN = NULL.
   If CLEAR_P is true, OP[0] had been cleard to Zero already.
   If CLEAR_P is false, nothing is known about OP[0].

   The effect on cc0 is as follows:

   Load 0 to any register except ZERO_REG : NONE
   Load ld register with any value        : NONE
   Anything else:                         : CLOBBER  */

static void
output_reload_in_const (rtx *op, rtx clobber_reg, int *len, bool clear_p)
{
  rtx src = op[1];
  rtx dest = op[0];
  rtx xval, xdest[4];
  int ival[4];
  int clobber_val = 1234;
  bool cooked_clobber_p = false;
  bool set_p = false;
  machine_mode mode = GET_MODE (dest);
  int n_bytes = GET_MODE_SIZE (mode);

  gcc_assert (REG_P (dest)
	      && CONSTANT_P (src));

  if (len)
    *len = 0;

  /* (REG:SI 14) is special: It's neither in LD_REGS nor in NO_LD_REGS
     but has some subregs that are in LD_REGS.  Use the MSB (REG:QI 17).  */

  if (REGNO (dest) < REG_16
      && END_REGNO (dest) > REG_16)
    {
      clobber_reg = all_regs_rtx[END_REGNO (dest) - 1];
    }

  /* We might need a clobber reg but don't have one.  Look at the value to
     be loaded more closely.  A clobber is only needed if it is a symbol
     or contains a byte that is neither 0, -1 or a power of 2.  */

  if (NULL_RTX == clobber_reg
      && !test_hard_reg_class (LD_REGS, dest)
      && (! (CONST_INT_P (src) || CONST_FIXED_P (src) || CONST_DOUBLE_P (src))
	  || !nxu16_popcount_each_byte (src, n_bytes,
				      (1 << 0) | (1 << 1) | (1 << 8))))
    {
      /* We have no clobber register but need one.  Cook one up.
	 That's cheaper than loading from constant pool.  */

      cooked_clobber_p = true;
      clobber_reg = all_regs_rtx[REG_Z + 1];
      nxu16_asm_len ("mov __tmp_reg__,%0", &clobber_reg, len, 1);
    }

  /* Now start filling DEST from LSB to MSB.  */

  for (int n = 0; n < n_bytes; n++)
    {
      bool done_byte = false;
      rtx xop[3];

      /* Crop the n-th destination byte.  */

      xdest[n] = nxu16_byte (dest, n);
      int ldreg_p = test_hard_reg_class (LD_REGS, xdest[n]);

      if (!CONST_INT_P (src)
	  && !CONST_FIXED_P (src)
	  && !CONST_DOUBLE_P (src))
	{
	  static const char *const asm_code[][2] =
	    {
	      { "ldi %2,lo8(%1)"  CR_TAB "mov %0,%2",    "ldi %0,lo8(%1)"  },
	      { "ldi %2,hi8(%1)"  CR_TAB "mov %0,%2",    "ldi %0,hi8(%1)"  },
	      { "ldi %2,hlo8(%1)" CR_TAB "mov %0,%2",    "ldi %0,hlo8(%1)" },
	      { "ldi %2,hhi8(%1)" CR_TAB "mov %0,%2",    "ldi %0,hhi8(%1)" }
	    };

	  xop[0] = xdest[n];
	  xop[1] = src;
	  xop[2] = clobber_reg;

	  nxu16_asm_len (asm_code[n][ldreg_p], xop, len, ldreg_p ? 1 : 2);

	  continue;
	}

      /* Crop the n-th source byte.  */

      xval = nxu16_byte (src, n);
      ival[n] = INTVAL (xval);

      /* Look if we can reuse the low word by means of MOVW.  */

      if (n == 2
	  && n_bytes >= 4
	  && nxu16_HAVE_MOVW)
	{
	  int lo16 = nxu16_int16 (src, 0);
	  int hi16 = nxu16_int16 (src, 2);

	  if (lo16 == hi16)
	    {
	      if (lo16 != 0 || ! clear_p)
		nxu16_asm_len ("movw %C0,%A0", &op[0], len, 1);

	      break;
	    }
	}

      /* Don't use CLR so that cc0 is set as expected.  */

      if (ival[n] == 0)
	{
	  if (!clear_p)
	    nxu16_asm_len (ldreg_p ? "ldi %0,0"
			 : nxu16_ZERO_REGNO == REGNO (xdest[n]) ? "clr %0"
			 : "mov %0,__zero_reg__",
			 &xdest[n], len, 1);
	  continue;
	}

      if (clobber_val == ival[n]
	  && REGNO (clobber_reg) == REGNO (xdest[n]))
	{
	  continue;
	}

      /* LD_REGS can use LDI to move a constant value */

      if (ldreg_p)
	{
	  xop[0] = xdest[n];
	  xop[1] = xval;
	  nxu16_asm_len ("ldi %0,lo8(%1)", xop, len, 1);
	  continue;
	}

      /* Try to reuse value already loaded in some lower byte. */

      for (int j = 0; j < n; j++)
	if (ival[j] == ival[n])
	  {
	    xop[0] = xdest[n];
	    xop[1] = xdest[j];

	    nxu16_asm_len ("mov %0,%1", xop, len, 1);
	    done_byte = true;
	    break;
	  }

      if (done_byte)
	continue;

      /* Need no clobber reg for -1: Use CLR/DEC */

      if (ival[n] == -1)
	{
	  if (!clear_p)
	    nxu16_asm_len ("clr %0", &xdest[n], len, 1);

	  nxu16_asm_len ("dec %0", &xdest[n], len, 1);
	  continue;
	}
      else if (ival[n] == 1)
	{
	  if (!clear_p)
	    nxu16_asm_len ("clr %0", &xdest[n], len, 1);

	  nxu16_asm_len ("inc %0", &xdest[n], len, 1);
	  continue;
	}

      /* Use T flag or INC to manage powers of 2 if we have
	 no clobber reg.  */

      if (NULL_RTX == clobber_reg
	  && single_one_operand (xval, QImode))
	{
	  xop[0] = xdest[n];
	  xop[1] = GEN_INT (exact_log2 (ival[n] & GET_MODE_MASK (QImode)));

	  gcc_assert (constm1_rtx != xop[1]);

	  if (!set_p)
	    {
	      set_p = true;
	      nxu16_asm_len ("set", xop, len, 1);
	    }

	  if (!clear_p)
	    nxu16_asm_len ("clr %0", xop, len, 1);

	  nxu16_asm_len ("bld %0,%1", xop, len, 1);
	  continue;
	}

      /* We actually need the LD_REGS clobber reg.  */

      gcc_assert (NULL_RTX != clobber_reg);

      xop[0] = xdest[n];
      xop[1] = xval;
      xop[2] = clobber_reg;
      clobber_val = ival[n];

      nxu16_asm_len ("ldi %2,lo8(%1)" CR_TAB
		   "mov %0,%2", xop, len, 2);
    }

  /* If we cooked up a clobber reg above, restore it.  */

  if (cooked_clobber_p)
    {
      nxu16_asm_len ("mov %0,__tmp_reg__", &clobber_reg, len, 1);
    }
}




const char *
output_movhi (rtx_insn *insn, rtx xop[], int *plen)
{
  rtx dest = xop[0];
  rtx src = xop[1];

  gcc_assert (GET_MODE_SIZE (GET_MODE (dest)) == 2);

  if (nxu16_mem_flash_p (src)
      || nxu16_mem_flash_p (dest))
    {
      return nxu16_out_lpm (insn, xop, plen);
    }

  if (REG_P (dest))
    {
      if (REG_P (src)) /* mov r,r */
	{
	  if (test_hard_reg_class (STACK_REG, dest))
	    {
	      if (nxu16_HAVE_8BIT_SP)
		return nxu16_asm_len ("out __SP_L__,%A1", xop, plen, -1);

	      if (nxu16_XMEGA)
		return nxu16_asm_len ("out __SP_L__,%A1" CR_TAB
				    "out __SP_H__,%B1", xop, plen, -2);

	      /* Use simple load of SP if no interrupts are  used.  */

	      return TARGET_NO_INTERRUPTS
		? nxu16_asm_len ("out __SP_H__,%B1" CR_TAB
			       "out __SP_L__,%A1", xop, plen, -2)
		: nxu16_asm_len ("in __tmp_reg__,__SREG__"  CR_TAB
			       "cli"                      CR_TAB
			       "out __SP_H__,%B1"         CR_TAB
			       "out __SREG__,__tmp_reg__" CR_TAB
			       "out __SP_L__,%A1", xop, plen, -5);
	    }
	  else if (test_hard_reg_class (STACK_REG, src))
	    {
	      return !nxu16_HAVE_SPH
		? nxu16_asm_len ("in %A0,__SP_L__" CR_TAB
			       "clr %B0", xop, plen, -2)

		: nxu16_asm_len ("in %A0,__SP_L__" CR_TAB
			       "in %B0,__SP_H__", xop, plen, -2);
	    }

	  return nxu16_HAVE_MOVW
	    ? nxu16_asm_len ("movw %0,%1", xop, plen, -1)

	    : nxu16_asm_len ("mov %A0,%A1" CR_TAB
			   "mov %B0,%B1", xop, plen, -2);
	} /* REG_P (src) */
      else if (CONSTANT_P (src))
	{
	  return output_reload_inhi (xop, NULL_RTX, plen);
	}
      else if (MEM_P (src))
	{
	  return out_movhi_r_mr (insn, xop, plen); /* mov r,m */
	}
    }
  else if (MEM_P (dest))
    {
      rtx xop[2];

      xop[0] = dest;
      xop[1] = src == CONST0_RTX (GET_MODE (dest)) ? zero_reg_rtx : src;

      return out_movhi_mr_r (insn, xop, plen);
    }

  fatal_insn ("invalid insn:", insn);

  return "";
}


/* Same as out_movqi_r_mr, but TINY does not have ADIW or SBIW */

static const char *
nxu16_out_movqi_r_mr_reg_disp_tiny (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx x = XEXP (src, 0);
  rtx base = XEXP (x, 0);

  if (plen)
    *plen = 0;

  if (!reg_overlap_mentioned_p (dest, base))
    {
      nxu16_asm_len (TINY_ADIW (%I1, %J1, %o1) CR_TAB
		   "ld %0,%b1", op, plen, 3);
      if (!reg_unused_after (insn, base))
	nxu16_asm_len (TINY_SBIW (%I1, %J1, %o1), op, plen, 2);
    }
  else
    {
      // PR98762: The base register overlaps dest and is only partly clobbered.
      rtx base2 = all_regs_rtx[1 ^ REGNO (dest)];

      if (!reg_unused_after (insn, base2))
	nxu16_asm_len ("mov __tmp_reg__,%0" , &base2, plen, 1);
      nxu16_asm_len (TINY_ADIW (%I1, %J1, %o1) CR_TAB
		   "ld %0,%b1", op, plen, 3);
      if (!reg_unused_after (insn, base2))
	nxu16_asm_len ("mov %0,__tmp_reg__" , &base2, plen, 1);
    }

  return "";
}

static const char *
out_movqi_r_mr (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx x = XEXP (src, 0);

  if (CONSTANT_ADDRESS_P (x))
    {
      int n_words = nxu16_TINY ? 1 : 2;
      return io_address_operand (x, QImode)
	? nxu16_asm_len ("in %0,%i1", op, plen, -1)
	: nxu16_asm_len ("lds %0,%m1", op, plen, -n_words);
    }

  if (GET_CODE (x) == PLUS
      && REG_P (XEXP (x, 0))
      && CONST_INT_P (XEXP (x, 1)))
    {
      /* memory access by reg+disp */

      int disp = INTVAL (XEXP (x, 1));

      if (nxu16_TINY)
	return nxu16_out_movqi_r_mr_reg_disp_tiny (insn, op, plen);

      if (disp - GET_MODE_SIZE (GET_MODE (src)) >= 63)
	{
	  if (REGNO (XEXP (x, 0)) != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (src)))
	    return nxu16_asm_len ("adiw r28,%o1-63" CR_TAB
				"ldd %0,Y+63"     CR_TAB
				"sbiw r28,%o1-63", op, plen, -3);

	  return nxu16_asm_len ("subi r28,lo8(-%o1)" CR_TAB
			      "sbci r29,hi8(-%o1)" CR_TAB
			      "ld %0,Y"            CR_TAB
			      "subi r28,lo8(%o1)"  CR_TAB
			      "sbci r29,hi8(%o1)", op, plen, -5);
	}
      else if (REGNO (XEXP (x, 0)) == REG_X)
	{
	  /* This is a paranoid case LEGITIMIZE_RELOAD_ADDRESS must exclude
	     it but I have this situation with extremal optimizing options.  */

	  nxu16_asm_len ("adiw r26,%o1" CR_TAB
		       "ld %0,X", op, plen, -2);

	  if (!reg_overlap_mentioned_p (dest, XEXP (x, 0))
	      && !reg_unused_after (insn, XEXP (x, 0)))
	    {
	      nxu16_asm_len ("sbiw r26,%o1", op, plen, 1);
	    }

	  return "";
	}

      return nxu16_asm_len ("ldd %0,%1", op, plen, -1);
    }

  return nxu16_asm_len ("ld %0,%1", op, plen, -1);
}


/* Same as movhi_r_mr, but TINY does not have ADIW, SBIW and LDD */

static const char *
nxu16_out_movhi_r_mr_reg_no_disp_tiny (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);

  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (base);

  if (reg_dest == reg_base)  /* R = (R) */
    return nxu16_asm_len ("ld __tmp_reg__,%1+" CR_TAB
			"ld %B0,%1"          CR_TAB
			"mov %A0,__tmp_reg__", op, plen, -3);

  nxu16_asm_len ("ld %A0,%1+" CR_TAB
	       "ld %B0,%1", op, plen, -2);

  if (!reg_unused_after (insn, base))
    nxu16_asm_len (TINY_SBIW (%E1, %F1, 1), op, plen, 2);

  return "";
}


/* Same as movhi_r_mr, but TINY does not have ADIW, SBIW and LDD */

static const char *
nxu16_out_movhi_r_mr_reg_disp_tiny (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);

  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (XEXP (base, 0));

  if (reg_base == reg_dest)
    {
      return nxu16_asm_len (TINY_ADIW (%I1, %J1, %o1) CR_TAB
			  "ld __tmp_reg__,%b1+"     CR_TAB
			  "ld %B0,%b1"              CR_TAB
			  "mov %A0,__tmp_reg__", op, plen, -5);
    }
  else
    {
      nxu16_asm_len (TINY_ADIW (%I1, %J1, %o1) CR_TAB
		   "ld %A0,%b1+"             CR_TAB
		   "ld %B0,%b1", op, plen, -4);

      if (!reg_unused_after (insn, XEXP (base, 0)))
	nxu16_asm_len (TINY_SBIW (%I1, %J1, %o1+1), op, plen, 2);

      return "";
    }
}


/* Same as movhi_r_mr, but TINY does not have ADIW, SBIW and LDD */

static const char *
nxu16_out_movhi_r_mr_pre_dec_tiny (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);

  /* "volatile" forces reading low byte first, even if less efficient,
     for correct operation with 16-bit I/O registers.  */
  bool mem_volatile_p = MEM_VOLATILE_P (src);

  if (reg_overlap_mentioned_p (dest, XEXP (base, 0)))
    fatal_insn ("incorrect insn:", insn);

  if (!mem_volatile_p)
    return nxu16_asm_len ("ld %B0,%1" CR_TAB
			"ld %A0,%1", op, plen, -2);

  return nxu16_asm_len (TINY_SBIW (%I1, %J1, 2)  CR_TAB
		      "ld %A0,%p1+"            CR_TAB
		      "ld %B0,%p1"             CR_TAB
		      TINY_SBIW (%I1, %J1, 1), op, plen, -6);
}


static const char *
out_movhi_r_mr (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);
  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (base);
  /* "volatile" forces reading low byte first, even if less efficient,
     for correct operation with 16-bit I/O registers.  */
  bool mem_volatile_p = MEM_VOLATILE_P (src);

  if (reg_base > 0)
    {
      if (nxu16_TINY)
	return nxu16_out_movhi_r_mr_reg_no_disp_tiny (insn, op, plen);

      if (reg_dest == reg_base)         /* R = (R) */
	return nxu16_asm_len ("ld __tmp_reg__,%1+" CR_TAB
			    "ld %B0,%1"          CR_TAB
			    "mov %A0,__tmp_reg__", op, plen, -3);

      if (reg_base != REG_X)
	return nxu16_asm_len ("ld %A0,%1" CR_TAB
			    "ldd %B0,%1+1", op, plen, -2);

      nxu16_asm_len ("ld %A0,X+" CR_TAB
		   "ld %B0,X", op, plen, -2);

      if (!reg_unused_after (insn, base))
	nxu16_asm_len ("sbiw r26,1", op, plen, 1);

      return "";
    }
  else if (GET_CODE (base) == PLUS) /* (R + i) */
    {
      int disp = INTVAL (XEXP (base, 1));
      int reg_base = true_regnum (XEXP (base, 0));

      if (nxu16_TINY)
	return nxu16_out_movhi_r_mr_reg_disp_tiny (insn, op, plen);

      if (disp > MAX_LD_OFFSET (GET_MODE (src)))
	{
	  if (REGNO (XEXP (base, 0)) != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  return disp <= 63 + MAX_LD_OFFSET (GET_MODE (src))
	    ? nxu16_asm_len ("adiw r28,%o1-62" CR_TAB
			   "ldd %A0,Y+62"    CR_TAB
			   "ldd %B0,Y+63"    CR_TAB
			   "sbiw r28,%o1-62", op, plen, -4)

	    : nxu16_asm_len ("subi r28,lo8(-%o1)" CR_TAB
			   "sbci r29,hi8(-%o1)" CR_TAB
			   "ld %A0,Y"           CR_TAB
			   "ldd %B0,Y+1"        CR_TAB
			   "subi r28,lo8(%o1)"  CR_TAB
			   "sbci r29,hi8(%o1)", op, plen, -6);
	}

      /* This is a paranoid case. LEGITIMIZE_RELOAD_ADDRESS must exclude
	 it but I have this situation with extremal
	 optimization options.  */

      if (reg_base == REG_X)
	{
	  if (reg_base == reg_dest)
	    return nxu16_asm_len ("adiw r26,%o1"      CR_TAB
				"ld __tmp_reg__,X+" CR_TAB
				"ld %B0,X"          CR_TAB
				"mov %A0,__tmp_reg__", op, plen, -4);

	  nxu16_asm_len ("adiw r26,%o1" CR_TAB
		       "ld %A0,X+"    CR_TAB
		       "ld %B0,X", op, plen, -3);

	  if (!reg_unused_after (insn, XEXP (base, 0)))
	    nxu16_asm_len ("sbiw r26,%o1+1", op, plen, 1);

	  return "";
	}

      return reg_base == reg_dest
	? nxu16_asm_len ("ldd __tmp_reg__,%A1" CR_TAB
		       "ldd %B0,%B1"         CR_TAB
		       "mov %A0,__tmp_reg__", op, plen, -3)

	: nxu16_asm_len ("ldd %A0,%A1" CR_TAB
		       "ldd %B0,%B1", op, plen, -2);
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    {
      if (nxu16_TINY)
	return nxu16_out_movhi_r_mr_pre_dec_tiny (insn, op, plen);

      if (reg_overlap_mentioned_p (dest, XEXP (base, 0)))
	fatal_insn ("incorrect insn:", insn);

      if (!mem_volatile_p)
	return nxu16_asm_len ("ld %B0,%1" CR_TAB
			    "ld %A0,%1", op, plen, -2);

      return REGNO (XEXP (base, 0)) == REG_X
	? nxu16_asm_len ("sbiw r26,2"  CR_TAB
		       "ld %A0,X+"   CR_TAB
		       "ld %B0,X"    CR_TAB
		       "sbiw r26,1", op, plen, -4)

	: nxu16_asm_len ("sbiw %r1,2"  CR_TAB
		       "ld %A0,%p1"  CR_TAB
		       "ldd %B0,%p1+1", op, plen, -3);
    }
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    {
      if (reg_overlap_mentioned_p (dest, XEXP (base, 0)))
	fatal_insn ("incorrect insn:", insn);

      return nxu16_asm_len ("ld %A0,%1"  CR_TAB
			  "ld %B0,%1", op, plen, -2);
    }
  else if (CONSTANT_ADDRESS_P (base))
    {
      int n_words = nxu16_TINY ? 2 : 4;
      return io_address_operand (base, HImode)
	? nxu16_asm_len ("in %A0,%i1" CR_TAB
		       "in %B0,%i1+1", op, plen, -2)

	: nxu16_asm_len ("lds %A0,%m1" CR_TAB
		       "lds %B0,%m1+1", op, plen, -n_words);
    }

  fatal_insn ("unknown move insn:",insn);
  return "";
}

static const char *
nxu16_out_movsi_r_mr_reg_no_disp_tiny (rtx_insn *insn, rtx op[], int *l)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);
  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (base);

  if (reg_dest == reg_base)
    {
      /* "ld r26,-X" is undefined */
      return *l = 9, (TINY_ADIW (%E1, %F1, 3) CR_TAB
		      "ld %D0,%1"             CR_TAB
		      "ld %C0,-%1"            CR_TAB
		      "ld __tmp_reg__,-%1"    CR_TAB
		      TINY_SBIW (%E1, %F1, 1) CR_TAB
		      "ld %A0,%1"             CR_TAB
		      "mov %B0,__tmp_reg__");
    }
  else if (reg_dest == reg_base - 2)
    {
      return *l = 5, ("ld %A0,%1+"            CR_TAB
		      "ld %B0,%1+"            CR_TAB
		      "ld __tmp_reg__,%1+"    CR_TAB
		      "ld %D0,%1"             CR_TAB
		      "mov %C0,__tmp_reg__");
    }
  else if (reg_unused_after (insn, base))
    {
      return *l = 4, ("ld %A0,%1+"    CR_TAB
		      "ld %B0,%1+"    CR_TAB
		      "ld %C0,%1+"    CR_TAB
		      "ld %D0,%1");
    }
  else
    {
      return *l = 6, ("ld %A0,%1+"    CR_TAB
		      "ld %B0,%1+"    CR_TAB
		      "ld %C0,%1+"    CR_TAB
		      "ld %D0,%1"     CR_TAB
		      TINY_SBIW (%E1, %F1, 3));
    }
}


static const char *
nxu16_out_movsi_r_mr_reg_disp_tiny (rtx_insn *insn, rtx op[], int *l)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);
  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (XEXP (base, 0));

  if (reg_dest == reg_base)
    {
      /* "ld r26,-X" is undefined */
      return *l = 9, (TINY_ADIW (%I1, %J1, %o1+3) CR_TAB
		      "ld %D0,%b1"                CR_TAB
		      "ld %C0,-%b1"               CR_TAB
		      "ld __tmp_reg__,-%b1"       CR_TAB
		      TINY_SBIW (%I1, %J1, 1)     CR_TAB
		      "ld %A0,%b1"                CR_TAB
		      "mov %B0,__tmp_reg__");
    }
  else if (reg_dest == reg_base - 2)
    {
      return *l = 7, (TINY_ADIW (%I1, %J1, %o1) CR_TAB
		      "ld %A0,%b1+"             CR_TAB
		      "ld %B0,%b1+"             CR_TAB
		      "ld __tmp_reg__,%b1+"     CR_TAB
		      "ld %D0,%b1"              CR_TAB
		      "mov %C0,__tmp_reg__");
    }
  else if (reg_unused_after (insn, XEXP (base, 0)))
    {
      return *l = 6, (TINY_ADIW (%I1, %J1, %o1) CR_TAB
		      "ld %A0,%b1+"             CR_TAB
		      "ld %B0,%b1+"             CR_TAB
		      "ld %C0,%b1+"             CR_TAB
		      "ld %D0,%b1");
    }
  else
    {
      return *l = 8, (TINY_ADIW (%I1, %J1, %o1)  CR_TAB
		      "ld %A0,%b1+"              CR_TAB
		      "ld %B0,%b1+"              CR_TAB
		      "ld %C0,%b1+"              CR_TAB
		      "ld %D0,%b1"               CR_TAB
		      TINY_SBIW (%I1, %J1, %o1+3));
    }
}

static const char *
out_movsi_r_mr (rtx_insn *insn, rtx op[], int *l)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);
  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (base);
  int tmp;

  if (!l)
    l = &tmp;

  if (reg_base > 0)
    {
      if (nxu16_TINY)
	return nxu16_out_movsi_r_mr_reg_no_disp_tiny (insn, op, l);

      if (reg_base == REG_X)  /* (R26) */
	{
	  if (reg_dest == REG_X)
	    /* "ld r26,-X" is undefined */
	    return *l=7, ("adiw r26,3"        CR_TAB
			  "ld r29,X"          CR_TAB
			  "ld r28,-X"         CR_TAB
			  "ld __tmp_reg__,-X" CR_TAB
			  "sbiw r26,1"        CR_TAB
			  "ld r26,X"          CR_TAB
			  "mov r27,__tmp_reg__");
	  else if (reg_dest == REG_X - 2)
	    return *l=5, ("ld %A0,X+"          CR_TAB
			  "ld %B0,X+"          CR_TAB
			  "ld __tmp_reg__,X+"  CR_TAB
			  "ld %D0,X"           CR_TAB
			  "mov %C0,__tmp_reg__");
	  else if (reg_unused_after (insn, base))
	    return  *l=4, ("ld %A0,X+" CR_TAB
			   "ld %B0,X+" CR_TAB
			   "ld %C0,X+" CR_TAB
			   "ld %D0,X");
	  else
	    return  *l=5, ("ld %A0,X+" CR_TAB
			   "ld %B0,X+" CR_TAB
			   "ld %C0,X+" CR_TAB
			   "ld %D0,X"  CR_TAB
			   "sbiw r26,3");
	}
      else
	{
	  if (reg_dest == reg_base)
	    return *l=5, ("ldd %D0,%1+3" CR_TAB
			  "ldd %C0,%1+2" CR_TAB
			  "ldd __tmp_reg__,%1+1"  CR_TAB
			  "ld %A0,%1"  CR_TAB
			  "mov %B0,__tmp_reg__");
	  else if (reg_base == reg_dest + 2)
	    return *l=5, ("ld %A0,%1"             CR_TAB
			  "ldd %B0,%1+1"          CR_TAB
			  "ldd __tmp_reg__,%1+2"  CR_TAB
			  "ldd %D0,%1+3"          CR_TAB
			  "mov %C0,__tmp_reg__");
	  else
	    return *l=4, ("ld %A0,%1"    CR_TAB
			  "ldd %B0,%1+1" CR_TAB
			  "ldd %C0,%1+2" CR_TAB
			  "ldd %D0,%1+3");
	}
    }
  else if (GET_CODE (base) == PLUS) /* (R + i) */
    {
      int disp = INTVAL (XEXP (base, 1));

      if (nxu16_TINY)
	return nxu16_out_movsi_r_mr_reg_disp_tiny (insn, op, l);

      if (disp > MAX_LD_OFFSET (GET_MODE (src)))
	{
	  if (REGNO (XEXP (base, 0)) != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (src)))
	    return *l = 6, ("adiw r28,%o1-60" CR_TAB
			    "ldd %A0,Y+60"    CR_TAB
			    "ldd %B0,Y+61"    CR_TAB
			    "ldd %C0,Y+62"    CR_TAB
			    "ldd %D0,Y+63"    CR_TAB
			    "sbiw r28,%o1-60");

	  return *l = 8, ("subi r28,lo8(-%o1)" CR_TAB
			  "sbci r29,hi8(-%o1)" CR_TAB
			  "ld %A0,Y"           CR_TAB
			  "ldd %B0,Y+1"        CR_TAB
			  "ldd %C0,Y+2"        CR_TAB
			  "ldd %D0,Y+3"        CR_TAB
			  "subi r28,lo8(%o1)"  CR_TAB
			  "sbci r29,hi8(%o1)");
	}

      reg_base = true_regnum (XEXP (base, 0));
      if (reg_base == REG_X)
	{
	  /* R = (X + d) */
	  if (reg_dest == REG_X)
	    {
	      *l = 7;
	      /* "ld r26,-X" is undefined */
	      return ("adiw r26,%o1+3"    CR_TAB
		      "ld r29,X"          CR_TAB
		      "ld r28,-X"         CR_TAB
		      "ld __tmp_reg__,-X" CR_TAB
		      "sbiw r26,1"        CR_TAB
		      "ld r26,X"          CR_TAB
		      "mov r27,__tmp_reg__");
	    }
	  *l = 6;
	  if (reg_dest == REG_X - 2)
	    return ("adiw r26,%o1"      CR_TAB
		    "ld r24,X+"         CR_TAB
		    "ld r25,X+"         CR_TAB
		    "ld __tmp_reg__,X+" CR_TAB
		    "ld r27,X"          CR_TAB
		    "mov r26,__tmp_reg__");

	  return ("adiw r26,%o1" CR_TAB
		  "ld %A0,X+"    CR_TAB
		  "ld %B0,X+"    CR_TAB
		  "ld %C0,X+"    CR_TAB
		  "ld %D0,X"     CR_TAB
		  "sbiw r26,%o1+3");
	}
      if (reg_dest == reg_base)
	return *l=5, ("ldd %D0,%D1"          CR_TAB
		      "ldd %C0,%C1"          CR_TAB
		      "ldd __tmp_reg__,%B1"  CR_TAB
		      "ldd %A0,%A1"          CR_TAB
		      "mov %B0,__tmp_reg__");
      else if (reg_dest == reg_base - 2)
	return *l=5, ("ldd %A0,%A1"          CR_TAB
		      "ldd %B0,%B1"          CR_TAB
		      "ldd __tmp_reg__,%C1"  CR_TAB
		      "ldd %D0,%D1"          CR_TAB
		      "mov %C0,__tmp_reg__");
      return *l=4, ("ldd %A0,%A1" CR_TAB
		    "ldd %B0,%B1" CR_TAB
		    "ldd %C0,%C1" CR_TAB
		    "ldd %D0,%D1");
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    return *l=4, ("ld %D0,%1" CR_TAB
		  "ld %C0,%1" CR_TAB
		  "ld %B0,%1" CR_TAB
		  "ld %A0,%1");
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    return *l=4, ("ld %A0,%1" CR_TAB
		  "ld %B0,%1" CR_TAB
		  "ld %C0,%1" CR_TAB
		  "ld %D0,%1");
  else if (CONSTANT_ADDRESS_P (base))
    {
      if (io_address_operand (base, SImode))
	{
	  *l = 4;
	  return ("in %A0,%i1"   CR_TAB
		  "in %B0,%i1+1" CR_TAB
		  "in %C0,%i1+2" CR_TAB
		  "in %D0,%i1+3");
	}
      else
	{
	  *l = nxu16_TINY ? 4 : 8;
	  return ("lds %A0,%m1"   CR_TAB
		  "lds %B0,%m1+1" CR_TAB
		  "lds %C0,%m1+2" CR_TAB
		  "lds %D0,%m1+3");
	}
    }

  fatal_insn ("unknown move insn:",insn);
  return "";
}

static const char *
nxu16_out_movsi_mr_r_reg_no_disp_tiny (rtx_insn *insn, rtx op[], int *l)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = true_regnum (base);
  int reg_src = true_regnum (src);

  if (reg_base == reg_src)
    {
      /* "ld r26,-X" is undefined */
      if (reg_unused_after (insn, base))
	{
	  return *l = 7, ("mov __tmp_reg__,%B1"   CR_TAB
			  "st %0,%A1"             CR_TAB
			  TINY_ADIW (%E0, %F0, 1) CR_TAB
			  "st %0+,__tmp_reg__"    CR_TAB
			  "st %0+,%C1"            CR_TAB
			  "st %0+,%D1");
	}
      else
	{
	  return *l = 9, ("mov __tmp_reg__,%B1"   CR_TAB
			  "st %0,%A1"             CR_TAB
			  TINY_ADIW (%E0, %F0, 1) CR_TAB
			  "st %0+,__tmp_reg__"    CR_TAB
			  "st %0+,%C1"            CR_TAB
			  "st %0+,%D1"            CR_TAB
			  TINY_SBIW (%E0, %F0, 3));
	}
    }
  else if (reg_base == reg_src + 2)
    {
      if (reg_unused_after (insn, base))
	return *l = 7, ("mov __zero_reg__,%C1" CR_TAB
			"mov __tmp_reg__,%D1"  CR_TAB
			"st %0+,%A1"           CR_TAB
			"st %0+,%B1"           CR_TAB
			"st %0+,__zero_reg__"  CR_TAB
			"st %0,__tmp_reg__"    CR_TAB
			"clr __zero_reg__");
      else
	return *l = 9, ("mov __zero_reg__,%C1" CR_TAB
			"mov __tmp_reg__,%D1"  CR_TAB
			"st %0+,%A1"           CR_TAB
			"st %0+,%B1"           CR_TAB
			"st %0+,__zero_reg__"  CR_TAB
			"st %0,__tmp_reg__"    CR_TAB
			"clr __zero_reg__"     CR_TAB
			TINY_SBIW (%E0, %F0, 3));
    }

  return *l = 6, ("st %0+,%A1" CR_TAB
		  "st %0+,%B1" CR_TAB
		  "st %0+,%C1" CR_TAB
		  "st %0,%D1"  CR_TAB
		  TINY_SBIW (%E0, %F0, 3));
}

static const char *
nxu16_out_movsi_mr_r_reg_disp_tiny (rtx op[], int *l)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = REGNO (XEXP (base, 0));
  int reg_src = true_regnum (src);

  if (reg_base == reg_src)
    {
      *l = 11;
      return ("mov __tmp_reg__,%A1"        CR_TAB
	      "mov __zero_reg__,%B1"       CR_TAB
	      TINY_ADIW (%I0, %J0, %o0)    CR_TAB
	      "st %b0+,__tmp_reg__"        CR_TAB
	      "st %b0+,__zero_reg__"       CR_TAB
	      "st %b0+,%C1"                CR_TAB
	      "st %b0,%D1"                 CR_TAB
	      "clr __zero_reg__"           CR_TAB
	      TINY_SBIW (%I0, %J0, %o0+3));
    }
  else if (reg_src == reg_base - 2)
    {
      // This awkward case can occur when ext-dce turns zero-extend:SI(HI)
      // into a paradoxical subreg, which register allocation may turn into
      // something like *(R28:HI + 7) = R26:SI.  There is actually no need
      // to store the upper 2 bytes of R26:SI as they are unused rubbish.
      // See PR116390.
      *l = 6;
      return (TINY_ADIW (%I0, %J0, %o0)     CR_TAB
	      "st %b0+,%A1"                 CR_TAB
	      "st %b0,%B1"                  CR_TAB
	      TINY_SBIW (%I0, %J0, %o0+1));
    }
  *l = 8;
  return (TINY_ADIW (%I0, %J0, %o0)     CR_TAB
	  "st %b0+,%A1"                 CR_TAB
	  "st %b0+,%B1"                 CR_TAB
	  "st %b0+,%C1"                 CR_TAB
	  "st %b0,%D1"                  CR_TAB
	  TINY_SBIW (%I0, %J0, %o0+3));
}

static const char *
out_movsi_mr_r (rtx_insn *insn, rtx op[], int *l)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = true_regnum (base);
  int reg_src = true_regnum (src);
  int tmp;

  if (!l)
    l = &tmp;

  if (CONSTANT_ADDRESS_P (base))
    {
      if (io_address_operand (base, SImode))
	{
	  return *l=4,("out %i0,%A1"   CR_TAB
		       "out %i0+1,%B1" CR_TAB
		       "out %i0+2,%C1" CR_TAB
		       "out %i0+3,%D1");
	}
      else
	{
	  *l = nxu16_TINY ? 4 : 8;
	  return ("sts %m0,%A1"   CR_TAB
		  "sts %m0+1,%B1" CR_TAB
		  "sts %m0+2,%C1" CR_TAB
		  "sts %m0+3,%D1");
	}
    }

  if (reg_base > 0)  /* (r) */
    {
      if (nxu16_TINY)
	return nxu16_out_movsi_mr_r_reg_no_disp_tiny (insn, op, l);

      if (reg_base == REG_X)  /* (R26) */
	{
	  if (reg_src == REG_X)
	    {
	      /* "st X+,r26" is undefined */
	      if (reg_unused_after (insn, base))
		return *l=6, ("mov __tmp_reg__,r27" CR_TAB
			      "st X,r26"            CR_TAB
			      "adiw r26,1"          CR_TAB
			      "st X+,__tmp_reg__"   CR_TAB
			      "st X+,r28"           CR_TAB
			      "st X,r29");
	      else
		return *l=7, ("mov __tmp_reg__,r27" CR_TAB
			      "st X,r26"            CR_TAB
			      "adiw r26,1"          CR_TAB
			      "st X+,__tmp_reg__"   CR_TAB
			      "st X+,r28"           CR_TAB
			      "st X,r29"            CR_TAB
			      "sbiw r26,3");
	    }
	  else if (reg_base == reg_src + 2)
	    {
	      if (reg_unused_after (insn, base))
		return *l=7, ("mov __zero_reg__,%C1" CR_TAB
			      "mov __tmp_reg__,%D1"  CR_TAB
			      "st %0+,%A1"           CR_TAB
			      "st %0+,%B1"           CR_TAB
			      "st %0+,__zero_reg__"  CR_TAB
			      "st %0,__tmp_reg__"    CR_TAB
			      "clr __zero_reg__");
	      else
		return *l=8, ("mov __zero_reg__,%C1" CR_TAB
			      "mov __tmp_reg__,%D1"  CR_TAB
			      "st %0+,%A1"           CR_TAB
			      "st %0+,%B1"           CR_TAB
			      "st %0+,__zero_reg__"  CR_TAB
			      "st %0,__tmp_reg__"    CR_TAB
			      "clr __zero_reg__"     CR_TAB
			      "sbiw r26,3");
	    }
	  return *l=5, ("st %0+,%A1" CR_TAB
			"st %0+,%B1" CR_TAB
			"st %0+,%C1" CR_TAB
			"st %0,%D1"  CR_TAB
			"sbiw r26,3");
	}
      else
	return *l=4, ("st %0,%A1"    CR_TAB
		      "std %0+1,%B1" CR_TAB
		      "std %0+2,%C1" CR_TAB
		      "std %0+3,%D1");
    }
  else if (GET_CODE (base) == PLUS) /* (R + i) */
    {
      int disp = INTVAL (XEXP (base, 1));

      if (nxu16_TINY)
	return nxu16_out_movsi_mr_r_reg_disp_tiny (op, l);

      reg_base = REGNO (XEXP (base, 0));
      if (disp > MAX_LD_OFFSET (GET_MODE (dest)))
	{
	  if (reg_base != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (dest)))
	    return *l = 6, ("adiw r28,%o0-60" CR_TAB
			    "std Y+60,%A1"    CR_TAB
			    "std Y+61,%B1"    CR_TAB
			    "std Y+62,%C1"    CR_TAB
			    "std Y+63,%D1"    CR_TAB
			    "sbiw r28,%o0-60");

	  return *l = 8, ("subi r28,lo8(-%o0)" CR_TAB
			  "sbci r29,hi8(-%o0)" CR_TAB
			  "st Y,%A1"           CR_TAB
			  "std Y+1,%B1"        CR_TAB
			  "std Y+2,%C1"        CR_TAB
			  "std Y+3,%D1"        CR_TAB
			  "subi r28,lo8(%o0)"  CR_TAB
			  "sbci r29,hi8(%o0)");
	}
      if (reg_base == REG_X)
	{
	  /* (X + d) = R */
	  if (reg_src == REG_X)
	    {
	      *l = 9;
	      return ("mov __tmp_reg__,r26"  CR_TAB
		      "mov __zero_reg__,r27" CR_TAB
		      "adiw r26,%o0"         CR_TAB
		      "st X+,__tmp_reg__"    CR_TAB
		      "st X+,__zero_reg__"   CR_TAB
		      "st X+,r28"            CR_TAB
		      "st X,r29"             CR_TAB
		      "clr __zero_reg__"     CR_TAB
		      "sbiw r26,%o0+3");
	    }
	  else if (reg_src == REG_X - 2)
	    {
	      *l = 9;
	      return ("mov __tmp_reg__,r26"  CR_TAB
		      "mov __zero_reg__,r27" CR_TAB
		      "adiw r26,%o0"         CR_TAB
		      "st X+,r24"            CR_TAB
		      "st X+,r25"            CR_TAB
		      "st X+,__tmp_reg__"    CR_TAB
		      "st X,__zero_reg__"    CR_TAB
		      "clr __zero_reg__"     CR_TAB
		      "sbiw r26,%o0+3");
	    }
	  *l = 6;
	  return ("adiw r26,%o0" CR_TAB
		  "st X+,%A1"    CR_TAB
		  "st X+,%B1"    CR_TAB
		  "st X+,%C1"    CR_TAB
		  "st X,%D1"     CR_TAB
		  "sbiw r26,%o0+3");
	}
      return *l=4, ("std %A0,%A1" CR_TAB
		    "std %B0,%B1" CR_TAB
		    "std %C0,%C1" CR_TAB
		    "std %D0,%D1");
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    return *l=4, ("st %0,%D1" CR_TAB
		  "st %0,%C1" CR_TAB
		  "st %0,%B1" CR_TAB
		  "st %0,%A1");
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    return *l=4, ("st %0,%A1" CR_TAB
		  "st %0,%B1" CR_TAB
		  "st %0,%C1" CR_TAB
		  "st %0,%D1");
  fatal_insn ("unknown move insn:",insn);
  return "";
}

const char *
output_movsisf (rtx_insn *insn, rtx operands[], int *l)
{
  int dummy;
  rtx dest = operands[0];
  rtx src = operands[1];
  int *real_l = l;

  if (nxu16_mem_flash_p (src)
      || nxu16_mem_flash_p (dest))
    {
      return nxu16_out_lpm (insn, operands, real_l);
    }

  if (!l)
    l = &dummy;

  gcc_assert (GET_MODE_SIZE (GET_MODE (dest)) == 4);

  if (REG_P (dest))
    {
      if (REG_P (src)) /* mov r,r */
	{
	  if (true_regnum (dest) > true_regnum (src))
	    {
	      if (nxu16_HAVE_MOVW)
		{
		  *l = 2;
		  return ("movw %C0,%C1" CR_TAB
			  "movw %A0,%A1");
		}
	      *l = 4;
	      return ("mov %D0,%D1" CR_TAB
		      "mov %C0,%C1" CR_TAB
		      "mov %B0,%B1" CR_TAB
		      "mov %A0,%A1");
	    }
	  else
	    {
	      if (nxu16_HAVE_MOVW)
		{
		  *l = 2;
		  return ("movw %A0,%A1" CR_TAB
			  "movw %C0,%C1");
		}
	      *l = 4;
	      return ("mov %A0,%A1" CR_TAB
		      "mov %B0,%B1" CR_TAB
		      "mov %C0,%C1" CR_TAB
		      "mov %D0,%D1");
	    }
	}
      else if (CONSTANT_P (src))
	{
	  return output_reload_insisf (operands, NULL_RTX, real_l);
	}
      else if (MEM_P (src))
	return out_movsi_r_mr (insn, operands, real_l); /* mov r,m */
    }
  else if (MEM_P (dest))
    {
      const char *templ;

      if (src == CONST0_RTX (GET_MODE (dest)))
	operands[1] = zero_reg_rtx;

      templ = out_movsi_mr_r (insn, operands, real_l);

      if (!real_l)
	output_asm_insn (templ, operands);

      operands[1] = src;
      return "";
    }
  fatal_insn ("invalid insn:", insn);
  return "";
}


/* Handle loads of 24-bit types from memory to register.  */

static const char *
nxu16_out_load_psi_reg_no_disp_tiny (rtx_insn *insn, rtx *op, int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);
  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (base);

  if (reg_base == reg_dest)
    {
      return nxu16_asm_len (TINY_ADIW (%E1, %F1, 2)   CR_TAB
			  "ld %C0,%1"               CR_TAB
			  "ld __tmp_reg__,-%1"      CR_TAB
			  TINY_SBIW (%E1, %F1, 1)   CR_TAB
			  "ld %A0,%1"               CR_TAB
			  "mov %B0,__tmp_reg__", op, plen, -8);
    }
  else
    {
      nxu16_asm_len ("ld %A0,%1+"  CR_TAB
		   "ld %B0,%1+"  CR_TAB
		   "ld %C0,%1", op, plen, -3);

      if (reg_dest != reg_base - 2
	  && !reg_unused_after (insn, base))
	{
	  nxu16_asm_len (TINY_SBIW (%E1, %F1, 2), op, plen, 2);
	}
      return "";
    }
}

static const char *
nxu16_out_load_psi_reg_disp_tiny (rtx_insn *insn, rtx *op, int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);
  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (base);

  reg_base = true_regnum (XEXP (base, 0));
  if (reg_base == reg_dest)
    {
      return nxu16_asm_len (TINY_ADIW (%I1, %J1, %o1+2) CR_TAB
			  "ld %C0,%b1"                CR_TAB
			  "ld __tmp_reg__,-%b1"       CR_TAB
			  TINY_SBIW (%I1, %J1, 1)     CR_TAB
			  "ld %A0,%b1"                CR_TAB
			  "mov %B0,__tmp_reg__", op, plen, -8);
    }
  else
    {
      nxu16_asm_len (TINY_ADIW (%I1, %J1, %o1)   CR_TAB
		   "ld %A0,%b1+"               CR_TAB
		   "ld %B0,%b1+"               CR_TAB
		   "ld %C0,%b1", op, plen, -5);

      if (reg_dest != reg_base - 2
	  && !reg_unused_after (insn, XEXP (base, 0)))
	nxu16_asm_len (TINY_SBIW (%I1, %J1, %o1+2), op, plen, 2);

      return "";
    }
}

static const char *
nxu16_out_load_psi (rtx_insn *insn, rtx *op, int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (src, 0);
  int reg_dest = true_regnum (dest);
  int reg_base = true_regnum (base);

  if (reg_base > 0)
    {
      if (nxu16_TINY)
	return nxu16_out_load_psi_reg_no_disp_tiny (insn, op, plen);

      if (reg_base == REG_X)  /* (R26) */
	{
	  if (reg_dest == REG_X)
	    /* "ld r26,-X" is undefined */
	    return nxu16_asm_len ("adiw r26,2"        CR_TAB
				"ld r28,X"          CR_TAB
				"ld __tmp_reg__,-X" CR_TAB
				"sbiw r26,1"        CR_TAB
				"ld r26,X"          CR_TAB
				"mov r27,__tmp_reg__", op, plen, -6);
	  else
	    {
	      nxu16_asm_len ("ld %A0,X+" CR_TAB
			   "ld %B0,X+" CR_TAB
			   "ld %C0,X", op, plen, -3);

	      if (reg_dest != REG_X - 2
		  && !reg_unused_after (insn, base))
		{
		  nxu16_asm_len ("sbiw r26,2", op, plen, 1);
		}

	      return "";
	    }
	}
      else /* reg_base != REG_X */
	{
	  if (reg_dest == reg_base)
	    return nxu16_asm_len ("ldd %C0,%1+2"          CR_TAB
				"ldd __tmp_reg__,%1+1"  CR_TAB
				"ld  %A0,%1"            CR_TAB
				"mov %B0,__tmp_reg__", op, plen, -4);
	  else
	    return nxu16_asm_len ("ld  %A0,%1"    CR_TAB
				"ldd %B0,%1+1"  CR_TAB
				"ldd %C0,%1+2", op, plen, -3);
	}
    }
  else if (GET_CODE (base) == PLUS) /* (R + i) */
    {
      int disp = INTVAL (XEXP (base, 1));

      if (nxu16_TINY)
	return nxu16_out_load_psi_reg_disp_tiny (insn, op, plen);

      if (disp > MAX_LD_OFFSET (GET_MODE (src)))
	{
	  if (REGNO (XEXP (base, 0)) != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (src)))
	    return nxu16_asm_len ("adiw r28,%o1-61" CR_TAB
				"ldd %A0,Y+61"    CR_TAB
				"ldd %B0,Y+62"    CR_TAB
				"ldd %C0,Y+63"    CR_TAB
				"sbiw r28,%o1-61", op, plen, -5);

	  return nxu16_asm_len ("subi r28,lo8(-%o1)" CR_TAB
			      "sbci r29,hi8(-%o1)" CR_TAB
			      "ld  %A0,Y"          CR_TAB
			      "ldd %B0,Y+1"        CR_TAB
			      "ldd %C0,Y+2"        CR_TAB
			      "subi r28,lo8(%o1)"  CR_TAB
			      "sbci r29,hi8(%o1)", op, plen, -7);
	}

      reg_base = true_regnum (XEXP (base, 0));
      if (reg_base == REG_X)
	{
	  /* R = (X + d) */
	  if (reg_dest == REG_X)
	    {
	      /* "ld r26,-X" is undefined */
	      return nxu16_asm_len ("adiw r26,%o1+2"     CR_TAB
				  "ld  r28,X"          CR_TAB
				  "ld  __tmp_reg__,-X" CR_TAB
				  "sbiw r26,1"         CR_TAB
				  "ld  r26,X"          CR_TAB
				  "mov r27,__tmp_reg__", op, plen, -6);
	    }

	  nxu16_asm_len ("adiw r26,%o1" CR_TAB
		       "ld %A0,X+"    CR_TAB
		       "ld %B0,X+"    CR_TAB
		       "ld %C0,X", op, plen, -4);

	  if (reg_dest != REG_W
	      && !reg_unused_after (insn, XEXP (base, 0)))
	    nxu16_asm_len ("sbiw r26,%o1+2", op, plen, 1);

	  return "";
	}

      if (reg_dest == reg_base)
	return nxu16_asm_len ("ldd %C0,%C1"          CR_TAB
			    "ldd __tmp_reg__,%B1"  CR_TAB
			    "ldd %A0,%A1"          CR_TAB
			    "mov %B0,__tmp_reg__", op, plen, -4);

      return nxu16_asm_len ("ldd %A0,%A1" CR_TAB
			  "ldd %B0,%B1" CR_TAB
			  "ldd %C0,%C1", op, plen, -3);
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    return nxu16_asm_len ("ld %C0,%1" CR_TAB
			"ld %B0,%1" CR_TAB
			"ld %A0,%1", op, plen, -3);
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    return nxu16_asm_len ("ld %A0,%1" CR_TAB
			"ld %B0,%1" CR_TAB
			"ld %C0,%1", op, plen, -3);

  else if (CONSTANT_ADDRESS_P (base))
    {
      int n_words = nxu16_TINY ? 3 : 6;
      return nxu16_asm_len ("lds %A0,%m1"   CR_TAB
			  "lds %B0,%m1+1" CR_TAB
			  "lds %C0,%m1+2", op, plen , -n_words);
    }

  fatal_insn ("unknown move insn:",insn);
  return "";
}


static const char *
nxu16_out_store_psi_reg_no_disp_tiny (rtx_insn *insn, rtx *op, int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = true_regnum (base);
  int reg_src = true_regnum (src);

  if (reg_base == reg_src)
    {
      nxu16_asm_len ("st %0,%A1"              CR_TAB
		   "mov __tmp_reg__,%B1"    CR_TAB
		   TINY_ADIW (%E0, %F0, 1)  CR_TAB /* st X+, r27 is undefined */
		   "st %0+,__tmp_reg__"     CR_TAB
		   "st %0,%C1", op, plen, -6);

    }
  else if (reg_src == reg_base - 2)
    {
      nxu16_asm_len ("st %0,%A1"              CR_TAB
		   "mov __tmp_reg__,%C1"    CR_TAB
		   TINY_ADIW (%E0, %F0, 1)  CR_TAB
		   "st %0+,%B1"             CR_TAB
		   "st %0,__tmp_reg__", op, plen, 6);
    }
  else
    {
      nxu16_asm_len ("st %0+,%A1"  CR_TAB
		   "st %0+,%B1" CR_TAB
		   "st %0,%C1", op, plen, -3);
    }

  if (!reg_unused_after (insn, base))
    nxu16_asm_len (TINY_SBIW (%E0, %F0, 2), op, plen, 2);

  return "";
}

static const char *
nxu16_out_store_psi_reg_disp_tiny (rtx_insn *insn, rtx *op, int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = REGNO (XEXP (base, 0));
  int reg_src = true_regnum (src);

  if (reg_src == reg_base)
    nxu16_asm_len ("mov __tmp_reg__,%A1"          CR_TAB
		 "mov __zero_reg__,%B1"         CR_TAB
		 TINY_ADIW (%I0, %J0, %o0)      CR_TAB
		 "st %b0+,__tmp_reg__"          CR_TAB
		 "st %b0+,__zero_reg__"         CR_TAB
		 "st %b0,%C1"                   CR_TAB
		 "clr __zero_reg__", op, plen, -8);
  else if (reg_src == reg_base - 2)
    nxu16_asm_len ("mov __tmp_reg__,%C1"          CR_TAB
		 TINY_ADIW (%I0, %J0, %o0)      CR_TAB
		 "st %b0+,%A1"                  CR_TAB
		 "st %b0+,%B1"                  CR_TAB
		 "st %b0,__tmp_reg__", op, plen, -6);
  else
    nxu16_asm_len (TINY_ADIW (%I0, %J0, %o0)      CR_TAB
		 "st %b0+,%A1"                  CR_TAB
		 "st %b0+,%B1"                  CR_TAB
		 "st %b0,%C1", op, plen, -5);

  if (!reg_unused_after (insn, XEXP (base, 0)))
    nxu16_asm_len (TINY_SBIW (%I0, %J0, %o0+2), op, plen, 2);

  return "";
}

/* Handle store of 24-bit type from register or zero to memory.  */

static const char *
nxu16_out_store_psi (rtx_insn *insn, rtx *op, int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = true_regnum (base);

  if (CONSTANT_ADDRESS_P (base))
    {
      int n_words = nxu16_TINY ? 3 : 6;
      return nxu16_asm_len ("sts %m0,%A1"   CR_TAB
			  "sts %m0+1,%B1" CR_TAB
			  "sts %m0+2,%C1", op, plen, -n_words);
    }

  if (reg_base > 0)  /* (r) */
    {
      if (nxu16_TINY)
	return nxu16_out_store_psi_reg_no_disp_tiny (insn, op, plen);

      if (reg_base == REG_X)  /* (R26) */
	{
	  gcc_assert (!reg_overlap_mentioned_p (base, src));

	  nxu16_asm_len ("st %0+,%A1"  CR_TAB
		       "st %0+,%B1" CR_TAB
		       "st %0,%C1", op, plen, -3);

	  if (!reg_unused_after (insn, base))
	    nxu16_asm_len ("sbiw r26,2", op, plen, 1);

	  return "";
	}
      else
	return nxu16_asm_len ("st %0,%A1"    CR_TAB
			    "std %0+1,%B1" CR_TAB
			    "std %0+2,%C1", op, plen, -3);
    }
  else if (GET_CODE (base) == PLUS) /* (R + i) */
    {
      int disp = INTVAL (XEXP (base, 1));

      if (nxu16_TINY)
	return nxu16_out_store_psi_reg_disp_tiny (insn, op, plen);

      reg_base = REGNO (XEXP (base, 0));

      if (disp > MAX_LD_OFFSET (GET_MODE (dest)))
	{
	  if (reg_base != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (dest)))
	    return nxu16_asm_len ("adiw r28,%o0-61" CR_TAB
				"std Y+61,%A1"    CR_TAB
				"std Y+62,%B1"    CR_TAB
				"std Y+63,%C1"    CR_TAB
				"sbiw r28,%o0-61", op, plen, -5);

	  return nxu16_asm_len ("subi r28,lo8(-%o0)" CR_TAB
			      "sbci r29,hi8(-%o0)" CR_TAB
			      "st Y,%A1"           CR_TAB
			      "std Y+1,%B1"        CR_TAB
			      "std Y+2,%C1"        CR_TAB
			      "subi r28,lo8(%o0)"  CR_TAB
			      "sbci r29,hi8(%o0)", op, plen, -7);
	}
      if (reg_base == REG_X)
	{
	  /* (X + d) = R */
	  gcc_assert (!reg_overlap_mentioned_p (XEXP (base, 0), src));

	  nxu16_asm_len ("adiw r26,%o0" CR_TAB
		       "st X+,%A1"    CR_TAB
		       "st X+,%B1"    CR_TAB
		       "st X,%C1", op, plen, -4);

	  if (!reg_unused_after (insn, XEXP (base, 0)))
	    nxu16_asm_len ("sbiw r26,%o0+2", op, plen, 1);

	  return "";
	}

      return nxu16_asm_len ("std %A0,%A1" CR_TAB
			  "std %B0,%B1" CR_TAB
			  "std %C0,%C1", op, plen, -3);
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    return nxu16_asm_len ("st %0,%C1" CR_TAB
			"st %0,%B1" CR_TAB
			"st %0,%A1", op, plen, -3);
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    return nxu16_asm_len ("st %0,%A1" CR_TAB
			"st %0,%B1" CR_TAB
			"st %0,%C1", op, plen, -3);

  fatal_insn ("unknown move insn:",insn);
  return "";
}


/* Move around 24-bit stuff.  */

const char *
nxu16_out_movpsi (rtx_insn *insn, rtx *op, int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];

  if (nxu16_mem_flash_p (src)
      || nxu16_mem_flash_p (dest))
    {
      return nxu16_out_lpm (insn, op, plen);
    }

  if (register_operand (dest, VOIDmode))
    {
      if (register_operand (src, VOIDmode)) /* mov r,r */
	{
	  if (true_regnum (dest) > true_regnum (src))
	    {
	      nxu16_asm_len ("mov %C0,%C1", op, plen, -1);

	      if (nxu16_HAVE_MOVW)
		return nxu16_asm_len ("movw %A0,%A1", op, plen, 1);
	      else
		return nxu16_asm_len ("mov %B0,%B1"  CR_TAB
				    "mov %A0,%A1", op, plen, 2);
	    }
	  else
	    {
	      if (nxu16_HAVE_MOVW)
		nxu16_asm_len ("movw %A0,%A1", op, plen, -1);
	      else
		nxu16_asm_len ("mov %A0,%A1"  CR_TAB
			     "mov %B0,%B1", op, plen, -2);

	      return nxu16_asm_len ("mov %C0,%C1", op, plen, 1);
	    }
	}
      else if (CONSTANT_P (src))
	{
	  return nxu16_out_reload_inpsi (op, NULL_RTX, plen);
	}
      else if (MEM_P (src))
	return nxu16_out_load_psi (insn, op, plen); /* mov r,m */
    }
  else if (MEM_P (dest))
    {
      rtx xop[2];

      xop[0] = dest;
      xop[1] = src == CONST0_RTX (GET_MODE (dest)) ? zero_reg_rtx : src;

      return nxu16_out_store_psi (insn, xop, plen);
    }

  fatal_insn ("invalid insn:", insn);
  return "";
}

static const char *
nxu16_out_movqi_mr_r_reg_disp_tiny (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx x = XEXP (dest, 0);

  if (reg_overlap_mentioned_p (src, XEXP (x, 0)))
    {
      nxu16_asm_len ("mov __tmp_reg__,%1"      CR_TAB
		   TINY_ADIW (%I0, %J0, %o0) CR_TAB
		   "st %b0,__tmp_reg__", op, plen, -4);
    }
  else
    {
      nxu16_asm_len (TINY_ADIW (%I0, %J0, %o0) CR_TAB
		   "st %b0,%1", op, plen, -3);
    }

  if (!reg_unused_after (insn, XEXP (x, 0)))
    nxu16_asm_len (TINY_SBIW (%I0, %J0, %o0), op, plen, 2);

  return "";
}

static const char *
out_movqi_mr_r (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx x = XEXP (dest, 0);

  if (CONSTANT_ADDRESS_P (x))
    {
      int n_words = nxu16_TINY ? 1 : 2;
      return io_address_operand (x, QImode)
	? nxu16_asm_len ("out %i0,%1", op, plen, -1)
	: nxu16_asm_len ("sts %m0,%1", op, plen, -n_words);
    }
  else if (GET_CODE (x) == PLUS
	   && REG_P (XEXP (x, 0))
	   && CONST_INT_P (XEXP (x, 1)))
    {
      /* memory access by reg+disp */

      int disp = INTVAL (XEXP (x, 1));

      if (nxu16_TINY)
	return nxu16_out_movqi_mr_r_reg_disp_tiny (insn, op, plen);

      if (disp - GET_MODE_SIZE (GET_MODE (dest)) >= 63)
	{
	  if (REGNO (XEXP (x, 0)) != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  if (disp <= 63 + MAX_LD_OFFSET (GET_MODE (dest)))
	    return nxu16_asm_len ("adiw r28,%o0-63" CR_TAB
				"std Y+63,%1"     CR_TAB
				"sbiw r28,%o0-63", op, plen, -3);

	  return nxu16_asm_len ("subi r28,lo8(-%o0)" CR_TAB
			      "sbci r29,hi8(-%o0)" CR_TAB
			      "st Y,%1"            CR_TAB
			      "subi r28,lo8(%o0)"  CR_TAB
			      "sbci r29,hi8(%o0)", op, plen, -5);
	}
      else if (REGNO (XEXP (x, 0)) == REG_X)
	{
	  if (reg_overlap_mentioned_p (src, XEXP (x, 0)))
	    {
	      nxu16_asm_len ("mov __tmp_reg__,%1" CR_TAB
			   "adiw r26,%o0"       CR_TAB
			   "st X,__tmp_reg__", op, plen, -3);
	    }
	  else
	    {
	      nxu16_asm_len ("adiw r26,%o0" CR_TAB
			   "st X,%1", op, plen, -2);
	    }

	  if (!reg_unused_after (insn, XEXP (x, 0)))
	    nxu16_asm_len ("sbiw r26,%o0", op, plen, 1);

	  return "";
	}

      return nxu16_asm_len ("std %0,%1", op, plen, -1);
    }

  return nxu16_asm_len ("st %0,%1", op, plen, -1);
}


/* Helper for the next function for XMEGA.  It does the same
   but with low byte first.  */

static const char *
nxu16_out_movhi_mr_r_xmega (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = true_regnum (base);
  int reg_src = true_regnum (src);

  /* "volatile" forces writing low byte first, even if less efficient,
     for correct operation with 16-bit I/O registers like SP.  */
  bool mem_volatile_p = MEM_VOLATILE_P (dest);

  if (CONSTANT_ADDRESS_P (base))
    {
      return io_address_operand (base, HImode)
	? nxu16_asm_len ("out %i0,%A1" CR_TAB
		       "out %i0+1,%B1", op, plen, -2)

	: nxu16_asm_len ("sts %m0,%A1" CR_TAB
		       "sts %m0+1,%B1", op, plen, -4);
    }

  if (reg_base > 0)
    {
      if (reg_base != REG_X)
	return nxu16_asm_len ("st %0,%A1" CR_TAB
			    "std %0+1,%B1", op, plen, -2);

      if (reg_src == REG_X)
	/* "st X+,r26" and "st -X,r26" are undefined.  */
	nxu16_asm_len ("mov __tmp_reg__,r27" CR_TAB
		     "st X,r26"            CR_TAB
		     "adiw r26,1"          CR_TAB
		     "st X,__tmp_reg__", op, plen, -4);
      else
	nxu16_asm_len ("st X+,%A1" CR_TAB
		     "st X,%B1", op, plen, -2);

      return reg_unused_after (insn, base)
	? ""
	: nxu16_asm_len ("sbiw r26,1", op, plen, 1);
    }
  else if (GET_CODE (base) == PLUS)
    {
      int disp = INTVAL (XEXP (base, 1));
      reg_base = REGNO (XEXP (base, 0));
      if (disp > MAX_LD_OFFSET (GET_MODE (dest)))
	{
	  if (reg_base != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  return disp <= 63 + MAX_LD_OFFSET (GET_MODE (dest))
	    ? nxu16_asm_len ("adiw r28,%o0-62" CR_TAB
			   "std Y+62,%A1"    CR_TAB
			   "std Y+63,%B1"    CR_TAB
			   "sbiw r28,%o0-62", op, plen, -4)

	    : nxu16_asm_len ("subi r28,lo8(-%o0)" CR_TAB
			   "sbci r29,hi8(-%o0)" CR_TAB
			   "st Y,%A1"           CR_TAB
			   "std Y+1,%B1"        CR_TAB
			   "subi r28,lo8(%o0)"  CR_TAB
			   "sbci r29,hi8(%o0)", op, plen, -6);
	}

      if (reg_base != REG_X)
	return nxu16_asm_len ("std %A0,%A1" CR_TAB
			    "std %B0,%B1", op, plen, -2);
      /* (X + d) = R */
      return reg_src == REG_X
	? nxu16_asm_len ("mov __tmp_reg__,r26"  CR_TAB
		       "mov __zero_reg__,r27" CR_TAB
		       "adiw r26,%o0"         CR_TAB
		       "st X+,__tmp_reg__"    CR_TAB
		       "st X,__zero_reg__"    CR_TAB
		       "clr __zero_reg__"     CR_TAB
		       "sbiw r26,%o0+1", op, plen, -7)

	: nxu16_asm_len ("adiw r26,%o0" CR_TAB
		       "st X+,%A1"    CR_TAB
		       "st X,%B1"     CR_TAB
		       "sbiw r26,%o0+1", op, plen, -4);
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    {
      if (!mem_volatile_p)
	return nxu16_asm_len ("st %0,%B1" CR_TAB
			    "st %0,%A1", op, plen, -2);

      return REGNO (XEXP (base, 0)) == REG_X
	? nxu16_asm_len ("sbiw r26,2"  CR_TAB
		       "st X+,%A1"   CR_TAB
		       "st X,%B1"    CR_TAB
		       "sbiw r26,1", op, plen, -4)

	: nxu16_asm_len ("sbiw %r0,2"  CR_TAB
		       "st %p0,%A1"  CR_TAB
		       "std %p0+1,%B1", op, plen, -3);
    }
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    {
      return nxu16_asm_len ("st %0,%A1"  CR_TAB
			  "st %0,%B1", op, plen, -2);

    }
  fatal_insn ("unknown move insn:",insn);
  return "";
}

static const char *
nxu16_out_movhi_mr_r_reg_no_disp_tiny (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = true_regnum (base);
  int reg_src = true_regnum (src);
  bool mem_volatile_p = MEM_VOLATILE_P (dest);

  if (reg_base == reg_src)
    {
      return !mem_volatile_p && reg_unused_after (insn, src)
	? nxu16_asm_len ("mov __tmp_reg__,%B1"   CR_TAB
		       "st %0,%A1"             CR_TAB
		       TINY_ADIW (%E0, %F0, 1) CR_TAB
		       "st %0,__tmp_reg__", op, plen, -5)
	: nxu16_asm_len ("mov __tmp_reg__,%B1"   CR_TAB
		       TINY_ADIW (%E0, %F0, 1) CR_TAB
		       "st %0,__tmp_reg__"     CR_TAB
		       TINY_SBIW (%E0, %F0, 1) CR_TAB
		       "st %0, %A1", op, plen, -7);
    }

  return !mem_volatile_p && reg_unused_after (insn, base)
    ? nxu16_asm_len ("st %0+,%A1" CR_TAB
		   "st %0,%B1", op, plen, -2)
    : nxu16_asm_len (TINY_ADIW (%E0, %F0, 1) CR_TAB
		   "st %0,%B1"             CR_TAB
		   "st -%0,%A1", op, plen, -4);
}

static const char *
nxu16_out_movhi_mr_r_reg_disp_tiny (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = REGNO (XEXP (base, 0));
  int reg_src = true_regnum (src);

  if (reg_src == reg_base)
    nxu16_asm_len ("mov __tmp_reg__,%A1"          CR_TAB
		 "mov __zero_reg__,%B1"         CR_TAB
		 TINY_ADIW (%I0, %J0, %o0+1)    CR_TAB
		 "st %b0,__zero_reg__"          CR_TAB
		 "st -%b0,__tmp_reg__"          CR_TAB
		 "clr __zero_reg__", op, plen, -7);
  else
    nxu16_asm_len (TINY_ADIW (%I0, %J0, %o0+1) CR_TAB
		 "st %b0,%B1"                CR_TAB
		 "st -%b0,%A1", op, plen, -4);

  if (!reg_unused_after (insn, XEXP (base, 0)))
    nxu16_asm_len (TINY_SBIW (%I0, %J0, %o0), op, plen, 2);

  return "";
}

static const char *
nxu16_out_movhi_mr_r_post_inc_tiny (rtx op[], int *plen)
{
  return nxu16_asm_len (TINY_ADIW (%I0, %J0, 1)  CR_TAB
		      "st %p0,%B1"    CR_TAB
		      "st -%p0,%A1"   CR_TAB
		      TINY_ADIW (%I0, %J0, 2), op, plen, -6);
}

static const char *
out_movhi_mr_r (rtx_insn *insn, rtx op[], int *plen)
{
  rtx dest = op[0];
  rtx src = op[1];
  rtx base = XEXP (dest, 0);
  int reg_base = true_regnum (base);
  int reg_src = true_regnum (src);

  /* "volatile" forces writing high-byte first (no-xmega) resp.
     low-byte first (xmega) even if less efficient, for correct
     operation with 16-bit I/O registers like.  */

  if (nxu16_XMEGA)
    return nxu16_out_movhi_mr_r_xmega (insn, op, plen);

  bool mem_volatile_p = MEM_VOLATILE_P (dest);

  if (CONSTANT_ADDRESS_P (base))
    {
      int n_words = nxu16_TINY ? 2 : 4;
      return io_address_operand (base, HImode)
	? nxu16_asm_len ("out %i0+1,%B1" CR_TAB
		       "out %i0,%A1", op, plen, -2)

	: nxu16_asm_len ("sts %m0+1,%B1" CR_TAB
		       "sts %m0,%A1", op, plen, -n_words);
    }

  if (reg_base > 0)
    {
      if (nxu16_TINY)
	return nxu16_out_movhi_mr_r_reg_no_disp_tiny (insn, op, plen);

      if (reg_base != REG_X)
	return nxu16_asm_len ("std %0+1,%B1" CR_TAB
			    "st %0,%A1", op, plen, -2);

      if (reg_src == REG_X)
	/* "st X+,r26" and "st -X,r26" are undefined.  */
	return !mem_volatile_p && reg_unused_after (insn, src)
	  ? nxu16_asm_len ("mov __tmp_reg__,r27" CR_TAB
			 "st X,r26"            CR_TAB
			 "adiw r26,1"          CR_TAB
			 "st X,__tmp_reg__", op, plen, -4)

	  : nxu16_asm_len ("mov __tmp_reg__,r27" CR_TAB
			 "adiw r26,1"          CR_TAB
			 "st X,__tmp_reg__"    CR_TAB
			 "sbiw r26,1"          CR_TAB
			 "st X,r26", op, plen, -5);

      return !mem_volatile_p && reg_unused_after (insn, base)
	? nxu16_asm_len ("st X+,%A1" CR_TAB
		       "st X,%B1", op, plen, -2)
	: nxu16_asm_len ("adiw r26,1" CR_TAB
		       "st X,%B1"   CR_TAB
		       "st -X,%A1", op, plen, -3);
    }
  else if (GET_CODE (base) == PLUS)
    {
      int disp = INTVAL (XEXP (base, 1));

      if (nxu16_TINY)
	return nxu16_out_movhi_mr_r_reg_disp_tiny (insn, op, plen);

      reg_base = REGNO (XEXP (base, 0));
      if (disp > MAX_LD_OFFSET (GET_MODE (dest)))
	{
	  if (reg_base != REG_Y)
	    fatal_insn ("incorrect insn:",insn);

	  return disp <= 63 + MAX_LD_OFFSET (GET_MODE (dest))
	    ? nxu16_asm_len ("adiw r28,%o0-62" CR_TAB
			   "std Y+63,%B1"    CR_TAB
			   "std Y+62,%A1"    CR_TAB
			   "sbiw r28,%o0-62", op, plen, -4)

	    : nxu16_asm_len ("subi r28,lo8(-%o0)" CR_TAB
			   "sbci r29,hi8(-%o0)" CR_TAB
			   "std Y+1,%B1"        CR_TAB
			   "st Y,%A1"           CR_TAB
			   "subi r28,lo8(%o0)"  CR_TAB
			   "sbci r29,hi8(%o0)", op, plen, -6);
	}

      if (reg_base != REG_X)
	return nxu16_asm_len ("std %B0,%B1" CR_TAB
			    "std %A0,%A1", op, plen, -2);
      /* (X + d) = R */
      return reg_src == REG_X
	? nxu16_asm_len ("mov __tmp_reg__,r26"  CR_TAB
		       "mov __zero_reg__,r27" CR_TAB
		       "adiw r26,%o0+1"       CR_TAB
		       "st X,__zero_reg__"    CR_TAB
		       "st -X,__tmp_reg__"    CR_TAB
		       "clr __zero_reg__"     CR_TAB
		       "sbiw r26,%o0", op, plen, -7)

	: nxu16_asm_len ("adiw r26,%o0+1" CR_TAB
		       "st X,%B1"       CR_TAB
		       "st -X,%A1"      CR_TAB
		       "sbiw r26,%o0", op, plen, -4);
    }
  else if (GET_CODE (base) == PRE_DEC) /* (--R) */
    {
      return nxu16_asm_len ("st %0,%B1" CR_TAB
			  "st %0,%A1", op, plen, -2);
    }
  else if (GET_CODE (base) == POST_INC) /* (R++) */
    {
      if (!mem_volatile_p)
	return nxu16_asm_len ("st %0,%A1"  CR_TAB
			    "st %0,%B1", op, plen, -2);

      if (nxu16_TINY)
	return nxu16_out_movhi_mr_r_post_inc_tiny (op, plen);

      return REGNO (XEXP (base, 0)) == REG_X
	? nxu16_asm_len ("adiw r26,1"  CR_TAB
		       "st X,%B1"    CR_TAB
		       "st -X,%A1"   CR_TAB
		       "adiw r26,2", op, plen, -4)

	: nxu16_asm_len ("std %p0+1,%B1" CR_TAB
		       "st %p0,%A1"    CR_TAB
		       "adiw %r0,2", op, plen, -3);
    }
  fatal_insn ("unknown move insn:",insn);
  return "";
}





/* Returns the condition of the branch following INSN, where INSN is some
   comparison.  If the next insn is not a branch or the condition code set
   by INSN might be used by more insns than the next one, return UNKNOWN.
   For now, just look at the next insn, which misses some opportunities like
   following jumps.  */

static rtx_code
compare_condition (rtx_insn *insn)
{
  rtx set;
  rtx_insn *next = next_real_nondebug_insn (insn);

  if (next
      && JUMP_P (next)
      // If SREG does not die in the next insn, it is used in more than one
      // branch.  This can happen due to pass .nxu16-ifelse optimizations.
      && dead_or_set_regno_p (next, REG_CC)
      // Branches are (set (pc) (if_then_else (COND (...)))).
      && (set = single_set (next))
      && GET_CODE (SET_SRC (set)) == IF_THEN_ELSE)
    {
      return GET_CODE (XEXP (SET_SRC (set), 0));
    }

  return UNKNOWN;
}


/* Returns true if INSN is a tst insn that only tests the sign.  */

static bool
compare_sign_p (rtx_insn *insn)
{
  rtx_code cond = compare_condition (insn);
  return (cond == GE || cond == LT);
}


/* Returns true if INSN is a compare insn with the EQ or NE condition.  */

static bool
compare_eq_p (rtx_insn *insn)
{
  rtx_code cond = compare_condition (insn);
  return (cond == EQ || cond == NE);
}


/* Implement `TARGET_CANONICALIZE_COMPARISON'.  */
/* Basically tries to convert "difficult" comparisons like GT[U]
   and LE[U] to simple ones.  Some asymmetric comparisons can be
   transformed to EQ or NE against zero.  */

static void
nxu16_canonicalize_comparison (int *icode, rtx *op0, rtx *op1, bool op0_fixed)
{
  rtx_code code = (rtx_code) *icode;
  machine_mode mode = GET_MODE (*op0);

  bool signed_p = code == GT || code == LE;
  bool unsigned_p = code == GTU || code == LEU;
  bool difficult_p = signed_p || unsigned_p;

  if (// Only do integers and fixed-points.
      (! SCALAR_INT_MODE_P (mode)
       && ! ALL_SCALAR_FIXED_POINT_MODE_P (mode))
      // Only do comparisons against a register.
      || ! register_operand (*op0, mode))
    return;

  // Canonicalize "difficult" reg-reg comparisons.

  if (! op0_fixed
      && difficult_p
      && register_operand (*op1, mode))
    {
      std::swap (*op0, *op1);
      *icode = (int) swap_condition (code);
      return;
    }

  // Canonicalize comparisons against compile-time constants.

  if (CONST_INT_P (*op1)
      || CONST_FIXED_P (*op1))
    {
      // INT_MODE of the same size.
      scalar_int_mode imode = int_mode_for_mode (mode).require ();

      unsigned HOST_WIDE_INT mask = GET_MODE_MASK (imode);
      unsigned HOST_WIDE_INT maxval = signed_p ? mask >> 1 : mask;

      // Convert value *op1 to imode.
      rtx xval = simplify_gen_subreg (imode, *op1, mode, 0);

      // Canonicalize difficult comparisons against const.
      if (difficult_p
	  && (UINTVAL (xval) & mask) != maxval)
	{
	  // Convert *op0 > *op1  to *op0 >= 1 + *op1.
	  // Convert *op0 <= *op1 to *op0 <  1 + *op1.
	  xval = simplify_binary_operation (PLUS, imode, xval, const1_rtx);

	  // Convert value back to its original mode.
	  *op1 = simplify_gen_subreg (mode, xval, imode, 0);

	  // Map  >  to  >=  and  <=  to  <.
	  *icode = (int) nxu16_normalize_condition (code);

	  return;
	}

      // Some asymmetric comparisons can be turned into EQ or NE.
      if (code == LTU && xval == const1_rtx)
	{
	  *icode = (int) EQ;
	  *op1 = CONST0_RTX (mode);
	  return;
	}

      if (code == GEU && xval == const1_rtx)
	{
	  *icode = (int) NE;
	  *op1 = CONST0_RTX (mode);
	  return;
	}
    }
}


/* Try to turn a GEU or LTU comparison of register XOP[1] into an
   NE / EQ comparison of the higher bytes of XOP[1] against 0.
   XOP[1] has scalar int or scalar fixed-point mode of 2, 3 or 4 bytes.
   XOP[2] is a compile-time constant, and XOP[0] = XOP[1] <comp> XOP[2]
   is the comparison operator.  XOP[3] is the branch label, and XOP[4]
   is a QImode scratch operand.
      When XOP[1] (viewed as a CONST_INT) is an integral power of 256,
   then a GTU or LTU comparison can be turned into a NE or EQ comparison
   of the high bytes against zero.  For example, the C code

	if (x >= 1)
	  ccc = 0;

   where x is an unsigned _Accum may be compiled as:

	or r24,r25		 ;  *cmpsi_lsr
	breq .L1		 ;  branch
	sts ccc,__zero_reg__	 ;  movqi_insn
     .L1:

   In the case of success, the operands will be such that they comprise
   a *cmp<mode>_lsr insn, where mode is HI, PSI or SI, and XOP[0] will be
   a NE or EQ branch condition.  Otherwise, XOP[] is unchanged.  */

void
nxu16_maybe_cmp_lsr (rtx *xop)
{
  rtx_code comp = GET_CODE (xop[0]);

  if ((comp == GEU || comp == LTU)
      && (CONST_INT_P (xop[2]) || CONST_FIXED_P (xop[2])))
    {
      rtx xreg = nxu16_to_int_mode (xop[1]);
      rtx xval = nxu16_to_int_mode (xop[2]);
      machine_mode imode = GET_MODE (xreg);
      auto uval = UINTVAL (xval) & GET_MODE_MASK (imode);
      int shift = exact_log2 (uval);

      if (shift == 8 || shift == 16 || shift == 24)
	{
	  // Operands such that the compare becomes *cmp<mode>_lsr.
	  xop[1] = gen_rtx_LSHIFTRT (imode, xreg, GEN_INT (shift));
	  xop[2] = const0_rtx;
	  xop[4] = gen_rtx_SCRATCH (QImode);
	  // Branch condition.
	  xop[0] = gen_rtx_fmt_ee (comp == GEU ? NE : EQ,
				   VOIDmode, xop[1], xop[2]);
	}
    }
}


/* Output an EQ / NE compare of HI, PSI or SI register XOP[0] against 0,
   where only the bits starting at XOP[1] are relevant.  XOP[1] is a
   const_int that is 8, 16 or 24.  Return "".
   PLEN == 0:  Output instructions.
   PLEN != 0:  Set *PLEN to the length of the sequence in words.  */

const char *
nxu16_out_cmp_lsr (rtx_insn *insn, rtx *xop, int *plen)
{
  rtx xreg = xop[0];
  const int n_bytes = GET_MODE_SIZE (GET_MODE (xreg));
  const int shift = INTVAL (xop[1]);
  const rtx_code cond = compare_condition (insn);

  gcc_assert (shift == 8 || shift == 16 || shift == 24);
  gcc_assert (shift < 8 * n_bytes);
  gcc_assert (cond == UNKNOWN || cond == NE || cond == EQ);

  const bool used_p = ! reg_unused_after (insn, xreg);

  if (plen)
    *plen = 0;

  if (shift / 8 == n_bytes - 1)
    {
      rtx xmsb = nxu16_byte (xreg, n_bytes - 1);
      nxu16_asm_len ("tst %0", &xmsb, plen, 1);
    }
  else if (n_bytes == 4
	   && shift <= 16
	   && nxu16_HAVE_ADIW
	   && REGNO (xreg) >= REG_22
	   // The sequence also works when xreg is unused after,
	   // but SBIW is slower than OR.
	   && used_p)
    {
      nxu16_asm_len ("sbiw %C0,0", &xreg, plen, 1);
      if (shift == 8)
	nxu16_asm_len ("cpc %B0,__zero_reg__", &xreg, plen, 1);
    }
  else
    {
      rtx op[2] = { nxu16_byte (xreg, shift / 8), tmp_reg_rtx };
      if (used_p)
	{
	  nxu16_asm_len ("mov %1,%0", op, plen, 1);
	  op[0] = tmp_reg_rtx;
	}

      for (int i = 1 + shift / 8; i < n_bytes; ++i)
	{
	  op[1] = nxu16_byte (xreg, i);
	  nxu16_asm_len ("or %0,%1", op, plen, 1);
	}
    }

  return "";
}


/* Output compare instruction

      compare (XOP[0], XOP[1])

   for a register XOP[0] and a compile-time constant XOP[1].  Return "".
   XOP[2] is an 8-bit scratch register as needed.

   PLEN == NULL:  Output instructions.
   PLEN != NULL:  Set *PLEN to the length (in words) of the sequence.
                  Don't output anything.  */

const char *
nxu16_out_compare (rtx_insn *insn, rtx *xop, int *plen)
{
  /* Register to compare and value to compare against. */
  rtx xreg = xop[0];
  rtx xval = xop[1];

  /* Number of bytes to operate on.  */
  int n_bytes = GET_MODE_SIZE (GET_MODE (xreg));

  /* Value (0..0xff) held in clobber register xop[2] or -1 if unknown.  */
  int clobber_val = -1;

  /* Map fixed mode operands to integer operands with the same binary
     representation.  They are easier to handle in the remainder.  */

  if (CONST_FIXED_P (xval))
    {
      xreg = nxu16_to_int_mode (xop[0]);
      xval = nxu16_to_int_mode (xop[1]);
    }

  gcc_assert (REG_P (xreg));
  gcc_assert ((CONST_INT_P (xval) && n_bytes <= 4)
	      || (const_double_operand (xval, VOIDmode) && n_bytes == 8));

  if (plen)
    *plen = 0;

  const rtx_code cond = compare_condition (insn);
  const bool eqne_p = cond == EQ || cond == NE;

  /* Comparisons == +/-1 and != +/-1 can be done similar to camparing
     against 0 by ORing the bytes.  This is one instruction shorter.
     Notice that 64-bit comparisons are always against reg:ALL8 18 (ACC_A)
     and therefore don't use this.  */

  if (eqne_p
      && ! test_hard_reg_class (LD_REGS, xreg)
      && reg_unused_after (insn, xreg))
    {
      if (xval == const1_rtx)
	{
	  nxu16_asm_len ("dec %A0" CR_TAB
		       "or %A0,%B0", xop, plen, 2);

	  if (n_bytes >= 3)
	    nxu16_asm_len ("or %A0,%C0", xop, plen, 1);

	  if (n_bytes >= 4)
	    nxu16_asm_len ("or %A0,%D0", xop, plen, 1);

	  return "";
	}
      else if (xval == constm1_rtx)
	{
	  if (n_bytes >= 4)
	    nxu16_asm_len ("and %A0,%D0", xop, plen, 1);

	  if (n_bytes >= 3)
	    nxu16_asm_len ("and %A0,%C0", xop, plen, 1);

	  return nxu16_asm_len ("and %A0,%B0" CR_TAB
			      "com %A0", xop, plen, 2);
	}
    }

  /* Comparisons == and != may change the order in which the sub-bytes are
     being compared.  Start with the high 16 bits so we can use SBIW.  */

  if (n_bytes == 4
      && eqne_p
      && nxu16_HAVE_ADIW
      && REGNO (xreg) >= REG_22
      && (xval == const0_rtx
	  || (IN_RANGE (nxu16_int16 (xval, 2), 0, 63)
	      && reg_unused_after (insn, xreg))))
    {
      xop[2] = nxu16_word (xval, 2);
      return nxu16_asm_len ("sbiw %C0,%2"      CR_TAB
			  "sbci %B0,hi8(%1)" CR_TAB
			  "sbci %A0,lo8(%1)", xop, plen, 3);
    }

  bool changed[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };

  /* The >= and < comparisons may skip the lower bytes when the according bytes
     of the constant are all zeros.  In that case, the comparison may start
     at a byte other than the LSB.  */

  const int start = ((cond == GEU || cond == LTU || cond == GE || cond == LT)
		     && INTVAL (xval) != 0)
    ? ctz_hwi (INTVAL (xval)) / 8
    : 0;

  for (int i = start; i < n_bytes; i++)
    {
      /* We compare byte-wise.  */
      xop[0] = nxu16_byte (xreg, i);
      xop[1] = nxu16_byte (xval, i);

      /* 8-bit value to compare with this byte.  */
      unsigned int val8 = nxu16_uint8 (xval, i);

      /* Word registers >= R24 can use SBIW/ADIW with 0..63.  */

      if (i == start
	  && i % 2 == 0
	  && n_bytes - start >= 2
	  && nxu16_adiw_reg_p (xop[0]))
	{
	  int val16 = nxu16_int16 (xval, i);

	  if (IN_RANGE (val16, 0, 63)
	      && (val8 == 0
		  || reg_unused_after (insn, xreg)))
	    {
	      nxu16_asm_len ("sbiw %0,%1", xop, plen, 1);
	      changed[i] = changed[i + 1] = val8 != 0;
	      i++;
	      continue;
	    }

	  if (IN_RANGE (val16, -63, -1)
	      && eqne_p
	      && n_bytes - start == 2
	      && reg_unused_after (insn, xreg))
	    {
	      return nxu16_asm_len ("adiw %0,%n1", xop, plen, 1);
	    }
	}

      /* Comparing against 0 is easy.  */

      if (val8 == 0)
	{
	  nxu16_asm_len (i == start
		       ? "cp %0,__zero_reg__"
		       : "cpc %0,__zero_reg__", xop, plen, 1);
	  continue;
	}

      /* Upper registers can compare and subtract-with-carry immediates.
	 Notice that compare instructions do the same as respective subtract
	 instruction; the only difference is that comparisons don't write
	 the result back to the target register.  */

      if (test_hard_reg_class (LD_REGS, xop[0]))
	{
	  if (i == start)
	    {
	      nxu16_asm_len ("cpi %0,%1", xop, plen, 1);
	      continue;
	    }
	  else if (reg_unused_after (insn, xreg))
	    {
	      nxu16_asm_len ("sbci %0,%1", xop, plen, 1);
	      changed[i] = true;
	      continue;
	    }
	}

      /* When byte comparisons for an EQ or NE comparison look like
	     compare (x[i], C)
	     compare (x[j], C)
	 then we can instead use
	     compare (x[i], C)
	     compare (x[j], x[i])
	 which is shorter, and the outcome of the comparison is the same.  */

      if (eqne_p)
	{
	  bool found = false;

	  for (int j = start; j < i && ! found; ++j)
	    if (val8 == nxu16_uint8 (xval, j)
		// Make sure that we didn't clobber x[j] above.
		&& ! changed[j])
	      {
		rtx op[] = { xop[0], nxu16_byte (xreg, j) };
		nxu16_asm_len ("cpc %0,%1", op, plen, 1);
		found = true;
	      }

	  if (found)
	    continue;
	}

      /* Must load the value into the scratch register.  */

      gcc_assert (REG_P (xop[2]));

      if (clobber_val != (int) val8)
	nxu16_asm_len ("ldi %2,%1", xop, plen, 1);
      clobber_val = (int) val8;

      nxu16_asm_len (i == start
		   ? "cp %0,%2"
		   : "cpc %0,%2", xop, plen, 1);
    }

  return "";
}


/* Prepare operands of compare_const_di2 to be used with nxu16_out_compare.  */

const char *
nxu16_out_compare64 (rtx_insn *insn, rtx *op, int *plen)
{
  rtx xop[3] = { gen_rtx_REG (DImode, ACC_A), op[0], op[1] };

  return nxu16_out_compare (insn, xop, plen);
}

/* Output test instruction for HImode.  */

const char *
nxu16_out_tsthi (rtx_insn *insn, rtx *op, int *plen)
{
  if (compare_sign_p (insn))
    {
      nxu16_asm_len ("tst %B0", op, plen, -1);
    }
  else if (reg_unused_after (insn, op[0])
	   && compare_eq_p (insn))
    {
      /* Faster than sbiw if we can clobber the operand.  */
      nxu16_asm_len ("or %A0,%B0", op, plen, -1);
    }
  else
    {
      nxu16_out_compare (insn, op, plen);
    }

  return "";
}


/* Output test instruction for PSImode.  */

const char *
nxu16_out_tstpsi (rtx_insn *insn, rtx *op, int *plen)
{
  if (compare_sign_p (insn))
    {
      nxu16_asm_len ("tst %C0", op, plen, -1);
    }
  else if (reg_unused_after (insn, op[0])
	   && compare_eq_p (insn))
    {
      /* Faster than sbiw if we can clobber the operand.  */
      nxu16_asm_len ("or %A0,%B0" CR_TAB
		   "or %A0,%C0", op, plen, -2);
    }
  else
    {
      nxu16_out_compare (insn, op, plen);
    }

  return "";
}


/* Output test instruction for SImode.  */

const char *
nxu16_out_tstsi (rtx_insn *insn, rtx *op, int *plen)
{
  if (compare_sign_p (insn))
    {
      nxu16_asm_len ("tst %D0", op, plen, -1);
    }
  else if (reg_unused_after (insn, op[0])
	   && compare_eq_p (insn))
    {
      /* Faster than sbiw if we can clobber the operand.  */
      nxu16_asm_len ("or %A0,%B0" CR_TAB
		   "or %A0,%C0" CR_TAB
		   "or %A0,%D0", op, plen, -3);
    }
  else
    {
      nxu16_out_compare (insn, op, plen);
    }

  return "";
}


/* Output a comparison of a zero- or sign-extended register against a
   plain register.  CODE is SIGN_EXTEND or ZERO_EXTEND.  Return "".

   PLEN != 0: Set *PLEN to the code length in words. Don't output anything.
   PLEN == 0: Print instructions.  */

const char *
nxu16_out_cmp_ext (rtx xop[], rtx_code code, int *plen)
{
  // The smaller reg is the one that's to be extended.  Get its index as z.
  int z = GET_MODE_SIZE (GET_MODE (xop[1])) < GET_MODE_SIZE (GET_MODE (xop[0]));
  rtx zreg = xop[z];
  rtx reg = xop[1 - z];
  machine_mode mode = GET_MODE (reg);
  machine_mode zmode = GET_MODE (zreg);
  rtx zex;

  if (plen)
    *plen = 0;

  // zex holds the extended bytes above zreg.  This is 0 for ZERO_EXTEND,
  // and 0 or -1 for SIGN_EXTEND.

  if (code == SIGN_EXTEND)
    {
      // Sign-extend the high-byte of zreg to tmp_reg.
      int zmsb = GET_MODE_SIZE (zmode) - 1;
      rtx xzmsb = nxu16_byte (zreg, zmsb);

      nxu16_asm_len ("mov __tmp_reg__,%0" CR_TAB
		   "rol __tmp_reg__"    CR_TAB
		   "sbc __tmp_reg__,__tmp_reg__", &xzmsb, plen, 3);
      zex = tmp_reg_rtx;
    }
  else if (code == ZERO_EXTEND)
    {
      zex = zero_reg_rtx;
    }
  else
    gcc_unreachable();

  // Now output n_bytes bytes of the very comparison.

  int n_bytes = GET_MODE_SIZE (mode);

  nxu16_asm_len ("cp %0,%1", xop, plen, 1);

  for (int b = 1; b < n_bytes; ++b)
    {
      rtx regs[2];
      regs[1 - z] = nxu16_byte (reg, b);
      regs[z] = b < GET_MODE_SIZE (zmode) ? nxu16_byte (zreg, b) : zex;

      nxu16_asm_len ("cpc %0,%1", regs, plen, 1);
    }

  return "";
}


/* Generate asm equivalent for various shifts.  This only handles cases
   that are not already carefully hand-optimized in ?sh??i3_out.

   OPERANDS[0] resp. %0 in TEMPL is the operand to be shifted.
   OPERANDS[2] is the shift count as CONST_INT, MEM or REG.
   OPERANDS[3] is a QImode scratch register from LD regs if
               available and SCRATCH, otherwise (no scratch available)

   TEMPL is an assembler template that shifts by one position.
   T_LEN is the length of this template.  */

void
out_shift_with_cnt (const char *templ, rtx_insn *insn, rtx operands[],
		    int *plen, int t_len)
{
  bool second_label = true;
  bool saved_in_tmp = false;
  bool use_zero_reg = false;
  rtx op[5];

  op[0] = operands[0];
  op[1] = operands[1];
  op[2] = operands[2];
  op[3] = operands[3];

  if (plen)
    *plen = 0;

  if (CONST_INT_P (operands[2]))
    {
      /* Operand 3 is a scratch register if this is a
         parallel with three elements i.e. a set,
         a clobber of a scratch, and clobber of REG_CC.
         If a scratch reg is not available, then the parallel
         will contain only a set and clobber of REG_CC. */
      bool scratch = (GET_CODE (PATTERN (insn)) == PARALLEL
		      && XVECLEN (PATTERN (insn), 0) == 3
		      && REG_P (operands[3]));
      int count = INTVAL (operands[2]);
      int max_len = 10;  /* If larger than this, always use a loop.  */

      if (count <= 0)
	return;

      if (count < 8 && !scratch)
	use_zero_reg = true;

      if (optimize_size)
	max_len = t_len + (scratch ? 3 : (use_zero_reg ? 4 : 5));

      if (t_len * count <= max_len)
	{
	  /* Output shifts inline with no loop - faster.  */

	  while (count-- > 0)
	    nxu16_asm_len (templ, op, plen, t_len);

	  return;
	}

      if (scratch)
	{
	  nxu16_asm_len ("ldi %3,%2", op, plen, 1);
	}
      else if (use_zero_reg)
	{
	  /* Hack to save one word: use __zero_reg__ as loop counter.
	     Set one bit, then shift in a loop until it is 0 again.  */

	  op[3] = zero_reg_rtx;

	  nxu16_asm_len ("set" CR_TAB
		       "bld %3,%2-1", op, plen, 2);
	}
      else
	{
	  /* No scratch register available, use one from LD_REGS (saved in
	     __tmp_reg__) that doesn't overlap with registers to shift.  */

	  op[3] = all_regs_rtx[((REGNO (op[0]) - 1) & 15) + REG_16];
	  op[4] = tmp_reg_rtx;
	  saved_in_tmp = true;

	  nxu16_asm_len ("mov %4,%3" CR_TAB
		       "ldi %3,%2", op, plen, 2);
	}

      second_label = false;
    }
  else if (MEM_P (op[2]))
    {
      rtx op_mov[2];

      op_mov[0] = op[3] = tmp_reg_rtx;
      op_mov[1] = op[2];

      out_movqi_r_mr (insn, op_mov, plen);
    }
  else if (register_operand (op[2], QImode))
    {
      op[3] = op[2];

      if (!reg_unused_after (insn, op[2])
	  || reg_overlap_mentioned_p (op[0], op[2]))
	{
	  op[3] = tmp_reg_rtx;
	  nxu16_asm_len ("mov %3,%2", op, plen, 1);
	}
    }
  else
    fatal_insn ("bad shift insn:", insn);

  if (second_label)
    nxu16_asm_len ("rjmp 2f", op, plen, 1);

  nxu16_asm_len ("1:", op, plen, 0);
  nxu16_asm_len (templ, op, plen, t_len);

  if (second_label)
    nxu16_asm_len ("2:", op, plen, 0);

  nxu16_asm_len (use_zero_reg ? "lsr %3" : "dec %3", op, plen, 1);
  nxu16_asm_len (second_label ? "brpl 1b" : "brne 1b", op, plen, 1);

  if (saved_in_tmp)
    nxu16_asm_len ("mov %3,%4", op, plen, 1);
}


/* 8bit shift left ((char)x << i)   */

const char *
ashlqi3_out (rtx_insn *insn, rtx operands[], int *len)
{
  if (CONST_INT_P (operands[2]))
    {
      int k;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	default:
	  if (INTVAL (operands[2]) < 8)
	    break;

	  *len = 1;
	  return "clr %0";

	case 1:
	  *len = 1;
	  return "lsl %0";

	case 2:
	  *len = 2;
	  return ("lsl %0" CR_TAB
		  "lsl %0");

	case 3:
	  *len = 3;
	  return ("lsl %0" CR_TAB
		  "lsl %0" CR_TAB
		  "lsl %0");

	case 4:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len = 2;
	      return ("swap %0" CR_TAB
		      "andi %0,0xf0");
	    }
	  *len = 4;
	  return ("lsl %0" CR_TAB
		  "lsl %0" CR_TAB
		  "lsl %0" CR_TAB
		  "lsl %0");

	case 5:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len = 3;
	      return ("swap %0" CR_TAB
		      "lsl %0"  CR_TAB
		      "andi %0,0xe0");
	    }
	  *len = 5;
	  return ("lsl %0" CR_TAB
		  "lsl %0" CR_TAB
		  "lsl %0" CR_TAB
		  "lsl %0" CR_TAB
		  "lsl %0");

	case 6:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len = 4;
	      return ("swap %0" CR_TAB
		      "lsl %0"  CR_TAB
		      "lsl %0"  CR_TAB
		      "andi %0,0xc0");
	    }
	  *len = 6;
	  return ("lsl %0" CR_TAB
		  "lsl %0" CR_TAB
		  "lsl %0" CR_TAB
		  "lsl %0" CR_TAB
		  "lsl %0" CR_TAB
		  "lsl %0");

	case 7:
	  *len = 3;
	  return ("ror %0" CR_TAB
		  "clr %0" CR_TAB
		  "ror %0");
	}
    }
  else if (CONSTANT_P (operands[2]))
    fatal_insn ("internal compiler error.  Incorrect shift:", insn);

  out_shift_with_cnt ("lsl %0",
		      insn, operands, len, 1);
  return "";
}


/* 16bit shift left ((short)x << i)   */

const char *
ashlhi3_out (rtx_insn *insn, rtx operands[], int *len)
{
  if (CONST_INT_P (operands[2]))
    {
      int scratch = (GET_CODE (PATTERN (insn)) == PARALLEL
		     && XVECLEN (PATTERN (insn), 0) == 3
		     && REG_P (operands[3]));
      int ldi_ok = test_hard_reg_class (LD_REGS, operands[0]);
      int k;
      int *t = len;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	default:
	  if (INTVAL (operands[2]) < 16)
	    break;

	  *len = 2;
	  return ("clr %B0" CR_TAB
		  "clr %A0");

	case 4:
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  if (ldi_ok)
	    {
	      *len = 6;
	      return ("swap %A0"      CR_TAB
		      "swap %B0"      CR_TAB
		      "andi %B0,0xf0" CR_TAB
		      "eor %B0,%A0"   CR_TAB
		      "andi %A0,0xf0" CR_TAB
		      "eor %B0,%A0");
	    }
	  if (scratch)
	    {
	      *len = 7;
	      return ("swap %A0"    CR_TAB
		      "swap %B0"    CR_TAB
		      "ldi %3,0xf0" CR_TAB
		      "and %B0,%3"  CR_TAB
		      "eor %B0,%A0" CR_TAB
		      "and %A0,%3"  CR_TAB
		      "eor %B0,%A0");
	    }
	  break;  /* optimize_size ? 6 : 8 */

	case 5:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  if (ldi_ok)
	    {
	      *len = 8;
	      return ("lsl %A0"       CR_TAB
		      "rol %B0"       CR_TAB
		      "swap %A0"      CR_TAB
		      "swap %B0"      CR_TAB
		      "andi %B0,0xf0" CR_TAB
		      "eor %B0,%A0"   CR_TAB
		      "andi %A0,0xf0" CR_TAB
		      "eor %B0,%A0");
	    }
	  if (scratch)
	    {
	      *len = 9;
	      return ("lsl %A0"     CR_TAB
		      "rol %B0"     CR_TAB
		      "swap %A0"    CR_TAB
		      "swap %B0"    CR_TAB
		      "ldi %3,0xf0" CR_TAB
		      "and %B0,%3"  CR_TAB
		      "eor %B0,%A0" CR_TAB
		      "and %A0,%3"  CR_TAB
		      "eor %B0,%A0");
	    }
	  break;  /* 10 */

	case 6:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  *len = 9;
	  return ("clr __tmp_reg__" CR_TAB
		  "lsr %B0"         CR_TAB
		  "ror %A0"         CR_TAB
		  "ror __tmp_reg__" CR_TAB
		  "lsr %B0"         CR_TAB
		  "ror %A0"         CR_TAB
		  "ror __tmp_reg__" CR_TAB
		  "mov %B0,%A0"     CR_TAB
		  "mov %A0,__tmp_reg__");

	case 7:
	  *len = 5;
	  return ("lsr %B0"     CR_TAB
		  "mov %B0,%A0" CR_TAB
		  "clr %A0"     CR_TAB
		  "ror %B0"     CR_TAB
		  "ror %A0");

	case 8:
	  return *len = 2, ("mov %B0,%A1" CR_TAB
			    "clr %A0");

	case 9:
	  *len = 3;
	  return ("mov %B0,%A0" CR_TAB
		  "clr %A0"     CR_TAB
		  "lsl %B0");

	case 10:
	  *len = 4;
	  return ("mov %B0,%A0" CR_TAB
		  "clr %A0"     CR_TAB
		  "lsl %B0"     CR_TAB
		  "lsl %B0");

	case 11:
	  *len = 5;
	  return ("mov %B0,%A0" CR_TAB
		  "clr %A0"     CR_TAB
		  "lsl %B0"     CR_TAB
		  "lsl %B0"     CR_TAB
		  "lsl %B0");

	case 12:
	  if (ldi_ok)
	    {
	      *len = 4;
	      return ("mov %B0,%A0" CR_TAB
		      "clr %A0"     CR_TAB
		      "swap %B0"    CR_TAB
		      "andi %B0,0xf0");
	    }
	  if (scratch)
	    {
	      *len = 5;
	      return ("mov %B0,%A0" CR_TAB
		      "clr %A0"     CR_TAB
		      "swap %B0"    CR_TAB
		      "ldi %3,0xf0" CR_TAB
		      "and %B0,%3");
	    }
	  *len = 6;
	  return ("mov %B0,%A0" CR_TAB
		  "clr %A0"     CR_TAB
		  "lsl %B0"     CR_TAB
		  "lsl %B0"     CR_TAB
		  "lsl %B0"     CR_TAB
		  "lsl %B0");

	case 13:
	  if (ldi_ok)
	    {
	      *len = 5;
	      return ("mov %B0,%A0" CR_TAB
		      "clr %A0"     CR_TAB
		      "swap %B0"    CR_TAB
		      "lsl %B0"     CR_TAB
		      "andi %B0,0xe0");
	    }
	  if (nxu16_HAVE_MUL && scratch)
	    {
	      *len = 5;
	      return ("ldi %3,0x20" CR_TAB
		      "mul %A0,%3"  CR_TAB
		      "mov %B0,r0"  CR_TAB
		      "clr %A0"     CR_TAB
		      "clr __zero_reg__");
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  if (scratch)
	    {
	      *len = 6;
	      return ("mov %B0,%A0" CR_TAB
		      "clr %A0"     CR_TAB
		      "swap %B0"    CR_TAB
		      "lsl %B0"     CR_TAB
		      "ldi %3,0xe0" CR_TAB
		      "and %B0,%3");
	    }
	  if (nxu16_HAVE_MUL)
	    {
	      *len = 6;
	      return ("set"        CR_TAB
		      "bld r1,5"   CR_TAB
		      "mul %A0,r1" CR_TAB
		      "mov %B0,r0" CR_TAB
		      "clr %A0"    CR_TAB
		      "clr __zero_reg__");
	    }
	  *len = 7;
	  return ("mov %B0,%A0" CR_TAB
		  "clr %A0"     CR_TAB
		  "lsl %B0"     CR_TAB
		  "lsl %B0"     CR_TAB
		  "lsl %B0"     CR_TAB
		  "lsl %B0"     CR_TAB
		  "lsl %B0");

	case 14:
	  if (nxu16_HAVE_MUL && ldi_ok)
	    {
	      *len = 5;
	      return ("ldi %B0,0x40" CR_TAB
		      "mul %A0,%B0"  CR_TAB
		      "mov %B0,r0"   CR_TAB
		      "clr %A0"      CR_TAB
		      "clr __zero_reg__");
	    }
	  if (nxu16_HAVE_MUL && scratch)
	    {
	      *len = 5;
	      return ("ldi %3,0x40" CR_TAB
		      "mul %A0,%3"  CR_TAB
		      "mov %B0,r0"  CR_TAB
		      "clr %A0"     CR_TAB
		      "clr __zero_reg__");
	    }
	  if (optimize_size && ldi_ok)
	    {
	      *len = 5;
	      return ("mov %B0,%A0" CR_TAB
		      "ldi %A0,6" "\n1:\t"
		      "lsl %B0"     CR_TAB
		      "dec %A0"     CR_TAB
		      "brne 1b");
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  *len = 6;
	  return ("clr %B0" CR_TAB
		  "lsr %A0" CR_TAB
		  "ror %B0" CR_TAB
		  "lsr %A0" CR_TAB
		  "ror %B0" CR_TAB
		  "clr %A0");

	case 15:
	  *len = 4;
	  return ("clr %B0" CR_TAB
		  "lsr %A0" CR_TAB
		  "ror %B0" CR_TAB
		  "clr %A0");
	}
      len = t;
    }
  out_shift_with_cnt ("lsl %A0" CR_TAB
		      "rol %B0", insn, operands, len, 2);
  return "";
}


/* 24-bit shift left */

const char *
nxu16_out_ashlpsi3 (rtx_insn *insn, rtx *op, int *plen)
{
  if (plen)
    *plen = 0;

  if (CONST_INT_P (op[2]))
    {
      switch (INTVAL (op[2]))
	{
	default:
	  if (INTVAL (op[2]) < 24)
	    break;

	  return nxu16_asm_len ("clr %A0" CR_TAB
			      "clr %B0" CR_TAB
			      "clr %C0", op, plen, 3);

	case 8:
	  {
	    int reg0 = REGNO (op[0]);
	    int reg1 = REGNO (op[1]);

	    if (reg0 >= reg1)
	      return nxu16_asm_len ("mov %C0,%B1"  CR_TAB
				  "mov %B0,%A1"  CR_TAB
				  "clr %A0", op, plen, 3);
	    else
	      return nxu16_asm_len ("clr %A0"      CR_TAB
				  "mov %B0,%A1"  CR_TAB
				  "mov %C0,%B1", op, plen, 3);
	  }

	case 16:
	  {
	    int reg0 = REGNO (op[0]);
	    int reg1 = REGNO (op[1]);

	    if (reg0 + 2 != reg1)
	      nxu16_asm_len ("mov %C0,%A0", op, plen, 1);

	    return nxu16_asm_len ("clr %B0"  CR_TAB
				"clr %A0", op, plen, 2);
	  }

	case 23:
	  return nxu16_asm_len ("clr %C0" CR_TAB
			      "lsr %A0" CR_TAB
			      "ror %C0" CR_TAB
			      "clr %B0" CR_TAB
			      "clr %A0", op, plen, 5);
	}
    }

  out_shift_with_cnt ("lsl %A0" CR_TAB
		      "rol %B0" CR_TAB
		      "rol %C0", insn, op, plen, 3);
  return "";
}


/* 32bit shift left ((long)x << i)   */

const char *
ashlsi3_out (rtx_insn *insn, rtx operands[], int *len)
{
  if (CONST_INT_P (operands[2]))
    {
      int k;
      int *t = len;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	default:
	  if (INTVAL (operands[2]) < 32)
	    break;

	  if (nxu16_HAVE_MOVW)
	    return *len = 3, ("clr %D0" CR_TAB
			      "clr %C0" CR_TAB
			      "movw %A0,%C0");
	  *len = 4;
	  return ("clr %D0" CR_TAB
		  "clr %C0" CR_TAB
		  "clr %B0" CR_TAB
		  "clr %A0");

	case 8:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    *len = 4;
	    if (reg0 >= reg1)
	      return ("mov %D0,%C1"  CR_TAB
		      "mov %C0,%B1"  CR_TAB
		      "mov %B0,%A1"  CR_TAB
		      "clr %A0");
	    else
	      return ("clr %A0"      CR_TAB
		      "mov %B0,%A1"  CR_TAB
		      "mov %C0,%B1"  CR_TAB
		      "mov %D0,%C1");
	  }

	case 16:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    if (reg0 + 2 == reg1)
	      return *len = 2, ("clr %B0"      CR_TAB
				"clr %A0");
	    if (nxu16_HAVE_MOVW)
	      return *len = 3, ("movw %C0,%A1" CR_TAB
				"clr %B0"      CR_TAB
				"clr %A0");
	    else
	      return *len = 4, ("mov %C0,%A1"  CR_TAB
				"mov %D0,%B1"  CR_TAB
				"clr %B0"      CR_TAB
				"clr %A0");
	  }

	case 24:
	  *len = 4;
	  return ("mov %D0,%A1"  CR_TAB
		  "clr %C0"      CR_TAB
		  "clr %B0"      CR_TAB
		  "clr %A0");

	case 31:
	  *len = 6;
	  return ("clr %D0" CR_TAB
		  "lsr %A0" CR_TAB
		  "ror %D0" CR_TAB
		  "clr %C0" CR_TAB
		  "clr %B0" CR_TAB
		  "clr %A0");
	}
      len = t;
    }
  out_shift_with_cnt ("lsl %A0" CR_TAB
		      "rol %B0" CR_TAB
		      "rol %C0" CR_TAB
		      "rol %D0", insn, operands, len, 4);
  return "";
}

/* 8bit arithmetic shift right  ((signed char)x >> i) */

const char *
ashrqi3_out (rtx_insn *insn, rtx operands[], int *len)
{
  if (CONST_INT_P (operands[2]))
    {
      int k;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	case 1:
	  *len = 1;
	  return "asr %0";

	case 2:
	  *len = 2;
	  return ("asr %0" CR_TAB
		  "asr %0");

	case 3:
	  *len = 3;
	  return ("asr %0" CR_TAB
		  "asr %0" CR_TAB
		  "asr %0");

	case 4:
	  *len = 4;
	  return ("asr %0" CR_TAB
		  "asr %0" CR_TAB
		  "asr %0" CR_TAB
		  "asr %0");

	case 5:
	  *len = 5;
	  return ("asr %0" CR_TAB
		  "asr %0" CR_TAB
		  "asr %0" CR_TAB
		  "asr %0" CR_TAB
		  "asr %0");

	case 6:
	  *len = 4;
	  return ("bst %0,6"  CR_TAB
		  "lsl %0"    CR_TAB
		  "sbc %0,%0" CR_TAB
		  "bld %0,0");

	default:
	  if (INTVAL (operands[2]) < 8)
	    break;

	  /* fall through */

	case 7:
	  *len = 2;
	  return ("lsl %0" CR_TAB
		  "sbc %0,%0");
	}
    }
  else if (CONSTANT_P (operands[2]))
    fatal_insn ("internal compiler error.  Incorrect shift:", insn);

  out_shift_with_cnt ("asr %0",
		      insn, operands, len, 1);
  return "";
}


/* 16bit arithmetic shift right  ((signed short)x >> i) */

const char *
ashrhi3_out (rtx_insn *insn, rtx operands[], int *len)
{
  if (CONST_INT_P (operands[2]))
    {
      int scratch = (GET_CODE (PATTERN (insn)) == PARALLEL
		     && XVECLEN (PATTERN (insn), 0) == 3
		     && REG_P (operands[3]));
      int ldi_ok = test_hard_reg_class (LD_REGS, operands[0]);
      int k;
      int *t = len;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	case 4:
	case 5:
	  /* XXX try to optimize this too? */
	  break;

	case 6:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  *len = 8;
	  return ("mov __tmp_reg__,%A0" CR_TAB
		  "mov %A0,%B0"         CR_TAB
		  "lsl __tmp_reg__"     CR_TAB
		  "rol %A0"             CR_TAB
		  "sbc %B0,%B0"         CR_TAB
		  "lsl __tmp_reg__"     CR_TAB
		  "rol %A0"             CR_TAB
		  "rol %B0");

	case 7:
	  *len = 4;
	  return ("lsl %A0"     CR_TAB
		  "mov %A0,%B0" CR_TAB
		  "rol %A0"     CR_TAB
		  "sbc %B0,%B0");

	case 8:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);

	    if (reg0 == reg1)
	      return *len = 3, ("mov %A0,%B0" CR_TAB
				"lsl %B0"     CR_TAB
				"sbc %B0,%B0");
	    else
	      return *len = 4, ("mov %A0,%B1" CR_TAB
				"clr %B0"     CR_TAB
				"sbrc %A0,7"  CR_TAB
				"dec %B0");
	  }

	case 9:
	  *len = 4;
	  return ("mov %A0,%B0" CR_TAB
		  "lsl %B0"      CR_TAB
		  "sbc %B0,%B0" CR_TAB
		  "asr %A0");

	case 10:
	  *len = 5;
	  return ("mov %A0,%B0" CR_TAB
		  "lsl %B0"     CR_TAB
		  "sbc %B0,%B0" CR_TAB
		  "asr %A0"     CR_TAB
		  "asr %A0");

	case 11:
	  if (nxu16_HAVE_MUL && ldi_ok)
	    {
	      *len = 5;
	      return ("ldi %A0,0x20" CR_TAB
		      "muls %B0,%A0" CR_TAB
		      "mov %A0,r1"   CR_TAB
		      "sbc %B0,%B0"  CR_TAB
		      "clr __zero_reg__");
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  *len = 6;
	  return ("mov %A0,%B0" CR_TAB
		  "lsl %B0"     CR_TAB
		  "sbc %B0,%B0" CR_TAB
		  "asr %A0"     CR_TAB
		  "asr %A0"     CR_TAB
		  "asr %A0");

	case 12:
	  if (nxu16_HAVE_MUL && ldi_ok)
	    {
	      *len = 5;
	      return ("ldi %A0,0x10" CR_TAB
		      "muls %B0,%A0" CR_TAB
		      "mov %A0,r1"   CR_TAB
		      "sbc %B0,%B0"  CR_TAB
		      "clr __zero_reg__");
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  *len = 7;
	  return ("mov %A0,%B0" CR_TAB
		  "lsl %B0"     CR_TAB
		  "sbc %B0,%B0" CR_TAB
		  "asr %A0"     CR_TAB
		  "asr %A0"     CR_TAB
		  "asr %A0"     CR_TAB
		  "asr %A0");

	case 13:
	  if (nxu16_HAVE_MUL && ldi_ok)
	    {
	      *len = 5;
	      return ("ldi %A0,0x08" CR_TAB
		      "muls %B0,%A0" CR_TAB
		      "mov %A0,r1"   CR_TAB
		      "sbc %B0,%B0"  CR_TAB
		      "clr __zero_reg__");
	    }
	  if (optimize_size)
	    break;  /* scratch ? 5 : 7 */
	  *len = 8;
	  return ("mov %A0,%B0" CR_TAB
		  "lsl %B0"     CR_TAB
		  "sbc %B0,%B0" CR_TAB
		  "asr %A0"     CR_TAB
		  "asr %A0"     CR_TAB
		  "asr %A0"     CR_TAB
		  "asr %A0"     CR_TAB
		  "asr %A0");

	case 14:
	  *len = 5;
	  return ("lsl %B0"     CR_TAB
		  "sbc %A0,%A0" CR_TAB
		  "lsl %B0"     CR_TAB
		  "mov %B0,%A0" CR_TAB
		  "rol %A0");

	default:
	  if (INTVAL (operands[2]) < 16)
	    break;

	  /* fall through */

	case 15:
	  return *len = 3, ("lsl %B0"     CR_TAB
			    "sbc %A0,%A0" CR_TAB
			    "mov %B0,%A0");
	}
      len = t;
    }
  out_shift_with_cnt ("asr %B0" CR_TAB
		      "ror %A0", insn, operands, len, 2);
  return "";
}


/* 24-bit arithmetic shift right */

const char *
nxu16_out_ashrpsi3 (rtx_insn *insn, rtx *op, int *plen)
{
  int dest = REGNO (op[0]);
  int src = REGNO (op[1]);

  if (CONST_INT_P (op[2]))
    {
      if (plen)
	*plen = 0;

      switch (INTVAL (op[2]))
	{
	case 8:
	  if (dest <= src)
	    return nxu16_asm_len ("mov %A0,%B1" CR_TAB
				"mov %B0,%C1" CR_TAB
				"clr %C0"     CR_TAB
				"sbrc %B0,7"  CR_TAB
				"dec %C0", op, plen, 5);
	  else
	    return nxu16_asm_len ("clr %C0"     CR_TAB
				"sbrc %C1,7"  CR_TAB
				"dec %C0"     CR_TAB
				"mov %B0,%C1" CR_TAB
				"mov %A0,%B1", op, plen, 5);

	case 16:
	  if (dest != src + 2)
	    nxu16_asm_len ("mov %A0,%C1", op, plen, 1);

	  return nxu16_asm_len ("clr %B0"     CR_TAB
			      "sbrc %A0,7"  CR_TAB
			      "com %B0"     CR_TAB
			      "mov %C0,%B0", op, plen, 4);

	default:
	  if (INTVAL (op[2]) < 24)
	    break;

	  /* fall through */

	case 23:
	  return nxu16_asm_len ("lsl %C0"     CR_TAB
			      "sbc %A0,%A0" CR_TAB
			      "mov %B0,%A0" CR_TAB
			      "mov %C0,%A0", op, plen, 4);
	} /* switch */
    }

  out_shift_with_cnt ("asr %C0" CR_TAB
		      "ror %B0" CR_TAB
		      "ror %A0", insn, op, plen, 3);
  return "";
}


/* 32-bit arithmetic shift right  ((signed long)x >> i) */

const char *
ashrsi3_out (rtx_insn *insn, rtx operands[], int *len)
{
  if (CONST_INT_P (operands[2]))
    {
      int k;
      int *t = len;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	case 8:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    *len=6;
	    if (reg0 <= reg1)
	      return ("mov %A0,%B1" CR_TAB
		      "mov %B0,%C1" CR_TAB
		      "mov %C0,%D1" CR_TAB
		      "clr %D0"     CR_TAB
		      "sbrc %C0,7"  CR_TAB
		      "dec %D0");
	    else
	      return ("clr %D0"     CR_TAB
		      "sbrc %D1,7"  CR_TAB
		      "dec %D0"     CR_TAB
		      "mov %C0,%D1" CR_TAB
		      "mov %B0,%C1" CR_TAB
		      "mov %A0,%B1");
	  }

	case 16:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);

	    if (reg0 == reg1 + 2)
	      return *len = 4, ("clr %D0"     CR_TAB
				"sbrc %B0,7"  CR_TAB
				"com %D0"     CR_TAB
				"mov %C0,%D0");
	    if (nxu16_HAVE_MOVW)
	      return *len = 5, ("movw %A0,%C1" CR_TAB
				"clr %D0"      CR_TAB
				"sbrc %B0,7"   CR_TAB
				"com %D0"      CR_TAB
				"mov %C0,%D0");
	    else
	      return *len = 6, ("mov %B0,%D1" CR_TAB
				"mov %A0,%C1" CR_TAB
				"clr %D0"     CR_TAB
				"sbrc %B0,7"  CR_TAB
				"com %D0"     CR_TAB
				"mov %C0,%D0");
	  }

	case 24:
	  return *len = 6, ("mov %A0,%D1" CR_TAB
			    "clr %D0"     CR_TAB
			    "sbrc %A0,7"  CR_TAB
			    "com %D0"     CR_TAB
			    "mov %B0,%D0" CR_TAB
			    "mov %C0,%D0");

	default:
	  if (INTVAL (operands[2]) < 32)
	    break;

	  /* fall through */

	case 31:
	  if (nxu16_HAVE_MOVW)
	    return *len = 4, ("lsl %D0"     CR_TAB
			      "sbc %A0,%A0" CR_TAB
			      "mov %B0,%A0" CR_TAB
			      "movw %C0,%A0");
	  else
	    return *len = 5, ("lsl %D0"     CR_TAB
			      "sbc %A0,%A0" CR_TAB
			      "mov %B0,%A0" CR_TAB
			      "mov %C0,%A0" CR_TAB
			      "mov %D0,%A0");
	}
      len = t;
    }
  out_shift_with_cnt ("asr %D0" CR_TAB
		      "ror %C0" CR_TAB
		      "ror %B0" CR_TAB
		      "ror %A0", insn, operands, len, 4);
  return "";
}

/* 8-bit logic shift right ((unsigned char)x >> i) */

const char *
lshrqi3_out (rtx_insn *insn, rtx operands[], int *len)
{
  if (CONST_INT_P (operands[2]))
    {
      int k;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	default:
	  if (INTVAL (operands[2]) < 8)
	    break;

	  *len = 1;
	  return "clr %0";

	case 1:
	  *len = 1;
	  return "lsr %0";

	case 2:
	  *len = 2;
	  return ("lsr %0" CR_TAB
		  "lsr %0");
	case 3:
	  *len = 3;
	  return ("lsr %0" CR_TAB
		  "lsr %0" CR_TAB
		  "lsr %0");

	case 4:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len=2;
	      return ("swap %0" CR_TAB
		      "andi %0,0x0f");
	    }
	  *len = 4;
	  return ("lsr %0" CR_TAB
		  "lsr %0" CR_TAB
		  "lsr %0" CR_TAB
		  "lsr %0");

	case 5:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len = 3;
	      return ("swap %0" CR_TAB
		      "lsr %0"  CR_TAB
		      "andi %0,0x7");
	    }
	  *len = 5;
	  return ("lsr %0" CR_TAB
		  "lsr %0" CR_TAB
		  "lsr %0" CR_TAB
		  "lsr %0" CR_TAB
		  "lsr %0");

	case 6:
	  if (test_hard_reg_class (LD_REGS, operands[0]))
	    {
	      *len = 4;
	      return ("swap %0" CR_TAB
		      "lsr %0"  CR_TAB
		      "lsr %0"  CR_TAB
		      "andi %0,0x3");
	    }
	  *len = 6;
	  return ("lsr %0" CR_TAB
		  "lsr %0" CR_TAB
		  "lsr %0" CR_TAB
		  "lsr %0" CR_TAB
		  "lsr %0" CR_TAB
		  "lsr %0");

	case 7:
	  *len = 3;
	  return ("bst %1,7" CR_TAB
		  "clr %0"   CR_TAB
		  "bld %0,0");
	}
    }
  else if (CONSTANT_P (operands[2]))
    fatal_insn ("internal compiler error.  Incorrect shift:", insn);

  out_shift_with_cnt ("lsr %0",
		      insn, operands, len, 1);
  return "";
}

/* 16-bit logic shift right ((unsigned short)x >> i) */

const char *
lshrhi3_out (rtx_insn *insn, rtx operands[], int *len)
{
  if (CONST_INT_P (operands[2]))
    {
      int scratch = (GET_CODE (PATTERN (insn)) == PARALLEL
		     && XVECLEN (PATTERN (insn), 0) == 3
		     && REG_P (operands[3]));
      int ldi_ok = test_hard_reg_class (LD_REGS, operands[0]);
      int k;
      int *t = len;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	default:
	  if (INTVAL (operands[2]) < 16)
	    break;

	  *len = 2;
	  return ("clr %B0" CR_TAB
		  "clr %A0");

	case 4:
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  if (ldi_ok)
	    {
	      *len = 6;
	      return ("swap %B0"      CR_TAB
		      "swap %A0"      CR_TAB
		      "andi %A0,0x0f" CR_TAB
		      "eor %A0,%B0"   CR_TAB
		      "andi %B0,0x0f" CR_TAB
		      "eor %A0,%B0");
	    }
	  if (scratch)
	    {
	      *len = 7;
	      return ("swap %B0"    CR_TAB
		      "swap %A0"    CR_TAB
		      "ldi %3,0x0f" CR_TAB
		      "and %A0,%3"  CR_TAB
		      "eor %A0,%B0" CR_TAB
		      "and %B0,%3"  CR_TAB
		      "eor %A0,%B0");
	    }
	  break;  /* optimize_size ? 6 : 8 */

	case 5:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  if (ldi_ok)
	    {
	      *len = 8;
	      return ("lsr %B0"       CR_TAB
		      "ror %A0"       CR_TAB
		      "swap %B0"      CR_TAB
		      "swap %A0"      CR_TAB
		      "andi %A0,0x0f" CR_TAB
		      "eor %A0,%B0"   CR_TAB
		      "andi %B0,0x0f" CR_TAB
		      "eor %A0,%B0");
	    }
	  if (scratch)
	    {
	      *len = 9;
	      return ("lsr %B0"     CR_TAB
		      "ror %A0"     CR_TAB
		      "swap %B0"    CR_TAB
		      "swap %A0"    CR_TAB
		      "ldi %3,0x0f" CR_TAB
		      "and %A0,%3"  CR_TAB
		      "eor %A0,%B0" CR_TAB
		      "and %B0,%3"  CR_TAB
		      "eor %A0,%B0");
	    }
	  break;  /* 10 */

	case 6:
	  if (optimize_size)
	    break;  /* scratch ? 5 : 6 */
	  *len = 9;
	  return ("clr __tmp_reg__" CR_TAB
		  "lsl %A0"         CR_TAB
		  "rol %B0"         CR_TAB
		  "rol __tmp_reg__" CR_TAB
		  "lsl %A0"         CR_TAB
		  "rol %B0"         CR_TAB
		  "rol __tmp_reg__" CR_TAB
		  "mov %A0,%B0"     CR_TAB
		  "mov %B0,__tmp_reg__");

	case 7:
	  *len = 5;
	  return ("lsl %A0"     CR_TAB
		  "mov %A0,%B0" CR_TAB
		  "rol %A0"     CR_TAB
		  "sbc %B0,%B0" CR_TAB
		  "neg %B0");

	case 8:
	  return *len = 2, ("mov %A0,%B1" CR_TAB
			    "clr %B0");

	case 9:
	  *len = 3;
	  return ("mov %A0,%B0" CR_TAB
		  "clr %B0"     CR_TAB
		  "lsr %A0");

	case 10:
	  *len = 4;
	  return ("mov %A0,%B0" CR_TAB
		  "clr %B0"     CR_TAB
		  "lsr %A0"     CR_TAB
		  "lsr %A0");

	case 11:
	  *len = 5;
	  return ("mov %A0,%B0" CR_TAB
		  "clr %B0"     CR_TAB
		  "lsr %A0"     CR_TAB
		  "lsr %A0"     CR_TAB
		  "lsr %A0");

	case 12:
	  if (ldi_ok)
	    {
	      *len = 4;
	      return ("mov %A0,%B0" CR_TAB
		      "clr %B0"     CR_TAB
		      "swap %A0"    CR_TAB
		      "andi %A0,0x0f");
	    }
	  if (scratch)
	    {
	      *len = 5;
	      return ("mov %A0,%B0" CR_TAB
		      "clr %B0"     CR_TAB
		      "swap %A0"    CR_TAB
		      "ldi %3,0x0f" CR_TAB
		      "and %A0,%3");
	    }
	  *len = 6;
	  return ("mov %A0,%B0" CR_TAB
		  "clr %B0"     CR_TAB
		  "lsr %A0"     CR_TAB
		  "lsr %A0"     CR_TAB
		  "lsr %A0"     CR_TAB
		  "lsr %A0");

	case 13:
	  if (ldi_ok)
	    {
	      *len = 5;
	      return ("mov %A0,%B0" CR_TAB
		      "clr %B0"     CR_TAB
		      "swap %A0"    CR_TAB
		      "lsr %A0"     CR_TAB
		      "andi %A0,0x07");
	    }
	  if (nxu16_HAVE_MUL && scratch)
	    {
	      *len = 5;
	      return ("ldi %3,0x08" CR_TAB
		      "mul %B0,%3"  CR_TAB
		      "mov %A0,r1"  CR_TAB
		      "clr %B0"     CR_TAB
		      "clr __zero_reg__");
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  if (scratch)
	    {
	      *len = 6;
	      return ("mov %A0,%B0" CR_TAB
		      "clr %B0"     CR_TAB
		      "swap %A0"    CR_TAB
		      "lsr %A0"     CR_TAB
		      "ldi %3,0x07" CR_TAB
		      "and %A0,%3");
	    }
	  if (nxu16_HAVE_MUL)
	    {
	      *len = 6;
	      return ("set"        CR_TAB
		      "bld r1,3"   CR_TAB
		      "mul %B0,r1" CR_TAB
		      "mov %A0,r1" CR_TAB
		      "clr %B0"    CR_TAB
		      "clr __zero_reg__");
	    }
	  *len = 7;
	  return ("mov %A0,%B0" CR_TAB
		  "clr %B0"     CR_TAB
		  "lsr %A0"     CR_TAB
		  "lsr %A0"     CR_TAB
		  "lsr %A0"     CR_TAB
		  "lsr %A0"     CR_TAB
		  "lsr %A0");

	case 14:
	  if (nxu16_HAVE_MUL && ldi_ok)
	    {
	      *len = 5;
	      return ("ldi %A0,0x04" CR_TAB
		      "mul %B0,%A0"  CR_TAB
		      "mov %A0,r1"   CR_TAB
		      "clr %B0"      CR_TAB
		      "clr __zero_reg__");
	    }
	  if (nxu16_HAVE_MUL && scratch)
	    {
	      *len = 5;
	      return ("ldi %3,0x04" CR_TAB
		      "mul %B0,%3"  CR_TAB
		      "mov %A0,r1"  CR_TAB
		      "clr %B0"     CR_TAB
		      "clr __zero_reg__");
	    }
	  if (optimize_size && ldi_ok)
	    {
	      *len = 5;
	      return ("mov %A0,%B0" CR_TAB
		      "ldi %B0,6" "\n1:\t"
		      "lsr %A0"     CR_TAB
		      "dec %B0"     CR_TAB
		      "brne 1b");
	    }
	  if (optimize_size && scratch)
	    break;  /* 5 */
	  *len = 6;
	  return ("clr %A0" CR_TAB
		  "lsl %B0" CR_TAB
		  "rol %A0" CR_TAB
		  "lsl %B0" CR_TAB
		  "rol %A0" CR_TAB
		  "clr %B0");

	case 15:
	  *len = 4;
	  return ("bst %B1,7" CR_TAB
		  "clr %A0"   CR_TAB
		  "clr %B0"   CR_TAB
		  "bld %A0,0");
	}
      len = t;
    }
  out_shift_with_cnt ("lsr %B0" CR_TAB
		      "ror %A0", insn, operands, len, 2);
  return "";
}


/* 24-bit logic shift right */

const char *
nxu16_out_lshrpsi3 (rtx_insn *insn, rtx *op, int *plen)
{
  int dest = REGNO (op[0]);
  int src = REGNO (op[1]);

  if (CONST_INT_P (op[2]))
    {
      if (plen)
	*plen = 0;

      switch (INTVAL (op[2]))
	{
	case 8:
	  if (dest <= src)
	    return nxu16_asm_len ("mov %A0,%B1" CR_TAB
				"mov %B0,%C1" CR_TAB
				"clr %C0", op, plen, 3);
	  else
	    return nxu16_asm_len ("clr %C0"     CR_TAB
				"mov %B0,%C1" CR_TAB
				"mov %A0,%B1", op, plen, 3);

	case 16:
	  if (dest != src + 2)
	    nxu16_asm_len ("mov %A0,%C1", op, plen, 1);

	  return nxu16_asm_len ("clr %B0"  CR_TAB
			      "clr %C0", op, plen, 2);

	default:
	  if (INTVAL (op[2]) < 24)
	    break;

	  /* fall through */

	case 23:
	  return nxu16_asm_len ("bst %C1,7" CR_TAB
			      "clr %A0"   CR_TAB
			      "clr %B0"   CR_TAB
			      "clr %C0"   CR_TAB
			      "bld %A0,0", op, plen, 5);
	} /* switch */
    }

  out_shift_with_cnt ("lsr %C0" CR_TAB
		      "ror %B0" CR_TAB
		      "ror %A0", insn, op, plen, 3);
  return "";
}


/* 32-bit logic shift right ((unsigned int)x >> i) */

const char *
lshrsi3_out (rtx_insn *insn, rtx operands[], int *len)
{
  if (CONST_INT_P (operands[2]))
    {
      int k;
      int *t = len;

      if (!len)
	len = &k;

      switch (INTVAL (operands[2]))
	{
	default:
	  if (INTVAL (operands[2]) < 32)
	    break;

	  if (nxu16_HAVE_MOVW)
	    return *len = 3, ("clr %D0" CR_TAB
			      "clr %C0" CR_TAB
			      "movw %A0,%C0");
	  *len = 4;
	  return ("clr %D0" CR_TAB
		  "clr %C0" CR_TAB
		  "clr %B0" CR_TAB
		  "clr %A0");

	case 8:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);
	    *len = 4;
	    if (reg0 <= reg1)
	      return ("mov %A0,%B1" CR_TAB
		      "mov %B0,%C1" CR_TAB
		      "mov %C0,%D1" CR_TAB
		      "clr %D0");
	    else
	      return ("clr %D0"     CR_TAB
		      "mov %C0,%D1" CR_TAB
		      "mov %B0,%C1" CR_TAB
		      "mov %A0,%B1");
	  }

	case 16:
	  {
	    int reg0 = true_regnum (operands[0]);
	    int reg1 = true_regnum (operands[1]);

	    if (reg0 == reg1 + 2)
	      return *len = 2, ("clr %C0"     CR_TAB
				"clr %D0");
	    if (nxu16_HAVE_MOVW)
	      return *len = 3, ("movw %A0,%C1" CR_TAB
				"clr %C0"      CR_TAB
				"clr %D0");
	    else
	      return *len = 4, ("mov %B0,%D1" CR_TAB
				"mov %A0,%C1" CR_TAB
				"clr %C0"     CR_TAB
				"clr %D0");
	  }

	case 24:
	  return *len = 4, ("mov %A0,%D1" CR_TAB
			    "clr %B0"     CR_TAB
			    "clr %C0"     CR_TAB
			    "clr %D0");

	case 31:
	  if (nxu16_HAVE_MOVW)
	    return *len = 5, ("bst %D1,7"    CR_TAB
			      "clr %A0"      CR_TAB
			      "clr %B0"      CR_TAB
			      "movw %C0,%A0" CR_TAB
			      "bld %A0,0");
	  *len = 6;
	  return ("bst %D1,7" CR_TAB
		  "clr %A0"   CR_TAB
		  "clr %B0"   CR_TAB
		  "clr %C0"   CR_TAB
		  "clr %D0"   CR_TAB
		  "bld %A0,0");
	}
      len = t;
    }
  out_shift_with_cnt ("lsr %D0" CR_TAB
		      "ror %C0" CR_TAB
		      "ror %B0" CR_TAB
		      "ror %A0", insn, operands, len, 4);
  return "";
}


/* When INSN is a PARALLEL with two SETs, a SET of REG_CC and a SET of a
   GPR, then return the second SET and set *CCMODE to the first SET's mode.
   Otherwise, return single_set and set *CCMODE to VOIDmode.  */

static rtx
nxu16_cc_set (rtx_insn *insn, machine_mode *ccmode)
{
  // single_set() not only depends on the anatomy of an insn but also
  // on REG_UNUSED notes, thus we have to analyze by hand so that the
  // result only depends on the pattern.

  rtx pat = PATTERN (insn);

  if (GET_CODE (pat) == PARALLEL
      && XVECLEN (pat, 0) == 2
      && GET_CODE (XVECEXP (pat, 0, 0)) == SET
      && GET_CODE (XVECEXP (pat, 0, 1)) == SET)
    {
      rtx ccset = XVECEXP (pat, 0, 0);
      *ccmode = GET_MODE (SET_DEST (ccset));
      return XVECEXP (pat, 0, 1);
    }

  *ccmode = VOIDmode;
  return single_set (insn);
}


/* Output addition of registers YOP[0] and YOP[1]

      YOP[0] += extend (YOP[1])

   or subtraction of registers YOP[0] and YOP[2]

      YOP[0] -= extend (YOP[2])

   where the integer modes satisfy  SI >= YOP[0].mode >= YOP[1/2].mode >= QI,
   and the extension may be sign-extend, zero-extend or reg (no extend).
   INSN is either a single_set or a true parallel insn.  In the latter case,
   INSN has two SETs: A SET of REG_CC and a SET like in the single_set case.
   Returns "".

   If PLEN == NULL output the instructions.
   If PLEN != NULL set *PLEN to the length of the sequence in words.  */

const char *
nxu16_out_plus_ext (rtx_insn *insn, rtx *yop, int *plen)
{
  rtx regs[2];
  machine_mode ccmode;

  /* Ouch! Whether or not an insn is a single_set does not only depend
     on the anatomy of the pattern, but also on REG_UNUSED notes.
     Hence we have to dig by hand... */

  const rtx src = SET_SRC (nxu16_cc_set (insn, &ccmode));
  const rtx_code add = GET_CODE (src);
  gcc_assert (GET_CODE (src) == PLUS || GET_CODE (src) == MINUS);

  // Use XOP[] in the remainder with XOP[0] = YOP[0] and XOP[1] = YOP[1/2].
  rtx xop[2] = { yop[0], yop[add == PLUS ? 1 : 2] };
  const rtx xreg = XEXP (src, add == PLUS ? 1 : 0);
  const rtx xext = XEXP (src, add == PLUS ? 0 : 1);
  const rtx_code ext = GET_CODE (xext);

  gcc_assert (REG_P (xreg)
	      && (ext == ZERO_EXTEND || ext == SIGN_EXTEND || ext == REG));

  const int n_bytes0 = GET_MODE_SIZE (GET_MODE (xop[0]));
  const int n_bytes1 = GET_MODE_SIZE (GET_MODE (xop[1]));
  rtx msb1 = all_regs_rtx[n_bytes1 - 1 + REGNO (xop[1])];

  const char *const s_ADD = add == PLUS ? "add %0,%1" : "sub %0,%1";
  const char *const s_ADC = add == PLUS ? "adc %0,%1" : "sbc %0,%1";
  const char *const s_DEC = add == PLUS
    ? "adc %0,__zero_reg__"  CR_TAB  "sbrc %1,7"  CR_TAB  "dec %0"
    : "sbc %0,__zero_reg__"  CR_TAB  "sbrc %1,7"  CR_TAB  "inc %0";

  // A register that containts 8 copies of $1.msb.
  rtx ext_reg = ext == ZERO_EXTEND ? zero_reg_rtx : NULL_RTX;

  if (plen)
    *plen = 0;

  if (ext == SIGN_EXTEND
      && (n_bytes0 > 1 + n_bytes1
	  || reg_overlap_mentioned_p (msb1, xop[0])
	  // The insn also wants to set SREG.N and SREG.Z.
	  || ccmode == CCZNmode))
    {
      // Sign-extending more than one byte: Set tmp_reg to 0 or -1
      // depending on $1.msb. Same for the pathological case where
      // $0 and $1 overlap.

      regs[0] = ext_reg = tmp_reg_rtx;
      regs[1] = msb1;

      nxu16_asm_len ("mov %0,%1"  CR_TAB
		   "lsl %0"     CR_TAB
		   "sbc %0,%0", regs, plen, 3);
    }

  // Adding the bytes of $1 is just plain additions / subtractions.
  // Same for the extended bytes when we have ext_reg.

  nxu16_asm_len (s_ADD, xop, plen, 1);

  for (int i = 1; i < n_bytes0; ++i)
    {
      regs[0] = all_regs_rtx[i + REGNO (xop[0])];
      regs[1] = i < n_bytes1 ? all_regs_rtx[i + REGNO (xop[1])] : ext_reg;

      if (! regs[1])
	{
	  // Extending just 1 byte:  This is one instruction shorter
	  // than sign-extending $1.msb to tmp_reg.

	  regs[1] = msb1;
	  nxu16_asm_len (s_DEC, regs, plen, 3);
	}
      else
	nxu16_asm_len (s_ADC, regs, plen, 1);
    }

  return "";
}


/* Output addition of register XOP[0] and compile time constant XOP[2].
   INSN is a single_set insn or an insn pattern.
   CODE == PLUS:  perform addition by using ADD instructions or
   CODE == MINUS: perform addition by using SUB instructions:

      XOP[0] = XOP[0] + XOP[2]

   Or perform addition/subtraction with register XOP[2] depending on CODE:

      XOP[0] = XOP[0] +/- XOP[2]

   If PLEN == NULL, print assembler instructions to perform the operation;
   otherwise, set *PLEN to the length of the instruction sequence (in words)
   printed with PLEN == NULL.  XOP[3] is an 8-bit scratch register or NULL_RTX.

   CODE_SAT == UNKNOWN: Perform ordinary, non-saturating operation.
   CODE_SAT != UNKNOWN: Perform operation and saturate according to CODE_SAT.
   If  CODE_SAT != UNKNOWN  then SIGN contains the sign of the summand resp.
   the subtrahend in the original insn, provided it is a compile time constant.
   In all other cases, SIGN is 0.

   If OUT_LABEL is true, print the final 0: label which is needed for
   saturated addition / subtraction.  The only case where OUT_LABEL = false
   is useful is for saturated addition / subtraction performed during
   fixed-point rounding, cf. `nxu16_out_round'.  */

static void
nxu16_out_plus_1 (rtx insn, rtx *xop, int *plen, rtx_code code,
		rtx_code code_sat, int sign, bool out_label)
{
  /* MODE of the operation.  */
  machine_mode mode = GET_MODE (xop[0]);

  /* INT_MODE of the same size.  */
  scalar_int_mode imode = int_mode_for_mode (mode).require ();

  /* Number of bytes to operate on.  */
  int n_bytes = GET_MODE_SIZE (mode);

  /* Value (0..0xff) held in clobber register op[3] or -1 if unknown.  */
  int clobber_val = -1;

  /* op[0]: 8-bit destination register
     op[1]: 8-bit const int
     op[2]: 8-bit scratch register */
  rtx op[3];

  /* Started the operation?  Before starting the operation we may skip
     adding 0.  This is no more true after the operation started because
     carry must be taken into account.  */
  bool started = false;

  /* Value to add.  There are two ways to add VAL: R += VAL and R -= -VAL.  */
  rtx xval = xop[2];

  /* Output a BRVC instruction.  Only needed with saturation.  */
  bool out_brvc = true;

  if (plen)
    *plen = 0;

  if (REG_P (xop[2]))
    {
      for (int i = 0; i < n_bytes; i++)
	{
	  /* We operate byte-wise on the destination.  */
	  op[0] = nxu16_byte (xop[0], i);
	  op[1] = nxu16_byte (xop[2], i);

	  if (i == 0)
	    nxu16_asm_len (code == PLUS ? "add %0,%1" : "sub %0,%1",
			 op, plen, 1);
	  else
	    nxu16_asm_len (code == PLUS ? "adc %0,%1" : "sbc %0,%1",
			 op, plen, 1);
	}

      if (reg_overlap_mentioned_p (xop[0], xop[2]))
	{
	  gcc_assert (REGNO (xop[0]) == REGNO (xop[2]));

	  if (MINUS == code)
	    return;
	}

      goto saturate;
    }

  if (CONST_FIXED_P (xval))
    xval = nxu16_to_int_mode (xval);

  /* Adding/Subtracting zero is a no-op.  */

  if (xval == const0_rtx)
    return;

  if (MINUS == code)
    xval = simplify_unary_operation (NEG, imode, xval, imode);

  op[2] = xop[3];

  if (SS_PLUS == code_sat && MINUS == code
      && sign < 0
      && 0x80 == nxu16_uint8 (xval, n_bytes - 1))
    {
      /* We compute x + 0x80 by means of SUB instructions.  We negated the
	 constant subtrahend above and are left with  x - (-128)  so that we
	 need something like SUBI r,128 which does not exist because SUBI sets
	 V according to the sign of the subtrahend.  Notice the only case
	 where this must be done is when NEG overflowed in case [2s] because
	 the V computation needs the right sign of the subtrahend.  */

      rtx msb = nxu16_byte (xop[0], n_bytes - 1);

      nxu16_asm_len ("subi %0,128" CR_TAB
		   "brmi 0f", &msb, plen, 2);
      out_brvc = false;

      goto saturate;
    }

  for (int i = 0; i < n_bytes; i++)
    {
      /* We operate byte-wise on the destination.  */
      rtx reg8 = nxu16_byte (xop[0], i);
      rtx xval8 = nxu16_byte (xval, i);

      /* 8-bit value to operate with this byte. */
      unsigned int val8 = UINTVAL (xval8) & GET_MODE_MASK (QImode);

      /* Registers R16..R31 can operate with immediate.  */
      bool ld_reg_p = test_hard_reg_class (LD_REGS, reg8);

      op[0] = reg8;
      op[1] = gen_int_mode (val8, QImode);

      /* To get usable cc0 no low-bytes must have been skipped.  */

      if (!started
	  && i % 2 == 0
	  && i + 2 <= n_bytes
	  && nxu16_adiw_reg_p (reg8))
	{
	  unsigned int val16 = nxu16_uint16 (xval, i);

	  /* Registers R24, X, Y, Z can use ADIW/SBIW with constants < 64
	     i.e. operate word-wise.  */

	  if (val16 < 64)
	    {
	      if (val16 != 0)
		{
		  started = true;
		  nxu16_asm_len (code == PLUS ? "adiw %0,%1" : "sbiw %0,%1",
			       op, plen, 1);
		}

	      i++;
	      continue;
	    }
	}

      if (nxu16_TINY
	  && optimize
	  && i == 0
	  && n_bytes == 2
	  // When that pass adjusts the frame pointer, then we know that
	  // reg Y points to ordinary memory, and the only side-effect
	  // of -Y and Y+ is the side effect on Y.
	  && nxu16_fuse_add >= 2
	  && frame_pointer_needed
	  && REGNO (xop[0]) == FRAME_POINTER_REGNUM)
	{
	  if (INSN_P (insn)
	      && _reg_unused_after (as_a <rtx_insn *> (insn), xop[0], false))
	    return;

	  if (nxu16_HAVE_8BIT_SP)
	    {
	      nxu16_asm_len ("subi %A0,%n2", xop, plen, 1);
	      return;
	    }
	  else if (xop[2] == const1_rtx || xop[2] == constm1_rtx)
	    {
	      nxu16_asm_len (xop[2] == const1_rtx
			   ? "ld __tmp_reg__,%a0+"
			   : "ld __tmp_reg__,-%a0", xop, plen, 1);
	      return;
	    }
	}

      if (val8 == 0)
	{
	  if (started)
	    nxu16_asm_len (code == PLUS
			 ? "adc %0,__zero_reg__" : "sbc %0,__zero_reg__",
			 op, plen, 1);
	  continue;
	}
      else if ((val8 == 1 || val8 == 0xff)
	       && UNKNOWN == code_sat
	       && !started
	       && i == n_bytes - 1)
	{
	  nxu16_asm_len ((code == PLUS) ^ (val8 == 1) ? "dec %0" : "inc %0",
		       op, plen, 1);
	  break;
	}

      switch (code)
	{
	case PLUS:

	  gcc_assert (plen != NULL || (op[2] && REG_P (op[2])));

	  if (plen != NULL && UNKNOWN != code_sat)
	    {
	      /* This belongs to the x + 0x80 corner case.  The code with
		 ADD instruction is not smaller, thus make this case
		 expensive so that the caller won't pick it.  */

	      *plen += 10;
	      break;
	    }

	  if (clobber_val != (int) val8)
	    nxu16_asm_len ("ldi %2,%1", op, plen, 1);
	  clobber_val = (int) val8;

	  nxu16_asm_len (started ? "adc %0,%2" : "add %0,%2", op, plen, 1);

	  break; /* PLUS */

	case MINUS:

	  if (ld_reg_p)
	    nxu16_asm_len (started ? "sbci %0,%1" : "subi %0,%1", op, plen, 1);
	  else
	    {
	      gcc_assert (plen != NULL || REG_P (op[2]));

	      if (clobber_val != (int) val8)
		nxu16_asm_len ("ldi %2,%1", op, plen, 1);
	      clobber_val = (int) val8;

	      nxu16_asm_len (started ? "sbc %0,%2" : "sub %0,%2", op, plen, 1);
	    }

	  break; /* MINUS */

	default:
	  /* Unknown code */
	  gcc_unreachable();
	}

      started = true;

    } /* for all sub-bytes */

 saturate:

  if (UNKNOWN == code_sat)
    return;

  /* Vanilla addition/subtraction is done.  We are left with saturation.

     We have to compute  A = A <op> B  where  A  is a register and
     B is a register or a non-zero compile time constant CONST.
     A is register class "r" if unsigned && B is REG.  Otherwise, A is in "d".
     B stands for the original operand $2 in INSN.  In the case of B = CONST,
     SIGN in { -1, 1 } is the sign of B.  Otherwise, SIGN is 0.

     CODE is the instruction flavor we use in the asm sequence to perform <op>.


     unsigned
     operation        |  code |  sat if  |    b is      | sat value |  case
     -----------------+-------+----------+--------------+-----------+-------
     +  as  a + b     |  add  |  C == 1  |  const, reg  | u+ = 0xff |  [1u]
     +  as  a - (-b)  |  sub  |  C == 0  |  const       | u+ = 0xff |  [2u]
     -  as  a - b     |  sub  |  C == 1  |  const, reg  | u- = 0    |  [3u]
     -  as  a + (-b)  |  add  |  C == 0  |  const       | u- = 0    |  [4u]


     signed
     operation        |  code |  sat if  |    b is      | sat value |  case
     -----------------+-------+----------+--------------+-----------+-------
     +  as  a + b     |  add  |  V == 1  |  const, reg  | s+        |  [1s]
     +  as  a - (-b)  |  sub  |  V == 1  |  const       | s+        |  [2s]
     -  as  a - b     |  sub  |  V == 1  |  const, reg  | s-        |  [3s]
     -  as  a + (-b)  |  add  |  V == 1  |  const       | s-        |  [4s]

     s+  =  b < 0  ?  -0x80 :  0x7f
     s-  =  b < 0  ?   0x7f : -0x80

     The cases a - b actually perform  a - (-(-b))  if B is CONST.
  */

  op[0] = nxu16_byte (xop[0], n_bytes - 1);
  op[1] = n_bytes > 1 ? nxu16_byte (xop[0], n_bytes - 2) : NULL_RTX;

  bool need_copy = true;
  int len_call = 1 + nxu16_HAVE_JMP_CALL;

  switch (code_sat)
    {
    default:
      gcc_unreachable();

    case SS_PLUS:
    case SS_MINUS:

      if (out_brvc)
	nxu16_asm_len ("brvc 0f", op, plen, 1);

      if (reg_overlap_mentioned_p (xop[0], xop[2]))
	{
	  /* [1s,reg] */

	  if (n_bytes == 1)
	    nxu16_asm_len ("ldi %0,0x7f" CR_TAB
			 "adc %0,__zero_reg__", op, plen, 2);
	  else
	    nxu16_asm_len ("ldi %0,0x7f" CR_TAB
			 "ldi %1,0xff" CR_TAB
			 "adc %1,__zero_reg__" CR_TAB
			 "adc %0,__zero_reg__", op, plen, 4);
	}
      else if (sign == 0 && PLUS == code)
	{
	  /* [1s,reg] */

	  op[2] = nxu16_byte (xop[2], n_bytes - 1);

	  if (n_bytes == 1)
	    nxu16_asm_len ("ldi %0,0x80" CR_TAB
			 "sbrs %2,7"   CR_TAB
			 "dec %0", op, plen, 3);
	  else
	    nxu16_asm_len ("ldi %0,0x80" CR_TAB
			 "cp %2,%0"    CR_TAB
			 "sbc %1,%1"   CR_TAB
			 "sbci %0,0", op, plen, 4);
	}
      else if (sign == 0 && MINUS == code)
	{
	  /* [3s,reg] */

	  op[2] = nxu16_byte (xop[2], n_bytes - 1);

	  if (n_bytes == 1)
	    nxu16_asm_len ("ldi %0,0x7f" CR_TAB
			 "sbrs %2,7"   CR_TAB
			 "inc %0", op, plen, 3);
	  else
	    nxu16_asm_len ("ldi %0,0x7f" CR_TAB
			 "cp %0,%2"    CR_TAB
			 "sbc %1,%1"   CR_TAB
			 "sbci %0,-1", op, plen, 4);
	}
      else if ((sign < 0) ^ (SS_MINUS == code_sat))
	{
	  /* [1s,const,B < 0] [2s,B < 0] */
	  /* [3s,const,B > 0] [4s,B > 0] */

	  if (n_bytes == 8)
	    {
	      nxu16_asm_len ("%~call __clr_8", op, plen, len_call);
	      need_copy = false;
	    }

	  nxu16_asm_len ("ldi %0,0x80", op, plen, 1);
	  if (n_bytes > 1 && need_copy)
	    nxu16_asm_len ("clr %1", op, plen, 1);
	}
      else if ((sign > 0) ^ (SS_MINUS == code_sat))
	{
	  /* [1s,const,B > 0] [2s,B > 0] */
	  /* [3s,const,B < 0] [4s,B < 0] */

	  if (n_bytes == 8)
	    {
	      nxu16_asm_len ("sec" CR_TAB
			   "%~call __sbc_8", op, plen, 1 + len_call);
	      need_copy = false;
	    }

	  nxu16_asm_len ("ldi %0,0x7f", op, plen, 1);
	  if (n_bytes > 1 && need_copy)
	    nxu16_asm_len ("ldi %1,0xff", op, plen, 1);
	}
      else
	gcc_unreachable();

      break;

    case US_PLUS:
      /* [1u] : [2u] */

      nxu16_asm_len (PLUS == code ? "brcc 0f" : "brcs 0f", op, plen, 1);

      if (n_bytes == 8)
	{
	  if (MINUS == code)
	    nxu16_asm_len ("sec", op, plen, 1);
	  nxu16_asm_len ("%~call __sbc_8", op, plen, len_call);

	  need_copy = false;
	}
      else
	{
	  if (MINUS == code && !test_hard_reg_class (LD_REGS, op[0]))
	    nxu16_asm_len ("sec" CR_TAB
			 "sbc %0,%0", op, plen, 2);
	  else
	    nxu16_asm_len (PLUS == code ? "sbc %0,%0" : "ldi %0,0xff",
			 op, plen, 1);
	}
      break; /* US_PLUS */

    case US_MINUS:
      /* [4u] : [3u] */

      nxu16_asm_len (PLUS == code ? "brcs 0f" : "brcc 0f", op, plen, 1);

      if (n_bytes == 8)
	{
	  nxu16_asm_len ("%~call __clr_8", op, plen, len_call);
	  need_copy = false;
	}
      else
	nxu16_asm_len ("clr %0", op, plen, 1);

      break;
    }

  /* We set the MSB in the unsigned case and the 2 MSBs in the signed case.
     Now copy the right value to the LSBs.  */

  if (need_copy && n_bytes > 1)
    {
      if (US_MINUS == code_sat || US_PLUS == code_sat)
	{
	  nxu16_asm_len ("mov %1,%0", op, plen, 1);

	  if (n_bytes > 2)
	    {
	      op[0] = xop[0];
	      if (nxu16_HAVE_MOVW)
		nxu16_asm_len ("movw %0,%1", op, plen, 1);
	      else
		nxu16_asm_len ("mov %A0,%1" CR_TAB
			     "mov %B0,%1", op, plen, 2);
	    }
	}
      else if (n_bytes > 2)
	{
	  op[0] = xop[0];
	  nxu16_asm_len ("mov %A0,%1" CR_TAB
		       "mov %B0,%1", op, plen, 2);
	}
    }

  if (need_copy && n_bytes == 8)
    {
      if (nxu16_HAVE_MOVW)
	nxu16_asm_len ("movw %r0+2,%0" CR_TAB
		     "movw %r0+4,%0", xop, plen, 2);
      else
	nxu16_asm_len ("mov %r0+2,%0" CR_TAB
		     "mov %r0+3,%0" CR_TAB
		     "mov %r0+4,%0" CR_TAB
		     "mov %r0+5,%0", xop, plen, 4);
    }

  if (out_label)
    nxu16_asm_len ("0:", op, plen, 0);
}


/* Output addition/subtraction of register XOP[0] and a constant XOP[2] that
   is not a compile-time constant:

      XOP[0] = XOP[0] +/- XOP[2]

   This is a helper for the function below.  The only insns that need this
   are additions/subtraction for pointer modes, i.e. HImode and PSImode.  */

static const char *
nxu16_out_plus_symbol (rtx *xop, rtx_code code, int *plen)
{
  machine_mode mode = GET_MODE (xop[0]);

  /* Only pointer modes want to add symbols.  */

  gcc_assert (mode == HImode || mode == PSImode);

  if (mode == HImode
      && const_0mod256_operand (xop[2], HImode))
    return nxu16_asm_len (PLUS == code
			? "subi %B0,hi8(-(%2))"
			: "subi %B0,hi8(%2)", xop, plen, -1);

  nxu16_asm_len (PLUS == code
	       ? "subi %A0,lo8(-(%2))" CR_TAB "sbci %B0,hi8(-(%2))"
	       : "subi %A0,lo8(%2)"    CR_TAB "sbci %B0,hi8(%2)",
	       xop, plen, -2);

  if (PSImode == mode)
    nxu16_asm_len (PLUS == code
		 ? "sbci %C0,hlo8(-(%2))"
		 : "sbci %C0,hlo8(%2)", xop, plen, 1);
  return "";
}


/* Prepare operands of addition/subtraction to be used with nxu16_out_plus_1.

   INSN is a single_set insn or an insn pattern with a binary operation as
   SET_SRC that is one of: PLUS, SS_PLUS, US_PLUS, MINUS, SS_MINUS, US_MINUS.

   XOP are the operands of INSN.  In the case of 64-bit operations with
   constant XOP[] has just one element:  The summand/subtrahend in XOP[0].
   The non-saturating insns up to 32 bits may or may not supply a "d" class
   scratch as XOP[3].

   If PLEN == NULL output the instructions.
   If PLEN != NULL set *PLEN to the length of the sequence in words.

   PLEN defaults to NULL.

   OUT_LABEL defaults to TRUE.  For a description, see nxu16_OUT_PLUS_1.

   Return ""  */

const char *
nxu16_out_plus (rtx insn, rtx *xop, int *plen, bool out_label)
{
  int len_plus, len_minus;
  rtx op[4];
  rtx xpattern = INSN_P (insn) ? single_set (as_a <rtx_insn *> (insn)) : insn;
  rtx xdest = SET_DEST (xpattern);
  machine_mode mode = GET_MODE (xdest);
  scalar_int_mode imode = int_mode_for_mode (mode).require ();
  int n_bytes = GET_MODE_SIZE (mode);
  rtx_code code_sat = GET_CODE (SET_SRC (xpattern));
  rtx_code code
    = (PLUS == code_sat || SS_PLUS == code_sat || US_PLUS == code_sat
       ? PLUS : MINUS);

  /* PLUS and MINUS don't saturate:  Use modular wrap-around.  */

  if (PLUS == code_sat || MINUS == code_sat)
    code_sat = UNKNOWN;

  if (n_bytes <= 4 && REG_P (xop[2]))
    {
      nxu16_out_plus_1 (insn, xop, plen, code, code_sat, 0, out_label);
      return "";
    }

  if (n_bytes == 8)
    {
      op[0] = gen_rtx_REG (DImode, ACC_A);
      op[1] = gen_rtx_REG (DImode, ACC_A);
      op[2] = nxu16_to_int_mode (xop[0]);
    }
  else
    {
      if (!REG_P (xop[2])
	  && !CONST_INT_P (xop[2])
	  && !CONST_FIXED_P (xop[2]))
	{
	  return nxu16_out_plus_symbol (xop, code, plen);
	}

      op[0] = nxu16_to_int_mode (xop[0]);
      op[1] = nxu16_to_int_mode (xop[1]);
      op[2] = nxu16_to_int_mode (xop[2]);
    }

  /* Saturations and 64-bit operations don't have a clobber operand.
     For the other cases, the caller will provide a proper XOP[3].  */

  xpattern = INSN_P (insn) ? PATTERN (insn) : insn;
  op[3] = PARALLEL == GET_CODE (xpattern) ? xop[3] : NULL_RTX;

  /* Saturation will need the sign of the original operand.  */

  int sign = nxu16_int8 (op[2], n_bytes - 1) < 0 ? -1 : 1;

  /* If we subtract and the subtrahend is a constant, then negate it
     so that nxu16_out_plus_1 can be used.  */

  if (MINUS == code)
    op[2] = simplify_unary_operation (NEG, imode, op[2], imode);

  /* Work out the shortest sequence.  */

  nxu16_out_plus_1 (insn, op, &len_minus, MINUS, code_sat, sign, out_label);
  nxu16_out_plus_1 (insn, op, &len_plus, PLUS, code_sat, sign, out_label);

  if (plen)
    *plen = (len_minus <= len_plus) ? len_minus : len_plus;
  else if (len_minus <= len_plus)
    nxu16_out_plus_1 (insn, op, NULL, MINUS, code_sat, sign, out_label);
  else
    nxu16_out_plus_1 (insn, op, NULL, PLUS, code_sat, sign, out_label);

  return "";
}


/* Output an addition with a compile-time constant that sets SREG.N:

      XOP[0] += XOP[1]

   where XOP[0] is a HI, PSI or SI register, and XOP[1] is a register or a
   compile-time constant.  XOP[2] is SCRATCH or a QI clobber reg.  Return "".

   If PLEN == NULL output the instructions.
   If PLEN != NULL set *PLEN to the length of the sequence in words.  */

const char *
nxu16_out_plus_set_N (rtx *xop, int *plen)
{
  gcc_assert (xop[1] != const0_rtx);

  // The output function for vanilla additions, nxu16_out_plus_1, can be
  // used because it always issues an operation on the MSB (except when
  // the addend is zero).

  rtx op[] = { xop[0], xop[0], xop[1], xop[2] };

  if (REG_P (xop[1]))
    {
      nxu16_out_plus_1 (NULL_RTX, op, plen, PLUS, UNKNOWN, 0, false);
    }
  else
    {
      int len_plus, len_minus;

      nxu16_out_plus_1 (NULL_RTX, op, &len_plus,  PLUS,  UNKNOWN, 0, false);
      nxu16_out_plus_1 (NULL_RTX, op, &len_minus, MINUS, UNKNOWN, 0, false);

      nxu16_out_plus_1 (NULL_RTX, op, plen, len_minus < len_plus ? MINUS : PLUS,
		      UNKNOWN, 0, false);
    }

  return "";
}


/* Output an instruction sequence for addition of REG in XOP[0] and CONST_INT
   in XOP[1] in such a way that SREG.Z and SREG.N are set according to the
   result.  The mode is HI, PSI or SI.  XOP[2] might be a d-regs clobber
   register.  If XOP[2] is SCRATCH, then the addition can be performed
   without a clobber reg.  Return "".

   If PLEN == NULL, then output the instructions.
   If PLEN != NULL, then set *PLEN to the length of the sequence in words. */

const char *
nxu16_out_plus_set_ZN (rtx *xop, int *plen)
{
  if (plen)
    *plen = 0;

  // Register to compare and value to compare against.
  rtx xreg = xop[0];
  rtx xval = xop[1];

  machine_mode mode = GET_MODE (xreg);

  // Number of bytes to operate on.
  int n_bytes = GET_MODE_SIZE (mode);

  if (n_bytes == 2
      && nxu16_adiw_reg_p (xreg)
      && IN_RANGE (INTVAL (xval), 1, 63))
    {
      // Add 16-bit value in [1..63] to a w register.
      return nxu16_asm_len ("adiw %0,%1", xop, plen, 1);
    }

  // Addition won't work; subtract the negative of XVAL instead.
  xval = simplify_unary_operation (NEG, mode, xval, mode);

  // Value (0..0xff) held in clobber register xop[2] or -1 if unknown.
  int clobber_val = -1;

  // [0] = Current sub-register.
  // [1] = Current partial xval.
  // [2] = 8-bit clobber d-register or SCRATCH.
  rtx op[3];
  op[2] = xop[2];

  // Work byte-wise from LSB to MSB.  The lower two bytes might be
  // SBIW'ed in one go.
  for (int i = 0; i < n_bytes; ++i)
    {
      op[0] = nxu16_byte (xreg, i);

      if (i == 0
	  && n_bytes >= 2
	  && nxu16_adiw_reg_p (op[0]))
	{
	  op[1] = nxu16_word (xval, 0);
	  if (IN_RANGE (INTVAL (op[1]), 0, 63))
	    {
	      // SBIW can handle the lower 16 bits.
	      nxu16_asm_len ("sbiw %0,%1", op, plen, 1);

	      // Next byte has already been handled: Skip it.
	      ++i;
	      continue;
	    }
	}

      op[1] = nxu16_byte (xval, i);

      if (test_hard_reg_class (LD_REGS, op[0]))
	{
	  // d-regs can subtract immediates.
	  nxu16_asm_len (i == 0
		       ? "subi %0,%1"
		       : "sbci %0,%1", op, plen, 1);
	}
      else
	{
	  int val8 = 0xff & INTVAL (op[1]);
	  if (val8 == 0)
	    {
	      // Any register can subtract 0.
	      nxu16_asm_len (i == 0
			   ? "sub %0,__zero_reg__"
			   : "sbc %0,__zero_reg__", op, plen, 1);
	    }
	  else
	    {
	      // Use d-register to hold partial xval.

	      if (val8 != clobber_val)
		{
		  // Load partial xval to QI clobber reg and memoize for later.
		  gcc_assert (REG_P (op[2]));
		  nxu16_asm_len ("ldi %2,%1", op, plen, 1);
		  clobber_val = val8;
		}

	      nxu16_asm_len (i == 0
			   ? "sub %0,%2"
			   : "sbc %0,%2", op, plen, 1);
	    }
	}
    } // Loop bytes.

  return "";
}


/* A helper worker for `op8_ZN_operator'.  Allow

     OP0 <code> OP1

  QImode operations that set SREG.N and SREG.Z in a usable way.
  these are:

  * OP0 is a QImode register, and
  * OP1 is a QImode register or CONST_INT, and

  the allowed operations is one of:

  * SHIFTs with a const_int offset in { 1, 2, 3 }.
  * MINUS and XOR with a register operand
  * IOR and AND with a register operand, or d-reg + const_int
  * PLUS with a register operand, or d-reg + const_int,
    or a const_int in { -2, -1, 1, 2 }.  */

bool
nxu16_op8_ZN_operator (rtx op)
{
  const rtx_code code = GET_CODE (op);
  rtx op0 = XEXP (op, 0);
  rtx op1 = XEXP (op, 1);

  if (! register_operand (op0, QImode)
      || ! (register_operand (op1, QImode)
	    || const_int_operand (op1, QImode)))
    return false;

  const bool reg1_p = REG_P (op1);
  const bool ld_reg0_p = test_hard_reg_class (LD_REGS, op0);

  switch (code)
    {
    default:
      break;

    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      return const_1_to_3_operand (op1, QImode);

    case MINUS:
    case XOR:
      return reg1_p;

    case IOR:
    case AND:
      return reg1_p || ld_reg0_p;

    case PLUS:
      return reg1_p || ld_reg0_p || abs1_abs2_operand (op1, QImode);
    }

  return false;
}


/* Output a QImode instruction sequence for

      XOP[0] = XOP[0]  <CODE>  XOP[2]

   where XOP[0] is a register, and the possible operands and CODEs
   are according to  `nxu16_op8_ZN_operator'  from above.  Return "".

   If PLEN == NULL, then output the instructions.
   If PLEN != NULL, then set *PLEN to the length of the sequence in words.  */

const char *
nxu16_out_op8_set_ZN (rtx_code code, rtx *xop, int *plen)
{
  const bool reg2_p = REG_P (xop[2]);
  const int ival = CONST_INT_P (xop[2]) ? (int) INTVAL (xop[2]) : 0;

  gcc_assert (op8_ZN_operator (gen_rtx_fmt_ee (code, QImode, xop[0], xop[2]),
			       QImode));
  if (plen)
    *plen = 0;

  const char *tpl = nullptr;
  int times = 1;

  if (code == ASHIFT)
    tpl = "lsl %0", times = ival;
  else if (code == LSHIFTRT)
    tpl = "lsr %0", times = ival;
  else if (code == ASHIFTRT)
    tpl = "asr %0", times = ival;
  else if (code == MINUS)
    tpl = "sub %0,%2";
  else if (code == XOR)
    tpl = "eor %0,%2";
  else if (code == AND)
    tpl = reg2_p ? "and %0,%2" : "andi %0,lo8(%2)";
  else if (code == IOR)
    tpl = reg2_p ? "or %0,%2" : "ori %0,lo8(%2)";
  else if (code == PLUS)
    {
      if (ival
	  && ! test_hard_reg_class (LD_REGS, xop[0]))
	{
	  tpl = ival > 0 ? "inc %0" : "dec %0";
	  times = std::abs (ival);
	}
      else
	tpl = reg2_p ? "add %0,%2" : "subi %0,lo8(%n2)";
    }
  else
    gcc_unreachable();

  for (int i = 0; i < times; ++i)
    nxu16_asm_len (tpl, xop, plen, 1);

  return "";
}


/* Used in the "length" attribute of insn "*op8.for.cczn.<code>".  */

int
nxu16_len_op8_set_ZN (rtx_code code, rtx *xop)
{
  int len;
  (void) nxu16_out_op8_set_ZN (code, xop, &len);

  return len;
}


/* Output bit operation (IOR, AND, XOR) with register XOP[0] and compile
   time constant XOP[2]:

      XOP[0] = XOP[0] <op> XOP[2]

   and return "".  If PLEN == NULL, print assembler instructions to perform the
   operation; otherwise, set *PLEN to the length of the instruction sequence
   (in words) printed with PLEN == NULL.  XOP[3] is either an 8-bit clobber
   register or SCRATCH if no clobber register is needed for the operation.
   INSN is an INSN_P or a pattern of an insn.  */

const char *
nxu16_out_bitop (rtx insn, rtx *xop, int *plen)
{
  /* CODE and MODE of the operation.  */
  rtx xpattern = INSN_P (insn) ? single_set (as_a <rtx_insn *> (insn)) : insn;
  rtx_code code = GET_CODE (SET_SRC (xpattern));
  machine_mode mode = GET_MODE (xop[0]);

  /* Number of bytes to operate on.  */
  int n_bytes = GET_MODE_SIZE (mode);

  /* Value of T-flag (0 or 1) or -1 if unknow.  */
  int set_t = -1;

  /* Value (0..0xff) held in clobber register op[3] or -1 if unknown.  */
  int clobber_val = -1;

  /* op[0]: 8-bit destination register
     op[1]: 8-bit const int
     op[2]: 8-bit clobber register, SCRATCH or NULL_RTX.
     op[3]: 8-bit register containing 0xff or NULL_RTX  */
  rtx op[4];

  op[2] = QImode == mode ? NULL_RTX : xop[3];
  op[3] = NULL_RTX;

  if (plen)
    *plen = 0;

  for (int i = 0; i < n_bytes; i++)
    {
      /* We operate byte-wise on the destination.  */
      rtx reg8 = nxu16_byte (xop[0], i);

      /* 8-bit value to operate with this byte. */
      unsigned int val8 = nxu16_uint8 (xop[2], i);

      /* Number of bits set in the current byte of the constant.  */
      int pop8 = popcount_hwi (val8);

      /* Registers R16..R31 can operate with immediate.  */
      bool ld_reg_p = test_hard_reg_class (LD_REGS, reg8);

      op[0] = reg8;
      op[1] = GEN_INT (val8);

      switch (code)
	{
	case IOR:

	  if (pop8 == 0)
	    continue;
	  else if (ld_reg_p)
	    nxu16_asm_len ("ori %0,%1", op, plen, 1);
	  else if (pop8 == 1)
	    {
	      if (set_t != 1)
		nxu16_asm_len ("set", op, plen, 1);
	      set_t = 1;

	      op[1] = GEN_INT (exact_log2 (val8));
	      nxu16_asm_len ("bld %0,%1", op, plen, 1);
	    }
	  else if (pop8 == 8)
	    {
	      if (op[3] != NULL_RTX)
		nxu16_asm_len ("mov %0,%3", op, plen, 1);
	      else
		nxu16_asm_len ("clr %0" CR_TAB
			     "dec %0", op, plen, 2);

	      op[3] = op[0];
	    }
	  else
	    {
	      if (clobber_val != (int) val8)
		nxu16_asm_len ("ldi %2,%1", op, plen, 1);
	      clobber_val = (int) val8;

	      nxu16_asm_len ("or %0,%2", op, plen, 1);
	    }

	  continue; /* IOR */

	case AND:

	  if (pop8 == 8)
	    continue;
	  else if (pop8 == 0)
	    nxu16_asm_len ("clr %0", op, plen, 1);
	  else if (ld_reg_p)
	    nxu16_asm_len ("andi %0,%1", op, plen, 1);
	  else if (pop8 == 7)
	    {
	      if (set_t != 0)
		nxu16_asm_len ("clt", op, plen, 1);
	      set_t = 0;

	      op[1] = GEN_INT (exact_log2 (GET_MODE_MASK (QImode) & ~val8));
	      nxu16_asm_len ("bld %0,%1", op, plen, 1);
	    }
	  else
	    {
	      if (clobber_val != (int) val8)
		nxu16_asm_len ("ldi %2,%1", op, plen, 1);
	      clobber_val = (int) val8;

	      nxu16_asm_len ("and %0,%2", op, plen, 1);
	    }

	  continue; /* AND */

	case XOR:

	  if (pop8 == 0)
	    continue;
	  else if (pop8 == 8)
	    nxu16_asm_len ("com %0", op, plen, 1);
	  else if (ld_reg_p && val8 == (1 << 7))
	    nxu16_asm_len ("subi %0,%1", op, plen, 1);
	  else
	    {
	      if (clobber_val != (int) val8)
		nxu16_asm_len ("ldi %2,%1", op, plen, 1);
	      clobber_val = (int) val8;

	      nxu16_asm_len ("eor %0,%2", op, plen, 1);
	    }

	  continue; /* XOR */

	default:
	  /* Unknown rtx_code */
	  gcc_unreachable();
	}
    } /* for all sub-bytes */

  return "";
}


/* Emit code for

       XOP[0] = XOP[0] <xior> (XOP[1] <shift> BITOFF)

   where XOP[0] and XOP[1] are hard registers with integer mode,
   <xior> is XOR or IOR, and <shift> is LSHIFTRT or ASHIFT with a
   non-negative shift offset BITOFF.  This function emits the operation
   in terms of byte-wise operations in QImode.  */

void
nxu16_emit_xior_with_shift (rtx_insn *insn, rtx *xop, int bitoff)
{
  rtx src = SET_SRC (single_set (insn));
  rtx_code xior = GET_CODE (src);
  gcc_assert (xior == XOR || xior == IOR);
  gcc_assert (bitoff % 8 == 0);

  // Work out the shift offset in bytes; negative for shift right.
  rtx_code shift = GET_CODE (XEXP (src, 0));
  int byteoff = 0?0
    : shift == ASHIFT ? bitoff / 8
    : shift == LSHIFTRT ? -bitoff / 8
    // Not a shift but something like REG or ZERO_EXTEND:
    // Use xop[1] as is, without shifting it.
    : 0;

  // Work out which hard REGNOs belong to the operands.
  int size0 = GET_MODE_SIZE (GET_MODE (xop[0]));
  int size1 = GET_MODE_SIZE (GET_MODE (xop[1]));
  int regno0_lo = REGNO (xop[0]), regno0_hi = regno0_lo + size0 - 1;
  int regno1_lo = REGNO (xop[1]), regno1_hi = regno1_lo + size1 - 1;
  int regoff = regno0_lo - regno1_lo + byteoff;

  // The order of insns matters in the rare case when xop[1] overlaps xop[0].
  int beg = regoff > 0 ? regno1_hi : regno1_lo;
  int end = regoff > 0 ? regno1_lo : regno1_hi;
  int inc = regoff > 0 ? -1 : 1;

  rtx (*gen)(rtx,rtx,rtx) = xior == XOR ? gen_xorqi3 : gen_iorqi3;

  for (int i = beg; i != end + inc; i += inc)
    {
      if (IN_RANGE (i + regoff, regno0_lo, regno0_hi))
	{
	  rtx reg0 = all_regs_rtx[i + regoff];
	  rtx reg1 = all_regs_rtx[i];
	  emit_insn (gen (reg0, reg0, reg1));
	}
    }
}


/* Output sign extension from XOP[1] to XOP[0] and return "".
   If PLEN == NULL, print assembler instructions to perform the operation;
   otherwise, set *PLEN to the length of the instruction sequence (in words)
   as printed with PLEN == NULL.  */

const char *
nxu16_out_sign_extend (rtx_insn *insn, rtx *xop, int *plen)
{
  // Size in bytes of source resp. destination operand.
  unsigned n_src = GET_MODE_SIZE (GET_MODE (xop[1]));
  unsigned n_dest = GET_MODE_SIZE (GET_MODE (xop[0]));
  rtx r_msb = all_regs_rtx[REGNO (xop[1]) + n_src - 1];

  if (plen)
    *plen = 0;

  // Copy destination to source

  if (REGNO (xop[0]) != REGNO (xop[1]))
    {
      gcc_assert (n_src <= 2);

      if (n_src == 2)
	nxu16_asm_len (nxu16_HAVE_MOVW
		     ? "movw %0,%1"
		     : "mov %B0,%B1", xop, plen, 1);
      if (n_src == 1 || !nxu16_HAVE_MOVW)
	nxu16_asm_len ("mov %A0,%A1", xop, plen, 1);
    }

  // Set Carry to the sign bit MSB.7...

  if (REGNO (xop[0]) == REGNO (xop[1])
      || !reg_unused_after (insn, r_msb))
    {
      nxu16_asm_len ("mov __tmp_reg__,%0", &r_msb, plen, 1);
      r_msb = tmp_reg_rtx;
    }

  nxu16_asm_len ("lsl %0", &r_msb, plen, 1);

  // ...and propagate it to all the new sign bits

  for (unsigned n = n_src; n < n_dest; n++)
    nxu16_asm_len ("sbc %0,%0", &all_regs_rtx[REGNO (xop[0]) + n], plen, 1);

  return "";
}


/* PLEN == NULL: Output code to add CONST_INT OP[0] to SP.
   PLEN != NULL: Set *PLEN to the length of that sequence.
   Return "".  */

const char *
nxu16_out_addto_sp (rtx *op, int *plen)
{
  int pc_len = nxu16_2_BYTE_PC ? 2 : 3;
  int addend = INTVAL (op[0]);

  if (plen)
    *plen = 0;

  if (addend < 0)
    {
      if (flag_verbose_asm || flag_print_asm_name)
	nxu16_asm_len (ASM_COMMENT_START "SP -= %n0", op, plen, 0);

      while (addend <= -pc_len)
	{
	  addend += pc_len;
	  nxu16_asm_len ("rcall .", op, plen, 1);
	}

      while (addend++ < 0)
	nxu16_asm_len ("push __tmp_reg__", op, plen, 1);
    }
  else if (addend > 0)
    {
      if (flag_verbose_asm || flag_print_asm_name)
	nxu16_asm_len (ASM_COMMENT_START "SP += %0", op, plen, 0);

      while (addend-- > 0)
	nxu16_asm_len ("pop __tmp_reg__", op, plen, 1);
    }

  return "";
}


/* Output instructions to insert an inverted bit into OP[0]: $0.$1 = ~$2.$3.
   If PLEN = NULL then output the respective instruction sequence which
   is a combination of BST / BLD and some instruction(s) to invert the bit.
   If PLEN != NULL then store the length of the sequence (in words) in *PLEN.
   Return "".  */

const char *
nxu16_out_insert_notbit (rtx_insn *insn, rtx op[], int *plen)
{
  if (INTVAL (op[1]) == 7
      && test_hard_reg_class (LD_REGS, op[0]))
    {
      /* If the inserted bit number is 7 and we have a d-reg, then invert
	 the bit after the insertion by means of SUBI *,0x80.  */

      if (INTVAL (op[3]) == 7
	  && REGNO (op[0]) == REGNO (op[2]))
	{
	  nxu16_asm_len ("subi %0,0x80", op, plen, -1);
	}
      else
	{
	  nxu16_asm_len ("bst %2,%3" CR_TAB
		       "bld %0,%1" CR_TAB
		       "subi %0,0x80", op, plen, -3);
	}
    }
  else if (test_hard_reg_class (LD_REGS, op[0])
	   && (INTVAL (op[1]) != INTVAL (op[3])
	       || !reg_overlap_mentioned_p (op[0], op[2])))
    {
      /* If the destination bit is in a d-reg we can jump depending
	 on the source bit and use ANDI / ORI.  This just applies if we
	 have not an early-clobber situation with the bit.  */

      nxu16_asm_len ("andi %0,~(1<<%1)" CR_TAB
		   "sbrs %2,%3"       CR_TAB
		   "ori %0,1<<%1", op, plen, -3);
    }
  else
    {
      /* Otherwise, invert the bit by means of COM before we store it with
	 BST and then undo the COM if needed.  */

      nxu16_asm_len ("com %2" CR_TAB
		   "bst %2,%3", op, plen, -2);

      if (!reg_unused_after (insn, op[2])
	  // A simple 'reg_unused_after' is not enough because that function
	  // assumes that the destination register is overwritten completely
	  // and hence is in order for our purpose.  This is not the case
	  // with BLD which just changes one bit of the destination.
	  || reg_overlap_mentioned_p (op[0], op[2]))
	{
	  /* Undo the COM from above.  */
	  nxu16_asm_len ("com %2", op, plen, 1);
	}

      nxu16_asm_len ("bld %0,%1", op, plen, 1);
    }

  return "";
}


/* Output instructions for  XOP[0] = (XOP[1] <Shift> XOP[2]) & XOP[3]  where
   -  XOP[0] and XOP[1] have the same mode which is one of: QI, HI, PSI, SI.
   -  XOP[3] is an exact const_int power of 2.
   -  XOP[2] and XOP[3] are const_int.
   -  <Shift> is any of: ASHIFT, LSHIFTRT, ASHIFTRT.
   -  The result depends on XOP[1].
   or  XOP[0] = XOP[1] & XOP[2]  where
   -  XOP[0] and XOP[1] have the same mode which is one of: HI, PSI, SI.
   -  XOP[2] is an exact const_int power of 2.
   Returns "".
   PLEN != 0: Set *PLEN to the code length in words.  Don't output anything.
   PLEN == 0: Output instructions.  */

const char*
nxu16_out_insv (rtx_insn *insn, rtx xop[], int *plen)
{
  machine_mode mode = GET_MODE (xop[0]);
  int n_bytes = GET_MODE_SIZE (mode);
  rtx xsrc = SET_SRC (single_set (insn));

  gcc_assert (AND == GET_CODE (xsrc));

  rtx xop2 = xop[2];
  rtx xop3 = xop[3];

  if (REG_P (XEXP (xsrc, 0)))
    {
      // This function can also handle AND with an exact power of 2,
      // which can be regarded as a XOP[1] shift with offset 0.
      rtx xshift = gen_rtx_ASHIFT (mode, xop[1], const0_rtx);
      xsrc = gen_rtx_AND (mode, xshift, xop[2]);
      xop3 = xop[2];
      xop2 = const0_rtx;
    }

  // Any of ASHIFT, LSHIFTRT, ASHIFTRT.
  rtx_code code = GET_CODE (XEXP (xsrc, 0));
  int shift = code == ASHIFT ? INTVAL (xop2) : -INTVAL (xop2);

  // Determines the position of the output bit.
  unsigned mask = GET_MODE_MASK (mode) & INTVAL (xop3);

  // Position of the output / input bit, respectively.
  int obit = exact_log2 (mask);
  int ibit = obit - shift;

  gcc_assert (IN_RANGE (obit, 0, GET_MODE_BITSIZE (mode) - 1));
  gcc_assert (IN_RANGE (ibit, 0, GET_MODE_BITSIZE (mode) - 1));

  // In the remainder, use the sub-bytes that hold the bits.
  rtx op[4] =
    {
      // Output
      nxu16_byte (xop[0], obit / 8), GEN_INT (obit & 7),
      // Input
      nxu16_byte (xop[1], ibit / 8), GEN_INT (ibit & 7)
    };
  obit &= 7;
  ibit &= 7;

  // The length of the default sequence at the end of this function.
  // We only emit anything other than the default when we find a sequence
  // that is strictly shorter than the default sequence; which is:
  // BST + <CLR-result-bytes> + BLD.
  const int len0 = 2 + n_bytes - (n_bytes == 4 && nxu16_HAVE_MOVW);

  // Finding something shorter than the default sequence implies that there
  // must be at most 2 instructions that deal with the bytes containing the
  // relevant bits.  In addition, we need  N_BYTES - 1  instructions to clear
  // the remaining result bytes.

  const int n_clr = n_bytes - 1;
  bool clr_p = false;
  bool andi_p = false;

  if (plen)
    *plen = 0;

  if (REGNO (op[0]) == REGNO (op[2])
      // Output reg allows ANDI.
      && test_hard_reg_class (LD_REGS, op[0]))
    {
      if (1 + n_clr < len0
	  // Same byte and bit: A single ANDI will do.
	  && obit == ibit)
	{
	  clr_p = andi_p = true;
	}
      else if (2 + n_clr < len0
	       // |obit - ibit| = 4:  SWAP + ANDI will do.
	       && (obit == ibit + 4 || obit == ibit - 4))
	{
	  nxu16_asm_len ("swap %0", op, plen, 1);
	  clr_p = andi_p = true;
	}
      else if (2 + n_clr < len0
	       // LSL + ANDI will do.
	       && obit == ibit + 1)
	{
	  nxu16_asm_len ("lsl %0", op, plen, 1);
	  clr_p = andi_p = true;
	}
      else if (2 + n_clr < len0
	       // LSR + ANDI will do.
	       && obit == ibit - 1)
	{
	  nxu16_asm_len ("lsr %0", op, plen, 1);
	  clr_p = andi_p = true;
	}
    }

  if (REGNO (op[0]) != REGNO (op[2])
      && obit == ibit)
    {
      if (2 + n_clr < len0
	  // Same bit but different byte: MOV + ANDI will do.
	  && test_hard_reg_class (LD_REGS, op[0]))
	{
	  nxu16_asm_len ("mov %0,%2", op, plen, 1);
	  clr_p = andi_p = true;
	}
      else if (2 + n_clr < len0
	       // Same bit but different byte:  We can use ANDI + MOV,
	       // but only if the input byte is LD_REGS and unused after.
	       && test_hard_reg_class (LD_REGS, op[2])
	       && reg_unused_after (insn, op[2]))
	{
	  nxu16_asm_len ("andi %2,1<<%3"  CR_TAB
		       "mov %0,%2", op, plen, 2);
	  clr_p = true;
	}
    }

  // Output remaining instructions of the shorter sequence.

  if (andi_p)
    nxu16_asm_len ("andi %0,1<<%1", op, plen, 1);

  if (clr_p)
    {
      for (int b = 0; b < n_bytes; ++b)
	{
	  rtx byte = nxu16_byte (xop[0], b);
	  if (REGNO (byte) != REGNO (op[0]))
	    nxu16_asm_len ("clr %0", &byte, plen, 1);
	}

      // CLR_P means we found a shorter sequence, so we are done now.
      return "";
    }

  // No shorter sequence found, just emit  BST, CLR*, BLD  sequence.

  nxu16_asm_len ("bst %2,%3", op, plen, -1);

  if (n_bytes == 4 && nxu16_HAVE_MOVW)
    nxu16_asm_len ("clr %A0"   CR_TAB
		 "clr %B0"   CR_TAB
		 "movw %C0,%A0", xop, plen, 3);
  else
    for (int b = 0; b < n_bytes; ++b)
      {
	rtx byte = nxu16_byte (xop[0], b);
	nxu16_asm_len ("clr %0", &byte, plen, 1);
      }

  return nxu16_asm_len ("bld %0,%1", op, plen, 1);
}


/* Output instructions to extract a bit to 8-bit register XOP[0].
   The input XOP[1] is a register or an 8-bit MEM in the lower I/O range.
   XOP[2] is the const_int bit position.  Return "".

   PLEN != 0: Set *PLEN to the code length in words.  Don't output anything.
   PLEN == 0: Output instructions.  */

const char *
nxu16_out_extr (rtx_insn *insn, rtx xop[], int *plen)
{
  rtx dest = xop[0];
  rtx src = xop[1];
  int bit = INTVAL (xop[2]);

  if (GET_MODE (src) != QImode)
    {
      src = xop[1] = nxu16_byte (src, bit / 8);
      bit %= 8;
      xop[2] = GEN_INT (bit);
    }

  if (MEM_P (src))
    {
      xop[1] = XEXP (src, 0); // address
      //gcc_assert (low_io_address_operand (xop[1], Pmode));

      return nxu16_asm_len ("clr %0"      CR_TAB
			  "sbic %i1,%2" CR_TAB
			  "inc %0", xop, plen, -3);
    }

  gcc_assert (REG_P (src));

  bool ld_dest_p = test_hard_reg_class (LD_REGS, dest);
  bool ld_src_p = test_hard_reg_class (LD_REGS, src);

  if (ld_dest_p
      && REGNO (src) == REGNO (dest))
    {
      if (bit == 0)
	return nxu16_asm_len ("andi %0,1", xop, plen, -1);
      if (bit == 1)
	return nxu16_asm_len ("lsr %0" CR_TAB
			    "andi %0,1", xop, plen, -2);
      if (bit == 4)
	return nxu16_asm_len ("swap %0" CR_TAB
			    "andi %0,1", xop, plen, -2);
    }

  if (bit == 0
      && REGNO (src) != REGNO (dest))
  {
    if (ld_dest_p)
      return nxu16_asm_len ("mov %0,%1" CR_TAB
			  "andi %0,1", xop, plen, -2);
    if (ld_src_p
	&& reg_unused_after (insn, src))
      return nxu16_asm_len ("andi %1,1" CR_TAB
			  "mov %0,%1", xop, plen, -2);
  }

  return nxu16_asm_len ("bst %1,%2" CR_TAB
		      "clr %0"    CR_TAB
		      "bld %0,0", xop, plen, -3);
}


/* Output instructions to extract a negated bit to 8-bit register XOP[0].
   The input XOP[1] is an 8-bit register or MEM in the lower I/O range.
   XOP[2] is the const_int bit position.  Return "".

   PLEN != 0: Set *PLEN to the code length in words.  Don't output anything.
   PLEN == 0: Output instructions.  */

const char *
nxu16_out_extr_not (rtx_insn * /* insn */, rtx xop[], int *plen)
{
  rtx dest = xop[0];
  rtx src = xop[1];
  int bit = INTVAL (xop[2]);

  if (MEM_P (src))
    {
      xop[1] = XEXP (src, 0); // address
      gcc_assert (low_io_address_operand (xop[1], Pmode));

      return nxu16_asm_len ("clr %0"      CR_TAB
			  "sbis %i1,%2" CR_TAB
			  "inc %0", xop, plen, -3);
    }

  gcc_assert (REG_P (src));

  bool ld_src_p = test_hard_reg_class (LD_REGS, src);

  if (ld_src_p
      && REGNO (src) == REGNO (dest))
    {
      if (bit == 0)
	return nxu16_asm_len ("inc %0" CR_TAB
			    "andi %0,1", xop, plen, -2);
      if (bit == 1)
	return nxu16_asm_len ("lsr %0" CR_TAB
			    "inc %0" CR_TAB
			    "andi %0,1", xop, plen, -3);
      if (bit == 4)
	return nxu16_asm_len ("swap %0" CR_TAB
			    "inc %0"  CR_TAB
			    "andi %0,1", xop, plen, -3);
    }

  if (bit == 7
      && ld_src_p)
    return nxu16_asm_len ("cpi %1,0x80" CR_TAB
			"sbc %0,%0"   CR_TAB
			"neg %0", xop, plen, -3);

  if (REGNO (src) != REGNO (dest))
    return nxu16_asm_len ("clr %0"     CR_TAB
			"sbrs %1,%2" CR_TAB
			"inc %0", xop, plen, -3);

  return nxu16_asm_len ("clr __tmp_reg__" CR_TAB
		      "sbrs %1,%2"      CR_TAB
		      "inc __tmp_reg__" CR_TAB
		      "mov %0,__tmp_reg__", xop, plen, -4);
}


/* Outputs instructions needed for fixed point type conversion.
   This includes converting between any fixed point type, as well
   as converting to any integer type.  Conversion between integer
   types is not supported.

   Converting signed fractional types requires a bit shift if converting
   to or from any unsigned fractional type because the decimal place is
   shifted by 1 bit.  When the destination is a signed fractional, the sign
   is stored in either the carry or T bit.  */

const char *
nxu16_out_fract (rtx_insn *insn, rtx operands[], bool intsigned, int *plen)
{
  rtx xop[6];
  rtx_code shift = UNKNOWN;
  bool sign_in_carry = false;
  bool msb_in_carry = false;
  bool lsb_in_tmp_reg = false;
  bool lsb_in_carry = false;
  bool frac_rounded = false;
  const char *code_ashift = "lsl %0";


#define MAY_CLOBBER(RR)                                                 \
  /* Shorthand used below.  */                                          \
  ((sign_bytes                                                          \
    && IN_RANGE (RR, dest.regno_msb - sign_bytes + 1, dest.regno_msb))  \
   || (offset && IN_RANGE (RR, dest.regno, dest.regno_msb))		\
   || (reg_unused_after (insn, all_regs_rtx[RR])                        \
       && !IN_RANGE (RR, dest.regno, dest.regno_msb)))

  struct
  {
    /* bytes       : Length of operand in bytes.
       ibyte       : Length of integral part in bytes.
       fbyte, fbit : Length of fractional part in bytes, bits.  */

    bool sbit;
    unsigned fbit, bytes, ibyte, fbyte;
    unsigned regno, regno_msb;
  } dest, src, *val[2] = { &dest, &src };

  if (plen)
    *plen = 0;

  /* Step 0:  Determine information on source and destination operand we
     ======   will need in the remainder.  */

  for (size_t i = 0; i < ARRAY_SIZE (val); i++)
    {
      machine_mode mode;

      xop[i] = operands[i];

      mode = GET_MODE (xop[i]);

      val[i]->bytes = GET_MODE_SIZE (mode);
      val[i]->regno = REGNO (xop[i]);
      val[i]->regno_msb = REGNO (xop[i]) + val[i]->bytes - 1;

      if (SCALAR_INT_MODE_P (mode))
	{
	  val[i]->sbit = intsigned;
	  val[i]->fbit = 0;
	}
      else if (ALL_SCALAR_FIXED_POINT_MODE_P (mode))
	{
	  val[i]->sbit = SIGNED_SCALAR_FIXED_POINT_MODE_P (mode);
	  val[i]->fbit = GET_MODE_FBIT (mode);
	}
      else
	fatal_insn ("unsupported fixed-point conversion", insn);

      val[i]->fbyte = (1 + val[i]->fbit) / BITS_PER_UNIT;
      val[i]->ibyte = val[i]->bytes - val[i]->fbyte;
    }

  // Byte offset of the decimal point taking into account different place
  // of the decimal point in input and output and different register numbers
  // of input and output.
  int offset = dest.regno - src.regno + dest.fbyte - src.fbyte;

  // Number of destination bytes that will come from sign / zero extension.
  int sign_bytes = (dest.ibyte - src.ibyte) * (dest.ibyte > src.ibyte);

  // Number of bytes at the low end to be filled with zeros.
  int zero_bytes = (dest.fbyte - src.fbyte) * (dest.fbyte > src.fbyte);

  // Do we have a 16-Bit register that is cleared?
  rtx clrw = NULL_RTX;

  bool sign_extend = src.sbit && sign_bytes;

  if (dest.fbit % 8 == 0 && src.fbit % 8 == 7)
    shift = ASHIFT;
  else if (dest.fbit % 8 == 7 && src.fbit % 8 == 0)
    shift = ASHIFTRT;
  else if (dest.fbit % 8 == src.fbit % 8)
    shift = UNKNOWN;
  else
    gcc_unreachable();

  /* If we need to round the fraction part, we might need to save/round it
     before clobbering any of it in Step 1.  Also, we might want to do
     the rounding now to make use of LD_REGS.  */
  if (SCALAR_INT_MODE_P (GET_MODE (xop[0]))
      && SCALAR_ACCUM_MODE_P (GET_MODE (xop[1]))
      && !TARGET_FRACT_CONV_TRUNC)
    {
      bool overlap
	= (src.regno <=
	   (offset ? dest.regno_msb - sign_bytes : dest.regno + zero_bytes - 1)
	   && dest.regno - offset -1 >= dest.regno);
      unsigned s0 = dest.regno - offset -1;
      bool use_src = true;
      unsigned sn;
      unsigned copied_msb = src.regno_msb;
      bool have_carry = false;

      if (src.ibyte > dest.ibyte)
	copied_msb -= src.ibyte - dest.ibyte;

      for (sn = s0; sn <= copied_msb; sn++)
	if (!IN_RANGE (sn, dest.regno, dest.regno_msb)
	    && !reg_unused_after (insn, all_regs_rtx[sn]))
	  use_src = false;
      if (use_src && TEST_HARD_REG_BIT (reg_class_contents[LD_REGS], s0))
	{
	  nxu16_asm_len ("tst %0" CR_TAB "brpl 0f",
		       &all_regs_rtx[src.regno_msb], plen, 2);
	  sn = src.regno;
	  if (sn < s0)
	    {
	      if (TEST_HARD_REG_BIT (reg_class_contents[LD_REGS], sn))
		nxu16_asm_len ("cpi %0,1", &all_regs_rtx[sn], plen, 1);
	      else
		nxu16_asm_len ("sec" CR_TAB
			     "cpc %0,__zero_reg__",
			     &all_regs_rtx[sn], plen, 2);
	      have_carry = true;
	    }
	  while (++sn < s0)
	    nxu16_asm_len ("cpc %0,__zero_reg__", &all_regs_rtx[sn], plen, 1);

	  nxu16_asm_len (have_carry ? "sbci %0,128" : "subi %0,129",
		       &all_regs_rtx[s0], plen, 1);
	  for (sn = src.regno + src.fbyte; sn <= copied_msb; sn++)
	    nxu16_asm_len ("sbci %0,255", &all_regs_rtx[sn], plen, 1);
	  nxu16_asm_len ("\n0:", NULL, plen, 0);
	  frac_rounded = true;
	}
      else if (use_src && overlap)
	{
	  nxu16_asm_len ("clr __tmp_reg__" CR_TAB
		       "sbrc %1,0"       CR_TAB
		       "dec __tmp_reg__", xop, plen, 1);
	  sn = src.regno;
	  if (sn < s0)
	    {
	      nxu16_asm_len ("add %0,__tmp_reg__", &all_regs_rtx[sn], plen, 1);
	      have_carry = true;
	    }

	  while (++sn < s0)
	    nxu16_asm_len ("adc %0,__tmp_reg__", &all_regs_rtx[sn], plen, 1);

	  if (have_carry)
	    nxu16_asm_len ("clt"                CR_TAB
			 "bld __tmp_reg__,7"  CR_TAB
			 "adc %0,__tmp_reg__",
			 &all_regs_rtx[s0], plen, 1);
	  else
	    nxu16_asm_len ("lsr __tmp_reg" CR_TAB
			 "add %0,__tmp_reg__",
			 &all_regs_rtx[s0], plen, 2);
	  for (sn = src.regno + src.fbyte; sn <= copied_msb; sn++)
	    nxu16_asm_len ("adc %0,__zero_reg__", &all_regs_rtx[sn], plen, 1);
	  frac_rounded = true;
	}
      else if (overlap)
	{
	  bool use_src
	    = (TEST_HARD_REG_BIT (reg_class_contents[LD_REGS], s0)
	       && (IN_RANGE (s0, dest.regno, dest.regno_msb)
		   || reg_unused_after (insn, all_regs_rtx[s0])));
	  xop[2] = all_regs_rtx[s0];
	  unsigned sn = src.regno;
	  if (!use_src || sn == s0)
	    nxu16_asm_len ("mov __tmp_reg__,%2", xop, plen, 1);
	  /* We need to consider to-be-discarded bits
	     if the value is negative.  */
	  if (sn < s0)
	    {
	      nxu16_asm_len ("tst %0" CR_TAB
			   "brpl 0f",
			   &all_regs_rtx[src.regno_msb], plen, 2);
	      /* Test to-be-discarded bytes for any nozero bits.
		 ??? Could use OR or SBIW to test two registers at once.  */
	      if (sn < s0)
		nxu16_asm_len ("cp %0,__zero_reg__", &all_regs_rtx[sn], plen, 1);

	      while (++sn < s0)
		nxu16_asm_len ("cpc %0,__zero_reg__", &all_regs_rtx[sn], plen, 1);
	      /* Set bit 0 in __tmp_reg__ if any of the lower bits was set.  */
	      if (use_src)
		nxu16_asm_len ("breq 0f" CR_TAB
			     "ori %2,1"
			     "\n0:\t" "mov __tmp_reg__,%2",
			     xop, plen, 3);
	      else
		nxu16_asm_len ("breq 0f" CR_TAB
			     "set"     CR_TAB
			     "bld __tmp_reg__,0\n0:",
			     xop, plen, 3);
	    }
	  lsb_in_tmp_reg = true;
	}
    }

  /* Step 1:  Clear bytes at the low end and copy payload bits from source
     ======   to destination.  */

  int step = offset < 0 ? 1 : -1;
  unsigned d0 = offset < 0 ? dest.regno : dest.regno_msb;

  // We cleared at least that number of registers.
  int clr_n = 0;

  for (; d0 >= dest.regno && d0 <= dest.regno_msb; d0 += step)
    {
      // Next regno of destination is needed for MOVW
      unsigned d1 = d0 + step;

      // Current and next regno of source
      signed s0 = d0 - offset;
      signed s1 = s0 + step;

      // Must current resp. next regno be CLRed?  This applies to the low
      // bytes of the destination that have no associated source bytes.
      bool clr0 = s0 < (signed) src.regno;
      bool clr1 = s1 < (signed) src.regno && d1 >= dest.regno;

      // First gather what code to emit (if any) and additional step to
      // apply if a MOVW is in use.  xop[2] is destination rtx and xop[3]
      // is the source rtx for the current loop iteration.
      const char *code = NULL;
      int stepw = 0;

      if (clr0)
	{
	  if (nxu16_HAVE_MOVW && clr1 && clrw)
	    {
	      xop[2] = all_regs_rtx[d0 & ~1];
	      xop[3] = clrw;
	      code = "movw %2,%3";
	      stepw = step;
	    }
	  else
	    {
	      xop[2] = all_regs_rtx[d0];
	      code = "clr %2";

	      if (++clr_n >= 2
		  && !clrw
		  && d0 % 2 == (step > 0))
		{
		  clrw = all_regs_rtx[d0 & ~1];
		}
	    }
	}
      else if (offset && s0 <= (signed) src.regno_msb)
	{
	  int movw = nxu16_HAVE_MOVW && offset % 2 == 0
	    && d0 % 2 == (offset > 0)
	    && d1 <= dest.regno_msb && d1 >= dest.regno
	    && s1 <= (signed) src.regno_msb  && s1 >= (signed) src.regno;

	  xop[2] = all_regs_rtx[d0 & ~movw];
	  xop[3] = all_regs_rtx[s0 & ~movw];
	  code = movw ? "movw %2,%3" : "mov %2,%3";
	  stepw = step * movw;
	}

      if (code)
	{
	  if (sign_extend && shift != ASHIFT && !sign_in_carry
	      && (d0 == src.regno_msb || d0 + stepw == src.regno_msb))
	    {
	      /* We are going to override the sign bit.  If we sign-extend,
		 store the sign in the Carry flag.  This is not needed if
		 the destination will be ASHIFT in the remainder because
		 the ASHIFT will set Carry without extra instruction.  */

	      nxu16_asm_len ("lsl %0", &all_regs_rtx[src.regno_msb], plen, 1);
	      sign_in_carry = true;
	    }

	  unsigned src_msb = dest.regno_msb - sign_bytes - offset + 1;

	  if (!sign_extend && shift == ASHIFTRT && !msb_in_carry
	      && src.ibyte > dest.ibyte
	      && (d0 == src_msb || d0 + stepw == src_msb))
	    {
	      /* We are going to override the MSB.  If we shift right,
		 store the MSB in the Carry flag.  This is only needed if
		 we don't sign-extend becaue with sign-extension the MSB
		 (the sign) will be produced by the sign extension.  */

	      nxu16_asm_len ("lsr %0", &all_regs_rtx[src_msb], plen, 1);
	      msb_in_carry = true;
	    }

	  unsigned src_lsb = dest.regno - offset -1;

	  if (shift == ASHIFT && src.fbyte > dest.fbyte && !lsb_in_carry
	      && !lsb_in_tmp_reg
	      && (d0 == src_lsb || d0 + stepw == src_lsb))
	    {
	      /* We are going to override the new LSB; store it into carry.  */

	      nxu16_asm_len ("lsl %0", &all_regs_rtx[src_lsb], plen, 1);
	      code_ashift = "rol %0";
	      lsb_in_carry = true;
	    }

	  nxu16_asm_len (code, xop, plen, 1);
	  d0 += stepw;
	}
    }

  /* Step 2:  Shift destination left by 1 bit position.  This might be needed
     ======   for signed input and unsigned output.  */

  if (shift == ASHIFT && src.fbyte > dest.fbyte && !lsb_in_carry)
    {
      unsigned s0 = dest.regno - offset -1;

      /* n1169 4.1.4 says:
	 "Conversions from a fixed-point to an integer type round toward zero."
	 Hence, converting a fract type to integer only gives a non-zero result
	 for -1.  */
      if (SCALAR_INT_MODE_P (GET_MODE (xop[0]))
	  && SCALAR_FRACT_MODE_P (GET_MODE (xop[1]))
	  && !TARGET_FRACT_CONV_TRUNC)
	{
	  gcc_assert (s0 == src.regno_msb);
	  /* Check if the input is -1.  We do that by checking if negating
	     the input causes an integer overflow.  */
	  unsigned sn = src.regno;
	  nxu16_asm_len ("cp __zero_reg__,%0", &all_regs_rtx[sn++], plen, 1);
	  while (sn <= s0)
	    nxu16_asm_len ("cpc __zero_reg__,%0", &all_regs_rtx[sn++], plen, 1);

	  /* Overflow goes with set carry.  Clear carry otherwise.  */
	  nxu16_asm_len ("brvs 0f" CR_TAB
		       "clc\n0:", NULL, plen, 2);
	}
      /* Likewise, when converting from accumulator types to integer, we
	 need to round up negative values.  */
      else if (SCALAR_INT_MODE_P (GET_MODE (xop[0]))
	       && SCALAR_ACCUM_MODE_P (GET_MODE (xop[1]))
	       && !TARGET_FRACT_CONV_TRUNC
	       && !frac_rounded)
	{
	  bool have_carry = false;

	  xop[2] = all_regs_rtx[s0];
	  if (!lsb_in_tmp_reg && !MAY_CLOBBER (s0))
	    nxu16_asm_len ("mov __tmp_reg__,%2", xop, plen, 1);
	  nxu16_asm_len ("tst %0" CR_TAB "brpl 0f",
		       &all_regs_rtx[src.regno_msb], plen, 2);
	  if (!lsb_in_tmp_reg)
	    {
	      unsigned sn = src.regno;
	      if (sn < s0)
		{
		  nxu16_asm_len ("cp __zero_reg__,%0", &all_regs_rtx[sn],
			       plen, 1);
		  have_carry = true;
		}
	      while (++sn < s0)
		nxu16_asm_len ("cpc __zero_reg__,%0", &all_regs_rtx[sn], plen, 1);
	      lsb_in_tmp_reg = !MAY_CLOBBER (s0);
	    }
	  /* Add in C and the rounding value 127.  */
	  /* If the destination msb is a sign byte, and in LD_REGS,
	     grab it as a temporary.  */
	  if (sign_bytes
	      && TEST_HARD_REG_BIT (reg_class_contents[LD_REGS],
				    dest.regno_msb))
	    {
	      xop[3] = all_regs_rtx[dest.regno_msb];
	      nxu16_asm_len ("ldi %3,127", xop, plen, 1);
	      nxu16_asm_len ((have_carry && lsb_in_tmp_reg ? "adc __tmp_reg__,%3"
			    : have_carry ? "adc %2,%3"
			    : lsb_in_tmp_reg ? "add __tmp_reg__,%3"
			    : "add %2,%3"),
			   xop, plen, 1);
	    }
	  else
	    {
	      /* Fall back to use __zero_reg__ as a temporary.  */
	      nxu16_asm_len ("dec __zero_reg__", NULL, plen, 1);
	      if (have_carry)
		nxu16_asm_len ("clt" CR_TAB
			     "bld __zero_reg__,7", NULL, plen, 2);
	      else
		nxu16_asm_len ("lsr __zero_reg__", NULL, plen, 1);
	      nxu16_asm_len (have_carry && lsb_in_tmp_reg
			   ? "adc __tmp_reg__,__zero_reg__"
			   : have_carry ? "adc %2,__zero_reg__"
			   : lsb_in_tmp_reg ? "add __tmp_reg__,__zero_reg__"
			   : "add %2,__zero_reg__",
			   xop, plen, 1);
	      nxu16_asm_len ("eor __zero_reg__,__zero_reg__", NULL, plen, 1);
	    }

	  for (d0 = dest.regno + zero_bytes;
	       d0 <= dest.regno_msb - sign_bytes; d0++)
	    nxu16_asm_len ("adc %0,__zero_reg__", &all_regs_rtx[d0], plen, 1);

	  nxu16_asm_len (lsb_in_tmp_reg
		       ? "\n0:\t" "lsl __tmp_reg__"
		       : "\n0:\t" "lsl %2",
		       xop, plen, 1);
	}
      else if (MAY_CLOBBER (s0))
	nxu16_asm_len ("lsl %0", &all_regs_rtx[s0], plen, 1);
      else
	nxu16_asm_len ("mov __tmp_reg__,%0" CR_TAB
		     "lsl __tmp_reg__", &all_regs_rtx[s0], plen, 2);

      code_ashift = "rol %0";
      lsb_in_carry = true;
    }

  if (shift == ASHIFT)
    {
      for (d0 = dest.regno + zero_bytes;
	   d0 <= dest.regno_msb - sign_bytes; d0++)
	{
	  nxu16_asm_len (code_ashift, &all_regs_rtx[d0], plen, 1);
	  code_ashift = "rol %0";
	}

      lsb_in_carry = false;
      sign_in_carry = true;
    }

  /* Step 4a:  Store MSB in carry if we don't already have it or will produce
     =======   it in sign-extension below.  */

  if (!sign_extend && shift == ASHIFTRT && !msb_in_carry
      && src.ibyte > dest.ibyte)
    {
      unsigned s0 = dest.regno_msb - sign_bytes - offset + 1;

      if (MAY_CLOBBER (s0))
	nxu16_asm_len ("lsr %0", &all_regs_rtx[s0], plen, 1);
      else
	nxu16_asm_len ("mov __tmp_reg__,%0" CR_TAB
		     "lsr __tmp_reg__", &all_regs_rtx[s0], plen, 2);

      msb_in_carry = true;
    }

  /* Step 3:  Sign-extend or zero-extend the destination as needed.
     ======   */

  if (sign_extend && !sign_in_carry)
    {
      unsigned s0 = src.regno_msb;

      if (MAY_CLOBBER (s0))
	nxu16_asm_len ("lsl %0", &all_regs_rtx[s0], plen, 1);
      else
	nxu16_asm_len ("mov __tmp_reg__,%0" CR_TAB
		     "lsl __tmp_reg__", &all_regs_rtx[s0], plen, 2);

      sign_in_carry = true;
    }

  gcc_assert (sign_in_carry + msb_in_carry + lsb_in_carry <= 1);

  unsigned copies = 0;
  rtx movw = sign_extend ? NULL_RTX : clrw;

  for (d0 = dest.regno_msb - sign_bytes + 1; d0 <= dest.regno_msb; d0++)
    {
      if (nxu16_HAVE_MOVW && movw
	  && d0 % 2 == 0 && d0 + 1 <= dest.regno_msb)
	{
	  xop[2] = all_regs_rtx[d0];
	  xop[3] = movw;
	  nxu16_asm_len ("movw %2,%3", xop, plen, 1);
	  d0++;
	}
      else
	{
	  nxu16_asm_len (sign_extend ? "sbc %0,%0" : "clr %0",
		       &all_regs_rtx[d0], plen, 1);

	  if (++copies >= 2 && !movw && d0 % 2 == 1)
	    movw = all_regs_rtx[d0-1];
	}
    } /* for */


  /* Step 4:  Right shift the destination.  This might be needed for
     ======   conversions from unsigned to signed.  */

  if (shift == ASHIFTRT)
    {
      const char *code_ashiftrt = "lsr %0";

      if (sign_extend || msb_in_carry)
	code_ashiftrt = "ror %0";

      if (src.sbit && src.ibyte == dest.ibyte)
	code_ashiftrt = "asr %0";

      for (d0 = dest.regno_msb - sign_bytes;
	   d0 >= dest.regno + zero_bytes - 1 && d0 >= dest.regno; d0--)
	{
	  nxu16_asm_len (code_ashiftrt, &all_regs_rtx[d0], plen, 1);
	  code_ashiftrt = "ror %0";
	}
    }

#undef MAY_CLOBBER

  return "";
}


/* Output fixed-point rounding.  XOP[0] = XOP[1] is the operand to round.
   XOP[2] is the rounding point, a CONST_INT.  The function prints the
   instruction sequence if PLEN = NULL and computes the length in words
   of the sequence if PLEN != NULL.  Most of this function deals with
   preparing operands for calls to `nxu16_out_plus' and `nxu16_out_bitop'.  */

const char *
nxu16_out_round (rtx_insn * /*insn*/, rtx *xop, int *plen)
{
  scalar_mode mode = as_a <scalar_mode> (GET_MODE (xop[0]));
  scalar_int_mode imode = int_mode_for_mode (mode).require ();
  // The smallest fractional bit not cleared by the rounding is 2^(-RP).
  int fbit = (int) GET_MODE_FBIT (mode);
  double_int i_add = double_int_zero.set_bit (fbit-1 - INTVAL (xop[2]));
  wide_int wi_add = wi::set_bit_in_zero (fbit-1 - INTVAL (xop[2]),
					 GET_MODE_PRECISION (imode));
  // Lengths of PLUS and AND parts.
  int len_add = 0, *plen_add = plen ? &len_add : NULL;
  int len_and = 0, *plen_and = plen ? &len_and : NULL;

  // Add-Saturate  1/2 * 2^(-RP).  Don't print the label "0:" when printing
  // the saturated addition so that we can emit the "rjmp 1f" before the
  // "0:" below.

  rtx xadd = const_fixed_from_double_int (i_add, mode);
  rtx xpattern, xsrc, op[4];

  xsrc = SIGNED_FIXED_POINT_MODE_P (mode)
    ? gen_rtx_SS_PLUS (mode, xop[1], xadd)
    : gen_rtx_US_PLUS (mode, xop[1], xadd);
  xpattern = gen_rtx_SET (xop[0], xsrc);

  op[0] = xop[0];
  op[1] = xop[1];
  op[2] = xadd;
  nxu16_out_plus (xpattern, op, plen_add, false /* Don't print "0:" */);

  nxu16_asm_len ("rjmp 1f" CR_TAB
	       "0:", NULL, plen_add, 1);

  // Keep  all bits from RP and higher:   ... 2^(-RP)
  // Clear all bits from RP+1 and lower:              2^(-RP-1) ...
  // Rounding point                           ^^^^^^^
  // Added above                                      ^^^^^^^^^
  rtx xreg = simplify_gen_subreg (imode, xop[0], mode, 0);
  rtx xmask = immed_wide_int_const (-wi_add - wi_add, imode);

  xpattern = gen_rtx_SET (xreg, gen_rtx_AND (imode, xreg, xmask));

  op[0] = xreg;
  op[1] = xreg;
  op[2] = xmask;
  op[3] = gen_rtx_SCRATCH (QImode);
  nxu16_out_bitop (xpattern, op, plen_and);
  nxu16_asm_len ("1:", NULL, plen, 0);

  if (plen)
    *plen = len_add + len_and;

  return "";
}


/* Create RTL split patterns for byte sized rotate expressions.  This
   produces a series of move instructions and considers overlap situations.
   Overlapping non-HImode operands need a scratch register.  */

bool
nxu16_rotate_bytes (rtx operands[])
{
  machine_mode mode = GET_MODE (operands[0]);
  bool overlapped = reg_overlap_mentioned_p (operands[0], operands[1]);
  bool same_reg = rtx_equal_p (operands[0], operands[1]);
  int num = INTVAL (operands[2]);
  rtx scratch = operands[3];
  /* Work out if byte or word move is needed.  Odd byte rotates need QImode.
     Word move if no scratch is needed, otherwise use size of scratch.  */
  machine_mode move_mode = QImode;

  if (num & 0xf)
    move_mode = QImode;
  else if ((mode == SImode && !same_reg) || !overlapped)
    move_mode = HImode;
  else
    move_mode = GET_MODE (scratch);

  /* Force DI rotate to use QI moves since other DI moves are currently split
     into QI moves so forward propagation works better.  */
  if (mode == DImode)
    move_mode = QImode;
  /* Make scratch smaller if needed.  */
  if (SCRATCH != GET_CODE (scratch)
      && HImode == GET_MODE (scratch)
      && QImode == move_mode)
    scratch = simplify_gen_subreg (move_mode, scratch, HImode, 0);

  int move_size = GET_MODE_SIZE (move_mode);
  /* Number of bytes/words to rotate.  */
  int offset = (num  >> 3) / move_size;
  /* Number of moves needed.  */
  int size = GET_MODE_SIZE (mode) / move_size;
  /* Himode byte swap is special case to avoid a scratch register.  */
  if (mode == HImode && same_reg)
    {
      /* HImode byte swap, using xor.  This is as quick as using scratch.  */
      rtx src, dst;
      src = simplify_gen_subreg (move_mode, operands[1], mode, 0);
      dst = simplify_gen_subreg (move_mode, operands[0], mode, 1);
      if (!rtx_equal_p (dst, src))
	{
	  emit_move_insn (dst, gen_rtx_XOR (QImode, dst, src));
	  emit_move_insn (src, gen_rtx_XOR (QImode, src, dst));
	  emit_move_insn (dst, gen_rtx_XOR (QImode, dst, src));
	}
    }
  else
    {
#define MAX_SIZE 8 /* GET_MODE_SIZE (DImode) / GET_MODE_SIZE (QImode)  */
      /* Create linked list of moves to determine move order.  */
      struct {
	rtx src, dst;
	int links;
      } move[MAX_SIZE + 8];
      int blocked, moves;

      gcc_assert (size <= MAX_SIZE);
      /* Generate list of subreg moves.  */
      for (int i = 0; i < size; i++)
	{
	  int from = i;
	  int to = (from + offset) % size;
	  move[i].src = simplify_gen_subreg (move_mode, operands[1],
					     mode, from * move_size);
	  move[i].dst = simplify_gen_subreg (move_mode, operands[0],
					     mode, to * move_size);
	  move[i].links = -1;
	}
      /* Mark dependence where a dst of one move is the src of another move.
	 The first move is a conflict as it must wait until second is
	 performed.  We ignore moves to self - we catch this later.  */
      if (overlapped)
	for (int i = 0; i < size; i++)
	  if (reg_overlap_mentioned_p (move[i].dst, operands[1]))
	    for (int j = 0; j < size; j++)
	      if (j != i && rtx_equal_p (move[j].src, move[i].dst))
		{
		  /* The dst of move i is the src of move j.  */
		  move[i].links = j;
		  break;
		}

      blocked = -1;
      moves = 0;
      /* Go through move list and perform non-conflicting moves.  As each
	 non-overlapping move is made, it may remove other conflicts
	 so the process is repeated until no conflicts remain.  */
      do
	{
	  blocked = -1;
	  moves = 0;
	  /* Emit move where dst is not also a src or we have used that
	     src already.  */
	  for (int i = 0; i < size; i++)
	    if (move[i].src != NULL_RTX)
	      {
		if (move[i].links == -1
		    || move[move[i].links].src == NULL_RTX)
		  {
		    moves++;
		    /* Ignore NOP moves to self.  */
		    if (!rtx_equal_p (move[i].dst, move[i].src))
		      emit_move_insn (move[i].dst, move[i].src);

		    /* Remove  conflict from list.  */
		    move[i].src = NULL_RTX;
		  }
		else
		  blocked = i;
	      }

	  /* Check for deadlock. This is when no moves occurred and we have
	     at least one blocked move.  */
	  if (moves == 0 && blocked != -1)
	    {
	      /* Need to use scratch register to break deadlock.
		 Add move to put dst of blocked move into scratch.
		 When this move occurs, it will break chain deadlock.
		 The scratch register is substituted for real move.  */

	      gcc_assert (SCRATCH != GET_CODE (scratch));

	      move[size].src = move[blocked].dst;
	      move[size].dst =  scratch;
	      /* Scratch move is never blocked.  */
	      move[size].links = -1;
	      /* Make sure we have valid link.  */
	      gcc_assert (move[blocked].links != -1);
	      /* Replace src of  blocking move with scratch reg.  */
	      move[move[blocked].links].src = scratch;
	      /* Make dependent on scratch move occurring.  */
	      move[blocked].links = size;
	      size=size+1;
	    }
	}
      while (blocked != -1);
    }
  return true;
}


/* Worker function for `ADJUST_INSN_LENGTH'.  */
/* Modifies the length assigned to instruction INSN
   LEN is the initially computed length of the insn.  */

int
nxu16_adjust_insn_length (rtx_insn *insn, int len)
{
  return len;
}


/* Implement `TARGET_ASM_INTEGER'.  */
/* Target hook for assembling integer objects.  The nxu16 version needs
   special handling for references to certain labels.  */

static bool
nxu16_assemble_integer (rtx x, unsigned int size, int aligned_p)
{
  if (size == POINTER_SIZE / BITS_PER_UNIT && aligned_p
      && text_segment_operand (x, VOIDmode))
    {
      fputs ("\t.word\tgs(", asm_out_file);
      output_addr_const (asm_out_file, x);
      fputs (")\n", asm_out_file);

      return true;
    }
  else if (GET_MODE (x) == PSImode)
    {
      /* This needs binutils 2.23+, see PR binutils/13503  */

      fputs ("\t.byte\tlo8(", asm_out_file);
      output_addr_const (asm_out_file, x);
      fputs (")" ASM_COMMENT_START "need binutils PR13503\n", asm_out_file);

      fputs ("\t.byte\thi8(", asm_out_file);
      output_addr_const (asm_out_file, x);
      fputs (")" ASM_COMMENT_START "need binutils PR13503\n", asm_out_file);

      fputs ("\t.byte\thh8(", asm_out_file);
      output_addr_const (asm_out_file, x);
      fputs (")" ASM_COMMENT_START "need binutils PR13503\n", asm_out_file);

      return true;
    }
  else if (CONST_FIXED_P (x))
    {
      /* varasm fails to handle big fixed modes that don't fit in hwi.  */

      for (unsigned n = 0; n < size; n++)
	default_assemble_integer (nxu16_byte (x, n), 1, aligned_p);

      return true;
    }

  if (nxu16_TINY
      && nxu16_address_tiny_pm_p (x))
    {
      x = plus_constant (Pmode, x, nxu16_arch->flash_pm_offset);
    }

  return default_assemble_integer (x, size, aligned_p);
}

/* Implement `TARGET_CLASS_MAX_NREGS'.  Reasons described in comments for
   nxu16_hard_regno_nregs. */

static unsigned char
nxu16_class_max_nregs (reg_class_t rclass, machine_mode mode)
{
  if (rclass == CC_REG && GET_MODE_CLASS (mode) == MODE_CC)
    return 1;

  return CEIL (GET_MODE_SIZE (mode), UNITS_PER_WORD);
}


/* Implement `TARGET_CLASS_LIKELY_SPILLED_P'.  */
/* Return value is nonzero if pseudos that have been
   assigned to registers of class CLASS would likely be spilled
   because registers of CLASS are needed for spill registers.  */

static bool
nxu16_class_likely_spilled_p (reg_class_t c)
{
  return (c != ALL_REGS
	  && (nxu16_TINY ? 1 : c != ADDW_REGS));
}


/* Valid attributes:
   progmem   -	Put data to program memory.
   signal    -	Make a function to be hardware interrupt.
		After function prologue interrupts remain disabled.
   interrupt -	Make a function to be hardware interrupt. Before function
		prologue interrupts are enabled by means of SEI.
   noblock   -	The function is an ISR that starts with a SEI instruction.
   naked     -	Don't generate function prologue/epilogue and RET
		instruction.  */

/* Handle a "progmem" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
nxu16_handle_progmem_attribute (tree *node, tree name, tree args,
			      int /*flags*/, bool *no_add_attrs)
{
  if (DECL_P (*node))
    {
      if (TREE_CODE (*node) == TYPE_DECL)
	{
	  /* This is really a decl attribute, not a type attribute,
	     but try to handle it for GCC 3.0 backwards compatibility.  */

	  tree type = TREE_TYPE (*node);
	  tree attr = tree_cons (name, args, TYPE_ATTRIBUTES (type));
	  tree newtype = build_type_attribute_variant (type, attr);

	  TYPE_MAIN_VARIANT (newtype) = TYPE_MAIN_VARIANT (type);
	  TREE_TYPE (*node) = newtype;
	  *no_add_attrs = true;
	}
      else if (TREE_STATIC (*node) || DECL_EXTERNAL (*node))
	{
	  *no_add_attrs = false;
	}
      else
	{
	  warning (OPT_Wattributes, "%qE attribute ignored",
		   name);
	  *no_add_attrs = true;
	}
    }

  return NULL_TREE;
}

/* Handle an attribute requiring a FUNCTION_DECL; arguments as in
   struct attribute_spec.handler.  */

static tree
nxu16_handle_fndecl_attribute (tree *node, tree name, tree /*args*/,
			     int /*flags*/, bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

static tree
nxu16_handle_fntype_attribute (tree *node, tree name, tree /*args*/,
			     int /*flags*/, bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_TYPE)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

static tree
nxu16_handle_absdata_attribute (tree *node, tree name, tree /* args */,
			      int /* flags */, bool *no_add)
{
  location_t loc = DECL_SOURCE_LOCATION (*node);

  if (nxu16_TINY)
    {
      if (TREE_CODE (*node) != VAR_DECL
	  || (!TREE_STATIC (*node) && !DECL_EXTERNAL (*node)))
	{
	  warning_at (loc, OPT_Wattributes, "%qE attribute only applies to"
		      " variables in static storage", name);
	  *no_add = true;
	}
    }
  else
    {
      warning_at (loc, OPT_Wattributes, "%qE attribute only supported"
		  " for reduced Tiny cores", name);
      *no_add = true;
    }

  return NULL_TREE;
}

static tree
nxu16_handle_addr_attribute (tree *node, tree name, tree args,
			   int /*flags*/, bool *no_add)
{
  bool io_p = startswith (IDENTIFIER_POINTER (name), "io");
  HOST_WIDE_INT io_start = nxu16_arch->sfr_offset;
  HOST_WIDE_INT io_end = strcmp (IDENTIFIER_POINTER (name), "io_low") == 0
    ? io_start + 0x1f
    : io_start + 0x3f;
  location_t loc = DECL_SOURCE_LOCATION (*node);

  if (!VAR_P (*node))
    {
      warning_at (loc, OPT_Wattributes, "%qE attribute only applies to "
		  "variables", name);
      *no_add = true;
      return NULL_TREE;
    }

  if (args != NULL_TREE)
    {
      if (TREE_CODE (TREE_VALUE (args)) == NON_LVALUE_EXPR)
	TREE_VALUE (args) = TREE_OPERAND (TREE_VALUE (args), 0);
      tree arg = TREE_VALUE (args);
      if (TREE_CODE (arg) != INTEGER_CST)
	{
	  warning_at (loc, OPT_Wattributes, "%qE attribute allows only an "
		      "integer constant argument", name);
	  *no_add = true;
	}
      else if (io_p
	       && (!tree_fits_shwi_p (arg)
		   || ! IN_RANGE (TREE_INT_CST_LOW (arg), io_start, io_end)))
	{
	  warning_at (loc, OPT_Wattributes, "%qE attribute address out of range"
		      " 0x%x%s0x%x", name, (int) io_start, "...", (int) io_end);
	  *no_add = true;
	}
      else
	{
	  tree attribs = DECL_ATTRIBUTES (*node);
	  const char *names[] = { "io", "io_low", "address", NULL };
	  for (const char **p = names; *p; p++)
	    {
	      tree other = lookup_attribute (*p, attribs);
	      if (other && TREE_VALUE (other))
		{
		  warning_at (loc, OPT_Wattributes,
			      "both %s and %qE attribute provide address",
			      *p, name);
		  *no_add = true;
		  break;
		}
	    }
	}
    }

  if (*no_add == false && io_p && !TREE_THIS_VOLATILE (*node))
    warning_at (loc, OPT_Wattributes, "%qE attribute on non-volatile variable",
		name);

  // Optimizers must not draw any conclusions from "static int addr;" etc.
  // because the contents of `addr' are not given by its initializer but
  // by the contents at the address as specified by the attribute.
  if (VAR_P (*node) && ! *no_add)
    TREE_THIS_VOLATILE (*node) = 1;

  return NULL_TREE;
}

rtx
nxu16_eval_addr_attrib (rtx x)
{
  if (SYMBOL_REF_P (x)
      && (SYMBOL_REF_FLAGS (x) & SYMBOL_FLAG_ADDRESS))
    {
      tree decl = SYMBOL_REF_DECL (x);
      tree attr = NULL_TREE;

      if (SYMBOL_REF_FLAGS (x) & SYMBOL_FLAG_IO)
	{
	  attr = lookup_attribute ("io", DECL_ATTRIBUTES (decl));
	  if (!attr || !TREE_VALUE (attr))
	    attr = lookup_attribute ("io_low", DECL_ATTRIBUTES (decl));
	}
      if (!attr || !TREE_VALUE (attr))
	attr = lookup_attribute ("address", DECL_ATTRIBUTES (decl));
      gcc_assert (attr && TREE_VALUE (attr) && TREE_VALUE (TREE_VALUE (attr)));
      return GEN_INT (TREE_INT_CST_LOW (TREE_VALUE (TREE_VALUE (attr))));
    }
  return x;
}


/* nxu16 attributes.  */
TARGET_GNU_ATTRIBUTES (nxu16_attribute_table,
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "progmem",   0, 0, false, false, false, false,
    nxu16_handle_progmem_attribute, NULL },
  { "signal", 0, -1, true,  false, false, false,
    nxu16_handle_fndecl_attribute, NULL },
  { "interrupt", 0, -1, true,  false, false, false,
    nxu16_handle_fndecl_attribute, NULL },
  { "noblock", 0, 0, true,  false, false, false,
    nxu16_handle_fndecl_attribute, NULL },
  { "no_gccisr", 0, 0, true,  false, false, false,
    nxu16_handle_fndecl_attribute, NULL },
  { "naked",     0, 0, false, true,  true,  false,
    nxu16_handle_fntype_attribute, NULL },
  { "OS_task",   0, 0, false, true,  true,  false,
    nxu16_handle_fntype_attribute, NULL },
  { "OS_main",   0, 0, false, true,  true,  false,
    nxu16_handle_fntype_attribute, NULL },
  { "io",        0, 1, true, false, false,  false,
    nxu16_handle_addr_attribute, NULL },
  { "io_low",    0, 1, true, false, false,  false,
    nxu16_handle_addr_attribute, NULL },
  { "address",   1, 1, true, false, false,  false,
    nxu16_handle_addr_attribute, NULL },
  { "absdata",   0, 0, true, false, false,  false,
    nxu16_handle_absdata_attribute, NULL }
});


/* Return true if we support address space AS for the architecture in effect
   and false, otherwise.  If LOC is not UNKNOWN_LOCATION then also issue
   a respective error.  */

bool
nxu16_addr_space_supported_p (addr_space_t as, location_t loc)
{
  if (nxu16_TINY)
    {
      if (loc != UNKNOWN_LOCATION)
	error_at (loc, "address spaces are not supported for reduced "
		  "Tiny devices");
      return false;
    }
  else if (nxu16_addrspace[as].segment >= nxu16_n_flash)
    {
      if (loc != UNKNOWN_LOCATION)
	error_at (loc, "address space %qs not supported for devices with "
		  "flash size up to %d KiB", nxu16_addrspace[as].name,
		  64 * nxu16_n_flash);
      return false;
    }

  return true;
}


/* Implement `TARGET_ADDR_SPACE_DIAGNOSE_USAGE'.  */

static void
nxu16_addr_space_diagnose_usage (addr_space_t as, location_t loc)
{
  (void) nxu16_addr_space_supported_p (as, loc);
}


/* Implement `TARGET_ADDR_SPACE_ZERO_ADDRESS_VALID'.  Zero is a valid
   address in all address spaces. Even in ADDR_SPACE_FLASH1 etc..,
   a zero address is valid and means 0x<RAMPZ val>0000, where RAMPZ is
   set to the appropriate segment value. */

static bool
nxu16_addr_space_zero_address_valid (addr_space_t)
{
  return true;
}


/* Look if DECL shall be placed in program memory space by
   means of attribute `progmem' or some address-space qualifier.
   Return non-zero if DECL is data that must end up in Flash and
   zero if the data lives in RAM (.bss, .data, .rodata, ...).

   Return 2   if DECL is located in 24-bit flash address-space
   Return 1   if DECL is located in 16-bit flash address-space
   Return -1  if attribute `progmem' occurs in DECL or ATTRIBUTES
   Return 0   otherwise  */

int
nxu16_progmem_p (tree decl, tree attributes)
{
  if (TREE_CODE (decl) != VAR_DECL)
    return 0;

  if (nxu16_decl_memx_p (decl))
    return 2;

  if (nxu16_decl_flash_p (decl))
    return 1;

  if (NULL_TREE
      != lookup_attribute ("progmem", attributes))
    return -1;

  tree a = decl;

  do
    a = TREE_TYPE(a);
  while (TREE_CODE (a) == ARRAY_TYPE);

  if (a == error_mark_node)
    return 0;

  if (NULL_TREE != lookup_attribute ("progmem", TYPE_ATTRIBUTES (a)))
    return -1;

  return 0;
}


/* Return true if DECL has attribute `absdata' set.  This function should
   only be used for nxu16_TINY.  */

static bool
nxu16_decl_absdata_p (tree decl, tree attributes)
{
  return (VAR_P (decl)
	  && NULL_TREE != lookup_attribute ("absdata", attributes));
}


/* Scan type TYP for pointer references to address space ASn.
   Return ADDR_SPACE_GENERIC (i.e. 0) if all pointers targeting
   the AS are also declared to be CONST.
   Otherwise, return the respective address space, i.e. a value != 0.  */

static addr_space_t
nxu16_nonconst_pointer_addrspace (tree typ)
{
  while (ARRAY_TYPE == TREE_CODE (typ))
    typ = TREE_TYPE (typ);

  if (POINTER_TYPE_P (typ))
    {
      tree target = TREE_TYPE (typ);

      /* Pointer to function: Test the function's return type.  */

      if (FUNCTION_TYPE == TREE_CODE (target))
	return nxu16_nonconst_pointer_addrspace (TREE_TYPE (target));

      /* "Ordinary" pointers... */

      while (TREE_CODE (target) == ARRAY_TYPE)
	target = TREE_TYPE (target);

      /* Pointers to non-generic address space must be const.  */

      addr_space_t as = TYPE_ADDR_SPACE (target);

      if (!ADDR_SPACE_GENERIC_P (as)
	  && !TYPE_READONLY (target)
	  && nxu16_addr_space_supported_p (as))
	{
	  return as;
	}

      /* Scan pointer's target type.  */

      return nxu16_nonconst_pointer_addrspace (target);
    }

  return ADDR_SPACE_GENERIC;
}


/* Sanity check NODE so that all pointers targeting non-generic address spaces
   go along with CONST qualifier.  Writing to these address spaces should
   be detected and complained about as early as possible.  */

static bool
nxu16_pgm_check_var_decl (tree node)
{
  const char *reason = NULL;

  addr_space_t as = ADDR_SPACE_GENERIC;

  gcc_assert (as == 0);

  if (nxu16_log.progmem)
    nxu16_edump ("%?: %t\n", node);

  switch (TREE_CODE (node))
    {
    default:
      break;

    case VAR_DECL:
      if (as = nxu16_nonconst_pointer_addrspace (TREE_TYPE (node)), as)
	reason = _("variable");
      break;

    case PARM_DECL:
      if (as = nxu16_nonconst_pointer_addrspace (TREE_TYPE (node)), as)
	reason = _("function parameter");
      break;

    case FIELD_DECL:
      if (as = nxu16_nonconst_pointer_addrspace (TREE_TYPE (node)), as)
	reason = _("structure field");
      break;

    case FUNCTION_DECL:
      if (as = nxu16_nonconst_pointer_addrspace (TREE_TYPE (TREE_TYPE (node))),
	  as)
	reason = _("return type of function");
      break;

    case POINTER_TYPE:
      if (as = nxu16_nonconst_pointer_addrspace (node), as)
	reason = _("pointer");
      break;
    }

  if (reason)
    {
      if (TYPE_P (node))
	error ("pointer targeting address space %qs must be const in %qT",
	       nxu16_addrspace[as].name, node);
      else
	error ("pointer targeting address space %qs must be const"
	       " in %s %q+D",
	       nxu16_addrspace[as].name, reason, node);
    }

  return reason == NULL;
}



#ifdef HAVE_LD_nxu16_nxu16XMEGA2_FLMAP
static const bool have_nxu16xmega2_flmap = true;
#else
static const bool have_nxu16xmega2_flmap = false;
#endif

#ifdef HAVE_LD_nxu16_nxu16XMEGA4_FLMAP
static const bool have_nxu16xmega4_flmap = true;
#else
static const bool have_nxu16xmega4_flmap = false;
#endif

#ifdef HAVE_LD_nxu16_nxu16XMEGA3_RODATA_IN_FLASH
static const bool have_nxu16xmega3_rodata_in_flash = true;
#else
static const bool have_nxu16xmega3_rodata_in_flash = false;
#endif


static bool
nxu16_rodata_in_flash_p ()
{
  switch (nxu16_arch_index)
    {
    default:
      break;

    case ARCH_nxu16TINY:
      return true;

    case ARCH_nxu16XMEGA3:
      return have_nxu16xmega3_rodata_in_flash;

    case ARCH_nxu16XMEGA2:
      return nxu16_flmap && have_nxu16xmega2_flmap && nxu16_rodata_in_ram != 1;

    case ARCH_nxu16XMEGA4:
      return nxu16_flmap && have_nxu16xmega4_flmap && nxu16_rodata_in_ram != 1;
    }

  return false;
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


/* Unnamed section callback for data_section
   to track need of __do_copy_data.  */

static void
nxu16_output_data_section_asm_op (const char *data)
{
  nxu16_need_copy_data_p = true;

  /* Dispatch to default.  */
  output_section_asm_op (data);
}


/* Unnamed section callback for bss_section
   to track need of __do_clear_bss.  */

static void
nxu16_output_bss_section_asm_op (const char *data)
{
  nxu16_need_clear_bss_p = true;

  /* Dispatch to default.  */
  output_section_asm_op (data);
}


/* Unnamed section callback for progmem*.data sections.  */

static void
nxu16_output_progmem_section_asm_op (const char *data)
{
  fprintf (asm_out_file, "\t.section\t%s,\"a\",@progbits\n", data);
}


/* A noswitch section callback to output symbol definitions for
   attributes "io", "io_low" and "address".  */

static bool
nxu16_output_addr_attrib (tree decl, const char *name,
			unsigned HOST_WIDE_INT /* size */,
			unsigned HOST_WIDE_INT /* align */)
{
  gcc_assert (DECL_RTL_SET_P (decl));

  FILE *stream = asm_out_file;
  bool local_p = ! DECL_WEAK (decl) && ! TREE_PUBLIC (decl);
  rtx symbol, mem = DECL_RTL (decl);

  if (mem != NULL_RTX && MEM_P (mem)
      && SYMBOL_REF_P ((symbol = XEXP (mem, 0)))
      && (SYMBOL_REF_FLAGS (symbol) & (SYMBOL_FLAG_IO | SYMBOL_FLAG_ADDRESS)))
    {
      if (! local_p)
	{
	  fprintf (stream, "\t%s\t", DECL_WEAK (decl) ? ".weak" : ".globl");
	  assemble_name (stream, name);
	  fprintf (stream, "\n");
	}

      if (SYMBOL_REF_FLAGS (symbol) & SYMBOL_FLAG_ADDRESS)
	{
	  assemble_name (stream, name);
	  fprintf (stream, " = %ld\n",
		   (long) INTVAL (nxu16_eval_addr_attrib (symbol)));
	}
      else if (local_p)
	{
	  const char *names[] = { "io", "io_low", "address", NULL };
	  for (const char **p = names; *p; ++p)
	    if (lookup_attribute (*p, DECL_ATTRIBUTES (decl)))
	      {
		error ("static attribute %qs declaration for %q+D needs an "
		       "address", *p, decl);
		break;
	      }
	}

      return true;
    }

  gcc_unreachable();

  return false;
}


/* A helper for the next function.  NODE is a decl that is associated with
   a symbol.  Return TRUE if the respective object may be accessed by LDS.
   There might still be other reasons for why LDS is not appropriate.
   This function is only appropriate for nxu16_TINY.  */

static bool
nxu16_decl_maybe_lds_p (tree node)
{
  if (!node
      || TREE_CODE (node) != VAR_DECL
      || DECL_SECTION_NAME (node) != NULL)
    return false;

  /* Don't use LDS for objects that go to .rodata.  The current default
     linker description file still locates .rodata in RAM, but this is not
     a must.  A better linker script would just keep .rodata in flash and
     add an offset of 0x4000 to the VMA.  Hence avoid LDS for such data.  */

  if (TREE_READONLY (node))
    return false;

  // C++ requires peeling arrays.

  do
    node = TREE_TYPE (node);
  while (ARRAY_TYPE == TREE_CODE (node));

  return (node != error_mark_node
	  && !TYPE_READONLY (node));
}




/* Implement `TARGET_ASM_FILE_START'.  */
/* Outputs some text at the start of each assembler file.  */

static void
nxu16_file_start (void)
{
  int sfr_offset = nxu16_arch->sfr_offset;

  if (nxu16_arch->asm_only)
    error ("architecture %qs supported for assembler only", nxu16_mmcu);

  default_file_start ();

  /* Print I/O addresses of some SFRs used with IN and OUT.  */

  if (nxu16_HAVE_SPH)
    fprintf (asm_out_file, "__SP_H__ = 0x%02x\n", nxu16_addr.sp_h - sfr_offset);

  fprintf (asm_out_file, "__SP_L__ = 0x%02x\n", nxu16_addr.sp_l - sfr_offset);
  fprintf (asm_out_file, "__SREG__ = 0x%02x\n", nxu16_addr.sreg - sfr_offset);
  if (nxu16_HAVE_RAMPZ)
    fprintf (asm_out_file, "__RAMPZ__ = 0x%02x\n", nxu16_addr.rampz - sfr_offset);
  if (nxu16_HAVE_RAMPY)
    fprintf (asm_out_file, "__RAMPY__ = 0x%02x\n", nxu16_addr.rampy - sfr_offset);
  if (nxu16_HAVE_RAMPX)
    fprintf (asm_out_file, "__RAMPX__ = 0x%02x\n", nxu16_addr.rampx - sfr_offset);
  if (nxu16_HAVE_RAMPD)
    fprintf (asm_out_file, "__RAMPD__ = 0x%02x\n", nxu16_addr.rampd - sfr_offset);
  if (nxu16_XMEGA || nxu16_TINY)
    fprintf (asm_out_file, "__CCP__ = 0x%02x\n", nxu16_addr.ccp - sfr_offset);
  fprintf (asm_out_file, "__tmp_reg__ = %d\n", nxu16_TMP_REGNO);
  fprintf (asm_out_file, "__zero_reg__ = %d\n", nxu16_ZERO_REGNO);
}


/* Implement `TARGET_ASM_FILE_END'.  */
/* Outputs to the stdio stream FILE some
   appropriate text to go at the end of an assembler file.  */

static void
nxu16_file_end (void)
{
  /* Output these only if there is anything in the
     .data* / .rodata* / .gnu.linkonce.* resp. .bss* or COMMON
     input section(s) - some code size can be saved by not
     linking in the initialization code from libgcc if resp.
     sections are empty, see PR18145.  */

  if (nxu16_need_copy_data_p
      || (nxu16_has_rodata_p && ! nxu16_rodata_in_flash_p ()))
    fputs (".global __do_copy_data\n", asm_out_file);

  if (nxu16_need_clear_bss_p)
    fputs (".global __do_clear_bss\n", asm_out_file);
}


/* Worker function for `ADJUST_REG_ALLOC_ORDER'.  */
/* Choose the order in which to allocate hard registers for
   pseudo-registers local to a basic block.

   Store the desired register order in the array `reg_alloc_order'.
   Element 0 should be the register to allocate first; element 1, the
   next register; and so on.  */

void
nxu16_adjust_reg_alloc_order (void)
{
  static const int order_0[] =
    {
      24, 25,
      18, 19, 20, 21, 22, 23,
      30, 31,
      26, 27, 28, 29,
      17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2,
      0, 1,
      32, 33, 34, 35
    };
  static const int tiny_order_0[] = {
    20, 21,
    22, 23,
    24, 25,
    30, 31,
    26, 27,
    28, 29,
    19, 18,
    16, 17,
    32, 33, 34, 35,
    15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0
  };
  static const int order_1[] =
    {
      18, 19, 20, 21, 22, 23, 24, 25,
      30, 31,
      26, 27, 28, 29,
      17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2,
      0, 1,
      32, 33, 34, 35
    };
  static const int tiny_order_1[] = {
    22, 23,
    24, 25,
    30, 31,
    26, 27,
    28, 29,
    21, 20, 19, 18,
    16, 17,
    32, 33, 34, 35,
    15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0
  };
  static const int order_2[] =
    {
      25, 24, 23, 22, 21, 20, 19, 18,
      30, 31,
      26, 27, 28, 29,
      17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2,
      1, 0,
      32, 33, 34, 35
    };

  /* Select specific register allocation order.
     Tiny Core (ATtiny4/5/9/10/20/40) devices have only 16 registers,
     so different allocation order should be used.  */

  const int *order = (TARGET_ORDER_1 ? (nxu16_TINY ? tiny_order_1 : order_1)
		      : TARGET_ORDER_2 ? (nxu16_TINY ? tiny_order_0 : order_2)
		      : (nxu16_TINY ? tiny_order_0 : order_0));

  for (size_t i = 0; i < ARRAY_SIZE (order_0); ++i)
    reg_alloc_order[i] = order[i];
}





/* Cost for mul highpart.  X is a LSHIFTRT, i.e. the outer TRUNCATE is
   already stripped off.  */

static int
nxu16_mul_highpart_cost (rtx x, int)
{
  if (nxu16_HAVE_MUL
      && LSHIFTRT == GET_CODE (x)
      && MULT == GET_CODE (XEXP (x, 0))
      && CONST_INT_P (XEXP (x, 1)))
    {
      // This is the wider mode.
      machine_mode mode = GET_MODE (x);

      // The middle-end might still have PR81444, i.e. it is calling the cost
      // functions with strange modes.  Fix this now by also considering
      // PSImode (should actually be SImode instead).
      if (HImode == mode || PSImode == mode || SImode == mode)
	{
	  return COSTS_N_INSNS (2);
	}
    }

  return 10000;
}


/* Return the expected cost of a conditional branch like
   (set (pc)
	(if_then_else (X)
		      (label_ref *)
		      (pc)))
   where X is some comparison operator.  */

static int
nxu16_cbranch_cost (rtx x)
{
  bool difficult_p = difficult_comparison_operator (x, VOIDmode);

  if (reload_completed)
    {
      // After reload, we basically just have plain branches.
      return COSTS_N_INSNS (1 + difficult_p);
    }

  rtx xreg = XEXP (x, 0);
  rtx xval = XEXP (x, 1);
  machine_mode mode = GET_MODE (xreg);
  if (mode == VOIDmode)
    mode = GET_MODE (xval);
  int size = GET_MODE_SIZE (mode);

  if (GET_CODE (xreg) == ZERO_EXTEND
      || GET_CODE (xval) == ZERO_EXTEND)
    {
      // *cbranch<HISI:mode>.<code><QIPSI:mode>.0/1, code = zero_extend.
      return COSTS_N_INSNS (size + 1);
    }

  if (GET_CODE (xreg) == SIGN_EXTEND
      || GET_CODE (xval) == SIGN_EXTEND)
    {
      // *cbranch<HISI:mode>.<code><QIPSI:mode>.0/1, code = sign_extend.
      // Make it a bit cheaper than it actually is (less reg pressure).
      return COSTS_N_INSNS (size + 1 + 1);
    }

  if (GET_CODE (xreg) == ZERO_EXTRACT
      && XEXP (xreg, 1) == const1_rtx)
    {
      // Branch on a single bit, with an additional edge due to less
      // register pressure.
      return (int) COSTS_N_INSNS (1.5);
    }

  bool reg_p = register_operand (xreg, mode);
  bool reg_or_0_p = reg_or_0_operand (xval, mode);

  return COSTS_N_INSNS (size
			// For the branch
			+ 1 + difficult_p
			// Combine might propagate constants other than zero
			// into the 2nd operand.  Make that more expensive.
			+ 1 * (!reg_p || !reg_or_0_p));
}




/* Test for extra memory constraint 'Q'.
   It's a memory address based on Y or Z pointer with valid displacement.  */

int
extra_constraint_Q (rtx x)
{
  int ok = 0;
  rtx plus = XEXP (x, 0);

  if (GET_CODE (plus) == PLUS
      && REG_P (XEXP (plus, 0))
      && CONST_INT_P (XEXP (plus, 1))
      && (INTVAL (XEXP (plus, 1))
	  <= MAX_LD_OFFSET (GET_MODE (x))))
    {
      rtx xx = XEXP (plus, 0);
      int regno = REGNO (xx);

      ok = (/* allocate pseudos */
	    regno >= FIRST_PSEUDO_REGISTER
	    /* strictly check */
	    || regno == REG_Z || regno == REG_Y
	    /* XXX frame & arg pointer checks */
	    || xx == frame_pointer_rtx
	    || xx == arg_pointer_rtx);

      if (nxu16_log.constraints)
	nxu16_edump ("\n%?=%d reload_completed=%d ra_in_progress=%d\n %r\n",
		   ok, reload_completed, ra_in_progress (), x);
    }

  return ok;
}

/* Convert condition code CONDITION to the valid nxu16 condition code.  */

rtx_code
nxu16_normalize_condition (rtx_code condition)
{
  switch (condition)
    {
    case GT:
      return GE;
    case GTU:
      return GEU;
    case LE:
      return LT;
    case LEU:
      return LTU;
    default:
      gcc_unreachable ();
    }
}


/* Returns register number for function return value.*/

static inline unsigned int
nxu16_ret_register (void)
{
  return REG_24;
}


/* Implement `TARGET_FUNCTION_VALUE_REGNO_P'.  */

static bool
nxu16_function_value_regno_p (const unsigned int regno)
{
  return regno == nxu16_ret_register ();
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

int
test_hard_reg_class (reg_class rclass, rtx x)
{
  int regno = true_regnum (x);
  if (regno < 0)
    return 0;

  if (TEST_HARD_REG_CLASS (rclass, regno))
    return 1;

  return 0;
}


/* Helper for `jump_over_one_insn_p':  Test if INSN is a 2-word instruction
   and thus is suitable to be skipped by CPSE, SBRC, etc.  */

static bool
nxu16_2word_insn_p (rtx_insn *insn)
{
  if (TARGET_SKIP_BUG || !insn || get_attr_length (insn) != 2)
    {
      return false;
    }

  switch (INSN_CODE (insn))
    {
    default:
      return (recog_memoized (insn) >= 0
	      // Transparent calls may be skipped.
	      && (get_attr_type (insn) == TYPE_XCALL
		  || get_attr_adjust_len (insn) == ADJUST_LEN_CALL));

    case CODE_FOR_movqi_insn:
    case CODE_FOR_movuqq_insn:
    case CODE_FOR_movqq_insn:
      {
	rtx set  = single_set (insn);
	rtx src  = SET_SRC (set);
	rtx dest = SET_DEST (set);

	/* Factor out LDS and STS from movqi_insn.  */

	if (MEM_P (dest)
	    && (REG_P (src) || src == CONST0_RTX (GET_MODE (dest))))
	  {
	    return CONSTANT_ADDRESS_P (XEXP (dest, 0));
	  }
	else if (REG_P (dest)
		 && MEM_P (src))
	  {
	    return CONSTANT_ADDRESS_P (XEXP (src, 0));
	  }

	return false;
      }

    case CODE_FOR_call_insn:
    case CODE_FOR_call_value_insn:
      return true;
    }
}


int
jump_over_one_insn_p (rtx_insn *insn, rtx dest)
{
  int uid = INSN_UID (GET_CODE (dest) == LABEL_REF
		      ? XEXP (dest, 0)
		      : dest);
  int jump_addr = INSN_ADDRESSES (INSN_UID (insn));
  int dest_addr = INSN_ADDRESSES (uid);
  int jump_offset = dest_addr - jump_addr - get_attr_length (insn);

  return (jump_offset == 1
	  || (jump_offset == 2
	      && nxu16_2word_insn_p (next_active_insn (insn))));
}







/* Implement `MODE_CODE_BASE_REG_CLASS'.  */

reg_class
nxu16_mode_code_base_reg_class (machine_mode /*mode*/, addr_space_t as,
			      rtx_code outer_code, rtx_code /*index_code*/)
{
  if (!ADDR_SPACE_GENERIC_P (as))
    {
      return POINTER_Z_REGS;
    }

  if (nxu16_TINY)
    // We allow all offsets for all pointer regs.  Pass .nxu16-fuse-add
    // will rectify it (register allocation cannot do it).
    return POINTER_REGS;

  if (!nxu16_strict_X)
    return reload_completed ? BASE_POINTER_REGS : POINTER_REGS;

  return PLUS == outer_code ? BASE_POINTER_REGS : POINTER_REGS;
}


/* Implement `REGNO_MODE_CODE_OK_FOR_BASE_P'.  */

bool
nxu16_regno_mode_code_ok_for_base_p (int regno, machine_mode /*mode*/,
				   addr_space_t as, rtx_code outer_code,
				   rtx_code /*index_code*/)
{
  bool ok = false;

  if (!ADDR_SPACE_GENERIC_P (as))
    {
      if (regno < FIRST_PSEUDO_REGISTER
	  && regno == REG_Z)
	{
	  return true;
	}

      if (reg_renumber)
	{
	  regno = reg_renumber[regno];

	  if (regno == REG_Z)
	    {
	      return true;
	    }
	}

      return false;
    }

  if (regno < FIRST_PSEUDO_REGISTER
      && (regno == REG_X
	  || regno == REG_Y
	  || regno == REG_Z
	  || regno == ARG_POINTER_REGNUM))
    {
      ok = true;
    }
  else if (reg_renumber)
    {
      regno = reg_renumber[regno];

      if (regno == REG_X
	  || regno == REG_Y
	  || regno == REG_Z
	  || regno == ARG_POINTER_REGNUM)
	{
	  ok = true;
	}
    }

  if (nxu16_strict_X
      // On Reduced Tiny, all registers are equal in that they do not
      // support PLUS addressing; respective addresses will be fake,
      // even for the frame pointer.  They must be handled in the
      // printers by add-store-sub sequences -- or may be split after
      // reload by `nxu16_split_tiny_move'.
      && ! nxu16_TINY
      && PLUS == outer_code
      && regno == REG_X)
    {
      ok = false;
    }

  return ok;
}


/* Reload the constant OP[1] into the HI register OP[0].
   CLOBBER_REG is a QI clobber reg needed to move vast majority of consts
   into a NO_LD_REGS register.  If CLOBBER_REG is NULL_RTX we either don't
   need a clobber reg or have to cook one up.

   PLEN == NULL: Output instructions.
   PLEN != NULL: Output nothing.  Set *PLEN to number of words occupied
		 by the insns printed.

   Return "".  */

const char *
output_reload_inhi (rtx *op, rtx clobber_reg, int *plen)
{
  output_reload_in_const (op, clobber_reg, plen, false);
  return "";
}


/* Reload a SI or SF compile time constant OP[1] into the register OP[0].
   CLOBBER_REG is a QI clobber reg needed to move vast majority of consts
   into a NO_LD_REGS register.  If CLOBBER_REG is NULL_RTX we either don't
   need a clobber reg or have to cook one up.

   LEN == NULL: Output instructions.

   LEN != NULL: Output nothing.  Set *LEN to number of words occupied
		by the insns printed.

   Return "".  */

const char *
output_reload_insisf (rtx *op, rtx clobber_reg, int *len)
{
  if (nxu16_HAVE_MOVW
      && !test_hard_reg_class (LD_REGS, op[0])
      && (CONST_INT_P (op[1])
	  || CONST_FIXED_P (op[1])
	  || CONST_DOUBLE_P (op[1])))
    {
      /* In some cases it is better to clear the destination beforehand, e.g.

	     CLR R2   CLR R3   MOVW R4,R2   INC R2

	 is shorther than

	     CLR R2   INC R2   CLR  R3      CLR R4   CLR R5

	 We find it too tedious to work that out in the print function.
	 Instead, we call the print function twice to get the lengths of
	 both methods and use the shortest one.  */

      int len_clr, len_noclr;
      output_reload_in_const (op, clobber_reg, &len_clr, true);
      output_reload_in_const (op, clobber_reg, &len_noclr, false);

      if (len_noclr - len_clr == 4)
	{
	  /* Default needs 4 CLR instructions: clear register beforehand.  */

	  nxu16_asm_len ("mov %A0,__zero_reg__" CR_TAB
		       "mov %B0,__zero_reg__" CR_TAB
		       "movw %C0,%A0", &op[0], len, 3);

	  output_reload_in_const (op, clobber_reg, len, true);

	  if (len)
	    *len += 3;

	  return "";
	}
    }

  /* Default: destination not pre-cleared.  */

  output_reload_in_const (op, clobber_reg, len, false);
  return "";
}

const char *
nxu16_out_reload_inpsi (rtx *op, rtx clobber_reg, int *len)
{
  output_reload_in_const (op, clobber_reg, len, false);
  return "";
}


/* Worker function for `ASM_OUTPUT_ADDR_VEC'.  */
/* Emit jump tables out-of-line so that branches crossing the table
   get shorter offsets.  If we have JUMP + CALL, then put the tables
   in a dedicated non-.text section so that CALLs get better chance to
   be relaxed to RCALLs.

   We emit the tables by hand because `function_rodata_section' does not
   work as expected, cf. PR71151, and we do *NOT* want the table to be
   in .rodata, hence setting JUMP_TABLES_IN_TEXT_SECTION = 0 is of limited
   use; and setting it to 1 attributes table lengths to branch offsets...
   Moreover, fincal.c keeps switching section before each table entry
   which we find too fragile as to rely on section caching.  */

void
nxu16_output_addr_vec (rtx_insn *labl, rtx table)
{
  FILE *stream = asm_out_file;

  app_disable();

  // Switch to appropriate (sub)section.

  if (DECL_SECTION_NAME (current_function_decl)
      && symtab_node::get (current_function_decl)
      && ! symtab_node::get (current_function_decl)->implicit_section)
    {
      // .subsection will emit the code after the function and in the
      // section as chosen by the user.

      switch_to_section (current_function_section ());
      fprintf (stream, "\t.subsection\t1\n");
    }
  else
    {
      // Since PR63223 there is no restriction where to put the table; it
      // may even reside above 128 KiB.  We put it in a section as high as
      // possible and avoid progmem in order not to waste flash <= 64 KiB.

      const char *sec_name = ".jumptables.gcc";

      // The table belongs to its host function, therefore use fine
      // grained sections so that, if that function is removed by
      // --gc-sections, the child table(s) may also be removed.  */

      tree asm_name = DECL_ASSEMBLER_NAME (current_function_decl);
      const char *fname = IDENTIFIER_POINTER (asm_name);
      fname = targetm.strip_name_encoding (fname);
      sec_name = ACONCAT ((sec_name, ".", fname, NULL));

      fprintf (stream, "\t.section\t%s,\"%s\",@progbits\n", sec_name,
	       nxu16_HAVE_JMP_CALL ? "a" : "ax");
    }

  // Output the label that preceeds the table.

  ASM_OUTPUT_ALIGN (stream, 1);
  targetm.asm_out.internal_label (stream, "L", CODE_LABEL_NUMBER (labl));

  // Output the table's content.

  int vlen = XVECLEN (table, 0);

  for (int idx = 0; idx < vlen; idx++)
    {
      int value = CODE_LABEL_NUMBER (XEXP (XVECEXP (table, 0, idx), 0));

      if (nxu16_HAVE_JMP_CALL)
	fprintf (stream, "\t.word gs(.L%d)\n", value);
      else
	fprintf (stream, "\trjmp .L%d\n", value);
    }

  // Switch back to original section.  As we clobbered the section above,
  // forget the current section before switching back.

  in_section = NULL;
  switch_to_section (current_function_section ());
}



/* Implement `TARGET_HARD_REGNO_SCRATCH_OK'.  */
/* Returns true if SCRATCH are safe to be allocated as a scratch
   registers (for a define_peephole2) in the current function.  */



/* Worker function for `HARD_REGNO_RENAME_OK'.  */
/* Return nonzero if register OLD_REG can be renamed to register NEW_REG.  */

int
nxu16_hard_regno_rename_ok (unsigned int old_reg, unsigned int new_reg)
{
  /* Interrupt functions can only use registers that have already been
     saved by the prologue, even if they would normally be
     call-clobbered.  */

  if ((cfun->machine->is_interrupt || cfun->machine->is_signal)
      && !df_regs_ever_live_p (new_reg))
    return 0;

  /* Don't allow hard registers that might be part of the frame pointer.
     Some places in the compiler just test for [HARD_]FRAME_POINTER_REGNUM
     and don't care for a frame pointer that spans more than one register.  */

  if ((!reload_completed || frame_pointer_needed)
      && (old_reg == REG_Y || old_reg == REG_Y + 1
	  || new_reg == REG_Y || new_reg == REG_Y + 1))
    {
      return 0;
    }

  return 1;
}

/* Output a branch that tests a single bit of a register (QI, HI, SI or DImode)
   or memory location in the I/O space (QImode only).

   Operand 0: comparison operator (must be EQ or NE, compare bit to zero).
   Operand 1: register operand to test, or CONST_INT memory address.
   Operand 2: bit number.
   Operand 3: label to jump to if the test is true.  */

const char *
nxu16_out_sbxx_branch (rtx_insn *insn, rtx xop[])
{
  

  return "";
}


/* Worker function for `TARGET_ASM_CONSTRUCTOR'.  */

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


/* Implement `TARGET_RETURN_IN_MEMORY'.  */

static bool
nxu16_return_in_memory (const_tree type, const_tree /*fntype*/)
{
  HOST_WIDE_INT size = int_size_in_bytes (type);
  HOST_WIDE_INT ret_size_limit = nxu16_TINY ? 4 : 8;

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











/* Helper for following function.  */

static bool
nxu16_reg_ok_for_pgm_addr (rtx reg, bool strict)
{
  gcc_assert (REG_P (reg));

  if (strict)
    {
      return REGNO (reg) == REG_Z;
    }

  /* Avoid combine to propagate hard regs.  */

  if (can_create_pseudo_p()
      && REGNO (reg) < REG_Z)
    {
      return false;
    }

  return true;
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

    case ADDR_SPACE_FLASH:
    case ADDR_SPACE_FLASH1:
    case ADDR_SPACE_FLASH2:
    case ADDR_SPACE_FLASH3:
    case ADDR_SPACE_FLASH4:
    case ADDR_SPACE_FLASH5:

      switch (GET_CODE (x))
	{
	case REG:
	  ok = nxu16_reg_ok_for_pgm_addr (x, strict);
	  break;

	case POST_INC:
	  ok = nxu16_reg_ok_for_pgm_addr (XEXP (x, 0), strict);
	  break;

	default:
	  break;
	}

      break; /* FLASH */

    case ADDR_SPACE_MEMX:
      if (REG_P (x))
	ok = (!strict
	      && can_create_pseudo_p());

      if (LO_SUM == GET_CODE (x))
	{
	  rtx hi = XEXP (x, 0);
	  rtx lo = XEXP (x, 1);

	  ok = (REG_P (hi)
		&& (!strict || REGNO (hi) < FIRST_PSEUDO_REGISTER)
		&& REG_P (lo)
		&& REGNO (lo) == REG_Z);
	}

      break; /* MEMX */
    }

  if (nxu16_log.legitimate_address_p)
    {
      nxu16_edump ("\n%?: ret=%b, mode=%m strict=%d "
		 "reload_completed=%d ra_in_progress=%d %s:",
		 ok, mode, strict, reload_completed, ra_in_progress (),
		 reg_renumber ? "(reg_renumber)" : "");

      if (GET_CODE (x) == PLUS
	  && REG_P (XEXP (x, 0))
	  && CONST_INT_P (XEXP (x, 1))
	  && IN_RANGE (INTVAL (XEXP (x, 1)), 0, MAX_LD_OFFSET (mode))
	  && reg_renumber)
	{
	  nxu16_edump ("(r%d ---> r%d)", REGNO (XEXP (x, 0)),
		     true_regnum (XEXP (x, 0)));
	}

      nxu16_edump ("\n%r\n", x);
    }

  return ok;
}


/* Implement `TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS'.  */

static rtx
nxu16_addr_space_legitimize_address (rtx x, rtx old_x,
				   machine_mode mode, addr_space_t as)
{
  if (ADDR_SPACE_GENERIC_P (as))
    return nxu16_legitimize_address (x, old_x, mode);

  if (nxu16_log.legitimize_address)
    {
      nxu16_edump ("\n%?: mode=%m\n %r\n", mode, old_x);
    }

  return old_x;
}





/* Implement `TARGET_ADDR_SPACE_SUBSET_P'.  */

static bool
nxu16_addr_space_subset_p (addr_space_t /*subset*/, addr_space_t /*superset*/)
{
  /* Allow any kind of pointer mess.  */

  return true;
}


/* Implement `TARGET_CONVERT_TO_TYPE'.  */

static tree
nxu16_convert_to_type (tree type, tree expr)
{
  /* Print a diagnose for pointer conversion that changes the address
     space of the pointer target to a non-enclosing address space,
     provided -Waddr-space-convert is on.

     FIXME: Filter out cases where the target object is known to
	    be located in the right memory, like in

		(const __flash*) PSTR ("text")

	    Also try to distinguish between explicit casts requested by
	    the user and implicit casts like

		void f (const __flash char*);

		void g (const char *p)
		{
		    f ((const __flash*) p);
		}

	    under the assumption that an explicit casts means that the user
	    knows what he is doing, e.g. interface with PSTR or old style
	    code with progmem and pgm_read_xxx.
  */

  if (nxu16_warn_addr_space_convert
      && expr != error_mark_node
      && POINTER_TYPE_P (type)
      && POINTER_TYPE_P (TREE_TYPE (expr)))
    {
      addr_space_t as_old = TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (expr)));
      addr_space_t as_new = TYPE_ADDR_SPACE (TREE_TYPE (type));

      if (nxu16_log.progmem)
	nxu16_edump ("%?: type = %t\nexpr = %t\n\n", type, expr);

      if (as_new != ADDR_SPACE_MEMX
	  && as_new != as_old)
	{
	  location_t loc = EXPR_LOCATION (expr);
	  const char *name_old = nxu16_addrspace[as_old].name;
	  const char *name_new = nxu16_addrspace[as_new].name;

	  warning (OPT_Waddr_space_convert,
		   "conversion from address space %qs to address space %qs",
		   ADDR_SPACE_GENERIC_P (as_old) ? "generic" : name_old,
		   ADDR_SPACE_GENERIC_P (as_new) ? "generic" : name_new);

	  return fold_build1_loc (loc, ADDR_SPACE_CONVERT_EXPR, type, expr);
	}
    }

  return NULL_TREE;
}


/* Implement `TARGET_LEGITIMATE_COMBINED_INSN'.  */
/* PR78883: Filter out paradoxical SUBREGs of MEM which are not handled
   properly by following passes.  As INSN_SCHEDULING is off and hence
   general_operand accepts such expressions, ditch them now.  */

static bool
nxu16_legitimate_combined_insn (rtx_insn *insn)
{
  subrtx_iterator::array_type array;

  FOR_EACH_SUBRTX (iter, array, PATTERN (insn), NONCONST)
    {
      const_rtx op = *iter;

      if (SUBREG_P (op)
	  && MEM_P (SUBREG_REG (op))
	  && (GET_MODE_SIZE (GET_MODE (op))
	      > GET_MODE_SIZE (GET_MODE (SUBREG_REG (op)))))
	{
	  return false;
	}
    }

  return true;
}


/* PR63633: The middle-end might come up with hard regs as input operands.

   RMASK is a bit mask representing a subset of hard registers R0...R31:
   Rn is an element of that set iff bit n of RMASK is set.
   OPMASK describes a subset of OP[]:  If bit n of OPMASK is 1 then
   OP[n] has to be fixed; otherwise OP[n] is left alone.

   For each element of OPMASK which is a hard register overlapping RMASK,
   replace OP[n] with a newly created pseudo register

   HREG == 0:  Also emit a move insn that copies the contents of that
	       hard register into the new pseudo.

   HREG != 0:  Also set HREG[n] to the hard register.  */

static void
nxu16_fix_operands (rtx *op, rtx *hreg, unsigned opmask, unsigned rmask)
{
  for (; opmask; opmask >>= 1, op++)
    {
      rtx reg = *op;

      if (hreg)
	*hreg = NULL_RTX;

      if ((opmask & 1)
	  && REG_P (reg)
	  && REGNO (reg) < FIRST_PSEUDO_REGISTER
	  // This hard-reg overlaps other prohibited hard regs?
	  && (rmask & regmask (GET_MODE (reg), REGNO (reg))))
	{
	  *op = gen_reg_rtx (GET_MODE (reg));
	  if (hreg == NULL)
	    emit_move_insn (*op, reg);
	  else
	    *hreg = reg;
	}

      if (hreg)
	hreg++;
    }
}


void
nxu16_fix_inputs (rtx *op, unsigned opmask, unsigned rmask)
{
  nxu16_fix_operands (op, NULL, opmask, rmask);
}


/* Helper for the function below:  If bit n of MASK is set and
   HREG[n] != NULL, then emit a move insn to copy OP[n] to HREG[n].
   Otherwise do nothing for that n.  Return TRUE.  */

static bool
nxu16_move_fixed_operands (rtx *op, rtx *hreg, unsigned mask)
{
  for (; mask; mask >>= 1, op++, hreg++)
    if ((mask & 1)
	&& *hreg)
      emit_move_insn (*hreg, *op);

  return true;
}


/* PR63633: The middle-end might come up with hard regs as output operands.

   GEN is a sequence generating function like gen_mulsi3 with 3 operands OP[].
   RMASK is a bit mask representing a subset of hard registers R0...R31:
   Rn is an element of that set iff bit n of RMASK is set.
   OPMASK describes a subset of OP[]:  If bit n of OPMASK is 1 then
   OP[n] has to be fixed; otherwise OP[n] is left alone.

   Emit the insn sequence as generated by GEN() with all elements of OPMASK
   which are hard registers overlapping RMASK replaced by newly created
   pseudo registers.  After the sequence has been emitted, emit insns that
   move the contents of respective pseudos to their hard regs.  */

bool
nxu16_emit3_fix_outputs (rtx (*gen)(rtx,rtx,rtx), rtx *op,
		       unsigned opmask, unsigned rmask)
{
  const int n = 3;
  rtx hreg[n];

  /* It is letigimate for GEN to call this function, and in order not to
     get self-recursive we use the following static kludge.  This is the
     only way not to duplicate all expanders and to avoid ugly and
     hard-to-maintain C-code instead of the much more appreciated RTL
     representation as supplied by define_expand.  */
  static bool lock = false;

  gcc_assert (opmask < (1u << n));

  if (lock)
    return false;

  nxu16_fix_operands (op, hreg, opmask, rmask);

  lock = true;
  emit_insn (gen (op[0], op[1], op[2]));
  lock = false;

  return nxu16_move_fixed_operands (op, hreg, opmask);
}


/* Worker function for cpymemhi expander.
   XOP[0]  Destination as MEM:BLK
   XOP[1]  Source      "     "
   XOP[2]  # Bytes to copy

   Return TRUE  if the expansion is accomplished.
   Return FALSE if the operand compination is not supported.  */

bool
nxu16_emit_cpymemhi (rtx *xop)
{
  machine_mode loop_mode;
  addr_space_t as = MEM_ADDR_SPACE (xop[1]);
  rtx loop_reg, addr1, insn;
  rtx a_hi8 = NULL_RTX;

  if (nxu16_mem_flash_p (xop[0]))
    return false;

  if (!CONST_INT_P (xop[2]))
    return false;

  HOST_WIDE_INT count = INTVAL (xop[2]);
  if (count <= 0)
    return false;

  rtx a_src  = XEXP (xop[1], 0);
  rtx a_dest = XEXP (xop[0], 0);

  if (PSImode == GET_MODE (a_src))
    {
      gcc_assert (as == ADDR_SPACE_MEMX);

      loop_mode = (count < 0x100) ? QImode : HImode;
      loop_reg = gen_rtx_REG (loop_mode, 24);
      emit_move_insn (loop_reg, gen_int_mode (count, loop_mode));

      addr1 = simplify_gen_subreg (HImode, a_src, PSImode, 0);
      a_hi8 = simplify_gen_subreg (QImode, a_src, PSImode, 2);
    }
  else
    {
      int segment = nxu16_addrspace[as].segment;

      if (segment
	  && nxu16_n_flash > 1)
	{
	  a_hi8 = GEN_INT (segment);
	  emit_move_insn (rampz_rtx, a_hi8 = copy_to_mode_reg (QImode, a_hi8));
	}
      else if (!ADDR_SPACE_GENERIC_P (as))
	{
	  as = ADDR_SPACE_FLASH;
	}

      addr1 = a_src;

      loop_mode = (count <= 0x100) ? QImode : HImode;
      loop_reg = copy_to_mode_reg (loop_mode, gen_int_mode (count, loop_mode));
    }

  rtx xas = GEN_INT (as);

  /* FIXME: Register allocator might come up with spill fails if it is left
	on its own.  Thus, we allocate the pointer registers by hand:
	Z = source address
	X = destination address  */

  emit_move_insn (lpm_addr_reg_rtx, addr1);
  emit_move_insn (gen_rtx_REG (HImode, REG_X), a_dest);

  /* FIXME: Register allocator does a bad job and might spill address
	register(s) inside the loop leading to additional move instruction
	to/from stack which could clobber tmp_reg.  Thus, do *not* emit
	load and store as separate insns.  Instead, we perform the copy
	by means of one monolithic insn.  */

  gcc_assert (TMP_REGNO == LPM_REGNO);

  if (as != ADDR_SPACE_MEMX)
    {
      /* Load instruction ([E]LPM or LD) is known at compile time:
	 Do the copy-loop inline.  */

      rtx (*fun) (rtx, rtx, rtx)
	= QImode == loop_mode ? gen_cpymem_qi : gen_cpymem_hi;

      insn = fun (xas, loop_reg, loop_reg);
    }
  else
    {
      rtx (*fun) (rtx, rtx)
	= QImode == loop_mode ? gen_cpymemx_qi : gen_cpymemx_hi;

      emit_move_insn (gen_rtx_REG (QImode, 23), a_hi8);

      insn = fun (xas, GEN_INT (nxu16_addr.rampz));
    }

  set_mem_addr_space (SET_SRC (XVECEXP (insn, 0, 0)), as);
  emit_insn (insn);

  return true;
}


/* Print assembler for cpymem_qi, cpymem_hi insns...
       $0     : Address Space
       $1, $2 : Loop register
       Z      : Source address
       X      : Destination address
*/

const char *
nxu16_out_cpymem (rtx_insn * /*insn*/, rtx *op, int *plen)
{
  addr_space_t as = (addr_space_t) INTVAL (op[0]);
  machine_mode loop_mode = GET_MODE (op[1]);
  bool sbiw_p = nxu16_adiw_reg_p (op[1]);
  rtx xop[3] = { op[0], op[1], tmp_reg_rtx };

  if (plen)
    *plen = 0;

  /* Loop label */

  nxu16_asm_len ("0:", xop, plen, 0);

  /* Load with post-increment */

  switch (as)
    {
    default:
      gcc_unreachable();

    case ADDR_SPACE_GENERIC:

      nxu16_asm_len ("ld %2,Z+", xop, plen, 1);
      break;

    case ADDR_SPACE_FLASH:

      if (nxu16_HAVE_LPMX)
	nxu16_asm_len ("lpm %2,Z+", xop, plen, 1);
      else
	nxu16_asm_len ("lpm" CR_TAB
		     "adiw r30,1", xop, plen, 2);
      break;

    case ADDR_SPACE_FLASH1:
    case ADDR_SPACE_FLASH2:
    case ADDR_SPACE_FLASH3:
    case ADDR_SPACE_FLASH4:
    case ADDR_SPACE_FLASH5:

      if (nxu16_HAVE_ELPMX)
	nxu16_asm_len ("elpm %2,Z+", xop, plen, 1);
      else
	nxu16_asm_len ("elpm" CR_TAB
		     "adiw r30,1", xop, plen, 2);
      break;
    }

  /* Store with post-increment */

  nxu16_asm_len ("st X+,%2", xop, plen, 1);

  /* Decrement loop-counter and set Z-flag */

  if (QImode == loop_mode)
    {
      nxu16_asm_len ("dec %1", xop, plen, 1);
    }
  else if (sbiw_p)
    {
      nxu16_asm_len ("sbiw %1,1", xop, plen, 1);
    }
  else
    {
      nxu16_asm_len ("subi %A1,1" CR_TAB
		   "sbci %B1,0", xop, plen, 2);
    }

  /* Loop until zero */

  return nxu16_asm_len ("brne 0b", xop, plen, 1);
}



/* Helper for __builtin_nxu16_delay_cycles */

static rtx
nxu16_mem_clobber (void)
{
  rtx mem = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (mem) = 1;
  return mem;
}




/* Compute the image of x under f, i.e. perform   x --> f(x)    */

static int
nxu16_map (unsigned int f, int x)
{
  return x < 8 ? (f >> (4 * x)) & 0xf : 0;
}


/* Return some metrics of map A.  */

enum
  {
    /* Number of fixed points in { 0 ... 7 } */
    MAP_FIXED_0_7,

    /* Size of preimage of non-fixed points in { 0 ... 7 } */
    MAP_NONFIXED_0_7,

    /* Mask representing the fixed points in { 0 ... 7 } */
    MAP_MASK_FIXED_0_7,

    /* Size of the preimage of { 0 ... 7 } */
    MAP_PREIMAGE_0_7,

    /* Mask that represents the preimage of { f } */
    MAP_MASK_PREIMAGE_F
  };

static unsigned
nxu16_map_metric (unsigned int a, int mode)
{
  unsigned metric = 0;

  for (unsigned i = 0; i < 8; i++)
    {
      unsigned ai = nxu16_map (a, i);

      if (mode == MAP_FIXED_0_7)
	metric += ai == i;
      else if (mode == MAP_NONFIXED_0_7)
	metric += ai < 8 && ai != i;
      else if (mode == MAP_MASK_FIXED_0_7)
	metric |= ((unsigned) (ai == i)) << i;
      else if (mode == MAP_PREIMAGE_0_7)
	metric += ai < 8;
      else if (mode == MAP_MASK_PREIMAGE_F)
	metric |= ((unsigned) (ai == 0xf)) << i;
      else
	gcc_unreachable();
    }

  return metric;
}


/* Return true if IVAL has a 0xf in its hexadecimal representation
   and false, otherwise.  Only nibbles 0..7 are taken into account.
   Used as constraint helper for C0f and Cxf.  */

bool
nxu16_has_nibble_0xf (rtx ival)
{
  unsigned int map = UINTVAL (ival) & GET_MODE_MASK (SImode);
  return nxu16_map_metric (map, MAP_MASK_PREIMAGE_F) != 0;
}


/* We have a set of bits that are mapped by a function F.
   Try to decompose F by means of a second function G so that

      F = F o G^-1 o G

   and

      cost (F o G^-1) + cost (G)  <  cost (F)

   Example:  Suppose builtin insert_bits supplies us with the map
   F = 0x3210ffff.  Instead of doing 4 bit insertions to get the high
   nibble of the result, we can just as well rotate the bits before inserting
   them and use the map 0x7654ffff which is cheaper than the original map.
   For this example G = G^-1 = 0x32107654 and F o G^-1 = 0x7654ffff.  */

typedef struct
{
  /* tree code of binary function G */
  tree_code code;

  /* The constant second argument of G */
  int arg;

  /* G^-1, the inverse of G (*, arg) */
  unsigned ginv;

  /* The cost of applying G (*, arg) */
  int cost;

  /* The composition F o G^-1 (*, arg) for some function F */
  unsigned int map;

  /* For debug purpose only */
  const char *str;
} nxu16_map_op_t;

static const nxu16_map_op_t nxu16_map_op[] =
  {
    { LROTATE_EXPR, 0, 0x76543210, 0, 0, "id" },
    { LROTATE_EXPR, 1, 0x07654321, 2, 0, "<<<" },
    { LROTATE_EXPR, 2, 0x10765432, 4, 0, "<<<" },
    { LROTATE_EXPR, 3, 0x21076543, 4, 0, "<<<" },
    { LROTATE_EXPR, 4, 0x32107654, 1, 0, "<<<" },
    { LROTATE_EXPR, 5, 0x43210765, 3, 0, "<<<" },
    { LROTATE_EXPR, 6, 0x54321076, 5, 0, "<<<" },
    { LROTATE_EXPR, 7, 0x65432107, 3, 0, "<<<" },
    { RSHIFT_EXPR, 1, 0x6543210c, 1, 0, ">>" },
    { RSHIFT_EXPR, 1, 0x7543210c, 1, 0, ">>" },
    { RSHIFT_EXPR, 2, 0x543210cc, 2, 0, ">>" },
    { RSHIFT_EXPR, 2, 0x643210cc, 2, 0, ">>" },
    { RSHIFT_EXPR, 2, 0x743210cc, 2, 0, ">>" },
    { LSHIFT_EXPR, 1, 0xc7654321, 1, 0, "<<" },
    { LSHIFT_EXPR, 2, 0xcc765432, 2, 0, "<<" }
  };


/* Try to decompose F as F = (F o G^-1) o G as described above.
   The result is a struct representing F o G^-1 and G.
   If result.cost < 0 then such a decomposition does not exist.  */

static nxu16_map_op_t
nxu16_map_decompose (unsigned int f, const nxu16_map_op_t *g, bool val_const_p)
{
  bool val_used_p = nxu16_map_metric (f, MAP_MASK_PREIMAGE_F) != 0;
  nxu16_map_op_t f_ginv = *g;
  unsigned int ginv = g->ginv;

  f_ginv.cost = -1;

  /* Step 1:  Computing F o G^-1  */

  for (int i = 7; i >= 0; i--)
    {
      int x = nxu16_map (f, i);

      if (x <= 7)
	{
	  x = nxu16_map (ginv, x);

	  /* The bit is no element of the image of G: no avail (cost = -1)  */

	  if (x > 7)
	    return f_ginv;
	}

      f_ginv.map = (f_ginv.map << 4) + x;
    }

  /* Step 2:  Compute the cost of the operations.
     The overall cost of doing an operation prior to the insertion is
      the cost of the insertion plus the cost of the operation.  */

  /* Step 2a:  Compute cost of F o G^-1  */

  if (nxu16_map_metric (f_ginv.map, MAP_NONFIXED_0_7) == 0)
    /* The mapping consists only of fixed points and can be folded
       to AND/OR logic in the remainder.  Reasonable cost is 3. */
    f_ginv.cost = 2 + (val_used_p && !val_const_p);
  else
    {
      rtx xop[4];

      /* Get the cost of the insn by calling the output worker with some
	 fake values.  Mimic effect of reloading xop[3]: Unused operands
	 are mapped to 0 and used operands are reloaded to xop[0].  */

      xop[0] = all_regs_rtx[REG_24];
      xop[1] = gen_int_mode (f_ginv.map, SImode);
      xop[2] = all_regs_rtx[REG_25];
      xop[3] = val_used_p ? xop[0] : const0_rtx;

      nxu16_out_insert_bits (xop, &f_ginv.cost);

      f_ginv.cost += val_const_p && val_used_p ? 1 : 0;
    }

  /* Step 2b:  Add cost of G  */

  f_ginv.cost += g->cost;

  if (nxu16_log.builtin)
    nxu16_edump (" %s%d=%d", g->str, g->arg, f_ginv.cost);

  return f_ginv;
}


/* Insert bits from XOP[1] into XOP[0] according to MAP.
   XOP[0] and XOP[1] don't overlap.
   If FIXP_P = true:  Move all bits according to MAP using BLD/BST sequences.
   If FIXP_P = false: Just move the bit if its position in the destination
   is different to its source position.  */

static void
nxu16_move_bits (rtx *xop, unsigned int map, bool fixp_p, int *plen)
{
  /* T-flag contains this bit of the source, i.e. of XOP[1]  */
  int t_bit_src = -1;

  /* We order the operations according to the requested source bit b.  */

  for (int b = 0; b < 8; b++)
    for (int bit_dest = 0; bit_dest < 8; bit_dest++)
      {
	int bit_src = nxu16_map (map, bit_dest);

	if (b != bit_src
	    || bit_src >= 8
	    /* Same position: No need to copy as requested by FIXP_P.  */
	    || (bit_dest == bit_src && !fixp_p))
	  continue;

	if (t_bit_src != bit_src)
	  {
	    /* Source bit is not yet in T: Store it to T.  */

	    t_bit_src = bit_src;

	    xop[3] = GEN_INT (bit_src);
	    nxu16_asm_len ("bst %T1%T3", xop, plen, 1);
	  }

	/* Load destination bit with T.  */

	xop[3] = GEN_INT (bit_dest);
	nxu16_asm_len ("bld %T0%T3", xop, plen, 1);
      }
}


/* PLEN == 0: Print assembler code for `insert_bits'.
   PLEN != 0: Compute code length in bytes.

   OP[0]:  Result
   OP[1]:  The mapping composed of nibbles. If nibble no. N is
	   0:   Bit N of result is copied from bit OP[2].0
	   ...  ...
	   7:   Bit N of result is copied from bit OP[2].7
	   0xf: Bit N of result is copied from bit OP[3].N
   OP[2]:  Bits to be inserted
   OP[3]:  Target value  */

const char *
nxu16_out_insert_bits (rtx *op, int *plen)
{
  unsigned int map = UINTVAL (op[1]) & GET_MODE_MASK (SImode);
  bool fixp_p = true;
  rtx xop[4];

  xop[0] = op[0];
  xop[1] = op[2];
  xop[2] = op[3];

  gcc_assert (REG_P (xop[2]) || CONST_INT_P (xop[2]));

  if (plen)
    *plen = 0;
  else if (flag_print_asm_name)
    fprintf (asm_out_file, ASM_COMMENT_START "map = 0x%08x\n", map);

  /* If MAP has fixed points it might be better to initialize the result
     with the bits to be inserted instead of moving all bits by hand.  */

  unsigned mask_fixed = nxu16_map_metric (map, MAP_MASK_FIXED_0_7);

  if (REGNO (xop[0]) == REGNO (xop[1]))
    {
      /* Avoid early-clobber conflicts */

      nxu16_asm_len ("mov __tmp_reg__,%1", xop, plen, 1);
      xop[1] = tmp_reg_rtx;
      fixp_p = false;
    }

  if (nxu16_map_metric (map, MAP_MASK_PREIMAGE_F))
    {
      /* XOP[2] is used and reloaded to XOP[0] already */

      int n_fix = 0, n_nofix = 0;

      gcc_assert (REG_P (xop[2]));

      /* Get the code size of the bit insertions; once with all bits
	 moved and once with fixed points omitted.  */

      nxu16_move_bits (xop, map, true, &n_fix);
      nxu16_move_bits (xop, map, false, &n_nofix);

      if (fixp_p && n_fix - n_nofix > 3)
	{
	  xop[3] = gen_int_mode (~mask_fixed, QImode);

	  nxu16_asm_len ("eor %0,%1"   CR_TAB
		       "andi %0,%3"  CR_TAB
		       "eor %0,%1", xop, plen, 3);
	  fixp_p = false;
	}
    }
  else
    {
      /* XOP[2] is unused */

      if (fixp_p && mask_fixed)
	{
	  nxu16_asm_len ("mov %0,%1", xop, plen, 1);
	  fixp_p = false;
	}
    }

  /* Move/insert remaining bits.  */

  nxu16_move_bits (xop, map, fixp_p, plen);

  return "";
}


/* IDs for all the nxu16 builtins.  */

enum nxu16_builtin_id
  {
#define DEF_BUILTIN(NAME, N_ARGS, TYPE, CODE, LIBNAME)  \
    nxu16_BUILTIN_ ## NAME,
#include "builtins.def"
#undef DEF_BUILTIN

    nxu16_BUILTIN_COUNT
  };

struct GTY(()) nxu16_builtin_description
{
  enum insn_code icode;
  int n_args;
  tree fndecl;
};


/* Notice that nxu16_bdesc[] and nxu16_builtin_id are initialized in such a way
   that a built-in's ID can be used to access the built-in by means of
   nxu16_bdesc[ID]  */

static GTY(()) nxu16_builtin_description
nxu16_bdesc[nxu16_BUILTIN_COUNT] =
  {
#define DEF_BUILTIN(NAME, N_ARGS, TYPE, ICODE, LIBNAME)         \
    { (enum insn_code) CODE_FOR_ ## ICODE, N_ARGS, NULL_TREE },
#include "builtins.def"
#undef DEF_BUILTIN
  };


/* Implement `TARGET_BUILTIN_DECL'.  */

static tree
nxu16_builtin_decl (unsigned id, bool /*initialize_p*/)
{
  if (id < nxu16_BUILTIN_COUNT)
    return nxu16_bdesc[id].fndecl;

  return error_mark_node;
}


static void
nxu16_init_builtin_int24 (void)
{
  tree int24_type  = make_signed_type (GET_MODE_BITSIZE (PSImode));
  tree uint24_type = make_unsigned_type (GET_MODE_BITSIZE (PSImode));

  lang_hooks.types.register_builtin_type (int24_type, "__int24");
  lang_hooks.types.register_builtin_type (uint24_type, "__uint24");
}


/* Implement `TARGET_INIT_BUILTINS' */
/* Set up all builtin functions for this target.  */

static void
nxu16_init_builtins (void)
{
  tree void_ftype_void
    = build_function_type_list (void_type_node, NULL_TREE);
  tree uintQI_ftype_uintQI
    = build_function_type_list (unsigned_intQI_type_node,
				unsigned_intQI_type_node,
				NULL_TREE);
  tree uintQI_ftype_uintQI_uintQI
    = build_function_type_list (unsigned_intQI_type_node,
				unsigned_intQI_type_node,
				unsigned_intQI_type_node,
				NULL_TREE);
  tree uintHI_ftype_uintQI_uintQI
    = build_function_type_list (unsigned_intHI_type_node,
				unsigned_intQI_type_node,
				unsigned_intQI_type_node,
				NULL_TREE);
  tree intHI_ftype_intQI_intQI
    = build_function_type_list (intHI_type_node,
				intQI_type_node,
				intQI_type_node,
				NULL_TREE);
  tree intHI_ftype_intQI_uintQI
    = build_function_type_list (intHI_type_node,
				intQI_type_node,
				unsigned_intQI_type_node,
				NULL_TREE);
  tree void_ftype_uintSI
    = build_function_type_list (void_type_node,
				unsigned_intSI_type_node,
				NULL_TREE);

  tree uintQI_ftype_uintSI_uintQI_uintQI
    = build_function_type_list (unsigned_intQI_type_node,
				unsigned_intSI_type_node,
				unsigned_intQI_type_node,
				unsigned_intQI_type_node,
				NULL_TREE);

  tree const_memx_void_node
    = build_qualified_type (void_type_node,
			    TYPE_QUAL_CONST
			    | ENCODE_QUAL_ADDR_SPACE (ADDR_SPACE_MEMX));

  tree const_memx_ptr_type_node
    = build_pointer_type_for_mode (const_memx_void_node, PSImode, false);

  tree intQI_ftype_const_memx_ptr
    = build_function_type_list (intQI_type_node,
				const_memx_ptr_type_node,
				NULL);

#define ITYP(T)                                                         \
  lang_hooks.types.type_for_size (TYPE_PRECISION (T), TYPE_UNSIGNED (T))

#define FX_FTYPE_FX(fx)                                                 \
  tree fx##r_ftype_##fx##r                                              \
    = build_function_type_list (node_##fx##r, node_##fx##r, NULL);      \
  tree fx##k_ftype_##fx##k                                              \
    = build_function_type_list (node_##fx##k, node_##fx##k, NULL)

#define FX_FTYPE_FX_INT(fx)                                             \
  tree fx##r_ftype_##fx##r_int                                          \
    = build_function_type_list (node_##fx##r, node_##fx##r,             \
                                integer_type_node, NULL);               \
  tree fx##k_ftype_##fx##k_int                                          \
    = build_function_type_list (node_##fx##k, node_##fx##k,             \
                                integer_type_node, NULL)

#define INT_FTYPE_FX(fx)                                                \
  tree int_ftype_##fx##r                                                \
    = build_function_type_list (integer_type_node, node_##fx##r, NULL); \
  tree int_ftype_##fx##k                                                \
    = build_function_type_list (integer_type_node, node_##fx##k, NULL)

#define INTX_FTYPE_FX(fx)                                               \
  tree int##fx##r_ftype_##fx##r                                         \
    = build_function_type_list (ITYP (node_##fx##r), node_##fx##r, NULL); \
  tree int##fx##k_ftype_##fx##k                                         \
    = build_function_type_list (ITYP (node_##fx##k), node_##fx##k, NULL)

#define FX_FTYPE_INTX(fx)                                               \
  tree fx##r_ftype_int##fx##r                                           \
    = build_function_type_list (node_##fx##r, ITYP (node_##fx##r), NULL); \
  tree fx##k_ftype_int##fx##k                                           \
    = build_function_type_list (node_##fx##k, ITYP (node_##fx##k), NULL)

  tree node_hr = short_fract_type_node;
  tree node_nr = fract_type_node;
  tree node_lr = long_fract_type_node;
  tree node_llr = long_long_fract_type_node;

  tree node_uhr = unsigned_short_fract_type_node;
  tree node_unr = unsigned_fract_type_node;
  tree node_ulr = unsigned_long_fract_type_node;
  tree node_ullr = unsigned_long_long_fract_type_node;

  tree node_hk = short_accum_type_node;
  tree node_nk = accum_type_node;
  tree node_lk = long_accum_type_node;
  tree node_llk = long_long_accum_type_node;

  tree node_uhk = unsigned_short_accum_type_node;
  tree node_unk = unsigned_accum_type_node;
  tree node_ulk = unsigned_long_accum_type_node;
  tree node_ullk = unsigned_long_long_accum_type_node;


  /* For absfx builtins.  */

  FX_FTYPE_FX (h);
  FX_FTYPE_FX (n);
  FX_FTYPE_FX (l);
  FX_FTYPE_FX (ll);

  /* For roundfx builtins.  */

  FX_FTYPE_FX_INT (h);
  FX_FTYPE_FX_INT (n);
  FX_FTYPE_FX_INT (l);
  FX_FTYPE_FX_INT (ll);

  FX_FTYPE_FX_INT (uh);
  FX_FTYPE_FX_INT (un);
  FX_FTYPE_FX_INT (ul);
  FX_FTYPE_FX_INT (ull);

  /* For countlsfx builtins.  */

  INT_FTYPE_FX (h);
  INT_FTYPE_FX (n);
  INT_FTYPE_FX (l);
  INT_FTYPE_FX (ll);

  INT_FTYPE_FX (uh);
  INT_FTYPE_FX (un);
  INT_FTYPE_FX (ul);
  INT_FTYPE_FX (ull);

  /* For bitsfx builtins.  */

  INTX_FTYPE_FX (h);
  INTX_FTYPE_FX (n);
  INTX_FTYPE_FX (l);
  INTX_FTYPE_FX (ll);

  INTX_FTYPE_FX (uh);
  INTX_FTYPE_FX (un);
  INTX_FTYPE_FX (ul);
  INTX_FTYPE_FX (ull);

  /* For fxbits builtins.  */

  FX_FTYPE_INTX (h);
  FX_FTYPE_INTX (n);
  FX_FTYPE_INTX (l);
  FX_FTYPE_INTX (ll);

  FX_FTYPE_INTX (uh);
  FX_FTYPE_INTX (un);
  FX_FTYPE_INTX (ul);
  FX_FTYPE_INTX (ull);


#define DEF_BUILTIN(NAME, N_ARGS, TYPE, CODE, LIBNAME)                  \
  {                                                                     \
    int id = nxu16_BUILTIN_ ## NAME;                                      \
    const char *Name = "__builtin_nxu16_" #NAME;                          \
    char *name = (char *) alloca (1 + strlen (Name));                   \
                                                                        \
    gcc_assert (id < nxu16_BUILTIN_COUNT);                                \
    nxu16_bdesc[id].fndecl                                                \
      = add_builtin_function (nxu16_tolower (name, Name), TYPE, id,       \
			      BUILT_IN_MD, LIBNAME, NULL_TREE);         \
  }
#include "builtins.def"
#undef DEF_BUILTIN

  nxu16_init_builtin_int24 ();
}


/* Subroutine of nxu16_expand_builtin to expand vanilla builtins
   with non-void result and 1 ... 3 arguments.  */

static rtx
nxu16_default_expand_builtin (enum insn_code icode, tree exp, rtx target)
{
  rtx pat, xop[3];
  int n_args = call_expr_nargs (exp);
  machine_mode tmode = insn_data[icode].operand[0].mode;

  gcc_assert (n_args >= 1 && n_args <= 3);

  if (target == NULL_RTX
      || GET_MODE (target) != tmode
      || !insn_data[icode].operand[0].predicate (target, tmode))
    {
      target = gen_reg_rtx (tmode);
    }

  for (int n = 0; n < n_args; n++)
    {
      tree arg = CALL_EXPR_ARG (exp, n);
      rtx op = expand_expr (arg, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      machine_mode opmode = GET_MODE (op);
      machine_mode mode = insn_data[icode].operand[n + 1].mode;

      if ((opmode == SImode || opmode == VOIDmode) && mode == HImode)
	{
	  opmode = HImode;
	  op = gen_lowpart (HImode, op);
	}

      /* In case the insn wants input operands in modes different from
	 the result, abort.  */

      gcc_assert (opmode == mode || opmode == VOIDmode);

      if (!insn_data[icode].operand[n + 1].predicate (op, mode))
	op = copy_to_mode_reg (mode, op);

      xop[n] = op;
    }

  switch (n_args)
    {
    case 1: pat = GEN_FCN (icode) (target, xop[0]); break;
    case 2: pat = GEN_FCN (icode) (target, xop[0], xop[1]); break;
    case 3: pat = GEN_FCN (icode) (target, xop[0], xop[1], xop[2]); break;

    default:
      gcc_unreachable();
    }

  if (pat == NULL_RTX)
    return NULL_RTX;

  emit_insn (pat);

  return target;
}


/* Implement `TARGET_EXPAND_BUILTIN'.  */
/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */


/* Helper for `nxu16_fold_builtin' that folds  absfx (FIXED_CST).  */

static tree
nxu16_fold_absfx (tree tval)
{
  if (FIXED_CST != TREE_CODE (tval))
    return NULL_TREE;

  /* Our fixed-points have no padding:  Use double_int payload directly.  */

  FIXED_VALUE_TYPE fval = TREE_FIXED_CST (tval);
  unsigned int bits = GET_MODE_BITSIZE (fval.mode);
  double_int ival = fval.data.sext (bits);

  if (!ival.is_negative())
    return tval;

  /* ISO/IEC TR 18037, 7.18a.6.2:  The absfx functions are saturating.  */

  fval.data = (ival == double_int::min_value (bits, false).sext (bits))
    ? double_int::max_value (bits, false)
    : -ival;

  return build_fixed (TREE_TYPE (tval), fval);
}


/* Implement `TARGET_FOLD_BUILTIN'.  */

static tree
nxu16_fold_builtin (tree fndecl, int /*n_args*/, tree *arg, bool /*ignore*/)
{
  unsigned int fcode = DECL_MD_FUNCTION_CODE (fndecl);
  tree val_type = TREE_TYPE (TREE_TYPE (fndecl));

  if (!optimize)
    return NULL_TREE;

  switch (fcode)
    {
    default:
      break;

    case nxu16_BUILTIN_SWAP:
      {
	return fold_build2 (LROTATE_EXPR, val_type, arg[0],
			    build_int_cst (val_type, 4));
      }

    case nxu16_BUILTIN_ABSHR:
    case nxu16_BUILTIN_ABSR:
    case nxu16_BUILTIN_ABSLR:
    case nxu16_BUILTIN_ABSLLR:

    case nxu16_BUILTIN_ABSHK:
    case nxu16_BUILTIN_ABSK:
    case nxu16_BUILTIN_ABSLK:
    case nxu16_BUILTIN_ABSLLK:
      /* GCC is not good with folding ABS for fixed-point.  Do it by hand.  */

      return nxu16_fold_absfx (arg[0]);

    case nxu16_BUILTIN_BITSHR:    case nxu16_BUILTIN_HRBITS:
    case nxu16_BUILTIN_BITSHK:    case nxu16_BUILTIN_HKBITS:
    case nxu16_BUILTIN_BITSUHR:   case nxu16_BUILTIN_UHRBITS:
    case nxu16_BUILTIN_BITSUHK:   case nxu16_BUILTIN_UHKBITS:

    case nxu16_BUILTIN_BITSR:     case nxu16_BUILTIN_RBITS:
    case nxu16_BUILTIN_BITSK:     case nxu16_BUILTIN_KBITS:
    case nxu16_BUILTIN_BITSUR:    case nxu16_BUILTIN_URBITS:
    case nxu16_BUILTIN_BITSUK:    case nxu16_BUILTIN_UKBITS:

    case nxu16_BUILTIN_BITSLR:    case nxu16_BUILTIN_LRBITS:
    case nxu16_BUILTIN_BITSLK:    case nxu16_BUILTIN_LKBITS:
    case nxu16_BUILTIN_BITSULR:   case nxu16_BUILTIN_ULRBITS:
    case nxu16_BUILTIN_BITSULK:   case nxu16_BUILTIN_ULKBITS:

    case nxu16_BUILTIN_BITSLLR:   case nxu16_BUILTIN_LLRBITS:
    case nxu16_BUILTIN_BITSLLK:   case nxu16_BUILTIN_LLKBITS:
    case nxu16_BUILTIN_BITSULLR:  case nxu16_BUILTIN_ULLRBITS:
    case nxu16_BUILTIN_BITSULLK:  case nxu16_BUILTIN_ULLKBITS:

      gcc_assert (TYPE_PRECISION (val_type)
		  == TYPE_PRECISION (TREE_TYPE (arg[0])));

      return build1 (VIEW_CONVERT_EXPR, val_type, arg[0]);

    case nxu16_BUILTIN_MASK1:
      {
	tree tmask = arg[0];
	tree toffs = arg[1];

	if (TREE_CODE (tmask) == INTEGER_CST
	    && TREE_CODE (toffs) == INTEGER_CST)
	  {
	    switch (0xff & TREE_INT_CST_LOW (tmask))
	      {
	      case 0x01:
	      case 0xfe:
		return fold_build2 (LROTATE_EXPR, val_type, arg[0], toffs);

	      case 0x80:
	      case 0x7f:
		return fold_build2 (RROTATE_EXPR, val_type, arg[0], toffs);
	      }
	  }
	break;
      } // nxu16_BUILTIN_MASK1

    case nxu16_BUILTIN_INSERT_BITS:
      {
	tree tbits = arg[1];
	tree tval = arg[2];
	tree map_type = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (fndecl)));
	bool changed = false;
	nxu16_map_op_t best_g;

	if (TREE_CODE (arg[0]) != INTEGER_CST)
	  {
	    /* No constant as first argument: Don't fold this and run into
	       error in nxu16_expand_builtin.  */

	    break;
	  }

	tree tmap = wide_int_to_tree (map_type, wi::to_wide (arg[0]));
	unsigned int map = TREE_INT_CST_LOW (tmap);

	if (TREE_CODE (tval) != INTEGER_CST
	    && nxu16_map_metric (map, MAP_MASK_PREIMAGE_F) == 0)
	  {
	    /* There are no F in the map, i.e. 3rd operand is unused.
	       Replace that argument with some constant to render
	       respective input unused.  */

	    tval = build_int_cst (val_type, 0);
	    changed = true;
	  }

	if (TREE_CODE (tbits) != INTEGER_CST
	    && nxu16_map_metric (map, MAP_PREIMAGE_0_7) == 0)
	  {
	    /* Similar for the bits to be inserted. If they are unused,
	       we can just as well pass 0.  */

	    tbits = build_int_cst (val_type, 0);
	  }

	if (TREE_CODE (tbits) == INTEGER_CST)
	  {
	    /* Inserting bits known at compile time is easy and can be
	       performed by AND and OR with appropriate masks.  */

	    int bits = TREE_INT_CST_LOW (tbits);
	    int mask_ior = 0, mask_and = 0xff;

	    for (size_t i = 0; i < 8; i++)
	      {
		int mi = nxu16_map (map, i);

		if (mi < 8)
		  {
		    if (bits & (1 << mi))     mask_ior |=  (1 << i);
		    else		      mask_and &= ~(1 << i);
		  }
	      }

	    tval = fold_build2 (BIT_IOR_EXPR, val_type, tval,
				build_int_cst (val_type, mask_ior));
	    return fold_build2 (BIT_AND_EXPR, val_type, tval,
				build_int_cst (val_type, mask_and));
	  }

	if (changed)
	  return build_call_expr (fndecl, 3, tmap, tbits, tval);

	/* If bits don't change their position, we can use vanilla logic
	   to merge the two arguments...  */

	if (nxu16_map_metric (map, MAP_NONFIXED_0_7) == 0
	    // ...except when we are copying just one bit. In that
	    // case, BLD/BST is better than XOR/AND/XOR, see PR90622.
	    && nxu16_map_metric (map, MAP_FIXED_0_7) != 1)
	  {
	    int mask_f = nxu16_map_metric (map, MAP_MASK_PREIMAGE_F);
	    tree tres, tmask = build_int_cst (val_type, mask_f ^ 0xff);

	    tres = fold_build2 (BIT_XOR_EXPR, val_type, tbits, tval);
	    tres = fold_build2 (BIT_AND_EXPR, val_type, tres, tmask);
	    return fold_build2 (BIT_XOR_EXPR, val_type, tres, tval);
	  }

	/* Try to decomposing map to reduce overall cost.  */

	if (nxu16_log.builtin)
	  nxu16_edump ("\n%?: %x\n%?: ROL cost: ", map);

	best_g = nxu16_map_op[0];
	best_g.cost = 1000;

	for (size_t i = 0; i < ARRAY_SIZE (nxu16_map_op); i++)
	  {
	    nxu16_map_op_t g
	      = nxu16_map_decompose (map, nxu16_map_op + i,
				   TREE_CODE (tval) == INTEGER_CST);

	    if (g.cost >= 0 && g.cost < best_g.cost)
	      best_g = g;
	  }

	if (nxu16_log.builtin)
	  nxu16_edump ("\n");

	if (best_g.arg == 0)
	  /* No optimization found */
	  break;

	/* Apply operation G to the 2nd argument.  */

	if (nxu16_log.builtin)
	  nxu16_edump ("%?: using OP(%s%d, %x) cost %d\n",
		     best_g.str, best_g.arg, best_g.map, best_g.cost);

	/* Do right-shifts arithmetically: They copy the MSB instead of
	   shifting in a non-usable value (0) as with logic right-shift.  */

	tbits = fold_convert (signed_char_type_node, tbits);
	tbits = fold_build2 (best_g.code, signed_char_type_node, tbits,
			     build_int_cst (val_type, best_g.arg));
	tbits = fold_convert (val_type, tbits);

	/* Use map o G^-1 instead of original map to undo the effect of G.  */

	tmap = wide_int_to_tree (map_type, best_g.map);

	return build_call_expr (fndecl, 3, tmap, tbits, tval);
      } /* nxu16_BUILTIN_INSERT_BITS */
    }

  return NULL_TREE;
}


/* Implement `TARGET_MD_ASM_ADJUST'.  */
/* Prepend to CLOBBERS hard registers that are automatically clobbered
   for an asm. We do this for CC_REGNUM to maintain source compatibility
   with the original cc0-based compiler.  */



/* Implement `TARGET_C_MODE_FOR_FLOATING_TYPE'.  Return SFmode or DFmode
   for TI_{LONG_,}DOUBLE_TYPE which is for {long,} double type, go with
   the default one for the others.  */

static machine_mode
nxu16_c_mode_for_floating_type (tree_index ti)
{
  if (ti == TI_DOUBLE_TYPE)
    return nxu16_double == 32 ? SFmode : DFmode;
  if (ti == TI_LONG_DOUBLE_TYPE)
    return nxu16_long_double == 32 ? SFmode : DFmode;
  return default_mode_for_floating_type (ti);
}


/* Worker function for `FLOAT_LIB_COMPARE_RETURNS_BOOL'.  */

bool
nxu16_float_lib_compare_returns_bool (machine_mode mode, rtx_code)
{
  if (mode == DFmode)
    {
#if WITH_DOUBLE_COMPARISON == 2
      return true;
#endif
    }

  // This is the GCC default and also what nxu16-LibC implements.
  return false;
}


/* Implement `TARGET_LRA_P'.  */

static bool
nxu16_use_lra_p ()
{
  return nxu16_lra_p;
}


void
nxu16_cpu_cpp_builtins (cpp_reader *pfile)
{
  builtin_define_std ("nxu16");

}

/* Initialize the GCC target structure.  */

#undef  TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.word\t"
#undef  TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.long\t"
#undef  TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP "\t.word\t"
#undef  TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP "\t.long\t"
#undef  TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER nxu16_assemble_integer
#undef  TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START nxu16_file_start
#undef  TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END nxu16_file_end

/*
#undef  TARGET_ASM_FUNCTION_END_PROLOGUE
#define TARGET_ASM_FUNCTION_END_PROLOGUE nxu16_asm_function_end_prologue
#undef  TARGET_ASM_FUNCTION_BEGIN_EPILOGUE
#define TARGET_ASM_FUNCTION_BEGIN_EPILOGUE nxu16_asm_function_begin_epilogue
*/

#undef  TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE nxu16_function_value
#undef  TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE nxu16_libcall_value
#undef  TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P nxu16_function_value_regno_p



#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE nxu16_attribute_table

/*
#undef  TARGET_INSERT_ATTRIBUTES
#define TARGET_INSERT_ATTRIBUTES nxu16_insert_attributes
#undef  TARGET_SECTION_TYPE_FLAGS
#define TARGET_SECTION_TYPE_FLAGS nxu16_section_type_flags

#undef  TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION nxu16_asm_named_section
#undef  TARGET_ASM_INIT_SECTIONS
#define TARGET_ASM_INIT_SECTIONS nxu16_asm_init_sections
#undef  TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO nxu16_encode_section_info
#undef  TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION nxu16_asm_select_section

#undef  TARGET_ASM_FINAL_POSTSCAN_INSN
#define TARGET_ASM_FINAL_POSTSCAN_INSN nxu16_asm_final_postscan_insn
*/

/*
#undef  TARGET_INSN_COST
#define TARGET_INSN_COST nxu16_insn_cost
#undef  TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST nxu16_register_move_cost
#undef  TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST nxu16_memory_move_cost
#undef  TARGET_RTX_COSTS
#define TARGET_RTX_COSTS nxu16_rtx_costs
#undef  TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST nxu16_address_cost
*/


#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG nxu16_function_arg
#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE nxu16_function_arg_advance

#undef  TARGET_SET_CURRENT_FUNCTION
#define TARGET_SET_CURRENT_FUNCTION nxu16_set_current_function

#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY nxu16_return_in_memory

/*
#undef  TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING hook_bool_CUMULATIVE_ARGS_true

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE nxu16_conditional_register_usage

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS nxu16_hard_regno_nregs

#undef  TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK nxu16_hard_regno_mode_ok
#undef  TARGET_HARD_REGNO_SCRATCH_OK
#define TARGET_HARD_REGNO_SCRATCH_OK nxu16_hard_regno_scratch_ok
#undef  TARGET_HARD_REGNO_CALL_PART_CLOBBERED
#define TARGET_HARD_REGNO_CALL_PART_CLOBBERED nxu16_hard_regno_call_part_clobbered

*/


/*
#undef  TARGET_CASE_VALUES_THRESHOLD
#define TARGET_CASE_VALUES_THRESHOLD nxu16_case_values_threshold

#undef  TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED nxu16_frame_pointer_required_p
#undef  TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE nxu16_can_eliminate

#undef  TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS
#define TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS nxu16_allocate_stack_slots_for_args
*//*
#undef TARGET_WARN_FUNC_RETURN
#define TARGET_WARN_FUNC_RETURN nxu16_warn_func_return

#undef  TARGET_CLASS_LIKELY_SPILLED_P
#define TARGET_CLASS_LIKELY_SPILLED_P nxu16_class_likely_spilled_p
*/

/*#undef  TARGET_CLASS_MAX_NREGS
#define TARGET_CLASS_MAX_NREGS nxu16_class_max_nregs
*/
#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE nxu16_option_override

/*
#undef  TARGET_CANNOT_MODIFY_JUMPS_P
#define TARGET_CANNOT_MODIFY_JUMPS_P nxu16_cannot_modify_jumps_p

#undef  TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL nxu16_function_ok_for_sibcall

#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS nxu16_init_builtins

#undef  TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL nxu16_builtin_decl
*/
/*
#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN nxu16_expand_builtin

#undef  TARGET_FOLD_BUILTIN
#define TARGET_FOLD_BUILTIN nxu16_fold_builtin

#undef  TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P nxu16_scalar_mode_supported_p

#undef  TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST nxu16_build_builtin_va_list

#undef  TARGET_FIXED_POINT_SUPPORTED_P
#define TARGET_FIXED_POINT_SUPPORTED_P hook_bool_void_true

#undef  TARGET_CONVERT_TO_TYPE
#define TARGET_CONVERT_TO_TYPE nxu16_convert_to_type

#undef TARGET_LRA_P
#define TARGET_LRA_P nxu16_use_lra_p

#undef  TARGET_ADDR_SPACE_SUBSET_P
#define TARGET_ADDR_SPACE_SUBSET_P nxu16_addr_space_subset_p

#undef  TARGET_ADDR_SPACE_CONVERT
#define TARGET_ADDR_SPACE_CONVERT nxu16_addr_space_convert
*/




/*
#undef  TARGET_ADDR_SPACE_ADDRESS_MODE
#define TARGET_ADDR_SPACE_ADDRESS_MODE nxu16_addr_space_address_mode

#undef  TARGET_ADDR_SPACE_POINTER_MODE
#define TARGET_ADDR_SPACE_POINTER_MODE nxu16_addr_space_pointer_mode
*/
#undef  TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P
#define TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P  nxu16_addr_space_legitimate_address_p

#undef  TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS
#define TARGET_ADDR_SPACE_LEGITIMIZE_ADDRESS nxu16_addr_space_legitimize_address
/*
#undef  TARGET_ADDR_SPACE_DIAGNOSE_USAGE
#define TARGET_ADDR_SPACE_DIAGNOSE_USAGE nxu16_addr_space_diagnose_usage

#undef  TARGET_ADDR_SPACE_ZERO_ADDRESS_VALID
#define TARGET_ADDR_SPACE_ZERO_ADDRESS_VALID nxu16_addr_space_zero_address_valid
*/

/*
#undef  TARGET_MODE_DEPENDENT_ADDRESS_P
#define TARGET_MODE_DEPENDENT_ADDRESS_P nxu16_mode_dependent_address_p
*/


#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND nxu16_print_operand
#undef  TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS nxu16_print_operand_address

/*#undef  TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P nxu16_print_operand_punct_valid_p
*/


/*
#undef TARGET_USE_BY_PIECES_INFRASTRUCTURE_P
#define TARGET_USE_BY_PIECES_INFRASTRUCTURE_P   nxu16_use_by_pieces_infrastructure_p

#undef  TARGET_LEGITIMATE_COMBINED_INSN
#define TARGET_LEGITIMATE_COMBINED_INSN nxu16_legitimate_combined_insn

#undef  TARGET_STARTING_FRAME_OFFSET
#define TARGET_STARTING_FRAME_OFFSET nxu16_starting_frame_offset

#undef  TARGET_MD_ASM_ADJUST
#define TARGET_MD_ASM_ADJUST nxu16_md_asm_adjust

#undef  TARGET_CAN_INLINE_P
#define TARGET_CAN_INLINE_P nxu16_can_inline_p

#undef  TARGET_CANONICALIZE_COMPARISON
#define TARGET_CANONICALIZE_COMPARISON nxu16_canonicalize_comparison
*/

/* According to the opening comment in PR86772, the following applies:
  "If the port does not (and never will in the future) need to mitigate
   against unsafe speculation."  */
   
   /*
#undef  TARGET_HAVE_SPECULATION_SAFE_VALUE
#define TARGET_HAVE_SPECULATION_SAFE_VALUE speculation_safe_value_not_needed

#undef TARGET_C_MODE_FOR_FLOATING_TYPE
#define TARGET_C_MODE_FOR_FLOATING_TYPE nxu16_c_mode_for_floating_type*/

gcc_target targetm = TARGET_INITIALIZER;


#include "gt-nxu16.h"
