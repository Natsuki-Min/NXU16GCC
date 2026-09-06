/* Prototypes for tm_p.h for nxu16 8-bit microcontrollers.
   Copyright (C) 2000-2024 Free Software Foundation, Inc.
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


extern int nxu16_function_arg_regno_p (int r);
extern void nxu16_cpu_cpp_builtins (cpp_reader * pfile);
extern enum reg_class nxu16_regno_reg_class (int r);
extern void asm_globalize_label (FILE *file, const char *name);
extern void nxu16_adjust_reg_alloc_order (void);
extern int nxu16_initial_elimination_offset (int from, int to);
extern int nxu16_simple_epilogue (void);
extern int nxu16_hard_regno_rename_ok (unsigned int, unsigned int);
extern rtx nxu16_return_addr_rtx (int count, rtx tem);
extern void nxu16_register_target_pragmas (void);
extern void nxu16_init_expanders (void);

#ifdef TREE_CODE
extern void nxu16_asm_output_aligned_decl_common (FILE*, tree, const char*, unsigned HOST_WIDE_INT, unsigned int, bool);
extern void nxu16_asm_asm_output_aligned_bss (FILE *, tree, const char *, unsigned HOST_WIDE_INT, int, void (*) (FILE *, tree, const char *, unsigned HOST_WIDE_INT, int));
extern void nxu16_declare_function_name (FILE *, const char *, tree);
extern void asm_output_external (FILE *file, tree decl, char *name);
extern int nxu16_progmem_p (tree decl, tree attributes);
extern bool nxu16_addr_space_supported_p (addr_space_t, location_t loc = UNKNOWN_LOCATION);

#ifdef RTX_CODE /* inside TREE_CODE */
extern void nxu16_init_cumulative_args (CUMULATIVE_ARGS*, tree, rtx, tree);
#endif /* RTX_CODE inside TREE_CODE */

#endif /* TREE_CODE */

#ifdef RTX_CODE
extern const char *output_movqi (rtx_insn *insn, rtx operands[], int *l);
extern const char *output_movhi (rtx_insn *insn, rtx operands[], int *l);
extern const char *output_movsisf (rtx_insn *insn, rtx operands[], int *l);
extern const char *nxu16_out_tstsi (rtx_insn *, rtx*, int*);
extern const char *nxu16_out_tsthi (rtx_insn *, rtx*, int*);
extern const char *nxu16_out_tstpsi (rtx_insn *, rtx*, int*);
extern const char *nxu16_out_compare (rtx_insn *, rtx*, int*);
extern const char *nxu16_out_compare64 (rtx_insn *, rtx*, int*);
extern const char *nxu16_cond_branch (rtx_insn *, rtx *);
extern const char *nxu16_out_movpsi (rtx_insn *, rtx*, int*);
extern const char *nxu16_out_sign_extend (rtx_insn *, rtx*, int*);
extern const char *nxu16_out_insert_notbit (rtx_insn *, rtx*, int*);
extern const char *nxu16_out_insv (rtx_insn *, rtx*, int*);
extern const char *nxu16_out_extr (rtx_insn *, rtx*, int*);
extern const char *nxu16_out_extr_not (rtx_insn *, rtx*, int*);
extern const char *nxu16_out_plus_set_ZN (rtx*, int*);
extern const char *nxu16_out_plus_set_N (rtx*, int*);
extern const char *nxu16_out_op8_set_ZN (rtx_code, rtx*, int*);
extern int nxu16_len_op8_set_ZN (rtx_code, rtx*);
extern bool nxu16_op8_ZN_operator (rtx);
extern const char *nxu16_out_cmp_ext (rtx*, rtx_code, int*);

extern const char *ashlqi3_out (rtx_insn *insn, rtx operands[], int *len);
extern const char *ashlhi3_out (rtx_insn *insn, rtx operands[], int *len);
extern const char *ashlsi3_out (rtx_insn *insn, rtx operands[], int *len);

extern const char *ashrqi3_out (rtx_insn *insn, rtx operands[], int *len);
extern const char *ashrhi3_out (rtx_insn *insn, rtx operands[], int *len);
extern const char *ashrsi3_out (rtx_insn *insn, rtx operands[], int *len);

extern const char *lshrqi3_out (rtx_insn *insn, rtx operands[], int *len);
extern const char *lshrhi3_out (rtx_insn *insn, rtx operands[], int *len);
extern const char *lshrsi3_out (rtx_insn *insn, rtx operands[], int *len);

extern const char *nxu16_out_ashlpsi3 (rtx_insn *, rtx*, int*);
extern const char *nxu16_out_ashrpsi3 (rtx_insn *, rtx*, int*);
extern const char *nxu16_out_lshrpsi3 (rtx_insn *, rtx*, int*);

extern bool nxu16_rotate_bytes (rtx operands[]);

extern const char* nxu16_out_fract (rtx_insn *, rtx[], bool, int*);
extern rtx nxu16_to_int_mode (rtx);

extern void nxu16_expand_prologue (void);
extern void nxu16_expand_epilogue (void);
extern bool nxu16_emit_cpymemhi (rtx*);
extern void nxu16_emit_xior_with_shift (rtx_insn*, rtx*, int);
extern int nxu16_epilogue_uses (int regno);

extern void nxu16_output_addr_vec (rtx_insn*, rtx);
extern const char *nxu16_out_sbxx_branch (rtx_insn *insn, rtx operands[]);
extern const char* nxu16_out_bitop (rtx, rtx*, int*);
extern const char* nxu16_out_plus (rtx, rtx*, int* =NULL, bool =true);
extern const char* nxu16_out_plus_ext (rtx_insn*, rtx*, int*);
extern const char* nxu16_out_round (rtx_insn *, rtx*, int* =NULL);
extern const char* nxu16_out_addto_sp (rtx*, int*);
extern const char* nxu16_out_xload (rtx_insn *, rtx*, int*);
extern const char* nxu16_out_cpymem (rtx_insn *, rtx*, int*);
extern const char* nxu16_out_insert_bits (rtx*, int*);
extern bool nxu16_popcount_each_byte (rtx, int, int);
extern bool nxu16_xor_noclobber_dconst (rtx, int);
extern bool nxu16_has_nibble_0xf (rtx);

extern int extra_constraint_Q (rtx x);
extern int nxu16_adjust_insn_length (rtx_insn *insn, int len);
extern const char* output_reload_inhi (rtx*, rtx, int*);
extern const char* output_reload_insisf (rtx*, rtx, int*);
extern const char* nxu16_out_reload_inpsi (rtx*, rtx, int*);
extern const char* nxu16_out_lpm (rtx_insn *, rtx*, int*);
extern const char* nxu16_out_cmp_lsr (rtx_insn *, rtx*, int*);
extern void nxu16_maybe_cmp_lsr (rtx *);
extern int reg_unused_after (rtx_insn *insn, rtx reg);
extern int nxu16_jump_mode (rtx x, rtx_insn *insn, int = 0);
extern int test_hard_reg_class (enum reg_class rclass, rtx x);
extern int jump_over_one_insn_p (rtx_insn *insn, rtx dest);

extern void nxu16_final_prescan_insn (rtx_insn *insn, rtx *operand,
				    int num_operands);
extern rtx_code nxu16_normalize_condition (rtx_code condition);
extern void out_shift_with_cnt (const char *templ, rtx_insn *insn,
				rtx operands[], int *len, int t_len);
extern enum reg_class nxu16_mode_code_base_reg_class (machine_mode, addr_space_t, rtx_code, rtx_code);
extern bool nxu16_regno_mode_code_ok_for_base_p (int, machine_mode, addr_space_t, rtx_code, rtx_code);
extern rtx nxu16_incoming_return_addr_rtx (void);
extern rtx nxu16_legitimize_reload_address (rtx*, machine_mode, int, int, int, int, rtx (*)(rtx,int));
extern bool nxu16_adiw_reg_p (rtx);
extern bool nxu16_mem_flash_p (rtx);
extern bool nxu16_mem_memx_p (rtx);
extern bool nxu16_load_libgcc_p (rtx);
extern bool nxu16_xload_libgcc_p (machine_mode);
extern rtx nxu16_eval_addr_attrib (rtx x);
extern void nxu16_cpu_cpp_builtins (cpp_reader *pfile);

extern bool nxu16_float_lib_compare_returns_bool (machine_mode, rtx_code);

static inline unsigned
regmask (machine_mode mode, unsigned regno)
{
  return ((1u << GET_MODE_SIZE (as_a <fixed_size_mode> (mode))) - 1) << regno;
}

extern void nxu16_fix_inputs (rtx*, unsigned, unsigned);
extern bool nxu16_emit3_fix_outputs (rtx (*)(rtx,rtx,rtx), rtx*, unsigned, unsigned);

extern rtx lpm_reg_rtx;
extern rtx lpm_addr_reg_rtx;
extern rtx tmp_reg_rtx;
extern rtx zero_reg_rtx;
extern rtx all_regs_rtx[32];
extern rtx rampz_rtx;
extern rtx cc_reg_rtx;
extern rtx ccn_reg_rtx;
extern rtx cczn_reg_rtx;

#endif /* RTX_CODE */

#ifdef REAL_VALUE_TYPE
extern void asm_output_float (FILE *file, REAL_VALUE_TYPE n);
#endif

extern bool nxu16_have_dimode;

/* From nxu16-passes.cc */

namespace gcc { class context; }
class rtl_opt_pass;

extern rtl_opt_pass *make_nxu16_pass_fuse_add (gcc::context *);
extern rtl_opt_pass *make_nxu16_pass_pre_proep (gcc::context *);
extern rtl_opt_pass *make_nxu16_pass_recompute_notes (gcc::context *);
extern rtl_opt_pass *make_nxu16_pass_casesi (gcc::context *);
extern rtl_opt_pass *make_nxu16_pass_ifelse (gcc::context *);
#ifdef RTX_CODE
extern bool nxu16_casei_sequence_check_operands (rtx *xop);
extern bool nxu16_split_fake_addressing_move (rtx_insn *insn, rtx *operands);
#endif /* RTX_CODE */

/* From nxu16-log.cc */

#define nxu16_dump(...) nxu16_vdump (NULL, __FUNCTION__, __VA_ARGS__)
#define nxu16_edump(...) nxu16_vdump (stderr, __FUNCTION__, __VA_ARGS__)
#define nxu16_fdump(FIL, ...) nxu16_vdump (FIL, __FUNCTION__, __VA_ARGS__)

extern int nxu16_vdump (FILE*, const char*, ...);
extern void nxu16_log_set_nxu16_log (void);

typedef struct
{
  unsigned address_cost :1;
  unsigned builtin :1;
  unsigned constraints :1;
  unsigned insn_addresses :1;
  unsigned legitimate_address_p :1;
  unsigned legitimize_address :1;
  unsigned legitimize_reload_address :1;
  unsigned progmem :1;
  unsigned rtx_costs :1;
} nxu16_log_t;

extern nxu16_log_t nxu16_log;
