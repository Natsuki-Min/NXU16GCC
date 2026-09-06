extern void nxu16_cpu_cpp_builtins (cpp_reader * pfile);
extern void nxu16_expand_prologue (void);
extern void nxu16_expand_epilogue (void);
extern enum reg_class nxu16_regno_reg_class (int r);
extern int nxu16_initial_elimination_offset (int from, int to);
extern rtx nxu16_return_addr_rtx (int count, rtx tem);
#ifdef RTX_CODE /* inside TREE_CODE */
extern void nxu16_init_cumulative_args (CUMULATIVE_ARGS*, tree, rtx, tree);
#endif /* RTX_CODE inside TREE_CODE */
extern int nxu16_function_arg_regno_p (int r);
extern int nxu16_epilogue_uses (int regno);
extern void nxu16_asm_out_ctor (rtx symbol, int priority);
extern void nxu16_asm_out_dtor (rtx symbol, int priority);
extern void nxu16_asm_output_aligned_decl_common (FILE *stream, tree /* decl */,
				    const char *name,
				    unsigned HOST_WIDE_INT size,
				    unsigned int align, bool local_p);
extern void nxu16_asm_asm_output_aligned_bss (FILE *file, tree decl, const char *name,
				unsigned HOST_WIDE_INT size, int align,
				void (*default_func)
				  (FILE *, tree, const char *,
				   unsigned HOST_WIDE_INT, int));
extern void nxu16_init_expanders (void);
extern int nxu16_accumulate_outgoing_args (void);				  
extern rtx nxu16_legitimize_reload_address (rtx*, machine_mode, int, int, int, int, rtx (*)(rtx,int));
