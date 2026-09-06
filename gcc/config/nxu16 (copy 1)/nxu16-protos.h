

extern enum reg_class nxu16_regno_reg_class (int r);
extern poly_int64 nxu16_initial_elimination_offset (int from, int to);
extern bool nxu16_legitimate_address_p (enum machine_mode mode, rtx x);
extern bool nxu16_mode_dependent_address_p (rtx x);
extern void nxu16_expand_prologue (void);
extern void nxu16_expand_epilogue (void);
