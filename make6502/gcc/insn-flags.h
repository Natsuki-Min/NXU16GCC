/* Generated automatically by the program `genflags'
   from the machine description file `md'.  */

#ifndef GCC_INSN_FLAGS_H
#define GCC_INSN_FLAGS_H

#define HAVE_movhi_noclob ((register_operand (operands[0], HImode) \
    || register_operand (operands[1], HImode)) \
   && (lra_in_progress || reload_completed))
#define HAVE_movsi_noclob ((register_operand (operands[0], SImode) \
    || register_operand (operands[1], SImode)) \
   && (lra_in_progress || reload_completed))
#define HAVE_addhi3_noclobacc (reload_completed)
#define HAVE_addhi3_clobacc (reload_completed)
#define HAVE_addhi3_noclob 1
#define HAVE_cbrhi_eq 1
#define HAVE_cbrhi_ne 1
#define HAVE_cbrhi_ltu 1
#define HAVE_cbrhi_gtu 1
#define HAVE_cbrhi_geu 1
#define HAVE_cbrhi_leu 1
#define HAVE_cbrhi_lt 1
#define HAVE_cbrhi_gt 1
#define HAVE_cbrhi_ge 1
#define HAVE_cbrhi_le 1
#define HAVE_loadqi_zp 1
#define HAVE_storeqi_zp 1
#define HAVE_movqi_noclob (m65x_valid_mov_operands (QImode, operands, true))
#define HAVE_movqi_insn (m65x_valid_mov_operands (QImode, operands, true))
#define HAVE_loadqi_xind 1
#define HAVE_loadqi_xind_nz 1
#define HAVE_storeqi_xind 1
#define HAVE_zp_clob_hardreg_movqi 1
#define HAVE_pushflags 1
#define HAVE_popflags 1
#define HAVE_movqi_loadstore_zp 1
#define HAVE_incdecqi3 1
#define HAVE_incdecqi3_nz 1
#define HAVE_addqi3 1
#define HAVE_addqi3_noclob 1
#define HAVE_subqi3 1
#define HAVE_clc 1
#define HAVE_sec 1
#define HAVE_adcqi3 1
#define HAVE_adcqi3_c 1
#define HAVE_sbcqi3 1
#define HAVE_sbcqi3_c 1
#define HAVE_sbcqi3_nzv 1
#define HAVE_negate_highbit 1
#define HAVE_andqi3_insn 1
#define HAVE_m65x_trbqi3 (TARGET_TRB_TSB)
#define HAVE_iorqi3 1
#define HAVE_iorqi3_nz 1
#define HAVE_xorqi3 1
#define HAVE_loadqi_nz 1
#define HAVE_ashlqi3_insn 1
#define HAVE_m65x_rorqi3 1
#define HAVE_m65x_rolqi3 1
#define HAVE_lshrqi3_insn 1
#define HAVE_ashrqi3 1
#define HAVE_one_cmplqi2 1
#define HAVE_compareqi 1
#define HAVE_condbranchqi_cc_c 1
#define HAVE_condbranchqi_cc_ui 1
#define HAVE_condbranchqi_cc_v 1
#define HAVE_condbranchqi_cc_nz 1
#define HAVE_indirect_jump 1
#define HAVE_jump 1
#define HAVE_nop 1
#define HAVE_call_sym (GET_CODE (operands[0]) == SYMBOL_REF)
#define HAVE_call_indirect 1
#define HAVE_call_value_sym (GET_CODE (operands[1]) == SYMBOL_REF)
#define HAVE_call_indirect_sym 1
#define HAVE_m65x_return 1
#define HAVE_m65x_savestack_s7s0_sp 1
#define HAVE_m65x_savestack_s7s0 1
#define HAVE_m65x_restorestack_s7s0_sp_rts 1
#define HAVE_m65x_restorestack_s7s0_rts 1
#define HAVE_movhi 1
#define HAVE_movsi 1
#define HAVE_addhi3 1
#define HAVE_addsi3 1
#define HAVE_subhi3 (can_create_pseudo_p ())
#define HAVE_subsi3 (can_create_pseudo_p ())
#define HAVE_lshrhi3 (can_create_pseudo_p ())
#define HAVE_ashlhi3 (can_create_pseudo_p ())
#define HAVE_cbranchhi4 1
#define HAVE_extendhisi2 1
#define HAVE_neghi2 1
#define HAVE_movqi 1
#define HAVE_loadqi_indy 1
#define HAVE_loadqi_indy_nz 1
#define HAVE_storeqi_indy 1
#define HAVE_pushqi1 1
#define HAVE_popqi1 1
#define HAVE_reload_inoutqi_zp 1
#define HAVE_negqi2 1
#define HAVE_andqi3 1
#define HAVE_ashlqi3 1
#define HAVE_lshrqi3 1
#define HAVE_cbranchqi4 1
#define HAVE_call 1
#define HAVE_call_value 1
#define HAVE_prologue 1
#define HAVE_epilogue 1
extern rtx        gen_movhi_noclob                  (rtx, rtx);
extern rtx        gen_movsi_noclob                  (rtx, rtx);
extern rtx        gen_addhi3_noclobacc              (rtx, rtx, rtx);
extern rtx        gen_addhi3_clobacc                (rtx, rtx, rtx);
extern rtx        gen_addhi3_noclob                 (rtx, rtx, rtx);
extern rtx        gen_cbrhi_eq                      (rtx, rtx, rtx);
extern rtx        gen_cbrhi_ne                      (rtx, rtx, rtx);
extern rtx        gen_cbrhi_ltu                     (rtx, rtx, rtx);
extern rtx        gen_cbrhi_gtu                     (rtx, rtx, rtx);
extern rtx        gen_cbrhi_geu                     (rtx, rtx, rtx);
extern rtx        gen_cbrhi_leu                     (rtx, rtx, rtx);
extern rtx        gen_cbrhi_lt                      (rtx, rtx, rtx);
extern rtx        gen_cbrhi_gt                      (rtx, rtx, rtx);
extern rtx        gen_cbrhi_ge                      (rtx, rtx, rtx);
extern rtx        gen_cbrhi_le                      (rtx, rtx, rtx);
extern rtx        gen_loadqi_zp                     (rtx, rtx);
extern rtx        gen_storeqi_zp                    (rtx, rtx);
extern rtx        gen_movqi_noclob                  (rtx, rtx);
extern rtx        gen_movqi_insn                    (rtx, rtx);
extern rtx        gen_loadqi_xind                   (rtx, rtx, rtx);
extern rtx        gen_loadqi_xind_nz                (rtx, rtx, rtx);
extern rtx        gen_storeqi_xind                  (rtx, rtx, rtx);
extern rtx        gen_zp_clob_hardreg_movqi         (rtx, rtx);
extern rtx        gen_pushflags                     (rtx);
extern rtx        gen_popflags                      (rtx);
extern rtx        gen_movqi_loadstore_zp            (rtx, rtx);
extern rtx        gen_incdecqi3                     (rtx, rtx, rtx);
extern rtx        gen_incdecqi3_nz                  (rtx, rtx, rtx);
extern rtx        gen_addqi3                        (rtx, rtx, rtx);
extern rtx        gen_addqi3_noclob                 (rtx, rtx, rtx);
extern rtx        gen_subqi3                        (rtx, rtx, rtx);
extern rtx        gen_clc                           (void);
extern rtx        gen_sec                           (void);
extern rtx        gen_adcqi3                        (rtx, rtx, rtx);
extern rtx        gen_adcqi3_c                      (rtx, rtx, rtx);
extern rtx        gen_sbcqi3                        (rtx, rtx, rtx);
extern rtx        gen_sbcqi3_c                      (rtx, rtx, rtx);
extern rtx        gen_sbcqi3_nzv                    (rtx, rtx, rtx);
extern rtx        gen_negate_highbit                (rtx, rtx);
extern rtx        gen_andqi3_insn                   (rtx, rtx, rtx);
extern rtx        gen_m65x_trbqi3                   (rtx, rtx, rtx);
extern rtx        gen_iorqi3                        (rtx, rtx, rtx);
extern rtx        gen_iorqi3_nz                     (rtx, rtx, rtx);
extern rtx        gen_xorqi3                        (rtx, rtx, rtx);
extern rtx        gen_loadqi_nz                     (rtx, rtx);
extern rtx        gen_ashlqi3_insn                  (rtx, rtx, rtx);
extern rtx        gen_m65x_rorqi3                   (rtx, rtx);
extern rtx        gen_m65x_rolqi3                   (rtx, rtx);
extern rtx        gen_lshrqi3_insn                  (rtx, rtx, rtx);
extern rtx        gen_ashrqi3                       (rtx, rtx, rtx);
extern rtx        gen_one_cmplqi2                   (rtx, rtx);
extern rtx        gen_compareqi                     (rtx, rtx);
extern rtx        gen_condbranchqi_cc_c             (rtx, rtx);
extern rtx        gen_condbranchqi_cc_ui            (rtx, rtx);
extern rtx        gen_condbranchqi_cc_v             (rtx, rtx);
extern rtx        gen_condbranchqi_cc_nz            (rtx, rtx);
extern rtx        gen_indirect_jump                 (rtx);
extern rtx        gen_jump                          (rtx);
extern rtx        gen_nop                           (void);
extern rtx        gen_call_sym                      (rtx, rtx);
extern rtx        gen_call_indirect                 (rtx, rtx);
extern rtx        gen_call_value_sym                (rtx, rtx, rtx);
extern rtx        gen_call_indirect_sym             (rtx, rtx, rtx);
extern rtx        gen_m65x_return                   (void);
extern rtx        gen_m65x_savestack_s7s0_sp        (rtx, rtx);
extern rtx        gen_m65x_savestack_s7s0           (void);
extern rtx        gen_m65x_restorestack_s7s0_sp_rts (void);
extern rtx        gen_m65x_restorestack_s7s0_rts    (void);
extern rtx        gen_movhi                         (rtx, rtx);
extern rtx        gen_movsi                         (rtx, rtx);
extern rtx        gen_addhi3                        (rtx, rtx, rtx);
extern rtx        gen_addsi3                        (rtx, rtx, rtx);
extern rtx        gen_subhi3                        (rtx, rtx, rtx);
extern rtx        gen_subsi3                        (rtx, rtx, rtx);
extern rtx        gen_lshrhi3                       (rtx, rtx, rtx);
extern rtx        gen_ashlhi3                       (rtx, rtx, rtx);
extern rtx        gen_cbranchhi4                    (rtx, rtx, rtx, rtx);
extern rtx        gen_extendhisi2                   (rtx, rtx);
extern rtx        gen_neghi2                        (rtx, rtx);
extern rtx        gen_movqi                         (rtx, rtx);
extern rtx        gen_loadqi_indy                   (rtx, rtx, rtx);
extern rtx        gen_loadqi_indy_nz                (rtx, rtx, rtx);
extern rtx        gen_storeqi_indy                  (rtx, rtx, rtx);
extern rtx        gen_pushqi1                       (rtx, rtx);
extern rtx        gen_popqi1                        (rtx, rtx);
extern rtx        gen_reload_inoutqi_zp             (rtx, rtx, rtx);
extern rtx        gen_negqi2                        (rtx, rtx);
extern rtx        gen_andqi3                        (rtx, rtx, rtx);
extern rtx        gen_ashlqi3                       (rtx, rtx, rtx);
extern rtx        gen_lshrqi3                       (rtx, rtx, rtx);
extern rtx        gen_cbranchqi4                    (rtx, rtx, rtx, rtx);
extern rtx        gen_call                          (rtx, rtx);
extern rtx        gen_call_value                    (rtx, rtx, rtx);
extern rtx        gen_prologue                      (void);
extern rtx        gen_epilogue                      (void);

#endif /* GCC_INSN_FLAGS_H */
