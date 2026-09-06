/* Generated automatically by the program `genconstants'
   from the machine description file `md'.  */

#ifndef GCC_INSN_CONSTANTS_H
#define GCC_INSN_CONSTANTS_H

#define HARDSP_REGNUM 80
#define TMP1_REGNUM 83
#define CARRY_REGNUM 72
#define Y_REGNUM 8
#define NZ_REGNUM 68
#define X_REGNUM 4
#define OVERFLOW_REGNUM 76
#define TMP0_REGNUM 82
#define ACC_REGNUM 0

enum unspec {
  UNSPEC_SBC_NZ = 0,
  UNSPEC_SBC_OVF = 1,
  UNSPEC_NEG_HIBIT = 2,
  UNSPEC_IOR_CC = 3,
  UNSPEC_ADC_C = 4,
  UNSPEC_SBC_C = 5,
  UNSPEC_ASL_C = 6,
  UNSPEC_LSR_C = 7,
  UNSPEC_INCDEC_NZ = 8,
  UNSPEC_LOAD_NZ = 9,
  UNSPEC_ROR = 10,
  UNSPEC_ROL = 11,
  UNSPEC_LOADQI = 12,
  UNSPEC_STOREQI = 13,
  UNSPEC_MOVZP = 14,
  UNSPEC_MOVZP_CLOB = 15,
  UNSPEC_FLAGS = 16,
  UNSPEC_FLAGS_NZ = 17,
  UNSPEC_FLAGS_C = 18,
  UNSPEC_FLAGS_V = 19,
  UNSPEC_XIND = 20,
  UNSPEC_CLC = 21,
  UNSPEC_SEC = 22
};
#define NUM_UNSPEC_VALUES 23
extern const char *const unspec_strings[];

enum unspecv {
  UNSPECV_SAVESTACK_S7S0_SP = 0,
  UNSPECV_RESTORESTACK_S7S0_SP_RTS = 1,
  UNSPECV_SAVESTACK_S7S0 = 2,
  UNSPECV_RESTORESTACK_S7S0_RTS = 3
};
#define NUM_UNSPECV_VALUES 4
extern const char *const unspecv_strings[];

#endif /* GCC_INSN_CONSTANTS_H */
