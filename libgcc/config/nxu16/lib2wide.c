/* Selected generic libgcc helpers for modes wider than the NXU16 register
   argument window.  Keeping these in a target wrapper avoids enabling the
   many still-unvalidated libgcc2 entry points for this byte-register ABI.  */

#define L_muldi3
#define L_negdi2
#define L_lshrdi3
#define L_ashldi3
#define L_ashrdi3
#define L_cmpdi2
#define L_ucmpdi2
#define L_clz
#define L_clzdi2
#define L_divdi3
#define L_moddi3
#define L_udivdi3
#define L_umoddi3

#include "../../libgcc2.c"
