/* IEEE-754 binary32 helpers.  fp-bit is used instead of soft-fp because its
   32-bit working type maps directly onto the established NXU16 SI ABI.  */

#define FLOAT
#define FLOAT_ONLY
#define SMALL_MACHINE

#include "../../fp-bit.c"
