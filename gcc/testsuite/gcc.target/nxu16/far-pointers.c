/* { dg-do compile } */
/* { dg-options "-Os" } */
/* { dg-final { scan-assembler "\\tpush[ \\t]+xr0" } } */
/* { dg-final { scan-assembler "\\tpush[ \\t]+r4" } } */
/* { dg-final { scan-assembler "\\tpush[ \\t]+er2" } } */
/* { dg-final { scan-assembler-times "\\tbl[ \\t]+__nxu16_indirect_call" 3 } } */
/* { dg-final { scan-assembler "\\tpush[ \\t]+xr0[\\n\\r]+\\tpop[ \\t]+pc" } } */
/* { dg-final { scan-assembler "\\tl[ \\t]+r[0-9]+,r[0-9]+:\\[er[0-9]+\\]" } } */

typedef void (*function_pointer) (void);

_Static_assert (sizeof (void *) == 2, "generic data pointers must be near");
_Static_assert (sizeof (function_pointer) == 4,
		"function pointers must be far");
_Static_assert (sizeof (unsigned char __near *) == 2,
		"near pointers must stay 16-bit");
_Static_assert (sizeof (unsigned char __far *) == 4,
		"far data pointers must use a 32-bit slot");
_Static_assert (sizeof (__SIZE_TYPE__) == 2,
		"size_t must match the near data address space");
_Static_assert (sizeof (__UINTPTR_TYPE__) == 2,
		"uintptr_t must represent ordinary data pointers");

volatile unsigned int call_count;

unsigned char
far_read (const unsigned char __far *pointer)
{
  return pointer[0] + pointer[3];
}

void
aligned_indirect_call (function_pointer target)
{
  target ();
  ++call_count;
}

void
unaligned_indirect_call (function_pointer argument)
{
  register function_pointer target asm ("r2") = argument;
  asm volatile ("" : "+r" (target));
  target ();
  asm volatile ("" ::: "memory");
}

/* A tail-call target must be copied into the call-clobbered XR0 slot before
   the sibling-call epilogue restores any call-saved registers.  */
void
indirect_tail_candidate (function_pointer target)
{
  target ();
}

int
indirect_tail_with_argument (int (*target) (int), int value)
{
  return target (value);
}
