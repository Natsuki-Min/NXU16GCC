/* { dg-do assemble } */
/* { dg-options "-O2 -fno-asynchronous-unwind-tables -fno-unwind-tables" } */

/* Exercise two ISA constraints that only become visible after reload:
   indirect call targets use a four-register far slot, and a word immediate
   only has a signed seven-bit encoding.  */

typedef int (*callback_type) (int);

int
call_stacked_callback (unsigned int a0, unsigned int a1,
		       unsigned int a2, unsigned int a3,
		       unsigned int a4, unsigned int a5,
		       unsigned int a6, unsigned int a7,
		       callback_type callback)
{
  return callback (a0) + a1 + a2 + a3 + a4 + a5 + a6 + a7;
}

unsigned int
large_word_constant (void)
{
  return 0x1234u;
}
