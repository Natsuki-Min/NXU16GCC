/* { dg-do compile } */
/* { dg-options "-O2 -fno-optimize-sibling-calls" } */
/* { dg-final { scan-assembler "\\t\\.section[ \\t]+\\.rodata" } } */
/* { dg-final { scan-assembler "\\t\\.byte[ \\t]+lo8\\(\\.L" } } */
/* { dg-final { scan-assembler "\\tb[ \\t]+er[0-9]+" } } */
/* { dg-final { scan-assembler-not "__ashlpsi3" } } */

extern void f0 (void); extern void f1 (void); extern void f2 (void);
extern void f3 (void); extern void f4 (void); extern void f5 (void);
extern void f6 (void); extern void f7 (void); extern void f8 (void);
extern void f9 (void);

void
dispatch (unsigned int value)
{
  switch (value)
    {
    case 0: f0 (); break;
    case 1: f1 (); break;
    case 2: f2 (); break;
    case 3: f3 (); break;
    case 4: f4 (); break;
    case 5: f5 (); break;
    case 6: f6 (); break;
    case 7: f7 (); break;
    case 8: f8 (); break;
    case 9: f9 (); break;
    default: break;
    }
}
