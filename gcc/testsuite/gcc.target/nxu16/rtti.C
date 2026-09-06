/* { dg-do compile } */
/* { dg-options "-O2 -frtti" } */

/* Compile-only coverage for the target's C++ RTTI lowering.  Linking and
   exception propagation still require a target C++ runtime.  */

struct base
{
  virtual ~base ();
};

struct derived : base
{
  int value;
};

derived *
as_derived (base *object)
{
  return dynamic_cast<derived *> (object);
}

