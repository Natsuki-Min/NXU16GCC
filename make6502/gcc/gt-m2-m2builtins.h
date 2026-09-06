/* Type information for m2/gm2-gcc/m2builtins.cc.
   Copyright (C) 2004-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This file is machine generated.  Do not edit.  */

void
gt_ggc_mx_vec_builtin_macro_definition_va_gc_ (void *x_p)
{
  vec<builtin_macro_definition,va_gc> * const x = (vec<builtin_macro_definition,va_gc> *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_mx (x);
    }
}

void
gt_ggc_mx (struct builtin_macro_definition& x_r ATTRIBUTE_UNUSED)
{
  struct builtin_macro_definition * ATTRIBUTE_UNUSED x = &x_r;
  gt_ggc_m_S ((*x).name);
  gt_ggc_m_S ((*x).builtinname);
  gt_ggc_m_9tree_node ((*x).function_node);
  gt_ggc_m_9tree_node ((*x).return_node);
}

void
gt_pch_nx_vec_builtin_macro_definition_va_gc_ (void *x_p)
{
  vec<builtin_macro_definition,va_gc> * const x = (vec<builtin_macro_definition,va_gc> *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_35vec_builtin_macro_definition_va_gc_))
    {
      gt_pch_nx (x);
    }
}

void
gt_pch_nx (struct builtin_macro_definition& x_r ATTRIBUTE_UNUSED)
{
  struct builtin_macro_definition * ATTRIBUTE_UNUSED x = &x_r;
  gt_pch_n_S ((*x).name);
  gt_pch_n_S ((*x).builtinname);
  gt_pch_n_9tree_node ((*x).function_node);
  gt_pch_n_9tree_node ((*x).return_node);
}

void
gt_pch_p_35vec_builtin_macro_definition_va_gc_ (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct vec<builtin_macro_definition,va_gc> * x ATTRIBUTE_UNUSED = (struct vec<builtin_macro_definition,va_gc> *)x_p;
  if ((void *)(x) == this_obj)
    gt_pch_nx (&((*x)), op, cookie);
}

void
gt_pch_nx (struct builtin_macro_definition* x ATTRIBUTE_UNUSED,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
    op (&((*x).name), NULL, cookie);
    op (&((*x).builtinname), NULL, cookie);
    op (&((*x).function_node), NULL, cookie);
    op (&((*x).return_node), NULL, cookie);
}

/* GC roots.  */

EXPORTED_CONST struct ggc_root_tab gt_ggc_r_gt_m2_m2builtins_h[] = {
  {
    &builtin_macros,
    1,
    sizeof (builtin_macros),
    &gt_ggc_mx_vec_builtin_macro_definition_va_gc_,
    &gt_pch_nx_vec_builtin_macro_definition_va_gc_
  },
  {
    &builtin_ftype_int_var,
    1,
    sizeof (builtin_ftype_int_var),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &floatptr_type_node,
    1,
    sizeof (floatptr_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &doubleptr_type_node,
    1,
    sizeof (doubleptr_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &long_doubleptr_type_node,
    1,
    sizeof (long_doubleptr_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &gm2_huge_vall_node,
    1,
    sizeof (gm2_huge_vall_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &gm2_huge_val_node,
    1,
    sizeof (gm2_huge_val_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &gm2_huge_valf_node,
    1,
    sizeof (gm2_huge_valf_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &gm2_isnan_node,
    1,
    sizeof (gm2_isnan_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &gm2_isfinite_node,
    1,
    sizeof (gm2_isfinite_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &gm2_memset_node,
    1,
    sizeof (gm2_memset_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &gm2_memcpy_node,
    1,
    sizeof (gm2_memcpy_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &gm2_alloca_node,
    1,
    sizeof (gm2_alloca_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &ldouble_ftype_ldouble,
    1,
    sizeof (ldouble_ftype_ldouble),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &double_ftype_double,
    1,
    sizeof (double_ftype_double),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &float_ftype_float,
    1,
    sizeof (float_ftype_float),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &ldouble_ftype_void,
    1,
    sizeof (ldouble_ftype_void),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &float_ftype_void,
    1,
    sizeof (float_ftype_void),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &double_ftype_void,
    1,
    sizeof (double_ftype_void),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &const_ptr_endlink,
    1,
    sizeof (const_ptr_endlink),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &ptr_endlink,
    1,
    sizeof (ptr_endlink),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &int_endlink,
    1,
    sizeof (int_endlink),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &math_endlink,
    1,
    sizeof (math_endlink),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &endlink,
    1,
    sizeof (endlink),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &unsigned_endlink,
    1,
    sizeof (unsigned_endlink),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &sizetype_endlink,
    1,
    sizeof (sizetype_endlink),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  LAST_GGC_ROOT_TAB
};

