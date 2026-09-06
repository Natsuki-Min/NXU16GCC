/* Type information for cp/lambda.cc.
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
gt_ggc_mx_vec_lambda_sig_count_va_gc_ (void *x_p)
{
  vec<lambda_sig_count,va_gc> * const x = (vec<lambda_sig_count,va_gc> *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_mx (x);
    }
}

void
gt_ggc_mx (struct lambda_sig_count& x_r ATTRIBUTE_UNUSED)
{
  struct lambda_sig_count * ATTRIBUTE_UNUSED x = &x_r;
  gt_ggc_m_9tree_node ((*x).fn);
}

void
gt_ggc_mx_vec_lambda_discriminator_va_gc_ (void *x_p)
{
  vec<lambda_discriminator,va_gc> * const x = (vec<lambda_discriminator,va_gc> *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_mx (x);
    }
}

void
gt_ggc_mx (struct lambda_discriminator& x_r ATTRIBUTE_UNUSED)
{
  struct lambda_discriminator * ATTRIBUTE_UNUSED x = &x_r;
  gt_ggc_m_9tree_node ((*x).scope);
  gt_ggc_m_27vec_lambda_sig_count_va_gc_ ((*x).discriminators);
}

void
gt_pch_nx_vec_lambda_sig_count_va_gc_ (void *x_p)
{
  vec<lambda_sig_count,va_gc> * const x = (vec<lambda_sig_count,va_gc> *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_27vec_lambda_sig_count_va_gc_))
    {
      gt_pch_nx (x);
    }
}

void
gt_pch_nx (struct lambda_sig_count& x_r ATTRIBUTE_UNUSED)
{
  struct lambda_sig_count * ATTRIBUTE_UNUSED x = &x_r;
  gt_pch_n_9tree_node ((*x).fn);
}

void
gt_pch_nx_vec_lambda_discriminator_va_gc_ (void *x_p)
{
  vec<lambda_discriminator,va_gc> * const x = (vec<lambda_discriminator,va_gc> *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_31vec_lambda_discriminator_va_gc_))
    {
      gt_pch_nx (x);
    }
}

void
gt_pch_nx (struct lambda_discriminator& x_r ATTRIBUTE_UNUSED)
{
  struct lambda_discriminator * ATTRIBUTE_UNUSED x = &x_r;
  gt_pch_n_9tree_node ((*x).scope);
  gt_pch_n_27vec_lambda_sig_count_va_gc_ ((*x).discriminators);
}

void
gt_pch_p_27vec_lambda_sig_count_va_gc_ (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct vec<lambda_sig_count,va_gc> * x ATTRIBUTE_UNUSED = (struct vec<lambda_sig_count,va_gc> *)x_p;
  if ((void *)(x) == this_obj)
    gt_pch_nx (&((*x)), op, cookie);
}

void
gt_pch_nx (struct lambda_sig_count* x ATTRIBUTE_UNUSED,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
    op (&((*x).fn), NULL, cookie);
}

void
gt_pch_p_31vec_lambda_discriminator_va_gc_ (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct vec<lambda_discriminator,va_gc> * x ATTRIBUTE_UNUSED = (struct vec<lambda_discriminator,va_gc> *)x_p;
  if ((void *)(x) == this_obj)
    gt_pch_nx (&((*x)), op, cookie);
}

void
gt_pch_nx (struct lambda_discriminator* x ATTRIBUTE_UNUSED,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
    op (&((*x).scope), NULL, cookie);
    op (&((*x).discriminators), NULL, cookie);
}

/* GC roots.  */

EXPORTED_CONST struct ggc_root_tab gt_ggc_r_gt_cp_lambda_h[] = {
  {
    &lambda_scope_stack,
    1,
    sizeof (lambda_scope_stack),
    &gt_ggc_mx_vec_lambda_discriminator_va_gc_,
    &gt_pch_nx_vec_lambda_discriminator_va_gc_
  },
  {
    &lambda_scope.scope,
    1,
    sizeof (lambda_scope.scope),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &lambda_scope.discriminators,
    1,
    sizeof (lambda_scope.discriminators),
    &gt_ggc_mx_vec_lambda_sig_count_va_gc_,
    &gt_pch_nx_vec_lambda_sig_count_va_gc_
  },
  {
    &max_id,
    1,
    sizeof (max_id),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &ptr_id,
    1,
    sizeof (ptr_id),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  LAST_GGC_ROOT_TAB
};

EXPORTED_CONST struct ggc_root_tab gt_pch_rs_gt_cp_lambda_h[] = {
  { &lambda_scope, 1, sizeof (lambda_scope), NULL, NULL },
  LAST_GGC_ROOT_TAB
};

