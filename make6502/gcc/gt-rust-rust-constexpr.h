/* Type information for rust/backend/rust-constexpr.cc.
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
gt_ggc_mx_rust_constexpr_fundef (void *x_p)
{
  struct rust_constexpr_fundef * const x = (struct rust_constexpr_fundef *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_9tree_node ((*x).decl);
      gt_ggc_m_9tree_node ((*x).body);
      gt_ggc_m_9tree_node ((*x).parms);
      gt_ggc_m_9tree_node ((*x).result);
    }
}

void
gt_ggc_mx (struct rust_constexpr_fundef& x_r ATTRIBUTE_UNUSED)
{
  struct rust_constexpr_fundef * ATTRIBUTE_UNUSED x = &x_r;
  gt_ggc_m_9tree_node ((*x).decl);
  gt_ggc_m_9tree_node ((*x).body);
  gt_ggc_m_9tree_node ((*x).parms);
  gt_ggc_m_9tree_node ((*x).result);
}

void
gt_ggc_mx (struct rust_constexpr_fundef *& x)
{
  if (x)
    gt_ggc_mx_rust_constexpr_fundef ((void *) x);
}

void
gt_ggc_mx_rust_constexpr_call (void *x_p)
{
  struct rust_constexpr_call * const x = (struct rust_constexpr_call *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_21rust_constexpr_fundef ((*x).fundef);
      gt_ggc_m_9tree_node ((*x).bindings);
      gt_ggc_m_9tree_node ((*x).result);
    }
}

void
gt_ggc_mx (struct rust_constexpr_call& x_r ATTRIBUTE_UNUSED)
{
  struct rust_constexpr_call * ATTRIBUTE_UNUSED x = &x_r;
  gt_ggc_m_21rust_constexpr_fundef ((*x).fundef);
  gt_ggc_m_9tree_node ((*x).bindings);
  gt_ggc_m_9tree_node ((*x).result);
}

void
gt_ggc_mx (struct rust_constexpr_call *& x)
{
  if (x)
    gt_ggc_mx_rust_constexpr_call ((void *) x);
}

void
gt_ggc_mx_hash_table_rust_constexpr_fundef_hasher_ (void *x_p)
{
  hash_table<rust_constexpr_fundef_hasher> * const x = (hash_table<rust_constexpr_fundef_hasher> *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_mx (x);
    }
}

void
gt_ggc_mx (struct rust_constexpr_fundef_hasher& x_r ATTRIBUTE_UNUSED)
{
  struct rust_constexpr_fundef_hasher * ATTRIBUTE_UNUSED x = &x_r;
}

void
gt_ggc_mx_hash_table_rust_constexpr_call_hasher_ (void *x_p)
{
  hash_table<rust_constexpr_call_hasher> * const x = (hash_table<rust_constexpr_call_hasher> *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_mx (x);
    }
}

void
gt_ggc_mx (struct rust_constexpr_call_hasher& x_r ATTRIBUTE_UNUSED)
{
  struct rust_constexpr_call_hasher * ATTRIBUTE_UNUSED x = &x_r;
}

void
gt_pch_nx_rust_constexpr_fundef (void *x_p)
{
  struct rust_constexpr_fundef * const x = (struct rust_constexpr_fundef *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_21rust_constexpr_fundef))
    {
      gt_pch_n_9tree_node ((*x).decl);
      gt_pch_n_9tree_node ((*x).body);
      gt_pch_n_9tree_node ((*x).parms);
      gt_pch_n_9tree_node ((*x).result);
    }
}

void
gt_pch_nx (struct rust_constexpr_fundef& x_r ATTRIBUTE_UNUSED)
{
  struct rust_constexpr_fundef * ATTRIBUTE_UNUSED x = &x_r;
  gt_pch_n_9tree_node ((*x).decl);
  gt_pch_n_9tree_node ((*x).body);
  gt_pch_n_9tree_node ((*x).parms);
  gt_pch_n_9tree_node ((*x).result);
}

void
gt_pch_nx (struct rust_constexpr_fundef *& x)
{
  if (x)
    gt_pch_nx_rust_constexpr_fundef ((void *) x);
}

void
gt_pch_nx_rust_constexpr_call (void *x_p)
{
  struct rust_constexpr_call * const x = (struct rust_constexpr_call *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_19rust_constexpr_call))
    {
      gt_pch_n_21rust_constexpr_fundef ((*x).fundef);
      gt_pch_n_9tree_node ((*x).bindings);
      gt_pch_n_9tree_node ((*x).result);
    }
}

void
gt_pch_nx (struct rust_constexpr_call& x_r ATTRIBUTE_UNUSED)
{
  struct rust_constexpr_call * ATTRIBUTE_UNUSED x = &x_r;
  gt_pch_n_21rust_constexpr_fundef ((*x).fundef);
  gt_pch_n_9tree_node ((*x).bindings);
  gt_pch_n_9tree_node ((*x).result);
}

void
gt_pch_nx (struct rust_constexpr_call *& x)
{
  if (x)
    gt_pch_nx_rust_constexpr_call ((void *) x);
}

void
gt_pch_nx_hash_table_rust_constexpr_fundef_hasher_ (void *x_p)
{
  hash_table<rust_constexpr_fundef_hasher> * const x = (hash_table<rust_constexpr_fundef_hasher> *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_40hash_table_rust_constexpr_fundef_hasher_))
    {
      gt_pch_nx (x);
    }
}

void
gt_pch_nx (struct rust_constexpr_fundef_hasher& x_r ATTRIBUTE_UNUSED)
{
  struct rust_constexpr_fundef_hasher * ATTRIBUTE_UNUSED x = &x_r;
}

void
gt_pch_nx_hash_table_rust_constexpr_call_hasher_ (void *x_p)
{
  hash_table<rust_constexpr_call_hasher> * const x = (hash_table<rust_constexpr_call_hasher> *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_38hash_table_rust_constexpr_call_hasher_))
    {
      gt_pch_nx (x);
    }
}

void
gt_pch_nx (struct rust_constexpr_call_hasher& x_r ATTRIBUTE_UNUSED)
{
  struct rust_constexpr_call_hasher * ATTRIBUTE_UNUSED x = &x_r;
}

void
gt_pch_p_21rust_constexpr_fundef (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct rust_constexpr_fundef * x ATTRIBUTE_UNUSED = (struct rust_constexpr_fundef *)x_p;
  if ((void *)(x) == this_obj)
    op (&((*x).decl), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).body), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).parms), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).result), NULL, cookie);
}

void
gt_pch_nx (struct rust_constexpr_fundef* x ATTRIBUTE_UNUSED,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
    op (&((*x).decl), NULL, cookie);
    op (&((*x).body), NULL, cookie);
    op (&((*x).parms), NULL, cookie);
    op (&((*x).result), NULL, cookie);
}

void
gt_pch_p_19rust_constexpr_call (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct rust_constexpr_call * x ATTRIBUTE_UNUSED = (struct rust_constexpr_call *)x_p;
  if ((void *)(x) == this_obj)
    op (&((*x).fundef), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).bindings), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).result), NULL, cookie);
}

void
gt_pch_nx (struct rust_constexpr_call* x ATTRIBUTE_UNUSED,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
    op (&((*x).fundef), NULL, cookie);
    op (&((*x).bindings), NULL, cookie);
    op (&((*x).result), NULL, cookie);
}

void
gt_pch_p_40hash_table_rust_constexpr_fundef_hasher_ (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct hash_table<rust_constexpr_fundef_hasher> * x ATTRIBUTE_UNUSED = (struct hash_table<rust_constexpr_fundef_hasher> *)x_p;
  if ((void *)(x) == this_obj)
    gt_pch_nx (&((*x)), op, cookie);
}

void
gt_pch_nx (struct rust_constexpr_fundef_hasher* x ATTRIBUTE_UNUSED,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
}

void
gt_pch_p_38hash_table_rust_constexpr_call_hasher_ (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct hash_table<rust_constexpr_call_hasher> * x ATTRIBUTE_UNUSED = (struct hash_table<rust_constexpr_call_hasher> *)x_p;
  if ((void *)(x) == this_obj)
    gt_pch_nx (&((*x)), op, cookie);
}

void
gt_pch_nx (struct rust_constexpr_call_hasher* x ATTRIBUTE_UNUSED,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
}

/* GC roots.  */

EXPORTED_CONST struct ggc_root_tab gt_ggc_r_gt_rust_rust_constexpr_h[] = {
  {
    &fundef_copies_table,
    1,
    sizeof (fundef_copies_table),
    &gt_ggc_mx_hash_map_tree_tree_decl_tree_traits_,
    &gt_pch_nx_hash_map_tree_tree_decl_tree_traits_
  },
  {
    &constexpr_call_table,
    1,
    sizeof (constexpr_call_table),
    &gt_ggc_mx_hash_table_rust_constexpr_call_hasher_,
    &gt_pch_nx_hash_table_rust_constexpr_call_hasher_
  },
  {
    &constexpr_fundef_table,
    1,
    sizeof (constexpr_fundef_table),
    &gt_ggc_mx_hash_table_rust_constexpr_fundef_hasher_,
    &gt_pch_nx_hash_table_rust_constexpr_fundef_hasher_
  },
  LAST_GGC_ROOT_TAB
};

EXPORTED_CONST struct ggc_root_tab gt_ggc_rd_gt_rust_rust_constexpr_h[] = {
  { &cv_cache, 1, sizeof (cv_cache), NULL, NULL },
  LAST_GGC_ROOT_TAB
};

