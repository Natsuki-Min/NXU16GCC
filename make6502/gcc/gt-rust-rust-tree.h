/* Type information for rust/backend/rust-tree.cc.
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
gt_ggc_mx_source_location_table_entry (void *x_p)
{
  struct source_location_table_entry * const x = (struct source_location_table_entry *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_9tree_node ((*x).var);
    }
}

void
gt_ggc_mx (struct source_location_table_entry& x_r ATTRIBUTE_UNUSED)
{
  struct source_location_table_entry * ATTRIBUTE_UNUSED x = &x_r;
  gt_ggc_m_9tree_node ((*x).var);
}

void
gt_ggc_mx (struct source_location_table_entry *& x)
{
  if (x)
    gt_ggc_mx_source_location_table_entry ((void *) x);
}

void
gt_ggc_mx_hash_table_rust_conv_type_hasher_ (void *x_p)
{
  hash_table<rust_conv_type_hasher> * const x = (hash_table<rust_conv_type_hasher> *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_mx (x);
    }
}

void
gt_ggc_mx (struct rust_conv_type_hasher& x_r ATTRIBUTE_UNUSED)
{
  struct rust_conv_type_hasher * ATTRIBUTE_UNUSED x = &x_r;
}

void
gt_ggc_mx_hash_table_rust_cplus_array_hasher_ (void *x_p)
{
  hash_table<rust_cplus_array_hasher> * const x = (hash_table<rust_cplus_array_hasher> *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_mx (x);
    }
}

void
gt_ggc_mx (struct rust_cplus_array_hasher& x_r ATTRIBUTE_UNUSED)
{
  struct rust_cplus_array_hasher * ATTRIBUTE_UNUSED x = &x_r;
}

void
gt_ggc_mx_hash_table_rust_source_location_table_entry_hash_ (void *x_p)
{
  hash_table<rust_source_location_table_entry_hash> * const x = (hash_table<rust_source_location_table_entry_hash> *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_mx (x);
    }
}

void
gt_ggc_mx (struct rust_source_location_table_entry_hash& x_r ATTRIBUTE_UNUSED)
{
  struct rust_source_location_table_entry_hash * ATTRIBUTE_UNUSED x = &x_r;
}

void
gt_pch_nx_source_location_table_entry (void *x_p)
{
  struct source_location_table_entry * const x = (struct source_location_table_entry *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_27source_location_table_entry))
    {
      gt_pch_n_9tree_node ((*x).var);
    }
}

void
gt_pch_nx (struct source_location_table_entry& x_r ATTRIBUTE_UNUSED)
{
  struct source_location_table_entry * ATTRIBUTE_UNUSED x = &x_r;
  gt_pch_n_9tree_node ((*x).var);
}

void
gt_pch_nx (struct source_location_table_entry *& x)
{
  if (x)
    gt_pch_nx_source_location_table_entry ((void *) x);
}

void
gt_pch_nx_hash_table_rust_conv_type_hasher_ (void *x_p)
{
  hash_table<rust_conv_type_hasher> * const x = (hash_table<rust_conv_type_hasher> *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_33hash_table_rust_conv_type_hasher_))
    {
      gt_pch_nx (x);
    }
}

void
gt_pch_nx (struct rust_conv_type_hasher& x_r ATTRIBUTE_UNUSED)
{
  struct rust_conv_type_hasher * ATTRIBUTE_UNUSED x = &x_r;
}

void
gt_pch_nx_hash_table_rust_cplus_array_hasher_ (void *x_p)
{
  hash_table<rust_cplus_array_hasher> * const x = (hash_table<rust_cplus_array_hasher> *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_35hash_table_rust_cplus_array_hasher_))
    {
      gt_pch_nx (x);
    }
}

void
gt_pch_nx (struct rust_cplus_array_hasher& x_r ATTRIBUTE_UNUSED)
{
  struct rust_cplus_array_hasher * ATTRIBUTE_UNUSED x = &x_r;
}

void
gt_pch_nx_hash_table_rust_source_location_table_entry_hash_ (void *x_p)
{
  hash_table<rust_source_location_table_entry_hash> * const x = (hash_table<rust_source_location_table_entry_hash> *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_49hash_table_rust_source_location_table_entry_hash_))
    {
      gt_pch_nx (x);
    }
}

void
gt_pch_nx (struct rust_source_location_table_entry_hash& x_r ATTRIBUTE_UNUSED)
{
  struct rust_source_location_table_entry_hash * ATTRIBUTE_UNUSED x = &x_r;
}

void
gt_pch_p_27source_location_table_entry (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct source_location_table_entry * x ATTRIBUTE_UNUSED = (struct source_location_table_entry *)x_p;
  if ((void *)(x) == this_obj)
    op (&((*x).var), NULL, cookie);
}

void
gt_pch_nx (struct source_location_table_entry* x ATTRIBUTE_UNUSED,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
    op (&((*x).var), NULL, cookie);
}

void
gt_pch_p_33hash_table_rust_conv_type_hasher_ (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct hash_table<rust_conv_type_hasher> * x ATTRIBUTE_UNUSED = (struct hash_table<rust_conv_type_hasher> *)x_p;
  if ((void *)(x) == this_obj)
    gt_pch_nx (&((*x)), op, cookie);
}

void
gt_pch_nx (struct rust_conv_type_hasher* x ATTRIBUTE_UNUSED,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
}

void
gt_pch_p_35hash_table_rust_cplus_array_hasher_ (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct hash_table<rust_cplus_array_hasher> * x ATTRIBUTE_UNUSED = (struct hash_table<rust_cplus_array_hasher> *)x_p;
  if ((void *)(x) == this_obj)
    gt_pch_nx (&((*x)), op, cookie);
}

void
gt_pch_nx (struct rust_cplus_array_hasher* x ATTRIBUTE_UNUSED,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
}

void
gt_pch_p_49hash_table_rust_source_location_table_entry_hash_ (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct hash_table<rust_source_location_table_entry_hash> * x ATTRIBUTE_UNUSED = (struct hash_table<rust_source_location_table_entry_hash> *)x_p;
  if ((void *)(x) == this_obj)
    gt_pch_nx (&((*x)), op, cookie);
}

void
gt_pch_nx (struct rust_source_location_table_entry_hash* x ATTRIBUTE_UNUSED,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
}

/* GC roots.  */

EXPORTED_CONST struct ggc_root_tab gt_ggc_r_gt_rust_rust_tree_h[] = {
  {
    &source_location_table,
    1,
    sizeof (source_location_table),
    &gt_ggc_mx_hash_table_rust_source_location_table_entry_hash_,
    &gt_pch_nx_hash_table_rust_source_location_table_entry_hash_
  },
  {
    &cplus_array_htab,
    1,
    sizeof (cplus_array_htab),
    &gt_ggc_mx_hash_table_rust_cplus_array_hasher_,
    &gt_pch_nx_hash_table_rust_cplus_array_hasher_
  },
  {
    &conv_type_names,
    1,
    sizeof (conv_type_names),
    &gt_ggc_mx_hash_table_rust_conv_type_hasher_,
    &gt_pch_nx_hash_table_rust_conv_type_hasher_
  },
  {
    &fixed_trees,
    1,
    sizeof (fixed_trees),
    &gt_ggc_mx_vec_tree_va_gc_,
    &gt_pch_nx_vec_tree_va_gc_
  },
  LAST_GGC_ROOT_TAB
};

EXPORTED_CONST struct ggc_root_tab gt_ggc_rd_gt_rust_rust_tree_h[] = {
  { &failed_type_completions, 1, sizeof (failed_type_completions), NULL, NULL },
  { &tree_vector_cache, 1, sizeof (tree_vector_cache), NULL, NULL },
  LAST_GGC_ROOT_TAB
};

EXPORTED_CONST struct ggc_root_tab gt_pch_rs_gt_rust_rust_tree_h[] = {
  { &source_location_id, 1, sizeof (source_location_id), NULL, NULL },
  LAST_GGC_ROOT_TAB
};

