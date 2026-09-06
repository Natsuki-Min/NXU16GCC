/* Type information for m2/gm2-gcc/m2block.cc.
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
gt_ggc_mx_binding_level (void *x_p)
{
  struct binding_level * const x = (struct binding_level *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_9tree_node ((*x).fndecl);
      gt_ggc_m_9tree_node ((*x).names);
      gt_ggc_m_9tree_node ((*x).context);
      gt_ggc_m_13binding_level ((*x).next);
      gt_ggc_m_13binding_level ((*x).list);
      gt_ggc_m_15vec_tree_va_gc_ ((*x).m2_statements);
      gt_ggc_m_9tree_node ((*x).constants);
      gt_ggc_m_9tree_node ((*x).init_functions);
      gt_ggc_m_9tree_node ((*x).types);
      gt_ggc_m_9tree_node ((*x).decl);
      gt_ggc_m_9tree_node ((*x).labels);
    }
}

void
gt_pch_nx_binding_level (void *x_p)
{
  struct binding_level * const x = (struct binding_level *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_13binding_level))
    {
      gt_pch_n_9tree_node ((*x).fndecl);
      gt_pch_n_9tree_node ((*x).names);
      gt_pch_n_9tree_node ((*x).context);
      gt_pch_n_13binding_level ((*x).next);
      gt_pch_n_13binding_level ((*x).list);
      gt_pch_n_15vec_tree_va_gc_ ((*x).m2_statements);
      gt_pch_n_9tree_node ((*x).constants);
      gt_pch_n_9tree_node ((*x).init_functions);
      gt_pch_n_9tree_node ((*x).types);
      gt_pch_n_9tree_node ((*x).decl);
      gt_pch_n_9tree_node ((*x).labels);
    }
}

void
gt_pch_p_13binding_level (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct binding_level * x ATTRIBUTE_UNUSED = (struct binding_level *)x_p;
  if ((void *)(x) == this_obj)
    op (&((*x).fndecl), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).names), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).context), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).next), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).list), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).m2_statements), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).constants), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).init_functions), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).types), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).decl), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).labels), NULL, cookie);
}

/* GC roots.  */

EXPORTED_CONST struct ggc_root_tab gt_ggc_r_gt_m2_m2block_h[] = {
  {
    &head_binding_level,
    1,
    sizeof (head_binding_level),
    &gt_ggc_mx_binding_level,
    &gt_pch_nx_binding_level
  },
  {
    &global_binding_level,
    1,
    sizeof (global_binding_level),
    &gt_ggc_mx_binding_level,
    &gt_pch_nx_binding_level
  },
  {
    &current_binding_level,
    1,
    sizeof (current_binding_level),
    &gt_ggc_mx_binding_level,
    &gt_pch_nx_binding_level
  },
  LAST_GGC_ROOT_TAB
};

