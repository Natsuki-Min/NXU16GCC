/* Type information for m2/gm2-gcc/rtegraph.cc.
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
gt_ggc_mx_rtenode (void *x_p)
{
  struct rtenode * const x = (struct rtenode *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_6gimple ((*x).grtenode);
      gt_ggc_m_9tree_node ((*x).func);
      gt_ggc_m_7rtenode ((*x).reachable_src);
      gt_ggc_m_19vec_rtenode__va_gc_ ((*x).function_call);
      gt_ggc_m_19vec_rtenode__va_gc_ ((*x).rts_call);
    }
}

void
gt_ggc_mx_vec_rtenode__va_gc_ (void *x_p)
{
  vec<rtenode*,va_gc> * const x = (vec<rtenode*,va_gc> *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_mx (x);
    }
}

void
gt_ggc_mx (struct rtenode *& x)
{
  if (x)
    gt_ggc_mx_rtenode ((void *) x);
}

void
gt_pch_nx_rtenode (void *x_p)
{
  struct rtenode * const x = (struct rtenode *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_7rtenode))
    {
      gt_pch_n_6gimple ((*x).grtenode);
      gt_pch_n_9tree_node ((*x).func);
      gt_pch_n_7rtenode ((*x).reachable_src);
      gt_pch_n_19vec_rtenode__va_gc_ ((*x).function_call);
      gt_pch_n_19vec_rtenode__va_gc_ ((*x).rts_call);
    }
}

void
gt_pch_nx_vec_rtenode__va_gc_ (void *x_p)
{
  vec<rtenode*,va_gc> * const x = (vec<rtenode*,va_gc> *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_19vec_rtenode__va_gc_))
    {
      gt_pch_nx (x);
    }
}

void
gt_pch_nx (struct rtenode *& x)
{
  if (x)
    gt_pch_nx_rtenode ((void *) x);
}

void
gt_pch_p_7rtenode (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct rtenode * x ATTRIBUTE_UNUSED = (struct rtenode *)x_p;
  if ((void *)(x) == this_obj)
    op (&((*x).grtenode), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).func), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).reachable_src), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).function_call), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).rts_call), NULL, cookie);
}

void
gt_pch_p_19vec_rtenode__va_gc_ (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct vec<rtenode*,va_gc> * x ATTRIBUTE_UNUSED = (struct vec<rtenode*,va_gc> *)x_p;
  if ((void *)(x) == this_obj)
    gt_pch_nx (&((*x)), op, cookie);
}

/* GC roots.  */

EXPORTED_CONST struct ggc_root_tab gt_ggc_r_gt_m2_rtegraph_h[] = {
  {
    &rtegraph_current_function,
    1,
    sizeof (rtegraph_current_function),
    &gt_ggc_mx_rtenode,
    &gt_pch_nx_rtenode
  },
  {
    &constructors,
    1,
    sizeof (constructors),
    &gt_ggc_mx_vec_rtenode__va_gc_,
    &gt_pch_nx_vec_rtenode__va_gc_
  },
  {
    &externs,
    1,
    sizeof (externs),
    &gt_ggc_mx_vec_rtenode__va_gc_,
    &gt_pch_nx_vec_rtenode__va_gc_
  },
  {
    &candidates,
    1,
    sizeof (candidates),
    &gt_ggc_mx_vec_rtenode__va_gc_,
    &gt_pch_nx_vec_rtenode__va_gc_
  },
  {
    &allnodes,
    1,
    sizeof (allnodes),
    &gt_ggc_mx_vec_rtenode__va_gc_,
    &gt_pch_nx_vec_rtenode__va_gc_
  },
  LAST_GGC_ROOT_TAB
};

