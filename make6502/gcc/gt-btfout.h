/* Type information for btfout.cc.
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
gt_ggc_mx_vec_ctf_dtdef_ref_va_gc_ (void *x_p)
{
  vec<ctf_dtdef_ref,va_gc> * const x = (vec<ctf_dtdef_ref,va_gc> *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_mx (x);
    }
}

void
gt_ggc_mx_hash_map_ctf_dtdef_ref_ctf_dtdef_ref_ (void *x_p)
{
  hash_map<ctf_dtdef_ref,ctf_dtdef_ref> * const x = (hash_map<ctf_dtdef_ref,ctf_dtdef_ref> *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_mx (x);
    }
}

void
gt_ggc_mx_hash_set_ctf_dtdef_ref_ (void *x_p)
{
  hash_set<ctf_dtdef_ref> * const x = (hash_set<ctf_dtdef_ref> *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_mx (x);
    }
}

void
gt_pch_nx_vec_ctf_dtdef_ref_va_gc_ (void *x_p)
{
  vec<ctf_dtdef_ref,va_gc> * const x = (vec<ctf_dtdef_ref,va_gc> *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_24vec_ctf_dtdef_ref_va_gc_))
    {
      gt_pch_nx (x);
    }
}

void
gt_pch_nx_hash_map_ctf_dtdef_ref_ctf_dtdef_ref_ (void *x_p)
{
  hash_map<ctf_dtdef_ref,ctf_dtdef_ref> * const x = (hash_map<ctf_dtdef_ref,ctf_dtdef_ref> *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_37hash_map_ctf_dtdef_ref_ctf_dtdef_ref_))
    {
      gt_pch_nx (x);
    }
}

void
gt_pch_nx_hash_set_ctf_dtdef_ref_ (void *x_p)
{
  hash_set<ctf_dtdef_ref> * const x = (hash_set<ctf_dtdef_ref> *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_23hash_set_ctf_dtdef_ref_))
    {
      gt_pch_nx (x);
    }
}

void
gt_pch_p_24vec_ctf_dtdef_ref_va_gc_ (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct vec<ctf_dtdef_ref,va_gc> * x ATTRIBUTE_UNUSED = (struct vec<ctf_dtdef_ref,va_gc> *)x_p;
  if ((void *)(x) == this_obj)
    gt_pch_nx (&((*x)), op, cookie);
}

void
gt_pch_p_37hash_map_ctf_dtdef_ref_ctf_dtdef_ref_ (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct hash_map<ctf_dtdef_ref,ctf_dtdef_ref> * x ATTRIBUTE_UNUSED = (struct hash_map<ctf_dtdef_ref,ctf_dtdef_ref> *)x_p;
  if ((void *)(x) == this_obj)
    gt_pch_nx (&((*x)), op, cookie);
}

void
gt_pch_p_23hash_set_ctf_dtdef_ref_ (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct hash_set<ctf_dtdef_ref> * x ATTRIBUTE_UNUSED = (struct hash_set<ctf_dtdef_ref> *)x_p;
  if ((void *)(x) == this_obj)
    gt_pch_nx (&((*x)), op, cookie);
}

/* GC roots.  */

EXPORTED_CONST struct ggc_root_tab gt_ggc_r_gt_btfout_h[] = {
  {
    &forwards,
    1,
    sizeof (forwards),
    &gt_ggc_mx_vec_ctf_dtdef_ref_va_gc_,
    &gt_pch_nx_vec_ctf_dtdef_ref_va_gc_
  },
  {
    &btf_used_types,
    1,
    sizeof (btf_used_types),
    &gt_ggc_mx_hash_set_ctf_dtdef_ref_,
    &gt_pch_nx_hash_set_ctf_dtdef_ref_
  },
  {
    &func_map,
    1,
    sizeof (func_map),
    &gt_ggc_mx_hash_map_ctf_dtdef_ref_ctf_dtdef_ref_,
    &gt_pch_nx_hash_map_ctf_dtdef_ref_ctf_dtdef_ref_
  },
  {
    &funcs,
    1,
    sizeof (funcs),
    &gt_ggc_mx_vec_ctf_dtdef_ref_va_gc_,
    &gt_pch_nx_vec_ctf_dtdef_ref_va_gc_
  },
  {
    &btf_info_section,
    1,
    sizeof (btf_info_section),
    &gt_ggc_mx_section,
    &gt_pch_nx_section
  },
  LAST_GGC_ROOT_TAB
};

