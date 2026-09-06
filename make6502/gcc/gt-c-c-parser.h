/* Type information for c/c-parser.cc.
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
gt_ggc_mx_c_parser (void *x_p)
{
  struct c_parser * const x = (struct c_parser *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      {
        size_t i0;
        size_t l0 = (size_t)(4);
        for (i0 = 0; i0 != l0; i0++) {
          gt_ggc_m_9tree_node ((*x).tokens_buf[i0].value);
        }
      }
      gt_ggc_m_18vec_c_token_va_gc_ ((*x).raw_tokens);
      gt_ggc_m_18vec_c_token_va_gc_ ((*x).in_omp_attribute_pragma);
      gt_ggc_m_9tree_node ((*x).in_omp_decl_attribute);
    }
}

void
gt_pch_nx_c_parser (void *x_p)
{
  struct c_parser * const x = (struct c_parser *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_8c_parser))
    {
      {
        size_t i0;
        size_t l0 = (size_t)(4);
        for (i0 = 0; i0 != l0; i0++) {
          gt_pch_n_9tree_node ((*x).tokens_buf[i0].value);
        }
      }
      gt_pch_n_18vec_c_token_va_gc_ ((*x).raw_tokens);
      gt_pch_n_18vec_c_token_va_gc_ ((*x).in_omp_attribute_pragma);
      gt_pch_n_9tree_node ((*x).in_omp_decl_attribute);
    }
}

void
gt_pch_p_8c_parser (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct c_parser * x ATTRIBUTE_UNUSED = (struct c_parser *)x_p;
  {
    size_t i0;
    size_t l0 = (size_t)(4);
    for (i0 = 0; i0 != l0; i0++) {
      if ((void *)(x) == this_obj)
        op (&((*x).tokens_buf[i0].value), NULL, cookie);
    }
  }
  if ((void *)(x) == this_obj)
    op (&((*x).raw_tokens), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).in_omp_attribute_pragma), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).in_omp_decl_attribute), NULL, cookie);
}

/* GC roots.  */

EXPORTED_CONST struct ggc_root_tab gt_ggc_r_gt_c_c_parser_h[] = {
  {
    &the_parser,
    1,
    sizeof (the_parser),
    &gt_ggc_mx_c_parser,
    &gt_pch_nx_c_parser
  },
  LAST_GGC_ROOT_TAB
};

