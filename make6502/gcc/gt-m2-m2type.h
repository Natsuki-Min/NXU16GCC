/* Type information for m2/gm2-gcc/m2type.cc.
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
gt_ggc_mx_struct_constructor (void *x_p)
{
  struct struct_constructor * const x = (struct struct_constructor *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_26vec_constructor_elt_va_gc_ ((*x).constructor_elements);
      gt_ggc_m_18struct_constructor ((*x).level);
    }
}

void
gt_ggc_mx_array_desc (void *x_p)
{
  struct array_desc * const x = (struct array_desc *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_9tree_node ((*x).index);
      gt_ggc_m_9tree_node ((*x).array);
      gt_ggc_m_10array_desc ((*x).next);
    }
}

void
gt_pch_nx_struct_constructor (void *x_p)
{
  struct struct_constructor * const x = (struct struct_constructor *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_18struct_constructor))
    {
      gt_pch_n_26vec_constructor_elt_va_gc_ ((*x).constructor_elements);
      gt_pch_n_18struct_constructor ((*x).level);
    }
}

void
gt_pch_nx_array_desc (void *x_p)
{
  struct array_desc * const x = (struct array_desc *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_10array_desc))
    {
      gt_pch_n_9tree_node ((*x).index);
      gt_pch_n_9tree_node ((*x).array);
      gt_pch_n_10array_desc ((*x).next);
    }
}

void
gt_pch_p_18struct_constructor (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct struct_constructor * x ATTRIBUTE_UNUSED = (struct struct_constructor *)x_p;
  if ((void *)(x) == this_obj)
    op (&((*x).constructor_elements), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).level), NULL, cookie);
}

void
gt_pch_p_10array_desc (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct array_desc * x ATTRIBUTE_UNUSED = (struct array_desc *)x_p;
  if ((void *)(x) == this_obj)
    op (&((*x).index), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).array), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).next), NULL, cookie);
}

/* GC roots.  */

EXPORTED_CONST struct ggc_root_tab gt_ggc_r_gt_m2_m2type_h[] = {
  {
    &m2_cardinal_address_type_node,
    1,
    sizeof (m2_cardinal_address_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_packed_boolean_type_node,
    1,
    sizeof (m2_packed_boolean_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_complex128_type_node,
    1,
    sizeof (m2_complex128_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_complex96_type_node,
    1,
    sizeof (m2_complex96_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_complex64_type_node,
    1,
    sizeof (m2_complex64_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_complex32_type_node,
    1,
    sizeof (m2_complex32_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_c_type_node,
    1,
    sizeof (m2_c_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_short_complex_type_node,
    1,
    sizeof (m2_short_complex_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_long_complex_type_node,
    1,
    sizeof (m2_long_complex_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_complex_type_node,
    1,
    sizeof (m2_complex_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_real128_type_node,
    1,
    sizeof (m2_real128_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_real96_type_node,
    1,
    sizeof (m2_real96_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_real64_type_node,
    1,
    sizeof (m2_real64_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_real32_type_node,
    1,
    sizeof (m2_real32_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_bitset32_type_node,
    1,
    sizeof (m2_bitset32_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_bitset16_type_node,
    1,
    sizeof (m2_bitset16_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_bitset8_type_node,
    1,
    sizeof (m2_bitset8_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_word64_type_node,
    1,
    sizeof (m2_word64_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_word32_type_node,
    1,
    sizeof (m2_word32_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_word16_type_node,
    1,
    sizeof (m2_word16_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_cardinal64_type_node,
    1,
    sizeof (m2_cardinal64_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_cardinal32_type_node,
    1,
    sizeof (m2_cardinal32_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_cardinal16_type_node,
    1,
    sizeof (m2_cardinal16_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_cardinal8_type_node,
    1,
    sizeof (m2_cardinal8_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_integer64_type_node,
    1,
    sizeof (m2_integer64_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_integer32_type_node,
    1,
    sizeof (m2_integer32_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_integer16_type_node,
    1,
    sizeof (m2_integer16_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_integer8_type_node,
    1,
    sizeof (m2_integer8_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_iso_word_type_node,
    1,
    sizeof (m2_iso_word_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_iso_byte_type_node,
    1,
    sizeof (m2_iso_byte_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_iso_loc_type_node,
    1,
    sizeof (m2_iso_loc_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_z_type_node,
    1,
    sizeof (m2_z_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_short_card_type_node,
    1,
    sizeof (m2_short_card_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_short_int_type_node,
    1,
    sizeof (m2_short_int_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_long_card_type_node,
    1,
    sizeof (m2_long_card_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_long_int_type_node,
    1,
    sizeof (m2_long_int_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_long_real_type_node,
    1,
    sizeof (m2_long_real_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_real_type_node,
    1,
    sizeof (m2_real_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_short_real_type_node,
    1,
    sizeof (m2_short_real_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_cardinal_type_node,
    1,
    sizeof (m2_cardinal_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_integer_type_node,
    1,
    sizeof (m2_integer_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &m2_char_type_node,
    1,
    sizeof (m2_char_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &bitnum_type_node,
    1,
    sizeof (bitnum_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &bitset_type_node,
    1,
    sizeof (bitset_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &proc_type_node,
    1,
    sizeof (proc_type_node),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &param_type_list,
    1,
    sizeof (param_type_list),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &list_of_arrays,
    1,
    sizeof (list_of_arrays),
    &gt_ggc_mx_array_desc,
    &gt_pch_nx_array_desc
  },
  {
    &top_constructor,
    1,
    sizeof (top_constructor),
    &gt_ggc_mx_struct_constructor,
    &gt_pch_nx_struct_constructor
  },
  LAST_GGC_ROOT_TAB
};

