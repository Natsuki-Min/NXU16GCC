/* Type information for rust.
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
gt_ggc_mx_lang_type (void *x_p)
{
  struct lang_type * const x = (struct lang_type *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_9tree_node ((*x).primary_base);
      gt_ggc_m_27vec_rust_tree_pair_s_va_gc_ ((*x).vcall_indices);
      gt_ggc_m_9tree_node ((*x).vtables);
      gt_ggc_m_9tree_node ((*x).typeinfo_var);
      gt_ggc_m_15vec_tree_va_gc_ ((*x).vbases);
      gt_ggc_m_9tree_node ((*x).as_base);
      gt_ggc_m_15vec_tree_va_gc_ ((*x).pure_virtuals);
      gt_ggc_m_9tree_node ((*x).friend_classes);
      gt_ggc_m_15vec_tree_va_gc_ ((*x).members);
      gt_ggc_m_9tree_node ((*x).key_method);
      gt_ggc_m_9tree_node ((*x).decl_list);
      gt_ggc_m_9tree_node ((*x).befriending_classes);
      gt_ggc_m_9tree_node ((*x).objc_info);
      gt_ggc_m_9tree_node ((*x).lambda_expr);
    }
}

void
gt_ggc_mx_lang_decl (void *x_p)
{
  struct lang_decl * const x = (struct lang_decl *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      switch ((int) (((*x).u).base.selector))
        {
        default:
          break;
        case lds_min:
          gt_ggc_m_9tree_node ((*x).u.min.template_info);
          gt_ggc_m_9tree_node ((*x).u.min.access);
          break;
        case lds_fn:
          gt_ggc_m_9tree_node ((*x).u.fn.min.template_info);
          gt_ggc_m_9tree_node ((*x).u.fn.min.access);
          gt_ggc_m_9tree_node ((*x).u.fn.befriending_classes);
          gt_ggc_m_9tree_node ((*x).u.fn.context);
          switch ((int) (((*x).u.fn).thunk_p))
            {
            case 0:
              gt_ggc_m_9tree_node ((*x).u.fn.u5.cloned_function);
              break;
            case 1:
              break;
            default:
              break;
            }
          gt_ggc_m_9tree_node ((*x).u.fn.saved_auto_return_type);
          break;
        case lds_ns:
          gt_ggc_m_15vec_tree_va_gc_ ((*x).u.ns.inlinees);
          gt_ggc_m_32hash_table_rust_named_decl_hash_ ((*x).u.ns.bindings);
          break;
        case lds_parm:
          break;
        case lds_decomp:
          gt_ggc_m_9tree_node ((*x).u.decomp.min.template_info);
          gt_ggc_m_9tree_node ((*x).u.decomp.min.access);
          gt_ggc_m_9tree_node ((*x).u.decomp.base);
          break;
        }
    }
}

void
gt_ggc_mx_language_function (void *x_p)
{
  struct language_function * const x = (struct language_function *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_15vec_tree_va_gc_ ((*x).base.local_typedefs);
      gt_ggc_m_9tree_node ((*x).x_cdtor_label);
      gt_ggc_m_9tree_node ((*x).x_current_class_ptr);
      gt_ggc_m_9tree_node ((*x).x_current_class_ref);
      gt_ggc_m_9tree_node ((*x).x_eh_spec_block);
      gt_ggc_m_9tree_node ((*x).x_in_charge_parm);
      gt_ggc_m_9tree_node ((*x).x_vtt_parm);
      gt_ggc_m_9tree_node ((*x).x_return_value);
      gt_ggc_m_33hash_table_rust_named_label_hash_ ((*x).x_named_labels);
      gt_ggc_m_15vec_tree_va_gc_ ((*x).infinite_loops);
    }
}

void
gt_ggc_mx_cxx_binding (void *x_p)
{
  struct cxx_binding * const x = (struct cxx_binding *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_11cxx_binding ((*x).previous);
      gt_ggc_m_9tree_node ((*x).value);
      gt_ggc_m_9tree_node ((*x).type);
    }
}

void
gt_ggc_mx_saved_scope (void *x_p)
{
  struct saved_scope * const x = (struct saved_scope *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_33vec_rust_cxx_saved_binding_va_gc_ ((*x).old_bindings);
      gt_ggc_m_9tree_node ((*x).old_namespace);
      gt_ggc_m_15vec_tree_va_gc_ ((*x).decl_ns_list);
      gt_ggc_m_9tree_node ((*x).class_name);
      gt_ggc_m_9tree_node ((*x).class_type);
      gt_ggc_m_9tree_node ((*x).access_specifier);
      gt_ggc_m_9tree_node ((*x).function_decl);
      gt_ggc_m_15vec_tree_va_gc_ ((*x).lang_base);
      gt_ggc_m_9tree_node ((*x).lang_name);
      gt_ggc_m_9tree_node ((*x).template_parms);
      gt_ggc_m_9tree_node ((*x).x_saved_tree);
      gt_ggc_m_9tree_node ((*x).x_current_class_ptr);
      gt_ggc_m_9tree_node ((*x).x_current_class_ref);
      gt_ggc_m_34vec_omp_declare_target_attr_va_gc_ ((*x).omp_declare_target_attribute);
      gt_ggc_m_11saved_scope ((*x).prev);
    }
}

void
gt_ggc_mx_vec_rust_cxx_saved_binding_va_gc_ (void *x_p)
{
  vec<rust_cxx_saved_binding,va_gc> * const x = (vec<rust_cxx_saved_binding,va_gc> *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_mx (x);
    }
}

void
gt_ggc_mx (struct rust_cxx_saved_binding& x_r ATTRIBUTE_UNUSED)
{
  struct rust_cxx_saved_binding * ATTRIBUTE_UNUSED x = &x_r;
  gt_ggc_m_9tree_node ((*x).identifier);
  gt_ggc_m_11cxx_binding ((*x).binding);
  gt_ggc_m_9tree_node ((*x).real_type_value);
}

void
gt_ggc_mx_vec_omp_declare_target_attr_va_gc_ (void *x_p)
{
  vec<omp_declare_target_attr,va_gc> * const x = (vec<omp_declare_target_attr,va_gc> *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_mx (x);
    }
}

void
gt_ggc_mx (struct omp_declare_target_attr& x_r ATTRIBUTE_UNUSED)
{
  struct omp_declare_target_attr * ATTRIBUTE_UNUSED x = &x_r;
}

void
gt_ggc_mx_vec_rust_cp_class_binding_va_gc_ (void *x_p)
{
  vec<rust_cp_class_binding,va_gc> * const x = (vec<rust_cp_class_binding,va_gc> *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_mx (x);
    }
}

void
gt_ggc_mx (struct rust_cp_class_binding& x_r ATTRIBUTE_UNUSED)
{
  struct rust_cp_class_binding * ATTRIBUTE_UNUSED x = &x_r;
  gt_ggc_m_11cxx_binding ((*x).base);
  gt_ggc_m_9tree_node ((*x).identifier);
}

void
gt_ggc_mx_rust_cp_binding_level (void *x_p)
{
  struct rust_cp_binding_level * const x = (struct rust_cp_binding_level *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_9tree_node ((*x).names);
      gt_ggc_m_15vec_tree_va_gc_ ((*x).using_directives);
      gt_ggc_m_32vec_rust_cp_class_binding_va_gc_ ((*x).class_shadowed);
      gt_ggc_m_9tree_node ((*x).type_shadowed);
      gt_ggc_m_9tree_node ((*x).blocks);
      gt_ggc_m_9tree_node ((*x).this_entity);
      gt_ggc_m_21rust_cp_binding_level ((*x).level_chain);
      gt_ggc_m_9tree_node ((*x).statement_list);
    }
}

void
gt_ggc_mx_rust_named_label_entry (void *x_p)
{
  struct rust_named_label_entry * const x = (struct rust_named_label_entry *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_9tree_node ((*x).name);
      gt_ggc_m_9tree_node ((*x).label_decl);
      gt_ggc_m_22rust_named_label_entry ((*x).outer);
      gt_ggc_m_21rust_cp_binding_level ((*x).binding_level);
      gt_ggc_m_9tree_node ((*x).names_in_scope);
      gt_ggc_m_15vec_tree_va_gc_ ((*x).bad_decls);
    }
}

void
gt_ggc_mx (struct rust_named_label_entry& x_r ATTRIBUTE_UNUSED)
{
  struct rust_named_label_entry * ATTRIBUTE_UNUSED x = &x_r;
  gt_ggc_m_9tree_node ((*x).name);
  gt_ggc_m_9tree_node ((*x).label_decl);
  gt_ggc_m_22rust_named_label_entry ((*x).outer);
  gt_ggc_m_21rust_cp_binding_level ((*x).binding_level);
  gt_ggc_m_9tree_node ((*x).names_in_scope);
  gt_ggc_m_15vec_tree_va_gc_ ((*x).bad_decls);
}

void
gt_ggc_mx (struct rust_named_label_entry *& x)
{
  if (x)
    gt_ggc_mx_rust_named_label_entry ((void *) x);
}

void
gt_ggc_mx_hash_table_rust_named_label_hash_ (void *x_p)
{
  hash_table<rust_named_label_hash> * const x = (hash_table<rust_named_label_hash> *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_mx (x);
    }
}

void
gt_ggc_mx (struct rust_named_label_hash& x_r ATTRIBUTE_UNUSED)
{
  struct rust_named_label_hash * ATTRIBUTE_UNUSED x = &x_r;
}

void
gt_ggc_mx_hash_table_rust_named_decl_hash_ (void *x_p)
{
  hash_table<rust_named_decl_hash> * const x = (hash_table<rust_named_decl_hash> *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_mx (x);
    }
}

void
gt_ggc_mx (struct rust_named_decl_hash& x_r ATTRIBUTE_UNUSED)
{
  struct rust_named_decl_hash * ATTRIBUTE_UNUSED x = &x_r;
}

void
gt_ggc_mx_vec_rust_tree_pair_s_va_gc_ (void *x_p)
{
  vec<rust_tree_pair_s,va_gc> * const x = (vec<rust_tree_pair_s,va_gc> *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_mx (x);
    }
}

void
gt_ggc_mx (struct rust_tree_pair_s& x_r ATTRIBUTE_UNUSED)
{
  struct rust_tree_pair_s * ATTRIBUTE_UNUSED x = &x_r;
  gt_ggc_m_9tree_node ((*x).purpose);
  gt_ggc_m_9tree_node ((*x).value);
}

void
gt_pch_nx_lang_type (void *x_p)
{
  struct lang_type * const x = (struct lang_type *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_9lang_type))
    {
      gt_pch_n_9tree_node ((*x).primary_base);
      gt_pch_n_27vec_rust_tree_pair_s_va_gc_ ((*x).vcall_indices);
      gt_pch_n_9tree_node ((*x).vtables);
      gt_pch_n_9tree_node ((*x).typeinfo_var);
      gt_pch_n_15vec_tree_va_gc_ ((*x).vbases);
      gt_pch_n_9tree_node ((*x).as_base);
      gt_pch_n_15vec_tree_va_gc_ ((*x).pure_virtuals);
      gt_pch_n_9tree_node ((*x).friend_classes);
      gt_pch_n_15vec_tree_va_gc_ ((*x).members);
      gt_pch_note_reorder ((*x).members, (*x).members, resort_type_member_vec);
      gt_pch_n_9tree_node ((*x).key_method);
      gt_pch_n_9tree_node ((*x).decl_list);
      gt_pch_n_9tree_node ((*x).befriending_classes);
      gt_pch_n_9tree_node ((*x).objc_info);
      gt_pch_n_9tree_node ((*x).lambda_expr);
    }
}

void
gt_pch_nx_lang_decl (void *x_p)
{
  struct lang_decl * const x = (struct lang_decl *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_9lang_decl))
    {
      switch ((int) (((*x).u).base.selector))
        {
        default:
          break;
        case lds_min:
          gt_pch_n_9tree_node ((*x).u.min.template_info);
          gt_pch_n_9tree_node ((*x).u.min.access);
          break;
        case lds_fn:
          gt_pch_n_9tree_node ((*x).u.fn.min.template_info);
          gt_pch_n_9tree_node ((*x).u.fn.min.access);
          gt_pch_n_9tree_node ((*x).u.fn.befriending_classes);
          gt_pch_n_9tree_node ((*x).u.fn.context);
          switch ((int) (((*x).u.fn).thunk_p))
            {
            case 0:
              gt_pch_n_9tree_node ((*x).u.fn.u5.cloned_function);
              break;
            case 1:
              break;
            default:
              break;
            }
          gt_pch_n_9tree_node ((*x).u.fn.saved_auto_return_type);
          break;
        case lds_ns:
          gt_pch_n_15vec_tree_va_gc_ ((*x).u.ns.inlinees);
          gt_pch_n_32hash_table_rust_named_decl_hash_ ((*x).u.ns.bindings);
          break;
        case lds_parm:
          break;
        case lds_decomp:
          gt_pch_n_9tree_node ((*x).u.decomp.min.template_info);
          gt_pch_n_9tree_node ((*x).u.decomp.min.access);
          gt_pch_n_9tree_node ((*x).u.decomp.base);
          break;
        }
    }
}

void
gt_pch_nx_language_function (void *x_p)
{
  struct language_function * const x = (struct language_function *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_17language_function))
    {
      gt_pch_n_15vec_tree_va_gc_ ((*x).base.local_typedefs);
      gt_pch_n_9tree_node ((*x).x_cdtor_label);
      gt_pch_n_9tree_node ((*x).x_current_class_ptr);
      gt_pch_n_9tree_node ((*x).x_current_class_ref);
      gt_pch_n_9tree_node ((*x).x_eh_spec_block);
      gt_pch_n_9tree_node ((*x).x_in_charge_parm);
      gt_pch_n_9tree_node ((*x).x_vtt_parm);
      gt_pch_n_9tree_node ((*x).x_return_value);
      gt_pch_n_33hash_table_rust_named_label_hash_ ((*x).x_named_labels);
      gt_pch_n_15vec_tree_va_gc_ ((*x).infinite_loops);
    }
}

void
gt_pch_nx_cxx_binding (void *x_p)
{
  struct cxx_binding * const x = (struct cxx_binding *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_11cxx_binding))
    {
      gt_pch_n_11cxx_binding ((*x).previous);
      gt_pch_n_9tree_node ((*x).value);
      gt_pch_n_9tree_node ((*x).type);
    }
}

void
gt_pch_nx_saved_scope (void *x_p)
{
  struct saved_scope * const x = (struct saved_scope *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_11saved_scope))
    {
      gt_pch_n_33vec_rust_cxx_saved_binding_va_gc_ ((*x).old_bindings);
      gt_pch_n_9tree_node ((*x).old_namespace);
      gt_pch_n_15vec_tree_va_gc_ ((*x).decl_ns_list);
      gt_pch_n_9tree_node ((*x).class_name);
      gt_pch_n_9tree_node ((*x).class_type);
      gt_pch_n_9tree_node ((*x).access_specifier);
      gt_pch_n_9tree_node ((*x).function_decl);
      gt_pch_n_15vec_tree_va_gc_ ((*x).lang_base);
      gt_pch_n_9tree_node ((*x).lang_name);
      gt_pch_n_9tree_node ((*x).template_parms);
      gt_pch_n_9tree_node ((*x).x_saved_tree);
      gt_pch_n_9tree_node ((*x).x_current_class_ptr);
      gt_pch_n_9tree_node ((*x).x_current_class_ref);
      gt_pch_n_34vec_omp_declare_target_attr_va_gc_ ((*x).omp_declare_target_attribute);
      gt_pch_n_11saved_scope ((*x).prev);
    }
}

void
gt_pch_nx_vec_rust_cxx_saved_binding_va_gc_ (void *x_p)
{
  vec<rust_cxx_saved_binding,va_gc> * const x = (vec<rust_cxx_saved_binding,va_gc> *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_33vec_rust_cxx_saved_binding_va_gc_))
    {
      gt_pch_nx (x);
    }
}

void
gt_pch_nx (struct rust_cxx_saved_binding& x_r ATTRIBUTE_UNUSED)
{
  struct rust_cxx_saved_binding * ATTRIBUTE_UNUSED x = &x_r;
  gt_pch_n_9tree_node ((*x).identifier);
  gt_pch_n_11cxx_binding ((*x).binding);
  gt_pch_n_9tree_node ((*x).real_type_value);
}

void
gt_pch_nx_vec_omp_declare_target_attr_va_gc_ (void *x_p)
{
  vec<omp_declare_target_attr,va_gc> * const x = (vec<omp_declare_target_attr,va_gc> *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_34vec_omp_declare_target_attr_va_gc_))
    {
      gt_pch_nx (x);
    }
}

void
gt_pch_nx (struct omp_declare_target_attr& x_r ATTRIBUTE_UNUSED)
{
  struct omp_declare_target_attr * ATTRIBUTE_UNUSED x = &x_r;
}

void
gt_pch_nx_vec_rust_cp_class_binding_va_gc_ (void *x_p)
{
  vec<rust_cp_class_binding,va_gc> * const x = (vec<rust_cp_class_binding,va_gc> *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_32vec_rust_cp_class_binding_va_gc_))
    {
      gt_pch_nx (x);
    }
}

void
gt_pch_nx (struct rust_cp_class_binding& x_r ATTRIBUTE_UNUSED)
{
  struct rust_cp_class_binding * ATTRIBUTE_UNUSED x = &x_r;
  gt_pch_n_11cxx_binding ((*x).base);
  gt_pch_n_9tree_node ((*x).identifier);
}

void
gt_pch_nx_rust_cp_binding_level (void *x_p)
{
  struct rust_cp_binding_level * const x = (struct rust_cp_binding_level *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_21rust_cp_binding_level))
    {
      gt_pch_n_9tree_node ((*x).names);
      gt_pch_n_15vec_tree_va_gc_ ((*x).using_directives);
      gt_pch_n_32vec_rust_cp_class_binding_va_gc_ ((*x).class_shadowed);
      gt_pch_n_9tree_node ((*x).type_shadowed);
      gt_pch_n_9tree_node ((*x).blocks);
      gt_pch_n_9tree_node ((*x).this_entity);
      gt_pch_n_21rust_cp_binding_level ((*x).level_chain);
      gt_pch_n_9tree_node ((*x).statement_list);
    }
}

void
gt_pch_nx_rust_named_label_entry (void *x_p)
{
  struct rust_named_label_entry * const x = (struct rust_named_label_entry *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_22rust_named_label_entry))
    {
      gt_pch_n_9tree_node ((*x).name);
      gt_pch_n_9tree_node ((*x).label_decl);
      gt_pch_n_22rust_named_label_entry ((*x).outer);
      gt_pch_n_21rust_cp_binding_level ((*x).binding_level);
      gt_pch_n_9tree_node ((*x).names_in_scope);
      gt_pch_n_15vec_tree_va_gc_ ((*x).bad_decls);
    }
}

void
gt_pch_nx (struct rust_named_label_entry& x_r ATTRIBUTE_UNUSED)
{
  struct rust_named_label_entry * ATTRIBUTE_UNUSED x = &x_r;
  gt_pch_n_9tree_node ((*x).name);
  gt_pch_n_9tree_node ((*x).label_decl);
  gt_pch_n_22rust_named_label_entry ((*x).outer);
  gt_pch_n_21rust_cp_binding_level ((*x).binding_level);
  gt_pch_n_9tree_node ((*x).names_in_scope);
  gt_pch_n_15vec_tree_va_gc_ ((*x).bad_decls);
}

void
gt_pch_nx (struct rust_named_label_entry *& x)
{
  if (x)
    gt_pch_nx_rust_named_label_entry ((void *) x);
}

void
gt_pch_nx_hash_table_rust_named_label_hash_ (void *x_p)
{
  hash_table<rust_named_label_hash> * const x = (hash_table<rust_named_label_hash> *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_33hash_table_rust_named_label_hash_))
    {
      gt_pch_nx (x);
    }
}

void
gt_pch_nx (struct rust_named_label_hash& x_r ATTRIBUTE_UNUSED)
{
  struct rust_named_label_hash * ATTRIBUTE_UNUSED x = &x_r;
}

void
gt_pch_nx_hash_table_rust_named_decl_hash_ (void *x_p)
{
  hash_table<rust_named_decl_hash> * const x = (hash_table<rust_named_decl_hash> *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_32hash_table_rust_named_decl_hash_))
    {
      gt_pch_nx (x);
    }
}

void
gt_pch_nx (struct rust_named_decl_hash& x_r ATTRIBUTE_UNUSED)
{
  struct rust_named_decl_hash * ATTRIBUTE_UNUSED x = &x_r;
}

void
gt_pch_nx_vec_rust_tree_pair_s_va_gc_ (void *x_p)
{
  vec<rust_tree_pair_s,va_gc> * const x = (vec<rust_tree_pair_s,va_gc> *)x_p;
  if (gt_pch_note_object (x, x, gt_pch_p_27vec_rust_tree_pair_s_va_gc_))
    {
      gt_pch_nx (x);
    }
}

void
gt_pch_nx (struct rust_tree_pair_s& x_r ATTRIBUTE_UNUSED)
{
  struct rust_tree_pair_s * ATTRIBUTE_UNUSED x = &x_r;
  gt_pch_n_9tree_node ((*x).purpose);
  gt_pch_n_9tree_node ((*x).value);
}

void
gt_pch_p_9lang_type (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct lang_type * x ATTRIBUTE_UNUSED = (struct lang_type *)x_p;
  if ((void *)(x) == this_obj)
    op (&((*x).primary_base), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).vcall_indices), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).vtables), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).typeinfo_var), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).vbases), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).as_base), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).pure_virtuals), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).friend_classes), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).members), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).key_method), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).decl_list), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).befriending_classes), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).objc_info), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).lambda_expr), NULL, cookie);
}

void
gt_pch_p_9lang_decl (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct lang_decl * x ATTRIBUTE_UNUSED = (struct lang_decl *)x_p;
  switch ((int) (((*x).u).base.selector))
    {
    default:
      break;
    case lds_min:
      if ((void *)(x) == this_obj)
        op (&((*x).u.min.template_info), NULL, cookie);
      if ((void *)(x) == this_obj)
        op (&((*x).u.min.access), NULL, cookie);
      break;
    case lds_fn:
      if ((void *)(x) == this_obj)
        op (&((*x).u.fn.min.template_info), NULL, cookie);
      if ((void *)(x) == this_obj)
        op (&((*x).u.fn.min.access), NULL, cookie);
      if ((void *)(x) == this_obj)
        op (&((*x).u.fn.befriending_classes), NULL, cookie);
      if ((void *)(x) == this_obj)
        op (&((*x).u.fn.context), NULL, cookie);
      switch ((int) (((*x).u.fn).thunk_p))
        {
        case 0:
          if ((void *)(x) == this_obj)
            op (&((*x).u.fn.u5.cloned_function), NULL, cookie);
          break;
        case 1:
          break;
        default:
          break;
        }
      if ((void *)(x) == this_obj)
        op (&((*x).u.fn.saved_auto_return_type), NULL, cookie);
      break;
    case lds_ns:
      if ((void *)(x) == this_obj)
        op (&((*x).u.ns.inlinees), NULL, cookie);
      if ((void *)(x) == this_obj)
        op (&((*x).u.ns.bindings), NULL, cookie);
      break;
    case lds_parm:
      break;
    case lds_decomp:
      if ((void *)(x) == this_obj)
        op (&((*x).u.decomp.min.template_info), NULL, cookie);
      if ((void *)(x) == this_obj)
        op (&((*x).u.decomp.min.access), NULL, cookie);
      if ((void *)(x) == this_obj)
        op (&((*x).u.decomp.base), NULL, cookie);
      break;
    }
}

void
gt_pch_p_17language_function (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct language_function * x ATTRIBUTE_UNUSED = (struct language_function *)x_p;
  if ((void *)(x) == this_obj)
    op (&((*x).base.local_typedefs), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).x_cdtor_label), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).x_current_class_ptr), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).x_current_class_ref), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).x_eh_spec_block), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).x_in_charge_parm), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).x_vtt_parm), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).x_return_value), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).x_named_labels), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).infinite_loops), NULL, cookie);
}

void
gt_pch_p_11cxx_binding (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct cxx_binding * x ATTRIBUTE_UNUSED = (struct cxx_binding *)x_p;
  if ((void *)(x) == this_obj)
    op (&((*x).previous), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).value), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).type), NULL, cookie);
}

void
gt_pch_p_11saved_scope (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct saved_scope * x ATTRIBUTE_UNUSED = (struct saved_scope *)x_p;
  if ((void *)(x) == this_obj)
    op (&((*x).old_bindings), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).old_namespace), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).decl_ns_list), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).class_name), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).class_type), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).access_specifier), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).function_decl), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).lang_base), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).lang_name), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).template_parms), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).x_saved_tree), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).x_current_class_ptr), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).x_current_class_ref), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).omp_declare_target_attribute), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).prev), NULL, cookie);
}

void
gt_pch_p_33vec_rust_cxx_saved_binding_va_gc_ (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct vec<rust_cxx_saved_binding,va_gc> * x ATTRIBUTE_UNUSED = (struct vec<rust_cxx_saved_binding,va_gc> *)x_p;
  if ((void *)(x) == this_obj)
    gt_pch_nx (&((*x)), op, cookie);
}

void
gt_pch_nx (struct rust_cxx_saved_binding* x ATTRIBUTE_UNUSED,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
    op (&((*x).identifier), NULL, cookie);
    op (&((*x).binding), NULL, cookie);
    op (&((*x).real_type_value), NULL, cookie);
}

void
gt_pch_p_34vec_omp_declare_target_attr_va_gc_ (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct vec<omp_declare_target_attr,va_gc> * x ATTRIBUTE_UNUSED = (struct vec<omp_declare_target_attr,va_gc> *)x_p;
  if ((void *)(x) == this_obj)
    gt_pch_nx (&((*x)), op, cookie);
}

void
gt_pch_nx (struct omp_declare_target_attr* x ATTRIBUTE_UNUSED,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
}

void
gt_pch_p_32vec_rust_cp_class_binding_va_gc_ (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct vec<rust_cp_class_binding,va_gc> * x ATTRIBUTE_UNUSED = (struct vec<rust_cp_class_binding,va_gc> *)x_p;
  if ((void *)(x) == this_obj)
    gt_pch_nx (&((*x)), op, cookie);
}

void
gt_pch_nx (struct rust_cp_class_binding* x ATTRIBUTE_UNUSED,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
    op (&((*x).base), NULL, cookie);
    op (&((*x).identifier), NULL, cookie);
}

void
gt_pch_p_21rust_cp_binding_level (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct rust_cp_binding_level * x ATTRIBUTE_UNUSED = (struct rust_cp_binding_level *)x_p;
  if ((void *)(x) == this_obj)
    op (&((*x).names), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).using_directives), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).class_shadowed), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).type_shadowed), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).blocks), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).this_entity), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).level_chain), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).statement_list), NULL, cookie);
}

void
gt_pch_p_22rust_named_label_entry (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct rust_named_label_entry * x ATTRIBUTE_UNUSED = (struct rust_named_label_entry *)x_p;
  if ((void *)(x) == this_obj)
    op (&((*x).name), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).label_decl), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).outer), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).binding_level), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).names_in_scope), NULL, cookie);
  if ((void *)(x) == this_obj)
    op (&((*x).bad_decls), NULL, cookie);
}

void
gt_pch_nx (struct rust_named_label_entry* x ATTRIBUTE_UNUSED,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
    op (&((*x).name), NULL, cookie);
    op (&((*x).label_decl), NULL, cookie);
    op (&((*x).outer), NULL, cookie);
    op (&((*x).binding_level), NULL, cookie);
    op (&((*x).names_in_scope), NULL, cookie);
    op (&((*x).bad_decls), NULL, cookie);
}

void
gt_pch_p_33hash_table_rust_named_label_hash_ (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct hash_table<rust_named_label_hash> * x ATTRIBUTE_UNUSED = (struct hash_table<rust_named_label_hash> *)x_p;
  if ((void *)(x) == this_obj)
    gt_pch_nx (&((*x)), op, cookie);
}

void
gt_pch_nx (struct rust_named_label_hash* x ATTRIBUTE_UNUSED,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
}

void
gt_pch_p_32hash_table_rust_named_decl_hash_ (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct hash_table<rust_named_decl_hash> * x ATTRIBUTE_UNUSED = (struct hash_table<rust_named_decl_hash> *)x_p;
  if ((void *)(x) == this_obj)
    gt_pch_nx (&((*x)), op, cookie);
}

void
gt_pch_nx (struct rust_named_decl_hash* x ATTRIBUTE_UNUSED,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
}

void
gt_pch_p_27vec_rust_tree_pair_s_va_gc_ (ATTRIBUTE_UNUSED void *this_obj,
	void *x_p,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
  struct vec<rust_tree_pair_s,va_gc> * x ATTRIBUTE_UNUSED = (struct vec<rust_tree_pair_s,va_gc> *)x_p;
  if ((void *)(x) == this_obj)
    gt_pch_nx (&((*x)), op, cookie);
}

void
gt_pch_nx (struct rust_tree_pair_s* x ATTRIBUTE_UNUSED,
	ATTRIBUTE_UNUSED gt_pointer_operator op,
	ATTRIBUTE_UNUSED void *cookie)
{
    op (&((*x).purpose), NULL, cookie);
    op (&((*x).value), NULL, cookie);
}

/* GC roots.  */

EXPORTED_CONST struct ggc_root_tab gt_ggc_r_gtype_rust_h[] = {
  {
    &scope_chain,
    1,
    sizeof (scope_chain),
    &gt_ggc_mx_saved_scope,
    &gt_pch_nx_saved_scope
  },
  {
    &cp_global_trees[0],
    1 * (CPTI_MAX),
    sizeof (cp_global_trees[0]),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  {
    &c_global_trees[0],
    1 * (CTI_MAX),
    sizeof (c_global_trees[0]),
    &gt_ggc_mx_tree_node,
    &gt_pch_nx_tree_node
  },
  LAST_GGC_ROOT_TAB
};

extern const struct ggc_root_tab gt_ggc_r_gt_coverage_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_symtab_thunks_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_caller_save_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_alias_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_attribs_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_cselib_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_cgraph_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_ipa_prop_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_ipa_sra_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_ipa_modref_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_dwarf2asm_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_dwarf2cfi_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_dwarf2ctf_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_dwarf2out_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_ctfout_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_btfout_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_tree_vect_generic_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_dojump_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_emit_rtl_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_explow_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_function_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_except_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_ggc_tests_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_gcse_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_godump_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_optabs_libfuncs_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_cfgrtl_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_stor_layout_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_stringpool_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_tree_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_varasm_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_tree_ssa_address_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_tree_ssa_loop_ivopts_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_gimple_expr_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_tree_scalar_evolution_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_tree_profile_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_tree_nested_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_omp_general_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_omp_low_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_targhooks_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_cgraphclones_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_trans_mem_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_vtable_verify_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_asan_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_ubsan_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_ipa_devirt_h[];
extern const struct ggc_root_tab gt_ggc_r_gtype_desc_cc[];
extern const struct ggc_root_tab gt_ggc_r_gt_analyzer_language_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_rust_rust_lang_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_rust_rust_constexpr_h[];
extern const struct ggc_root_tab gt_ggc_r_gtype_rust_h[];
extern const struct ggc_root_tab gt_ggc_r_gt_rust_rust_tree_h[];
EXPORTED_CONST struct ggc_root_tab * const gt_ggc_rtab[] = {
  gt_ggc_r_gt_coverage_h,
  gt_ggc_r_gt_symtab_thunks_h,
  gt_ggc_r_gt_caller_save_h,
  gt_ggc_r_gt_alias_h,
  gt_ggc_r_gt_attribs_h,
  gt_ggc_r_gt_cselib_h,
  gt_ggc_r_gt_cgraph_h,
  gt_ggc_r_gt_ipa_prop_h,
  gt_ggc_r_gt_ipa_sra_h,
  gt_ggc_r_gt_ipa_modref_h,
  gt_ggc_r_gt_dwarf2asm_h,
  gt_ggc_r_gt_dwarf2cfi_h,
  gt_ggc_r_gt_dwarf2ctf_h,
  gt_ggc_r_gt_dwarf2out_h,
  gt_ggc_r_gt_ctfout_h,
  gt_ggc_r_gt_btfout_h,
  gt_ggc_r_gt_tree_vect_generic_h,
  gt_ggc_r_gt_dojump_h,
  gt_ggc_r_gt_emit_rtl_h,
  gt_ggc_r_gt_explow_h,
  gt_ggc_r_gt_function_h,
  gt_ggc_r_gt_except_h,
  gt_ggc_r_gt_ggc_tests_h,
  gt_ggc_r_gt_gcse_h,
  gt_ggc_r_gt_godump_h,
  gt_ggc_r_gt_optabs_libfuncs_h,
  gt_ggc_r_gt_cfgrtl_h,
  gt_ggc_r_gt_stor_layout_h,
  gt_ggc_r_gt_stringpool_h,
  gt_ggc_r_gt_tree_h,
  gt_ggc_r_gt_varasm_h,
  gt_ggc_r_gt_tree_ssa_address_h,
  gt_ggc_r_gt_tree_ssa_loop_ivopts_h,
  gt_ggc_r_gt_gimple_expr_h,
  gt_ggc_r_gt_tree_scalar_evolution_h,
  gt_ggc_r_gt_tree_profile_h,
  gt_ggc_r_gt_tree_nested_h,
  gt_ggc_r_gt_omp_general_h,
  gt_ggc_r_gt_omp_low_h,
  gt_ggc_r_gt_targhooks_h,
  gt_ggc_r_gt_cgraphclones_h,
  gt_ggc_r_gt_trans_mem_h,
  gt_ggc_r_gt_vtable_verify_h,
  gt_ggc_r_gt_asan_h,
  gt_ggc_r_gt_ubsan_h,
  gt_ggc_r_gt_ipa_devirt_h,
  gt_ggc_r_gtype_desc_cc,
  gt_ggc_r_gt_analyzer_language_h,
  gt_ggc_r_gt_rust_rust_lang_h,
  gt_ggc_r_gt_rust_rust_constexpr_h,
  gt_ggc_r_gtype_rust_h,
  gt_ggc_r_gt_rust_rust_tree_h,
  NULL
};
extern const struct ggc_root_tab gt_ggc_rd_gt_alias_h[];
extern const struct ggc_root_tab gt_ggc_rd_gt_bitmap_h[];
extern const struct ggc_root_tab gt_ggc_rd_gt_emit_rtl_h[];
extern const struct ggc_root_tab gt_ggc_rd_gt_ggc_tests_h[];
extern const struct ggc_root_tab gt_ggc_rd_gt_lists_h[];
extern const struct ggc_root_tab gt_ggc_rd_gt_tree_iterator_h[];
extern const struct ggc_root_tab gt_ggc_rd_gt_tree_phinodes_h[];
extern const struct ggc_root_tab gt_ggc_rd_gt_ipa_strub_h[];
extern const struct ggc_root_tab gt_ggc_rd_gt_rust_rust_constexpr_h[];
extern const struct ggc_root_tab gt_ggc_rd_gt_rust_rust_tree_h[];
EXPORTED_CONST struct ggc_root_tab * const gt_ggc_deletable_rtab[] = {
  gt_ggc_rd_gt_alias_h,
  gt_ggc_rd_gt_bitmap_h,
  gt_ggc_rd_gt_emit_rtl_h,
  gt_ggc_rd_gt_ggc_tests_h,
  gt_ggc_rd_gt_lists_h,
  gt_ggc_rd_gt_tree_iterator_h,
  gt_ggc_rd_gt_tree_phinodes_h,
  gt_ggc_rd_gt_ipa_strub_h,
  gt_ggc_rd_gt_rust_rust_constexpr_h,
  gt_ggc_rd_gt_rust_rust_tree_h,
  NULL
};
extern void gt_clear_caches_gt_ipa_prop_h ();
extern void gt_clear_caches_gt_emit_rtl_h ();
extern void gt_clear_caches_gt_function_h ();
extern void gt_clear_caches_gt_tree_h ();
extern void gt_clear_caches_gt_varasm_h ();
extern void gt_clear_caches_gt_trans_mem_h ();
extern void gt_clear_caches_gt_ubsan_h ();
void
gt_clear_caches ()
{
  gt_clear_caches_gt_ipa_prop_h ();
  gt_clear_caches_gt_emit_rtl_h ();
  gt_clear_caches_gt_function_h ();
  gt_clear_caches_gt_tree_h ();
  gt_clear_caches_gt_varasm_h ();
  gt_clear_caches_gt_trans_mem_h ();
  gt_clear_caches_gt_ubsan_h ();
}
extern const struct ggc_root_tab gt_pch_rs_gt_alias_h[];
extern const struct ggc_root_tab gt_pch_rs_gt_dwarf2asm_h[];
extern const struct ggc_root_tab gt_pch_rs_gt_dwarf2cfi_h[];
extern const struct ggc_root_tab gt_pch_rs_gt_dwarf2out_h[];
extern const struct ggc_root_tab gt_pch_rs_gt_tree_vect_generic_h[];
extern const struct ggc_root_tab gt_pch_rs_gt_emit_rtl_h[];
extern const struct ggc_root_tab gt_pch_rs_gt_function_h[];
extern const struct ggc_root_tab gt_pch_rs_gt_except_h[];
extern const struct ggc_root_tab gt_pch_rs_gt_tree_h[];
extern const struct ggc_root_tab gt_pch_rs_gt_varasm_h[];
extern const struct ggc_root_tab gt_pch_rs_gt_gimple_expr_h[];
extern const struct ggc_root_tab gt_pch_rs_gt_ubsan_h[];
extern const struct ggc_root_tab gt_pch_rs_gtype_desc_cc[];
extern const struct ggc_root_tab gt_pch_rs_gt_rust_rust_tree_h[];
EXPORTED_CONST struct ggc_root_tab * const gt_pch_scalar_rtab[] = {
  gt_pch_rs_gt_alias_h,
  gt_pch_rs_gt_dwarf2asm_h,
  gt_pch_rs_gt_dwarf2cfi_h,
  gt_pch_rs_gt_dwarf2out_h,
  gt_pch_rs_gt_tree_vect_generic_h,
  gt_pch_rs_gt_emit_rtl_h,
  gt_pch_rs_gt_function_h,
  gt_pch_rs_gt_except_h,
  gt_pch_rs_gt_tree_h,
  gt_pch_rs_gt_varasm_h,
  gt_pch_rs_gt_gimple_expr_h,
  gt_pch_rs_gt_ubsan_h,
  gt_pch_rs_gtype_desc_cc,
  gt_pch_rs_gt_rust_rust_tree_h,
  NULL
};
