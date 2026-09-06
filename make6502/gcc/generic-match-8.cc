/* Generated automatically by the program `genmatch' from
   a IL pattern matching and simplification description.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-function"

#include "generic-match-auto.h"

bool
tree_bit_xor_cst (tree t, tree *res_ops)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  switch (TREE_CODE (t))
    {
    case BIT_XOR_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	if (uniform_integer_cst_p (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	      {
		res_ops[0] = captures[0];
		res_ops[1] = captures[1];
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 4, __FILE__, __LINE__, false);
		return true;
	      }
	    }
	  }
        break;
      }
    default:;
    }
  return false;
}

bool
tree_with_possible_nonzero_bits (tree t)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  switch (TREE_CODE (t))
    {
    case INTEGER_CST:
      {
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { t };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 32, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case SSA_NAME:
      {
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { t };
	  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
	    {
	      {
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 33, __FILE__, __LINE__, false);
		return true;
	      }
	    }
	}
        break;
      }
    default:;
    }
  return false;
}

bool
tree_usadd_left_part_1 (tree t, tree *res_ops)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  switch (TREE_CODE (t))
    {
    case PLUS_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
	    {
	      {
		res_ops[0] = captures[0];
		res_ops[1] = captures[1];
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 40, __FILE__, __LINE__, false);
		return true;
	      }
	    }
	}
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p1, _p0 };
	  if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
	    {
	      {
		res_ops[0] = captures[0];
		res_ops[1] = captures[1];
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 40, __FILE__, __LINE__, false);
		return true;
	      }
	    }
	}
        break;
      }
    default:;
    }
  return false;
}

bool
tree_signed_integer_sat_trunc (tree t, tree *res_ops)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  switch (TREE_CODE (t))
    {
    case COND_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	tree _p2 = TREE_OPERAND (t, 2);
	switch (TREE_CODE (_p0))
	  {
	  case GT_EXPR:
	    {
	      tree _q20 = TREE_OPERAND (_p0, 0);
	      tree _q21 = TREE_OPERAND (_p0, 1);
	      switch (TREE_CODE (_q20))
	        {
		case PLUS_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    switch (TREE_CODE (_q30))
		      {
		      CASE_CONVERT:
		        {
			  tree _q40 = TREE_OPERAND (_q30, 0);
			  switch (TREE_CODE (_q31))
			    {
			    case INTEGER_CST:
			      {
				switch (TREE_CODE (_q21))
				  {
				  case INTEGER_CST:
				    {
				      switch (TREE_CODE (_p1))
				        {
					case BIT_XOR_EXPR:
					  {
					    tree _q80 = TREE_OPERAND (_p1, 0);
					    tree _q81 = TREE_OPERAND (_p1, 1);
					    switch (TREE_CODE (_q80))
					      {
					      case NEGATE_EXPR:
					        {
						  tree _q90 = TREE_OPERAND (_q80, 0);
						  switch (TREE_CODE (_q90))
						    {
						    CASE_CONVERT:
						      {
							tree _q100 = TREE_OPERAND (_q90, 0);
							switch (TREE_CODE (_q100))
							  {
							  case LT_EXPR:
							    {
							      tree _q110 = TREE_OPERAND (_q100, 0);
							      tree _q111 = TREE_OPERAND (_q100, 1);
							      if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
								{
								  if (integer_zerop (_q111))
								    {
								      switch (TREE_CODE (_q81))
								        {
									case INTEGER_CST:
									  {
									    switch (TREE_CODE (_p2))
									      {
									      CASE_CONVERT:
									        {
										  tree _q150 = TREE_OPERAND (_p2, 0);
										  if ((_q150 == _q40 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q40, 0) && types_match (_q150, _q40)))
										    {
										      {
											tree captures[5] ATTRIBUTE_UNUSED = { _q40, _q30, _q31, _q21, _q81 };
											if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
 && !TYPE_UNSIGNED (TREE_TYPE (captures[0])) && TYPE_UNSIGNED (TREE_TYPE (captures[1]))
)
											  {
											    {
 unsigned itype_prec = TYPE_PRECISION (TREE_TYPE (captures[0]));
 unsigned otype_prec = TYPE_PRECISION (type);
 wide_int offset = wi::uhwi (HOST_WIDE_INT_1U << (otype_prec - 1),
 itype_prec);
 wide_int limit_0 = wi::mask (otype_prec, false, itype_prec);
 wide_int limit_1 = wi::uhwi ((HOST_WIDE_INT_1U << otype_prec) - 3,
 itype_prec);
 wide_int limit_2 = wi::uhwi ((HOST_WIDE_INT_1U << otype_prec) - 2,
 itype_prec);
 wide_int otype_max = wi::mask (otype_prec - 1, false, otype_prec);
 wide_int itype_max = wi::mask (otype_prec - 1, false, itype_prec);
 wide_int int_cst_1 = wi::to_wide (captures[2]);
 wide_int int_cst_2 = wi::to_wide (captures[3]);
 wide_int int_cst_3 = wi::to_wide (captures[4]);
												if (((wi::eq_p (int_cst_1, offset) && wi::eq_p (int_cst_2, limit_0))
 || (wi::eq_p (int_cst_1, itype_max) && wi::eq_p (int_cst_2, limit_2))
 || (wi::eq_p (int_cst_1, offset) && wi::eq_p (int_cst_2, limit_2))
 || (wi::eq_p (int_cst_1, itype_max) && wi::eq_p (int_cst_2, limit_1)))
 && wi::eq_p (int_cst_3, otype_max)
)
												  {
												    {
												      res_ops[0] = captures[0];
												      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 78, __FILE__, __LINE__, false);
												      return true;
												    }
												  }
											    }
											  }
										      }
										    }
									          break;
									        }
									      default:;
									      }
									    break;
									  }
								        default:;
								        }
								    }
							        }
							      break;
							    }
						          default:;
						          }
						        break;
						      }
						    default:;
						    }
					          break;
					        }
					      case INTEGER_CST:
					        {
						  switch (TREE_CODE (_q81))
						    {
						    case NEGATE_EXPR:
						      {
							tree _q100 = TREE_OPERAND (_q81, 0);
							switch (TREE_CODE (_q100))
							  {
							  CASE_CONVERT:
							    {
							      tree _q110 = TREE_OPERAND (_q100, 0);
							      switch (TREE_CODE (_q110))
							        {
								case LT_EXPR:
								  {
								    tree _q120 = TREE_OPERAND (_q110, 0);
								    tree _q121 = TREE_OPERAND (_q110, 1);
								    if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
								      {
									if (integer_zerop (_q121))
									  {
									    switch (TREE_CODE (_p2))
									      {
									      CASE_CONVERT:
									        {
										  tree _q150 = TREE_OPERAND (_p2, 0);
										  if ((_q150 == _q40 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q40, 0) && types_match (_q150, _q40)))
										    {
										      {
											tree captures[5] ATTRIBUTE_UNUSED = { _q40, _q30, _q31, _q21, _q80 };
											if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
 && !TYPE_UNSIGNED (TREE_TYPE (captures[0])) && TYPE_UNSIGNED (TREE_TYPE (captures[1]))
)
											  {
											    {
 unsigned itype_prec = TYPE_PRECISION (TREE_TYPE (captures[0]));
 unsigned otype_prec = TYPE_PRECISION (type);
 wide_int offset = wi::uhwi (HOST_WIDE_INT_1U << (otype_prec - 1),
 itype_prec);
 wide_int limit_0 = wi::mask (otype_prec, false, itype_prec);
 wide_int limit_1 = wi::uhwi ((HOST_WIDE_INT_1U << otype_prec) - 3,
 itype_prec);
 wide_int limit_2 = wi::uhwi ((HOST_WIDE_INT_1U << otype_prec) - 2,
 itype_prec);
 wide_int otype_max = wi::mask (otype_prec - 1, false, otype_prec);
 wide_int itype_max = wi::mask (otype_prec - 1, false, itype_prec);
 wide_int int_cst_1 = wi::to_wide (captures[2]);
 wide_int int_cst_2 = wi::to_wide (captures[3]);
 wide_int int_cst_3 = wi::to_wide (captures[4]);
												if (((wi::eq_p (int_cst_1, offset) && wi::eq_p (int_cst_2, limit_0))
 || (wi::eq_p (int_cst_1, itype_max) && wi::eq_p (int_cst_2, limit_2))
 || (wi::eq_p (int_cst_1, offset) && wi::eq_p (int_cst_2, limit_2))
 || (wi::eq_p (int_cst_1, itype_max) && wi::eq_p (int_cst_2, limit_1)))
 && wi::eq_p (int_cst_3, otype_max)
)
												  {
												    {
												      res_ops[0] = captures[0];
												      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 78, __FILE__, __LINE__, false);
												      return true;
												    }
												  }
											    }
											  }
										      }
										    }
									          break;
									        }
									      default:;
									      }
									  }
								      }
								    break;
								  }
							        default:;
							        }
							      break;
							    }
						          default:;
						          }
						        break;
						      }
						    default:;
						    }
					          break;
					        }
					      default:;
					      }
					    break;
					  }
				        default:;
				        }
				      break;
				    }
			          default:;
			          }
			        break;
			      }
			    default:;
			    }
		          break;
		        }
		      case INTEGER_CST:
		        {
			  switch (TREE_CODE (_q31))
			    {
			    CASE_CONVERT:
			      {
				tree _q50 = TREE_OPERAND (_q31, 0);
				switch (TREE_CODE (_q21))
				  {
				  case INTEGER_CST:
				    {
				      switch (TREE_CODE (_p1))
				        {
					case BIT_XOR_EXPR:
					  {
					    tree _q80 = TREE_OPERAND (_p1, 0);
					    tree _q81 = TREE_OPERAND (_p1, 1);
					    switch (TREE_CODE (_q80))
					      {
					      case NEGATE_EXPR:
					        {
						  tree _q90 = TREE_OPERAND (_q80, 0);
						  switch (TREE_CODE (_q90))
						    {
						    CASE_CONVERT:
						      {
							tree _q100 = TREE_OPERAND (_q90, 0);
							switch (TREE_CODE (_q100))
							  {
							  case LT_EXPR:
							    {
							      tree _q110 = TREE_OPERAND (_q100, 0);
							      tree _q111 = TREE_OPERAND (_q100, 1);
							      if ((_q110 == _q50 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q50, 0) && types_match (_q110, _q50)))
								{
								  if (integer_zerop (_q111))
								    {
								      switch (TREE_CODE (_q81))
								        {
									case INTEGER_CST:
									  {
									    switch (TREE_CODE (_p2))
									      {
									      CASE_CONVERT:
									        {
										  tree _q150 = TREE_OPERAND (_p2, 0);
										  if ((_q150 == _q50 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q50, 0) && types_match (_q150, _q50)))
										    {
										      {
											tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q31, _q30, _q21, _q81 };
											if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
 && !TYPE_UNSIGNED (TREE_TYPE (captures[0])) && TYPE_UNSIGNED (TREE_TYPE (captures[1]))
)
											  {
											    {
 unsigned itype_prec = TYPE_PRECISION (TREE_TYPE (captures[0]));
 unsigned otype_prec = TYPE_PRECISION (type);
 wide_int offset = wi::uhwi (HOST_WIDE_INT_1U << (otype_prec - 1),
 itype_prec);
 wide_int limit_0 = wi::mask (otype_prec, false, itype_prec);
 wide_int limit_1 = wi::uhwi ((HOST_WIDE_INT_1U << otype_prec) - 3,
 itype_prec);
 wide_int limit_2 = wi::uhwi ((HOST_WIDE_INT_1U << otype_prec) - 2,
 itype_prec);
 wide_int otype_max = wi::mask (otype_prec - 1, false, otype_prec);
 wide_int itype_max = wi::mask (otype_prec - 1, false, itype_prec);
 wide_int int_cst_1 = wi::to_wide (captures[2]);
 wide_int int_cst_2 = wi::to_wide (captures[3]);
 wide_int int_cst_3 = wi::to_wide (captures[4]);
												if (((wi::eq_p (int_cst_1, offset) && wi::eq_p (int_cst_2, limit_0))
 || (wi::eq_p (int_cst_1, itype_max) && wi::eq_p (int_cst_2, limit_2))
 || (wi::eq_p (int_cst_1, offset) && wi::eq_p (int_cst_2, limit_2))
 || (wi::eq_p (int_cst_1, itype_max) && wi::eq_p (int_cst_2, limit_1)))
 && wi::eq_p (int_cst_3, otype_max)
)
												  {
												    {
												      res_ops[0] = captures[0];
												      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 78, __FILE__, __LINE__, false);
												      return true;
												    }
												  }
											    }
											  }
										      }
										    }
									          break;
									        }
									      default:;
									      }
									    break;
									  }
								        default:;
								        }
								    }
							        }
							      break;
							    }
						          default:;
						          }
						        break;
						      }
						    default:;
						    }
					          break;
					        }
					      case INTEGER_CST:
					        {
						  switch (TREE_CODE (_q81))
						    {
						    case NEGATE_EXPR:
						      {
							tree _q100 = TREE_OPERAND (_q81, 0);
							switch (TREE_CODE (_q100))
							  {
							  CASE_CONVERT:
							    {
							      tree _q110 = TREE_OPERAND (_q100, 0);
							      switch (TREE_CODE (_q110))
							        {
								case LT_EXPR:
								  {
								    tree _q120 = TREE_OPERAND (_q110, 0);
								    tree _q121 = TREE_OPERAND (_q110, 1);
								    if ((_q120 == _q50 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q50, 0) && types_match (_q120, _q50)))
								      {
									if (integer_zerop (_q121))
									  {
									    switch (TREE_CODE (_p2))
									      {
									      CASE_CONVERT:
									        {
										  tree _q150 = TREE_OPERAND (_p2, 0);
										  if ((_q150 == _q50 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q50, 0) && types_match (_q150, _q50)))
										    {
										      {
											tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q31, _q30, _q21, _q80 };
											if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
 && !TYPE_UNSIGNED (TREE_TYPE (captures[0])) && TYPE_UNSIGNED (TREE_TYPE (captures[1]))
)
											  {
											    {
 unsigned itype_prec = TYPE_PRECISION (TREE_TYPE (captures[0]));
 unsigned otype_prec = TYPE_PRECISION (type);
 wide_int offset = wi::uhwi (HOST_WIDE_INT_1U << (otype_prec - 1),
 itype_prec);
 wide_int limit_0 = wi::mask (otype_prec, false, itype_prec);
 wide_int limit_1 = wi::uhwi ((HOST_WIDE_INT_1U << otype_prec) - 3,
 itype_prec);
 wide_int limit_2 = wi::uhwi ((HOST_WIDE_INT_1U << otype_prec) - 2,
 itype_prec);
 wide_int otype_max = wi::mask (otype_prec - 1, false, otype_prec);
 wide_int itype_max = wi::mask (otype_prec - 1, false, itype_prec);
 wide_int int_cst_1 = wi::to_wide (captures[2]);
 wide_int int_cst_2 = wi::to_wide (captures[3]);
 wide_int int_cst_3 = wi::to_wide (captures[4]);
												if (((wi::eq_p (int_cst_1, offset) && wi::eq_p (int_cst_2, limit_0))
 || (wi::eq_p (int_cst_1, itype_max) && wi::eq_p (int_cst_2, limit_2))
 || (wi::eq_p (int_cst_1, offset) && wi::eq_p (int_cst_2, limit_2))
 || (wi::eq_p (int_cst_1, itype_max) && wi::eq_p (int_cst_2, limit_1)))
 && wi::eq_p (int_cst_3, otype_max)
)
												  {
												    {
												      res_ops[0] = captures[0];
												      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 78, __FILE__, __LINE__, false);
												      return true;
												    }
												  }
											    }
											  }
										      }
										    }
									          break;
									        }
									      default:;
									      }
									  }
								      }
								    break;
								  }
							        default:;
							        }
							      break;
							    }
						          default:;
						          }
						        break;
						      }
						    default:;
						    }
					          break;
					        }
					      default:;
					      }
					    break;
					  }
				        default:;
				        }
				      break;
				    }
			          default:;
			          }
			        break;
			      }
			    default:;
			    }
		          break;
		        }
		      default:;
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    default:;
    }
  return false;
}

tree
generic_simplify_28 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && (
0
 || !TREE_SIDE_EFFECTS (captures[1]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail256;
      {
	tree res_op0;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[2];
	  if (TREE_TYPE (_o1[0]) != boolean_type_node)
	    {
	      _r1 = fold_build1_loc (loc, NOP_EXPR, boolean_type_node, _o1[0]);
	    }
	  else
	    _r1 = _o1[0];
	  res_op0 = _r1;
	}
	tree res_op1;
	res_op1 = captures[1];
	tree res_op2;
	res_op2 = captures[0];
	tree _r;
	_r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 126, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail256:;
    }
  return NULL_TREE;
}

tree
generic_simplify_30 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (VECTOR_TYPE_P (type)
 && known_eq (TYPE_VECTOR_SUBPARTS (type),
 TYPE_VECTOR_SUBPARTS (TREE_TYPE (captures[3])))
 && (TYPE_MODE (TREE_TYPE (type))
 == TYPE_MODE (TREE_TYPE (TREE_TYPE (captures[3]))))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail258;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	{
	  tree _o1[1], _r1;
	  {
	    tree _o2[3], _r2;
	    _o2[0] = captures[2];
	    {
	      tree _o3[1], _r3;
	      _o3[0] = captures[3];
	      _r3 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o3[0]), _o3[0]);
	      _o2[1] = _r3;
	    }
	    _o2[2] = captures[4];
	    _r2 = fold_build3_loc (loc, VEC_COND_EXPR, TREE_TYPE (_o2[1]), _o2[0], _o2[1], _o2[2]);
	    _o1[0] = _r2;
	  }
	  _r1 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, _o1[0]);
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 128, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail258:;
    }
  return NULL_TREE;
}

tree
generic_simplify_37 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (VECTOR_FLOAT_TYPE_P (type)
 && TYPE_VECTOR_SUBPARTS (type).is_constant ()
)
    {
      {
 tree perm_cst = captures[1];
 vec_perm_builder builder;
 bool full_perm_p = false;
 if (tree_to_vec_perm_builder (&builder, perm_cst))
 {
 unsigned HOST_WIDE_INT nelts;
 nelts = TYPE_VECTOR_SUBPARTS (type).to_constant ();
 vec_perm_indices sel (builder, 1, nelts);
 if (sel.encoding ().encoded_full_vector_p ())
 {
 auto_sbitmap seen (nelts);
 bitmap_clear (seen);
 unsigned HOST_WIDE_INT count = 0, i;
 for (i = 0; i < nelts; i++)
 {
 if (!bitmap_set_bit (seen, sel[i].to_constant ()))
 break;
 count++;
 }
 full_perm_p = count == nelts;
 }
 }
	  if (full_perm_p
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail265;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail265;
	      {
		tree res_op0;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[2];
		  _r1 = fold_build2_loc (loc, op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  captures[3] = _r1;
		}
		res_op0 = unshare_expr (captures[3]);
		tree res_op1;
		res_op1 = captures[3];
		tree res_op2;
		res_op2 = captures[1];
		tree _r;
		_r = fold_build3_loc (loc, VEC_PERM_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 135, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail265:;
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_48 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_SATURATING (type)
)
    {
      if ((!FLOAT_TYPE_P (type) || flag_associative_math)
 && !FIXED_POINT_TYPE_P (type)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail276;
	  {
	    tree res_op0;
	    res_op0 = captures[1];
	    tree _r;
	    _r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 145, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail276:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_54 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_SATURATING (type)
)
    {
      if ((!FLOAT_TYPE_P (type) || flag_associative_math)
 && !FIXED_POINT_TYPE_P (type)
)
	{
	  if (!TYPE_OVERFLOW_TRAPS (type)
 && TREE_CODE (type) != COMPLEX_TYPE
 && tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
	    {
	      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail286;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail286;
	      {
		tree res_op0;
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[0];
		  if (TREE_TYPE (_o1[0]) != type)
		    {
		      _r1 = fold_build1_loc (loc, NOP_EXPR, type, _o1[0]);
		    }
		  else
		    _r1 = _o1[0];
		  res_op0 = _r1;
		}
		tree _r;
		_r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 155, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail286:;
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_58 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && (
0
 || !TREE_SIDE_EFFECTS (captures[1]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail292;
      {
	tree res_op0;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[2];
	  if (TREE_TYPE (_o1[0]) != boolean_type_node)
	    {
	      _r1 = fold_build1_loc (loc, NOP_EXPR, boolean_type_node, _o1[0]);
	    }
	  else
	    _r1 = _o1[0];
	  res_op0 = _r1;
	}
	tree res_op1;
	res_op1 = captures[1];
	tree res_op2;
	res_op2 = captures[0];
	tree _r;
	_r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 161, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail292:;
    }
  return NULL_TREE;
}

tree
generic_simplify_62 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_CODE (captures[1]) == SSA_NAME && num_imm_uses (captures[1]) == 2
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail296;
      {
	if (! tree_invariant_p (captures[2])) goto next_after_fail296;
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  {
	    tree _o2[3], _r2;
	    _o2[0] = unshare_expr (captures[2]);
	    _o2[1] = captures[2];
	    _o2[2] = captures[4];
	    _r2 = fold_build3_loc (loc, VEC_PERM_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1], _o2[2]);
	    _o1[0] = _r2;
	  }
	  _o1[1] = captures[3];
	  _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree res_op1;
	res_op1 = captures[5];
	tree _r;
	_r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 165, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail296:;
    }
  return NULL_TREE;
}

tree
generic_simplify_68 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail302;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail302;
  {
    tree _r;
    _r = captures[2];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 171, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail302:;
  return NULL_TREE;
}

tree
generic_simplify_72 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_PRECISION (TREE_TYPE (captures[0])) == 1
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail306;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  _o1[1] =  build_one_cst (type);
	  _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, BIT_IOR_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 174, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail306:;
    }
  return NULL_TREE;
}

tree
generic_simplify_84 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail318;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    {
      tree _o1[1], _r1;
      _o1[0] = captures[2];
      _r1 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
      res_op1 = _r1;
    }
    tree _r;
    _r = fold_build2_loc (loc, bitop, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 182, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail318:;
  return NULL_TREE;
}

tree
generic_simplify_92 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 bool wascmp;
      if ((bitwise_inverted_equal_p (captures[0], captures[2], wascmp)
 || bitwise_inverted_equal_p (captures[1], captures[2], wascmp))
 && (!wascmp || element_precision (type) == 1)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail326;
	  {
	    tree _r;
	    _r =  build_all_ones_cst (TREE_TYPE (captures[0]));
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (TREE_SIDE_EFFECTS (captures[2]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 188, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail326:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_99 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop),
 const enum tree_code ARG_UNUSED (rbitop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail337;
  {
    tree _r;
    _r = captures[0];
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 192, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail337:;
  return NULL_TREE;
}

tree
generic_simplify_101 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop),
 const enum tree_code ARG_UNUSED (rbitop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail339;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail339;
  {
    tree _r;
    _r = captures[2];
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 194, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail339:;
  return NULL_TREE;
}

tree
generic_simplify_108 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (bitwise_equal_p (captures[4], captures[7])
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail347;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  {
	    tree _o2[2], _r2;
	    _o2[0] = captures[5];
	    {
	      tree _o3[1], _r3;
	      _o3[0] = captures[6];
	      if (TREE_TYPE (_o3[0]) != TREE_TYPE (_o2[0]))
		{
		  _r3 = fold_build1_loc (loc, NOP_EXPR, TREE_TYPE (_o2[0]), _o3[0]);
		}
	      else
	        _r3 = _o3[0];
	      _o2[1] = _r3;
	    }
	    _r2 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
	    _o1[0] = _r2;
	  }
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[4];
	    if (TREE_TYPE (_o2[0]) != TREE_TYPE (_o1[0]))
	      {
		_r2 = fold_build1_loc (loc, NOP_EXPR, TREE_TYPE (_o1[0]), _o2[0]);
	      }
	    else
	      _r2 = _o2[0];
	    _o1[1] = _r2;
	  }
	  _r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree _r;
	_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	if (TREE_SIDE_EFFECTS (captures[7]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[7]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 196, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail347:;
    }
  return NULL_TREE;
}

tree
generic_simplify_116 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (captures[5])
 && invert_tree_comparison (cmp, HONOR_NANS (captures[2])) == icmp
)
    {
      if (integer_onep (captures[4])
)
	{
	  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail357;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail357;
	  {
	    tree res_op0;
	    {
	      tree _o1[3], _r1;
	      _o1[0] = captures[1];
	      _o1[1] = captures[6];
	      _o1[2] = captures[9];
	      _r1 = fold_build3_loc (loc, VEC_COND_EXPR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2]);
	      res_op0 = _r1;
	    }
	    tree res_op1;
	    res_op1 = captures[4];
	    tree _r;
	    _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 207, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail357:;
	}
      else
	{
	  if (integer_minus_onep (captures[4])
)
	    {
	      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail358;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail358;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		res_op1 = captures[6];
		tree res_op2;
		res_op2 = captures[9];
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 208, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail358:;
	    }
	}
    }
  else
    {
      if (integer_zerop (captures[4])
 && invert_tree_comparison (cmp, HONOR_NANS (captures[2])) == icmp
)
	{
	  if (integer_onep (captures[5])
)
	    {
	      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail359;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail359;
	      {
		tree res_op0;
		{
		  tree _o1[3], _r1;
		  _o1[0] = captures[1];
		  _o1[1] = captures[9];
		  _o1[2] = captures[6];
		  _r1 = fold_build3_loc (loc, VEC_COND_EXPR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2]);
		  res_op0 = _r1;
		}
		tree res_op1;
		res_op1 = captures[5];
		tree _r;
		_r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 209, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail359:;
	    }
	  else
	    {
	      if (integer_minus_onep (captures[5])
)
		{
		  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail360;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail360;
		  {
		    tree res_op0;
		    res_op0 = captures[1];
		    tree res_op1;
		    res_op1 = captures[9];
		    tree res_op2;
		    res_op2 = captures[6];
		    tree _r;
		    _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 210, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail360:;
		}
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_134 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (eqne))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (eqne == EQ_EXPR
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail410;
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail410;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail410;
      {
	tree _r;
	_r = captures[0];
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 234, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail410:;
    }
  else
    {
      if (eqne == NE_EXPR
)
	{
	  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail411;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail411;
	  {
	    tree _r;
	    _r =  constant_boolean_node (true, type);
	    if (TREE_SIDE_EFFECTS (captures[2]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 235, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail411:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_138 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && tree_int_cst_sgn (captures[5]) > 0
 && (tree_nonzero_bits (captures[0]) & tree_nonzero_bits (captures[4])) == 0
)
    {
      {
 tree t = type;
 if (!TYPE_OVERFLOW_WRAPS (t))
 t = unsigned_type_for (t);
 wide_int wone = wi::one (TYPE_PRECISION (type));
 wide_int c = wi::add (wi::to_wide (captures[3]),
 wi::lshift (wone, wi::to_wide (captures[5])));
	  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail415;
	  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail415;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail415;
	  {
	    tree res_op0;
	    {
	      tree _o1[2], _r1;
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[2];
		if (TREE_TYPE (_o2[0]) != t)
		  {
		    _r2 = fold_build1_loc (loc, NOP_EXPR, t, _o2[0]);
		  }
		else
		  _r2 = _o2[0];
		_o1[0] = _r2;
	      }
	      _o1[1] =  wide_int_to_tree (t, c);
	      _r1 = fold_build2_loc (loc, MULT_EXPR, t, _o1[0], _o1[1]);
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 200, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail415:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_143 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail420;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail420;
  {
    tree _r;
    _r = captures[1];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 239, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail420:;
  return NULL_TREE;
}

tree
generic_simplify_145 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail422;
  {
    tree res_op0;
    {
      tree _o1[2], _r1;
      _o1[0] = captures[1];
      _o1[1] = captures[2];
      _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      res_op0 = _r1;
    }
    tree _r;
    _r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 241, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail422:;
  return NULL_TREE;
}

tree
generic_simplify_149 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail426;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree _r;
    _r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 245, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail426:;
  return NULL_TREE;
}

tree
generic_simplify_154 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail431;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 = captures[1];
    tree _r;
    _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 250, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail431:;
  return NULL_TREE;
}

tree
generic_simplify_160 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (opo),
 const enum tree_code ARG_UNUSED (opi))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail437;
  {
    tree res_op0;
    {
      tree _o1[1], _r1;
      _o1[0] = captures[1];
      _r1 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
      res_op0 = _r1;
    }
    tree res_op1;
    res_op1 = captures[2];
    tree _r;
    _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 256, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail437:;
  return NULL_TREE;
}

tree
generic_simplify_165 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && !TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 && types_match (TREE_TYPE (captures[1]), TREE_TYPE (captures[3]))
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail443;
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail443;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail443;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  _o1[1] = captures[3];
	  _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree res_op1;
	res_op1 =  build_zero_cst (TREE_TYPE (captures[1]));
	tree _r;
	_r = fold_build2_loc (loc, LT_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 261, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail443:;
    }
  return NULL_TREE;
}

tree
generic_simplify_175 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail454;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail454;
  {
    tree _r;
    _r =  build_zero_cst (type);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 270, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail454:;
  return NULL_TREE;
}

tree
generic_simplify_180 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!canonicalize_math_p ()
)
    {
      if (INTEGRAL_TYPE_P (type)
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
)
	{
	  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail461;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail461;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree res_op1;
	    res_op1 = captures[3];
	    tree res_op2;
	    res_op2 =  build_zero_cst (type);
	    tree _r;
	    _r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 274, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail461:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_186 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (POW))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
    {
      if (!TREE_OVERFLOW (captures[2])
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail468;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree res_op1;
	    {
	      tree _o1[2], _r1;
	      _o1[0] = captures[2];
	      _o1[1] =  build_one_cst (type);
	      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      res_op1 = _r1;
	    }
	    tree _r;
	    _r = maybe_build_call_expr_loc (loc, POW, type, 2, res_op0, res_op1);
	    if (!_r)
	      goto next_after_fail468;
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 280, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail468:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_196 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (element_precision (captures[0]) >= element_precision (captures[1])
 && types_match (captures[1], captures[3])
)
    {
      if (wi::lt_p (wi::to_wide (captures[2]), 0, TYPE_SIGN (TREE_TYPE (captures[2])))
)
	{
	  if (!TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail478;
	      {
		tree res_op0;
		res_op0 = captures[3];
		tree res_op1;
		res_op1 = captures[1];
		tree _r;
		_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		if (TREE_SIDE_EFFECTS (captures[2]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 290, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail478:;
	    }
	  else
	    {
	      if (tree_expr_nonzero_p (captures[1]) && tree_expr_nonzero_p (captures[3])
)
		{
		  {
 tree utype = unsigned_type_for (TREE_TYPE (captures[1]));
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail479;
		      {
			tree res_op0;
			{
			  tree _o1[1], _r1;
			  _o1[0] = captures[3];
			  if (TREE_TYPE (_o1[0]) != utype)
			    {
			      _r1 = fold_build1_loc (loc, NOP_EXPR, utype, _o1[0]);
			    }
			  else
			    _r1 = _o1[0];
			  res_op0 = _r1;
			}
			tree res_op1;
			{
			  tree _o1[1], _r1;
			  _o1[0] = captures[1];
			  if (TREE_TYPE (_o1[0]) != utype)
			    {
			      _r1 = fold_build1_loc (loc, NOP_EXPR, utype, _o1[0]);
			    }
			  else
			    _r1 = _o1[0];
			  res_op1 = _r1;
			}
			tree _r;
			_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
			if (TREE_SIDE_EFFECTS (captures[2]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 291, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail479:;
		  }
		}
	    }
	}
      else
	{
	  if (wi::gt_p (wi::to_wide (captures[2]), 1, TYPE_SIGN (TREE_TYPE (captures[2])))
)
	    {
	      if (TYPE_UNSIGNED (TREE_TYPE (captures[1])) || !TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail480;
		  {
		    tree res_op0;
		    res_op0 = captures[1];
		    tree res_op1;
		    res_op1 = captures[3];
		    tree _r;
		    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		    if (TREE_SIDE_EFFECTS (captures[2]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 292, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail480:;
		}
	      else
		{
		  {
 tree utype = unsigned_type_for (TREE_TYPE (captures[1]));
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail481;
		      {
			tree res_op0;
			{
			  tree _o1[1], _r1;
			  _o1[0] = captures[1];
			  if (TREE_TYPE (_o1[0]) != utype)
			    {
			      _r1 = fold_build1_loc (loc, NOP_EXPR, utype, _o1[0]);
			    }
			  else
			    _r1 = _o1[0];
			  res_op0 = _r1;
			}
			tree res_op1;
			{
			  tree _o1[1], _r1;
			  _o1[0] = captures[3];
			  if (TREE_TYPE (_o1[0]) != utype)
			    {
			      _r1 = fold_build1_loc (loc, NOP_EXPR, utype, _o1[0]);
			    }
			  else
			    _r1 = _o1[0];
			  res_op1 = _r1;
			}
			tree _r;
			_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
			if (TREE_SIDE_EFFECTS (captures[2]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 293, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail481:;
		  }
		}
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_219 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_fits_shwi_p (captures[3])
 && tree_to_shwi (captures[3]) > 0
 && tree_to_shwi (captures[3]) < TYPE_PRECISION (TREE_TYPE (captures[2]))
)
    {
      {
 tree t0 = TREE_TYPE (captures[2]);
 unsigned int prec = TYPE_PRECISION (t0);
 wide_int c1 = wi::to_wide (captures[3]);
 wide_int c2 = wi::to_wide (captures[4]);
 wide_int c3 = wi::to_wide (captures[5]);
 wide_int sb = wi::set_bit_in_zero (prec - 1, prec);
	  if ((c2 & c3) != c3
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail522;
	      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail522;
	      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail522;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail522;
	      {
		tree _r;
		_r =  constant_boolean_node (cmp == NE_EXPR, type);
		if (TREE_SIDE_EFFECTS (captures[2]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 332, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail522:;
	    }
	  else
	    {
	      if (TYPE_UNSIGNED (t0)
)
		{
		  if ((c3 & wi::arshift (sb, c1 - 1)) != 0
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail523;
		      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail523;
		      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail523;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail523;
		      {
			tree _r;
			_r =  constant_boolean_node (cmp == NE_EXPR, type);
			if (TREE_SIDE_EFFECTS (captures[2]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 333, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail523:;
		    }
		  else
		    {
		      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail524;
		      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail524;
		      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail524;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail524;
		      {
			tree res_op0;
			{
			  tree _o1[2], _r1;
			  _o1[0] = captures[2];
			  _o1[1] =  wide_int_to_tree (t0, c2 << c1);
			  _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			  res_op0 = _r1;
			}
			tree res_op1;
			res_op1 =  wide_int_to_tree (t0, c3 << c1);
			tree _r;
			_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 334, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail524:;
		    }
		}
	      else
		{
		  {
 wide_int smask = wi::arshift (sb, c1);
		      if ((c2 & smask) == 0
)
			{
			  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail525;
			  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail525;
			  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail525;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail525;
			  {
			    tree res_op0;
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[2];
			      _o1[1] =  wide_int_to_tree (t0, c2 << c1);
			      _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      res_op0 = _r1;
			    }
			    tree res_op1;
			    res_op1 =  wide_int_to_tree (t0, c3 << c1);
			    tree _r;
			    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 335, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail525:;
			}
		      else
			{
			  if ((c3 & smask) == 0
)
			    {
			      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail526;
			      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail526;
			      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail526;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail526;
			      {
				tree res_op0;
				{
				  tree _o1[2], _r1;
				  _o1[0] = captures[2];
				  _o1[1] =  wide_int_to_tree (t0, (c2 << c1) | sb);
				  _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				  res_op0 = _r1;
				}
				tree res_op1;
				res_op1 =  wide_int_to_tree (t0, c3 << c1);
				tree _r;
				_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 336, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail526:;
			    }
			  else
			    {
			      if ((c2 & smask) != (c3 & smask)
)
				{
				  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail527;
				  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail527;
				  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail527;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail527;
				  {
				    tree _r;
				    _r =  constant_boolean_node (cmp == NE_EXPR, type);
				    if (TREE_SIDE_EFFECTS (captures[2]))
				      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 337, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail527:;
				}
			      else
				{
				  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail528;
				  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail528;
				  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail528;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail528;
				  {
				    tree res_op0;
				    {
				      tree _o1[2], _r1;
				      _o1[0] = captures[2];
				      _o1[1] =  wide_int_to_tree (t0, (c2 << c1) | sb);
				      _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				      res_op0 = _r1;
				    }
				    tree res_op1;
				    res_op1 =  wide_int_to_tree (t0, (c3 << c1) | sb);
				    tree _r;
				    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 338, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail528:;
				}
			    }
			}
		  }
		}
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_248 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (scmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (FLOAT_TYPE_P (TREE_TYPE (captures[0]))
 || (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && (cmp == EQ_EXPR
 || cmp == NE_EXPR
 || TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))))
)
    {
      {
 tree tem = const_unop (NEGATE_EXPR, TREE_TYPE (captures[0]), captures[1]);
	  if (tem && !TREE_OVERFLOW (tem)
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail605;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail605;
	      {
		tree res_op0;
		res_op0 = captures[0];
		tree res_op1;
		res_op1 =  tem;
		tree _r;
		_r = fold_build2_loc (loc, scmp, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 413, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail605:;
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_251 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (TREE_TYPE (captures[0]), TREE_TYPE (captures[1]))
 && wi::bit_and_not (wi::to_wide (captures[2]), wi::to_wide (captures[3])) != 0
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail620;
      {
	tree _r;
	_r =  constant_boolean_node (cmp == NE_EXPR, type);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (TREE_SIDE_EFFECTS (captures[2]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	if (TREE_SIDE_EFFECTS (captures[3]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 421, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail620:;
    }
  return NULL_TREE;
}

tree
generic_simplify_253 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail625;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail625;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 = captures[1];
    tree _r;
    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 426, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail625:;
  return NULL_TREE;
}

tree
generic_simplify_255 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail627;
  {
    tree res_op0;
    {
      tree _o1[2], _r1;
      _o1[0] = captures[0];
      {
	tree _o2[1], _r2;
	_o2[0] = captures[1];
	_r2 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
	if (EXPR_P (_r2))
	  goto next_after_fail627;
	_o1[1] = _r2;
      }
      _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      res_op0 = _r1;
    }
    tree res_op1;
    res_op1 =  build_zero_cst (TREE_TYPE (captures[0]));
    tree _r;
    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 428, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail627:;
  return NULL_TREE;
}

tree
generic_simplify_258 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (TREE_TYPE (captures[0]), TREE_TYPE (captures[1]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail630;
      {
	tree res_op0;
	res_op0 = captures[1];
	tree res_op1;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[2];
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[3];
	    if (TREE_TYPE (_o2[0]) != TREE_TYPE (res_op0))
	      {
		_r2 = fold_build1_loc (loc, NOP_EXPR, TREE_TYPE (res_op0), _o2[0]);
	      }
	    else
	      _r2 = _o2[0];
	    _o1[1] = _r2;
	  }
	  _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 431, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail630:;
    }
  return NULL_TREE;
}

tree
generic_simplify_262 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (ncmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && type_has_mode_precision_p (TREE_TYPE (captures[1]))
 && element_precision (captures[0]) >= element_precision (captures[1])
 && wi::only_sign_bit_p (wi::to_wide (captures[2]), element_precision (captures[1]))
)
    {
      {
 tree stype = signed_type_for (TREE_TYPE (captures[1]));
	  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail634;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail634;
	  {
	    tree res_op0;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[1];
	      if (TREE_TYPE (_o1[0]) != stype)
		{
		  _r1 = fold_build1_loc (loc, NOP_EXPR, stype, _o1[0]);
		}
	      else
	        _r1 = _o1[0];
	      res_op0 = _r1;
	    }
	    tree res_op1;
	    res_op1 =  build_zero_cst (stype);
	    tree _r;
	    _r = fold_build2_loc (loc, ncmp, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[2]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 435, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail634:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_266 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (ncmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && !TYPE_UNSIGNED (TREE_TYPE (captures[0]))
 && types_match (captures[0], captures[2])
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail638;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail638;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[0];
	  _o1[1] = captures[2];
	  _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree res_op1;
	res_op1 = captures[1];
	tree _r;
	_r = fold_build2_loc (loc, ncmp, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 437, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail638:;
    }
  return NULL_TREE;
}

tree
generic_simplify_270 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (((POINTER_TYPE_P (TREE_TYPE (captures[0]))
 && !FUNC_OR_METHOD_TYPE_P (TREE_TYPE (TREE_TYPE (captures[0])))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && !(
1
 && TREE_CODE (TREE_TYPE (captures[0])) == REFERENCE_TYPE
 && (flag_sanitize & (SANITIZE_NULL | SANITIZE_ALIGNMENT))))
 || (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && POINTER_TYPE_P (TREE_TYPE (captures[1]))
 && !FUNC_OR_METHOD_TYPE_P (TREE_TYPE (TREE_TYPE (captures[1])))))
 && TYPE_PRECISION (TREE_TYPE (captures[0])) == TYPE_PRECISION (TREE_TYPE (captures[1]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail656;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[1];
	  if (TREE_TYPE (_o1[0]) != TREE_TYPE (res_op0))
	    {
	      _r1 = fold_build1_loc (loc, NOP_EXPR, TREE_TYPE (res_op0), _o1[0]);
	    }
	  else
	    _r1 = _o1[0];
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 447, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail656:;
    }
  return NULL_TREE;
}

tree
generic_simplify_274 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (rcmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TREE_OVERFLOW (captures[0]) && !TREE_OVERFLOW (captures[2])
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[1]))
)
    {
      {
 tree res = int_const_binop (MINUS_EXPR, captures[0], captures[2]);
	  if (TREE_OVERFLOW (res)
)
	    {
	      if (cmp == NE_EXPR
)
		{
		  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail665;
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail665;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail665;
		  {
		    tree _r;
		    _r =  constant_boolean_node (true, type);
		    if (TREE_SIDE_EFFECTS (captures[1]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 456, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail665:;
		}
	      else
		{
		  if (cmp == EQ_EXPR
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail666;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail666;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail666;
		      {
			tree _r;
			_r =  constant_boolean_node (false, type);
			if (TREE_SIDE_EFFECTS (captures[1]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 457, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail666:;
		    }
		  else
		    {
		      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail667;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail667;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail667;
		      {
			tree _r;
 bool less = cmp == LE_EXPR || cmp == LT_EXPR;
 bool ovf_high = wi::lt_p (wi::to_wide (captures[0]), 0,
 TYPE_SIGN (TREE_TYPE (captures[0])));			_r = 
 constant_boolean_node (less == ovf_high, type);
			if (TREE_SIDE_EFFECTS (captures[1]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 458, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail667:;
		    }
		}
	    }
	  else
	    {
	      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail668;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail668;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail668;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		res_op1 =  res;
		tree _r;
		_r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 459, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail668:;
	    }
      }
    }
  else
    {
      if (TYPE_UNSIGNED (TREE_TYPE (captures[1]))
)
	{
	  if (cmp == EQ_EXPR || cmp == NE_EXPR
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail669;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[2];
		  _r1 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  res_op1 = _r1;
		}
		tree _r;
		_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 460, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail669:;
	    }
	  else
	    {
	      if (cmp == LE_EXPR || cmp == GT_EXPR
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail670;
		  {
		    if (! tree_invariant_p (captures[2])) goto next_after_fail670;
		    tree res_op0;
		    {
		      tree _o1[2], _r1;
		      _o1[0] = captures[1];
		      {
			tree _o2[2], _r2;
			_o2[0] = unshare_expr (captures[2]);
			_o2[1] = captures[0];
			_r2 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
			_o1[1] = _r2;
		      }
		      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      res_op0 = _r1;
		    }
		    tree res_op1;
		    res_op1 = captures[2];
		    tree _r;
		    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 461, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail670:;
		}
	      else
		{
		  if (cmp == LT_EXPR || cmp == GE_EXPR
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail671;
		      {
			if (! tree_invariant_p (captures[2])) goto next_after_fail671;
			tree res_op0;
			{
			  tree _o1[2], _r1;
			  _o1[0] = captures[1];
			  {
			    tree _o2[2], _r2;
			    _o2[0] = unshare_expr (captures[2]);
			    {
			      tree _o3[2], _r3;
			      _o3[0] = captures[0];
			      _o3[1] =  build_one_cst (TREE_TYPE (captures[1]));
			      _r3 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
			      _o2[1] = _r3;
			    }
			    _r2 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
			    _o1[1] = _r2;
			  }
			  _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			  res_op0 = _r1;
			}
			tree res_op1;
			res_op1 = captures[2];
			tree _r;
			_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 462, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail671:;
		    }
		}
	    }
	}
      else
	{
	  if (!TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[1]))
)
	    {
	      if (cmp == EQ_EXPR || cmp == NE_EXPR
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail672;
		  {
		    tree res_op0;
		    res_op0 = captures[1];
		    tree res_op1;
		    {
		      tree _o1[2], _r1;
		      _o1[0] = captures[0];
		      _o1[1] = captures[2];
		      _r1 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      res_op1 = _r1;
		    }
		    tree _r;
		    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 463, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail672:;
		}
	      else
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail673;
		  {
		    tree res_op0;
		    {
		      tree _o1[2], _r1;
		      _o1[0] = captures[1];
		      {
			tree _o2[2], _r2;
			_o2[0] = captures[0];
			_o2[1] =  build_one_cst (TREE_TYPE (captures[1]));
			_r2 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
			_o1[1] = _r2;
		      }
		      _r1 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      res_op0 = _r1;
		    }
		    tree res_op1;
		    {
		      tree _o1[1], _r1;
		      _o1[0] = captures[2];
		      _r1 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
		      res_op1 = _r1;
		    }
		    tree _r;
		    _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 464, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail673:;
		}
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_306 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (mod))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_UNSIGNED (type)
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail715;
      {
	tree _r;
	_r =  build_zero_cst (type);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 504, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail715:;
    }
  return NULL_TREE;
}

tree
generic_simplify_316 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (SINH),
 const combined_fn ARG_UNUSED (COSH),
 const combined_fn ARG_UNUSED (TANH))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail727;
      {
	tree res_op0;
	res_op0 = captures[1];
	tree _r;
	_r = maybe_build_call_expr_loc (loc, TANH, type, 1, res_op0);
	if (!_r)
	  goto next_after_fail727;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 516, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail727:;
    }
  return NULL_TREE;
}

tree
generic_simplify_321 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (POW))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail732;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[2];
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[3];
	    _r2 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
	    _o1[1] = _r2;
	  }
	  _r1 = maybe_build_call_expr_loc (loc, POW, TREE_TYPE (_o1[0]), 2, _o1[0], _o1[1]);
	  if (!_r1)
	    goto next_after_fail732;
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 521, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail732:;
    }
  return NULL_TREE;
}

tree
generic_simplify_329 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[1]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail742;
      {
	tree res_op0;
	res_op0 = captures[2];
	tree res_op1;
	res_op1 = captures[1];
	tree _r;
	_r = fold_build2_loc (loc, op, type, res_op0, res_op1);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 531, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail742:;
    }
  return NULL_TREE;
}

tree
generic_simplify_336 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (cmp2))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (TREE_TYPE (captures[0]), TREE_TYPE (captures[2]))
 && TYPE_UNSIGNED (TREE_TYPE (captures[2])) && !TYPE_UNSIGNED (TREE_TYPE (captures[0]))
 && wi::gt_p (wi::to_wide (captures[1]), 0, TYPE_SIGN (TREE_TYPE (captures[1])))
)
    {
      {
 tree utype = TREE_TYPE (captures[2]);
 wide_int denom = wi::to_wide (captures[1]);
 wide_int right = wi::to_wide (captures[2]);
 wide_int smax = wi::sdiv_trunc (wi::max_value (TREE_TYPE (captures[0])), denom);
 wide_int smin = wi::sdiv_trunc (wi::min_value (TREE_TYPE (captures[0])), denom);
 bool small = wi::leu_p (right, smax);
 bool large = wi::geu_p (right, smin);
	  if (small || large
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail751;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail751;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail751;
	      {
		tree res_op0;
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[0];
		  if (TREE_TYPE (_o1[0]) != utype)
		    {
		      _r1 = fold_build1_loc (loc, NOP_EXPR, utype, _o1[0]);
		    }
		  else
		    _r1 = _o1[0];
		  res_op0 = _r1;
		}
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  {
		    tree _o2[1], _r2;
		    _o2[0] = captures[1];
		    if (TREE_TYPE (_o2[0]) != TREE_TYPE (res_op0))
		      {
			_r2 = fold_build1_loc (loc, NOP_EXPR, TREE_TYPE (res_op0), _o2[0]);
		      }
		    else
		      _r2 = _o2[0];
		    _o1[1] = _r2;
		  }
		  _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  res_op1 = _r1;
		}
		tree _r;
		_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 540, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail751:;
	    }
	  else
	    {
	      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail752;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail752;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail752;
	      {
		tree res_op0;
		res_op0 = captures[0];
		tree res_op1;
		res_op1 =  build_zero_cst (TREE_TYPE (captures[0]));
		tree _r;
		_r = fold_build2_loc (loc, cmp2, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 541, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail752:;
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_350 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (acmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree cst = uniform_integer_cst_p (captures[1]);
      if (tree_int_cst_sgn (cst) == -1
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail803;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree res_op1;
	    res_op1 =  build_uniform_cst (TREE_TYPE (captures[1]),
 wide_int_to_tree (TREE_TYPE (cst),
 wi::to_wide (cst)
 + 1));
	    tree _r;
	    _r = fold_build2_loc (loc, acmp, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 575, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail803:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_354 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (out))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 && types_match (TREE_TYPE (captures[1]), TREE_TYPE (captures[2]))
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail807;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail807;
      {
	tree res_op0;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  _r1 = fold_build1_loc (loc, IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (_o1[0])), _o1[0]);
	  res_op0 = _r1;
	}
	tree res_op1;
	res_op1 =  build_zero_cst (TREE_TYPE (captures[1]));
	tree _r;
	_r = fold_build2_loc (loc, out, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 561, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail807:;
    }
  return NULL_TREE;
}

tree
generic_simplify_359 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (shift))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((
0
 || !sanitize_flags_p (SANITIZE_SHIFT_EXPONENT))
 && (TYPE_UNSIGNED (type)
 || shift == LSHIFT_EXPR
 || tree_expr_nonnegative_p (captures[0]))
 && wi::ges_p (wi::to_wide (uniform_integer_cst_p (captures[1])),
 element_precision (type))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail816;
      {
	tree _r;
	_r =  build_zero_cst (type);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 584, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail816:;
    }
  return NULL_TREE;
}

tree
generic_simplify_364 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (shiftrotate))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree tem = uniform_vector_p (captures[1]);
      if (tem
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail821;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree res_op1;
	    res_op1 =  tem;
	    tree _r;
	    _r = fold_build2_loc (loc, shiftrotate, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 588, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail821:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_370 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (0
 && TREE_CODE (captures[3]) != INTEGER_CST
 && tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
 && types_match (type, captures[2])
 && !POINTER_TYPE_P (TREE_TYPE (captures[2]))
 && TREE_CODE (TREE_TYPE (captures[2])) != OFFSET_TYPE
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail834;
      {
	tree res_op0;
	res_op0 = captures[2];
	tree res_op1;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[3];
	  if (TREE_TYPE (_o1[0]) != type)
	    {
	      _r1 = fold_build1_loc (loc, NOP_EXPR, type, _o1[0]);
	    }
	  else
	    _r1 = _o1[0];
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, bitop, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 600, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail834:;
    }
  return NULL_TREE;
}

tree
generic_simplify_373 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && op != MULT_EXPR
 && op != RDIV_EXPR
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[2]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && type_has_mode_precision_p (TREE_TYPE (captures[2]))
 && type_has_mode_precision_p (TREE_TYPE (captures[4]))
 && type_has_mode_precision_p (type)
 && TYPE_PRECISION (TREE_TYPE (captures[1])) > TYPE_PRECISION (TREE_TYPE (captures[2]))
 && types_match (captures[2], type)
 && (types_match (captures[2], captures[4])
 || poly_int_tree_p (captures[3]))
)
    {
      if (TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[2]))
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail838;
	  {
	    tree res_op0;
	    res_op0 = captures[2];
	    tree res_op1;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[4];
	      if (TREE_TYPE (_o1[0]) != type)
		{
		  _r1 = fold_build1_loc (loc, NOP_EXPR, type, _o1[0]);
		}
	      else
	        _r1 = _o1[0];
	      res_op1 = _r1;
	    }
	    tree _r;
	    _r = fold_build2_loc (loc, op, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 604, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail838:;
	}
      else
	{
	  {
 tree utype = unsigned_type_for (TREE_TYPE (captures[2]));
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail839;
	      {
		tree res_op0;
		{
		  tree _o1[2], _r1;
		  {
		    tree _o2[1], _r2;
		    _o2[0] = captures[2];
		    if (TREE_TYPE (_o2[0]) != utype)
		      {
			_r2 = fold_build1_loc (loc, NOP_EXPR, utype, _o2[0]);
		      }
		    else
		      _r2 = _o2[0];
		    _o1[0] = _r2;
		  }
		  {
		    tree _o2[1], _r2;
		    _o2[0] = captures[4];
		    if (TREE_TYPE (_o2[0]) != utype)
		      {
			_r2 = fold_build1_loc (loc, NOP_EXPR, utype, _o2[0]);
		      }
		    else
		      _r2 = _o2[0];
		    _o1[1] = _r2;
		  }
		  _r1 = fold_build2_loc (loc, op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  res_op0 = _r1;
		}
		tree _r;
		_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 605, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail839:;
	  }
	}
    }
  else
    {
      if (FLOAT_TYPE_P (type)
 && DECIMAL_FLOAT_TYPE_P (TREE_TYPE (captures[0]))
 == DECIMAL_FLOAT_TYPE_P (type)
)
	{
	  {
 tree arg0 = strip_float_extensions (captures[2]);
 tree arg1 = strip_float_extensions (captures[4]);
 tree itype = TREE_TYPE (captures[0]);
 tree ty1 = TREE_TYPE (arg0);
 tree ty2 = TREE_TYPE (arg1);
 enum tree_code code = TREE_CODE (itype);
	      if (FLOAT_TYPE_P (ty1)
 && FLOAT_TYPE_P (ty2)
)
		{
		  {
 tree newtype = type;
 if (TYPE_MODE (ty1) == SDmode
 || TYPE_MODE (ty2) == SDmode
 || TYPE_MODE (type) == SDmode)
 newtype = dfloat32_type_node;
 if (TYPE_MODE (ty1) == DDmode
 || TYPE_MODE (ty2) == DDmode
 || TYPE_MODE (type) == DDmode)
 newtype = dfloat64_type_node;
 if (TYPE_MODE (ty1) == TDmode
 || TYPE_MODE (ty2) == TDmode
 || TYPE_MODE (type) == TDmode)
 newtype = dfloat128_type_node;
		      if ((newtype == dfloat32_type_node
 || newtype == dfloat64_type_node
 || newtype == dfloat128_type_node)
 && newtype == type
 && types_match (newtype, type)
)
			{
			  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail840;
			  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail840;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail840;
			  {
			    tree res_op0;
			    {
			      tree _o1[1], _r1;
			      _o1[0] = captures[2];
			      if (TREE_TYPE (_o1[0]) != newtype)
				{
				  _r1 = fold_build1_loc (loc, NOP_EXPR, newtype, _o1[0]);
				}
			      else
			        _r1 = _o1[0];
			      res_op0 = _r1;
			    }
			    tree res_op1;
			    {
			      tree _o1[1], _r1;
			      _o1[0] = captures[4];
			      if (TREE_TYPE (_o1[0]) != newtype)
				{
				  _r1 = fold_build1_loc (loc, NOP_EXPR, newtype, _o1[0]);
				}
			      else
			        _r1 = _o1[0];
			      res_op1 = _r1;
			    }
			    tree _r;
			    _r = fold_build2_loc (loc, op, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 606, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail840:;
			}
		      else
			{
			  {
 if (element_precision (ty1) > element_precision (newtype))
 newtype = ty1;
 if (element_precision (ty2) > element_precision (newtype))
 newtype = ty2;
			      if (element_precision (newtype) < element_precision (itype)
 && (!VECTOR_MODE_P (TYPE_MODE (newtype))
 || target_supports_op_p (newtype, op, optab_default))
 && (flag_unsafe_math_optimizations
 || (element_precision (newtype) == element_precision (type)
 && real_can_shorten_arithmetic (element_mode (itype),
 element_mode (type))
 && !excess_precision_type (newtype)))
 && !types_match (itype, newtype)
)
				{
				  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail841;
				  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail841;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail841;
				  {
				    tree res_op0;
				    {
				      tree _o1[2], _r1;
				      {
					tree _o2[1], _r2;
					_o2[0] = captures[2];
					if (TREE_TYPE (_o2[0]) != newtype)
					  {
					    _r2 = fold_build1_loc (loc, NOP_EXPR, newtype, _o2[0]);
					  }
					else
					  _r2 = _o2[0];
					_o1[0] = _r2;
				      }
				      {
					tree _o2[1], _r2;
					_o2[0] = captures[4];
					if (TREE_TYPE (_o2[0]) != newtype)
					  {
					    _r2 = fold_build1_loc (loc, NOP_EXPR, newtype, _o2[0]);
					  }
					else
					  _r2 = _o2[0];
					_o1[1] = _r2;
				      }
				      _r1 = fold_build2_loc (loc, op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				      res_op0 = _r1;
				    }
				    tree _r;
				    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 607, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail841:;
				}
			  }
			}
		  }
		}
	  }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_398 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree pmop[2];
 tree utype = fold_bit_and_mask (TREE_TYPE (captures[1]), captures[3], op, captures[1], ERROR_MARK,
 NULL_TREE, NULL_TREE, captures[2], ERROR_MARK,
 NULL_TREE, NULL_TREE, pmop);
      if (utype
)
	{
	  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail875;
	  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail875;
	  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail875;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail875;
	  {
	    tree res_op0;
	    {
	      tree _o1[2], _r1;
	      {
		tree _o2[2], _r2;
		{
		  tree _o3[1], _r3;
		  _o3[0] =  pmop[0];
		  if (TREE_TYPE (_o3[0]) != utype)
		    {
		      _r3 = fold_build1_loc (loc, NOP_EXPR, utype, _o3[0]);
		    }
		  else
		    _r3 = _o3[0];
		  _o2[0] = _r3;
		}
		{
		  tree _o3[1], _r3;
		  _o3[0] =  pmop[1];
		  if (TREE_TYPE (_o3[0]) != utype)
		    {
		      _r3 = fold_build1_loc (loc, NOP_EXPR, utype, _o3[0]);
		    }
		  else
		    _r3 = _o3[0];
		  _o2[1] = _r3;
		}
		_r2 = fold_build2_loc (loc, op, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
		_o1[0] = _r2;
	      }
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[3];
		if (TREE_TYPE (_o2[0]) != utype)
		  {
		    _r2 = fold_build1_loc (loc, NOP_EXPR, utype, _o2[0]);
		  }
		else
		  _r2 = _o2[0];
		_o1[1] = _r2;
	      }
	      _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 628, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail875:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_413 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (eqne))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (eqne == EQ_EXPR
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail895;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail895;
      {
	tree _r;
	_r =  constant_boolean_node (false, type);
	if (TREE_SIDE_EFFECTS (captures[2]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 644, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail895:;
    }
  else
    {
      if (eqne == NE_EXPR
)
	{
	  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail896;
	  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail896;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail896;
	  {
	    tree _r;
	    _r = captures[0];
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 645, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail896:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_418 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (eqne))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (eqne == EQ_EXPR
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail920;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail920;
      {
	tree _r;
	_r =  constant_boolean_node (false, type);
	if (TREE_SIDE_EFFECTS (captures[2]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 644, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail920:;
    }
  else
    {
      if (eqne == NE_EXPR
)
	{
	  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail921;
	  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail921;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail921;
	  {
	    tree _r;
	    _r = captures[0];
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 645, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail921:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_422 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (code2),
 const enum tree_code ARG_UNUSED (code1))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((TREE_CODE (captures[2]) == INTEGER_CST
 && TREE_CODE (captures[5]) == INTEGER_CST)
 || ((INTEGRAL_TYPE_P (TREE_TYPE (captures[2]))
 || POINTER_TYPE_P (TREE_TYPE (captures[2])))
 && bitwise_equal_p (captures[2], captures[5]))
)
    {
      {
 bool one_before = false;
 bool one_after = false;
 int cmp = 0;
 bool allbits = true;
 if (TREE_CODE (captures[2]) == INTEGER_CST
 && TREE_CODE (captures[5]) == INTEGER_CST)
 {
 allbits = TYPE_PRECISION (TREE_TYPE (captures[2])) <= TYPE_PRECISION (TREE_TYPE (captures[5]));
 auto t1 = wi::to_wide (fold_convert (TREE_TYPE (captures[5]), captures[2]));
 auto t2 = wi::to_wide (captures[5]);
 cmp = wi::cmp (t1, t2, TYPE_SIGN (TREE_TYPE (captures[5])));
 if (cmp < 0
 && t1 == t2 - 1)
 one_before = true;
 if (cmp > 0
 && t1 == t2 + 1)
 one_after = true;
 }
 bool val;
 switch (code2)
 {
 case EQ_EXPR: val = (cmp == 0); break;
 case NE_EXPR: val = (cmp != 0); break;
 case LT_EXPR: val = (cmp < 0); break;
 case GT_EXPR: val = (cmp > 0); break;
 case LE_EXPR: val = (cmp <= 0); break;
 case GE_EXPR: val = (cmp >= 0); break;
 default: gcc_unreachable ();
 }
	  if (code1 == EQ_EXPR && val
)
	    {
	      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail926;
	      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail926;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail926;
	      {
		tree _r;
		_r = captures[0];
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 648, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail926:;
	    }
	  else
	    {
	      if (code1 == EQ_EXPR && !val
)
		{
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail927;
		  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail927;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail927;
		  {
		    tree _r;
		    _r =  constant_boolean_node (false, type);
		    if (TREE_SIDE_EFFECTS (captures[1]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 649, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail927:;
		}
	      else
		{
		  if (code1 == NE_EXPR && !val && allbits
)
		    {
		      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail928;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail928;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail928;
		      {
			tree _r;
			_r = captures[3];
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 650, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail928:;
		    }
		  else
		    {
		      if (code1 == NE_EXPR
 && code2 == GE_EXPR
 && cmp == 0
 && allbits
)
			{
			  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail929;
			  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail929;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail929;
			  {
			    tree res_op0;
			    res_op0 = captures[4];
			    tree res_op1;
			    {
			      tree _o1[1], _r1;
			      _o1[0] = captures[2];
			      if (TREE_TYPE (_o1[0]) != TREE_TYPE (res_op0))
				{
				  _r1 = fold_build1_loc (loc, NOP_EXPR, TREE_TYPE (res_op0), _o1[0]);
				}
			      else
			        _r1 = _o1[0];
			      res_op1 = _r1;
			    }
			    tree _r;
			    _r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 651, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail929:;
			}
		      else
			{
			  if (code1 == NE_EXPR
 && code2 == LE_EXPR
 && cmp == 0
 && allbits
)
			    {
			      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail930;
			      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail930;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail930;
			      {
				tree res_op0;
				res_op0 = captures[4];
				tree res_op1;
				{
				  tree _o1[1], _r1;
				  _o1[0] = captures[2];
				  if (TREE_TYPE (_o1[0]) != TREE_TYPE (res_op0))
				    {
				      _r1 = fold_build1_loc (loc, NOP_EXPR, TREE_TYPE (res_op0), _o1[0]);
				    }
				  else
				    _r1 = _o1[0];
				  res_op1 = _r1;
				}
				tree _r;
				_r = fold_build2_loc (loc, LT_EXPR, type, res_op0, res_op1);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 652, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail930:;
			    }
			  else
			    {
			      if (code1 == NE_EXPR
 && code2 == GT_EXPR
 && one_after
 && allbits
)
				{
				  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail931;
				  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail931;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail931;
				  {
				    tree res_op0;
				    res_op0 = captures[4];
				    tree res_op1;
				    {
				      tree _o1[1], _r1;
				      _o1[0] = captures[2];
				      if (TREE_TYPE (_o1[0]) != TREE_TYPE (res_op0))
					{
					  _r1 = fold_build1_loc (loc, NOP_EXPR, TREE_TYPE (res_op0), _o1[0]);
					}
				      else
				        _r1 = _o1[0];
				      res_op1 = _r1;
				    }
				    tree _r;
				    _r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 653, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail931:;
				}
			      else
				{
				  if (code1 == NE_EXPR
 && code2 == LT_EXPR
 && one_before
 && allbits
)
				    {
				      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail932;
				      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail932;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail932;
				      {
					tree res_op0;
					res_op0 = captures[4];
					tree res_op1;
					{
					  tree _o1[1], _r1;
					  _o1[0] = captures[2];
					  if (TREE_TYPE (_o1[0]) != TREE_TYPE (res_op0))
					    {
					      _r1 = fold_build1_loc (loc, NOP_EXPR, TREE_TYPE (res_op0), _o1[0]);
					    }
					  else
					    _r1 = _o1[0];
					  res_op1 = _r1;
					}
					tree _r;
					_r = fold_build2_loc (loc, LT_EXPR, type, res_op0, res_op1);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 654, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail932:;
				    }
				}
			    }
			}
		    }
		}
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_450 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (! TYPE_UNSIGNED (type)
 && ! HONOR_SIGN_DEPENDENT_ROUNDING (type)
 && single_use (captures[0])
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail972;
      {
	tree res_op0;
	res_op0 = captures[1];
	tree res_op1;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[2];
	  _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 684, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail972:;
    }
  return NULL_TREE;
}

tree
generic_simplify_457 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (minmax))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail980;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail980;
  {
    tree _r;
    _r = captures[0];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 691, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail980:;
  return NULL_TREE;
}

tree
generic_simplify_465 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op),
 const enum tree_code ARG_UNUSED (logic))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1000;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 = captures[1];
    tree _r;
    _r = fold_build2_loc (loc, logic, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 699, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1000:;
  return NULL_TREE;
}

tree
generic_simplify_470 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (FMIN_ALL))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_finite_math_only
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1005;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 = captures[1];
	tree _r;
	_r = fold_build2_loc (loc, MIN_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 704, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1005:;
    }
  return NULL_TREE;
}

tree
generic_simplify_474 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (minmax))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree_code code = minmax_from_comparison (cmp, captures[0], captures[3], captures[0], captures[1]);
      if (code == MIN_EXPR
)
	{
	  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1012;
	  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1012;
	  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1012;
	  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1012;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1012;
	  {
	    tree res_op0;
	    {
	      tree _o1[2], _r1;
	      _o1[0] = captures[0];
	      _o1[1] = captures[3];
	      _r1 = fold_build2_loc (loc, MIN_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      res_op0 = _r1;
	    }
	    tree res_op1;
	    res_op1 = captures[2];
	    tree _r;
	    _r = fold_build2_loc (loc, minmax, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 711, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1012:;
	}
      else
	{
	  if (code == MAX_EXPR
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1013;
	      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1013;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1013;
	      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1013;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1013;
	      {
		tree res_op0;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  res_op0 = _r1;
		}
		tree res_op1;
		res_op1 = captures[2];
		tree _r;
		_r = fold_build2_loc (loc, minmax, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 712, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1013:;
	    }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_490 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1030;
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1030;
  if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1030;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1030;
  {
    tree _r;
    _r = captures[3];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 729, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1030:;
  return NULL_TREE;
}

tree
generic_simplify_497 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && !TYPE_UNSIGNED (TREE_TYPE (captures[0]))
 && TYPE_PRECISION (TREE_TYPE (captures[0])) == TYPE_PRECISION (type)
)
    {
      {
 tree shifter = build_int_cst (integer_type_node, TYPE_PRECISION (type) - 1);
	  if (cmp == GE_EXPR
)
	    {
	      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1039;
	      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1039;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1039;
	      {
		tree res_op0;
		{
		  tree _o1[1], _r1;
		  {
		    tree _o2[2], _r2;
		    _o2[0] = captures[0];
		    _o2[1] = shifter;
		    _r2 = fold_build2_loc (loc, RSHIFT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
		    _o1[0] = _r2;
		  }
		  if (TREE_TYPE (_o1[0]) != type)
		    {
		      _r1 = fold_build1_loc (loc, NOP_EXPR, type, _o1[0]);
		    }
		  else
		    _r1 = _o1[0];
		  res_op0 = _r1;
		}
		tree res_op1;
		res_op1 = captures[1];
		tree _r;
		_r = fold_build2_loc (loc, BIT_XOR_EXPR, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 737, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1039:;
	    }
	  else
	    {
	      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1040;
	      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1040;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1040;
	      {
		tree res_op0;
		{
		  tree _o1[2], _r1;
		  {
		    tree _o2[1], _r2;
		    {
		      tree _o3[2], _r3;
		      _o3[0] = captures[0];
		      _o3[1] = shifter;
		      _r3 = fold_build2_loc (loc, RSHIFT_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
		      _o2[0] = _r3;
		    }
		    if (TREE_TYPE (_o2[0]) != type)
		      {
			_r2 = fold_build1_loc (loc, NOP_EXPR, type, _o2[0]);
		      }
		    else
		      _r2 = _o2[0];
		    _o1[0] = _r2;
		  }
		  _o1[1] = captures[1];
		  _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  res_op0 = _r1;
		}
		tree _r;
		_r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 738, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1040:;
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_516 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (cond_len_op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree op_type = TREE_TYPE (captures[6]);
      if (inverse_conditions_p (captures[0], captures[2])
 && element_precision (type) == element_precision (op_type)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1061;
	  {
	    tree res_op0;
	    {
	      tree _o1[7], _r1;
	      _o1[0] = captures[2];
	      _o1[1] = captures[3];
	      _o1[2] = captures[4];
	      _o1[3] = captures[5];
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[1];
		_r2 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, op_type, _o2[0]);
		_o1[4] = _r2;
	      }
	      _o1[5] = captures[7];
	      _o1[6] = captures[8];
	      _r1 = maybe_build_call_expr_loc (loc, cond_len_op, TREE_TYPE (_o1[1]), 7, _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5], _o1[6]);
	      if (!_r1)
	        goto next_after_fail1061;
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (TREE_SIDE_EFFECTS (captures[6]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[6]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 754, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1061:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_528 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (sins),
 const combined_fn ARG_UNUSED (atans),
 const combined_fn ARG_UNUSED (sqrts),
 const combined_fn ARG_UNUSED (copysigns))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 REAL_VALUE_TYPE r_cst;
 build_sinatan_real (&r_cst, type);
 tree t_cst = build_real (type, r_cst);
 tree t_one = build_one_cst (type);
      if (SCALAR_FLOAT_TYPE_P (type)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1073;
	  {
	    if (! tree_invariant_p (captures[1])) goto next_after_fail1073;
	    tree res_op0;
	    {
	      tree _o1[2], _r1;
	      {
		tree _o2[1], _r2;
		_o2[0] = unshare_expr (captures[1]);
		_r2 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
		_o1[0] = _r2;
	      }
	      _o1[1] =  t_cst;
	      _r1 = fold_build2_loc (loc, LT_EXPR, boolean_type_node, _o1[0], _o1[1]);
	      res_op0 = _r1;
	    }
	    tree res_op1;
	    {
	      tree _o1[2], _r1;
	      _o1[0] = unshare_expr (captures[1]);
	      {
		tree _o2[1], _r2;
		{
		  tree _o3[2], _r3;
		  {
		    tree _o4[2], _r4;
		    _o4[0] = unshare_expr (captures[1]);
		    _o4[1] = unshare_expr (captures[1]);
		    _r4 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o4[0]), _o4[0], _o4[1]);
		    _o3[0] = _r4;
		  }
		  _o3[1] =  t_one;
		  _r3 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
		  _o2[0] = _r3;
		}
		_r2 = maybe_build_call_expr_loc (loc, sqrts, TREE_TYPE (_o2[0]), 1, _o2[0]);
		if (!_r2)
		  goto next_after_fail1073;
		_o1[1] = _r2;
	      }
	      _r1 = fold_build2_loc (loc, RDIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      res_op1 = _r1;
	    }
	    tree res_op2;
	    {
	      tree _o1[2], _r1;
	      _o1[0] =  t_one;
	      _o1[1] = captures[1];
	      _r1 = maybe_build_call_expr_loc (loc, copysigns, TREE_TYPE (_o1[0]), 2, _o1[0], _o1[1]);
	      if (!_r1)
	        goto next_after_fail1073;
	      res_op2 = _r1;
	    }
	    tree _r;
	    _r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 766, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1073:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_551 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (canonicalize_math_after_vectorization_p ()
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1096;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 = captures[1];
	tree res_op2;
	res_op2 = captures[2];
	tree _r;
	_r = maybe_build_call_expr_loc (loc, CFN_FMA, type, 3, res_op0, res_op1, res_op2);
	if (!_r)
	  goto next_after_fail1096;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 789, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1096:;
    }
  return NULL_TREE;
}

tree
generic_simplify_560 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (rot),
 const combined_fn ARG_UNUSED (popcount))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && (
0
 || !TREE_SIDE_EFFECTS (captures[3]))
)
    {
      {
 tree type0 = TREE_TYPE (captures[0]);
 tree type1 = TREE_TYPE (captures[1]);
 unsigned int prec0 = TYPE_PRECISION (type0);
 unsigned int prec1 = TYPE_PRECISION (type1);
	  if (prec0 == prec1 || (prec0 > prec1 && TYPE_UNSIGNED (type1))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1105;
	      {
		tree res_op0;
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[2];
		  if (TREE_TYPE (_o1[0]) != type0)
		    {
		      _r1 = fold_build1_loc (loc, NOP_EXPR, type0, _o1[0]);
		    }
		  else
		    _r1 = _o1[0];
		  res_op0 = _r1;
		}
		tree _r;
		_r = maybe_build_call_expr_loc (loc, popcount, type, 1, res_op0);
		if (!_r)
		  goto next_after_fail1105;
		if (TREE_SIDE_EFFECTS (captures[3]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 795, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1105:;
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_570 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (reduc))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree ctor = (TREE_CODE (captures[0]) == SSA_NAME
 ? gimple_assign_rhs1 (SSA_NAME_DEF_STMT (captures[0])) : captures[0]);
 tree elt = ctor_single_nonzero_element (ctor);
      if (elt
 && !HONOR_SNANS (type)
 && !HONOR_SIGNED_ZEROS (type)
)
	{
	  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1116;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1116;
	  {
	    tree _r;
	    _r =  elt;
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 802, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1116:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_VIEW_CONVERT_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if ((
0
 && useless_type_conversion_p (type, TREE_TYPE (captures[0])))
 || (
1
 && type == TREE_TYPE (captures[0]))
)
      {
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1171;
	{
	  tree _r;
	  _r = captures[0];
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 815, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1171:;
      }
  }
  switch (TREE_CODE (_p0))
    {
    case VIEW_CONVERT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1172;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree _r;
	    _r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 846, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1172:;
	}
        break;
      }
    default:;
    }
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if ((INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type))
 && (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0])))
 && TYPE_PRECISION (type) == TYPE_PRECISION (TREE_TYPE (captures[0]))
)
      {
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1173;
	{
	  tree res_op0;
	  res_op0 = captures[0];
	  tree _r;
	  _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 847, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1173:;
      }
  }
  switch (TREE_CODE (_p0))
    {
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
	  if ((INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0])))
 && (INTEGRAL_TYPE_P (TREE_TYPE (captures[1])) || POINTER_TYPE_P (TREE_TYPE (captures[1])))
 && TYPE_SIZE (TREE_TYPE (captures[0])) == TYPE_SIZE (TREE_TYPE (captures[1]))
 && (TYPE_PRECISION (TREE_TYPE (captures[0])) == TYPE_PRECISION (TREE_TYPE (captures[1]))
 || (TYPE_PRECISION (TREE_TYPE (captures[0])) > TYPE_PRECISION (TREE_TYPE (captures[1]))
 && TYPE_UNSIGNED (TREE_TYPE (captures[1]))))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1174;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree _r;
		_r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 848, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1174:;
	    }
	}
        break;
      }
    case CONSTRUCTOR:
      {
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	  {
 tree ctor = (TREE_CODE (captures[0]) == SSA_NAME
 ? gimple_assign_rhs1 (SSA_NAME_DEF_STMT (captures[0])) : captures[0]);
	      if (CONSTRUCTOR_NELTS (ctor) == 0
)
		{
		  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1175;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1175;
		  {
		    tree _r;
		    _r =  build_zero_cst (type);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 849, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1175:;
		}
	      else
		{
		  if (CONSTRUCTOR_NELTS (ctor) == 1
 && VECTOR_TYPE_P (TREE_TYPE (ctor))
 && operand_equal_p (TYPE_SIZE (type),
 TYPE_SIZE (TREE_TYPE
 (CONSTRUCTOR_ELT (ctor, 0)->value)))
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1176;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1176;
		      {
			tree res_op0;
			res_op0 =  CONSTRUCTOR_ELT (ctor, 0)->value;
			tree _r;
			_r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 850, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1176:;
		    }
		}
	  }
	}
        break;
      }
    case VEC_COND_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	tree _q22 = TREE_OPERAND (_p0, 2);
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q22 };
	  if (VECTOR_TYPE_P (type) && VECTOR_TYPE_P (TREE_TYPE (captures[2]))
 && known_eq (TYPE_VECTOR_SUBPARTS (type),
 TYPE_VECTOR_SUBPARTS (TREE_TYPE (captures[2])))
 && tree_nop_conversion_p (TREE_TYPE (type), TREE_TYPE (TREE_TYPE (captures[2])))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1177;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[2];
		  _r1 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, _o1[0]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1177;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[3];
		  _r1 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, _o1[0]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1177;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 851, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1177:;
	    }
	}
        break;
      }
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_CONJ_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case CONJ_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      {
		tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
		tree res = generic_simplify_500 (loc, type, _p0, captures);
		if (res) return res;
	      }
	      break;
	    }
	  case COMPLEX_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31 };
		{
 tree itype = TREE_TYPE (type);
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1236;
		    {
		      tree res_op0;
		      {
			tree _o1[1], _r1;
			_o1[0] = captures[2];
			if (TREE_TYPE (_o1[0]) != itype)
			  {
			    _r1 = fold_build1_loc (loc, NOP_EXPR, itype, _o1[0]);
			  }
			else
			  _r1 = _o1[0];
			res_op0 = _r1;
		      }
		      tree res_op1;
		      {
			tree _o1[1], _r1;
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[3];
			  if (TREE_TYPE (_o2[0]) != itype)
			    {
			      _r2 = fold_build1_loc (loc, NOP_EXPR, itype, _o2[0]);
			    }
			  else
			    _r2 = _o2[0];
			  _o1[0] = _r2;
			}
			_r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
			res_op1 = _r1;
		      }
		      tree _r;
		      _r = fold_build2_loc (loc, COMPLEX_EXPR, type, res_op0, res_op1);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 861, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1236:;
		}
	      }
	      break;
	    }
          default:;
          }
        break;
      }
    case CONJ_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	  tree res = generic_simplify_500 (loc, type, _p0, captures);
	  if (res) return res;
	}
        break;
      }
    case COMPLEX_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21 };
	  {
 tree itype = TREE_TYPE (type);
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1237;
	      {
		tree res_op0;
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[2];
		  if (TREE_TYPE (_o1[0]) != itype)
		    {
		      _r1 = fold_build1_loc (loc, NOP_EXPR, itype, _o1[0]);
		    }
		  else
		    _r1 = _o1[0];
		  res_op0 = _r1;
		}
		tree res_op1;
		{
		  tree _o1[1], _r1;
		  {
		    tree _o2[1], _r2;
		    _o2[0] = captures[3];
		    if (TREE_TYPE (_o2[0]) != itype)
		      {
			_r2 = fold_build1_loc (loc, NOP_EXPR, itype, _o2[0]);
		      }
		    else
		      _r2 = _o2[0];
		    _o1[0] = _r2;
		  }
		  _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
		  res_op1 = _r1;
		}
		tree _r;
		_r = fold_build2_loc (loc, COMPLEX_EXPR, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 861, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1237:;
	  }
	}
        break;
      }
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_POINTER_PLUS_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1251;
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1251;
	{
	  tree res_op0;
	  res_op0 = captures[0];
	  tree _r;
	  _r = non_lvalue_loc (loc, res_op0);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 862, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1251:;
      }
    }
  switch (TREE_CODE (_p0))
    {
    case POINTER_PLUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1252;
	  {
	    tree res_op0;
	    res_op0 = captures[1];
	    tree res_op1;
	    {
	      tree _o1[2], _r1;
	      _o1[0] = captures[2];
	      _o1[1] = captures[3];
	      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      res_op1 = _r1;
	    }
	    tree _r;
	    _r = fold_build2_loc (loc, POINTER_PLUS_EXPR, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 872, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1252:;
	}
        break;
      }
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case POINTER_PLUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1 };
		if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1253;
		{
		  tree res_op0;
		  {
		    tree _o1[2], _r1;
		    _o1[0] = captures[2];
		    {
		      tree _o2[2], _r2;
		      _o2[0] = captures[3];
		      _o2[1] = captures[4];
		      _r2 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
		      _o1[1] = _r2;
		    }
		    _r1 = fold_build2_loc (loc, POINTER_PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		    res_op0 = _r1;
		  }
		  tree _r;
		  _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 873, __FILE__, __LINE__, true);
		  return _r;
		}
next_after_fail1253:;
	      }
	      break;
	    }
          default:;
          }
        break;
      }
    default:;
    }
if (integer_zerop (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p1 };
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1254;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1254;
      {
	tree res_op0;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  if (TREE_TYPE (_o1[0]) != type)
	    {
	      _r1 = fold_build1_loc (loc, NOP_EXPR, type, _o1[0]);
	    }
	  else
	    _r1 = _o1[0];
	  res_op0 = _r1;
	}
	tree _r;
	_r = non_lvalue_loc (loc, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 874, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1254:;
    }
  }
  switch (TREE_CODE (_p1))
    {
    CASE_CONVERT:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	switch (TREE_CODE (_q30))
	  {
	  case MINUS_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      switch (TREE_CODE (_q40))
	        {
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    switch (TREE_CODE (_q41))
		      {
		      CASE_CONVERT:
		        {
			  tree _q70 = TREE_OPERAND (_q41, 0);
			  if ((_q70 == _p0 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _p0, 0) && types_match (_q70, _p0)))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q50 };
				if (TYPE_PRECISION (TREE_TYPE (captures[1])) == TYPE_PRECISION (TREE_TYPE (captures[2]))
 && ((
0
 && useless_type_conversion_p (type, TREE_TYPE (captures[3])))
 || (
1
 && type == TREE_TYPE (captures[3])))
)
				  {
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1255;
				    {
				      tree _r;
				      _r = captures[3];
				      if (TREE_SIDE_EFFECTS (captures[0]))
					_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 875, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1255:;
				  }
			      }
			    }
		          break;
		        }
		      default:;
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
	  case POINTER_DIFF_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || operand_equal_p (_q41, _p0, 0))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q41, _p1, _q30, _q40, _p0 };
		    if (TYPE_PRECISION (TREE_TYPE (captures[1])) >= TYPE_PRECISION (TREE_TYPE (captures[2]))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1256;
			{
			  tree res_op0;
			  res_op0 = captures[3];
			  tree _r;
			  _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
			  if (TREE_SIDE_EFFECTS (captures[4]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[4]), _r);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 876, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1256:;
		      }
		  }
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case MINUS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	switch (TREE_CODE (_q30))
	  {
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      switch (TREE_CODE (_q31))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_q31, 0);
		    if ((_q60 == _p0 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p0, 0) && types_match (_q60, _p0)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _p1, _q40 };
			  if (TYPE_PRECISION (TREE_TYPE (captures[1])) == TYPE_PRECISION (TREE_TYPE (captures[2]))
 && ((
0
 && useless_type_conversion_p (type, TREE_TYPE (captures[3])))
 || (
1
 && type == TREE_TYPE (captures[3])))
)
			    {
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1257;
			      {
				tree _r;
				_r = captures[3];
				if (TREE_SIDE_EFFECTS (captures[0]))
				  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 875, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail1257:;
			    }
			}
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case POINTER_DIFF_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || operand_equal_p (_q31, _p0, 0))
	  {
	    {
	      tree captures[5] ATTRIBUTE_UNUSED = { _q31, _p1, _p1, _q30, _p0 };
	      if (TYPE_PRECISION (TREE_TYPE (captures[1])) >= TYPE_PRECISION (TREE_TYPE (captures[2]))
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1258;
		  {
		    tree res_op0;
		    res_op0 = captures[3];
		    tree _r;
		    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		    if (TREE_SIDE_EFFECTS (captures[4]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[4]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 876, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1258:;
		}
	    }
	  }
        break;
      }
    case NEGATE_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	switch (TREE_CODE (_q30))
	  {
	  case BIT_AND_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      switch (TREE_CODE (_q40))
	        {
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
		      {
			switch (TREE_CODE (_q41))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q41 };
				{
 tree algn = wide_int_to_tree (TREE_TYPE (captures[0]), ~wi::to_wide (captures[1]));
				    if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1259;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1259;
				    {
				      tree res_op0;
				      res_op0 = captures[0];
				      tree res_op1;
				      res_op1 =  algn;
				      tree _r;
				      _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 877, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1259:;
				}
			      }
			      break;
			    }
		          default:;
		          }
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_NE_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p1))
    {
    case NEGATE_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	      tree res = generic_simplify_190 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case NEGATE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	      tree res = generic_simplify_190 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case CEIL_MOD_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_pow2p (_q21))
	  {
	    if (integer_zerop (_p1))
	      {
		{
		  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		  tree res = generic_simplify_191 (loc, type, _p0, _p1, captures, NE_EXPR, CEIL_MOD_EXPR);
		  if (res) return res;
		}
	      }
	  }
        break;
      }
    case FLOOR_MOD_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_pow2p (_q21))
	  {
	    if (integer_zerop (_p1))
	      {
		{
		  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		  tree res = generic_simplify_191 (loc, type, _p0, _p1, captures, NE_EXPR, FLOOR_MOD_EXPR);
		  if (res) return res;
		}
	      }
	  }
        break;
      }
    case ROUND_MOD_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_pow2p (_q21))
	  {
	    if (integer_zerop (_p1))
	      {
		{
		  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		  tree res = generic_simplify_191 (loc, type, _p0, _p1, captures, NE_EXPR, ROUND_MOD_EXPR);
		  if (res) return res;
		}
	      }
	  }
        break;
      }
    case TRUNC_MOD_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_pow2p (_q21))
	  {
	    if (integer_zerop (_p1))
	      {
		{
		  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		  tree res = generic_simplify_191 (loc, type, _p0, _p1, captures, NE_EXPR, TRUNC_MOD_EXPR);
		  if (res) return res;
		}
	      }
	  }
        break;
      }
    case LSHIFT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (tree_zero_one_valued_p (_q20))
	  {
	    switch (TREE_CODE (_q21))
	      {
	      case INTEGER_CST:
	        {
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			if (tree_fits_shwi_p (captures[1])
 && tree_to_shwi (captures[1]) > 0
 && tree_to_shwi (captures[1]) < TYPE_PRECISION (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1541;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree _r;
			      _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
			      if (TREE_SIDE_EFFECTS (captures[1]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			      if (TREE_SIDE_EFFECTS (captures[2]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 958, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1541:;
			  }
		      }
		    }
	          break;
	        }
	      default:;
	      }
	  }
        break;
      }
    case MULT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case MULT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
		    tree res = generic_simplify_194 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
		    tree res = generic_simplify_194 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
		    tree res = generic_simplify_194 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
		    tree res = generic_simplify_194 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case MULT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    tree _q51 = TREE_OPERAND (_p1, 1);
		    if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
			  tree res = generic_simplify_195 (loc, type, _p0, _p1, captures, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case EXACT_DIV_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_q31))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      CASE_CONVERT:
		        {
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  switch (TREE_CODE (_q60))
			    {
			    case EXACT_DIV_EXPR:
			      {
				tree _q70 = TREE_OPERAND (_q60, 0);
				tree _q71 = TREE_OPERAND (_q60, 1);
				if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q30, _q31, _q70 };
				      tree res = generic_simplify_196 (loc, type, _p0, _p1, captures, NE_EXPR);
				      if (res) return res;
				    }
				  }
			        break;
			      }
			    default:;
			    }
		          break;
		        }
		      default:;
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_p1))
	  {
	  case PLUS_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      tree _q41 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q41))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_q41, 0);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q40, _q20 };
			  tree res = generic_simplify_198 (loc, type, _p0, _p1, captures, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q40))
	        {
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q41, _q20 };
			  tree res = generic_simplify_198 (loc, type, _p0, _p1, captures, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      if ((_q41 == _q20 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _q20, 0) && types_match (_q41, _q20)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q40, _q20 };
		    tree res = generic_simplify_198 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q41, _q20 };
		    tree res = generic_simplify_198 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
      {
	tree _p1_pops[1];
	if (tree_nop_convert (_p1, _p1_pops))
	  {
	    tree _q40 = _p1_pops[0];
	    switch (TREE_CODE (_q40))
	      {
	      case PLUS_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_q40, 0);
		  tree _q51 = TREE_OPERAND (_q40, 1);
		  switch (TREE_CODE (_q51))
		    {
		    CASE_CONVERT:
		      {
			tree _q70 = TREE_OPERAND (_q51, 0);
			if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q20 };
			      tree res = generic_simplify_197 (loc, type, _p0, _p1, captures, NE_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
		  switch (TREE_CODE (_q50))
		    {
		    CASE_CONVERT:
		      {
			tree _q60 = TREE_OPERAND (_q50, 0);
			if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q40, _q51, _q20 };
			      tree res = generic_simplify_197 (loc, type, _p0, _p1, captures, NE_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q20 };
			tree res = generic_simplify_197 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
		    }
		  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q40, _q51, _q20 };
			tree res = generic_simplify_197 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	  }
      }
        break;
      }
    case EXACT_DIV_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case EXACT_DIV_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    tree _q51 = TREE_OPERAND (_p1, 1);
		    if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q50 };
			  tree res = generic_simplify_196 (loc, type, _p0, _p1, captures, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case TRUNC_DIV_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
		      tree res = generic_simplify_199 (loc, type, _p0, _p1, captures, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case PLUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case PLUS_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
		    if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))
 || TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[0])))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1542;
			{
			  tree res_op0;
			  res_op0 = captures[0];
			  tree res_op1;
			  res_op1 = captures[2];
			  tree _r;
			  _r = fold_build2_loc (loc, NE_EXPR, type, res_op0, res_op1);
			  if (TREE_SIDE_EFFECTS (captures[1]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 892, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1542:;
		      }
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
		    if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))
 || TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[0])))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1543;
			{
			  tree res_op0;
			  res_op0 = captures[0];
			  tree res_op1;
			  res_op1 = captures[2];
			  tree _r;
			  _r = fold_build2_loc (loc, NE_EXPR, type, res_op0, res_op1);
			  if (TREE_SIDE_EFFECTS (captures[1]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 892, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1543:;
		      }
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
		    if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))
 || TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[0])))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1544;
			{
			  tree res_op0;
			  res_op0 = captures[0];
			  tree res_op1;
			  res_op1 = captures[2];
			  tree _r;
			  _r = fold_build2_loc (loc, NE_EXPR, type, res_op0, res_op1);
			  if (TREE_SIDE_EFFECTS (captures[1]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 892, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1544:;
		      }
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
		    if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))
 || TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[0])))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1545;
			{
			  tree res_op0;
			  res_op0 = captures[0];
			  tree res_op1;
			  res_op1 = captures[2];
			  tree _r;
			  _r = fold_build2_loc (loc, NE_EXPR, type, res_op0, res_op1);
			  if (TREE_SIDE_EFFECTS (captures[1]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 892, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1545:;
		      }
		  }
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q21))
	  {
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      switch (TREE_CODE (_p1))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q40 };
			  tree res = generic_simplify_198 (loc, type, _p0, _p1, captures, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q20))
	  {
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_p1))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q30 };
			  tree res = generic_simplify_198 (loc, type, _p0, _p1, captures, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q30 };
		    tree res = generic_simplify_198 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q21))
	  {
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q40 };
		    tree res = generic_simplify_198 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_p1))
	  {
	  CASE_CONVERT:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21 };
		    tree res = generic_simplify_198 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q20 };
		    tree res = generic_simplify_198 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21 };
	      tree res = generic_simplify_198 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q20 };
	      tree res = generic_simplify_198 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case POINTER_PLUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case POINTER_PLUS_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
		    tree res = generic_simplify_200 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q20))
	  {
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_p1))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q30, _q21 };
			  tree res = generic_simplify_201 (loc, type, _p0, _p1, captures, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q30, _q21 };
		    tree res = generic_simplify_201 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_p1))
	  {
	  CASE_CONVERT:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21 };
		    tree res = generic_simplify_201 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21 };
	      tree res = generic_simplify_201 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case MINUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case MINUS_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
		    if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))
 || TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[0])))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1546;
			{
			  tree res_op0;
			  res_op0 = captures[0];
			  tree res_op1;
			  res_op1 = captures[2];
			  tree _r;
			  _r = fold_build2_loc (loc, NE_EXPR, type, res_op0, res_op1);
			  if (TREE_SIDE_EFFECTS (captures[1]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 893, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1546:;
		      }
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
		    if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[1]))
 || TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[1])))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1547;
			{
			  tree res_op0;
			  res_op0 = captures[2];
			  tree res_op1;
			  res_op1 = captures[1];
			  tree _r;
			  _r = fold_build2_loc (loc, NE_EXPR, type, res_op0, res_op1);
			  if (TREE_SIDE_EFFECTS (captures[0]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 894, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1547:;
		      }
		  }
	        }
	      break;
	    }
          default:;
          }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p0, _q21 };
	      tree res = generic_simplify_202 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case POINTER_DIFF_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case POINTER_DIFF_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q50 };
		    tree res = generic_simplify_203 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q51 };
		    tree res = generic_simplify_204 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    default:;
    }
{
  tree _p0_pops[1];
  if (tree_logical_inverted_value (_p0, _p0_pops))
    {
      tree _q20 = _p0_pops[0];
      if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	{
	  if (tree_truth_valued_p (_p1))
	    {
	      {
		tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		tree res = generic_simplify_192 (loc, type, _p0, _p1, captures, NE_EXPR);
		if (res) return res;
	      }
	    }
        }
    }
}
{
  tree _p0_pops[1];
  if (tree_nop_convert (_p0, _p0_pops))
    {
      tree _q20 = _p0_pops[0];
      switch (TREE_CODE (_q20))
        {
	case PLUS_EXPR:
	  {
	    tree _q30 = TREE_OPERAND (_q20, 0);
	    tree _q31 = TREE_OPERAND (_q20, 1);
	    switch (TREE_CODE (_q31))
	      {
	      CASE_CONVERT:
	        {
		  tree _q50 = TREE_OPERAND (_q31, 0);
		  switch (TREE_CODE (_p1))
		    {
		    CASE_CONVERT:
		      {
			tree _q70 = TREE_OPERAND (_p1, 0);
			if ((_q70 == _q50 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q50, 0) && types_match (_q70, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q50 };
			      tree res = generic_simplify_197 (loc, type, _p0, _p1, captures, NE_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
	          break;
	        }
	      default:;
	      }
	    switch (TREE_CODE (_q30))
	      {
	      CASE_CONVERT:
	        {
		  tree _q40 = TREE_OPERAND (_q30, 0);
		  switch (TREE_CODE (_p1))
		    {
		    CASE_CONVERT:
		      {
			tree _q70 = TREE_OPERAND (_p1, 0);
			if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q40 };
			      tree res = generic_simplify_197 (loc, type, _p0, _p1, captures, NE_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
		  if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q40 };
			tree res = generic_simplify_197 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	    switch (TREE_CODE (_q31))
	      {
	      CASE_CONVERT:
	        {
		  tree _q50 = TREE_OPERAND (_q31, 0);
		  if ((_p1 == _q50 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q50, 0) && types_match (_p1, _q50)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q50 };
			tree res = generic_simplify_197 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	    switch (TREE_CODE (_p1))
	      {
	      CASE_CONVERT:
	        {
		  tree _q60 = TREE_OPERAND (_p1, 0);
		  if ((_q60 == _q31 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q31, 0) && types_match (_q60, _q31)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31 };
			tree res = generic_simplify_197 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
		    }
		  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q30 };
			tree res = generic_simplify_197 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	    if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
	      {
		{
		  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31 };
		  tree res = generic_simplify_197 (loc, type, _p0, _p1, captures, NE_EXPR);
		  if (res) return res;
		}
	      }
	    if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
	      {
		{
		  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q30 };
		  tree res = generic_simplify_197 (loc, type, _p0, _p1, captures, NE_EXPR);
		  if (res) return res;
		}
	      }
	    break;
	  }
	case POINTER_PLUS_EXPR:
	  {
	    tree _q30 = TREE_OPERAND (_q20, 0);
	    tree _q31 = TREE_OPERAND (_q20, 1);
	    switch (TREE_CODE (_q30))
	      {
	      CASE_CONVERT:
	        {
		  tree _q40 = TREE_OPERAND (_q30, 0);
		  switch (TREE_CODE (_p1))
		    {
		    CASE_CONVERT:
		      {
			tree _q70 = TREE_OPERAND (_p1, 0);
			if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q40, _q31 };
			      tree res = generic_simplify_205 (loc, type, _p0, _p1, captures, NE_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
		  if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q40, _q31 };
			tree res = generic_simplify_205 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	    switch (TREE_CODE (_p1))
	      {
	      CASE_CONVERT:
	        {
		  tree _q60 = TREE_OPERAND (_p1, 0);
		  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31 };
			tree res = generic_simplify_205 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	    if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
	      {
		{
		  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31 };
		  tree res = generic_simplify_205 (loc, type, _p0, _p1, captures, NE_EXPR);
		  if (res) return res;
		}
	      }
	    break;
	  }
        default:;
        }
    }
}
if (tree_truth_valued_p (_p0))
  {
    {
      tree _p1_pops[1];
      if (tree_logical_inverted_value (_p1, _p1_pops))
        {
	  tree _q30 = _p1_pops[0];
	  if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	    {
	      {
		tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
		tree res = generic_simplify_192 (loc, type, _p0, _p1, captures, NE_EXPR);
		if (res) return res;
	      }
	    }
        }
    }
  }
  switch (TREE_CODE (_p1))
    {
    case PLUS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	switch (TREE_CODE (_q31))
	  {
	  CASE_CONVERT:
	    {
	      tree _q50 = TREE_OPERAND (_q31, 0);
	      if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q30, _p0 };
		    tree res = generic_simplify_198 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q30))
	  {
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q31, _p0 };
		    tree res = generic_simplify_198 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q30, _p0 };
	      tree res = generic_simplify_198 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q31, _p0 };
	      tree res = generic_simplify_198 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
{
  tree _p1_pops[1];
  if (tree_nop_convert (_p1, _p1_pops))
    {
      tree _q30 = _p1_pops[0];
      switch (TREE_CODE (_q30))
        {
	case PLUS_EXPR:
	  {
	    tree _q40 = TREE_OPERAND (_q30, 0);
	    tree _q41 = TREE_OPERAND (_q30, 1);
	    switch (TREE_CODE (_q41))
	      {
	      CASE_CONVERT:
	        {
		  tree _q60 = TREE_OPERAND (_q41, 0);
		  if ((_q60 == _p0 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p0, 0) && types_match (_q60, _p0)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q30, _q40, _p0 };
			tree res = generic_simplify_197 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	    switch (TREE_CODE (_q40))
	      {
	      CASE_CONVERT:
	        {
		  tree _q50 = TREE_OPERAND (_q40, 0);
		  if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q30, _q41, _p0 };
			tree res = generic_simplify_197 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	    if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
	      {
		{
		  tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q30, _q40, _p0 };
		  tree res = generic_simplify_197 (loc, type, _p0, _p1, captures, NE_EXPR);
		  if (res) return res;
		}
	      }
	    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
	      {
		{
		  tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q30, _q41, _p0 };
		  tree res = generic_simplify_197 (loc, type, _p0, _p1, captures, NE_EXPR);
		  if (res) return res;
		}
	      }
	    break;
	  }
        default:;
        }
    }
}
  switch (TREE_CODE (_p0))
    {
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_p1))
	  {
	  case POINTER_PLUS_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      tree _q41 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q40))
	        {
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q20, _q41 };
			  tree res = generic_simplify_201 (loc, type, _p0, _p1, captures, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q20, _q41 };
		    tree res = generic_simplify_201 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
      {
	tree _p1_pops[1];
	if (tree_nop_convert (_p1, _p1_pops))
	  {
	    tree _q40 = _p1_pops[0];
	    switch (TREE_CODE (_q40))
	      {
	      case POINTER_PLUS_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_q40, 0);
		  tree _q51 = TREE_OPERAND (_q40, 1);
		  switch (TREE_CODE (_q50))
		    {
		    CASE_CONVERT:
		      {
			tree _q60 = TREE_OPERAND (_q50, 0);
			if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q40, _q20, _q51 };
			      tree res = generic_simplify_205 (loc, type, _p0, _p1, captures, NE_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
		  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q40, _q20, _q51 };
			tree res = generic_simplify_205 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	  }
      }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case POINTER_PLUS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	switch (TREE_CODE (_q30))
	  {
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _p0, _q31 };
		    tree res = generic_simplify_201 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _p0, _q31 };
	      tree res = generic_simplify_201 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
{
  tree _p1_pops[1];
  if (tree_nop_convert (_p1, _p1_pops))
    {
      tree _q30 = _p1_pops[0];
      switch (TREE_CODE (_q30))
        {
	case POINTER_PLUS_EXPR:
	  {
	    tree _q40 = TREE_OPERAND (_q30, 0);
	    tree _q41 = TREE_OPERAND (_q30, 1);
	    switch (TREE_CODE (_q40))
	      {
	      CASE_CONVERT:
	        {
		  tree _q50 = TREE_OPERAND (_q40, 0);
		  if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q30, _p0, _q41 };
			tree res = generic_simplify_205 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
	      {
		{
		  tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q30, _p0, _q41 };
		  tree res = generic_simplify_205 (loc, type, _p0, _p1, captures, NE_EXPR);
		  if (res) return res;
		}
	      }
	    break;
	  }
        default:;
        }
    }
}
  switch (TREE_CODE (_p0))
    {
    case ADDR_EXPR:
      {
	switch (TREE_CODE (_p1))
	  {
	  case POINTER_PLUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_p1, 0);
	      tree _q31 = TREE_OPERAND (_p1, 1);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q31 };
		tree res = generic_simplify_206 (loc, type, _p0, _p1, captures, NE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
        break;
      }
    case POINTER_PLUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case ADDR_EXPR:
	    {
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q21 };
		tree res = generic_simplify_206 (loc, type, _p0, _p1, captures, NE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q20))
	  {
	  case ADDR_EXPR:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case POINTER_PLUS_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    tree _q51 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q50))
		      {
		      case ADDR_EXPR:
		        {
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q51 };
			    tree res = generic_simplify_207 (loc, type, _p0, _p1, captures, NE_EXPR);
			    if (res) return res;
			  }
		          break;
		        }
		      default:;
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case MINUS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q31 };
	      tree res = generic_simplify_202 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case TRUNC_DIV_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_208 (loc, type, _p0, _p1, captures, NE_EXPR, GE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_p1))
	  {
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      switch (TREE_CODE (_q40))
	        {
		case MINUS_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_q50))
		      {
		      case INTEGER_CST:
		        {
			  switch (TREE_CODE (_q51))
			    {
			    CASE_CONVERT:
			      {
				tree _q70 = TREE_OPERAND (_q51, 0);
				if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
				  {
				    {
				      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q50 };
				      tree res = generic_simplify_209 (loc, type, _p0, _p1, captures, NE_EXPR);
				      if (res) return res;
				    }
				  }
			        break;
			      }
			    default:;
			    }
		          break;
		        }
		      default:;
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q20))
	  {
	  case MINUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_q30))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_q31))
		      {
		      CASE_CONVERT:
		        {
			  tree _q50 = TREE_OPERAND (_q31, 0);
			  switch (TREE_CODE (_p1))
			    {
			    CASE_CONVERT:
			      {
				tree _q70 = TREE_OPERAND (_p1, 0);
				if ((_q70 == _q50 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q50, 0) && types_match (_q70, _q50)))
				  {
				    {
				      tree captures[2] ATTRIBUTE_UNUSED = { _q50, _q30 };
				      tree res = generic_simplify_209 (loc, type, _p0, _p1, captures, NE_EXPR);
				      if (res) return res;
				    }
				  }
			        break;
			      }
			    default:;
			    }
			  if ((_p1 == _q50 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q50, 0) && types_match (_p1, _q50)))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q50, _q30 };
				tree res = generic_simplify_209 (loc, type, _p0, _p1, captures, NE_EXPR);
				if (res) return res;
			      }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_p1))
		      {
		      CASE_CONVERT:
		        {
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  if ((_q60 == _q31 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q31, 0) && types_match (_q60, _q31)))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q31, _q30 };
				tree res = generic_simplify_209 (loc, type, _p0, _p1, captures, NE_EXPR);
				if (res) return res;
			      }
			    }
		          break;
		        }
		      default:;
		      }
		    if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q31, _q30 };
			  tree res = generic_simplify_209 (loc, type, _p0, _p1, captures, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_p1))
	  {
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      switch (TREE_CODE (_q40))
	        {
		case MINUS_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_q50))
		      {
		      case INTEGER_CST:
		        {
			  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q50 };
				tree res = generic_simplify_209 (loc, type, _p0, _p1, captures, NE_EXPR);
				if (res) return res;
			      }
			    }
		          break;
		        }
		      default:;
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
	  case MINUS_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      tree _q41 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q40))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_q41))
		      {
		      CASE_CONVERT:
		        {
			  tree _q60 = TREE_OPERAND (_q41, 0);
			  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
				tree res = generic_simplify_209 (loc, type, _p0, _p1, captures, NE_EXPR);
				if (res) return res;
			      }
			    }
		          break;
		        }
		      default:;
		      }
		    if ((_q41 == _q20 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _q20, 0) && types_match (_q41, _q20)))
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
			  tree res = generic_simplify_209 (loc, type, _p0, _p1, captures, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case MINUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_q21))
	        {
		CASE_CONVERT:
		  {
		    tree _q40 = TREE_OPERAND (_q21, 0);
		    switch (TREE_CODE (_p1))
		      {
		      CASE_CONVERT:
		        {
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
				tree res = generic_simplify_209 (loc, type, _p0, _p1, captures, NE_EXPR);
				if (res) return res;
			      }
			    }
		          break;
		        }
		      default:;
		      }
		    if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
			  tree res = generic_simplify_209 (loc, type, _p0, _p1, captures, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_p1))
	        {
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			  tree res = generic_simplify_209 (loc, type, _p0, _p1, captures, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
		    tree res = generic_simplify_209 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    CASE_CONVERT:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	switch (TREE_CODE (_q30))
	  {
	  case MINUS_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      switch (TREE_CODE (_q40))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_q41))
		      {
		      CASE_CONVERT:
		        {
			  tree _q60 = TREE_OPERAND (_q41, 0);
			  if ((_q60 == _p0 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p0, 0) && types_match (_q60, _p0)))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q40 };
				tree res = generic_simplify_209 (loc, type, _p0, _p1, captures, NE_EXPR);
				if (res) return res;
			      }
			    }
		          break;
		        }
		      default:;
		      }
		    if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q40 };
			  tree res = generic_simplify_209 (loc, type, _p0, _p1, captures, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case MINUS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	switch (TREE_CODE (_q30))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_q31))
	        {
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_q31, 0);
		    if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
			  tree res = generic_simplify_209 (loc, type, _p0, _p1, captures, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
		    tree res = generic_simplify_209 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case LT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case GT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = GT_EXPR;
			const enum tree_code rcmp = NE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1548;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1548:;
			  }
		      }
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = GT_EXPR;
			const enum tree_code rcmp = NE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1549;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1549:;
			  }
		      }
		    }
	        }
	      break;
	    }
	  case EQ_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = EQ_EXPR;
			const enum tree_code rcmp = LE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1550;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1550:;
			  }
		      }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = EQ_EXPR;
			const enum tree_code rcmp = LE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1551;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1551:;
			  }
		      }
		    }
	        }
	      break;
	    }
	  case NE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = NE_EXPR;
			const enum tree_code rcmp = GT_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1552;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1552:;
			  }
		      }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = NE_EXPR;
			const enum tree_code rcmp = GT_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1553;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1553:;
			  }
		      }
		    }
	        }
	      break;
	    }
	  case LT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = GT_EXPR;
			const enum tree_code rcmp = NE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1554;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1554:;
			  }
		      }
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = GT_EXPR;
			const enum tree_code rcmp = NE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1555;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1555:;
			  }
		      }
		    }
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case LE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case GE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = GE_EXPR;
			const enum tree_code rcmp = NE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1556;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1556:;
			  }
		      }
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = GE_EXPR;
			const enum tree_code rcmp = NE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1557;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1557:;
			  }
		      }
		    }
	        }
	      break;
	    }
	  case EQ_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = EQ_EXPR;
			const enum tree_code rcmp = LT_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1558;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1558:;
			  }
		      }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = EQ_EXPR;
			const enum tree_code rcmp = LT_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1559;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1559:;
			  }
		      }
		    }
	        }
	      break;
	    }
	  case NE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = NE_EXPR;
			const enum tree_code rcmp = GE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1560;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1560:;
			  }
		      }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = NE_EXPR;
			const enum tree_code rcmp = GE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1561;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1561:;
			  }
		      }
		    }
	        }
	      break;
	    }
	  case LE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = GE_EXPR;
			const enum tree_code rcmp = NE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1562;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1562:;
			  }
		      }
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = GE_EXPR;
			const enum tree_code rcmp = NE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1563;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1563:;
			  }
		      }
		    }
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case GT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case GT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = GT_EXPR;
			const enum tree_code rcmp = NE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1564;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1564:;
			  }
		      }
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = GT_EXPR;
			const enum tree_code rcmp = NE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1565;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1565:;
			  }
		      }
		    }
	        }
	      break;
	    }
	  case EQ_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = EQ_EXPR;
			const enum tree_code rcmp = LE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1566;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1566:;
			  }
		      }
		    }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = EQ_EXPR;
			const enum tree_code rcmp = LE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1567;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1567:;
			  }
		      }
		    }
	        }
	      break;
	    }
	  case NE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = NE_EXPR;
			const enum tree_code rcmp = GT_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1568;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1568:;
			  }
		      }
		    }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = NE_EXPR;
			const enum tree_code rcmp = GT_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1569;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1569:;
			  }
		      }
		    }
	        }
	      break;
	    }
	  case LT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = GT_EXPR;
			const enum tree_code rcmp = NE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1570;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1570:;
			  }
		      }
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = GT_EXPR;
			const enum tree_code rcmp = NE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1571;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1571:;
			  }
		      }
		    }
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case GE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case GE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = GE_EXPR;
			const enum tree_code rcmp = NE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1572;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1572:;
			  }
		      }
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = GE_EXPR;
			const enum tree_code rcmp = NE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1573;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1573:;
			  }
		      }
		    }
	        }
	      break;
	    }
	  case EQ_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = EQ_EXPR;
			const enum tree_code rcmp = LT_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1574;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1574:;
			  }
		      }
		    }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = EQ_EXPR;
			const enum tree_code rcmp = LT_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1575;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1575:;
			  }
		      }
		    }
	        }
	      break;
	    }
	  case NE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = NE_EXPR;
			const enum tree_code rcmp = GE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1576;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1576:;
			  }
		      }
		    }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = NE_EXPR;
			const enum tree_code rcmp = GE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1577;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1577:;
			  }
		      }
		    }
	        }
	      break;
	    }
	  case LE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = GE_EXPR;
			const enum tree_code rcmp = NE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1578;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1578:;
			  }
		      }
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = GE_EXPR;
			const enum tree_code rcmp = NE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1579;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1579:;
			  }
		      }
		    }
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case EQ_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case LT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = EQ_EXPR;
			const enum tree_code rcmp = LE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1580;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1580:;
			  }
		      }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = EQ_EXPR;
			const enum tree_code rcmp = LE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1581;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1581:;
			  }
		      }
		    }
	        }
	      break;
	    }
	  case LE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = EQ_EXPR;
			const enum tree_code rcmp = LT_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1582;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1582:;
			  }
		      }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = EQ_EXPR;
			const enum tree_code rcmp = LT_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1583;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1583:;
			  }
		      }
		    }
	        }
	      break;
	    }
	  case GT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = EQ_EXPR;
			const enum tree_code rcmp = LE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1584;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1584:;
			  }
		      }
		    }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = EQ_EXPR;
			const enum tree_code rcmp = LE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1585;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1585:;
			  }
		      }
		    }
	        }
	      break;
	    }
	  case GE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = EQ_EXPR;
			const enum tree_code rcmp = LT_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1586;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1586:;
			  }
		      }
		    }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = EQ_EXPR;
			const enum tree_code rcmp = LT_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1587;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1587:;
			  }
		      }
		    }
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case NE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case LT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = NE_EXPR;
			const enum tree_code rcmp = GT_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1588;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1588:;
			  }
		      }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = NE_EXPR;
			const enum tree_code rcmp = GT_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1589;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1589:;
			  }
		      }
		    }
	        }
	      break;
	    }
	  case LE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = NE_EXPR;
			const enum tree_code rcmp = GE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1590;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1590:;
			  }
		      }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = NE_EXPR;
			const enum tree_code rcmp = GE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1591;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1591:;
			  }
		      }
		    }
	        }
	      break;
	    }
	  case GT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = NE_EXPR;
			const enum tree_code rcmp = GT_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1592;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1592:;
			  }
		      }
		    }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LT_EXPR;
			const enum tree_code cmp2 = NE_EXPR;
			const enum tree_code rcmp = GT_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1593;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1593:;
			  }
		      }
		    }
	        }
	      break;
	    }
	  case GE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = NE_EXPR;
			const enum tree_code rcmp = GE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1594;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1594:;
			  }
		      }
		    }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			const enum tree_code cmp1 = LE_EXPR;
			const enum tree_code cmp2 = NE_EXPR;
			const enum tree_code rcmp = GE_EXPR;
			if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1595;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 915, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1595:;
			  }
		      }
		    }
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case MIN_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MIN_EXPR, NE_EXPR, GT_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MIN_EXPR, NE_EXPR, GT_EXPR);
	      if (res) return res;
	    }
	  }
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      tree res = generic_simplify_213 (loc, type, _p0, _p1, captures, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case MAX_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MAX_EXPR, NE_EXPR, LT_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MAX_EXPR, NE_EXPR, LT_EXPR);
	      if (res) return res;
	    }
	  }
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      tree res = generic_simplify_214 (loc, type, _p0, _p1, captures, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    default:;
    }
{
  tree _p0_pops[1];
  if (tree_with_possible_nonzero_bits2 (_p0, _p0_pops))
    {
      tree _q20 = _p0_pops[0];
      {
	tree _p1_pops[1];
	if (tree_with_certain_nonzero_bits2 (_p1, _p1_pops))
	  {
	    tree _q40 = _p1_pops[0];
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
	      tree res = generic_simplify_210 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
      }
    }
}
{
  tree _p0_pops[1];
  if (tree_with_certain_nonzero_bits2 (_p0, _p0_pops))
    {
      tree _q20 = _p0_pops[0];
      {
	tree _p1_pops[1];
	if (tree_with_possible_nonzero_bits2 (_p1, _p1_pops))
	  {
	    tree _q40 = _p1_pops[0];
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
	      tree res = generic_simplify_210 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
      }
    }
}
  switch (TREE_CODE (_p1))
    {
    case MIN_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MIN_EXPR, NE_EXPR, GT_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MIN_EXPR, NE_EXPR, GT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case MAX_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MAX_EXPR, NE_EXPR, LT_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MAX_EXPR, NE_EXPR, LT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case BIT_AND_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  case LSHIFT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_pow2p (_q30))
		{
		  if (integer_pow2p (_q21))
		    {
		      if (integer_zerop (_p1))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q21 };
			    tree res = generic_simplify_215 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
			    if (res) return res;
			  }
		        }
		    }
	        }
	      switch (TREE_CODE (_q31))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_q21))
		      {
		      case INTEGER_CST:
		        {
			  switch (TREE_CODE (_p1))
			    {
			    case INTEGER_CST:
			      {
				{
				  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1 };
				  tree res = generic_simplify_216 (loc, type, _p0, _p1, captures, NE_EXPR);
				  if (res) return res;
				}
			        break;
			      }
			    default:;
			    }
		          break;
		        }
		      default:;
		      }
		    break;
		  }
	        default:;
	        }
	      if (integer_onep (_q30))
		{
		  if (integer_pow2p (_q21))
		    {
		      if (integer_zerop (_p1))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q31, _q21 };
			    tree res = generic_simplify_217 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
			    if (res) return res;
			  }
		        }
		    }
	        }
	      break;
	    }
	  case RSHIFT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_pow2p (_q30))
		{
		  if (integer_pow2p (_q21))
		    {
		      if (integer_zerop (_p1))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q21 };
			    tree res = generic_simplify_218 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
			    if (res) return res;
			  }
		        }
		    }
	        }
	      switch (TREE_CODE (_q31))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_q21))
		      {
		      case INTEGER_CST:
		        {
			  switch (TREE_CODE (_p1))
			    {
			    case INTEGER_CST:
			      {
				{
				  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1 };
				  tree res = generic_simplify_219 (loc, type, _p0, _p1, captures, NE_EXPR);
				  if (res) return res;
				}
			        break;
			      }
			    default:;
			    }
		          break;
		        }
		      default:;
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
      {
	tree _q20_pops[1];
	if (tree_nop_convert (_q20, _q20_pops))
	  {
	    tree _q30 = _q20_pops[0];
	    switch (TREE_CODE (_q30))
	      {
	      case LSHIFT_EXPR:
	        {
		  tree _q40 = TREE_OPERAND (_q30, 0);
		  tree _q41 = TREE_OPERAND (_q30, 1);
		  if (integer_onep (_q40))
		    {
		      if (integer_pow2p (_q21))
			{
			  if (integer_zerop (_p1))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q41, _q21 };
				tree res = generic_simplify_217 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
				if (res) return res;
			      }
			    }
		        }
		    }
	          break;
	        }
	      default:;
	      }
	  }
      }
        break;
      }
    case RSHIFT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_onep (_q20))
	  {
	    if (integer_zerop (_p1))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		  tree res = generic_simplify_220 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
		  if (res) return res;
		}
	      }
	  }
        break;
      }
    case LSHIFT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      tree res = generic_simplify_221 (loc, type, _p0, _p1, captures, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case LROTATE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case LROTATE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
		    tree res = generic_simplify_222 (loc, type, _p0, _p1, captures, LROTATE_EXPR, RROTATE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      tree res = generic_simplify_223 (loc, type, _p0, _p1, captures, LROTATE_EXPR, RROTATE_EXPR, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_p1))
	  {
	  case INTEGER_CST:
	    {
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		tree res = generic_simplify_224 (loc, type, _p0, _p1, captures, LROTATE_EXPR, RROTATE_EXPR, NE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
        break;
      }
    case RROTATE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case RROTATE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
		    tree res = generic_simplify_222 (loc, type, _p0, _p1, captures, RROTATE_EXPR, LROTATE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      tree res = generic_simplify_223 (loc, type, _p0, _p1, captures, RROTATE_EXPR, LROTATE_EXPR, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_p1))
	  {
	  case INTEGER_CST:
	    {
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		tree res = generic_simplify_224 (loc, type, _p0, _p1, captures, RROTATE_EXPR, LROTATE_EXPR, NE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
        break;
      }
    case VEC_COND_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	tree _q22 = TREE_OPERAND (_p0, 2);
	switch (TREE_CODE (_p1))
	  {
	  case VEC_COND_EXPR:
	    {
	      tree _q60 = TREE_OPERAND (_p1, 0);
	      tree _q61 = TREE_OPERAND (_p1, 1);
	      tree _q62 = TREE_OPERAND (_p1, 2);
	      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q22, _p1, _q61, _q62 };
		    if (VECTOR_TYPE_P (type)
 && (TREE_CODE_CLASS (NE_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1596;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, NE_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1596;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, NE_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1596;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1596:;
		      }
		  }
	        }
	      break;
	    }
          default:;
          }
	{
	  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q22, _p1 };
	  if (VECTOR_TYPE_P (type)
 && (TREE_CODE_CLASS (NE_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1597;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1597;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, NE_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1597;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, NE_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1597;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1597:;
	    }
	}
        break;
      }
    case CALL_EXPR:
      switch (get_call_combined_fn (_p0))
        {
	case CFN_BUILT_IN_BSWAP128:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		      tree res = generic_simplify_227 (loc, type, _p0, _p1, captures, NE_EXPR, CFN_BUILT_IN_BSWAP128);
		      if (res) return res;
		    }
		    break;
		  }
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_BSWAP128:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q40 };
			    tree res = generic_simplify_226 (loc, type, _p0, _p1, captures, NE_EXPR, CFN_BUILT_IN_BSWAP128);
			    if (res) return res;
			  }
		        }
		      break;
		    default:;
		    }
		  break;
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_BSWAP16:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		      tree res = generic_simplify_227 (loc, type, _p0, _p1, captures, NE_EXPR, CFN_BUILT_IN_BSWAP16);
		      if (res) return res;
		    }
		    break;
		  }
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_BSWAP16:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q40 };
			    tree res = generic_simplify_226 (loc, type, _p0, _p1, captures, NE_EXPR, CFN_BUILT_IN_BSWAP16);
			    if (res) return res;
			  }
		        }
		      break;
		    default:;
		    }
		  break;
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_BSWAP32:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		      tree res = generic_simplify_227 (loc, type, _p0, _p1, captures, NE_EXPR, CFN_BUILT_IN_BSWAP32);
		      if (res) return res;
		    }
		    break;
		  }
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_BSWAP32:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q40 };
			    tree res = generic_simplify_226 (loc, type, _p0, _p1, captures, NE_EXPR, CFN_BUILT_IN_BSWAP32);
			    if (res) return res;
			  }
		        }
		      break;
		    default:;
		    }
		  break;
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_BSWAP64:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		      tree res = generic_simplify_227 (loc, type, _p0, _p1, captures, NE_EXPR, CFN_BUILT_IN_BSWAP64);
		      if (res) return res;
		    }
		    break;
		  }
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_BSWAP64:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q40 };
			    tree res = generic_simplify_226 (loc, type, _p0, _p1, captures, NE_EXPR, CFN_BUILT_IN_BSWAP64);
			    if (res) return res;
			  }
		        }
		      break;
		    default:;
		    }
		  break;
	        default:;
	        }
	    }
	  break;
	default:;
        }
      break;
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case VEC_COND_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	tree _q32 = TREE_OPERAND (_p1, 2);
	{
	  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32 };
	  if (VECTOR_TYPE_P (type)
 && (TREE_CODE_CLASS (NE_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1598;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1598;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, NE_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1598;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, NE_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1598;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1598:;
	    }
	}
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case COND_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	tree _q22 = TREE_OPERAND (_p0, 2);
	switch (TREE_CODE (_p1))
	  {
	  case COND_EXPR:
	    {
	      tree _q60 = TREE_OPERAND (_p1, 0);
	      tree _q61 = TREE_OPERAND (_p1, 1);
	      tree _q62 = TREE_OPERAND (_p1, 2);
	      if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
		{
		  if ((_q62 == _q22 && ! TREE_SIDE_EFFECTS (_q62)) || (operand_equal_p (_q62, _q22, 0) && types_match (_q62, _q22)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q22, _q60 };
			const enum tree_code eqne = NE_EXPR;
			if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
			  {
			    if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1599;
			    if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1599;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1599;
			    {
			      tree res_op0;
			      {
				tree _o1[2], _r1;
				{
				  tree _o2[2], _r2;
				  _o2[0] = captures[0];
				  _o2[1] = captures[3];
				  _r2 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				  _o1[0] = _r2;
				}
				{
				  tree _o2[2], _r2;
				  _o2[0] = captures[1];
				  _o2[1] = captures[2];
				  _r2 = fold_build2_loc (loc, NE_EXPR, type, _o2[0], _o2[1]);
				  _o1[1] = _r2;
				}
				_r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				res_op0 = _r1;
			      }
			      tree res_op1;
			      res_op1 =  constant_boolean_node (eqne == NE_EXPR, type);
			      tree res_op2;
			      res_op2 =  constant_boolean_node (eqne != NE_EXPR, type);
			      tree _r;
			      _r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 952, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1599:;
			  }
		      }
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q21, _q22, _q20 };
			const enum tree_code eqne = NE_EXPR;
			if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
			  {
			    if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1600;
			    if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1600;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1600;
			    {
			      tree res_op0;
			      {
				tree _o1[2], _r1;
				{
				  tree _o2[2], _r2;
				  _o2[0] = captures[0];
				  _o2[1] = captures[3];
				  _r2 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				  _o1[0] = _r2;
				}
				{
				  tree _o2[2], _r2;
				  _o2[0] = captures[1];
				  _o2[1] = captures[2];
				  _r2 = fold_build2_loc (loc, NE_EXPR, type, _o2[0], _o2[1]);
				  _o1[1] = _r2;
				}
				_r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				res_op0 = _r1;
			      }
			      tree res_op1;
			      res_op1 =  constant_boolean_node (eqne == NE_EXPR, type);
			      tree res_op2;
			      res_op2 =  constant_boolean_node (eqne != NE_EXPR, type);
			      tree _r;
			      _r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 952, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1600:;
			  }
		      }
		    }
	        }
	      if ((_q61 == _q22 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q22, 0) && types_match (_q61, _q22)))
		{
		  if ((_q62 == _q21 && ! TREE_SIDE_EFFECTS (_q62)) || (operand_equal_p (_q62, _q21, 0) && types_match (_q62, _q21)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q22, _q60 };
			const enum tree_code eqne = NE_EXPR;
			if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
			  {
			    if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1601;
			    if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1601;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1601;
			    {
			      tree res_op0;
			      {
				tree _o1[2], _r1;
				{
				  tree _o2[2], _r2;
				  _o2[0] = captures[0];
				  _o2[1] = captures[3];
				  _r2 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				  _o1[0] = _r2;
				}
				{
				  tree _o2[2], _r2;
				  _o2[0] = captures[1];
				  _o2[1] = captures[2];
				  _r2 = fold_build2_loc (loc, EQ_EXPR, type, _o2[0], _o2[1]);
				  _o1[1] = _r2;
				}
				_r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				res_op0 = _r1;
			      }
			      tree res_op1;
			      res_op1 =  constant_boolean_node (eqne != NE_EXPR, type);
			      tree res_op2;
			      res_op2 =  constant_boolean_node (eqne == NE_EXPR, type);
			      tree _r;
			      _r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 953, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1601:;
			  }
		      }
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q22, _q21, _q20 };
			const enum tree_code eqne = NE_EXPR;
			if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
			  {
			    if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1602;
			    if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1602;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1602;
			    {
			      tree res_op0;
			      {
				tree _o1[2], _r1;
				{
				  tree _o2[2], _r2;
				  _o2[0] = captures[0];
				  _o2[1] = captures[3];
				  _r2 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				  _o1[0] = _r2;
				}
				{
				  tree _o2[2], _r2;
				  _o2[0] = captures[1];
				  _o2[1] = captures[2];
				  _r2 = fold_build2_loc (loc, EQ_EXPR, type, _o2[0], _o2[1]);
				  _o1[1] = _r2;
				}
				_r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				res_op0 = _r1;
			      }
			      tree res_op1;
			      res_op1 =  constant_boolean_node (eqne != NE_EXPR, type);
			      tree res_op2;
			      res_op2 =  constant_boolean_node (eqne == NE_EXPR, type);
			      tree _r;
			      _r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 953, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1602:;
			  }
		      }
		    }
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case VEC_COND_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	tree _q22 = TREE_OPERAND (_p0, 2);
	switch (TREE_CODE (_p1))
	  {
	  case VEC_COND_EXPR:
	    {
	      tree _q60 = TREE_OPERAND (_p1, 0);
	      tree _q61 = TREE_OPERAND (_p1, 1);
	      tree _q62 = TREE_OPERAND (_p1, 2);
	      if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
		{
		  if ((_q62 == _q22 && ! TREE_SIDE_EFFECTS (_q62)) || (operand_equal_p (_q62, _q22, 0) && types_match (_q62, _q22)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q22, _q60 };
			const enum tree_code eqne = NE_EXPR;
			if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1603;
			    {
			      tree res_op0;
			      {
				tree _o1[2], _r1;
				{
				  tree _o2[2], _r2;
				  _o2[0] = captures[0];
				  _o2[1] = captures[3];
				  _r2 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				  _o1[0] = _r2;
				}
				{
				  tree _o2[2], _r2;
				  _o2[0] = captures[1];
				  _o2[1] = captures[2];
				  _r2 = fold_build2_loc (loc, NE_EXPR, type, _o2[0], _o2[1]);
				  _o1[1] = _r2;
				}
				_r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				res_op0 = _r1;
			      }
			      tree res_op1;
			      res_op1 =  constant_boolean_node (eqne == NE_EXPR, type);
			      tree res_op2;
			      res_op2 =  constant_boolean_node (eqne != NE_EXPR, type);
			      tree _r;
			      _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 952, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1603:;
			  }
		      }
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q21, _q22, _q20 };
			const enum tree_code eqne = NE_EXPR;
			if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1604;
			    {
			      tree res_op0;
			      {
				tree _o1[2], _r1;
				{
				  tree _o2[2], _r2;
				  _o2[0] = captures[0];
				  _o2[1] = captures[3];
				  _r2 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				  _o1[0] = _r2;
				}
				{
				  tree _o2[2], _r2;
				  _o2[0] = captures[1];
				  _o2[1] = captures[2];
				  _r2 = fold_build2_loc (loc, NE_EXPR, type, _o2[0], _o2[1]);
				  _o1[1] = _r2;
				}
				_r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				res_op0 = _r1;
			      }
			      tree res_op1;
			      res_op1 =  constant_boolean_node (eqne == NE_EXPR, type);
			      tree res_op2;
			      res_op2 =  constant_boolean_node (eqne != NE_EXPR, type);
			      tree _r;
			      _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 952, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1604:;
			  }
		      }
		    }
	        }
	      if ((_q61 == _q22 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q22, 0) && types_match (_q61, _q22)))
		{
		  if ((_q62 == _q21 && ! TREE_SIDE_EFFECTS (_q62)) || (operand_equal_p (_q62, _q21, 0) && types_match (_q62, _q21)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q22, _q60 };
			const enum tree_code eqne = NE_EXPR;
			if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1605;
			    {
			      tree res_op0;
			      {
				tree _o1[2], _r1;
				{
				  tree _o2[2], _r2;
				  _o2[0] = captures[0];
				  _o2[1] = captures[3];
				  _r2 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				  _o1[0] = _r2;
				}
				{
				  tree _o2[2], _r2;
				  _o2[0] = captures[1];
				  _o2[1] = captures[2];
				  _r2 = fold_build2_loc (loc, EQ_EXPR, type, _o2[0], _o2[1]);
				  _o1[1] = _r2;
				}
				_r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				res_op0 = _r1;
			      }
			      tree res_op1;
			      res_op1 =  constant_boolean_node (eqne != NE_EXPR, type);
			      tree res_op2;
			      res_op2 =  constant_boolean_node (eqne == NE_EXPR, type);
			      tree _r;
			      _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 953, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1605:;
			  }
		      }
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q22, _q21, _q20 };
			const enum tree_code eqne = NE_EXPR;
			if (!HONOR_NANS (captures[1])
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
 && types_match (type, TREE_TYPE (captures[0]))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1606;
			    {
			      tree res_op0;
			      {
				tree _o1[2], _r1;
				{
				  tree _o2[2], _r2;
				  _o2[0] = captures[0];
				  _o2[1] = captures[3];
				  _r2 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				  _o1[0] = _r2;
				}
				{
				  tree _o2[2], _r2;
				  _o2[0] = captures[1];
				  _o2[1] = captures[2];
				  _r2 = fold_build2_loc (loc, EQ_EXPR, type, _o2[0], _o2[1]);
				  _o1[1] = _r2;
				}
				_r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				res_op0 = _r1;
			      }
			      tree res_op1;
			      res_op1 =  constant_boolean_node (eqne != NE_EXPR, type);
			      tree res_op2;
			      res_op2 =  constant_boolean_node (eqne == NE_EXPR, type);
			      tree _r;
			      _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 953, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1606:;
			  }
		      }
		    }
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case LT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_282 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR, UNGE_EXPR);
	      if (res) return res;
	    }
	  }
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_283 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR, UNGE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case LE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_282 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR, UNGT_EXPR);
	      if (res) return res;
	    }
	  }
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_283 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR, UNGT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case EQ_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_282 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR, NE_EXPR);
	      if (res) return res;
	    }
	  }
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_283 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR, NE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case NE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_282 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR, EQ_EXPR);
	      if (res) return res;
	    }
	  }
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_283 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR, EQ_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case GE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_282 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR, UNLT_EXPR);
	      if (res) return res;
	    }
	  }
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_283 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR, UNLT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case GT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_282 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR, UNLE_EXPR);
	      if (res) return res;
	    }
	  }
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_283 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR, UNLE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case UNORDERED_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_282 (loc, type, _p0, _p1, captures, UNORDERED_EXPR, ORDERED_EXPR, ORDERED_EXPR);
	      if (res) return res;
	    }
	  }
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_283 (loc, type, _p0, _p1, captures, UNORDERED_EXPR, ORDERED_EXPR, ORDERED_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case ORDERED_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_282 (loc, type, _p0, _p1, captures, ORDERED_EXPR, UNORDERED_EXPR, UNORDERED_EXPR);
	      if (res) return res;
	    }
	  }
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_283 (loc, type, _p0, _p1, captures, ORDERED_EXPR, UNORDERED_EXPR, UNORDERED_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case UNLT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_282 (loc, type, _p0, _p1, captures, UNLT_EXPR, GE_EXPR, GE_EXPR);
	      if (res) return res;
	    }
	  }
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_283 (loc, type, _p0, _p1, captures, UNLT_EXPR, GE_EXPR, GE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case UNLE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_282 (loc, type, _p0, _p1, captures, UNLE_EXPR, GT_EXPR, GT_EXPR);
	      if (res) return res;
	    }
	  }
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_283 (loc, type, _p0, _p1, captures, UNLE_EXPR, GT_EXPR, GT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case UNGT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_282 (loc, type, _p0, _p1, captures, UNGT_EXPR, LE_EXPR, LE_EXPR);
	      if (res) return res;
	    }
	  }
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_283 (loc, type, _p0, _p1, captures, UNGT_EXPR, LE_EXPR, LE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case UNGE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_282 (loc, type, _p0, _p1, captures, UNGE_EXPR, LT_EXPR, LT_EXPR);
	      if (res) return res;
	    }
	  }
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_283 (loc, type, _p0, _p1, captures, UNGE_EXPR, LT_EXPR, LT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case UNEQ_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_282 (loc, type, _p0, _p1, captures, UNEQ_EXPR, LTGT_EXPR, LTGT_EXPR);
	      if (res) return res;
	    }
	  }
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_283 (loc, type, _p0, _p1, captures, UNEQ_EXPR, LTGT_EXPR, LTGT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case LTGT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_282 (loc, type, _p0, _p1, captures, LTGT_EXPR, UNEQ_EXPR, UNEQ_EXPR);
	      if (res) return res;
	    }
	  }
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_283 (loc, type, _p0, _p1, captures, LTGT_EXPR, UNEQ_EXPR, UNEQ_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case MINUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_230 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case POINTER_DIFF_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_231 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case MULT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      tree res = generic_simplify_233 (loc, type, _p0, _p1, captures, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    if (integer_zerop (_p1))
	      {
		{
		  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
		  tree res = generic_simplify_232 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		  if (res) return res;
		}
	      }
	      break;
	    }
          default:;
          }
        break;
      }
    default:;
    }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_284 (loc, type, _p0, _p1, captures, NE_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case BIT_NOT_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	      tree res = generic_simplify_235 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case BIT_NOT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	      tree res = generic_simplify_235 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
	switch (TREE_CODE (_p1))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      {
		tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _p1, _q40 };
		tree res = generic_simplify_236 (loc, type, _p0, _p1, captures, NE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
      {
	tree _p1_pops[1];
	if (tree_nop_convert (_p1, _p1_pops))
	  {
	    tree _q40 = _p1_pops[0];
	    switch (TREE_CODE (_q40))
	      {
	      case BIT_NOT_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_q40, 0);
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q40, _q50 };
		    tree res = generic_simplify_236 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	          break;
	        }
	      default:;
	      }
	  }
      }
      if (CONSTANT_CLASS_P (_p1))
	{
	  {
	    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
	    tree res = generic_simplify_237 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
	    if (res) return res;
	  }
        }
        break;
      }
    default:;
    }
{
  tree _p0_pops[1];
  if (tree_nop_convert (_p0, _p0_pops))
    {
      tree _q20 = _p0_pops[0];
      switch (TREE_CODE (_q20))
        {
	case BIT_NOT_EXPR:
	  {
	    tree _q30 = TREE_OPERAND (_q20, 0);
	    switch (TREE_CODE (_p1))
	      {
	      case BIT_NOT_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _p1, _q50 };
		    tree res = generic_simplify_238 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	          break;
	        }
	      default:;
	      }
	  {
	    tree _p1_pops[1];
	    if (tree_nop_convert (_p1, _p1_pops))
	      {
		tree _q50 = _p1_pops[0];
		switch (TREE_CODE (_q50))
		  {
		  case BIT_NOT_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_q50, 0);
		      {
			tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q50, _q60 };
			tree res = generic_simplify_238 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
		      break;
		    }
	          default:;
	          }
	      }
	  }
	  if (CONSTANT_CLASS_P (_p1))
	    {
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _p1 };
		tree res = generic_simplify_237 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		if (res) return res;
	      }
	    }
	    break;
	  }
        default:;
        }
    }
}
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  tree res = generic_simplify_239 (loc, type, _p0, _p1, captures, NE_EXPR);
	  if (res) return res;
	}
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case PLUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case REAL_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case REAL_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      tree res = generic_simplify_240 (loc, type, _p0, _p1, captures, PLUS_EXPR, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case MINUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case REAL_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case REAL_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      tree res = generic_simplify_240 (loc, type, _p0, _p1, captures, MINUS_EXPR, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q20))
	  {
	  case REAL_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case REAL_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      tree res = generic_simplify_241 (loc, type, _p0, _p1, captures, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case FLOAT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_p1))
	  {
	  case FLOAT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q40 };
		tree res = generic_simplify_244 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case REAL_CST:
	    {
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		tree res = generic_simplify_245 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
        break;
      }
    case EXACT_DIV_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case INTEGER_CST:
	    {
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		tree res = generic_simplify_246 (loc, type, _p0, _p1, captures, NE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
        break;
      }
    case NEGATE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_p1))
	  {
	  case NEGATE_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
		tree res = generic_simplify_247 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
      if (CONSTANT_CLASS_P (_p1))
	{
	  {
	    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
	    tree res = generic_simplify_248 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
	    if (res) return res;
	  }
        }
        break;
      }
    case ABS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	if (zerop (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
	      const enum tree_code eqne = NE_EXPR;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1607;
	      {
		tree res_op0;
		res_op0 = captures[0];
		tree res_op1;
		res_op1 =  build_zero_cst (TREE_TYPE (captures[0]));
		tree _r;
		_r = fold_build2_loc (loc, eqne, type, res_op0, res_op1);
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 954, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1607:;
	    }
	  }
        break;
      }
    case ABSU_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	if (zerop (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
	      const enum tree_code eqne = NE_EXPR;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1608;
	      {
		tree res_op0;
		res_op0 = captures[0];
		tree res_op1;
		res_op1 =  build_zero_cst (TREE_TYPE (captures[0]));
		tree _r;
		_r = fold_build2_loc (loc, eqne, type, res_op0, res_op1);
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 954, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1608:;
	    }
	  }
        break;
      }
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_p1))
	  {
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
		tree res = generic_simplify_249 (loc, type, _p0, _p1, captures, NE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _p1 };
	  tree res = generic_simplify_250 (loc, type, _p0, _p1, captures, NE_EXPR);
	  if (res) return res;
	}
	switch (TREE_CODE (_q20))
	  {
	  case BIT_IOR_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_q31))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q30, _q31, _p1 };
			    tree res = generic_simplify_251 (loc, type, _p0, _p1, captures, NE_EXPR);
			    if (res) return res;
			  }
		          break;
		        }
		      default:;
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case ADDR_EXPR:
      {
	switch (TREE_CODE (_p1))
	  {
	  case SSA_NAME:
	    {
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		tree res = generic_simplify_252 (loc, type, _p0, _p1, captures, NE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
        break;
      }
    case BIT_IOR_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
		      tree res = generic_simplify_251 (loc, type, _p0, _p1, captures, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case BIT_XOR_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_253 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_254 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_254 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case CALL_EXPR:
      switch (get_call_combined_fn (_p0))
        {
	case CFN_BUILT_IN_SQRTF:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case REAL_CST:
		  {
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		      tree res = generic_simplify_242 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTF, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_SQRTF:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
			    tree res = generic_simplify_243 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTF, NE_EXPR);
			    if (res) return res;
			  }
		        }
		      break;
		    default:;
		    }
		  break;
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_SQRTL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case REAL_CST:
		  {
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		      tree res = generic_simplify_242 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTL, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_SQRTL:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
			    tree res = generic_simplify_243 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTL, NE_EXPR);
			    if (res) return res;
			  }
		        }
		      break;
		    default:;
		    }
		  break;
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_SQRT:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case REAL_CST:
		  {
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		      tree res = generic_simplify_242 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRT, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_SQRT:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
			    tree res = generic_simplify_243 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRT, NE_EXPR);
			    if (res) return res;
			  }
		        }
		      break;
		    default:;
		    }
		  break;
	        default:;
	        }
	    }
	  break;
	case CFN_SQRT:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case REAL_CST:
		  {
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		      tree res = generic_simplify_242 (loc, type, _p0, _p1, captures, CFN_SQRT, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_SQRT:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
			    tree res = generic_simplify_243 (loc, type, _p0, _p1, captures, CFN_SQRT, NE_EXPR);
			    if (res) return res;
			  }
		        }
		      break;
		    default:;
		    }
		  break;
	        default:;
	        }
	    }
	  break;
	default:;
        }
      break;
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case BIT_XOR_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
	      tree res = generic_simplify_254 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	      tree res = generic_simplify_254 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case BIT_AND_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_255 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_255 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case BIT_AND_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
	      tree res = generic_simplify_255 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	      tree res = generic_simplify_255 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case BIT_AND_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_q30))
	        {
		CASE_CONVERT:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    switch (TREE_CODE (_q31))
		      {
		      case INTEGER_CST:
		        {
			  switch (TREE_CODE (_p1))
			    {
			    CASE_CONVERT:
			      {
				tree _q70 = TREE_OPERAND (_p1, 0);
				if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q31 };
				      tree res = generic_simplify_256 (loc, type, _p0, _p1, captures, NE_EXPR);
				      if (res) return res;
				    }
				  }
			        break;
			      }
			    default:;
			    }
		          break;
		        }
		      default:;
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_p1))
	  {
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      switch (TREE_CODE (_q40))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_q50))
		      {
		      CASE_CONVERT:
		        {
			  tree _q60 = TREE_OPERAND (_q50, 0);
			  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
			    {
			      switch (TREE_CODE (_q51))
			        {
				case INTEGER_CST:
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q50, _q20, _q51 };
				      tree res = generic_simplify_256 (loc, type, _p0, _p1, captures, NE_EXPR);
				      if (res) return res;
				    }
				    break;
				  }
			        default:;
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case BIT_IOR_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_257 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_257 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case BIT_IOR_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _p0 };
	      tree res = generic_simplify_257 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q31, _p0 };
	      tree res = generic_simplify_257 (loc, type, _p0, _p1, captures, NE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case BIT_XOR_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_q31))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q30, _q31, _p1 };
			    tree res = generic_simplify_258 (loc, type, _p0, _p1, captures, NE_EXPR);
			    if (res) return res;
			  }
		          break;
		        }
		      default:;
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case BIT_XOR_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
		      tree res = generic_simplify_258 (loc, type, _p0, _p1, captures, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    default:;
    }
{
  tree _p0_pops[1];
  if (tree_nop_convert (_p0, _p0_pops))
    {
      tree _q20 = _p0_pops[0];
      if (integer_zerop (_p1))
	{
	  {
	    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	    tree res = generic_simplify_259 (loc, type, _p0, _p1, captures, NE_EXPR);
	    if (res) return res;
	  }
        }
    }
}
  if (integer_zerop (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_259 (loc, type, _p0, _p1, captures, NE_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p0))
    {
    case BIT_AND_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case BIT_AND_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_260 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_260 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_260 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_260 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	if (integer_pow2p (_q21))
	  {
	    if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	      {
		{
		  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		  tree res = generic_simplify_261 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
		  if (res) return res;
		}
	      }
	  }
	switch (TREE_CODE (_q20))
	  {
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      if (integer_pow2p (_q21))
		{
		  if (integer_zerop (_p1))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q21 };
			tree res = generic_simplify_262 (loc, type, _p0, _p1, captures, NE_EXPR, LT_EXPR);
			if (res) return res;
		      }
		    }
	        }
	      break;
	    }
          default:;
          }
	if (integer_pow2p (_q21))
	  {
	    if (integer_zerop (_p1))
	      {
		{
		  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q20, _q21 };
		  tree res = generic_simplify_263 (loc, type, _p0, _p1, captures, NE_EXPR, LT_EXPR);
		  if (res) return res;
		}
	      }
	  }
        break;
      }
    case GE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_q21))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case GE_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if (integer_zerop (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
			tree res = generic_simplify_264 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR, LT_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      case LT_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if (integer_zerop (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q51, _q20 };
			tree res = generic_simplify_265 (loc, type, _p0, _p1, captures, NE_EXPR, GE_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	  }
        break;
      }
    case LT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_q21))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case LT_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if (integer_zerop (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
			tree res = generic_simplify_264 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR, LT_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      case GE_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if (integer_zerop (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
			tree res = generic_simplify_266 (loc, type, _p0, _p1, captures, NE_EXPR, GE_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	  }
        break;
      }
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case ADDR_EXPR:
	    {
	      switch (TREE_CODE (_p1))
	        {
		CASE_CONVERT:
		  {
		    tree _q40 = TREE_OPERAND (_p1, 0);
		    switch (TREE_CODE (_q40))
		      {
		      case ADDR_EXPR:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q40 };
			    tree res = generic_simplify_267 (loc, type, _p0, _p1, captures, NE_EXPR);
			    if (res) return res;
			  }
		          break;
		        }
		      default:;
		      }
		    break;
		  }
		case ADDR_EXPR:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_267 (loc, type, _p0, _p1, captures, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case ADDR_EXPR:
      {
	switch (TREE_CODE (_p1))
	  {
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_p1, 0);
	      switch (TREE_CODE (_q30))
	        {
		case ADDR_EXPR:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _q30 };
		      tree res = generic_simplify_268 (loc, type, _p0, _p1, captures, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
	  case ADDR_EXPR:
	    {
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _p1 };
		tree res = generic_simplify_268 (loc, type, _p0, _p1, captures, NE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
        break;
      }
    default:;
    }
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    tree res = generic_simplify_269 (loc, type, _p0, _p1, captures, NE_EXPR);
    if (res) return res;
  }
  switch (TREE_CODE (_p0))
    {
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_p1))
	  {
	  case INTEGER_CST:
	    {
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		tree res = generic_simplify_270 (loc, type, _p0, _p1, captures, NE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q20))
	  {
	  case NE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_q31))
	        {
		case INTEGER_CST:
		  {
		    if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
			  tree res = generic_simplify_271 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
	  case EQ_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_q31))
	        {
		case INTEGER_CST:
		  {
		    if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
			  tree res = generic_simplify_271 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    CASE_CONVERT:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	switch (TREE_CODE (_q30))
	  {
	  case NE_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  switch (TREE_CODE (_q41))
		    {
		    case INTEGER_CST:
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q41 };
			  tree res = generic_simplify_271 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		        break;
		      }
		    default:;
		    }
	        }
	      break;
	    }
	  case EQ_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  switch (TREE_CODE (_q41))
		    {
		    case INTEGER_CST:
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q41 };
			  tree res = generic_simplify_271 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
			  if (res) return res;
			}
		        break;
		      }
		    default:;
		    }
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    default:;
    }
if (integer_zerop (_p1))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (TREE_CODE (TREE_TYPE (captures[0])) == BOOLEAN_TYPE
 && types_match (type, TREE_TYPE (captures[0]))
)
	{
	  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1609;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1609;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree _r;
	    _r = non_lvalue_loc (loc, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 959, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1609:;
	}
    }
  }
  switch (TREE_CODE (_p0))
    {
    case BIT_AND_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case VECTOR_CST:
	    {
	      if (integer_zerop (_p1))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		    const enum tree_code cmp = NE_EXPR;
		    const enum tree_code icmp = GT_EXPR;
		    {
 tree csts = bitmask_inv_cst_vector_p (captures[2]);
			if (csts && (VECTOR_TYPE_P (TREE_TYPE (captures[2])) || single_use (captures[0]))
)
			  {
			    {
 auto optab = VECTOR_TYPE_P (TREE_TYPE (captures[2]))
 ? optab_vector : optab_default;
 tree utype = unsigned_type_for (TREE_TYPE (captures[2]));
				if (target_supports_op_p (utype, icmp, optab)
 || (optimize_vectors_before_lowering_p ()
 && (!target_supports_op_p (type, cmp, optab)
 || !target_supports_op_p (type, BIT_AND_EXPR, optab)))
)
				  {
				    if (TYPE_UNSIGNED (TREE_TYPE (captures[2]))
)
				      {
					if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1610;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1610;
					{
					  tree res_op0;
					  res_op0 = captures[1];
					  tree res_op1;
					  res_op1 =  csts;
					  tree _r;
					  _r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
					  if (TREE_SIDE_EFFECTS (captures[2]))
					    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 956, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1610:;
				      }
				    else
				      {
					if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1611;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1611;
					{
					  tree res_op0;
					  {
					    tree _o1[1], _r1;
					    _o1[0] = captures[1];
					    _r1 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, utype, _o1[0]);
					    res_op0 = _r1;
					  }
					  tree res_op1;
					  res_op1 =  csts;
					  tree _r;
					  _r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
					  if (TREE_SIDE_EFFECTS (captures[2]))
					    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 957, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1611:;
				      }
				  }
			    }
			  }
		    }
		  }
	        }
	      break;
	    }
	  case INTEGER_CST:
	    {
	      if (integer_zerop (_p1))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		    const enum tree_code cmp = NE_EXPR;
		    const enum tree_code icmp = GT_EXPR;
		    {
 tree csts = bitmask_inv_cst_vector_p (captures[2]);
			if (csts && (VECTOR_TYPE_P (TREE_TYPE (captures[2])) || single_use (captures[0]))
)
			  {
			    {
 auto optab = VECTOR_TYPE_P (TREE_TYPE (captures[2]))
 ? optab_vector : optab_default;
 tree utype = unsigned_type_for (TREE_TYPE (captures[2]));
				if (target_supports_op_p (utype, icmp, optab)
 || (optimize_vectors_before_lowering_p ()
 && (!target_supports_op_p (type, cmp, optab)
 || !target_supports_op_p (type, BIT_AND_EXPR, optab)))
)
				  {
				    if (TYPE_UNSIGNED (TREE_TYPE (captures[2]))
)
				      {
					if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1612;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1612;
					{
					  tree res_op0;
					  res_op0 = captures[1];
					  tree res_op1;
					  res_op1 =  csts;
					  tree _r;
					  _r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
					  if (TREE_SIDE_EFFECTS (captures[2]))
					    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 956, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1612:;
				      }
				    else
				      {
					if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1613;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1613;
					{
					  tree res_op0;
					  {
					    tree _o1[1], _r1;
					    _o1[0] = captures[1];
					    _r1 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, utype, _o1[0]);
					    res_op0 = _r1;
					  }
					  tree res_op1;
					  res_op1 =  csts;
					  tree _r;
					  _r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
					  if (TREE_SIDE_EFFECTS (captures[2]))
					    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 957, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1613:;
				      }
				  }
			    }
			  }
		    }
		  }
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q20))
	  {
	  case VECTOR_CST:
	    {
	      if (integer_zerop (_p1))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
		    const enum tree_code cmp = NE_EXPR;
		    const enum tree_code icmp = GT_EXPR;
		    {
 tree csts = bitmask_inv_cst_vector_p (captures[2]);
			if (csts && (VECTOR_TYPE_P (TREE_TYPE (captures[2])) || single_use (captures[0]))
)
			  {
			    {
 auto optab = VECTOR_TYPE_P (TREE_TYPE (captures[2]))
 ? optab_vector : optab_default;
 tree utype = unsigned_type_for (TREE_TYPE (captures[2]));
				if (target_supports_op_p (utype, icmp, optab)
 || (optimize_vectors_before_lowering_p ()
 && (!target_supports_op_p (type, cmp, optab)
 || !target_supports_op_p (type, BIT_AND_EXPR, optab)))
)
				  {
				    if (TYPE_UNSIGNED (TREE_TYPE (captures[2]))
)
				      {
					if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1614;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1614;
					{
					  tree res_op0;
					  res_op0 = captures[1];
					  tree res_op1;
					  res_op1 =  csts;
					  tree _r;
					  _r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
					  if (TREE_SIDE_EFFECTS (captures[2]))
					    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 956, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1614:;
				      }
				    else
				      {
					if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1615;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1615;
					{
					  tree res_op0;
					  {
					    tree _o1[1], _r1;
					    _o1[0] = captures[1];
					    _r1 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, utype, _o1[0]);
					    res_op0 = _r1;
					  }
					  tree res_op1;
					  res_op1 =  csts;
					  tree _r;
					  _r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
					  if (TREE_SIDE_EFFECTS (captures[2]))
					    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 957, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1615:;
				      }
				  }
			    }
			  }
		    }
		  }
	        }
	      break;
	    }
	  case INTEGER_CST:
	    {
	      if (integer_zerop (_p1))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
		    const enum tree_code cmp = NE_EXPR;
		    const enum tree_code icmp = GT_EXPR;
		    {
 tree csts = bitmask_inv_cst_vector_p (captures[2]);
			if (csts && (VECTOR_TYPE_P (TREE_TYPE (captures[2])) || single_use (captures[0]))
)
			  {
			    {
 auto optab = VECTOR_TYPE_P (TREE_TYPE (captures[2]))
 ? optab_vector : optab_default;
 tree utype = unsigned_type_for (TREE_TYPE (captures[2]));
				if (target_supports_op_p (utype, icmp, optab)
 || (optimize_vectors_before_lowering_p ()
 && (!target_supports_op_p (type, cmp, optab)
 || !target_supports_op_p (type, BIT_AND_EXPR, optab)))
)
				  {
				    if (TYPE_UNSIGNED (TREE_TYPE (captures[2]))
)
				      {
					if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1616;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1616;
					{
					  tree res_op0;
					  res_op0 = captures[1];
					  tree res_op1;
					  res_op1 =  csts;
					  tree _r;
					  _r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
					  if (TREE_SIDE_EFFECTS (captures[2]))
					    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 956, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1616:;
				      }
				    else
				      {
					if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1617;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1617;
					{
					  tree res_op0;
					  {
					    tree _o1[1], _r1;
					    _o1[0] = captures[1];
					    _r1 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, utype, _o1[0]);
					    res_op0 = _r1;
					  }
					  tree res_op1;
					  res_op1 =  csts;
					  tree _r;
					  _r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
					  if (TREE_SIDE_EFFECTS (captures[2]))
					    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 957, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1617:;
				      }
				  }
			    }
			  }
		    }
		  }
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case RSHIFT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  case MULT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_q30))
	        {
		CASE_CONVERT:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    switch (TREE_CODE (_q31))
		      {
		      CASE_CONVERT:
		        {
			  tree _q60 = TREE_OPERAND (_q31, 0);
			  switch (TREE_CODE (_q21))
			    {
			    case INTEGER_CST:
			      {
				if (integer_zerop (_p1))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q60, _q21 };
				      tree res = generic_simplify_272 (loc, type, _p0, _p1, captures, NE_EXPR);
				      if (res) return res;
				    }
				  }
			        break;
			      }
			    default:;
			    }
		          break;
		        }
		      default:;
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case PLUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
		      tree res = generic_simplify_273 (loc, type, _p0, _p1, captures, PLUS_EXPR, MINUS_EXPR, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case MINUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
		      tree res = generic_simplify_273 (loc, type, _p0, _p1, captures, MINUS_EXPR, PLUS_EXPR, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q20))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      tree res = generic_simplify_274 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case CALL_EXPR:
      switch (get_call_combined_fn (_p0))
        {
	case CFN_BUILT_IN_CLZ:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_275 (loc, type, _p0, _p1, captures, NE_EXPR, GE_EXPR, CFN_BUILT_IN_CLZ);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_CTZ:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_277 (loc, type, _p0, _p1, captures, NE_EXPR, CFN_BUILT_IN_CTZ);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_FFS:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_280 (loc, type, _p0, _p1, captures, NE_EXPR, CFN_BUILT_IN_FFS);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_REDUC_IOR:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      if (integer_zerop (_p1))
		{
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    tree res = generic_simplify_281 (loc, type, _p0, _p1, captures, NE_EXPR);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_FFSIMAX:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_280 (loc, type, _p0, _p1, captures, NE_EXPR, CFN_BUILT_IN_FFSIMAX);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_POPCOUNT:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      if (integer_zerop (_p1))
		{
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    tree res = generic_simplify_279 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR, CFN_BUILT_IN_POPCOUNT);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_CLZL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_275 (loc, type, _p0, _p1, captures, NE_EXPR, GE_EXPR, CFN_BUILT_IN_CLZL);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_CTZL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_277 (loc, type, _p0, _p1, captures, NE_EXPR, CFN_BUILT_IN_CTZL);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_FFSL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_280 (loc, type, _p0, _p1, captures, NE_EXPR, CFN_BUILT_IN_FFSL);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_POPCOUNTLL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      if (integer_zerop (_p1))
		{
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    tree res = generic_simplify_279 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR, CFN_BUILT_IN_POPCOUNTLL);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_CTZIMAX:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_277 (loc, type, _p0, _p1, captures, NE_EXPR, CFN_BUILT_IN_CTZIMAX);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_POPCOUNT:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      if (integer_zerop (_p1))
		{
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    tree res = generic_simplify_279 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR, CFN_POPCOUNT);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_CLZ:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_275 (loc, type, _p0, _p1, captures, NE_EXPR, GE_EXPR, CFN_CLZ);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
		      tree res = generic_simplify_276 (loc, type, _p0, _p1, captures, NE_EXPR, GE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_CTZ:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_277 (loc, type, _p0, _p1, captures, NE_EXPR, CFN_CTZ);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
		      tree res = generic_simplify_278 (loc, type, _p0, _p1, captures, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_FFS:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_280 (loc, type, _p0, _p1, captures, NE_EXPR, CFN_FFS);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_POPCOUNTIMAX:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      if (integer_zerop (_p1))
		{
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    tree res = generic_simplify_279 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR, CFN_BUILT_IN_POPCOUNTIMAX);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_CLZIMAX:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_275 (loc, type, _p0, _p1, captures, NE_EXPR, GE_EXPR, CFN_BUILT_IN_CLZIMAX);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_POPCOUNTL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      if (integer_zerop (_p1))
		{
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    tree res = generic_simplify_279 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR, CFN_BUILT_IN_POPCOUNTL);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_CLZLL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_275 (loc, type, _p0, _p1, captures, NE_EXPR, GE_EXPR, CFN_BUILT_IN_CLZLL);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_CTZLL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_277 (loc, type, _p0, _p1, captures, NE_EXPR, CFN_BUILT_IN_CTZLL);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_FFSLL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_280 (loc, type, _p0, _p1, captures, NE_EXPR, CFN_BUILT_IN_FFSLL);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	default:;
        }
      break;
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify (location_t, enum tree_code,
                  const tree, tree, tree, tree, tree)
{
  return NULL_TREE;
}

tree
generic_simplify (location_t, enum tree_code,
                  const tree, tree, tree, tree, tree, tree, tree)
{
  return NULL_TREE;
}
void
generic_dump_logs (const char *file1, int line1_id, const char *file2, int line2, bool simplify)
{
  static int dbg_line_numbers[1077] = {
	160, 163, 172, 174, 177, 180, 182, 188, 196, 194, 198, 200, 919, 921, 2033, 2039, 2041, 2044, 2049, 2052, 
	2265, 2269, 2271, 2273, 2275, 2277, 2279, 2281, 2308, 2324, 2332, 2314, 2871, 2873, 2878, 2876, 2883, 2885, 3065, 3075, 
	3087, 3094, 3101, 3108, 3115, 3122, 3135, 3141, 3146, 3151, 3156, 3162, 3194, 3168, 3173, 3178, 3205, 3219, 3233, 3261, 
	3247, 3274, 3281, 3315, 3322, 3331, 3288, 3295, 3308, 3301, 3350, 3368, 3382, 3396, 3409, 3422, 3464, 3449, 3482, 5521, 
	8645, 8666, 10760, 10763, 10768, 10983, 10986, 11007, 11012, 8478, 253, 280, 281, 475, 1277, 1413, 1733, 1783, 1788, 1892, 
	1851, 3989, 3927, 2387, 2419, 4032, 4033, 4037, 4038, 4048, 4052, 4056, 4071, 4074, 4078, 4004, 4007, 4012, 4159, 4165, 
	4171, 4353, 4354, 4380, 4399, 4418, 4740, 5438, 6671, 8159, 8219, 9689, 9737, 9751, 10787, 11100, 903, 909, 1393, 1402, 
	1869, 1860, 1807, 2536, 3940, 3980, 3985, 3986, 3994, 3995, 3998, 4015, 4090, 4093, 4097, 4179, 4201, 4239, 4250, 4289, 
	4304, 4746, 6681, 8224, 9747, 10791, 1283, 1287, 1379, 1388, 1418, 1423, 1465, 1450, 1457, 1491, 1946, 1496, 1503, 1523, 
	1626, 1650, 1740, 1745, 1755, 1765, 1879, 1938, 1966, 1976, 1978, 2179, 2198, 2206, 2211, 2216, 2232, 2222, 2223, 4475, 
	4488, 4498, 2246, 2257, 2260, 2292, 2370, 2399, 2401, 2406, 2408, 2529, 2934, 2936, 3548, 3563, 3581, 3764, 3765, 3766, 
	3771, 3776, 3782, 3788, 3820, 3821, 3826, 3827, 3832, 3837, 3841, 3890, 3904, 3553, 3572, 3574, 3558, 4512, 4524, 7325, 
	9042, 1428, 1433, 1528, 1533, 1750, 1840, 1760, 1904, 1770, 1775, 1898, 1950, 1911, 1917, 2510, 1924, 1984, 4752, 6740, 
	6742, 6791, 6799, 9834, 9840, 443, 449, 481, 1222, 1242, 1247, 2025, 2026, 2063, 2545, 5431, 5465, 5467, 5516, 8152, 
	8513, 8507, 8552, 8557, 311, 866, 2297, 2524, 2563, 2573, 2612, 2618, 2621, 2626, 2791, 2642, 2644, 2645, 2648, 2650, 
	2651, 2653, 2655, 2657, 2659, 2661, 2663, 2677, 2678, 2712, 2797, 2822, 2738, 2765, 2805, 2810, 2835, 2842, 2893, 3862, 
	4692, 4700, 4703, 4709, 4712, 4978, 4979, 5019, 5023, 5152, 4986, 4987, 5037, 5040, 5041, 5046, 5049, 5052, 5053, 4993, 
	5005, 5008, 5197, 5201, 5206, 5449, 5581, 5585, 6751, 6767, 6769, 6781, 6811, 6817, 6818, 6833, 6834, 6838, 6856, 6861, 
	6890, 6898, 6908, 6917, 6921, 6927, 6934, 6950, 6954, 6958, 6965, 6966, 6971, 6972, 6979, 6980, 6982, 6983, 6999, 7010, 
	7022, 7026, 7028, 7033, 7036, 7039, 7047, 7109, 7110, 7114, 7115, 7122, 7126, 7130, 7133, 7139, 7140, 7144, 7147, 7154, 
	7174, 7177, 7180, 7183, 7232, 7235, 7239, 7246, 7252, 7260, 7269, 7270, 7368, 7378, 7417, 7423, 7439, 7452, 7454, 7456, 
	7482, 7524, 7503, 7504, 7511, 7513, 7529, 7535, 7540, 7549, 7555, 7561, 7566, 7571, 7603, 7680, 7585, 7595, 7718, 7720, 
	7722, 7724, 7726, 7728, 7732, 7734, 7757, 7780, 7892, 7899, 7900, 7908, 7909, 8072, 9086, 9088, 9176, 9178, 9179, 9185, 
	9201, 9203, 9205, 9223, 9224, 9493, 9496, 9510, 9519, 9570, 9571, 9625, 9627, 9697, 10106, 10108, 10110, 10542, 6747, 6758, 
	6760, 6868, 455, 1195, 1216, 8463, 8468, 8473, 514, 515, 526, 531, 538, 547, 555, 564, 610, 613, 801, 970, 
	1001, 575, 831, 835, 840, 846, 850, 858, 946, 726, 736, 738, 740, 8146, 8518, 8540, 8523, 8528, 8533, 8547, 
	8568, 8573, 788, 791, 1601, 2586, 2589, 2686, 2698, 2780, 2724, 2751, 2863, 2866, 4730, 4722, 6705, 8002, 7282, 7285, 
	7311, 7312, 7805, 7807, 7809, 7811, 7815, 7817, 7819, 7821, 7825, 7830, 7837, 7842, 7862, 7864, 7866, 7868, 8015, 8023, 
	8033, 8041, 8051, 8173, 8175, 9100, 9119, 9159, 9555, 9557, 9558, 9606, 9609, 9611, 2602, 6695, 7355, 10127, 10129, 10131, 
	10133, 10137, 892, 928, 1267, 4858, 4902, 4905, 4913, 4953, 4955, 4956, 4957, 5164, 4883, 4892, 5635, 5638, 5639, 5647, 
	2190, 3873, 3878, 4638, 8925, 8927, 8958, 8993, 11044, 1159, 8411, 8432, 1168, 8458, 8872, 1182, 1186, 8453, 8623, 1361, 
	1445, 1477, 1438, 1470, 1485, 1518, 1559, 1570, 1581, 1593, 2431, 1822, 1830, 1835, 1958, 2287, 2551, 3061, 5123, 5141, 
	5142, 5606, 5612, 5660, 3518, 3520, 3530, 3532, 3637, 3638, 3639, 3644, 3649, 3655, 3661, 3693, 3694, 3699, 3700, 3705, 
	3710, 3714, 3538, 3543, 3588, 6883, 7328, 9028, 9030, 9732, 1508, 1513, 2489, 1681, 2494, 5175, 2469, 2500, 2504, 5189, 
	4681, 6732, 6734, 2070, 2108, 2131, 2132, 2474, 3965, 9440, 4531, 4535, 4545, 4563, 4621, 4623, 4625, 4627, 4676, 9050, 
	4549, 4556, 4541, 4648, 4657, 4660, 6146, 6148, 6150, 6250, 6253, 6399, 6401, 6415, 6419, 6477, 4770, 4789, 4796, 4802, 
	4808, 4814, 6483, 4782, 4825, 4838, 6360, 6364, 6372, 6615, 9059, 9060, 6625, 9894, 9902, 7633, 7634, 7648, 7649, 4845, 
	8101, 8104, 8123, 5557, 5569, 5572, 5576, 10336, 10342, 10350, 10356, 10365, 10371, 10379, 10385, 10431, 10439, 7924, 6876, 6872, 
	8082, 8089, 8209, 8214, 8302, 8374, 8391, 8423, 8443, 8448, 8686, 8663, 8642, 8484, 8488, 8766, 8776, 8783, 8792, 8810, 
	8817, 8798, 9065, 9071, 9430, 9436, 9433, 9444, 9450, 9458, 9464, 9472, 9478, 9528, 9712, 9727, 9993, 9996, 9801, 9815, 
	9828, 10512, 10526, 10617, 10618, 218, 1990, 2000, 5692, 2009, 1109, 2442, 3042, 5217, 5219, 5229, 5321, 5332, 5345, 5368, 
	5378, 5387, 5399, 5425, 5700, 6100, 1678, 1692, 1699, 1721, 1728, 2302, 2517, 1995, 2004, 2013, 2079, 2083, 2114, 2120, 
	3952, 6658, 3960, 9454, 9468, 9482, 5234, 5243, 5255, 5265, 5271, 5709, 5280, 5277, 5529, 5537, 5545, 5548, 5532, 5540, 
	5551, 5563, 236, 708, 2355, 5478, 5484, 5486, 5728, 5740, 5750, 11059, 2941, 2945, 241, 2962, 2966, 2978, 259, 296, 
	1397, 1687, 1795, 1802, 1812, 1817, 2089, 2986, 2998, 2992, 3004, 2100, 2707, 2732, 2759, 2743, 2770, 4213, 4185, 4260, 
	4269, 4312, 4323, 5491, 5502, 11025, 11026, 1606, 1611, 7318, 10490, 10494, 1634, 1639, 2482, 3853, 246, 299, 2716, 3011, 
	3018, 3025, 3031, 4222, 3036, 4278, 4326, 4335, 5494, 355, 303, 323, 332, 340, 414, 429, 437, 627, 667, 962, 
	1147, 1152, 1250, 1253, 1353, 2345, 5507, 8181, 8187, 8350, 8562, 2462, 5907, 5915, 7385, 7956, 7982, 7983, 2452, 7950, 
	953, 590, 593, 717, 746, 752, 758, 766, 771, 776, 819, 823, 8193, 8199, 7874, 877, 1308, 4874, 1297, 1300, 
	1332, 1343, 4851, 4897, 1367, 1540, 1616, 1644, 2237, 4964, 4965, 7322, 11032, 4570, 4574, 4602, 4603, 4669, 4581, 4585, 
	4593, 4594, 4934, 5526, 7932, 7934, 7941, 7943, 6880, 4758, 6379, 6384, 6452, 6458, 6467, 6468, 5678, 5680, 6490, 6496, 
	6499, 6508, 6512, 6517, 6546, 6551, 6612, 6559, 6564, 6622, 7663, 7665, 6581, 6582, 6583, 6633, 6602, 6603, 6604, 6645, 
	6646, 7694, 7699, 6525, 5684, 5686, 5926, 5928, 9231, 9237, 9242, 9251, 9253, 9273, 9318, 9321, 9322, 9354, 9360, 9362, 
	9363, 9385, 9390, 9395, 9423, 10641, 10643, 10678, 10752, 10755, 10840, 10775, 10781, 10909, 10911, 10973, 10975
  };

  fprintf (dump_file, "%s %s:%d, %s:%d\n",
	  simplify ? "Applying pattern" : "Matching expression", file1, dbg_line_numbers[line1_id], file2, line2);
}

#pragma GCC diagnostic pop
