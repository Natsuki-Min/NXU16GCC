/* Generated automatically by the program `genmatch' from
   a IL pattern matching and simplification description.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-function"

#include "generic-match-auto.h"

bool
tree_bit_not_with_nop (tree t, tree *res_ops)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  switch (TREE_CODE (t))
    {
    case BIT_NOT_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	  {
	    res_ops[0] = captures[0];
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 2, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    CASE_CONVERT:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	switch (TREE_CODE (_p0))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q20 = TREE_OPERAND (_p0, 0);
	      {
		tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
		  {
		    {
		      res_ops[0] = captures[0];
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 3, __FILE__, __LINE__, false);
		      return true;
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
  return false;
}

bool
tree_with_certain_nonzero_bits2 (tree t, tree *res_ops)
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
	    res_ops[0] = captures[0];
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 36, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case BIT_IOR_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	switch (TREE_CODE (_p1))
	  {
	  case INTEGER_CST:
	    {
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _p1, _p0 };
		{
		  res_ops[0] = captures[0];
		  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 37, __FILE__, __LINE__, false);
		  return true;
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
  return false;
}

bool
tree_unsigned_integer_sat_add (tree t, tree *res_ops)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  switch (TREE_CODE (t))
    {
    case BIT_IOR_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree _p0_pops[2];
	  if (tree_usadd_left_part_1 (_p0, _p0_pops))
	    {
	      tree _q20 = _p0_pops[0];
	      tree _q21 = _p0_pops[1];
	      {
		tree _p1_pops[2];
		if (tree_usadd_right_part_1 (_p1, _p1_pops))
		  {
		    tree _q50 = _p1_pops[0];
		    tree _q51 = _p1_pops[1];
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			      {
				res_ops[0] = captures[0];
				res_ops[1] = captures[1];
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 46, __FILE__, __LINE__, false);
				return true;
			      }
			    }
			  }
		      }
		  }
	      }
	    }
	}
	{
	  tree _p0_pops[2];
	  if (tree_usadd_right_part_1 (_p0, _p0_pops))
	    {
	      tree _q20 = _p0_pops[0];
	      tree _q21 = _p0_pops[1];
	      {
		tree _p1_pops[2];
		if (tree_usadd_left_part_1 (_p1, _p1_pops))
		  {
		    tree _q50 = _p1_pops[0];
		    tree _q51 = _p1_pops[1];
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			      {
				res_ops[0] = captures[0];
				res_ops[1] = captures[1];
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 46, __FILE__, __LINE__, false);
				return true;
			      }
			    }
			  }
		      }
		  }
	      }
	    }
	}
	{
	  tree _p0_pops[2];
	  if (tree_usadd_left_part_2 (_p0, _p0_pops))
	    {
	      tree _q20 = _p0_pops[0];
	      tree _q21 = _p0_pops[1];
	      {
		tree _p1_pops[2];
		if (tree_usadd_right_part_2 (_p1, _p1_pops))
		  {
		    tree _q50 = _p1_pops[0];
		    tree _q51 = _p1_pops[1];
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			      {
				res_ops[0] = captures[0];
				res_ops[1] = captures[1];
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 47, __FILE__, __LINE__, false);
				return true;
			      }
			    }
			  }
		      }
		  }
	      }
	    }
	}
	{
	  tree _p0_pops[2];
	  if (tree_usadd_right_part_2 (_p0, _p0_pops))
	    {
	      tree _q20 = _p0_pops[0];
	      tree _q21 = _p0_pops[1];
	      {
		tree _p1_pops[2];
		if (tree_usadd_left_part_2 (_p1, _p1_pops))
		  {
		    tree _q50 = _p1_pops[0];
		    tree _q51 = _p1_pops[1];
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			      {
				res_ops[0] = captures[0];
				res_ops[1] = captures[1];
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 47, __FILE__, __LINE__, false);
				return true;
			      }
			    }
			  }
		      }
		  }
	      }
	    }
	}
        break;
      }
    case COND_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	tree _p2 = TREE_OPERAND (t, 2);
	switch (TREE_CODE (_p0))
	  {
	  case GE_EXPR:
	    {
	      tree _q20 = TREE_OPERAND (_p0, 0);
	      tree _q21 = TREE_OPERAND (_p0, 1);
	      {
		tree _q20_pops[2];
		if (tree_usadd_left_part_1 (_q20, _q20_pops))
		  {
		    tree _q30 = _q20_pops[0];
		    tree _q31 = _q20_pops[1];
		    if ((_q21 == _q30 && ! TREE_SIDE_EFFECTS (_q21)) || (operand_equal_p (_q21, _q30, 0) && types_match (_q21, _q30)))
		      {
			if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
			  {
			    if (integer_minus_onep (_p2))
			      {
				{
				  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q20 };
				  {
				    res_ops[0] = captures[0];
				    res_ops[1] = captures[1];
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 48, __FILE__, __LINE__, false);
				    return true;
				  }
				}
			      }
			  }
		      }
		  }
	      }
	      break;
	    }
	  case LT_EXPR:
	    {
	      tree _q20 = TREE_OPERAND (_p0, 0);
	      tree _q21 = TREE_OPERAND (_p0, 1);
	      {
		tree _q20_pops[2];
		if (tree_usadd_left_part_1 (_q20, _q20_pops))
		  {
		    tree _q30 = _q20_pops[0];
		    tree _q31 = _q20_pops[1];
		    if ((_q21 == _q30 && ! TREE_SIDE_EFFECTS (_q21)) || (operand_equal_p (_q21, _q30, 0) && types_match (_q21, _q30)))
		      {
			if (integer_minus_onep (_p1))
			  {
			    if ((_p2 == _q20 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q20, 0) && types_match (_p2, _q20)))
			      {
				{
				  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q20 };
				  {
				    res_ops[0] = captures[0];
				    res_ops[1] = captures[1];
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 49, __FILE__, __LINE__, false);
				    return true;
				  }
				}
			      }
			  }
		      }
		  }
	      }
	      break;
	    }
	  case EQ_EXPR:
	    {
	      tree _q20 = TREE_OPERAND (_p0, 0);
	      tree _q21 = TREE_OPERAND (_p0, 1);
	      switch (TREE_CODE (_q20))
	        {
		case IMAGPART_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    switch (TREE_CODE (_q30))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_q30))
		          {
			  case CFN_ADD_OVERFLOW:
			    if (call_expr_nargs (_q30) == 2)
    {
				tree _q40 = CALL_EXPR_ARG (_q30, 0);
				tree _q41 = CALL_EXPR_ARG (_q30, 1);
				if (integer_zerop (_q21))
				  {
				    {
				      tree _p1_pops[2];
				      if (tree_usadd_left_part_2 (_p1, _p1_pops))
				        {
					  tree _q80 = _p1_pops[0];
					  tree _q81 = _p1_pops[1];
					  if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
					    {
					      if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
						{
						  if (integer_minus_onep (_p2))
						    {
						      {
							tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q41 };
							{
							  res_ops[0] = captures[0];
							  res_ops[1] = captures[1];
							  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 50, __FILE__, __LINE__, false);
							  return true;
							}
						      }
						    }
					        }
					    }
					  if ((_q80 == _q41 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q41, 0) && types_match (_q80, _q41)))
					    {
					      if ((_q81 == _q40 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q40, 0) && types_match (_q81, _q40)))
						{
						  if (integer_minus_onep (_p2))
						    {
						      {
							tree captures[2] ATTRIBUTE_UNUSED = { _q41, _q40 };
							{
							  res_ops[0] = captures[0];
							  res_ops[1] = captures[1];
							  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 50, __FILE__, __LINE__, false);
							  return true;
							}
						      }
						    }
					        }
					    }
				        }
				    }
				  }
			      }
			    break;
			  default:;
		          }
		        break;
		      default:;
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
	      switch (TREE_CODE (_q20))
	        {
		case IMAGPART_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    switch (TREE_CODE (_q30))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_q30))
		          {
			  case CFN_ADD_OVERFLOW:
			    if (call_expr_nargs (_q30) == 2)
    {
				tree _q40 = CALL_EXPR_ARG (_q30, 0);
				tree _q41 = CALL_EXPR_ARG (_q30, 1);
				if (integer_zerop (_q21))
				  {
				    if (integer_minus_onep (_p1))
				      {
					{
					  tree _p2_pops[2];
					  if (tree_usadd_left_part_2 (_p2, _p2_pops))
					    {
					      tree _q90 = _p2_pops[0];
					      tree _q91 = _p2_pops[1];
					      if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
						{
						  if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
						    {
						      {
							tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q41 };
							{
							  res_ops[0] = captures[0];
							  res_ops[1] = captures[1];
							  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 51, __FILE__, __LINE__, false);
							  return true;
							}
						      }
						    }
					        }
					      if ((_q90 == _q41 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q41, 0) && types_match (_q90, _q41)))
						{
						  if ((_q91 == _q40 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q40, 0) && types_match (_q91, _q40)))
						    {
						      {
							tree captures[2] ATTRIBUTE_UNUSED = { _q41, _q40 };
							{
							  res_ops[0] = captures[0];
							  res_ops[1] = captures[1];
							  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 51, __FILE__, __LINE__, false);
							  return true;
							}
						      }
						    }
					        }
					    }
					}
				      }
				  }
				switch (TREE_CODE (_q41))
				  {
				  case INTEGER_CST:
				    {
				      if (integer_zerop (_q21))
					{
					  if (integer_minus_onep (_p1))
					    {
					      switch (TREE_CODE (_p2))
					        {
						case REALPART_EXPR:
						  {
						    tree _q90 = TREE_OPERAND (_p2, 0);
						    if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
						      {
							{
							  tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _q30 };
							  if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0]) && int_fits_type_p (captures[1], type)
)
							    {
							      {
								res_ops[0] = captures[0];
								res_ops[1] = captures[1];
								if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 52, __FILE__, __LINE__, false);
								return true;
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
			      }
			    break;
			  default:;
		          }
		        break;
		      default:;
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
	      {
		tree _q21_pops[2];
		if (tree_usadd_left_part_1 (_q21, _q21_pops))
		  {
		    tree _q40 = _q21_pops[0];
		    tree _q41 = _q21_pops[1];
		    if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
		      {
			if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
			  {
			    if (integer_minus_onep (_p2))
			      {
				{
				  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q41, _q21 };
				  {
				    res_ops[0] = captures[0];
				    res_ops[1] = captures[1];
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 53, __FILE__, __LINE__, false);
				    return true;
				  }
				}
			      }
			  }
		      }
		  }
	      }
	      break;
	    }
	  case GT_EXPR:
	    {
	      tree _q20 = TREE_OPERAND (_p0, 0);
	      tree _q21 = TREE_OPERAND (_p0, 1);
	      {
		tree _q21_pops[2];
		if (tree_usadd_left_part_1 (_q21, _q21_pops))
		  {
		    tree _q40 = _q21_pops[0];
		    tree _q41 = _q21_pops[1];
		    if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
		      {
			if (integer_minus_onep (_p1))
			  {
			    if ((_p2 == _q21 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q21, 0) && types_match (_p2, _q21)))
			      {
				{
				  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q41, _q21 };
				  {
				    res_ops[0] = captures[0];
				    res_ops[1] = captures[1];
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 54, __FILE__, __LINE__, false);
				    return true;
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
    case PLUS_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	switch (TREE_CODE (_p0))
	  {
	  case MIN_EXPR:
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
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _q21 };
			    if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
			      {
				{
 unsigned precision = TYPE_PRECISION (type);
 wide_int cst_1 = wi::to_wide (captures[1]);
 wide_int cst_2 = wi::to_wide (captures[2]);
 wide_int max = wi::mask (precision, false, precision);
 wide_int sum = wi::add (cst_1, cst_2);
				    if (wi::eq_p (max, sum)
)
				      {
					{
					  res_ops[0] = captures[0];
					  res_ops[1] = captures[1];
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 55, __FILE__, __LINE__, false);
					  return true;
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
generic_simplify_26 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (plusminus))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_SATURATING (type)
 && (!FLOAT_TYPE_P (type) || flag_associative_math)
)
    {
      if (!ALL_FRACT_MODE_P (TYPE_MODE (type))
)
	{
	  if ((!ANY_INTEGRAL_TYPE_P (type)
 || TYPE_OVERFLOW_WRAPS (type)
 || (INTEGRAL_TYPE_P (type)
 && ((tree_expr_nonzero_p (captures[1])
 && (plusminus == MINUS_EXPR
 || expr_not_equal_to (captures[1],
 wi::minus_one (TYPE_PRECISION (type)))))
 || expr_not_equal_to (captures[2],
 (plusminus == PLUS_EXPR
 ? wi::max_value (TYPE_PRECISION (type), SIGNED)
 : wi::min_value (TYPE_PRECISION (type), SIGNED))))))
 && single_use (captures[0])
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail254;
	      {
		tree res_op0;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] =  build_one_cst (type);
		  _r1 = fold_build2_loc (loc, plusminus, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  res_op0 = _r1;
		}
		tree res_op1;
		res_op1 = captures[1];
		tree _r;
		_r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 124, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail254:;
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_31 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail259;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[0];
	  _o1[1] = captures[2];
	  _r1 = fold_build2_loc (loc, op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree res_op1;
	res_op1 = captures[1];
	tree _r;
	_r = fold_build2_loc (loc, RDIV_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 129, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail259:;
    }
  return NULL_TREE;
}

tree
generic_simplify_35 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (log1),
 const enum tree_code ARG_UNUSED (log2),
 const combined_fn ARG_UNUSED (popcount))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail263;
  {
    tree res_op0;
    {
      tree _o1[2], _r1;
      _o1[0] = captures[2];
      _o1[1] = captures[5];
      _r1 = fold_build2_loc (loc, log2, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      res_op0 = _r1;
    }
    tree _r;
    _r = maybe_build_call_expr_loc (loc, popcount, type, 1, res_op0);
    if (!_r)
      goto next_after_fail263;
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 133, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail263:;
  return NULL_TREE;
}

tree
generic_simplify_40 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail268;
  {
    tree res_op0;
    {
      tree _o1[2], _r1;
      _o1[0] = captures[1];
      _o1[1] = unshare_expr (captures[2]);
      _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      res_op0 = _r1;
    }
    tree res_op1;
    res_op1 = captures[2];
    tree _r;
    _r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 138, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail268:;
  return NULL_TREE;
}

tree
generic_simplify_45 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
 && tree_nop_conversion_p (type, TREE_TYPE (captures[3]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail273;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[3];
	    _r2 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
	    _o1[0] = _r2;
	  }
	  _o1[1] = captures[0];
	  _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree _r;
	_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 143, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail273:;
    }
  return NULL_TREE;
}

tree
generic_simplify_51 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail281;
	  {
	    tree res_op0;
	    res_op0 = captures[1];
	    tree _r;
	    _r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 150, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail281:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_56 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (INTEGRAL_TYPE_P (type)
 && TYPE_OVERFLOW_UNDEFINED (type)
 && TREE_CODE (captures[1]) != INTEGER_CST
 && element_precision (type) <= element_precision (TREE_TYPE (captures[1]))
)
	    {
	      {
 tree utype = unsigned_type_for (type);
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail288;
		  {
		    tree res_op0;
		    {
		      tree _o1[1], _r1;
		      {
			tree _o2[1], _r2;
			_o2[0] = captures[1];
			if (TREE_TYPE (_o2[0]) != utype)
			  {
			    _r2 = fold_build1_loc (loc, NOP_EXPR, utype, _o2[0]);
			  }
			else
			  _r2 = _o2[0];
			_o1[0] = _r2;
		      }
		      _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
		      res_op0 = _r1;
		    }
		    tree _r;
		    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		    if (TREE_SIDE_EFFECTS (captures[2]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 157, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail288:;
	      }
	    }
	  else
	    {
	      if (element_precision (type) <= element_precision (TREE_TYPE (captures[1]))
 || (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[1])))
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail289;
		  {
		    tree res_op0;
		    {
		      tree _o1[1], _r1;
		      _o1[0] = captures[1];
		      if (TREE_TYPE (_o1[0]) != type)
			{
			  _r1 = fold_build1_loc (loc, NOP_EXPR, type, _o1[0]);
			}
		      else
		        _r1 = _o1[0];
		      res_op0 = _r1;
		    }
		    tree _r;
		    _r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
		    if (TREE_SIDE_EFFECTS (captures[2]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 158, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail289:;
		}
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_66 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[2]))
 && TYPE_PRECISION (TREE_TYPE (captures[0])) == TYPE_PRECISION (TREE_TYPE (captures[2]))
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail300;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail300;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[0];
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[2];
	    if (TREE_TYPE (_o2[0]) != TREE_TYPE (_o1[0]))
	      {
		_r2 = fold_build1_loc (loc, NOP_EXPR, TREE_TYPE (_o1[0]), _o2[0]);
	      }
	    else
	      _r2 = _o2[0];
	    _o1[1] = _r2;
	  }
	  _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree res_op1;
	res_op1 = captures[1];
	tree _r;
	_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 169, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail300:;
    }
  return NULL_TREE;
}

tree
generic_simplify_79 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail313;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail313;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 = captures[3];
    tree _r;
    _r = fold_build2_loc (loc, BIT_IOR_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 179, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail313:;
  return NULL_TREE;
}

tree
generic_simplify_88 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op),
 const enum tree_code ARG_UNUSED (rop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (element_precision (type) <= element_precision (TREE_TYPE (captures[0]))
 && element_precision (type) <= element_precision (TREE_TYPE (captures[1]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail322;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[0];
	    if (TREE_TYPE (_o2[0]) != type)
	      {
		_r2 = fold_build1_loc (loc, NOP_EXPR, type, _o2[0]);
	      }
	    else
	      _r2 = _o2[0];
	    _o1[0] = _r2;
	  }
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[1];
	    if (TREE_TYPE (_o2[0]) != type)
	      {
		_r2 = fold_build1_loc (loc, NOP_EXPR, type, _o2[0]);
	      }
	    else
	      _r2 = _o2[0];
	    _o1[1] = _r2;
	  }
	  _r1 = fold_build2_loc (loc, rop, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree _r;
	_r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 186, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail322:;
    }
  return NULL_TREE;
}

tree
generic_simplify_98 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (((TREE_CODE (captures[3]) == INTEGER_CST
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && (int_fits_type_p (captures[3], TREE_TYPE (captures[1]))
 || tree_nop_conversion_p (TREE_TYPE (captures[1]), type)))
 || types_match (captures[1], captures[3]))
 && !POINTER_TYPE_P (TREE_TYPE (captures[1]))
 && !VECTOR_TYPE_P (TREE_TYPE (captures[1]))
 && TREE_CODE (TREE_TYPE (captures[1])) != OFFSET_TYPE
 && (bitop != BIT_AND_EXPR ||
0
)
 && (
 TYPE_PRECISION (TREE_TYPE (captures[1])) < TYPE_PRECISION (type)
 || (
0
 && TYPE_PRECISION (TREE_TYPE (captures[1])) == TYPE_PRECISION (type))
 || GET_MODE_CLASS (TYPE_MODE (type)) != MODE_INT
 || !type_has_mode_precision_p (type)
 || (
0
 && TREE_CODE (captures[3]) != INTEGER_CST
 && tree_nop_conversion_p (type, TREE_TYPE (captures[1]))
 && single_use (captures[0])
 && single_use (captures[2])))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail336;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[3];
	    if (TREE_TYPE (_o2[0]) != TREE_TYPE (_o1[0]))
	      {
		_r2 = fold_build1_loc (loc, NOP_EXPR, TREE_TYPE (_o1[0]), _o2[0]);
	      }
	    else
	      _r2 = _o2[0];
	    _o1[1] = _r2;
	  }
	  _r1 = fold_build2_loc (loc, bitop, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree _r;
	_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 191, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail336:;
    }
  return NULL_TREE;
}

tree
generic_simplify_110 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && operand_equal_p (captures[2], captures[6], 0)
 && (tree_nonzero_bits (captures[0]) & tree_nonzero_bits (captures[4])) == 0
)
    {
      {
 tree t = type;
 if (!TYPE_OVERFLOW_WRAPS (t))
 t = unsigned_type_for (t);
 wide_int c = wi::add (wi::to_wide (captures[3]), wi::to_wide (captures[7]));
	  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail349;
	  if (TREE_SIDE_EFFECTS (captures[7])) goto next_after_fail349;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail349;
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
	    if (TREE_SIDE_EFFECTS (captures[6]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[6]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 199, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail349:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_120 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail365;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail365;
  {
    tree _r;
    _r = captures[2];
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 215, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail365:;
  return NULL_TREE;
}

tree
generic_simplify_124 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (code2),
 const enum tree_code ARG_UNUSED (code1))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((TREE_CODE (captures[2]) == INTEGER_CST
 && TREE_CODE (captures[4]) == INTEGER_CST)
 || ((INTEGRAL_TYPE_P (TREE_TYPE (captures[2]))
 || POINTER_TYPE_P (TREE_TYPE (captures[2])))
 && operand_equal_p (captures[2], captures[4]))
)
    {
      {
 int cmp = 0;
 if (TREE_CODE (captures[2]) == INTEGER_CST
 && TREE_CODE (captures[4]) == INTEGER_CST)
 cmp = tree_int_cst_compare (captures[2], captures[4]);
	  if ((code1 == LT_EXPR || code1 == LE_EXPR)
 && (code2 == LT_EXPR || code2 == LE_EXPR)
)
	    {
	      if ((cmp < 0) || (cmp == 0 && code1 == LT_EXPR)
)
		{
		  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail381;
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail381;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail381;
		  {
		    tree _r;
		    _r = captures[3];
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 224, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail381:;
		}
	      else
		{
		  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail382;
		  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail382;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail382;
		  {
		    tree _r;
		    _r = captures[0];
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 225, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail382:;
		}
	    }
	  else
	    {
	      if ((code1 == GT_EXPR || code1 == GE_EXPR)
 && (code2 == GT_EXPR || code2 == GE_EXPR)
)
		{
		  if ((cmp > 0) || (cmp == 0 && code1 == GT_EXPR)
)
		    {
		      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail383;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail383;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail383;
		      {
			tree _r;
			_r = captures[3];
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 226, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail383:;
		    }
		  else
		    {
		      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail384;
		      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail384;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail384;
		      {
			tree _r;
			_r = captures[0];
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 227, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail384:;
		    }
		}
	      else
		{
		  if (cmp == 0
 && ((code1 == LT_EXPR && code2 == GT_EXPR)
 || (code1 == GT_EXPR && code2 == LT_EXPR))
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail385;
		      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail385;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail385;
		      {
			tree res_op0;
			res_op0 = captures[1];
			tree res_op1;
			res_op1 = captures[4];
			tree _r;
			_r = fold_build2_loc (loc, NE_EXPR, type, res_op0, res_op1);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 228, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail385:;
		    }
		  else
		    {
		      if (cmp >= 0
 && (code1 == LT_EXPR || code1 == LE_EXPR)
 && (code2 == GT_EXPR || code2 == GE_EXPR)
)
			{
			  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail386;
			  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail386;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail386;
			  {
			    tree _r;
			    _r =  constant_boolean_node (true, type);
			    if (TREE_SIDE_EFFECTS (captures[1]))
			      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 229, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail386:;
			}
		      else
			{
			  if (cmp <= 0
 && (code1 == GT_EXPR || code1 == GE_EXPR)
 && (code2 == LT_EXPR || code2 == LE_EXPR)
)
			    {
			      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail387;
			      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail387;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail387;
			      {
				tree _r;
				_r =  constant_boolean_node (true, type);
				if (TREE_SIDE_EFFECTS (captures[1]))
				  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 230, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail387:;
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
generic_simplify_140 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && tree_int_cst_sgn (captures[2]) > 0
 && tree_int_cst_sgn (captures[4]) > 0
 && (tree_nonzero_bits (captures[0]) & tree_nonzero_bits (captures[3])) == 0
)
    {
      {
 tree t = type;
 if (!TYPE_OVERFLOW_WRAPS (t))
 t = unsigned_type_for (t);
 wide_int wone = wi::one (TYPE_PRECISION (t));
 wide_int c = wi::add (wi::lshift (wone, wi::to_wide (captures[2])),
 wi::lshift (wone, wi::to_wide (captures[4])));
	  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail417;
	  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail417;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail417;
	  {
	    tree res_op0;
	    {
	      tree _o1[2], _r1;
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[1];
		if (TREE_TYPE (_o2[0]) != t)
		  {
		    _r2 = fold_build1_loc (loc, NOP_EXPR, t, _o2[0]);
		  }
		else
		  _r2 = _o2[0];
		_o1[0] = _r2;
	      }
	      _o1[1] =  wide_int_to_tree (t,c);
	      _r1 = fold_build2_loc (loc, MULT_EXPR, t, _o1[0], _o1[1]);
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 237, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail417:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_150 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail427;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 = captures[1];
    tree _r;
    _r = fold_build2_loc (loc, BIT_XOR_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 246, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail427:;
  return NULL_TREE;
}

tree
generic_simplify_153 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail430;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 = captures[1];
    tree _r;
    _r = fold_build2_loc (loc, BIT_XOR_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 249, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail430:;
  return NULL_TREE;
}

tree
generic_simplify_159 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[2]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail436;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[2];
	    _r2 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, _o2[0]);
	    _o1[0] = _r2;
	  }
	  _o1[1] = captures[3];
	  _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree _r;
	_r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 255, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail436:;
    }
  return NULL_TREE;
}

tree
generic_simplify_166 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && !TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 && types_match (TREE_TYPE (captures[1]), TREE_TYPE (captures[3]))
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail444;
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail444;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail444;
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
	_r = fold_build2_loc (loc, GE_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 262, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail444:;
    }
  return NULL_TREE;
}

tree
generic_simplify_173 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (copysigns))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail452;
  {
    if (! tree_invariant_p (captures[1])) goto next_after_fail452;
    tree res_op0;
    res_op0 = unshare_expr (captures[1]);
    tree res_op1;
    res_op1 = captures[1];
    tree _r;
    _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
    if (TREE_SIDE_EFFECTS (captures[2]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 268, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail452:;
  return NULL_TREE;
}

tree
generic_simplify_183 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[2]))
 && tree_nop_conversion_p (TREE_TYPE (captures[0]), TREE_TYPE (captures[2]))
)
    {
      {
 wi::overflow_type overflow;
 wide_int mul = wi::mul (wi::to_wide (captures[1]), wi::to_wide (captures[2]),
 TYPE_SIGN (type), &overflow);
	  if (types_match (type, TREE_TYPE (captures[2]))
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[2])) && !overflow
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail464;
	      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail464;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail464;
	      {
		tree res_op0;
		res_op0 = captures[0];
		tree res_op1;
		res_op1 =  wide_int_to_tree (type, mul);
		tree _r;
		_r = fold_build2_loc (loc, op, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 276, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail464:;
	    }
	  else
	    {
	      {
 tree utype = unsigned_type_for (type);
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail465;
		  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail465;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail465;
		  {
		    tree res_op0;
		    {
		      tree _o1[2], _r1;
		      {
			tree _o2[1], _r2;
			_o2[0] = captures[0];
			if (TREE_TYPE (_o2[0]) != utype)
			  {
			    _r2 = fold_build1_loc (loc, NOP_EXPR, utype, _o2[0]);
			  }
			else
			  _r2 = _o2[0];
			_o1[0] = _r2;
		      }
		      {
			tree _o2[2], _r2;
			{
			  tree _o3[1], _r3;
			  _o3[0] = captures[1];
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
			  _o3[0] = captures[2];
			  if (TREE_TYPE (_o3[0]) != utype)
			    {
			      _r3 = fold_build1_loc (loc, NOP_EXPR, utype, _o3[0]);
			    }
			  else
			    _r3 = _o3[0];
			  _o2[1] = _r3;
			}
			_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
			_o1[1] = _r2;
		      }
		      _r1 = fold_build2_loc (loc, op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      res_op0 = _r1;
		    }
		    tree _r;
		    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 277, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail465:;
	      }
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_203 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_OVERFLOW_SANITIZED (TREE_TYPE (captures[2]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail501;
      {
	tree res_op0;
	res_op0 = captures[1];
	tree res_op1;
	res_op1 = captures[3];
	tree _r;
	_r = fold_build2_loc (loc, op, type, res_op0, res_op1);
	if (TREE_SIDE_EFFECTS (captures[2]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 312, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail501:;
    }
  return NULL_TREE;
}

tree
generic_simplify_207 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (neeq))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 poly_int64 diff; tree inner_type = TREE_TYPE (captures[1]);
      if (ptr_difference_const (captures[0], captures[2], &diff)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail505;
	  {
	    tree res_op0;
	    {
	      tree _o1[2], _r1;
	      _o1[0] =  build_int_cst_type (inner_type, diff);
	      _o1[1] = captures[1];
	      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      res_op0 = _r1;
	    }
	    tree res_op1;
	    res_op1 = captures[3];
	    tree _r;
	    _r = fold_build2_loc (loc, neeq, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (TREE_SIDE_EFFECTS (captures[2]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 315, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail505:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_216 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_fits_shwi_p (captures[3])
 && tree_to_shwi (captures[3]) > 0
 && tree_to_shwi (captures[3]) < TYPE_PRECISION (TREE_TYPE (captures[2]))
)
    {
      if (tree_to_shwi (captures[3]) > wi::ctz (wi::to_wide (captures[5]))
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail517;
	  {
	    tree _r;
	    _r =  constant_boolean_node (cmp == NE_EXPR, type);
	    if (TREE_SIDE_EFFECTS (captures[2]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	    if (TREE_SIDE_EFFECTS (captures[3]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
	    if (TREE_SIDE_EFFECTS (captures[4]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[4]), _r);
	    if (TREE_SIDE_EFFECTS (captures[5]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[5]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 327, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail517:;
	}
      else
	{
	  {
 wide_int c1 = wi::to_wide (captures[3]);
 wide_int c2 = wi::lrshift (wi::to_wide (captures[4]), c1);
 wide_int c3 = wi::lrshift (wi::to_wide (captures[5]), c1);
	      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail518;
	      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail518;
	      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail518;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail518;
	      {
		tree res_op0;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] =  wide_int_to_tree (TREE_TYPE (captures[2]), c2);
		  _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  res_op0 = _r1;
		}
		tree res_op1;
		res_op1 =  wide_int_to_tree (TREE_TYPE (captures[2]), c3);
		tree _r;
		_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 328, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail518:;
	  }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_227 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const combined_fn ARG_UNUSED (bswap))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree ctype = TREE_TYPE (captures[1]);
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail537;
      {
	tree res_op0;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  if (TREE_TYPE (_o1[0]) != ctype)
	    {
	      _r1 = fold_build1_loc (loc, NOP_EXPR, ctype, _o1[0]);
	    }
	  else
	    _r1 = _o1[0];
	  res_op0 = _r1;
	}
	tree res_op1;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[1];
	  _r1 = maybe_build_call_expr_loc (loc, bswap, TREE_TYPE (_o1[0]), 1, _o1[0]);
	  if (!_r1)
	    goto next_after_fail537;
	  if (EXPR_P (_r1))
	    goto next_after_fail537;
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 347, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail537:;
  }
  return NULL_TREE;
}

tree
generic_simplify_234 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (! FLOAT_TYPE_P (TREE_TYPE (captures[0]))
 || ! tree_expr_maybe_nan_p (captures[0])
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail549;
      {
	tree _r;
	_r =  constant_boolean_node (true, type);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 358, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail549:;
    }
  else
    {
      if (cmp != EQ_EXPR
 && (! FLOAT_TYPE_P (TREE_TYPE (captures[0]))
 || ! flag_trapping_math)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail550;
	  {
	    tree res_op0;
	    res_op0 = unshare_expr (captures[0]);
	    tree res_op1;
	    res_op1 = captures[0];
	    tree _r;
	    _r = fold_build2_loc (loc, EQ_EXPR, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 359, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail550:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_239 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (captures[1]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail555;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 =  build_real (TREE_TYPE (captures[1]), dconst0);
	tree _r;
	_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 363, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail555:;
    }
  else
    {
      if (TREE_CODE (captures[0]) == REAL_CST
 && REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (captures[0]))
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail556;
	  {
	    tree res_op0;
	    res_op0 =  build_real (TREE_TYPE (captures[0]), dconst0);
	    tree res_op1;
	    res_op1 = captures[1];
	    tree _r;
	    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 364, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail556:;
	}
      else
	{
	  if (REAL_VALUE_ISNAN (TREE_REAL_CST (captures[1]))
 && (cmp == EQ_EXPR || cmp == NE_EXPR || !flag_trapping_math)
 && !tree_expr_signaling_nan_p (captures[1])
 && !tree_expr_maybe_signaling_nan_p (captures[0])
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail557;
	      {
		tree _r;
		_r =  constant_boolean_node (cmp == NE_EXPR, type);
		if (TREE_SIDE_EFFECTS (captures[0]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 365, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail557:;
	    }
	  else
	    {
	      if (TREE_CODE (captures[0]) == REAL_CST
 && REAL_VALUE_ISNAN (TREE_REAL_CST (captures[0]))
 && (cmp == EQ_EXPR || cmp == NE_EXPR || !flag_trapping_math)
 && !tree_expr_signaling_nan_p (captures[0])
 && !tree_expr_signaling_nan_p (captures[1])
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail558;
		  {
		    tree _r;
		    _r =  constant_boolean_node (cmp == NE_EXPR, type);
		    if (TREE_SIDE_EFFECTS (captures[0]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
		    if (TREE_SIDE_EFFECTS (captures[1]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 366, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail558:;
		}
	      else
		{
		  if (REAL_VALUE_ISINF (TREE_REAL_CST (captures[1]))
 && MODE_HAS_INFINITIES (TYPE_MODE (TREE_TYPE (captures[1])))
)
		    {
		      {
 REAL_VALUE_TYPE max;
 enum tree_code code = cmp;
 bool neg = REAL_VALUE_NEGATIVE (TREE_REAL_CST (captures[1]));
 if (neg)
 code = swap_tree_comparison (code);
			  if (code == GT_EXPR
 && !(HONOR_NANS (captures[0]) && flag_trapping_math)
)
			    {
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail559;
			      {
				tree _r;
				_r =  constant_boolean_node (false, type);
				if (TREE_SIDE_EFFECTS (captures[0]))
				  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
				if (TREE_SIDE_EFFECTS (captures[1]))
				  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 367, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail559:;
			    }
			  else
			    {
			      if (code == LE_EXPR
)
				{
				  if (! HONOR_NANS (captures[0])
)
				    {
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail560;
				      {
					tree _r;
					_r =  constant_boolean_node (true, type);
					if (TREE_SIDE_EFFECTS (captures[0]))
					  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
					if (TREE_SIDE_EFFECTS (captures[1]))
					  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 368, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail560:;
				    }
				  else
				    {
				      if (!flag_trapping_math
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail561;
					  {
					    if (! tree_invariant_p (captures[0])) goto next_after_fail561;
					    tree res_op0;
					    res_op0 = unshare_expr (captures[0]);
					    tree res_op1;
					    res_op1 = captures[0];
					    tree _r;
					    _r = fold_build2_loc (loc, EQ_EXPR, type, res_op0, res_op1);
					    if (TREE_SIDE_EFFECTS (captures[1]))
					      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 369, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail561:;
					}
				    }
				}
			      else
				{
				  if ((code == EQ_EXPR && !(HONOR_NANS (captures[0]) && flag_trapping_math))
 || code == GE_EXPR
)
				    {
				      {
 real_maxval (&max, neg, TYPE_MODE (TREE_TYPE (captures[0])));
					  if (neg
)
					    {
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail562;
					      {
						tree res_op0;
						res_op0 = captures[0];
						tree res_op1;
						res_op1 =  build_real (TREE_TYPE (captures[0]), max);
						tree _r;
						_r = fold_build2_loc (loc, LT_EXPR, type, res_op0, res_op1);
						if (TREE_SIDE_EFFECTS (captures[1]))
						  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 370, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail562:;
					    }
					  else
					    {
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail563;
					      {
						tree res_op0;
						res_op0 = captures[0];
						tree res_op1;
						res_op1 =  build_real (TREE_TYPE (captures[0]), max);
						tree _r;
						_r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
						if (TREE_SIDE_EFFECTS (captures[1]))
						  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 371, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail563:;
					    }
				      }
				    }
				  else
				    {
				      if (code == LT_EXPR
)
					{
					  {
 real_maxval (&max, neg, TYPE_MODE (TREE_TYPE (captures[0])));
					      if (neg
)
						{
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail564;
						  {
						    tree res_op0;
						    res_op0 = captures[0];
						    tree res_op1;
						    res_op1 =  build_real (TREE_TYPE (captures[0]), max);
						    tree _r;
						    _r = fold_build2_loc (loc, GE_EXPR, type, res_op0, res_op1);
						    if (TREE_SIDE_EFFECTS (captures[1]))
						      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
						    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 372, __FILE__, __LINE__, true);
						    return _r;
						  }
next_after_fail564:;
						}
					      else
						{
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail565;
						  {
						    tree res_op0;
						    res_op0 = captures[0];
						    tree res_op1;
						    res_op1 =  build_real (TREE_TYPE (captures[0]), max);
						    tree _r;
						    _r = fold_build2_loc (loc, LE_EXPR, type, res_op0, res_op1);
						    if (TREE_SIDE_EFFECTS (captures[1]))
						      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
						    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 373, __FILE__, __LINE__, true);
						    return _r;
						  }
next_after_fail565:;
						}
					  }
					}
				      else
					{
					  if (code == NE_EXPR
)
					    {
					      {
 real_maxval (&max, neg, TYPE_MODE (TREE_TYPE (captures[0])));
						  if (! HONOR_NANS (captures[0])
)
						    {
						      if (neg
)
							{
							  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail566;
							  {
							    tree res_op0;
							    res_op0 = captures[0];
							    tree res_op1;
							    res_op1 =  build_real (TREE_TYPE (captures[0]), max);
							    tree _r;
							    _r = fold_build2_loc (loc, GE_EXPR, type, res_op0, res_op1);
							    if (TREE_SIDE_EFFECTS (captures[1]))
							      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
							    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 374, __FILE__, __LINE__, true);
							    return _r;
							  }
next_after_fail566:;
							}
						      else
							{
							  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail567;
							  {
							    tree res_op0;
							    res_op0 = captures[0];
							    tree res_op1;
							    res_op1 =  build_real (TREE_TYPE (captures[0]), max);
							    tree _r;
							    _r = fold_build2_loc (loc, LE_EXPR, type, res_op0, res_op1);
							    if (TREE_SIDE_EFFECTS (captures[1]))
							      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
							    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 375, __FILE__, __LINE__, true);
							    return _r;
							  }
next_after_fail567:;
							}
						    }
						  else
						    {
						      if (neg
)
							{
							  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail568;
							  {
							    tree res_op0;
							    res_op0 = captures[0];
							    tree res_op1;
							    res_op1 =  build_real (TREE_TYPE (captures[0]), max);
							    tree _r;
							    _r = fold_build2_loc (loc, UNGE_EXPR, type, res_op0, res_op1);
							    if (TREE_SIDE_EFFECTS (captures[1]))
							      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
							    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 376, __FILE__, __LINE__, true);
							    return _r;
							  }
next_after_fail568:;
							}
						      else
							{
							  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail569;
							  {
							    tree res_op0;
							    res_op0 = captures[0];
							    tree res_op1;
							    res_op1 =  build_real (TREE_TYPE (captures[0]), max);
							    tree _r;
							    _r = fold_build2_loc (loc, UNLE_EXPR, type, res_op0, res_op1);
							    if (TREE_SIDE_EFFECTS (captures[1]))
							      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
							    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 377, __FILE__, __LINE__, true);
							    return _r;
							  }
next_after_fail569:;
							}
						    }
					      }
					    }
					}
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
generic_simplify_275 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op),
 const enum tree_code ARG_UNUSED (cmp),
 const combined_fn ARG_UNUSED (clz))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (captures[2]) && single_use (captures[0])
)
    {
      {
 tree stype = signed_type_for (TREE_TYPE (captures[1]));
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail674;
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
	    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[2]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 465, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail674:;
      }
    }
  else
    {
      if (wi::to_wide (captures[2]) == TYPE_PRECISION (TREE_TYPE (captures[1])) - 1
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail675;
	  {
	    tree res_op0;
	    res_op0 = captures[1];
	    tree res_op1;
	    res_op1 =  build_one_cst (TREE_TYPE (captures[1]));
	    tree _r;
	    _r = fold_build2_loc (loc, op, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[2]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 466, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail675:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_278 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 HOST_WIDE_INT zero_val = tree_to_shwi (captures[2]);
 tree type0 = TREE_TYPE (captures[1]);
 int prec = TYPE_PRECISION (type0);
      if (prec <= MAX_FIXED_MODE_SIZE
)
	{
	  if (tree_int_cst_sgn (captures[3]) < 0 || wi::to_widest (captures[3]) >= prec
)
	    {
	      if (zero_val != wi::to_widest (captures[3])
)
		{
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail680;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail680;
		  {
		    tree _r;
		    _r =  constant_boolean_node (op == EQ_EXPR ? false : true, type);
		    if (TREE_SIDE_EFFECTS (captures[1]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		    if (TREE_SIDE_EFFECTS (captures[3]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 471, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail680:;
		}
	    }
	  else
	    {
	      if (zero_val < 0 || zero_val >= prec
)
		{
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail681;
		  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail681;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail681;
		  {
		    tree res_op0;
		    {
		      tree _o1[2], _r1;
		      _o1[0] = captures[1];
		      _o1[1] =  wide_int_to_tree (type0,
 wi::mask (tree_to_uhwi (captures[3]) + 1,
 false, prec));
		      _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      res_op0 = _r1;
		    }
		    tree res_op1;
		    res_op1 =  wide_int_to_tree (type0,
 wi::shifted_mask (tree_to_uhwi (captures[3]), 1,
 false, prec));
		    tree _r;
		    _r = fold_build2_loc (loc, op, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 472, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail681:;
		}
	    }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_286 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (copysigns))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!REAL_VALUE_NEGATIVE (TREE_REAL_CST (captures[1]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail692;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree _r;
	_r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 483, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail692:;
    }
  return NULL_TREE;
}

tree
generic_simplify_290 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (COPYSIGN_ALL))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail696;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree _r;
    _r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 487, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail696:;
  return NULL_TREE;
}

tree
generic_simplify_297 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (div))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && TYPE_OVERFLOW_UNDEFINED (type)
 && !integer_zerop (captures[0])
 && (!flag_non_call_exceptions || tree_expr_nonzero_p (captures[0]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail705;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[0];
	  _o1[1] =  build_zero_cst (type);
	  _r1 = fold_build2_loc (loc, LT_EXPR, boolean_type_node, _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree res_op1;
	res_op1 =  build_minus_one_cst (type);
	tree res_op2;
	res_op2 =  build_one_cst (type);
	tree _r;
	_r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 494, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail705:;
    }
  return NULL_TREE;
}

tree
generic_simplify_303 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (floor_divmod),
 const enum tree_code ARG_UNUSED (trunc_divmod))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((INTEGRAL_TYPE_P (type) || VECTOR_INTEGER_TYPE_P (type))
 && TYPE_UNSIGNED (type)
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail712;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 = captures[1];
	tree _r;
	_r = fold_build2_loc (loc, trunc_divmod, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 501, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail712:;
    }
  return NULL_TREE;
}

tree
generic_simplify_309 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (mod))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (type)
 && TYPE_OVERFLOW_UNDEFINED (type)
 && wi::multiple_of_p (wi::to_wide (captures[1]), wi::to_wide (captures[2]),
 TYPE_SIGN (type))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail718;
      {
	tree _r;
	_r =  build_zero_cst (type);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (TREE_SIDE_EFFECTS (captures[2]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 507, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail718:;
    }
  return NULL_TREE;
}

tree
generic_simplify_319 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (TAN),
 const combined_fn ARG_UNUSED (SIN),
 const combined_fn ARG_UNUSED (COS))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
    {
      if (! HONOR_NANS (captures[1])
 && ! HONOR_INFINITIES (captures[1])
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail730;
	  {
	    tree res_op0;
	    res_op0 =  build_one_cst (type);
	    tree res_op1;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[1];
	      _r1 = maybe_build_call_expr_loc (loc, COS, TREE_TYPE (_o1[0]), 1, _o1[0]);
	      if (!_r1)
	        goto next_after_fail730;
	      res_op1 = _r1;
	    }
	    tree _r;
	    _r = fold_build2_loc (loc, RDIV_EXPR, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 519, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail730:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_327 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[1]))
 && !TYPE_OVERFLOW_SANITIZED (TREE_TYPE (captures[1]))
 && (CONSTANT_CLASS_P (captures[1]) || single_use (captures[0]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail740;
      {
	tree res_op0;
	res_op0 = captures[1];
	tree res_op1;
	res_op1 =  build_zero_cst (TREE_TYPE (captures[1]));
	tree _r;
	_r = fold_build2_loc (loc, op, type, res_op0, res_op1);
	if (TREE_SIDE_EFFECTS (captures[2]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 529, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail740:;
    }
  return NULL_TREE;
}

tree
generic_simplify_335 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (wi::gt_p (wi::to_wide (captures[1]), 0, TYPE_SIGN (TREE_TYPE (captures[1])))
)
    {
      {
 wi::overflow_type ovf;
 wide_int prod = wi::mul (wi::to_wide (captures[2]), wi::to_wide (captures[1]),
 TYPE_SIGN (TREE_TYPE (captures[1])), &ovf);
	  if (ovf
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail749;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail749;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail749;
	      {
		tree _r;
		_r =  constant_boolean_node (wi::lt_p (wi::to_wide (captures[2]), 0,
 TYPE_SIGN (TREE_TYPE (captures[2])))
 != (cmp == LT_EXPR || cmp == LE_EXPR), type);
		if (TREE_SIDE_EFFECTS (captures[0]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 538, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail749:;
	    }
	  else
	    {
	      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail750;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail750;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail750;
	      {
		tree res_op0;
		res_op0 = captures[0];
		tree res_op1;
		res_op1 =  wide_int_to_tree (TREE_TYPE (captures[0]), prod);
		tree _r;
		_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 539, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail750:;
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_347 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op),
 const enum tree_code ARG_UNUSED (cmp),
 const combined_fn ARG_UNUSED (ctz))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 bool ok = true;
 HOST_WIDE_INT val = 0;
 if (!tree_fits_shwi_p (captures[2]))
 ok = false;
 else
 {
 val = tree_to_shwi (captures[2]);
 if (op == GT_EXPR || op == LE_EXPR)
 {
 if (val == HOST_WIDE_INT_MAX)
 ok = false;
 else
 val++;
 }
 }
 tree type0 = TREE_TYPE (captures[1]);
 int prec = TYPE_PRECISION (type0);
      if (ok && prec <= MAX_FIXED_MODE_SIZE
)
	{
	  if (val <= 0
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail796;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail796;
	      {
		tree _r;
		_r =  constant_boolean_node (cmp == EQ_EXPR ? true : false, type);
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 568, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail796:;
	    }
	  else
	    {
	      if (val >= prec
)
		{
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail797;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail797;
		  {
		    tree _r;
		    _r =  constant_boolean_node (cmp == EQ_EXPR ? false : true, type);
		    if (TREE_SIDE_EFFECTS (captures[1]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 569, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail797:;
		}
	      else
		{
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail798;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail798;
		  {
		    tree res_op0;
		    {
		      tree _o1[2], _r1;
		      _o1[0] = captures[1];
		      _o1[1] =  wide_int_to_tree (type0,
 wi::mask (val, false, prec));
		      _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      res_op0 = _r1;
		    }
		    tree res_op1;
		    res_op1 =  build_zero_cst (type0);
		    tree _r;
		    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 570, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail798:;
		}
	    }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_356 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (cmp2),
 const enum tree_code ARG_UNUSED (cmp3),
 const enum tree_code ARG_UNUSED (bit_op),
 const combined_fn ARG_UNUSED (ffs))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 int prec = TYPE_PRECISION (TREE_TYPE (captures[1]));
      if (integer_zerop (captures[2])
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail809;
	  {
	    tree res_op0;
	    res_op0 = captures[1];
	    tree res_op1;
	    res_op1 =  build_zero_cst (TREE_TYPE (captures[1]));
	    tree _r;
	    _r = fold_build2_loc (loc, cmp2, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[2]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 577, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail809:;
	}
      else
	{
	  if (tree_int_cst_sgn (captures[2]) < 0
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail810;
	      {
		tree _r;
		_r =  constant_boolean_node (cmp == GT_EXPR ? true : false, type);
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (TREE_SIDE_EFFECTS (captures[2]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 578, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail810:;
	    }
	  else
	    {
	      if (wi::to_widest (captures[2]) >= prec
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail811;
		  {
		    tree _r;
		    _r =  constant_boolean_node (cmp == GT_EXPR ? false : true, type);
		    if (TREE_SIDE_EFFECTS (captures[1]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		    if (TREE_SIDE_EFFECTS (captures[2]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 579, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail811:;
		}
	      else
		{
		  if (wi::to_widest (captures[2]) == prec - 1
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail812;
		      {
			tree res_op0;
			res_op0 = captures[1];
			tree res_op1;
			res_op1 =  wide_int_to_tree (TREE_TYPE (captures[1]),
 wi::shifted_mask (prec - 1, 1,
 false, prec));
			tree _r;
			_r = fold_build2_loc (loc, cmp3, type, res_op0, res_op1);
			if (TREE_SIDE_EFFECTS (captures[2]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 580, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail812:;
		    }
		  else
		    {
		      if (single_use (captures[0])
)
			{
			  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail813;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail813;
			  {
			    if (! tree_invariant_p (captures[1])) goto next_after_fail813;
			    tree res_op0;
			    {
			      tree _o1[2], _r1;
			      _o1[0] = unshare_expr (captures[1]);
			      _o1[1] =  build_zero_cst (TREE_TYPE (captures[1]));
			      _r1 = fold_build2_loc (loc, cmp2, type, _o1[0], _o1[1]);
			      res_op0 = _r1;
			    }
			    tree res_op1;
			    {
			      tree _o1[2], _r1;
			      {
				tree _o2[2], _r2;
				_o2[0] = captures[1];
				_o2[1] =  wide_int_to_tree (TREE_TYPE (captures[1]),
 wi::mask (tree_to_uhwi (captures[2]),
 false, prec));
				_r2 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				_o1[0] = _r2;
			      }
			      _o1[1] =  build_zero_cst (TREE_TYPE (captures[1]));
			      _r1 = fold_build2_loc (loc, cmp3, type, _o1[0], _o1[1]);
			      res_op1 = _r1;
			    }
			    tree _r;
			    _r = fold_build2_loc (loc, bit_op, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 581, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail813:;
			}
		    }
		}
	    }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_376 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail850;
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
next_after_fail850:;
	}
      else
	{
	  {
 tree utype = unsigned_type_for (TREE_TYPE (captures[2]));
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail851;
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
next_after_fail851:;
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
			  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail852;
			  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail852;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail852;
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
next_after_fail852:;
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
				  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail853;
				  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail853;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail853;
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
next_after_fail853:;
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
generic_simplify_404 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 bool wascmp;
      if (bitwise_inverted_equal_p (captures[0], captures[2], wascmp)
 || bitwise_inverted_equal_p (captures[1], captures[2], wascmp)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail881;
	  {
	    tree _r;
	    _r =  wascmp ? constant_boolean_node (false, type) : build_zero_cst (type);
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (TREE_SIDE_EFFECTS (captures[2]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 634, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail881:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_411 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (bswap))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (BITS_PER_UNIT == 8
 && tree_fits_uhwi_p (captures[3])
 && tree_fits_uhwi_p (captures[4])
)
    {
      {
 unsigned HOST_WIDE_INT prec = TYPE_PRECISION (TREE_TYPE (captures[1]));
 unsigned HOST_WIDE_INT bits = tree_to_uhwi (captures[3]);
 unsigned HOST_WIDE_INT mask = tree_to_uhwi (captures[4]);
 unsigned HOST_WIDE_INT lo = bits & 7;
 unsigned HOST_WIDE_INT hi = bits - lo;
	  if (bits < prec
 && mask < (256u>>lo)
 && bits < TYPE_PRECISION (TREE_TYPE(captures[0]))
)
	    {
	      {
 unsigned HOST_WIDE_INT ns = (prec - (hi + 8)) + lo;
		  if (ns == 0
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail892;
		      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail892;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail892;
		      {
			tree res_op0;
			{
			  tree _o1[1], _r1;
			  _o1[0] = captures[2];
			  if (TREE_TYPE (_o1[0]) != type)
			    {
			      _r1 = fold_build1_loc (loc, NOP_EXPR, type, _o1[0]);
			    }
			  else
			    _r1 = _o1[0];
			  res_op0 = _r1;
			}
			tree res_op1;
			res_op1 = captures[4];
			tree _r;
			_r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 641, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail892:;
		    }
		  else
		    {
		      {
 tree utype = unsigned_type_for (TREE_TYPE (captures[2]));
 tree nst = build_int_cst (integer_type_node, ns);
			  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail893;
			  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail893;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail893;
			  {
			    tree res_op0;
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[2], _r2;
				{
				  tree _o3[1], _r3;
				  _o3[0] = captures[2];
				  if (TREE_TYPE (_o3[0]) != utype)
				    {
				      _r3 = fold_build1_loc (loc, NOP_EXPR, utype, _o3[0]);
				    }
				  else
				    _r3 = _o3[0];
				  _o2[0] = _r3;
				}
				_o2[1] = nst;
				_r2 = fold_build2_loc (loc, RSHIFT_EXPR, utype, _o2[0], _o2[1]);
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
			    res_op1 = captures[4];
			    tree _r;
			    _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 642, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail893:;
		      }
		    }
	      }
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_425 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail941;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail941;
  {
    tree _r;
    _r = captures[2];
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 662, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail941:;
  return NULL_TREE;
}

tree
generic_simplify_426 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail942;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail942;
  {
    tree _r;
    _r = captures[2];
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 663, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail942:;
  return NULL_TREE;
}

tree
generic_simplify_428 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (shift))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (TREE_TYPE (captures[0]), TREE_TYPE (captures[1]))
 && TYPE_PRECISION (type) <= HOST_BITS_PER_WIDE_INT
 && tree_fits_uhwi_p (captures[4])
 && tree_to_uhwi (captures[4]) > 0
 && tree_to_uhwi (captures[4]) < TYPE_PRECISION (type)
)
    {
      {
 unsigned int shiftc = tree_to_uhwi (captures[4]);
 unsigned HOST_WIDE_INT mask = TREE_INT_CST_LOW (captures[5]);
 unsigned HOST_WIDE_INT newmask, zerobits = 0;
 tree shift_type = TREE_TYPE (captures[2]);
 unsigned int prec;
 if (shift == LSHIFT_EXPR)
 zerobits = ((HOST_WIDE_INT_1U << shiftc) - 1);
 else if (shift == RSHIFT_EXPR
 && type_has_mode_precision_p (shift_type))
 {
 prec = TYPE_PRECISION (TREE_TYPE (captures[2]));
 tree arg00 = captures[3];
 if (captures[2] != captures[3]
 && TYPE_UNSIGNED (TREE_TYPE (captures[3])))
 {
 tree inner_type = TREE_TYPE (captures[3]);
 if (type_has_mode_precision_p (inner_type)
 && TYPE_PRECISION (inner_type) < prec)
 {
 prec = TYPE_PRECISION (inner_type);
 if (shiftc < prec)
 shift_type = inner_type;
 }
 }
 zerobits = HOST_WIDE_INT_M1U;
 if (shiftc < prec)
 {
 zerobits >>= HOST_BITS_PER_WIDE_INT - shiftc;
 zerobits <<= prec - shiftc;
 }
 if (!TYPE_UNSIGNED (TREE_TYPE (captures[2]))
 && prec == TYPE_PRECISION (TREE_TYPE (captures[2])))
 {
 if ((mask & zerobits) == 0)
 shift_type = unsigned_type_for (TREE_TYPE (captures[2]));
 else
 zerobits = 0;
 }
 }
	  if ((mask & zerobits) == mask
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail944;
	      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail944;
	      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail944;
	      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail944;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail944;
	      {
		tree _r;
		_r =  build_int_cst (type, 0);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 638, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail944:;
	    }
	  else
	    {
	      {
 newmask = mask | zerobits;
		  if (newmask != mask && (newmask & (newmask + 1)) == 0
)
		    {
		      {
 for (prec = BITS_PER_UNIT;
 prec < HOST_BITS_PER_WIDE_INT; prec <<= 1)
 if (newmask == (HOST_WIDE_INT_1U << prec) - 1)
 break;
			  if (prec < HOST_BITS_PER_WIDE_INT
 || newmask == HOST_WIDE_INT_M1U
)
			    {
			      {
 tree newmaskt = build_int_cst_type (TREE_TYPE (captures[5]), newmask);
				  if (!tree_int_cst_equal (newmaskt, captures[5])
)
				    {
				      if (shift_type != TREE_TYPE (captures[2])
)
					{
					  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail945;
					  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail945;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail945;
					  {
					    tree res_op0;
					    {
					      tree _o1[1], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[2];
						  if (TREE_TYPE (_o3[0]) != shift_type)
						    {
						      _r3 = fold_build1_loc (loc, NOP_EXPR, shift_type, _o3[0]);
						    }
						  else
						    _r3 = _o3[0];
						  _o2[0] = _r3;
						}
						_o2[1] = captures[4];
						_r2 = fold_build2_loc (loc, shift, shift_type, _o2[0], _o2[1]);
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
					    res_op1 =  newmaskt;
					    tree _r;
					    _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 639, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail945:;
					}
				      else
					{
					  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail946;
					  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail946;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail946;
					  {
					    tree res_op0;
					    res_op0 = captures[0];
					    tree res_op1;
					    res_op1 =  newmaskt;
					    tree _r;
					    _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 640, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail946:;
					}
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
generic_simplify_449 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!HONOR_SIGN_DEPENDENT_ROUNDING (type)
 && !HONOR_SIGNED_ZEROS (type)
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail971;
      {
	tree res_op0;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[1];
	  _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	  res_op0 = _r1;
	}
	tree res_op1;
	res_op1 = captures[0];
	tree _r;
	_r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 683, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail971:;
    }
  return NULL_TREE;
}

tree
generic_simplify_456 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (minmax))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail979;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail979;
  {
    tree _r;
    _r = captures[0];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 691, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail979:;
  return NULL_TREE;
}

tree
generic_simplify_463 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (minmax),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 poly_int64 off0, off1;
 tree base0, base1;
 int equal = address_compare (cmp, TREE_TYPE (captures[0]), captures[1], captures[3], base0, base1,
 off0, off1,
1
);
      if (equal == 1
)
	{
	  if (minmax == MIN_EXPR
)
	    {
	      if (known_le (off0, off1)
)
		{
		  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail995;
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail995;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail995;
		  {
		    tree _r;
		    _r = captures[0];
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 694, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail995:;
		}
	      else
		{
		  if (known_gt (off0, off1)
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail996;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail996;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail996;
		      {
			tree _r;
			_r = captures[2];
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 695, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail996:;
		    }
		}
	    }
	  else
	    {
	      if (known_ge (off0, off1)
)
		{
		  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail997;
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail997;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail997;
		  {
		    tree _r;
		    _r = captures[0];
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 696, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail997:;
		}
	      else
		{
		  if (known_lt (off0, off1)
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail998;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail998;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail998;
		      {
			tree _r;
			_r = captures[2];
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 697, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail998:;
		    }
		}
	    }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_481 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1021;
  if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1021;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1021;
  {
    tree _r;
    _r = captures[2];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 720, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1021:;
  return NULL_TREE;
}

tree
generic_simplify_485 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (bitwise_equal_p (captures[0], captures[2])
 && tree_expr_nonzero_p (captures[3])
 && expr_no_side_effects_p (captures[2])
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1025;
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1025;
      if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1025;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1025;
      {
	tree _r;
	_r = captures[1];
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 724, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1025:;
    }
  return NULL_TREE;
}

tree
generic_simplify_491 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (neeq),
 const combined_fn ARG_UNUSED (sign))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (neeq == NE_EXPR
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1031;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1031;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree _r;
	_r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 730, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1031:;
    }
  else
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1032;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1032;
      {
	tree res_op0;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  _r1 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	  res_op0 = _r1;
	}
	tree _r;
	_r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 731, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1032:;
    }
  return NULL_TREE;
}

tree
generic_simplify_503 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop),
 const combined_fn ARG_UNUSED (bswap))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1048;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    {
      tree _o1[1], _r1;
      _o1[0] = captures[1];
      _r1 = maybe_build_call_expr_loc (loc, bswap, TREE_TYPE (_o1[0]), 1, _o1[0]);
      if (!_r1)
        goto next_after_fail1048;
      res_op1 = _r1;
    }
    tree _r;
    _r = fold_build2_loc (loc, bitop, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 746, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1048:;
  return NULL_TREE;
}

tree
generic_simplify_509 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (cond_op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree op_type = TREE_TYPE (captures[3]);
      if (element_precision (type) == element_precision (op_type)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1054;
	  {
	    tree res_op0;
	    {
	      tree _o1[4], _r1;
	      _o1[0] = captures[0];
	      _o1[1] = captures[1];
	      _o1[2] = captures[2];
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[4];
		_r2 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, op_type, _o2[0]);
		_o1[3] = _r2;
	      }
	      _r1 = maybe_build_call_expr_loc (loc, cond_op, TREE_TYPE (_o1[1]), 4, _o1[0], _o1[1], _o1[2], _o1[3]);
	      if (!_r1)
	        goto next_after_fail1054;
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
	    if (TREE_SIDE_EFFECTS (captures[3]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 747, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1054:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_517 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (cond_op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1062;
  {
    tree res_op0;
    {
      tree _o1[2], _r1;
      _o1[0] = captures[2];
      _o1[1] = captures[0];
      _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      res_op0 = _r1;
    }
    tree res_op1;
    res_op1 = captures[3];
    tree res_op2;
    res_op2 = captures[4];
    tree res_op3;
    res_op3 = captures[5];
    tree _r;
    _r = maybe_build_call_expr_loc (loc, cond_op, type, 4, res_op0, res_op1, res_op2, res_op3);
    if (!_r)
      goto next_after_fail1062;
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 755, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1062:;
  return NULL_TREE;
}

tree
generic_simplify_524 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (logs))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations
)
    {
      if (! HONOR_SIGN_DEPENDENT_ROUNDING (type)
 && ! HONOR_NANS (type) && ! HONOR_INFINITIES (type)
 && ! flag_trapping_math
 && ! flag_errno_math
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1069;
	  {
	    tree res_op0;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[2];
	      _r1 = maybe_build_call_expr_loc (loc, logs, TREE_TYPE (_o1[0]), 1, _o1[0]);
	      if (!_r1)
	        goto next_after_fail1069;
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 762, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1069:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_536 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1081;
  {
    tree _r;
    _r = captures[0];
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 774, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1081:;
  return NULL_TREE;
}

tree
generic_simplify_544 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (SIGNBIT))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1089;
  {
    tree _r;
    _r =  integer_zero_node;
    if (TREE_SIDE_EFFECTS (captures[0]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 782, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1089:;
  return NULL_TREE;
}

tree
generic_simplify_549 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (canonicalize_math_after_vectorization_p ()
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1094;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 = captures[1];
	tree res_op2;
	res_op2 = captures[2];
	tree _r;
	_r = maybe_build_call_expr_loc (loc, CFN_FNMS, type, 3, res_op0, res_op1, res_op2);
	if (!_r)
	  goto next_after_fail1094;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 787, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1094:;
    }
  return NULL_TREE;
}

tree
generic_simplify_558 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (ctz))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree t = TREE_TYPE (captures[0]);
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1103;
      {
	tree res_op0;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[1];
	  if (TREE_TYPE (_o1[0]) != t)
	    {
	      _r1 = fold_build1_loc (loc, NOP_EXPR, t, _o1[0]);
	    }
	  else
	    _r1 = _o1[0];
	  res_op0 = _r1;
	}
	tree _r;
	_r = maybe_build_call_expr_loc (loc, ctz, type, 1, res_op0);
	if (!_r)
	  goto next_after_fail1103;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 793, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1103:;
  }
  return NULL_TREE;
}

tree
generic_simplify_567 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (bswap),
 const combined_fn ARG_UNUSED (parity))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_PRECISION (TREE_TYPE (captures[0]))
 >= TYPE_PRECISION (TREE_TYPE (captures[1]))
)
    {
      {
 tree type0 = TREE_TYPE (captures[0]);
 tree type1 = TREE_TYPE (captures[1]);
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1113;
	  {
	    tree res_op0;
	    {
	      tree _o1[1], _r1;
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[2];
		if (TREE_TYPE (_o2[0]) != type1)
		  {
		    _r2 = fold_build1_loc (loc, NOP_EXPR, type1, _o2[0]);
		  }
		else
		  _r2 = _o2[0];
		_o1[0] = _r2;
	      }
	      if (TREE_TYPE (_o1[0]) != type0)
		{
		  _r1 = fold_build1_loc (loc, NOP_EXPR, type0, _o1[0]);
		}
	      else
	        _r1 = _o1[0];
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = maybe_build_call_expr_loc (loc, parity, type, 1, res_op0);
	    if (!_r)
	      goto next_after_fail1113;
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 799, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1113:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_NEGATE_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case PLUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (tree_negate_expr_p (_q21))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_449 (loc, type, _p0, captures);
	      if (res) return res;
	    }
	  }
	if (tree_negate_expr_p (_q20))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_449 (loc, type, _p0, captures);
	      if (res) return res;
	    }
	  }
        break;
      }
    case MINUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	  if ((ANY_INTEGRAL_TYPE_P (type) && !TYPE_OVERFLOW_SANITIZED (type))
 || (FLOAT_TYPE_P (type)
 && !HONOR_SIGN_DEPENDENT_ROUNDING (type)
 && !HONOR_SIGNED_ZEROS (type))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1159;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		res_op1 = captures[0];
		tree _r;
		_r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 836, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1159:;
	    }
	}
        break;
      }
    case POINTER_DIFF_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	  if (TYPE_OVERFLOW_UNDEFINED (type)
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1160;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		res_op1 = captures[0];
		tree _r;
		_r = fold_build2_loc (loc, POINTER_DIFF_EXPR, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 837, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1160:;
	    }
	}
        break;
      }
    case MULT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (tree_negate_expr_p (_q21))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_450 (loc, type, _p0, captures);
	      if (res) return res;
	    }
	  }
	if (tree_negate_expr_p (_q20))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
	      tree res = generic_simplify_450 (loc, type, _p0, captures);
	      if (res) return res;
	    }
	  }
        break;
      }
    case RDIV_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (tree_negate_expr_p (_q21))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      if (! HONOR_SIGN_DEPENDENT_ROUNDING (type)
 && single_use (captures[0])
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1161;
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
		    _r = fold_build2_loc (loc, RDIV_EXPR, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 838, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1161:;
		}
	    }
	  }
	if (tree_negate_expr_p (_q20))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      if (! HONOR_SIGN_DEPENDENT_ROUNDING (type)
 && single_use (captures[0])
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1162;
		  {
		    tree res_op0;
		    {
		      tree _o1[1], _r1;
		      _o1[0] = captures[1];
		      _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
		      res_op0 = _r1;
		    }
		    tree res_op1;
		    res_op1 = captures[2];
		    tree _r;
		    _r = fold_build2_loc (loc, RDIV_EXPR, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 839, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1162:;
		}
	    }
	  }
        break;
      }
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case RSHIFT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_q31))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
		      tree res = generic_simplify_451 (loc, type, _p0, captures);
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
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      {
		tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
		if (!TYPE_SATURATING (type)
)
		  {
		    if (INTEGRAL_TYPE_P (type)
 && (TYPE_PRECISION (type) <= TYPE_PRECISION (TREE_TYPE (captures[0]))
 || (!TYPE_UNSIGNED (TREE_TYPE (captures[0]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))))
 && !TYPE_OVERFLOW_SANITIZED (type)
 && !TYPE_OVERFLOW_SANITIZED (TREE_TYPE (captures[0]))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1163;
			{
			  tree res_op0;
			  res_op0 = captures[0];
			  tree _r;
			  _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 840, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1163:;
		      }
		  }
	      }
	      break;
	    }
          default:;
          }
      {
	tree _q20_pops[1];
	if (tree_logical_inverted_value (_q20, _q20_pops))
	  {
	    tree _q30 = _q20_pops[0];
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q30 };
	      if (INTEGRAL_TYPE_P (type)
 && TREE_CODE (type) != BOOLEAN_TYPE
 && TYPE_PRECISION (type) > 1
 && TREE_CODE (captures[2]) == SSA_NAME
 && ssa_name_has_boolean_range (captures[2])
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1164;
		  {
		    tree res_op0;
		    {
		      tree _o1[1], _r1;
		      _o1[0] = captures[2];
		      if (TREE_TYPE (_o1[0]) != type)
			{
			  _r1 = fold_build1_loc (loc, NOP_EXPR, type, _o1[0]);
			}
		      else
		        _r1 = _o1[0];
		      res_op0 = _r1;
		    }
		    tree res_op1;
		    res_op1 =  build_all_ones_cst (type);
		    tree _r;
		    _r = fold_build2_loc (loc, PLUS_EXPR, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 841, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1164:;
		}
	    }
	  }
      }
      if (tree_negate_expr_p (_q20))
	{
	  {
	    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	    if (!TYPE_SATURATING (type)
)
	      {
		if (SCALAR_FLOAT_TYPE_P (type)
 && ((DECIMAL_FLOAT_TYPE_P (type)
 == DECIMAL_FLOAT_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_PRECISION (type) >= TYPE_PRECISION (TREE_TYPE (captures[0])))
 || !HONOR_SIGN_DEPENDENT_ROUNDING (type))
)
		  {
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1165;
		    {
		      tree res_op0;
		      {
			tree _o1[1], _r1;
			_o1[0] = captures[0];
			_r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
			res_op0 = _r1;
		      }
		      tree _r;
		      _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 842, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1165:;
		  }
	      }
	  }
        }
        break;
      }
    case RSHIFT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		tree res = generic_simplify_451 (loc, type, _p0, captures);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
        break;
      }
    case BIT_NOT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	  tree res = generic_simplify_452 (loc, type, _p0, captures);
	  if (res) return res;
	}
        break;
      }
    case NEGATE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	  tree res = generic_simplify_453 (loc, type, _p0, captures);
	  if (res) return res;
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1166;
	  {
	    tree res_op0;
	    res_op0 = captures[1];
	    tree res_op1;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[2];
	      _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	      if (EXPR_P (_r1))
	        goto next_after_fail1166;
	      res_op1 = _r1;
	    }
	    tree res_op2;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[3];
	      _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	      if (EXPR_P (_r1))
	        goto next_after_fail1166;
	      res_op2 = _r1;
	    }
	    tree _r;
	    _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 808, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1166:;
	}
        break;
      }
    case CALL_EXPR:
      switch (get_call_combined_fn (_p0))
        {
	case CFN_BUILT_IN_FMA:
	  if (call_expr_nargs (_p0) == 3)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      tree _q22 = CALL_EXPR_ARG (_p0, 2);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q22 };
		tree res = generic_simplify_454 (loc, type, _p0, captures, CFN_BUILT_IN_FMA);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_BUILT_IN_FMAF:
	  if (call_expr_nargs (_p0) == 3)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      tree _q22 = CALL_EXPR_ARG (_p0, 2);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q22 };
		tree res = generic_simplify_454 (loc, type, _p0, captures, CFN_BUILT_IN_FMAF);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_BUILT_IN_FMAL:
	  if (call_expr_nargs (_p0) == 3)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      tree _q22 = CALL_EXPR_ARG (_p0, 2);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q22 };
		tree res = generic_simplify_454 (loc, type, _p0, captures, CFN_BUILT_IN_FMAL);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_FMA:
	  if (call_expr_nargs (_p0) == 3)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      tree _q22 = CALL_EXPR_ARG (_p0, 2);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q22 };
		tree res = generic_simplify_454 (loc, type, _p0, captures, CFN_FMA);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_FMS:
	  if (call_expr_nargs (_p0) == 3)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      tree _q22 = CALL_EXPR_ARG (_p0, 2);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q22 };
		if (canonicalize_math_after_vectorization_p ()
)
		  {
		    if (single_use (captures[0])
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1167;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  res_op1 = captures[2];
			  tree res_op2;
			  res_op2 = captures[3];
			  tree _r;
			  _r = maybe_build_call_expr_loc (loc, CFN_FNMA, type, 3, res_op0, res_op1, res_op2);
			  if (!_r)
			    goto next_after_fail1167;
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 843, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1167:;
		      }
		  }
	      }
	    }
	  break;
	case CFN_FNMA:
	  if (call_expr_nargs (_p0) == 3)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      tree _q22 = CALL_EXPR_ARG (_p0, 2);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q22 };
		if (canonicalize_math_after_vectorization_p ()
)
		  {
		    if (single_use (captures[0])
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1168;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  res_op1 = captures[2];
			  tree res_op2;
			  res_op2 = captures[3];
			  tree _r;
			  _r = maybe_build_call_expr_loc (loc, CFN_FMS, type, 3, res_op0, res_op1, res_op2);
			  if (!_r)
			    goto next_after_fail1168;
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 844, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1168:;
		      }
		  }
	      }
	    }
	  break;
	case CFN_FNMS:
	  if (call_expr_nargs (_p0) == 3)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      tree _q22 = CALL_EXPR_ARG (_p0, 2);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q22 };
		if (canonicalize_math_after_vectorization_p ()
)
		  {
		    if (single_use (captures[0])
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1169;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  res_op1 = captures[2];
			  tree res_op2;
			  res_op2 = captures[3];
			  tree _r;
			  _r = maybe_build_call_expr_loc (loc, CFN_FMA, type, 3, res_op0, res_op1, res_op2);
			  if (!_r)
			    goto next_after_fail1169;
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 845, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1169:;
		      }
		  }
	      }
	    }
	  break;
	default:;
        }
      break;
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
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
	      tree res = generic_simplify_452 (loc, type, _p0, captures);
	      if (res) return res;
	    }
	    break;
	  }
	case NEGATE_EXPR:
	  {
	    tree _q30 = TREE_OPERAND (_q20, 0);
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
	      tree res = generic_simplify_453 (loc, type, _p0, captures);
	      if (res) return res;
	    }
	    break;
	  }
        default:;
        }
    }
}
{
  tree _p0_pops[1];
  if (tree_logical_inverted_value (_p0, _p0_pops))
    {
      tree _q20 = _p0_pops[0];
      {
	tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _q20 };
	if (INTEGRAL_TYPE_P (type)
 && TREE_CODE (type) != BOOLEAN_TYPE
 && TYPE_PRECISION (type) > 1
 && TREE_CODE (captures[2]) == SSA_NAME
 && ssa_name_has_boolean_range (captures[2])
)
	  {
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1170;
	    {
	      tree res_op0;
	      {
		tree _o1[1], _r1;
		_o1[0] = captures[2];
		if (TREE_TYPE (_o1[0]) != type)
		  {
		    _r1 = fold_build1_loc (loc, NOP_EXPR, type, _o1[0]);
		  }
		else
		  _r1 = _o1[0];
		res_op0 = _r1;
	      }
	      tree res_op1;
	      res_op1 =  build_all_ones_cst (type);
	      tree _r;
	      _r = fold_build2_loc (loc, PLUS_EXPR, type, res_op0, res_op1);
	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 841, __FILE__, __LINE__, true);
	      return _r;
	    }
next_after_fail1170:;
	  }
      }
    }
}
  {
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    tree res = generic_simplify_377 (loc, type, _p0, captures);
    if (res) return res;
  }
  return NULL_TREE;
}

tree
generic_simplify_MULT_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p1))
    {
    case VECTOR_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (initializer_each_zero_or_onep (captures[1])
 && !HONOR_SNANS (type)
 && !HONOR_SIGNED_ZEROS (type)
)
	    {
	      {
 tree itype = FLOAT_TYPE_P (type) ? unsigned_type_for (type) : type;
		  if (itype
 && (!VECTOR_MODE_P (TYPE_MODE (type))
 || (VECTOR_MODE_P (TYPE_MODE (itype))
 && optab_handler (and_optab,
 TYPE_MODE (itype)) != CODE_FOR_nothing))
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1411;
		      {
			tree res_op0;
			{
			  tree _o1[2], _r1;
			  {
			    tree _o2[1], _r2;
			    _o2[0] = captures[0];
			    _r2 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, itype, _o2[0]);
			    _o1[0] = _r2;
			  }
			  {
			    tree _o2[2], _r2;
			    _o2[0] = captures[1];
			    _o2[1] =  build_zero_cst (type);
			    _r2 = fold_build2_loc (loc, NE_EXPR, itype, _o2[0], _o2[1]);
			    _o1[1] = _r2;
			  }
			  _r1 = fold_build2_loc (loc, BIT_AND_EXPR, itype, _o1[0], _o1[1]);
			  res_op0 = _r1;
			}
			tree _r;
			_r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 929, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1411:;
		    }
	      }
	    }
	}
        break;
      }
    default:;
    }
if (integer_zerop (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1412;
      {
	tree _r;
	_r = captures[1];
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 930, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1412:;
    }
  }
if (real_zerop (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (!tree_expr_maybe_nan_p (captures[0])
 && (!HONOR_NANS (type) || !tree_expr_maybe_infinite_p (captures[0]))
 && (!HONOR_SIGNED_ZEROS (type) || tree_expr_nonnegative_p (captures[0]))
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1413;
	  {
	    tree _r;
	    _r = captures[1];
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 931, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1413:;
	}
    }
  }
if (real_onep (_p1))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (!tree_expr_maybe_signaling_nan_p (captures[0])
 && (!HONOR_SIGNED_ZEROS (type)
 || !COMPLEX_FLOAT_TYPE_P (type))
)
	{
	  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1414;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1414;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree _r;
	    _r = non_lvalue_loc (loc, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 932, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1414:;
	}
    }
  }
if (real_minus_onep (_p1))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (!tree_expr_maybe_signaling_nan_p (captures[0])
 && (!HONOR_SIGNED_ZEROS (type)
 || !COMPLEX_FLOAT_TYPE_P (type))
)
	{
	  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1415;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1415;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree _r;
	    _r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 933, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1415:;
	}
    }
  }
  switch (TREE_CODE (_p0))
    {
    case BIT_AND_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  case RSHIFT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (uniform_integer_cst_p (_q31))
		{
		  if (uniform_integer_cst_p (_q21))
		    {
		      if (uniform_integer_cst_p (_p1))
			{
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q21, _p1 };
			    {
 tree rshift_cst = uniform_integer_cst_p (captures[1]);
 tree bit_and_cst = uniform_integer_cst_p (captures[2]);
 tree mult_cst = uniform_integer_cst_p (captures[3]);
				if (VECTOR_TYPE_P (type)
 && tree_fits_uhwi_p (rshift_cst)
 && tree_fits_uhwi_p (mult_cst)
 && tree_fits_uhwi_p (bit_and_cst)
)
				  {
				    {
 HOST_WIDE_INT vec_elem_bits = vector_element_bits (type);
 poly_int64 vec_nelts = TYPE_VECTOR_SUBPARTS (type);
 poly_int64 vec_bits = vec_elem_bits * vec_nelts;
 unsigned HOST_WIDE_INT cmp_bits_i, bit_and_i, mult_i;
 unsigned HOST_WIDE_INT target_mult_i, target_bit_and_i;
 cmp_bits_i = tree_to_uhwi (rshift_cst) + 1;
 mult_i = tree_to_uhwi (mult_cst);
 target_mult_i = (HOST_WIDE_INT_1U << cmp_bits_i) - 1;
 bit_and_i = tree_to_uhwi (bit_and_cst);
 target_bit_and_i = 0;
 for (unsigned i = 0; i < vec_elem_bits / cmp_bits_i; i++)
 target_bit_and_i = (target_bit_and_i << cmp_bits_i) | 1U;
					if ((exact_log2 (cmp_bits_i)) >= 0
 && cmp_bits_i < HOST_BITS_PER_WIDE_INT
 && multiple_p (vec_bits, cmp_bits_i)
 && vec_elem_bits <= HOST_BITS_PER_WIDE_INT
 && target_mult_i == mult_i
 && target_bit_and_i == bit_and_i
)
					  {
					    {
 tree cmp_type = build_nonstandard_integer_type (cmp_bits_i, 0);
 poly_int64 vector_type_nelts = exact_div (vec_bits, cmp_bits_i);
 tree vec_cmp_type = build_vector_type (cmp_type, vector_type_nelts);
 tree vec_truth_type = truth_type_for (vec_cmp_type);
 tree zeros = build_zero_cst (vec_cmp_type);
 tree ones = build_all_ones_cst (vec_cmp_type);
						if (expand_vec_cmp_expr_p (vec_cmp_type, vec_truth_type, LT_EXPR)
 && expand_vec_cond_expr_p (vec_cmp_type, vec_truth_type, LT_EXPR)
)
						  {
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1416;
						    {
						      tree res_op0;
						      {
							tree _o1[3], _r1;
							{
							  tree _o2[2], _r2;
							  {
							    tree _o3[1], _r3;
							    _o3[0] = captures[0];
							    _r3 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, vec_cmp_type, _o3[0]);
							    _o2[0] = _r3;
							  }
							  _o2[1] =  zeros;
							  _r2 = fold_build2_loc (loc, LT_EXPR, vec_truth_type, _o2[0], _o2[1]);
							  _o1[0] = _r2;
							}
							_o1[1] =  ones;
							_o1[2] =  zeros;
							_r1 = fold_build3_loc (loc, VEC_COND_EXPR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2]);
							res_op0 = _r1;
						      }
						      tree _r;
						      _r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
						      if (TREE_SIDE_EFFECTS (captures[1]))
							_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
						      if (TREE_SIDE_EFFECTS (captures[2]))
							_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
						      if (TREE_SIDE_EFFECTS (captures[3]))
							_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
						      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 934, __FILE__, __LINE__, true);
						      return _r;
						    }
next_after_fail1416:;
						  }
					    }
					  }
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
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case COND_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	tree _q32 = TREE_OPERAND (_p1, 2);
	switch (TREE_CODE (_q30))
	  {
	  case GT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  if (real_zerop (_q41))
		    {
		      if (real_onep (_q31))
			{
			  if (real_minus_onep (_q32))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
				if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
				  {
				    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1417;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1417;
				    {
				      tree res_op0;
				      {
					tree _o1[1], _r1;
					_o1[0] = captures[0];
					_r1 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					res_op0 = _r1;
				      }
				      tree _r;
				      _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 935, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1417:;
				  }
			      }
			    }
		        }
		    }
	        }
	      break;
	    }
	  case GE_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  if (real_zerop (_q41))
		    {
		      if (real_onep (_q31))
			{
			  if (real_minus_onep (_q32))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
				if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
				  {
				    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1418;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1418;
				    {
				      tree res_op0;
				      {
					tree _o1[1], _r1;
					_o1[0] = captures[0];
					_r1 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					res_op0 = _r1;
				      }
				      tree _r;
				      _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 935, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1418:;
				  }
			      }
			    }
		        }
		    }
	        }
	      break;
	    }
	  case LT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  if (real_zerop (_q41))
		    {
		      if (real_onep (_q31))
			{
			  if (real_minus_onep (_q32))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
				if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
				  {
				    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1419;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1419;
				    {
				      tree res_op0;
				      {
					tree _o1[1], _r1;
					_o1[0] = captures[0];
					_r1 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					res_op0 = _r1;
				      }
				      tree _r;
				      _r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 935, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1419:;
				  }
			      }
			    }
		        }
		    }
	        }
	      break;
	    }
	  case LE_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  if (real_zerop (_q41))
		    {
		      if (real_onep (_q31))
			{
			  if (real_minus_onep (_q32))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
				if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
				  {
				    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1420;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1420;
				    {
				      tree res_op0;
				      {
					tree _o1[1], _r1;
					_o1[0] = captures[0];
					_r1 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					res_op0 = _r1;
				      }
				      tree _r;
				      _r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 935, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1420:;
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
  switch (TREE_CODE (_p0))
    {
    case COND_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	tree _q22 = TREE_OPERAND (_p0, 2);
	switch (TREE_CODE (_q20))
	  {
	  case GT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (real_zerop (_q31))
		{
		  if (real_onep (_q21))
		    {
		      if (real_minus_onep (_q22))
			{
			  if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q21 };
				if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
				  {
				    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1421;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1421;
				    {
				      tree res_op0;
				      {
					tree _o1[1], _r1;
					_o1[0] = captures[0];
					_r1 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					res_op0 = _r1;
				      }
				      tree _r;
				      _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 935, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1421:;
				  }
			      }
			    }
		        }
		    }
		  if (real_minus_onep (_q21))
		    {
		      if (real_onep (_q22))
			{
			  if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q22 };
				if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
				  {
				    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1422;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1422;
				    {
				      tree res_op0;
				      {
					tree _o1[1], _r1;
					_o1[0] = captures[0];
					_r1 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					res_op0 = _r1;
				      }
				      tree _r;
				      _r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 936, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1422:;
				  }
			      }
			    }
		        }
		    }
	        }
	      break;
	    }
	  case GE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (real_zerop (_q31))
		{
		  if (real_onep (_q21))
		    {
		      if (real_minus_onep (_q22))
			{
			  if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q21 };
				if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
				  {
				    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1423;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1423;
				    {
				      tree res_op0;
				      {
					tree _o1[1], _r1;
					_o1[0] = captures[0];
					_r1 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					res_op0 = _r1;
				      }
				      tree _r;
				      _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 935, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1423:;
				  }
			      }
			    }
		        }
		    }
		  if (real_minus_onep (_q21))
		    {
		      if (real_onep (_q22))
			{
			  if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q22 };
				if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
				  {
				    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1424;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1424;
				    {
				      tree res_op0;
				      {
					tree _o1[1], _r1;
					_o1[0] = captures[0];
					_r1 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					res_op0 = _r1;
				      }
				      tree _r;
				      _r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 936, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1424:;
				  }
			      }
			    }
		        }
		    }
	        }
	      break;
	    }
	  case LT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (real_zerop (_q31))
		{
		  if (real_onep (_q21))
		    {
		      if (real_minus_onep (_q22))
			{
			  if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q21 };
				if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
				  {
				    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1425;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1425;
				    {
				      tree res_op0;
				      {
					tree _o1[1], _r1;
					_o1[0] = captures[0];
					_r1 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					res_op0 = _r1;
				      }
				      tree _r;
				      _r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 935, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1425:;
				  }
			      }
			    }
		        }
		    }
		  if (real_minus_onep (_q21))
		    {
		      if (real_onep (_q22))
			{
			  if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q22 };
				if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
				  {
				    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1426;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1426;
				    {
				      tree res_op0;
				      {
					tree _o1[1], _r1;
					_o1[0] = captures[0];
					_r1 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					res_op0 = _r1;
				      }
				      tree _r;
				      _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 936, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1426:;
				  }
			      }
			    }
		        }
		    }
	        }
	      break;
	    }
	  case LE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (real_zerop (_q31))
		{
		  if (real_onep (_q21))
		    {
		      if (real_minus_onep (_q22))
			{
			  if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q21 };
				if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
				  {
				    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1427;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1427;
				    {
				      tree res_op0;
				      {
					tree _o1[1], _r1;
					_o1[0] = captures[0];
					_r1 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					res_op0 = _r1;
				      }
				      tree _r;
				      _r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 935, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1427:;
				  }
			      }
			    }
		        }
		    }
		  if (real_minus_onep (_q21))
		    {
		      if (real_onep (_q22))
			{
			  if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q22 };
				if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
				  {
				    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1428;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1428;
				    {
				      tree res_op0;
				      {
					tree _o1[1], _r1;
					_o1[0] = captures[0];
					_r1 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					res_op0 = _r1;
				      }
				      tree _r;
				      _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 936, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1428:;
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
  switch (TREE_CODE (_p1))
    {
    case COND_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	tree _q32 = TREE_OPERAND (_p1, 2);
	switch (TREE_CODE (_q30))
	  {
	  case GT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  if (real_zerop (_q41))
		    {
		      if (real_minus_onep (_q31))
			{
			  if (real_onep (_q32))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q32 };
				if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
				  {
				    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1429;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1429;
				    {
				      tree res_op0;
				      {
					tree _o1[1], _r1;
					_o1[0] = captures[0];
					_r1 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					res_op0 = _r1;
				      }
				      tree _r;
				      _r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 936, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1429:;
				  }
			      }
			    }
		        }
		    }
	        }
	      break;
	    }
	  case GE_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  if (real_zerop (_q41))
		    {
		      if (real_minus_onep (_q31))
			{
			  if (real_onep (_q32))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q32 };
				if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
				  {
				    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1430;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1430;
				    {
				      tree res_op0;
				      {
					tree _o1[1], _r1;
					_o1[0] = captures[0];
					_r1 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					res_op0 = _r1;
				      }
				      tree _r;
				      _r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 936, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1430:;
				  }
			      }
			    }
		        }
		    }
	        }
	      break;
	    }
	  case LT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  if (real_zerop (_q41))
		    {
		      if (real_minus_onep (_q31))
			{
			  if (real_onep (_q32))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q32 };
				if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
				  {
				    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1431;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1431;
				    {
				      tree res_op0;
				      {
					tree _o1[1], _r1;
					_o1[0] = captures[0];
					_r1 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					res_op0 = _r1;
				      }
				      tree _r;
				      _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 936, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1431:;
				  }
			      }
			    }
		        }
		    }
	        }
	      break;
	    }
	  case LE_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  if (real_zerop (_q41))
		    {
		      if (real_minus_onep (_q31))
			{
			  if (real_onep (_q32))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q32 };
				if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
				  {
				    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1432;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1432;
				    {
				      tree res_op0;
				      {
					tree _o1[1], _r1;
					_o1[0] = captures[0];
					_r1 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					res_op0 = _r1;
				      }
				      tree _r;
				      _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 936, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1432:;
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
    case CALL_EXPR:
      switch (get_call_combined_fn (_p1))
        {
	case CFN_BUILT_IN_COPYSIGNF16:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			tree res = generic_simplify_168 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF16);
			if (res) return res;
		      }
		    }
	        }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF32:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			tree res = generic_simplify_168 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF32);
			if (res) return res;
		      }
		    }
	        }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF64:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			tree res = generic_simplify_168 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF64);
			if (res) return res;
		      }
		    }
	        }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF128X:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			tree res = generic_simplify_168 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF128X);
			if (res) return res;
		      }
		    }
	        }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF128:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			tree res = generic_simplify_168 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF128);
			if (res) return res;
		      }
		    }
	        }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF32X:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			tree res = generic_simplify_168 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF32X);
			if (res) return res;
		      }
		    }
	        }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF64X:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			tree res = generic_simplify_168 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF64X);
			if (res) return res;
		      }
		    }
	        }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGN:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			tree res = generic_simplify_168 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGN);
			if (res) return res;
		      }
		    }
	        }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			tree res = generic_simplify_168 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF);
			if (res) return res;
		      }
		    }
	        }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNL:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			tree res = generic_simplify_168 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNL);
			if (res) return res;
		      }
		    }
	        }
	    }
	  break;
	case CFN_COPYSIGN:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			tree res = generic_simplify_168 (loc, type, _p0, _p1, captures, CFN_COPYSIGN);
			if (res) return res;
		      }
		    }
	        }
	    }
	  break;
	default:;
        }
      break;
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case CALL_EXPR:
      switch (get_call_combined_fn (_p0))
        {
	case CFN_BUILT_IN_COPYSIGNF16:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if (real_onep (_q20))
		{
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q21 };
			tree res = generic_simplify_169 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF16);
			if (res) return res;
		      }
		    }
		  switch (TREE_CODE (_q21))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q40 = TREE_OPERAND (_q21, 0);
			if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _q40 };
			      tree res = generic_simplify_170 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF16);
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
	case CFN_BUILT_IN_COPYSIGNF32:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if (real_onep (_q20))
		{
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q21 };
			tree res = generic_simplify_169 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF32);
			if (res) return res;
		      }
		    }
		  switch (TREE_CODE (_q21))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q40 = TREE_OPERAND (_q21, 0);
			if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _q40 };
			      tree res = generic_simplify_170 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF32);
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
	case CFN_BUILT_IN_COPYSIGNF64:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if (real_onep (_q20))
		{
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q21 };
			tree res = generic_simplify_169 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF64);
			if (res) return res;
		      }
		    }
		  switch (TREE_CODE (_q21))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q40 = TREE_OPERAND (_q21, 0);
			if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _q40 };
			      tree res = generic_simplify_170 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF64);
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
	case CFN_BUILT_IN_COPYSIGNF128X:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if (real_onep (_q20))
		{
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q21 };
			tree res = generic_simplify_169 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF128X);
			if (res) return res;
		      }
		    }
		  switch (TREE_CODE (_q21))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q40 = TREE_OPERAND (_q21, 0);
			if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _q40 };
			      tree res = generic_simplify_170 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF128X);
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
	case CFN_BUILT_IN_COPYSIGNF128:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if (real_onep (_q20))
		{
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q21 };
			tree res = generic_simplify_169 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF128);
			if (res) return res;
		      }
		    }
		  switch (TREE_CODE (_q21))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q40 = TREE_OPERAND (_q21, 0);
			if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _q40 };
			      tree res = generic_simplify_170 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF128);
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
	case CFN_BUILT_IN_COPYSIGNF32X:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if (real_onep (_q20))
		{
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q21 };
			tree res = generic_simplify_169 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF32X);
			if (res) return res;
		      }
		    }
		  switch (TREE_CODE (_q21))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q40 = TREE_OPERAND (_q21, 0);
			if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _q40 };
			      tree res = generic_simplify_170 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF32X);
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
	case CFN_BUILT_IN_COPYSIGNF64X:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if (real_onep (_q20))
		{
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q21 };
			tree res = generic_simplify_169 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF64X);
			if (res) return res;
		      }
		    }
		  switch (TREE_CODE (_q21))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q40 = TREE_OPERAND (_q21, 0);
			if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _q40 };
			      tree res = generic_simplify_170 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF64X);
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
	case CFN_BUILT_IN_COPYSIGN:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if (real_onep (_q20))
		{
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q21 };
			tree res = generic_simplify_169 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGN);
			if (res) return res;
		      }
		    }
		  switch (TREE_CODE (_q21))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q40 = TREE_OPERAND (_q21, 0);
			if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _q40 };
			      tree res = generic_simplify_170 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGN);
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
	case CFN_BUILT_IN_COPYSIGNF:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if (real_onep (_q20))
		{
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q21 };
			tree res = generic_simplify_169 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF);
			if (res) return res;
		      }
		    }
		  switch (TREE_CODE (_q21))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q40 = TREE_OPERAND (_q21, 0);
			if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _q40 };
			      tree res = generic_simplify_170 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF);
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
	case CFN_BUILT_IN_COPYSIGNL:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if (real_onep (_q20))
		{
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q21 };
			tree res = generic_simplify_169 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNL);
			if (res) return res;
		      }
		    }
		  switch (TREE_CODE (_q21))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q40 = TREE_OPERAND (_q21, 0);
			if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _q40 };
			      tree res = generic_simplify_170 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNL);
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
	case CFN_COPYSIGN:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if (real_onep (_q20))
		{
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q21 };
			tree res = generic_simplify_169 (loc, type, _p0, _p1, captures, CFN_COPYSIGN);
			if (res) return res;
		      }
		    }
		  switch (TREE_CODE (_q21))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q40 = TREE_OPERAND (_q21, 0);
			if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _q40 };
			      tree res = generic_simplify_170 (loc, type, _p0, _p1, captures, CFN_COPYSIGN);
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
	default:;
        }
      break;
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case CALL_EXPR:
      switch (get_call_combined_fn (_p1))
        {
	case CFN_BUILT_IN_COPYSIGNF16:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  switch (TREE_CODE (_q31))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q50 = TREE_OPERAND (_q31, 0);
			if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			      tree res = generic_simplify_171 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF16);
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
	case CFN_BUILT_IN_COPYSIGNF32:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  switch (TREE_CODE (_q31))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q50 = TREE_OPERAND (_q31, 0);
			if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			      tree res = generic_simplify_171 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF32);
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
	case CFN_BUILT_IN_COPYSIGNF64:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  switch (TREE_CODE (_q31))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q50 = TREE_OPERAND (_q31, 0);
			if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			      tree res = generic_simplify_171 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF64);
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
	case CFN_BUILT_IN_COPYSIGNF128X:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  switch (TREE_CODE (_q31))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q50 = TREE_OPERAND (_q31, 0);
			if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			      tree res = generic_simplify_171 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF128X);
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
	case CFN_BUILT_IN_COPYSIGNF128:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  switch (TREE_CODE (_q31))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q50 = TREE_OPERAND (_q31, 0);
			if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			      tree res = generic_simplify_171 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF128);
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
	case CFN_BUILT_IN_COPYSIGNF32X:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  switch (TREE_CODE (_q31))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q50 = TREE_OPERAND (_q31, 0);
			if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			      tree res = generic_simplify_171 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF32X);
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
	case CFN_BUILT_IN_COPYSIGNF64X:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  switch (TREE_CODE (_q31))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q50 = TREE_OPERAND (_q31, 0);
			if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			      tree res = generic_simplify_171 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF64X);
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
	case CFN_BUILT_IN_COPYSIGN:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  switch (TREE_CODE (_q31))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q50 = TREE_OPERAND (_q31, 0);
			if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			      tree res = generic_simplify_171 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGN);
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
	case CFN_BUILT_IN_COPYSIGNF:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  switch (TREE_CODE (_q31))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q50 = TREE_OPERAND (_q31, 0);
			if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			      tree res = generic_simplify_171 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF);
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
	case CFN_BUILT_IN_COPYSIGNL:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  switch (TREE_CODE (_q31))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q50 = TREE_OPERAND (_q31, 0);
			if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			      tree res = generic_simplify_171 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNL);
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
	case CFN_COPYSIGN:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if (real_onep (_q30))
		{
		  switch (TREE_CODE (_q31))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q50 = TREE_OPERAND (_q31, 0);
			if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
			  {
			    {
			      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			      tree res = generic_simplify_171 (loc, type, _p0, _p1, captures, CFN_COPYSIGN);
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
	default:;
        }
      break;
    default:;
    }
if (integer_onep (_p1))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      tree res = generic_simplify_172 (loc, type, _p0, _p1, captures, MULT_EXPR);
      if (res) return res;
    }
  }
  switch (TREE_CODE (_p0))
    {
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
		      {
 wi::overflow_type overflow;
 wide_int mul = wi::mul (wi::to_wide (captures[1]), wi::to_wide (captures[2]),
 TYPE_SIGN (type), &overflow);
			  if (!overflow || TYPE_OVERFLOW_WRAPS (type)
)
			    {
			      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1433;
			      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1433;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1433;
			      {
				tree res_op0;
				res_op0 = captures[0];
				tree res_op1;
				res_op1 =  wide_int_to_tree (type, mul);
				tree _r;
				_r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 937, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail1433:;
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
    case PLUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  case MULT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
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
				  {
 bool overflowed = true;
 wi::overflow_type ovf1, ovf2;
 wide_int mul = wi::mul (wi::to_wide (captures[3]), wi::to_wide (captures[5]),
 TYPE_SIGN (type), &ovf1);
 wide_int add = wi::mul (wi::to_wide (captures[4]), wi::to_wide (captures[5]),
 TYPE_SIGN (type), &ovf2);
 if (TYPE_OVERFLOW_UNDEFINED (type))
 {
 }
 else
 overflowed = false;
				      if (!overflowed
)
					{
					  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1434;
					  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1434;
					  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail1434;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1434;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      _o1[0] = captures[2];
					      _o1[1] =  wide_int_to_tree (type, mul);
					      _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree res_op1;
					    res_op1 =  wide_int_to_tree (type, add);
					    tree _r;
					    _r = fold_build2_loc (loc, PLUS_EXPR, type, res_op0, res_op1);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 938, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1434:;
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
    case TRUNC_DIV_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_pow2p (_q21))
	  {
	    if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) && TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1435;
		      {
			tree res_op0;
			res_op0 = captures[0];
			tree res_op1;
			{
			  tree _o1[1], _r1;
			  _o1[0] = captures[1];
			  _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
			  res_op1 = _r1;
			}
			tree _r;
			_r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 939, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1435:;
		    }
		}
	      }
	  }
        break;
      }
    case ABS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1436;
	      {
		if (! tree_invariant_p (captures[1])) goto next_after_fail1436;
		tree res_op0;
		res_op0 = unshare_expr (captures[1]);
		tree res_op1;
		res_op1 = captures[1];
		tree _r;
		_r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 940, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1436:;
	    }
	  }
        break;
      }
    case ABSU_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20 };
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1437;
	      {
		tree res_op0;
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[1];
		  if (TREE_TYPE (_o1[0]) != type)
		    {
		      _r1 = fold_build1_loc (loc, NOP_EXPR, type, _o1[0]);
		    }
		  else
		    _r1 = _o1[0];
		  captures[2] = _r1;
		}
		res_op0 = unshare_expr (captures[2]);
		tree res_op1;
		res_op1 = captures[2];
		tree _r;
		_r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 941, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1437:;
	    }
	  }
        break;
      }
    case CALL_EXPR:
      switch (get_call_combined_fn (_p0))
        {
	case CFN_BUILT_IN_COPYSIGNF16:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		    tree res = generic_simplify_173 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF16);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF32:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		    tree res = generic_simplify_173 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF32);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF64:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		    tree res = generic_simplify_173 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF64);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF128X:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		    tree res = generic_simplify_173 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF128X);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF128:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		    tree res = generic_simplify_173 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF128);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF32X:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		    tree res = generic_simplify_173 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF32X);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF64X:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		    tree res = generic_simplify_173 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF64X);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGN:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		    tree res = generic_simplify_173 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGN);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		    tree res = generic_simplify_173 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNF);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNL:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		    tree res = generic_simplify_173 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COPYSIGNL);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COPYSIGN:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		    tree res = generic_simplify_173 (loc, type, _p0, _p1, captures, CFN_COPYSIGN);
		    if (res) return res;
		  }
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
    CASE_CONVERT:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	switch (TREE_CODE (_q30))
	  {
	  case LSHIFT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if (integer_onep (_q40))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q40, _q41 };
		    tree res = generic_simplify_174 (loc, type, _p0, _p1, captures);
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
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case LSHIFT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_onep (_q30))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q30, _q31 };
		    tree res = generic_simplify_174 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case EQ_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_zerop (_q31))
		{
		  if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
			tree res = generic_simplify_175 (loc, type, _p0, _p1, captures);
			if (res) return res;
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
  switch (TREE_CODE (_p1))
    {
    case LSHIFT_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if (integer_onep (_q30))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q31 };
	      tree res = generic_simplify_174 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case LSHIFT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_onep (_q20))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q21 };
	      tree res = generic_simplify_174 (loc, type, _p0, _p1, captures);
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
	tree _q30 = TREE_OPERAND (_p1, 0);
	switch (TREE_CODE (_q30))
	  {
	  case EQ_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  if (integer_zerop (_q41))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			tree res = generic_simplify_176 (loc, type, _p0, _p1, captures);
			if (res) return res;
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
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    if (integer_zerop (_q31))
	      {
		{
		  tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
		  tree res = generic_simplify_176 (loc, type, _p0, _p1, captures);
		  if (res) return res;
		}
	      }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case EQ_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_q21))
	  {
	    if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	      {
		{
		  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		  tree res = generic_simplify_175 (loc, type, _p0, _p1, captures);
		  if (res) return res;
		}
	      }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case VEC_COND_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	tree _q32 = TREE_OPERAND (_p1, 2);
	switch (TREE_CODE (_q30))
	  {
	  case EQ_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  if (integer_zerop (_q41))
		    {
		      if (integer_zerop (_q32))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1438;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1438;
			    {
			      tree _r;
			      _r =  build_zero_cst (type);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 942, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1438:;
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
  switch (TREE_CODE (_p0))
    {
    case VEC_COND_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	tree _q22 = TREE_OPERAND (_p0, 2);
	switch (TREE_CODE (_q20))
	  {
	  case EQ_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_zerop (_q31))
		{
		  if (integer_zerop (_q22))
		    {
		      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q21 };
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1439;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1439;
			    {
			      tree _r;
			      _r =  build_zero_cst (type);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 942, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1439:;
			  }
		        }
		    }
	        }
	      break;
	    }
	  case NE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_zerop (_q31))
		{
		  if (integer_zerop (_q21))
		    {
		      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q22 };
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1440;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1440;
			    {
			      tree _r;
			      _r =  build_zero_cst (type);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 943, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1440:;
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
  switch (TREE_CODE (_p1))
    {
    case VEC_COND_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	tree _q32 = TREE_OPERAND (_p1, 2);
	switch (TREE_CODE (_q30))
	  {
	  case NE_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  if (integer_zerop (_q41))
		    {
		      if (integer_zerop (_q31))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q32 };
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1441;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1441;
			    {
			      tree _r;
			      _r =  build_zero_cst (type);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 943, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1441:;
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
  switch (TREE_CODE (_p0))
    {
    case RDIV_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  case REAL_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case REAL_CST:
		  {
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
		      if (flag_associative_math
 && single_use (captures[0])
)
			{
			  {
 tree tem = const_binop (MULT_EXPR, type, captures[1], captures[3]);
			      if (tem
)
				{
				  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1442;
				  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1442;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1442;
				  {
				    tree res_op0;
				    res_op0 =  tem;
				    tree res_op1;
				    res_op1 = captures[2];
				    tree _r;
				    _r = fold_build2_loc (loc, RDIV_EXPR, type, res_op0, res_op1);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 944, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail1442:;
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
	case BIT_IOR_EXPR:
	  {
	    tree _q30 = TREE_OPERAND (_q20, 0);
	    tree _q31 = TREE_OPERAND (_q20, 1);
	    switch (TREE_CODE (_q30))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q40 = TREE_OPERAND (_q30, 0);
		  switch (TREE_CODE (_q40))
		    {
		    CASE_CONVERT:
		      {
			tree _q50 = TREE_OPERAND (_q40, 0);
			switch (TREE_CODE (_q50))
			  {
			  case LT_EXPR:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      tree _q61 = TREE_OPERAND (_q50, 1);
			      if (integer_zerop (_q61))
				{
				  if (integer_onep (_q31))
				    {
				      {
					tree _p1_pops[1];
					if (tree_nop_convert (_p1, _p1_pops))
					  {
					    tree _q100 = _p1_pops[0];
					    if ((_q100 == _q60 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q60, 0) && types_match (_q100, _q60)))
					      {
						{
						  tree captures[1] ATTRIBUTE_UNUSED = { _q60 };
						  tree res = generic_simplify_177 (loc, type, _p0, _p1, captures);
						  if (res) return res;
						}
					      }
					  }
				      }
				      if ((_p1 == _q60 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q60, 0) && types_match (_p1, _q60)))
					{
					  {
					    tree captures[1] ATTRIBUTE_UNUSED = { _q60 };
					    tree res = generic_simplify_177 (loc, type, _p0, _p1, captures);
					    if (res) return res;
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
			tree _q50 = TREE_OPERAND (_q40, 0);
			tree _q51 = TREE_OPERAND (_q40, 1);
			if (integer_zerop (_q51))
			  {
			    if (integer_onep (_q31))
			      {
				{
				  tree _p1_pops[1];
				  if (tree_nop_convert (_p1, _p1_pops))
				    {
				      tree _q90 = _p1_pops[0];
				      if ((_q90 == _q50 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q50, 0) && types_match (_q90, _q50)))
					{
					  {
					    tree captures[1] ATTRIBUTE_UNUSED = { _q50 };
					    tree res = generic_simplify_177 (loc, type, _p0, _p1, captures);
					    if (res) return res;
					  }
				        }
				    }
				}
				if ((_p1 == _q50 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q50, 0) && types_match (_p1, _q50)))
				  {
				    {
				      tree captures[1] ATTRIBUTE_UNUSED = { _q50 };
				      tree res = generic_simplify_177 (loc, type, _p0, _p1, captures);
				      if (res) return res;
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
	  {
	    tree _q30_pops[1];
	    if (tree_nop_convert (_q30, _q30_pops))
	      {
		tree _q40 = _q30_pops[0];
		switch (TREE_CODE (_q40))
		  {
		  case NEGATE_EXPR:
		    {
		      tree _q50 = TREE_OPERAND (_q40, 0);
		      switch (TREE_CODE (_q50))
		        {
			CASE_CONVERT:
			  {
			    tree _q60 = TREE_OPERAND (_q50, 0);
			    switch (TREE_CODE (_q60))
			      {
			      case LT_EXPR:
			        {
				  tree _q70 = TREE_OPERAND (_q60, 0);
				  tree _q71 = TREE_OPERAND (_q60, 1);
				  if (integer_zerop (_q71))
				    {
				      if (integer_onep (_q31))
					{
					  {
					    tree _p1_pops[1];
					    if (tree_nop_convert (_p1, _p1_pops))
					      {
						tree _q110 = _p1_pops[0];
						if ((_q110 == _q70 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q70, 0) && types_match (_q110, _q70)))
						  {
						    {
						      tree captures[1] ATTRIBUTE_UNUSED = { _q70 };
						      tree res = generic_simplify_177 (loc, type, _p0, _p1, captures);
						      if (res) return res;
						    }
						  }
					      }
					  }
					  if ((_p1 == _q70 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q70, 0) && types_match (_p1, _q70)))
					    {
					      {
						tree captures[1] ATTRIBUTE_UNUSED = { _q70 };
						tree res = generic_simplify_177 (loc, type, _p0, _p1, captures);
						if (res) return res;
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
			    tree _q60 = TREE_OPERAND (_q50, 0);
			    tree _q61 = TREE_OPERAND (_q50, 1);
			    if (integer_zerop (_q61))
			      {
				if (integer_onep (_q31))
				  {
				    {
				      tree _p1_pops[1];
				      if (tree_nop_convert (_p1, _p1_pops))
				        {
					  tree _q100 = _p1_pops[0];
					  if ((_q100 == _q60 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q60, 0) && types_match (_q100, _q60)))
					    {
					      {
						tree captures[1] ATTRIBUTE_UNUSED = { _q60 };
						tree res = generic_simplify_177 (loc, type, _p0, _p1, captures);
						if (res) return res;
					      }
					    }
				        }
				    }
				    if ((_p1 == _q60 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q60, 0) && types_match (_p1, _q60)))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q60 };
					  tree res = generic_simplify_177 (loc, type, _p0, _p1, captures);
					  if (res) return res;
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
      switch (TREE_CODE (_p1))
        {
	case BIT_IOR_EXPR:
	  {
	    tree _q40 = TREE_OPERAND (_p1, 0);
	    tree _q41 = TREE_OPERAND (_p1, 1);
	    switch (TREE_CODE (_q40))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_q40, 0);
		  switch (TREE_CODE (_q50))
		    {
		    CASE_CONVERT:
		      {
			tree _q60 = TREE_OPERAND (_q50, 0);
			switch (TREE_CODE (_q60))
			  {
			  case LT_EXPR:
			    {
			      tree _q70 = TREE_OPERAND (_q60, 0);
			      tree _q71 = TREE_OPERAND (_q60, 1);
			      if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
				{
				  if (integer_zerop (_q71))
				    {
				      if (integer_onep (_q41))
					{
					  {
					    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
					    tree res = generic_simplify_178 (loc, type, _p0, _p1, captures);
					    if (res) return res;
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
			tree _q60 = TREE_OPERAND (_q50, 0);
			tree _q61 = TREE_OPERAND (_q50, 1);
			if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
			  {
			    if (integer_zerop (_q61))
			      {
				if (integer_onep (_q41))
				  {
				    {
				      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
				      tree res = generic_simplify_178 (loc, type, _p0, _p1, captures);
				      if (res) return res;
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
	  {
	    tree _q40_pops[1];
	    if (tree_nop_convert (_q40, _q40_pops))
	      {
		tree _q50 = _q40_pops[0];
		switch (TREE_CODE (_q50))
		  {
		  case NEGATE_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_q50, 0);
		      switch (TREE_CODE (_q60))
		        {
			CASE_CONVERT:
			  {
			    tree _q70 = TREE_OPERAND (_q60, 0);
			    switch (TREE_CODE (_q70))
			      {
			      case LT_EXPR:
			        {
				  tree _q80 = TREE_OPERAND (_q70, 0);
				  tree _q81 = TREE_OPERAND (_q70, 1);
				  if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
				    {
				      if (integer_zerop (_q81))
					{
					  if (integer_onep (_q41))
					    {
					      {
						tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
						tree res = generic_simplify_178 (loc, type, _p0, _p1, captures);
						if (res) return res;
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
			    tree _q70 = TREE_OPERAND (_q60, 0);
			    tree _q71 = TREE_OPERAND (_q60, 1);
			    if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
			      {
				if (integer_zerop (_q71))
				  {
				    if (integer_onep (_q41))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
					  tree res = generic_simplify_178 (loc, type, _p0, _p1, captures);
					  if (res) return res;
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
    {
      tree _p1_pops[1];
      if (tree_nop_convert (_p1, _p1_pops))
        {
	  tree _q40 = _p1_pops[0];
	  switch (TREE_CODE (_q40))
	    {
	    case BIT_IOR_EXPR:
	      {
		tree _q50 = TREE_OPERAND (_q40, 0);
		tree _q51 = TREE_OPERAND (_q40, 1);
		switch (TREE_CODE (_q50))
		  {
		  case NEGATE_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_q50, 0);
		      switch (TREE_CODE (_q60))
		        {
			CASE_CONVERT:
			  {
			    tree _q70 = TREE_OPERAND (_q60, 0);
			    switch (TREE_CODE (_q70))
			      {
			      case LT_EXPR:
			        {
				  tree _q80 = TREE_OPERAND (_q70, 0);
				  tree _q81 = TREE_OPERAND (_q70, 1);
				  if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
				    {
				      if (integer_zerop (_q81))
					{
					  if (integer_onep (_q51))
					    {
					      {
						tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
						tree res = generic_simplify_178 (loc, type, _p0, _p1, captures);
						if (res) return res;
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
			    tree _q70 = TREE_OPERAND (_q60, 0);
			    tree _q71 = TREE_OPERAND (_q60, 1);
			    if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
			      {
				if (integer_zerop (_q71))
				  {
				    if (integer_onep (_q51))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
					  tree res = generic_simplify_178 (loc, type, _p0, _p1, captures);
					  if (res) return res;
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
	      {
		tree _q50_pops[1];
		if (tree_nop_convert (_q50, _q50_pops))
		  {
		    tree _q60 = _q50_pops[0];
		    switch (TREE_CODE (_q60))
		      {
		      case NEGATE_EXPR:
		        {
			  tree _q70 = TREE_OPERAND (_q60, 0);
			  switch (TREE_CODE (_q70))
			    {
			    CASE_CONVERT:
			      {
				tree _q80 = TREE_OPERAND (_q70, 0);
				switch (TREE_CODE (_q80))
				  {
				  case LT_EXPR:
				    {
				      tree _q90 = TREE_OPERAND (_q80, 0);
				      tree _q91 = TREE_OPERAND (_q80, 1);
				      if ((_q90 == _q20 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q20, 0) && types_match (_q90, _q20)))
					{
					  if (integer_zerop (_q91))
					    {
					      if (integer_onep (_q51))
						{
						  {
						    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
						    tree res = generic_simplify_178 (loc, type, _p0, _p1, captures);
						    if (res) return res;
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
				tree _q80 = TREE_OPERAND (_q70, 0);
				tree _q81 = TREE_OPERAND (_q70, 1);
				if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
				  {
				    if (integer_zerop (_q81))
				      {
					if (integer_onep (_q51))
					  {
					    {
					      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
					      tree res = generic_simplify_178 (loc, type, _p0, _p1, captures);
					      if (res) return res;
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
        }
    }
    }
}
  {
    tree _p1_pops[1];
    if (tree_nop_convert (_p1, _p1_pops))
      {
	tree _q30 = _p1_pops[0];
	switch (TREE_CODE (_q30))
	  {
	  case BIT_IOR_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      switch (TREE_CODE (_q40))
	        {
		case NEGATE_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    switch (TREE_CODE (_q50))
		      {
		      CASE_CONVERT:
		        {
			  tree _q60 = TREE_OPERAND (_q50, 0);
			  switch (TREE_CODE (_q60))
			    {
			    case LT_EXPR:
			      {
				tree _q70 = TREE_OPERAND (_q60, 0);
				tree _q71 = TREE_OPERAND (_q60, 1);
				if ((_q70 == _p0 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _p0, 0) && types_match (_q70, _p0)))
				  {
				    if (integer_zerop (_q71))
				      {
					if (integer_onep (_q41))
					  {
					    {
					      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
					      tree res = generic_simplify_178 (loc, type, _p0, _p1, captures);
					      if (res) return res;
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
		    break;
		  }
	        default:;
	        }
	    {
	      tree _q40_pops[1];
	      if (tree_nop_convert (_q40, _q40_pops))
	        {
		  tree _q50 = _q40_pops[0];
		  switch (TREE_CODE (_q50))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q60 = TREE_OPERAND (_q50, 0);
			switch (TREE_CODE (_q60))
			  {
			  CASE_CONVERT:
			    {
			      tree _q70 = TREE_OPERAND (_q60, 0);
			      switch (TREE_CODE (_q70))
			        {
				case LT_EXPR:
				  {
				    tree _q80 = TREE_OPERAND (_q70, 0);
				    tree _q81 = TREE_OPERAND (_q70, 1);
				    if ((_q80 == _p0 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _p0, 0) && types_match (_q80, _p0)))
				      {
					if (integer_zerop (_q81))
					  {
					    if (integer_onep (_q41))
					      {
						{
						  tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
						  tree res = generic_simplify_178 (loc, type, _p0, _p1, captures);
						  if (res) return res;
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
      }
  }
  switch (TREE_CODE (_p0))
    {
    case BIT_IOR_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  case NEGATE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_q30))
	        {
		CASE_CONVERT:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    switch (TREE_CODE (_q40))
		      {
		      case LT_EXPR:
		        {
			  tree _q50 = TREE_OPERAND (_q40, 0);
			  tree _q51 = TREE_OPERAND (_q40, 1);
			  if (integer_zerop (_q51))
			    {
			      if (integer_onep (_q21))
				{
				  {
				    tree _p1_pops[1];
				    if (tree_nop_convert (_p1, _p1_pops))
				      {
					tree _q90 = _p1_pops[0];
					if ((_q90 == _q50 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q50, 0) && types_match (_q90, _q50)))
					  {
					    {
					      tree captures[1] ATTRIBUTE_UNUSED = { _q50 };
					      tree res = generic_simplify_177 (loc, type, _p0, _p1, captures);
					      if (res) return res;
					    }
					  }
				      }
				  }
				  if ((_p1 == _q50 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q50, 0) && types_match (_p1, _q50)))
				    {
				      {
					tree captures[1] ATTRIBUTE_UNUSED = { _q50 };
					tree res = generic_simplify_177 (loc, type, _p0, _p1, captures);
					if (res) return res;
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
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    if (integer_zerop (_q41))
		      {
			if (integer_onep (_q21))
			  {
			    {
			      tree _p1_pops[1];
			      if (tree_nop_convert (_p1, _p1_pops))
			        {
				  tree _q80 = _p1_pops[0];
				  if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
				    {
				      {
					tree captures[1] ATTRIBUTE_UNUSED = { _q40 };
					tree res = generic_simplify_177 (loc, type, _p0, _p1, captures);
					if (res) return res;
				      }
				    }
			        }
			    }
			    if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
			      {
				{
				  tree captures[1] ATTRIBUTE_UNUSED = { _q40 };
				  tree res = generic_simplify_177 (loc, type, _p0, _p1, captures);
				  if (res) return res;
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
      {
	tree _q20_pops[1];
	if (tree_nop_convert (_q20, _q20_pops))
	  {
	    tree _q30 = _q20_pops[0];
	    switch (TREE_CODE (_q30))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q40 = TREE_OPERAND (_q30, 0);
		  switch (TREE_CODE (_q40))
		    {
		    CASE_CONVERT:
		      {
			tree _q50 = TREE_OPERAND (_q40, 0);
			switch (TREE_CODE (_q50))
			  {
			  case LT_EXPR:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      tree _q61 = TREE_OPERAND (_q50, 1);
			      if (integer_zerop (_q61))
				{
				  if (integer_onep (_q21))
				    {
				      {
					tree _p1_pops[1];
					if (tree_nop_convert (_p1, _p1_pops))
					  {
					    tree _q100 = _p1_pops[0];
					    if ((_q100 == _q60 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q60, 0) && types_match (_q100, _q60)))
					      {
						{
						  tree captures[1] ATTRIBUTE_UNUSED = { _q60 };
						  tree res = generic_simplify_177 (loc, type, _p0, _p1, captures);
						  if (res) return res;
						}
					      }
					  }
				      }
				      if ((_p1 == _q60 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q60, 0) && types_match (_p1, _q60)))
					{
					  {
					    tree captures[1] ATTRIBUTE_UNUSED = { _q60 };
					    tree res = generic_simplify_177 (loc, type, _p0, _p1, captures);
					    if (res) return res;
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
			tree _q50 = TREE_OPERAND (_q40, 0);
			tree _q51 = TREE_OPERAND (_q40, 1);
			if (integer_zerop (_q51))
			  {
			    if (integer_onep (_q21))
			      {
				{
				  tree _p1_pops[1];
				  if (tree_nop_convert (_p1, _p1_pops))
				    {
				      tree _q90 = _p1_pops[0];
				      if ((_q90 == _q50 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q50, 0) && types_match (_q90, _q50)))
					{
					  {
					    tree captures[1] ATTRIBUTE_UNUSED = { _q50 };
					    tree res = generic_simplify_177 (loc, type, _p0, _p1, captures);
					    if (res) return res;
					  }
				        }
				    }
				}
				if ((_p1 == _q50 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q50, 0) && types_match (_p1, _q50)))
				  {
				    {
				      tree captures[1] ATTRIBUTE_UNUSED = { _q50 };
				      tree res = generic_simplify_177 (loc, type, _p0, _p1, captures);
				      if (res) return res;
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
  switch (TREE_CODE (_p1))
    {
    case BIT_IOR_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	switch (TREE_CODE (_q30))
	  {
	  case NEGATE_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      switch (TREE_CODE (_q40))
	        {
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    switch (TREE_CODE (_q50))
		      {
		      case LT_EXPR:
		        {
			  tree _q60 = TREE_OPERAND (_q50, 0);
			  tree _q61 = TREE_OPERAND (_q50, 1);
			  if ((_q60 == _p0 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p0, 0) && types_match (_q60, _p0)))
			    {
			      if (integer_zerop (_q61))
				{
				  if (integer_onep (_q31))
				    {
				      {
					tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
					tree res = generic_simplify_178 (loc, type, _p0, _p1, captures);
					if (res) return res;
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
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
		      {
			if (integer_zerop (_q51))
			  {
			    if (integer_onep (_q31))
			      {
				{
				  tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
				  tree res = generic_simplify_178 (loc, type, _p0, _p1, captures);
				  if (res) return res;
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
      {
	tree _q30_pops[1];
	if (tree_nop_convert (_q30, _q30_pops))
	  {
	    tree _q40 = _q30_pops[0];
	    switch (TREE_CODE (_q40))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_q40, 0);
		  switch (TREE_CODE (_q50))
		    {
		    CASE_CONVERT:
		      {
			tree _q60 = TREE_OPERAND (_q50, 0);
			switch (TREE_CODE (_q60))
			  {
			  case LT_EXPR:
			    {
			      tree _q70 = TREE_OPERAND (_q60, 0);
			      tree _q71 = TREE_OPERAND (_q60, 1);
			      if ((_q70 == _p0 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _p0, 0) && types_match (_q70, _p0)))
				{
				  if (integer_zerop (_q71))
				    {
				      if (integer_onep (_q31))
					{
					  {
					    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
					    tree res = generic_simplify_178 (loc, type, _p0, _p1, captures);
					    if (res) return res;
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
			tree _q60 = TREE_OPERAND (_q50, 0);
			tree _q61 = TREE_OPERAND (_q50, 1);
			if ((_q60 == _p0 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p0, 0) && types_match (_q60, _p0)))
			  {
			    if (integer_zerop (_q61))
			      {
				if (integer_onep (_q31))
				  {
				    {
				      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
				      tree res = generic_simplify_178 (loc, type, _p0, _p1, captures);
				      if (res) return res;
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
{
  tree _p1_pops[1];
  if (tree_nop_convert (_p1, _p1_pops))
    {
      tree _q30 = _p1_pops[0];
      switch (TREE_CODE (_q30))
        {
	case BIT_IOR_EXPR:
	  {
	    tree _q40 = TREE_OPERAND (_q30, 0);
	    tree _q41 = TREE_OPERAND (_q30, 1);
	    switch (TREE_CODE (_q40))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_q40, 0);
		  switch (TREE_CODE (_q50))
		    {
		    case LT_EXPR:
		      {
			tree _q60 = TREE_OPERAND (_q50, 0);
			tree _q61 = TREE_OPERAND (_q50, 1);
			if ((_q60 == _p0 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p0, 0) && types_match (_q60, _p0)))
			  {
			    if (integer_zerop (_q61))
			      {
				if (integer_onep (_q41))
				  {
				    {
				      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
				      tree res = generic_simplify_178 (loc, type, _p0, _p1, captures);
				      if (res) return res;
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
	  {
	    tree _q40_pops[1];
	    if (tree_nop_convert (_q40, _q40_pops))
	      {
		tree _q50 = _q40_pops[0];
		switch (TREE_CODE (_q50))
		  {
		  case NEGATE_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_q50, 0);
		      switch (TREE_CODE (_q60))
		        {
			case LT_EXPR:
			  {
			    tree _q70 = TREE_OPERAND (_q60, 0);
			    tree _q71 = TREE_OPERAND (_q60, 1);
			    if ((_q70 == _p0 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _p0, 0) && types_match (_q70, _p0)))
			      {
				if (integer_zerop (_q71))
				  {
				    if (integer_onep (_q41))
				      {
					{
					  tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
					  tree res = generic_simplify_178 (loc, type, _p0, _p1, captures);
					  if (res) return res;
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
    }
}
  switch (TREE_CODE (_p0))
    {
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case NEGATE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_p1))
	        {
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    if (tree_negate_expr_p (_q50))
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q50 };
			  tree res = generic_simplify_179 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	    if (tree_negate_expr_p (_p1))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _q30, _p1 };
		  tree res = generic_simplify_179 (loc, type, _p0, _p1, captures);
		  if (res) return res;
		}
	      }
	      break;
	    }
	  case LT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _p1 };
		tree res = generic_simplify_180 (loc, type, _p0, _p1, captures, LT_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case LE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _p1 };
		tree res = generic_simplify_180 (loc, type, _p0, _p1, captures, LE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case EQ_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _p1 };
		tree res = generic_simplify_180 (loc, type, _p0, _p1, captures, EQ_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case NE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _p1 };
		tree res = generic_simplify_180 (loc, type, _p0, _p1, captures, NE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case GE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _p1 };
		tree res = generic_simplify_180 (loc, type, _p0, _p1, captures, GE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case GT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _p1 };
		tree res = generic_simplify_180 (loc, type, _p0, _p1, captures, GT_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case UNORDERED_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _p1 };
		tree res = generic_simplify_180 (loc, type, _p0, _p1, captures, UNORDERED_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case ORDERED_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _p1 };
		tree res = generic_simplify_180 (loc, type, _p0, _p1, captures, ORDERED_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case UNLT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _p1 };
		tree res = generic_simplify_180 (loc, type, _p0, _p1, captures, UNLT_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case UNLE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _p1 };
		tree res = generic_simplify_180 (loc, type, _p0, _p1, captures, UNLE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case UNGT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _p1 };
		tree res = generic_simplify_180 (loc, type, _p0, _p1, captures, UNGT_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case UNGE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _p1 };
		tree res = generic_simplify_180 (loc, type, _p0, _p1, captures, UNGE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case UNEQ_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _p1 };
		tree res = generic_simplify_180 (loc, type, _p0, _p1, captures, UNEQ_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case LTGT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _p1 };
		tree res = generic_simplify_180 (loc, type, _p0, _p1, captures, LTGT_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
      if (tree_negate_expr_p (_q20))
	{
	  switch (TREE_CODE (_p1))
	    {
	    CASE_CONVERT:
	      {
		tree _q40 = TREE_OPERAND (_p1, 0);
		switch (TREE_CODE (_q40))
		  {
		  case NEGATE_EXPR:
		    {
		      tree _q50 = TREE_OPERAND (_q40, 0);
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q50, _q20 };
			tree res = generic_simplify_179 (loc, type, _p0, _p1, captures);
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
		tree _q40 = TREE_OPERAND (_p1, 0);
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
		  tree res = generic_simplify_179 (loc, type, _p0, _p1, captures);
		  if (res) return res;
		}
	        break;
	      }
	    default:;
	    }
        }
        break;
      }
    case NEGATE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_p1))
	  {
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      if (tree_negate_expr_p (_q40))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
		    tree res = generic_simplify_179 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
      if (tree_negate_expr_p (_p1))
	{
	  {
	    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
	    tree res = generic_simplify_179 (loc, type, _p0, _p1, captures);
	    if (res) return res;
	  }
        }
        break;
      }
    default:;
    }
if (tree_negate_expr_p (_p0))
  {
    switch (TREE_CODE (_p1))
      {
      CASE_CONVERT:
        {
	  tree _q30 = TREE_OPERAND (_p1, 0);
	  switch (TREE_CODE (_q30))
	    {
	    case NEGATE_EXPR:
	      {
		tree _q40 = TREE_OPERAND (_q30, 0);
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _q40, _p0 };
		  tree res = generic_simplify_179 (loc, type, _p0, _p1, captures);
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
	  tree _q30 = TREE_OPERAND (_p1, 0);
	  {
	    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _p0 };
	    tree res = generic_simplify_179 (loc, type, _p0, _p1, captures);
	    if (res) return res;
	  }
          break;
        }
      default:;
      }
  }
if (tree_zero_one_valued_p (_p0))
  {
    if (tree_zero_one_valued_p (_p1))
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (INTEGRAL_TYPE_P (type)
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1443;
	      {
		tree res_op0;
		res_op0 = captures[0];
		tree res_op1;
		res_op1 = captures[1];
		tree _r;
		_r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 945, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1443:;
	    }
	}
      }
  }
  switch (TREE_CODE (_p1))
    {
    CASE_CONVERT:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	switch (TREE_CODE (_q30))
	  {
	  case LT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p0 };
		tree res = generic_simplify_181 (loc, type, _p0, _p1, captures, LT_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case LE_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p0 };
		tree res = generic_simplify_181 (loc, type, _p0, _p1, captures, LE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case EQ_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p0 };
		tree res = generic_simplify_181 (loc, type, _p0, _p1, captures, EQ_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case NE_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p0 };
		tree res = generic_simplify_181 (loc, type, _p0, _p1, captures, NE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case GE_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p0 };
		tree res = generic_simplify_181 (loc, type, _p0, _p1, captures, GE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case GT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p0 };
		tree res = generic_simplify_181 (loc, type, _p0, _p1, captures, GT_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case UNORDERED_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p0 };
		tree res = generic_simplify_181 (loc, type, _p0, _p1, captures, UNORDERED_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case ORDERED_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p0 };
		tree res = generic_simplify_181 (loc, type, _p0, _p1, captures, ORDERED_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case UNLT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p0 };
		tree res = generic_simplify_181 (loc, type, _p0, _p1, captures, UNLT_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case UNLE_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p0 };
		tree res = generic_simplify_181 (loc, type, _p0, _p1, captures, UNLE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case UNGT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p0 };
		tree res = generic_simplify_181 (loc, type, _p0, _p1, captures, UNGT_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case UNGE_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p0 };
		tree res = generic_simplify_181 (loc, type, _p0, _p1, captures, UNGE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case UNEQ_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p0 };
		tree res = generic_simplify_181 (loc, type, _p0, _p1, captures, UNEQ_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case LTGT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p0 };
		tree res = generic_simplify_181 (loc, type, _p0, _p1, captures, LTGT_EXPR);
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
  switch (TREE_CODE (_p0))
    {
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case EXACT_DIV_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    if ((_q60 == _q31 && ! TREE_SIDE_EFFECTS (_q60)) || operand_equal_p (_q60, _q31, 0))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q31 };
			  tree res = generic_simplify_182 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q31, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q31 };
		    tree res = generic_simplify_182 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case PLUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_q30))
	        {
		CASE_CONVERT:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    switch (TREE_CODE (_q40))
		      {
		      case EXACT_DIV_EXPR:
		        {
			  tree _q50 = TREE_OPERAND (_q40, 0);
			  tree _q51 = TREE_OPERAND (_q40, 1);
			  switch (TREE_CODE (_q51))
			    {
			    case INTEGER_CST:
			      {
				switch (TREE_CODE (_q31))
				  {
				  case INTEGER_CST:
				    {
				      if ((_p1 == _q51 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q51, 0))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q50, _q51, _q31, _q51 };
					    tree res = generic_simplify_183 (loc, type, _p0, _p1, captures, PLUS_EXPR);
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
		case EXACT_DIV_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_q41))
		      {
		      case INTEGER_CST:
		        {
			  switch (TREE_CODE (_q31))
			    {
			    case INTEGER_CST:
			      {
				if ((_p1 == _q41 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q41, 0))
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q41, _q31, _q41 };
				      tree res = generic_simplify_183 (loc, type, _p0, _p1, captures, PLUS_EXPR);
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
	  case MINUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_q30))
	        {
		CASE_CONVERT:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    switch (TREE_CODE (_q40))
		      {
		      case EXACT_DIV_EXPR:
		        {
			  tree _q50 = TREE_OPERAND (_q40, 0);
			  tree _q51 = TREE_OPERAND (_q40, 1);
			  switch (TREE_CODE (_q51))
			    {
			    case INTEGER_CST:
			      {
				switch (TREE_CODE (_q31))
				  {
				  case INTEGER_CST:
				    {
				      if ((_p1 == _q51 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q51, 0))
					{
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q50, _q51, _q31, _q51 };
					    tree res = generic_simplify_183 (loc, type, _p0, _p1, captures, MINUS_EXPR);
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
		case EXACT_DIV_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_q41))
		      {
		      case INTEGER_CST:
		        {
			  switch (TREE_CODE (_q31))
			    {
			    case INTEGER_CST:
			      {
				if ((_p1 == _q41 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q41, 0))
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q41, _q31, _q41 };
				      tree res = generic_simplify_183 (loc, type, _p0, _p1, captures, MINUS_EXPR);
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
    case EXACT_DIV_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  CASE_CONVERT:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q21, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q21 };
		    tree res = generic_simplify_182 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q21, 0))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q21 };
	      tree res = generic_simplify_182 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
        break;
      }
    case PLUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_q30))
	        {
		case EXACT_DIV_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_q41))
		      {
		      case INTEGER_CST:
		        {
			  switch (TREE_CODE (_q21))
			    {
			    case INTEGER_CST:
			      {
				if ((_p1 == _q41 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q41, 0))
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q41, _q21, _q41 };
				      tree res = generic_simplify_183 (loc, type, _p0, _p1, captures, PLUS_EXPR);
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
	  case EXACT_DIV_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_q31))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_q21))
		      {
		      case INTEGER_CST:
		        {
			  if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q31, 0))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q21, _q31 };
				tree res = generic_simplify_183 (loc, type, _p0, _p1, captures, PLUS_EXPR);
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
    case MINUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_q30))
	        {
		case EXACT_DIV_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_q41))
		      {
		      case INTEGER_CST:
		        {
			  switch (TREE_CODE (_q21))
			    {
			    case INTEGER_CST:
			      {
				if ((_p1 == _q41 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q41, 0))
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q41, _q21, _q41 };
				      tree res = generic_simplify_183 (loc, type, _p0, _p1, captures, MINUS_EXPR);
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
	  case EXACT_DIV_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_q31))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_q21))
		      {
		      case INTEGER_CST:
		        {
			  if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q31, 0))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q21, _q31 };
				tree res = generic_simplify_183 (loc, type, _p0, _p1, captures, MINUS_EXPR);
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
  if (integer_minus_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1444;
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1444;
	{
	  tree res_op0;
	  res_op0 = captures[0];
	  tree _r;
	  _r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 946, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1444:;
      }
    }
  switch (TREE_CODE (_p0))
    {
    case MULT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
		tree res = generic_simplify_184 (loc, type, _p0, _p1, captures);
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
    case MULT_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	switch (TREE_CODE (_q31))
	  {
	  case INTEGER_CST:
	    {
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q30, _q31, _p0 };
		tree res = generic_simplify_184 (loc, type, _p0, _p1, captures);
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
  switch (TREE_CODE (_p0))
    {
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
 && (TREE_CODE_CLASS (MULT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1445;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1445;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1445;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1445:;
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
 && (TREE_CODE_CLASS (MULT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1446;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1446;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1446;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1446;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1446:;
	    }
	}
        break;
      }
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
 && (TREE_CODE_CLASS (MULT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1447;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1447;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1447;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1447;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1447:;
	    }
	}
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case CALL_EXPR:
      switch (get_call_combined_fn (_p0))
        {
	case CFN_BUILT_IN_SQRTF:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		    tree res = generic_simplify_185 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTF);
		    if (res) return res;
		  }
	        }
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_SQRTF:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1448;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_SQRTF, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1448;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 947, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1448:;
			      }
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
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		    tree res = generic_simplify_185 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTL);
		    if (res) return res;
		  }
	        }
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_SQRTL:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1449;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_SQRTL, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1449;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 947, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1449:;
			      }
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
	case CFN_BUILT_IN_COS:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_TAN:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _p0 };
				tree res = generic_simplify_187 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_TAN, CFN_BUILT_IN_COS, CFN_BUILT_IN_SIN);
				if (res) return res;
			      }
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
	case CFN_BUILT_IN_EXP:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_EXP:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1450;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1450;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 948, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1450:;
			      }
			  }
		        }
		      break;
		    case CFN_BUILT_IN_POW:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  tree _q41 = CALL_EXPR_ARG (_p1, 1);
			  switch (TREE_CODE (_q40))
			    {
			    case REAL_CST:
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q40, _q41, _p0, _q20 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1451;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOG, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1451;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1451;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1451:;
					}
				    }
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
	    }
	  break;
	case CFN_BUILT_IN_POW:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      switch (TREE_CODE (_q20))
	        {
		case REAL_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_p1))
		          {
			  case CFN_BUILT_IN_EXP:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_p1, 0);
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1452;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOG, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1452;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1452;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1452:;
					}
				    }
				}
			      }
			    break;
			  case CFN_BUILT_IN_EXP2:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_p1, 0);
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1453;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOG2, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1453;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP2, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1453;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1453:;
					}
				    }
				}
			      }
			    break;
			  case CFN_BUILT_IN_EXP10:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_p1, 0);
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1454;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOG10, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1454;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP10, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1454;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1454:;
					}
				    }
				}
			      }
			    break;
			  case CFN_BUILT_IN_POW10:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_p1, 0);
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1455;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOG10, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1455;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_POW10, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1455;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1455:;
					}
				    }
				}
			      }
			    break;
			  default:;
		          }
		        break;
		      default:;
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q21))
	        {
		case REAL_CST:
		  {
		    if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p0, _q21 };
			  tree res = generic_simplify_186 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POW);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_TAN:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_COS:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_187 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_TAN, CFN_BUILT_IN_COS, CFN_BUILT_IN_SIN);
				if (res) return res;
			      }
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
	case CFN_BUILT_IN_EXP10F:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_EXP10F:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1456;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP10F, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1456;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 948, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1456:;
			      }
			  }
		        }
		      break;
		    case CFN_BUILT_IN_POWF:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  tree _q41 = CALL_EXPR_ARG (_p1, 1);
			  switch (TREE_CODE (_q40))
			    {
			    case REAL_CST:
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q40, _q41, _p0, _q20 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1457;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOG10F, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1457;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP10F, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1457;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1457:;
					}
				    }
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
	    }
	  break;
	case CFN_BUILT_IN_EXP10L:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_EXP10L:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1458;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP10L, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1458;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 948, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1458:;
			      }
			  }
		        }
		      break;
		    case CFN_BUILT_IN_POWL:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  tree _q41 = CALL_EXPR_ARG (_p1, 1);
			  switch (TREE_CODE (_q40))
			    {
			    case REAL_CST:
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q40, _q41, _p0, _q20 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1459;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOG10L, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1459;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP10L, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1459;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1459:;
					}
				    }
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
	    }
	  break;
	case CFN_BUILT_IN_CBRT:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_CBRT:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1460;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_CBRT, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1460;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 947, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1460:;
			      }
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
	case CFN_BUILT_IN_COSF:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_TANF:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _p0 };
				tree res = generic_simplify_187 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_TANF, CFN_BUILT_IN_COSF, CFN_BUILT_IN_SINF);
				if (res) return res;
			      }
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
	case CFN_BUILT_IN_COSL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_TANL:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _p0 };
				tree res = generic_simplify_187 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_TANL, CFN_BUILT_IN_COSL, CFN_BUILT_IN_SINL);
				if (res) return res;
			      }
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
	case CFN_BUILT_IN_EXP2:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_POW:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  tree _q41 = CALL_EXPR_ARG (_p1, 1);
			  switch (TREE_CODE (_q40))
			    {
			    case REAL_CST:
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q40, _q41, _p0, _q20 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1461;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOG2, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1461;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP2, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1461;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1461:;
					}
				    }
				}
			        break;
			      }
			    default:;
			    }
		        }
		      break;
		    case CFN_BUILT_IN_EXP2:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1462;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP2, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1462;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 948, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1462:;
			      }
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
	case CFN_BUILT_IN_EXPF:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_EXPF:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1463;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXPF, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1463;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 948, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1463:;
			      }
			  }
		        }
		      break;
		    case CFN_BUILT_IN_POWF:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  tree _q41 = CALL_EXPR_ARG (_p1, 1);
			  switch (TREE_CODE (_q40))
			    {
			    case REAL_CST:
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q40, _q41, _p0, _q20 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1464;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOGF, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1464;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXPF, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1464;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1464:;
					}
				    }
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
	    }
	  break;
	case CFN_BUILT_IN_EXPL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_EXPL:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1465;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXPL, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1465;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 948, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1465:;
			      }
			  }
		        }
		      break;
		    case CFN_BUILT_IN_POWL:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  tree _q41 = CALL_EXPR_ARG (_p1, 1);
			  switch (TREE_CODE (_q40))
			    {
			    case REAL_CST:
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q40, _q41, _p0, _q20 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1466;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOGL, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1466;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXPL, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1466;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1466:;
					}
				    }
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
	    }
	  break;
	case CFN_BUILT_IN_POWF:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      switch (TREE_CODE (_q20))
	        {
		case REAL_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_p1))
		          {
			  case CFN_BUILT_IN_EXP10F:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_p1, 0);
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1467;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOG10F, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1467;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP10F, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1467;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1467:;
					}
				    }
				}
			      }
			    break;
			  case CFN_BUILT_IN_EXPF:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_p1, 0);
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1468;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOGF, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1468;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXPF, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1468;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1468:;
					}
				    }
				}
			      }
			    break;
			  case CFN_BUILT_IN_POW10F:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_p1, 0);
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1469;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOG10F, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1469;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_POW10F, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1469;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1469:;
					}
				    }
				}
			      }
			    break;
			  case CFN_BUILT_IN_EXP2F:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_p1, 0);
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1470;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOG2F, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1470;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP2F, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1470;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1470:;
					}
				    }
				}
			      }
			    break;
			  default:;
		          }
		        break;
		      default:;
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q21))
	        {
		case REAL_CST:
		  {
		    if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p0, _q21 };
			  tree res = generic_simplify_186 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POWF);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_POWL:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      switch (TREE_CODE (_q20))
	        {
		case REAL_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_p1))
		          {
			  case CFN_BUILT_IN_EXP10L:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_p1, 0);
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1471;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOG10L, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1471;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP10L, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1471;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1471:;
					}
				    }
				}
			      }
			    break;
			  case CFN_BUILT_IN_EXPL:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_p1, 0);
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1472;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOGL, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1472;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXPL, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1472;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1472:;
					}
				    }
				}
			      }
			    break;
			  case CFN_BUILT_IN_POW10L:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_p1, 0);
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1473;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOG10L, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1473;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_POW10L, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1473;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1473:;
					}
				    }
				}
			      }
			    break;
			  case CFN_BUILT_IN_EXP2L:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_p1, 0);
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1474;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOG2L, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1474;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP2L, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1474;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1474:;
					}
				    }
				}
			      }
			    break;
			  default:;
		          }
		        break;
		      default:;
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q21))
	        {
		case REAL_CST:
		  {
		    if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p0, _q21 };
			  tree res = generic_simplify_186 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POWL);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_SQRT:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		    tree res = generic_simplify_185 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRT);
		    if (res) return res;
		  }
	        }
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_SQRT:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1475;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_SQRT, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1475;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 947, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1475:;
			      }
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
	case CFN_BUILT_IN_TANF:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_COSF:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_187 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_TANF, CFN_BUILT_IN_COSF, CFN_BUILT_IN_SINF);
				if (res) return res;
			      }
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
	case CFN_BUILT_IN_TANL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_COSL:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_187 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_TANL, CFN_BUILT_IN_COSL, CFN_BUILT_IN_SINL);
				if (res) return res;
			      }
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
	case CFN_BUILT_IN_SQRTF128:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		    tree res = generic_simplify_185 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTF128);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_SQRTF32X:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		    tree res = generic_simplify_185 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTF32X);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_SQRTF64X:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		    tree res = generic_simplify_185 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTF64X);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_SQRTF16:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		    tree res = generic_simplify_185 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTF16);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_SQRTF32:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		    tree res = generic_simplify_185 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTF32);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_SQRTF64:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		    tree res = generic_simplify_185 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTF64);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COS:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_TAN:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _p0 };
				tree res = generic_simplify_187 (loc, type, _p0, _p1, captures, CFN_TAN, CFN_COS, CFN_SIN);
				if (res) return res;
			      }
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
	case CFN_EXP:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_EXP:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1476;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_EXP, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1476;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 948, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1476:;
			      }
			  }
		        }
		      break;
		    case CFN_POW:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  tree _q41 = CALL_EXPR_ARG (_p1, 1);
			  switch (TREE_CODE (_q40))
			    {
			    case REAL_CST:
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q40, _q41, _p0, _q20 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1477;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_LOG, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1477;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_EXP, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1477;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1477:;
					}
				    }
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
	    }
	  break;
	case CFN_POW:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      switch (TREE_CODE (_q20))
	        {
		case REAL_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_p1))
		          {
			  case CFN_EXP:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_p1, 0);
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1478;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_LOG, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1478;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_EXP, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1478;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1478:;
					}
				    }
				}
			      }
			    break;
			  case CFN_EXP2:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_p1, 0);
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1479;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_LOG2, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1479;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_EXP2, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1479;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1479:;
					}
				    }
				}
			      }
			    break;
			  case CFN_EXP10:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_p1, 0);
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1480;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_LOG10, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1480;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_EXP10, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1480;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1480:;
					}
				    }
				}
			      }
			    break;
			  default:;
		          }
		        break;
		      default:;
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q21))
	        {
		case REAL_CST:
		  {
		    if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p0, _q21 };
			  tree res = generic_simplify_186 (loc, type, _p0, _p1, captures, CFN_POW);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_TAN:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_COS:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_187 (loc, type, _p0, _p1, captures, CFN_TAN, CFN_COS, CFN_SIN);
				if (res) return res;
			      }
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
	case CFN_BUILT_IN_POW10F:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_POWF:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  tree _q41 = CALL_EXPR_ARG (_p1, 1);
			  switch (TREE_CODE (_q40))
			    {
			    case REAL_CST:
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q40, _q41, _p0, _q20 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1481;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOG10F, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1481;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_POW10F, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1481;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1481:;
					}
				    }
				}
			        break;
			      }
			    default:;
			    }
		        }
		      break;
		    case CFN_BUILT_IN_POW10F:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1482;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_POW10F, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1482;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 948, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1482:;
			      }
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
	case CFN_BUILT_IN_POW10L:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_POWL:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  tree _q41 = CALL_EXPR_ARG (_p1, 1);
			  switch (TREE_CODE (_q40))
			    {
			    case REAL_CST:
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q40, _q41, _p0, _q20 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1483;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOG10L, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1483;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_POW10L, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1483;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1483:;
					}
				    }
				}
			        break;
			      }
			    default:;
			    }
		        }
		      break;
		    case CFN_BUILT_IN_POW10L:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1484;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_POW10L, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1484;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 948, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1484:;
			      }
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
	case CFN_BUILT_IN_SQRTF128X:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		    tree res = generic_simplify_185 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTF128X);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_EXP2:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_POW:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  tree _q41 = CALL_EXPR_ARG (_p1, 1);
			  switch (TREE_CODE (_q40))
			    {
			    case REAL_CST:
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q40, _q41, _p0, _q20 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1485;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_LOG2, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1485;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_EXP2, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1485;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1485:;
					}
				    }
				}
			        break;
			      }
			    default:;
			    }
		        }
		      break;
		    case CFN_EXP2:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1486;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_EXP2, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1486;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 948, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1486:;
			      }
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
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
		    tree res = generic_simplify_185 (loc, type, _p0, _p1, captures, CFN_SQRT);
		    if (res) return res;
		  }
	        }
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_SQRT:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1487;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_SQRT, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1487;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 947, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1487:;
			      }
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
	case CFN_EXP10:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_POW:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  tree _q41 = CALL_EXPR_ARG (_p1, 1);
			  switch (TREE_CODE (_q40))
			    {
			    case REAL_CST:
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q40, _q41, _p0, _q20 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1488;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_LOG10, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1488;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_EXP10, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1488;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1488:;
					}
				    }
				}
			        break;
			      }
			    default:;
			    }
		        }
		      break;
		    case CFN_EXP10:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1489;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_EXP10, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1489;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 948, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1489:;
			      }
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
	case CFN_BUILT_IN_CBRTF:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_CBRTF:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1490;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_CBRTF, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1490;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 947, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1490:;
			      }
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
	case CFN_BUILT_IN_CBRTL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_CBRTL:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1491;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_CBRTL, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1491;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 947, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1491:;
			      }
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
	case CFN_BUILT_IN_EXP10:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_POW:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  tree _q41 = CALL_EXPR_ARG (_p1, 1);
			  switch (TREE_CODE (_q40))
			    {
			    case REAL_CST:
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q40, _q41, _p0, _q20 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1492;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOG10, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1492;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP10, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1492;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1492:;
					}
				    }
				}
			        break;
			      }
			    default:;
			    }
		        }
		      break;
		    case CFN_BUILT_IN_EXP10:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1493;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP10, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1493;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 948, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1493:;
			      }
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
	case CFN_BUILT_IN_EXP2F:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_POWF:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  tree _q41 = CALL_EXPR_ARG (_p1, 1);
			  switch (TREE_CODE (_q40))
			    {
			    case REAL_CST:
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q40, _q41, _p0, _q20 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1494;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOG2F, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1494;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP2F, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1494;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1494:;
					}
				    }
				}
			        break;
			      }
			    default:;
			    }
		        }
		      break;
		    case CFN_BUILT_IN_EXP2F:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1495;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP2F, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1495;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 948, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1495:;
			      }
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
	case CFN_BUILT_IN_EXP2L:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_POWL:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  tree _q41 = CALL_EXPR_ARG (_p1, 1);
			  switch (TREE_CODE (_q40))
			    {
			    case REAL_CST:
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q40, _q41, _p0, _q20 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1496;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOG2L, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1496;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP2L, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1496;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1496:;
					}
				    }
				}
			        break;
			      }
			    default:;
			    }
		        }
		      break;
		    case CFN_BUILT_IN_EXP2L:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1497;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP2L, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1497;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 948, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1497:;
			      }
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
	case CFN_BUILT_IN_POW10:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_POW:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  tree _q41 = CALL_EXPR_ARG (_p1, 1);
			  switch (TREE_CODE (_q40))
			    {
			    case REAL_CST:
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q40, _q41, _p0, _q20 };
				  if (flag_unsafe_math_optimizations
)
				    {
				      if (real_compare (GT_EXPR, TREE_REAL_CST_PTR (captures[1]), &dconst0)
 && real_isfinite (TREE_REAL_CST_PTR (captures[1]))
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1498;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      {
						tree _o2[2], _r2;
						{
						  tree _o3[1], _r3;
						  _o3[0] = captures[1];
						  _r3 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_LOG10, TREE_TYPE (_o3[0]), 1, _o3[0]);
						  if (!_r3)
						    goto next_after_fail1498;
						  _o2[0] = _r3;
						}
						_o2[1] = captures[2];
						_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
						_o1[0] = _r2;
					      }
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree _r;
					    _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_POW10, type, 1, res_op0);
					    if (!_r)
					      goto next_after_fail1498;
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 949, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1498:;
					}
				    }
				}
			        break;
			      }
			    default:;
			    }
		        }
		      break;
		    case CFN_BUILT_IN_POW10:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    if (flag_unsafe_math_optimizations
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1499;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] = captures[3];
				    _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_POW10, type, 1, res_op0);
				  if (!_r)
				    goto next_after_fail1499;
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 948, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1499:;
			      }
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
    case CALL_EXPR:
      switch (get_call_combined_fn (_p1))
        {
	case CFN_BUILT_IN_POW:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  switch (TREE_CODE (_q31))
		    {
		    case REAL_CST:
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q31 };
			  tree res = generic_simplify_186 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POW);
			  if (res) return res;
			}
		        break;
		      }
		    default:;
		    }
	        }
	    }
	  break;
	case CFN_BUILT_IN_POWF:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  switch (TREE_CODE (_q31))
		    {
		    case REAL_CST:
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q31 };
			  tree res = generic_simplify_186 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POWF);
			  if (res) return res;
			}
		        break;
		      }
		    default:;
		    }
	        }
	    }
	  break;
	case CFN_BUILT_IN_POWL:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  switch (TREE_CODE (_q31))
		    {
		    case REAL_CST:
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q31 };
			  tree res = generic_simplify_186 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POWL);
			  if (res) return res;
			}
		        break;
		      }
		    default:;
		    }
	        }
	    }
	  break;
	case CFN_POW:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  switch (TREE_CODE (_q31))
		    {
		    case REAL_CST:
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q31 };
			  tree res = generic_simplify_186 (loc, type, _p0, _p1, captures, CFN_POW);
			  if (res) return res;
			}
		        break;
		      }
		    default:;
		    }
	        }
	    }
	  break;
	default:;
        }
      break;
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case VEC_PERM_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	tree _q22 = TREE_OPERAND (_p0, 2);
	if ((_q21 == _q20 && ! TREE_SIDE_EFFECTS (_q21)) || (operand_equal_p (_q21, _q20, 0) && types_match (_q21, _q20)))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case VEC_PERM_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p1, 0);
		  tree _q61 = TREE_OPERAND (_p1, 1);
		  tree _q62 = TREE_OPERAND (_p1, 2);
		  if ((_q61 == _q60 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q60, 0) && types_match (_q61, _q60)))
		    {
		      if ((_q62 == _q22 && ! TREE_SIDE_EFFECTS (_q62)) || (operand_equal_p (_q62, _q22, 0) && types_match (_q62, _q22)))
			{
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q22, _q60 };
			    if (VECTOR_INTEGER_TYPE_P (type)
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1500;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[0];
				    _o1[1] = captures[2];
				    _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    captures[3] = _r1;
				  }
				  res_op0 = unshare_expr (captures[3]);
				  tree res_op1;
				  res_op1 = captures[3];
				  tree res_op2;
				  res_op2 = captures[1];
				  tree _r;
				  _r = fold_build3_loc (loc, VEC_PERM_EXPR, type, res_op0, res_op1, res_op2);
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 871, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1500:;
			      }
			  }
		        }
		    }
	          break;
	        }
	      default:;
	      }
	    switch (TREE_CODE (_q22))
	      {
	      case VECTOR_CST:
	        {
		  switch (TREE_CODE (_p1))
		    {
		    case VEC_PERM_EXPR:
		      {
			tree _q60 = TREE_OPERAND (_p1, 0);
			tree _q61 = TREE_OPERAND (_p1, 1);
			tree _q62 = TREE_OPERAND (_p1, 2);
			if ((_q61 == _q60 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q60, 0) && types_match (_q61, _q60)))
			  {
			    if ((_q62 == _q22 && ! TREE_SIDE_EFFECTS (_q62)) || (operand_equal_p (_q62, _q22, 0) && types_match (_q62, _q22)))
			      {
				switch (TREE_CODE (_q62))
				  {
				  case VECTOR_CST:
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q22, _q60 };
					tree res = generic_simplify_37 (loc, type, _p0, _p1, captures, MULT_EXPR);
					if (res) return res;
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
	  }
        break;
      }
    case CALL_EXPR:
      switch (get_call_combined_fn (_p0))
        {
	case CFN_BUILT_IN_POW:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_POW:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q50 = CALL_EXPR_ARG (_p1, 0);
			  tree _q51 = CALL_EXPR_ARG (_p1, 1);
			  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
			    {
			      {
				tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
				tree res = generic_simplify_188 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POW);
				if (res) return res;
			      }
			    }
			  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			    {
			      {
				tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				tree res = generic_simplify_189 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POW);
				if (res) return res;
			      }
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
	case CFN_BUILT_IN_POWF:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_POWF:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q50 = CALL_EXPR_ARG (_p1, 0);
			  tree _q51 = CALL_EXPR_ARG (_p1, 1);
			  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
			    {
			      {
				tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
				tree res = generic_simplify_188 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POWF);
				if (res) return res;
			      }
			    }
			  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			    {
			      {
				tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				tree res = generic_simplify_189 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POWF);
				if (res) return res;
			      }
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
	case CFN_BUILT_IN_POWI:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_POWI:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q50 = CALL_EXPR_ARG (_p1, 0);
			  tree _q51 = CALL_EXPR_ARG (_p1, 1);
			  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			    {
			      {
				tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
				  {
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1501;
				    {
				      tree res_op0;
				      {
					tree _o1[2], _r1;
					_o1[0] = captures[1];
					_o1[1] = captures[4];
					_r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					res_op0 = _r1;
				      }
				      tree res_op1;
				      res_op1 = captures[2];
				      tree _r;
				      _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_POWI, type, 2, res_op0, res_op1);
				      if (!_r)
				        goto next_after_fail1501;
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 950, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1501:;
				  }
			      }
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
	case CFN_BUILT_IN_POWL:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_POWL:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q50 = CALL_EXPR_ARG (_p1, 0);
			  tree _q51 = CALL_EXPR_ARG (_p1, 1);
			  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
			    {
			      {
				tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
				tree res = generic_simplify_188 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POWL);
				if (res) return res;
			      }
			    }
			  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			    {
			      {
				tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				tree res = generic_simplify_189 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POWL);
				if (res) return res;
			      }
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
	case CFN_POW:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_POW:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q50 = CALL_EXPR_ARG (_p1, 0);
			  tree _q51 = CALL_EXPR_ARG (_p1, 1);
			  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
			    {
			      {
				tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
				tree res = generic_simplify_188 (loc, type, _p0, _p1, captures, CFN_POW);
				if (res) return res;
			      }
			    }
			  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			    {
			      {
				tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				tree res = generic_simplify_189 (loc, type, _p0, _p1, captures, CFN_POW);
				if (res) return res;
			      }
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
	case CFN_BUILT_IN_POWIF:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_POWIF:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q50 = CALL_EXPR_ARG (_p1, 0);
			  tree _q51 = CALL_EXPR_ARG (_p1, 1);
			  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			    {
			      {
				tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
				  {
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1502;
				    {
				      tree res_op0;
				      {
					tree _o1[2], _r1;
					_o1[0] = captures[1];
					_o1[1] = captures[4];
					_r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					res_op0 = _r1;
				      }
				      tree res_op1;
				      res_op1 = captures[2];
				      tree _r;
				      _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_POWIF, type, 2, res_op0, res_op1);
				      if (!_r)
				        goto next_after_fail1502;
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 950, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1502:;
				  }
			      }
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
	case CFN_BUILT_IN_POWIL:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_POWIL:
		      if (call_expr_nargs (_p1) == 2)
    {
			  tree _q50 = CALL_EXPR_ARG (_p1, 0);
			  tree _q51 = CALL_EXPR_ARG (_p1, 1);
			  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			    {
			      {
				tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
				if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
				  {
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1503;
				    {
				      tree res_op0;
				      {
					tree _o1[2], _r1;
					_o1[0] = captures[1];
					_o1[1] = captures[4];
					_r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					res_op0 = _r1;
				      }
				      tree res_op1;
				      res_op1 = captures[2];
				      tree _r;
				      _r = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_POWIL, type, 2, res_op0, res_op1);
				      if (!_r)
				        goto next_after_fail1503;
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 950, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1503:;
				  }
			      }
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
  return NULL_TREE;
}
#pragma GCC diagnostic pop
