/* Generated automatically by the program `genmatch' from
   a IL pattern matching and simplification description.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-function"

#include "generic-match-auto.h"

bool
tree_maybe_bit_not (tree t, tree *res_ops)
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
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 8, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    default:;
    }
{
  tree t_pops[1];
  if (tree_bit_not_with_nop (t, t_pops))
    {
      tree _p0 = t_pops[0];
      {
	tree captures[2] ATTRIBUTE_UNUSED = { t, _p0 };
	{
	  res_ops[0] = captures[0];
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 9, __FILE__, __LINE__, false);
	  return true;
	}
      }
    }
}
{
  tree t_pops[1];
  if (tree_maybe_cmp (t, t_pops))
    {
      tree _p0 = t_pops[0];
      {
	tree captures[2] ATTRIBUTE_UNUSED = { t, _p0 };
	{
	  res_ops[0] = captures[0];
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 10, __FILE__, __LINE__, false);
	  return true;
	}
      }
    }
}
{
  tree t_pops[2];
  if (tree_bit_xor_cst (t, t_pops))
    {
      tree _p0 = t_pops[0];
      tree _p1 = t_pops[1];
      {
	tree captures[3] ATTRIBUTE_UNUSED = { t, _p0, _p1 };
	{
	  res_ops[0] = captures[0];
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 11, __FILE__, __LINE__, false);
	  return true;
	}
      }
    }
}
  return false;
}

bool
tree_max_value (tree t)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  if (uniform_integer_cst_p (t))
    {
      {
	{
 tree int_cst = uniform_integer_cst_p (t);
 tree itype = TREE_TYPE (int_cst);
	    if ((INTEGRAL_TYPE_P (itype)
 || POINTER_TYPE_P (itype))
 && wi::eq_p (wi::to_wide (int_cst), wi::max_value (itype))
)
	      {
		{
		  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 39, __FILE__, __LINE__, false);
		  return true;
		}
	      }
	}
      }
    }
  return false;
}

bool
tree_usadd_right_part_2 (tree t, tree *res_ops)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  switch (TREE_CODE (t))
    {
    case NEGATE_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	switch (TREE_CODE (_p0))
	  {
	  CASE_CONVERT:
	    {
	      tree _q20 = TREE_OPERAND (_p0, 0);
	      switch (TREE_CODE (_q20))
	        {
		case NE_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    switch (TREE_CODE (_q30))
		      {
		      case IMAGPART_EXPR:
		        {
			  tree _q40 = TREE_OPERAND (_q30, 0);
			  switch (TREE_CODE (_q40))
			    {
			    case CALL_EXPR:
			      switch (get_call_combined_fn (_q40))
			        {
				case CFN_ADD_OVERFLOW:
				  if (call_expr_nargs (_q40) == 2)
    {
				      tree _q50 = CALL_EXPR_ARG (_q40, 0);
				      tree _q51 = CALL_EXPR_ARG (_q40, 1);
				      if (integer_zerop (_q31))
					{
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q50, _q51 };
					    if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
					      {
						{
						  res_ops[0] = captures[0];
						  res_ops[1] = captures[1];
						  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 44, __FILE__, __LINE__, false);
						  return true;
						}
					      }
					  }
					  {
					    tree captures[2] ATTRIBUTE_UNUSED = { _q51, _q50 };
					    if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
					      {
						{
						  res_ops[0] = captures[0];
						  res_ops[1] = captures[1];
						  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 44, __FILE__, __LINE__, false);
						  return true;
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
	        default:;
	        }
	      break;
	    }
	  case IMAGPART_EXPR:
	    {
	      tree _q20 = TREE_OPERAND (_p0, 0);
	      switch (TREE_CODE (_q20))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_q20))
		    {
		    case CFN_ADD_OVERFLOW:
		      if (call_expr_nargs (_q20) == 2)
    {
			  tree _q30 = CALL_EXPR_ARG (_q20, 0);
			  tree _q31 = CALL_EXPR_ARG (_q20, 1);
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
			    if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
			      {
				{
				  res_ops[0] = captures[0];
				  res_ops[1] = captures[1];
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 45, __FILE__, __LINE__, false);
				  return true;
				}
			      }
			  }
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q31, _q30 };
			    if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
			      {
				{
				  res_ops[0] = captures[0];
				  res_ops[1] = captures[1];
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 45, __FILE__, __LINE__, false);
				  return true;
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
    default:;
    }
  return false;
}

bool
tree_ctz_table_index (tree t, tree *res_ops)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  switch (TREE_CODE (t))
    {
    case RSHIFT_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	switch (TREE_CODE (_p0))
	  {
	  case MULT_EXPR:
	    {
	      tree _q20 = TREE_OPERAND (_p0, 0);
	      tree _q21 = TREE_OPERAND (_p0, 1);
	      switch (TREE_CODE (_q20))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    switch (TREE_CODE (_q30))
		      {
		      case NEGATE_EXPR:
		        {
			  tree _q40 = TREE_OPERAND (_q30, 0);
			  if ((_q31 == _q40 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _q40, 0) && types_match (_q31, _q40)))
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
					    tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q21, _p1 };
					    {
					      res_ops[0] = captures[0];
					      res_ops[1] = captures[1];
					      res_ops[2] = captures[2];
					      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 85, __FILE__, __LINE__, false);
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
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q31))
		      {
		      case NEGATE_EXPR:
		        {
			  tree _q50 = TREE_OPERAND (_q31, 0);
			  if ((_q50 == _q30 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q30, 0) && types_match (_q50, _q30)))
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
					    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q21, _p1 };
					    {
					      res_ops[0] = captures[0];
					      res_ops[1] = captures[1];
					      res_ops[2] = captures[2];
					      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 85, __FILE__, __LINE__, false);
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
generic_simplify_5 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && TYPE_OVERFLOW_WRAPS (type)
 && !TYPE_SATURATING (type)
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail224;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  _o1[1] = captures[4];
	  _r1 = fold_build2_loc (loc, op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree res_op1;
	res_op1 = captures[2];
	tree _r;
	_r = fold_build2_loc (loc, LSHIFT_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 94, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail224:;
    }
  return NULL_TREE;
}

tree
generic_simplify_7 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail226;
  {
    if (! tree_invariant_p (captures[2])) goto next_after_fail226;
    tree res_op0;
    {
      tree _o1[2], _r1;
      _o1[0] = captures[0];
      _o1[1] = unshare_expr (captures[2]);
      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
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
    _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 96, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail226:;
  return NULL_TREE;
}

tree
generic_simplify_11 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_OVERFLOW_SANITIZED (type) && !TYPE_OVERFLOW_TRAPS (type)
 && !TYPE_SATURATING (type) && !TYPE_OVERFLOW_SANITIZED (TREE_TYPE (captures[0]))
 && !TYPE_OVERFLOW_TRAPS (TREE_TYPE (captures[0]))
 && !TYPE_SATURATING (TREE_TYPE (captures[0]))
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail230;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail230;
      {
	tree res_op0;
	{
	  tree _o1[1], _r1;
	  {
	    tree _o2[2], _r2;
	    _o2[0] = captures[1];
	    _o2[1] = captures[2];
	    _r2 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
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
	tree _r;
	_r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 100, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail230:;
    }
  return NULL_TREE;
}

tree
generic_simplify_14 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && invert_tree_comparison (cmp, HONOR_NANS (captures[1])) == icmp
 && canonicalize_math_after_vectorization_p ()
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail233;
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail233;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail233;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 = captures[3];
	tree res_op2;
	res_op2 = captures[5];
	tree _r;
	_r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 103, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail233:;
    }
  return NULL_TREE;
}

tree
generic_simplify_17 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (outer_op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_SATURATING (type)
)
    {
      if ((!FLOAT_TYPE_P (type) || flag_associative_math)
 && !FIXED_POINT_TYPE_P (type)
)
	{
	  if (!ANY_INTEGRAL_TYPE_P (type) || TYPE_OVERFLOW_WRAPS (type)
)
	    {
	      if (!CONSTANT_CLASS_P (captures[1])
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail242;
		  {
		    tree res_op0;
		    {
		      tree _o1[2], _r1;
		      {
			tree _o2[1], _r2;
			_o2[0] = captures[0];
			_r2 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, _o2[0]);
			_o1[0] = _r2;
		      }
		      _o1[1] = captures[2];
		      _r1 = fold_build2_loc (loc, outer_op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      if (EXPR_P (_r1))
		        goto next_after_fail242;
		      res_op0 = _r1;
		    }
		    tree res_op1;
		    {
		      tree _o1[1], _r1;
		      _o1[0] = captures[1];
		      _r1 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, _o1[0]);
		      res_op1 = _r1;
		    }
		    tree _r;
		    _r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 112, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail242:;
		}
	    }
	  else
	    {
	      if (!ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 || TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[1]))
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail243;
		  {
		    tree res_op0;
		    {
		      tree _o1[2], _r1;
		      {
			tree _o2[2], _r2;
			_o2[0] = captures[0];
			{
			  tree _o3[1], _r3;
			  _o3[0] = captures[2];
			  _r3 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, TREE_TYPE (_o2[0]), _o3[0]);
			  _o2[1] = _r3;
			}
			_r2 = fold_build2_loc (loc, outer_op, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
			if (EXPR_P (_r2))
			  goto next_after_fail243;
			_o1[0] = _r2;
		      }
		      _o1[1] = captures[1];
		      _r1 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      res_op0 = _r1;
		    }
		    tree _r;
		    _r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 113, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail243:;
		}
	      else
		{
		  if (types_match (type, captures[1]) && !TYPE_OVERFLOW_SANITIZED (type)
)
		    {
		      {
 tree cst = const_binop (outer_op, type, captures[0], captures[2]);
			  if (cst && !TREE_OVERFLOW (cst)
)
			    {
			      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail244;
			      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail244;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail244;
			      {
				tree res_op0;
				res_op0 =  cst;
				tree res_op1;
				res_op1 = captures[1];
				tree _r;
				_r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 114, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail244:;
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
generic_simplify_23 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
	    {
	      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail250;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail250;
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
		_r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 120, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail250:;
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_27 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_SATURATING (type)
 && (!FLOAT_TYPE_P (type) || flag_associative_math)
)
    {
      if (!ALL_FRACT_MODE_P (TYPE_MODE (type))
)
	{
	  if (TREE_CODE (type) == INTEGER_TYPE
 && wi::neg_p (wi::to_wide (captures[3]))
 && wi::to_wide (captures[3]) != wi::min_value (TYPE_PRECISION (type), SIGNED)
)
	    {
	      {
 wide_int c0 = wi::to_wide (captures[1]);
 wide_int c2 = wi::to_wide (captures[3]);
 wide_int c2_abs = wi::abs (c2);
		  if (wi::multiple_of_p (c2_abs, c0, TYPE_SIGN (type))
)
		    {
		      {
 wide_int div_res = wi::sdiv_trunc (c2, c0);
 tree div_cst = wide_int_to_tree (type, div_res);
			  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail255;
			  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail255;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail255;
			  {
			    tree res_op0;
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[2];
			      _o1[1] =  div_cst;
			      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      res_op0 = _r1;
			    }
			    tree res_op1;
			    res_op1 = captures[1];
			    tree _r;
			    _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 125, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail255:;
		      }
		    }
	      }
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_34 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (POPCOUNT))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail262;
  {
    tree res_op0;
    {
      tree _o1[1], _r1;
      _o1[0] = captures[2];
      _r1 = maybe_build_call_expr_loc (loc, POPCOUNT, type, 1, _o1[0]);
      if (!_r1)
        goto next_after_fail262;
      res_op0 = _r1;
    }
    tree res_op1;
    {
      tree _o1[1], _r1;
      _o1[0] = captures[3];
      _r1 = maybe_build_call_expr_loc (loc, POPCOUNT, type, 1, _o1[0]);
      if (!_r1)
        goto next_after_fail262;
      res_op1 = _r1;
    }
    tree _r;
    _r = fold_build2_loc (loc, PLUS_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 132, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail262:;
  return NULL_TREE;
}

tree
generic_simplify_38 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 || (VECTOR_INTEGER_TYPE_P (type)
 && ((optimize_vectors_before_lowering_p ()
 && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST)
 || target_supports_op_p (type, TRUNC_MOD_EXPR,
 optab_vector)))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail266;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[0];
	  _o1[1] = captures[1];
	  _r1 = fold_build2_loc (loc, TRUNC_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree _r;
	_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 136, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail266:;
    }
  return NULL_TREE;
}

tree
generic_simplify_43 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_OVERFLOW_SANITIZED (type) && !TYPE_OVERFLOW_TRAPS (type)
 && !TYPE_SATURATING (type) && !TYPE_OVERFLOW_SANITIZED (TREE_TYPE (captures[0]))
 && !TYPE_OVERFLOW_TRAPS (TREE_TYPE (captures[0]))
 && !TYPE_SATURATING (TREE_TYPE (captures[0]))
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail271;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail271;
      {
	tree res_op0;
	{
	  tree _o1[1], _r1;
	  {
	    tree _o2[2], _r2;
	    _o2[0] = captures[1];
	    _o2[1] = captures[2];
	    _r2 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
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
	tree _r;
	_r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 141, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail271:;
    }
  return NULL_TREE;
}

tree
generic_simplify_52 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail282;
	  {
	    tree res_op0;
	    res_op0 = captures[1];
	    tree res_op1;
	    res_op1 = captures[2];
	    tree _r;
	    _r = fold_build2_loc (loc, PLUS_EXPR, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 151, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail282:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_57 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
 && element_precision (type) <= element_precision (TREE_TYPE (captures[1]))
 && element_precision (type) <= element_precision (TREE_TYPE (captures[2]))
)
	    {
	      {
 tree utype = unsigned_type_for (type);
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail290;
		  {
		    tree res_op0;
		    {
		      tree _o1[2], _r1;
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
		      {
			tree _o2[1], _r2;
			_o2[0] = captures[2];
			if (TREE_TYPE (_o2[0]) != utype)
			  {
			    _r2 = fold_build1_loc (loc, NOP_EXPR, utype, _o2[0]);
			  }
			else
			  _r2 = _o2[0];
			_o1[1] = _r2;
		      }
		      _r1 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      res_op0 = _r1;
		    }
		    tree _r;
		    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		    if (TREE_SIDE_EFFECTS (captures[3]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 159, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail290:;
	      }
	    }
	  else
	    {
	      if (((element_precision (type) <= element_precision (TREE_TYPE (captures[1])))
 == (element_precision (type) <= element_precision (TREE_TYPE (captures[2]))))
 && (element_precision (type) <= element_precision (TREE_TYPE (captures[1]))
 || (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[2]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[2]))))
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail291;
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
		    tree res_op1;
		    {
		      tree _o1[1], _r1;
		      _o1[0] = captures[2];
		      if (TREE_TYPE (_o1[0]) != type)
			{
			  _r1 = fold_build1_loc (loc, NOP_EXPR, type, _o1[0]);
			}
		      else
		        _r1 = _o1[0];
		      res_op1 = _r1;
		    }
		    tree _r;
		    _r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
		    if (TREE_SIDE_EFFECTS (captures[3]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 160, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail291:;
		}
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_76 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail310;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail310;
  {
    tree _r;
    _r = captures[2];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 177, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail310:;
  return NULL_TREE;
}

tree
generic_simplify_83 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail317;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree _r;
    _r = non_lvalue_loc (loc, res_op0);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 181, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail317:;
  return NULL_TREE;
}

tree
generic_simplify_90 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op),
 const enum tree_code ARG_UNUSED (rop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[1]))
 && tree_nop_conversion_p (type, TREE_TYPE (captures[2]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail324;
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
	tree res_op1;
	{
	  tree _o1[2], _r1;
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[1];
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
	    _o2[0] = captures[2];
	    if (TREE_TYPE (_o2[0]) != type)
	      {
		_r2 = fold_build1_loc (loc, NOP_EXPR, type, _o2[0]);
	      }
	    else
	      _r2 = _o2[0];
	    _o1[1] = _r2;
	  }
	  _r1 = fold_build2_loc (loc, op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, rop, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 187, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail324:;
    }
  return NULL_TREE;
}

tree
generic_simplify_100 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop),
 const enum tree_code ARG_UNUSED (rbitop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 bool wascmp;
      if (bitwise_inverted_equal_p (captures[2], captures[0], wascmp)
 && (!wascmp || element_precision (type) == 1)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail338;
	  {
	    tree res_op0;
	    res_op0 = captures[2];
	    tree res_op1;
	    res_op1 = captures[1];
	    tree _r;
	    _r = fold_build2_loc (loc, bitop, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 193, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail338:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_106 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop),
 const enum tree_code ARG_UNUSED (rbitop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail344;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail344;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 = captures[2];
    tree _r;
    _r = fold_build2_loc (loc, bitop, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 195, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail344:;
  return NULL_TREE;
}

tree
generic_simplify_112 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && (tree_nonzero_bits (captures[0]) & tree_nonzero_bits (captures[2])) == 0
)
    {
      {
 tree t = type;
 if (!TYPE_OVERFLOW_WRAPS (t))
 t = unsigned_type_for (t);
 wide_int c = wi::add (wi::to_wide (captures[3]), 1);
	  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail351;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail351;
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
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 201, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail351:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_117 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail361;
  {
    if (! tree_invariant_p (captures[1])) goto next_after_fail361;
    tree res_op0;
    {
      tree _o1[2], _r1;
      {
	tree _o2[2], _r2;
	_o2[0] = unshare_expr (captures[1]);
	_o2[1] = captures[4];
	_r2 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
	_o1[0] = _r2;
      }
      _o1[1] = captures[2];
      _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      res_op0 = _r1;
    }
    tree res_op1;
    res_op1 = captures[1];
    tree _r;
    _r = fold_build2_loc (loc, BIT_XOR_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 211, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail361:;
  return NULL_TREE;
}

tree
generic_simplify_125 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail388;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail388;
  {
    tree res_op0;
    {
      tree _o1[2], _r1;
      _o1[0] =  build_zero_cst (TREE_TYPE (captures[0]));
      _o1[1] = captures[3];
      _r1 = fold_build2_loc (loc, cmp, type, _o1[0], _o1[1]);
      res_op0 = _r1;
    }
    tree res_op1;
    res_op1 = captures[4];
    tree _r;
    _r = fold_build2_loc (loc, BIT_IOR_EXPR, type, res_op0, res_op1);
    if (TREE_SIDE_EFFECTS (captures[2]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 231, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail388:;
  return NULL_TREE;
}

tree
generic_simplify_129 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail392;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail392;
  {
    tree _r;
    _r = captures[2];
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 215, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail392:;
  return NULL_TREE;
}

tree
generic_simplify_133 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail409;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail409;
  {
    tree _r;
    _r =  constant_boolean_node (true, type);
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 233, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail409:;
  return NULL_TREE;
}

tree
generic_simplify_136 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[1]))
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail413;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail413;
      {
	tree res_op0;
	res_op0 = captures[3];
	tree res_op1;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  _o1[1] =  build_int_cst (TREE_TYPE (captures[1]), 1);
	  _r1 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, LE_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 216, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail413:;
    }
  return NULL_TREE;
}

tree
generic_simplify_139 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && (tree_nonzero_bits (captures[0]) & tree_nonzero_bits (captures[2])) == 0
)
    {
      {
 tree t = type;
 if (!TYPE_OVERFLOW_WRAPS (t))
 t = unsigned_type_for (t);
 wide_int c = wi::add (wi::to_wide (captures[3]), 1);
	  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail416;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail416;
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
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 201, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail416:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_142 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail419;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail419;
  {
    tree _r;
    _r = captures[1];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 239, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail419:;
  return NULL_TREE;
}

tree
generic_simplify_144 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (logic),
 const enum tree_code ARG_UNUSED (op),
 const enum tree_code ARG_UNUSED (ext))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TREE_CODE (captures[1]) != INTEGER_CST
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail421;
      {
	tree res_op0;
	res_op0 = captures[1];
	tree res_op1;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[2];
	  _o1[1] = captures[4];
	  _r1 = fold_build2_loc (loc, ext, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, op, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 240, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail421:;
    }
  return NULL_TREE;
}

tree
generic_simplify_151 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail428;
  {
    tree res_op0;
    res_op0 = captures[1];
    tree res_op1;
    {
      tree _o1[1], _r1;
      _o1[0] = captures[2];
      _r1 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
      res_op1 = _r1;
    }
    tree _r;
    _r = fold_build2_loc (loc, BIT_IOR_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 247, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail428:;
  return NULL_TREE;
}

tree
generic_simplify_155 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail432;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[0];
	    _r2 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
	    _o1[1] = _r2;
	  }
	  _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree _r;
	_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 251, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail432:;
    }
  return NULL_TREE;
}

tree
generic_simplify_163 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && (
0
 || !TREE_SIDE_EFFECTS (captures[1]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail440;
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
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 258, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail440:;
    }
  return NULL_TREE;
}

tree
generic_simplify_171 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (COPYSIGN_ALL))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail450;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail450;
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
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 266, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail450:;
    }
  return NULL_TREE;
}

tree
generic_simplify_178 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && !TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
    {
      if (TYPE_UNSIGNED (type)
)
	{
	  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail458;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail458;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree _r;
	    _r = fold_build1_loc (loc, ABSU_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 271, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail458:;
	}
      else
	{
	  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail459;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail459;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree _r;
	    _r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 272, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail459:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_188 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (POW))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail470;
      {
	tree res_op0;
	res_op0 = captures[1];
	tree res_op1;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[2];
	  _o1[1] = captures[4];
	  _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op1 = _r1;
	}
	tree _r;
	_r = maybe_build_call_expr_loc (loc, POW, type, 2, res_op0, res_op1);
	if (!_r)
	  goto next_after_fail470;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 282, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail470:;
    }
  return NULL_TREE;
}

tree
generic_simplify_195 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[0]))
 && (TREE_INT_CST_LOW (captures[1]) & 1) != 0
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail477;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 = captures[2];
	tree _r;
	_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 289, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail477:;
    }
  return NULL_TREE;
}

tree
generic_simplify_201 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (TREE_TYPE (captures[1]), TREE_TYPE (captures[2]))
 && tree_nop_conversion_p (TREE_TYPE (captures[0]), TREE_TYPE (captures[2]))
 && (CONSTANT_CLASS_P (captures[3]) || (single_use (captures[1]) && single_use (captures[0])))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail499;
      {
	tree res_op0;
	res_op0 = captures[3];
	tree res_op1;
	res_op1 =  build_zero_cst (TREE_TYPE (captures[3]));
	tree _r;
	_r = fold_build2_loc (loc, op, type, res_op0, res_op1);
	if (TREE_SIDE_EFFECTS (captures[2]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 310, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail499:;
    }
  return NULL_TREE;
}

tree
generic_simplify_208 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (ocmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TYPE_UNSIGNED (TREE_TYPE (captures[0]))
 && TREE_CODE (TREE_TYPE (captures[0])) != COMPLEX_TYPE
 && (VECTOR_TYPE_P (type) || !VECTOR_TYPE_P (TREE_TYPE (captures[0])))
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail506;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail506;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 = captures[1];
	tree _r;
	_r = fold_build2_loc (loc, ocmp, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 316, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail506:;
    }
  return NULL_TREE;
}

tree
generic_simplify_213 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (wi::lt_p (wi::to_wide (captures[1]), wi::to_wide (captures[2]),
 TYPE_SIGN (TREE_TYPE (captures[0])))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail511;
      {
	tree _r;
	_r =  constant_boolean_node (cmp == NE_EXPR, type);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (TREE_SIDE_EFFECTS (captures[2]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 321, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail511:;
    }
  else
    {
      if (wi::gt_p (wi::to_wide (captures[1]), wi::to_wide (captures[2]),
 TYPE_SIGN (TREE_TYPE (captures[0])))
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail512;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree res_op1;
	    res_op1 = captures[2];
	    tree _r;
	    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 322, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail512:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_221 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 int cand = wi::ctz (wi::to_wide (captures[2])) - wi::ctz (wi::to_wide (captures[0]));
      if (cand < 0
 || (!integer_zerop (captures[2])
 && wi::lshift (wi::to_wide (captures[0]), cand) != wi::to_wide (captures[2]))
)
	{
	  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail530;
	  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail530;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail530;
	  {
	    tree _r;
	    _r =  constant_boolean_node (cmp == NE_EXPR, type);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 340, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail530:;
	}
      else
	{
	  if (!integer_zerop (captures[2])
 && wi::lshift (wi::to_wide (captures[0]), cand) == wi::to_wide (captures[2])
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail531;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail531;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail531;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		res_op1 =  build_int_cst (TREE_TYPE (captures[1]), cand);
		tree _r;
		_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 341, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail531:;
	    }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_230 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (single_use (captures[0])
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail541;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail541;
      {
	tree res_op0;
	res_op0 = captures[1];
	tree res_op1;
	res_op1 = captures[2];
	tree _r;
	_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 351, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail541:;
    }
  return NULL_TREE;
}

tree
generic_simplify_235 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail551;
  {
    tree _r;
    _r =  constant_boolean_node (cmp == NE_EXPR, type);
    if (TREE_SIDE_EFFECTS (captures[0]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 360, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail551:;
  return NULL_TREE;
}

tree
generic_simplify_238 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (single_use (captures[1]) && single_use (captures[3])
)
    {
      {
 tree otype = TREE_TYPE (captures[0]);
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail554;
	  {
	    tree res_op0;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[4];
	      if (TREE_TYPE (_o1[0]) != otype)
		{
		  _r1 = fold_build1_loc (loc, NOP_EXPR, otype, _o1[0]);
		}
	      else
	        _r1 = _o1[0];
	      res_op0 = _r1;
	    }
	    tree res_op1;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[2];
	      if (TREE_TYPE (_o1[0]) != otype)
		{
		  _r1 = fold_build1_loc (loc, NOP_EXPR, otype, _o1[0]);
		}
	      else
	        _r1 = _o1[0];
	      res_op1 = _r1;
	    }
	    tree _r;
	    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 361, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail554:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_243 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (sq),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations && ! flag_errno_math
)
    {
      if (! HONOR_NANS (captures[0])
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail591;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree res_op1;
	    res_op1 = captures[1];
	    tree _r;
	    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 399, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail591:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_247 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail604;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 = captures[1];
	tree _r;
	_r = fold_build2_loc (loc, scmp, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 412, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail604:;
    }
  return NULL_TREE;
}

tree
generic_simplify_249 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && !(targetm.have_canonicalize_funcptr_for_compare ()
 && ((POINTER_TYPE_P (TREE_TYPE (captures[1]))
 && FUNC_OR_METHOD_TYPE_P (TREE_TYPE (TREE_TYPE (captures[1]))))
 || (POINTER_TYPE_P (TREE_TYPE (captures[3]))
 && FUNC_OR_METHOD_TYPE_P (TREE_TYPE (TREE_TYPE (captures[3]))))))
 && single_use (captures[0])
)
    {
      if (TYPE_PRECISION (TREE_TYPE (captures[1])) == TYPE_PRECISION (TREE_TYPE (captures[0]))
 && (TREE_CODE (captures[3]) == INTEGER_CST
 || captures[2] != captures[3])
 && (TYPE_UNSIGNED (TREE_TYPE (captures[1])) == TYPE_UNSIGNED (TREE_TYPE (captures[0]))
 || cmp == NE_EXPR
 || cmp == EQ_EXPR)
 && !POINTER_TYPE_P (TREE_TYPE (captures[1]))
 && (TREE_CODE (TREE_TYPE (captures[1])) != BOOLEAN_TYPE
 || TREE_CODE (TREE_TYPE (captures[3])) == BOOLEAN_TYPE)
)
	{
	  if (TREE_CODE (captures[2]) == INTEGER_CST
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail606;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail606;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		res_op1 =  force_fit_type (TREE_TYPE (captures[1]),
 wide_int::from (wi::to_wide (captures[2]),
 MAX (TYPE_PRECISION (TREE_TYPE (captures[2])),
 TYPE_PRECISION (TREE_TYPE (captures[1]))),
 TYPE_SIGN (TREE_TYPE (captures[2]))),
 0, TREE_OVERFLOW (captures[2]));
		tree _r;
		_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		if (TREE_SIDE_EFFECTS (captures[3]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 414, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail606:;
	    }
	  else
	    {
	      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail607;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail607;
	      {
		tree res_op0;
		res_op0 = captures[1];
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
		_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 415, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail607:;
	    }
	}
      else
	{
	  if (TYPE_PRECISION (TREE_TYPE (captures[0])) > TYPE_PRECISION (TREE_TYPE (captures[1]))
)
	    {
	      if ((cmp == EQ_EXPR || cmp == NE_EXPR
 || TYPE_UNSIGNED (TREE_TYPE (captures[0])) == TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 || (!TYPE_UNSIGNED (TREE_TYPE (captures[0]))
 && TYPE_UNSIGNED (TREE_TYPE (captures[1]))))
 && (types_match (TREE_TYPE (captures[3]), TREE_TYPE (captures[1]))
 || ((TYPE_PRECISION (TREE_TYPE (captures[1]))
 >= TYPE_PRECISION (TREE_TYPE (captures[3])))
 && (TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 == TYPE_UNSIGNED (TREE_TYPE (captures[3]))))
 || (TREE_CODE (captures[2]) == INTEGER_CST
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && int_fits_type_p (captures[2], TREE_TYPE (captures[1]))))
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail608;
		  {
		    tree res_op0;
		    res_op0 = captures[1];
		    tree res_op1;
		    {
		      tree _o1[1], _r1;
		      _o1[0] = captures[3];
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
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 416, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail608:;
		}
	      else
		{
		  if (TREE_CODE (captures[2]) == INTEGER_CST
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && !int_fits_type_p (captures[2], TREE_TYPE (captures[1]))
)
		    {
		      {
 tree min = lower_bound_in_type (TREE_TYPE (captures[3]), TREE_TYPE (captures[1]));
 tree max = upper_bound_in_type (TREE_TYPE (captures[3]), TREE_TYPE (captures[1]));
 bool above = integer_nonzerop (const_binop (LT_EXPR, type, max, captures[2]));
 bool below = integer_nonzerop (const_binop (LT_EXPR, type, captures[2], min));
			  if (above || below
)
			    {
			      if (cmp == EQ_EXPR || cmp == NE_EXPR
)
				{
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail609;
				  {
				    tree _r;
				    _r =  constant_boolean_node (cmp == EQ_EXPR ? false : true, type);
				    if (TREE_SIDE_EFFECTS (captures[1]))
				      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
				    if (TREE_SIDE_EFFECTS (captures[3]))
				      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 417, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail609:;
				}
			      else
				{
				  if (cmp == LT_EXPR || cmp == LE_EXPR
)
				    {
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail610;
				      {
					tree _r;
					_r =  constant_boolean_node (above ? true : false, type);
					if (TREE_SIDE_EFFECTS (captures[1]))
					  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
					if (TREE_SIDE_EFFECTS (captures[3]))
					  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 418, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail610:;
				    }
				  else
				    {
				      if (cmp == GT_EXPR || cmp == GE_EXPR
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail611;
					  {
					    tree _r;
					    _r =  constant_boolean_node (above ? false : true, type);
					    if (TREE_SIDE_EFFECTS (captures[1]))
					      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
					    if (TREE_SIDE_EFFECTS (captures[3]))
					      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 419, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail611:;
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
  else
    {
      if (FLOAT_TYPE_P (TREE_TYPE (captures[1]))
 && (DECIMAL_FLOAT_TYPE_P (TREE_TYPE (captures[0]))
 == DECIMAL_FLOAT_TYPE_P (TREE_TYPE (captures[1])))
 && (DECIMAL_FLOAT_TYPE_P (TREE_TYPE (captures[0]))
 == DECIMAL_FLOAT_TYPE_P (TREE_TYPE (captures[3])))
)
	{
	  {
 tree type1 = TREE_TYPE (captures[3]);
 if (TREE_CODE (captures[3]) == REAL_CST && !DECIMAL_FLOAT_TYPE_P (type1))
 {
 REAL_VALUE_TYPE orig = TREE_REAL_CST (captures[3]);
 if (TYPE_PRECISION (type1) > TYPE_PRECISION (float_type_node)
 && exact_real_truncate (TYPE_MODE (float_type_node), &orig))
 type1 = float_type_node;
 if (TYPE_PRECISION (type1) > TYPE_PRECISION (double_type_node)
 && exact_real_truncate (TYPE_MODE (double_type_node), &orig))
 type1 = double_type_node;
 }
 tree newtype
 = (element_precision (TREE_TYPE (captures[1])) > element_precision (type1)
 ? TREE_TYPE (captures[1]) : type1);
	      if (element_precision (TREE_TYPE (captures[0])) > element_precision (newtype)
 && (!VECTOR_TYPE_P (type) || is_truth_type_for (newtype, type))
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail612;
		  {
		    tree res_op0;
		    {
		      tree _o1[1], _r1;
		      _o1[0] = captures[1];
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
		      _o1[0] = captures[3];
		      if (TREE_TYPE (_o1[0]) != newtype)
			{
			  _r1 = fold_build1_loc (loc, NOP_EXPR, newtype, _o1[0]);
			}
		      else
		        _r1 = _o1[0];
		      res_op1 = _r1;
		    }
		    tree _r;
		    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 420, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail612:;
		}
	  }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_273 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op),
 const enum tree_code ARG_UNUSED (rop),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TREE_OVERFLOW (captures[2]) && !TREE_OVERFLOW (captures[3])
 && !TYPE_OVERFLOW_SANITIZED (TREE_TYPE (captures[1]))
 && !TYPE_OVERFLOW_TRAPS (TREE_TYPE (captures[1]))
 && !TYPE_SATURATING (TREE_TYPE (captures[1]))
)
    {
      {
 tree res = int_const_binop (rop, captures[3], captures[2]);
	  if (TREE_OVERFLOW (res)
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[1]))
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail663;
	      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail663;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail663;
	      {
		tree _r;
		_r =  constant_boolean_node (cmp == NE_EXPR, type);
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 454, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail663:;
	    }
	  else
	    {
	      if (single_use (captures[0])
)
		{
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail664;
		  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail664;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail664;
		  {
		    tree res_op0;
		    res_op0 = captures[1];
		    tree res_op1;
		    res_op1 =  TREE_OVERFLOW (res)
 ? drop_tree_overflow (res) : res;
		    tree _r;
		    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 455, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail664:;
		}
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_277 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op),
 const combined_fn ARG_UNUSED (ctz))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree type0 = TREE_TYPE (captures[1]);
 int prec = TYPE_PRECISION (type0);
      if (prec <= MAX_FIXED_MODE_SIZE
)
	{
	  if (tree_int_cst_sgn (captures[2]) < 0 || wi::to_widest (captures[2]) >= prec
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail678;
	      {
		tree _r;
		_r =  constant_boolean_node (op == EQ_EXPR ? false : true, type);
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (TREE_SIDE_EFFECTS (captures[2]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 469, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail678:;
	    }
	  else
	    {
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail679;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail679;
	      {
		tree res_op0;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[1];
		  _o1[1] =  wide_int_to_tree (type0,
 wi::mask (tree_to_uhwi (captures[2]) + 1,
 false, prec));
		  _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  res_op0 = _r1;
		}
		tree res_op1;
		res_op1 =  wide_int_to_tree (type0,
 wi::shifted_mask (tree_to_uhwi (captures[2]), 1,
 false, prec));
		tree _r;
		_r = fold_build2_loc (loc, op, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 470, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail679:;
	    }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_284 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (cmp != NE_EXPR
 || ! FLOAT_TYPE_P (TREE_TYPE (captures[0]))
 || ! tree_expr_maybe_nan_p (captures[0])
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail690;
      {
	tree _r;
	_r =  constant_boolean_node (false, type);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 481, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail690:;
    }
  return NULL_TREE;
}

tree
generic_simplify_288 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (COPYSIGN_ALL))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail694;
  {
    tree _r;
    _r = captures[0];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 485, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail694:;
  return NULL_TREE;
}

tree
generic_simplify_293 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (div))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!integer_zerop (captures[1])
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail701;
      {
	tree _r;
	_r = captures[0];
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 490, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail701:;
    }
  return NULL_TREE;
}

tree
generic_simplify_298 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (div))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((INTEGRAL_TYPE_P (type) || VECTOR_INTEGER_TYPE_P (type))
 && TYPE_OVERFLOW_UNDEFINED (type)
 && !integer_zerop (captures[0])
 && (!flag_non_call_exceptions || tree_expr_nonzero_p (captures[0]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail706;
      {
	tree _r;
	_r =  build_minus_one_cst (type);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 495, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail706:;
    }
  return NULL_TREE;
}

tree
generic_simplify_302 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (div))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && wi::multiple_of_p (wi::to_widest (captures[1]), wi::to_widest (captures[2]), SIGNED)
)
    {
      if (TYPE_OVERFLOW_UNDEFINED (type) && !TYPE_OVERFLOW_SANITIZED (type)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail711;
	  {
	    tree res_op0;
	    res_op0 = captures[1];
	    tree res_op1;
	    res_op1 = captures[2];
	    tree _r;
	    _r = fold_build2_loc (loc, div, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 500, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail711:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_311 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (FLOAT_TYPE_P (type)
 && ! HONOR_NANS (type)
 && ! HONOR_INFINITIES (type)
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail720;
      {
	tree _r;
	_r =  build_minus_one_cst (type);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 509, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail720:;
    }
  return NULL_TREE;
}

tree
generic_simplify_317 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (TANH),
 const combined_fn ARG_UNUSED (SINH),
 const combined_fn ARG_UNUSED (COSH))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail728;
      {
	tree res_op0;
	res_op0 = build_one_cst (type);
	tree res_op1;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[1];
	  _r1 = maybe_build_call_expr_loc (loc, COSH, TREE_TYPE (_o1[0]), 1, _o1[0]);
	  if (!_r1)
	    goto next_after_fail728;
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, RDIV_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 517, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail728:;
    }
  return NULL_TREE;
}

tree
generic_simplify_323 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail735;
      {
	tree _r;
	_r =  constant_boolean_node (cmp == LT_EXPR, type);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 524, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail735:;
    }
  return NULL_TREE;
}

tree
generic_simplify_330 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (bitop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
)
    {
      if (bitop == BIT_AND_EXPR || tree_expr_nonnegative_p (captures[1])
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail743;
	  {
	    tree _r;
	    _r =  constant_boolean_node (cmp == GE_EXPR || cmp == LE_EXPR, type);
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 532, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail743:;
	}
      else
	{
	  if (TREE_CODE (captures[1]) == INTEGER_CST && wi::neg_p (wi::to_wide (captures[1]))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail744;
	      {
		tree _r;
		_r =  constant_boolean_node (cmp == LT_EXPR, type);
		if (TREE_SIDE_EFFECTS (captures[0]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 533, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail744:;
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_342 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (out))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 && types_match (TREE_TYPE (captures[1]), TREE_TYPE (captures[2]))
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail788;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail788;
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
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 560, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail788:;
    }
  return NULL_TREE;
}

tree
generic_simplify_348 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 bool ok = true;
 HOST_WIDE_INT val = 0;
 if (!tree_fits_shwi_p (captures[3]))
 ok = false;
 else
 {
 val = tree_to_shwi (captures[3]);
 if (op == GT_EXPR || op == LE_EXPR)
 {
 if (val == HOST_WIDE_INT_MAX)
 ok = false;
 else
 val++;
 }
 }
 HOST_WIDE_INT zero_val = tree_to_shwi (captures[2]);
 tree type0 = TREE_TYPE (captures[1]);
 int prec = TYPE_PRECISION (type0);
 if (prec > MAX_FIXED_MODE_SIZE)
 ok = false;
      if (val <= 0
)
	{
	  if (ok && zero_val >= val
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail799;
	      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail799;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail799;
	      {
		tree _r;
		_r =  constant_boolean_node (cmp == EQ_EXPR ? true : false, type);
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 571, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail799:;
	    }
	}
      else
	{
	  if (val >= prec
)
	    {
	      if (ok && zero_val < val
)
		{
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail800;
		  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail800;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail800;
		  {
		    tree _r;
		    _r =  constant_boolean_node (cmp == EQ_EXPR ? false : true, type);
		    if (TREE_SIDE_EFFECTS (captures[1]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 572, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail800:;
		}
	    }
	  else
	    {
	      if (ok && (zero_val < 0 || zero_val >= prec)
)
		{
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail801;
		  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail801;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail801;
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
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 573, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail801:;
		}
	    }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_360 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (wi::ltu_p (wi::to_wide (captures[1]), element_precision (type))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail817;
      {
	tree res_op0;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  _r1 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, _o1[0]);
	  res_op0 = _r1;
	}
	tree res_op1;
	{
	  tree _o1[2], _r1;
	  _o1[0] =  build_minus_one_cst (type);
	  _o1[1] = captures[1];
	  _r1 = fold_build2_loc (loc, LSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 585, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail817:;
    }
  return NULL_TREE;
}

tree
generic_simplify_367 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bit_op),
 const enum tree_code ARG_UNUSED (shift))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[2]))
)
    {
      {
 tree mask = int_const_binop (shift, fold_convert (type, captures[3]), captures[4]);
	  if (mask
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail827;
	      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail827;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail827;
	      {
		tree res_op0;
		{
		  tree _o1[2], _r1;
		  {
		    tree _o2[1], _r2;
		    _o2[0] = captures[2];
		    if (TREE_TYPE (_o2[0]) != type)
		      {
			_r2 = fold_build1_loc (loc, NOP_EXPR, type, _o2[0]);
		      }
		    else
		      _r2 = _o2[0];
		    _o1[0] = _r2;
		  }
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, shift, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  res_op0 = _r1;
		}
		tree res_op1;
		res_op1 =  mask;
		tree _r;
		_r = fold_build2_loc (loc, bit_op, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 593, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail827:;
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_372 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (minmax))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && types_match (captures[1], type) && int_fits_type_p (captures[2], type)
 && TYPE_SIGN (TREE_TYPE (captures[0])) == TYPE_SIGN (type)
 && TYPE_PRECISION (TREE_TYPE (captures[0])) > TYPE_PRECISION (type)
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail837;
      {
	tree res_op0;
	res_op0 = captures[1];
	tree res_op1;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[2];
	  if (TREE_TYPE (_o1[0]) != type)
	    {
	      _r1 = fold_build1_loc (loc, NOP_EXPR, type, _o1[0]);
	    }
	  else
	    _r1 = _o1[0];
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, minmax, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 603, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail837:;
    }
  return NULL_TREE;
}

tree
generic_simplify_375 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail846;
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
next_after_fail846:;
	}
      else
	{
	  {
 tree utype = unsigned_type_for (TREE_TYPE (captures[2]));
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail847;
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
next_after_fail847:;
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
			  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail848;
			  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail848;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail848;
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
next_after_fail848:;
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
				  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail849;
				  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail849;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail849;
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
next_after_fail849:;
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
generic_simplify_403 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail880;
  {
    tree res_op0;
    {
      tree _o1[2], _r1;
      _o1[0] = captures[1];
      _o1[1] = captures[2];
      _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      res_op0 = _r1;
    }
    tree _r;
    _r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 633, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail880:;
  return NULL_TREE;
}

tree
generic_simplify_407 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail884;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail884;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree res_op1;
	    res_op1 = captures[3];
	    tree res_op2;
	    res_op2 =  build_zero_cst (type);
	    tree _r;
	    _r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 636, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail884:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_416 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail906;
	      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail906;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail906;
	      {
		tree _r;
		_r = captures[0];
		if (TREE_SIDE_EFFECTS (captures[4]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[4]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 648, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail906:;
	    }
	  else
	    {
	      if (code1 == EQ_EXPR && !val
)
		{
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail907;
		  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail907;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail907;
		  {
		    tree _r;
		    _r =  constant_boolean_node (false, type);
		    if (TREE_SIDE_EFFECTS (captures[4]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[4]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 649, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail907:;
		}
	      else
		{
		  if (code1 == NE_EXPR && !val && allbits
)
		    {
		      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail908;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail908;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail908;
		      {
			tree _r;
			_r = captures[3];
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 650, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail908:;
		    }
		  else
		    {
		      if (code1 == NE_EXPR
 && code2 == GE_EXPR
 && cmp == 0
 && allbits
)
			{
			  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail909;
			  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail909;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail909;
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
next_after_fail909:;
			}
		      else
			{
			  if (code1 == NE_EXPR
 && code2 == LE_EXPR
 && cmp == 0
 && allbits
)
			    {
			      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail910;
			      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail910;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail910;
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
next_after_fail910:;
			    }
			  else
			    {
			      if (code1 == NE_EXPR
 && code2 == GT_EXPR
 && one_after
 && allbits
)
				{
				  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail911;
				  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail911;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail911;
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
next_after_fail911:;
				}
			      else
				{
				  if (code1 == NE_EXPR
 && code2 == LT_EXPR
 && one_before
 && allbits
)
				    {
				      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail912;
				      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail912;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail912;
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
next_after_fail912:;
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
generic_simplify_440 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_UNSIGNED (TREE_TYPE (captures[2]))
 && (element_precision (TREE_TYPE (captures[2]))
 <= element_precision (TREE_TYPE (captures[3]))
 || !TYPE_UNSIGNED (TREE_TYPE (captures[3])))
)
    {
      {
 tree shift_type = TREE_TYPE (captures[2]);
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail961;
	  {
	    tree res_op0;
	    {
	      tree _o1[2], _r1;
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[3];
		if (TREE_TYPE (_o2[0]) != shift_type)
		  {
		    _r2 = fold_build1_loc (loc, NOP_EXPR, shift_type, _o2[0]);
		  }
		else
		  _r2 = _o2[0];
		_o1[0] = _r2;
	      }
	      _o1[1] = captures[4];
	      _r1 = fold_build2_loc (loc, RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 675, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail961:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_446 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (minmax),
 const enum tree_code ARG_UNUSED (maxmin))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail967;
  {
    tree res_op0;
    res_op0 = captures[1];
    tree res_op1;
    {
      tree _o1[1], _r1;
      _o1[0] = captures[2];
      _r1 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
      res_op1 = _r1;
    }
    tree _r;
    _r = fold_build2_loc (loc, maxmin, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 680, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail967:;
  return NULL_TREE;
}

tree
generic_simplify_452 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail975;
  {
    tree res_op0;
    {
      tree _o1[1], _r1;
      _o1[0] = captures[0];
      _r1 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, _o1[0]);
      res_op0 = _r1;
    }
    tree res_op1;
    res_op1 =  build_each_one_cst (type);
    tree _r;
    _r = fold_build2_loc (loc, PLUS_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 687, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail975:;
  return NULL_TREE;
}

tree
generic_simplify_460 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
		  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail983;
		  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail983;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail983;
		  {
		    tree _r;
		    _r = captures[0];
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 694, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail983:;
		}
	      else
		{
		  if (known_gt (off0, off1)
)
		    {
		      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail984;
		      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail984;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail984;
		      {
			tree _r;
			_r = captures[2];
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 695, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail984:;
		    }
		}
	    }
	  else
	    {
	      if (known_ge (off0, off1)
)
		{
		  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail985;
		  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail985;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail985;
		  {
		    tree _r;
		    _r = captures[0];
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 696, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail985:;
		}
	      else
		{
		  if (known_lt (off0, off1)
)
		    {
		      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail986;
		      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail986;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail986;
		      {
			tree _r;
			_r = captures[2];
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 697, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail986:;
		    }
		}
	    }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_477 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && TYPE_PRECISION (type) > 1
 && (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])))
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1017;
      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1017;
      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1017;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1017;
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
	  _o1[1] = captures[2];
	  _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree res_op1;
	res_op1 = captures[1];
	tree _r;
	_r = fold_build2_loc (loc, op, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 716, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1017:;
    }
  return NULL_TREE;
}

tree
generic_simplify_486 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (bitwise_equal_p (captures[0], captures[3])
 && expr_no_side_effects_p (captures[2])
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1026;
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1026;
      if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1026;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1026;
      {
	tree _r;
	_r = captures[1];
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 725, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1026:;
    }
  return NULL_TREE;
}

tree
generic_simplify_495 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (func))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (wi::to_widest (captures[4]) == TYPE_PRECISION (TREE_TYPE (captures[3])) - 1
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1036;
      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1036;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1036;
      {
	tree _r;
	_r = captures[2];
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 734, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1036:;
    }
  return NULL_TREE;
}

tree
generic_simplify_502 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (bswap))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1047;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree _r;
    _r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 745, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1047:;
  return NULL_TREE;
}

tree
generic_simplify_506 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1051;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1051;
  {
    tree _r;
    _r = captures[2];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 728, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1051:;
  return NULL_TREE;
}

tree
generic_simplify_512 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (cond_op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree op_type = TREE_TYPE (captures[6]);
      if (inverse_conditions_p (captures[0], captures[2])
 && element_precision (type) == element_precision (op_type)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1057;
	  {
	    tree res_op0;
	    {
	      tree _o1[5], _r1;
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
	      _r1 = maybe_build_call_expr_loc (loc, cond_op, TREE_TYPE (_o1[1]), 5, _o1[0], _o1[1], _o1[2], _o1[3], _o1[4]);
	      if (!_r1)
	        goto next_after_fail1057;
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (TREE_SIDE_EFFECTS (captures[6]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[6]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 750, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1057:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_521 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1066;
  {
    tree _r;
    _r =  constant_boolean_node (true, type);
    if (TREE_SIDE_EFFECTS (captures[0]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 759, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1066:;
  return NULL_TREE;
}

tree
generic_simplify_526 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (logs),
 const combined_fn ARG_UNUSED (pows))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1071;
      {
	tree res_op0;
	res_op0 = captures[1];
	tree res_op1;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  _r1 = maybe_build_call_expr_loc (loc, logs, TREE_TYPE (_o1[0]), 1, _o1[0]);
	  if (!_r1)
	    goto next_after_fail1071;
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 764, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1071:;
    }
  return NULL_TREE;
}

tree
generic_simplify_535 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1080;
  {
    tree _r;
    _r = captures[0];
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 773, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1080:;
  return NULL_TREE;
}

tree
generic_simplify_539 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (floors))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (canonicalize_math_p ()
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1084;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree _r;
	_r = fold_build1_loc (loc, FIX_TRUNC_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 777, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1084:;
    }
  return NULL_TREE;
}

tree
generic_simplify_547 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (fmas))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (canonicalize_math_after_vectorization_p ()
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1092;
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
	  goto next_after_fail1092;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 785, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1092:;
    }
  return NULL_TREE;
}

tree
generic_simplify_556 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (ctz))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree t = TREE_TYPE (captures[0]);
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1101;
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
	  goto next_after_fail1101;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 793, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1101:;
  }
  return NULL_TREE;
}

tree
generic_simplify_565 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1111;
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
	      goto next_after_fail1111;
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 799, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1111:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_BIT_NOT_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case BIT_AND_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q21 };
		tree res = generic_simplify_435 (loc, type, _p0, captures);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q21))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q40, _q20 };
		tree res = generic_simplify_435 (loc, type, _p0, captures);
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
	switch (TREE_CODE (_q20))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q21 };
		tree res = generic_simplify_436 (loc, type, _p0, captures);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q21))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q40, _q20 };
		tree res = generic_simplify_436 (loc, type, _p0, captures);
		if (res) return res;
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
	  case BIT_NOT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q21 };
		if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1147;
		{
		  tree res_op0;
		  res_op0 = captures[0];
		  tree res_op1;
		  res_op1 = captures[1];
		  tree _r;
		  _r = fold_build2_loc (loc, PLUS_EXPR, type, res_op0, res_op1);
		  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 826, __FILE__, __LINE__, true);
		  return _r;
		}
next_after_fail1147:;
	      }
	      break;
	    }
          default:;
          }
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1148;
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
	    _r = fold_build2_loc (loc, PLUS_EXPR, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 827, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1148:;
	}
	if (integer_each_onep (_q21))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	      tree res = generic_simplify_437 (loc, type, _p0, captures);
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
	  case BIT_NOT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q21 };
		tree res = generic_simplify_438 (loc, type, _p0, captures);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q21))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
		tree res = generic_simplify_438 (loc, type, _p0, captures);
		if (res) return res;
	      }
	      break;
	    }
	  case INTEGER_CST:
	    {
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		if ((INTEGRAL_TYPE_P (type)
 && TYPE_UNSIGNED (type))
 || (!TYPE_OVERFLOW_SANITIZED (type)
 && may_negate_without_overflow_p (captures[2]))
)
		  {
		    if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1149;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1149;
		    {
		      tree res_op0;
		      {
			tree _o1[1], _r1;
			_o1[0] = captures[1];
			_r1 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
			res_op0 = _r1;
		      }
		      tree res_op1;
		      res_op1 =  const_unop (NEGATE_EXPR, type, captures[2]);
		      tree _r;
		      _r = fold_build2_loc (loc, PLUS_EXPR, type, res_op0, res_op1);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 828, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1149:;
		  }
	      }
	      break;
	    }
          default:;
          }
      if (integer_all_onesp (_q21))
	{
	  {
	    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	    tree res = generic_simplify_439 (loc, type, _p0, captures);
	    if (res) return res;
	  }
        }
        break;
      }
    case RSHIFT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	  if (!TYPE_UNSIGNED (TREE_TYPE (captures[1]))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1150;
	      {
		tree res_op0;
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[1];
		  _r1 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1150;
		  res_op0 = _r1;
		}
		tree res_op1;
		res_op1 = captures[2];
		tree _r;
		_r = fold_build2_loc (loc, RSHIFT_EXPR, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 829, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1150:;
	    }
	  else
	    {
	      if (INTEGRAL_TYPE_P (type)
 && !wi::neg_p (tree_nonzero_bits (captures[1]))
)
		{
		  {
 tree stype = signed_type_for (TREE_TYPE (captures[1]));
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1151;
		      {
			tree res_op0;
			{
			  tree _o1[2], _r1;
			  {
			    tree _o2[1], _r2;
			    {
			      tree _o3[1], _r3;
			      _o3[0] = captures[1];
			      if (TREE_TYPE (_o3[0]) != stype)
				{
				  _r3 = fold_build1_loc (loc, NOP_EXPR, stype, _o3[0]);
				}
			      else
			        _r3 = _o3[0];
			      _o2[0] = _r3;
			    }
			    _r2 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
			    if (EXPR_P (_r2))
			      goto next_after_fail1151;
			    _o1[0] = _r2;
			  }
			  _o1[1] = captures[2];
			  _r1 = fold_build2_loc (loc, RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			  res_op0 = _r1;
			}
			tree _r;
			_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 830, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1151:;
		  }
		}
	    }
	}
	switch (TREE_CODE (_q20))
	  {
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_q30))
	        {
		case BIT_NOT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    {
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q40, _q21 };
		      tree res = generic_simplify_440 (loc, type, _p0, captures);
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
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      {
		tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q30, _q21 };
		tree res = generic_simplify_440 (loc, type, _p0, captures);
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1152;
	  {
	    tree _r;
	    _r = captures[0];
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 831, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1152:;
	}
        break;
      }
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case NEGATE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      {
		tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
		tree res = generic_simplify_441 (loc, type, _p0, captures);
		if (res) return res;
	      }
	      break;
	    }
	  case MINUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_each_onep (_q31))
		{
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
		    tree res = generic_simplify_437 (loc, type, _p0, captures);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case PLUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_all_onesp (_q31))
		{
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
		    tree res = generic_simplify_439 (loc, type, _p0, captures);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case BIT_XOR_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_q31))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
		      tree res = generic_simplify_442 (loc, type, _p0, captures);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q30))
	        {
		case BIT_NOT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q31 };
		      tree res = generic_simplify_443 (loc, type, _p0, captures);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q31))
	        {
		case BIT_NOT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q31, 0);
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q50, _q30 };
		      tree res = generic_simplify_443 (loc, type, _p0, captures);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
	  case RSHIFT_EXPR:
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
		      case BIT_NOT_EXPR:
		        {
			  tree _q50 = TREE_OPERAND (_q40, 0);
			  {
			    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q50, _q31 };
			    tree res = generic_simplify_444 (loc, type, _p0, captures);
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
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    {
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q40, _q31 };
		      tree res = generic_simplify_444 (loc, type, _p0, captures);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
	  case LROTATE_EXPR:
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
		      case BIT_NOT_EXPR:
		        {
			  tree _q50 = TREE_OPERAND (_q40, 0);
			  {
			    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q50, _q31 };
			    tree res = generic_simplify_445 (loc, type, _p0, captures, LROTATE_EXPR);
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
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    {
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q40, _q31 };
		      tree res = generic_simplify_445 (loc, type, _p0, captures, LROTATE_EXPR);
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
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_q30))
	        {
		CASE_CONVERT:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    switch (TREE_CODE (_q40))
		      {
		      case BIT_NOT_EXPR:
		        {
			  tree _q50 = TREE_OPERAND (_q40, 0);
			  {
			    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q50, _q31 };
			    tree res = generic_simplify_445 (loc, type, _p0, captures, RROTATE_EXPR);
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
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    {
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q40, _q31 };
		      tree res = generic_simplify_445 (loc, type, _p0, captures, RROTATE_EXPR);
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
    case NEGATE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	  tree res = generic_simplify_441 (loc, type, _p0, captures);
	  if (res) return res;
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
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		tree res = generic_simplify_442 (loc, type, _p0, captures);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q20))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q21 };
		tree res = generic_simplify_443 (loc, type, _p0, captures);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q21))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
		tree res = generic_simplify_443 (loc, type, _p0, captures);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	if (tree_truth_valued_p (_q20))
	  {
	    if (tree_truth_valued_p (_q21))
	      {
		{
		  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_PRECISION (TREE_TYPE (captures[1])) == 1
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1153;
		      {
			tree res_op0;
			{
			  tree _o1[2], _r1;
			  _o1[0] = captures[1];
			  _o1[1] = captures[2];
			  _r1 = fold_build2_loc (loc, EQ_EXPR, boolean_type_node, _o1[0], _o1[1]);
			  res_op0 = _r1;
			}
			tree _r;
			_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 832, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1153:;
		    }
		}
	      }
	  }
        break;
      }
    case MIN_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q21 };
		tree res = generic_simplify_446 (loc, type, _p0, captures, MIN_EXPR, MAX_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q21))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q40, _q20 };
		tree res = generic_simplify_446 (loc, type, _p0, captures, MIN_EXPR, MAX_EXPR);
		if (res) return res;
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
	switch (TREE_CODE (_q20))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q21 };
		tree res = generic_simplify_446 (loc, type, _p0, captures, MAX_EXPR, MIN_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q21))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q40, _q20 };
		tree res = generic_simplify_446 (loc, type, _p0, captures, MAX_EXPR, MIN_EXPR);
		if (res) return res;
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
	switch (TREE_CODE (_q20))
	  {
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_q30))
	        {
		case BIT_NOT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    {
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q40, _q21 };
		      tree res = generic_simplify_447 (loc, type, _p0, captures, LROTATE_EXPR);
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
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      {
		tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q30, _q21 };
		tree res = generic_simplify_447 (loc, type, _p0, captures, LROTATE_EXPR);
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
	switch (TREE_CODE (_q20))
	  {
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_q30))
	        {
		case BIT_NOT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    {
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q40, _q21 };
		      tree res = generic_simplify_447 (loc, type, _p0, captures, RROTATE_EXPR);
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
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      {
		tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q30, _q21 };
		tree res = generic_simplify_447 (loc, type, _p0, captures, RROTATE_EXPR);
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
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q22 };
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1154;
	  {
	    tree res_op0;
	    res_op0 = captures[1];
	    tree res_op1;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[2];
	      _r1 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	      if (EXPR_P (_r1))
	        goto next_after_fail1154;
	      res_op1 = _r1;
	    }
	    tree res_op2;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[3];
	      _r1 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	      if (EXPR_P (_r1))
	        goto next_after_fail1154;
	      res_op2 = _r1;
	    }
	    tree _r;
	    _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 808, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1154:;
	}
        break;
      }
    case LT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	  tree res = generic_simplify_448 (loc, type, _p0, captures, LT_EXPR, GE_EXPR, UNGE_EXPR);
	  if (res) return res;
	}
        break;
      }
    case LE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	  tree res = generic_simplify_448 (loc, type, _p0, captures, LE_EXPR, GT_EXPR, UNGT_EXPR);
	  if (res) return res;
	}
        break;
      }
    case EQ_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	  tree res = generic_simplify_448 (loc, type, _p0, captures, EQ_EXPR, NE_EXPR, NE_EXPR);
	  if (res) return res;
	}
        break;
      }
    case NE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	  tree res = generic_simplify_448 (loc, type, _p0, captures, NE_EXPR, EQ_EXPR, EQ_EXPR);
	  if (res) return res;
	}
        break;
      }
    case GE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	  tree res = generic_simplify_448 (loc, type, _p0, captures, GE_EXPR, LT_EXPR, UNLT_EXPR);
	  if (res) return res;
	}
        break;
      }
    case GT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	  tree res = generic_simplify_448 (loc, type, _p0, captures, GT_EXPR, LE_EXPR, UNLE_EXPR);
	  if (res) return res;
	}
        break;
      }
    case UNORDERED_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	  tree res = generic_simplify_448 (loc, type, _p0, captures, UNORDERED_EXPR, ORDERED_EXPR, ORDERED_EXPR);
	  if (res) return res;
	}
        break;
      }
    case ORDERED_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	  tree res = generic_simplify_448 (loc, type, _p0, captures, ORDERED_EXPR, UNORDERED_EXPR, UNORDERED_EXPR);
	  if (res) return res;
	}
        break;
      }
    case UNLT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	  tree res = generic_simplify_448 (loc, type, _p0, captures, UNLT_EXPR, GE_EXPR, GE_EXPR);
	  if (res) return res;
	}
        break;
      }
    case UNLE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	  tree res = generic_simplify_448 (loc, type, _p0, captures, UNLE_EXPR, GT_EXPR, GT_EXPR);
	  if (res) return res;
	}
        break;
      }
    case UNGT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	  tree res = generic_simplify_448 (loc, type, _p0, captures, UNGT_EXPR, LE_EXPR, LE_EXPR);
	  if (res) return res;
	}
        break;
      }
    case UNGE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	  tree res = generic_simplify_448 (loc, type, _p0, captures, UNGE_EXPR, LT_EXPR, LT_EXPR);
	  if (res) return res;
	}
        break;
      }
    case UNEQ_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	  tree res = generic_simplify_448 (loc, type, _p0, captures, UNEQ_EXPR, LTGT_EXPR, LTGT_EXPR);
	  if (res) return res;
	}
        break;
      }
    case LTGT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	  tree res = generic_simplify_448 (loc, type, _p0, captures, LTGT_EXPR, UNEQ_EXPR, UNEQ_EXPR);
	  if (res) return res;
	}
        break;
      }
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_TRUNC_DIV_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_172 (loc, type, _p0, _p1, captures, TRUNC_DIV_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p0))
    {
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
		case LSHIFT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    if (integer_onep (_q50))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q50, _q51 };
			  tree res = generic_simplify_291 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
	  case LSHIFT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      tree _q41 = TREE_OPERAND (_p1, 1);
	      if (integer_onep (_q40))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q40, _q41 };
		    tree res = generic_simplify_291 (loc, type, _p0, _p1, captures);
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
	  case LSHIFT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if (integer_onep (_q40))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q40, _q41 };
		    tree res = generic_simplify_292 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case LSHIFT_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if (integer_onep (_q30))
	  {
	    {
	      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q30, _q31 };
	      tree res = generic_simplify_292 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	tree res = generic_simplify_293 (loc, type, _p0, _p1, captures, TRUNC_DIV_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  tree res = generic_simplify_295 (loc, type, _p0, _p1, captures, TRUNC_DIV_EXPR);
	  if (res) return res;
	}
        break;
      }
    default:;
    }
if (integer_minus_onep (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      tree res = generic_simplify_294 (loc, type, _p0, _p1, captures, TRUNC_DIV_EXPR);
      if (res) return res;
    }
  }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_296 (loc, type, _p0, _p1, captures, TRUNC_DIV_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case ABS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	      tree res = generic_simplify_297 (loc, type, _p0, _p1, captures, TRUNC_DIV_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case ABS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	      tree res = generic_simplify_297 (loc, type, _p0, _p1, captures, TRUNC_DIV_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case NEGATE_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	      tree res = generic_simplify_298 (loc, type, _p0, _p1, captures, TRUNC_DIV_EXPR);
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
	      tree res = generic_simplify_298 (loc, type, _p0, _p1, captures, TRUNC_DIV_EXPR);
	      if (res) return res;
	    }
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
		      tree res = generic_simplify_299 (loc, type, _p0, _p1, captures, TRUNC_DIV_EXPR);
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
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case BIT_AND_EXPR:
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
			    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
			    tree res = generic_simplify_300 (loc, type, _p0, _p1, captures, TRUNC_DIV_EXPR);
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
    case BIT_AND_EXPR:
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
		      tree res = generic_simplify_300 (loc, type, _p0, _p1, captures, TRUNC_DIV_EXPR);
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
    case MULT_EXPR:
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
		      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1618;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1618;
		      {
			tree res_op0;
			res_op0 = captures[0];
			tree res_op1;
			res_op1 =  wide_int_to_tree
 (type, wi::mask (TYPE_PRECISION (type)
 - wi::exact_log2 (wi::to_wide (captures[1])),
 false, TYPE_PRECISION (type)));
			tree _r;
			_r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 960, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1618:;
		    }
		}
	      }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_301 (loc, type, _p0, _p1, captures, TRUNC_DIV_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_301 (loc, type, _p0, _p1, captures, TRUNC_DIV_EXPR);
	      if (res) return res;
	    }
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
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			switch (TREE_CODE (_q51))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
				tree res = generic_simplify_302 (loc, type, _p0, _p1, captures, TRUNC_DIV_EXPR);
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
 && (TREE_CODE_CLASS (TRUNC_DIV_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1619;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, TRUNC_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1619;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, TRUNC_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1619;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1619:;
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
 && (TREE_CODE_CLASS (TRUNC_DIV_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1620;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1620;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, TRUNC_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1620;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, TRUNC_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1620;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1620:;
	    }
	}
        break;
      }
    default:;
    }
if (integer_onep (_p0))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (INTEGRAL_TYPE_P (type)
 && TYPE_PRECISION (type) > 1
 && !integer_zerop (captures[1])
 && (!flag_non_call_exceptions || tree_expr_nonzero_p (captures[1]))
)
	{
	  if (TYPE_UNSIGNED (type)
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1621;
	      {
		tree res_op0;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[1];
		  _o1[1] =  build_one_cst (type);
		  _r1 = fold_build2_loc (loc, EQ_EXPR, boolean_type_node, _o1[0], _o1[1]);
		  res_op0 = _r1;
		}
		tree _r;
		_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		if (TREE_SIDE_EFFECTS (captures[0]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 961, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1621:;
	    }
	  else
	    {
	      if (!canonicalize_math_p ()
)
		{
		  {
 tree utype = unsigned_type_for (type);
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1622;
		      {
			if (! tree_invariant_p (captures[1])) goto next_after_fail1622;
			tree res_op0;
			{
			  tree _o1[2], _r1;
			  {
			    tree _o2[2], _r2;
			    {
			      tree _o3[1], _r3;
			      _o3[0] = unshare_expr (captures[1]);
			      if (TREE_TYPE (_o3[0]) != utype)
				{
				  _r3 = fold_build1_loc (loc, NOP_EXPR, utype, _o3[0]);
				}
			      else
			        _r3 = _o3[0];
			      _o2[0] = _r3;
			    }
			    _o2[1] =  build_one_cst (utype);
			    _r2 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
			    _o1[0] = _r2;
			  }
			  _o1[1] =  build_int_cst (utype, 2);
			  _r1 = fold_build2_loc (loc, LE_EXPR, boolean_type_node, _o1[0], _o1[1]);
			  res_op0 = _r1;
			}
			tree res_op1;
			res_op1 = captures[1];
			tree res_op2;
			res_op2 =  build_zero_cst (type);
			tree _r;
			_r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
			if (TREE_SIDE_EFFECTS (captures[0]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 962, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1622:;
		  }
		}
	    }
	}
    }
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
 && (TREE_CODE_CLASS (TRUNC_DIV_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1623;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1623;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, TRUNC_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1623;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, TRUNC_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1623;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1623:;
	    }
	}
        break;
      }
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_ROUND_DIV_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_172 (loc, type, _p0, _p1, captures, ROUND_DIV_EXPR);
	if (res) return res;
      }
    }
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	tree res = generic_simplify_293 (loc, type, _p0, _p1, captures, ROUND_DIV_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  tree res = generic_simplify_295 (loc, type, _p0, _p1, captures, ROUND_DIV_EXPR);
	  if (res) return res;
	}
        break;
      }
    default:;
    }
if (integer_minus_onep (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      tree res = generic_simplify_294 (loc, type, _p0, _p1, captures, ROUND_DIV_EXPR);
      if (res) return res;
    }
  }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_296 (loc, type, _p0, _p1, captures, ROUND_DIV_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case ABS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	      tree res = generic_simplify_297 (loc, type, _p0, _p1, captures, ROUND_DIV_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case ABS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	      tree res = generic_simplify_297 (loc, type, _p0, _p1, captures, ROUND_DIV_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case NEGATE_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	      tree res = generic_simplify_298 (loc, type, _p0, _p1, captures, ROUND_DIV_EXPR);
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
	      tree res = generic_simplify_298 (loc, type, _p0, _p1, captures, ROUND_DIV_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case BIT_AND_EXPR:
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
			    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
			    tree res = generic_simplify_300 (loc, type, _p0, _p1, captures, ROUND_DIV_EXPR);
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
    case BIT_AND_EXPR:
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
		      tree res = generic_simplify_300 (loc, type, _p0, _p1, captures, ROUND_DIV_EXPR);
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
    case MULT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_301 (loc, type, _p0, _p1, captures, ROUND_DIV_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_301 (loc, type, _p0, _p1, captures, ROUND_DIV_EXPR);
	      if (res) return res;
	    }
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
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			switch (TREE_CODE (_q51))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
				tree res = generic_simplify_302 (loc, type, _p0, _p1, captures, ROUND_DIV_EXPR);
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
 && (TREE_CODE_CLASS (ROUND_DIV_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1630;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, ROUND_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1630;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, ROUND_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1630;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1630:;
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
 && (TREE_CODE_CLASS (ROUND_DIV_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1631;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1631;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, ROUND_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1631;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, ROUND_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1631;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1631:;
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
 && (TREE_CODE_CLASS (ROUND_DIV_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1632;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1632;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, ROUND_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1632;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, ROUND_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1632;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1632:;
	    }
	}
        break;
      }
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_LT_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
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
	      if (real_zerop (_p1))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		    tree res = generic_simplify_322 (loc, type, _p0, _p1, captures, LT_EXPR, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
        break;
      }
    case TRUNC_MOD_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_323 (loc, type, _p0, _p1, captures, LT_EXPR);
	      if (res) return res;
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
		    tree res = generic_simplify_324 (loc, type, _p0, _p1, captures, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
		    tree res = generic_simplify_324 (loc, type, _p0, _p1, captures, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
		    tree res = generic_simplify_324 (loc, type, _p0, _p1, captures, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
		    tree res = generic_simplify_324 (loc, type, _p0, _p1, captures, LT_EXPR);
		    if (res) return res;
		  }
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
				      tree res = generic_simplify_196 (loc, type, _p0, _p1, captures, LT_EXPR);
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
			  tree res = generic_simplify_196 (loc, type, _p0, _p1, captures, LT_EXPR);
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
		      tree res = generic_simplify_199 (loc, type, _p0, _p1, captures, LT_EXPR);
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
		    tree res = generic_simplify_325 (loc, type, _p0, _p1, captures, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
		    tree res = generic_simplify_325 (loc, type, _p0, _p1, captures, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
		    tree res = generic_simplify_325 (loc, type, _p0, _p1, captures, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
		    tree res = generic_simplify_325 (loc, type, _p0, _p1, captures, LT_EXPR);
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
		case PLUS_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    tree _q51 = TREE_OPERAND (_p1, 1);
		    if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
			  tree res = generic_simplify_326 (loc, type, _p0, _p1, captures, LT_EXPR);
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
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_327 (loc, type, _p0, _p1, captures, LT_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
	      tree res = generic_simplify_327 (loc, type, _p0, _p1, captures, LT_EXPR);
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
		    tree res = generic_simplify_328 (loc, type, _p0, _p1, captures, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
		    tree res = generic_simplify_329 (loc, type, _p0, _p1, captures, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
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
		    tree res = generic_simplify_203 (loc, type, _p0, _p1, captures, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q51 };
		    tree res = generic_simplify_204 (loc, type, _p0, _p1, captures, LT_EXPR);
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
    case PLUS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q30, _p0 };
	      tree res = generic_simplify_327 (loc, type, _p0, _p1, captures, GT_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q31, _p0 };
	      tree res = generic_simplify_327 (loc, type, _p0, _p1, captures, GT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case MINUS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q31 };
	      tree res = generic_simplify_202 (loc, type, _p0, _p1, captures, LT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case MINUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p0, _q21 };
	      tree res = generic_simplify_202 (loc, type, _p0, _p1, captures, GT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case BIT_IOR_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (tree_expr_nonnegative_p (_q20))
	  {
	    if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		  tree res = generic_simplify_330 (loc, type, _p0, _p1, captures, LT_EXPR, BIT_IOR_EXPR);
		  if (res) return res;
		}
	      }
	  }
	if (tree_expr_nonnegative_p (_q21))
	  {
	    if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
		  tree res = generic_simplify_330 (loc, type, _p0, _p1, captures, LT_EXPR, BIT_IOR_EXPR);
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
    case BIT_AND_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    if (tree_expr_nonnegative_p (_q30))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
		  tree res = generic_simplify_330 (loc, type, _p0, _p1, captures, GT_EXPR, BIT_AND_EXPR);
		  if (res) return res;
		}
	      }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    if (tree_expr_nonnegative_p (_q31))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
		  tree res = generic_simplify_330 (loc, type, _p0, _p1, captures, GT_EXPR, BIT_AND_EXPR);
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
    case MIN_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MIN_EXPR, LT_EXPR, GT_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MIN_EXPR, LT_EXPR, GT_EXPR);
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
		      tree res = generic_simplify_331 (loc, type, _p0, _p1, captures, MIN_EXPR, LT_EXPR, BIT_IOR_EXPR);
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
  switch (TREE_CODE (_p1))
    {
    case MAX_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MAX_EXPR, GT_EXPR, LT_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MAX_EXPR, GT_EXPR, LT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case MIN_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
	      tree res = generic_simplify_332 (loc, type, _p0, _p1, captures, MIN_EXPR, LT_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	      tree res = generic_simplify_332 (loc, type, _p0, _p1, captures, MIN_EXPR, LT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case MAX_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_332 (loc, type, _p0, _p1, captures, MAX_EXPR, GT_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_332 (loc, type, _p0, _p1, captures, MAX_EXPR, GT_EXPR);
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
		      tree res = generic_simplify_331 (loc, type, _p0, _p1, captures, MAX_EXPR, LT_EXPR, BIT_AND_EXPR);
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
 && (TREE_CODE_CLASS (LT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1674;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, LT_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1674;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, LT_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1674;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1674:;
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
 && (TREE_CODE_CLASS (LT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1675;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1675;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, LT_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1675;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, LT_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1675;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1675:;
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
 && (TREE_CODE_CLASS (LT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1676;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1676;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, LT_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1676;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, LT_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1676;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1676:;
	    }
	}
        break;
      }
    default:;
    }
if (uniform_integer_cst_p (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      tree res = generic_simplify_333 (loc, type, _p0, _p1, captures, LT_EXPR, LE_EXPR);
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
	      if (integer_zerop (_p1))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
		    tree res = generic_simplify_232 (loc, type, _p0, _p1, captures, LT_EXPR, GT_EXPR);
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
	tree res = generic_simplify_284 (loc, type, _p0, _p1, captures, LT_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p0))
    {
    case BIT_NOT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_p1))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      {
		tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _p1, _q40 };
		tree res = generic_simplify_236 (loc, type, _p0, _p1, captures, LT_EXPR);
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
		    tree res = generic_simplify_236 (loc, type, _p0, _p1, captures, LT_EXPR);
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
	    tree res = generic_simplify_237 (loc, type, _p0, _p1, captures, LT_EXPR, GT_EXPR);
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
		    tree res = generic_simplify_238 (loc, type, _p0, _p1, captures, LT_EXPR);
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
			tree res = generic_simplify_238 (loc, type, _p0, _p1, captures, LT_EXPR);
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
		tree res = generic_simplify_237 (loc, type, _p0, _p1, captures, LT_EXPR, GT_EXPR);
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
	  tree res = generic_simplify_239 (loc, type, _p0, _p1, captures, LT_EXPR);
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
		      tree res = generic_simplify_240 (loc, type, _p0, _p1, captures, PLUS_EXPR, LT_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
	  case INTEGER_CST:
	    {
	      if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q20, _q21 };
		    tree res = generic_simplify_334 (loc, type, _p0, _p1, captures, LT_EXPR, GT_EXPR);
		    if (res) return res;
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
		      tree res = generic_simplify_240 (loc, type, _p0, _p1, captures, MINUS_EXPR, LT_EXPR);
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
		      tree res = generic_simplify_241 (loc, type, _p0, _p1, captures, LT_EXPR);
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
		tree res = generic_simplify_244 (loc, type, _p0, _p1, captures, LT_EXPR, LT_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case REAL_CST:
	    {
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		tree res = generic_simplify_245 (loc, type, _p0, _p1, captures, LT_EXPR, LE_EXPR);
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
		      tree res = generic_simplify_335 (loc, type, _p0, _p1, captures, LT_EXPR);
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
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
			    tree res = generic_simplify_336 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR);
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
	switch (TREE_CODE (_p1))
	  {
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
		tree res = generic_simplify_249 (loc, type, _p0, _p1, captures, LT_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _p1 };
	  tree res = generic_simplify_250 (loc, type, _p0, _p1, captures, LT_EXPR);
	  if (res) return res;
	}
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
			    tree res = generic_simplify_267 (loc, type, _p0, _p1, captures, LT_EXPR);
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
		      tree res = generic_simplify_267 (loc, type, _p0, _p1, captures, LT_EXPR);
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
	if (uniform_integer_cst_p (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
	      tree res = generic_simplify_337 (loc, type, _p0, _p1, captures, LT_EXPR);
	      if (res) return res;
	    }
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
		tree res = generic_simplify_247 (loc, type, _p0, _p1, captures, LT_EXPR, GT_EXPR);
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
	    tree res = generic_simplify_248 (loc, type, _p0, _p1, captures, LT_EXPR, GT_EXPR);
	    if (res) return res;
	  }
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
		      tree res = generic_simplify_268 (loc, type, _p0, _p1, captures, LT_EXPR);
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
		tree res = generic_simplify_268 (loc, type, _p0, _p1, captures, LT_EXPR);
		if (res) return res;
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
		      tree res = generic_simplify_242 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTF, LT_EXPR);
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
			    tree res = generic_simplify_243 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTF, LT_EXPR);
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
		      tree res = generic_simplify_242 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTL, LT_EXPR);
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
			    tree res = generic_simplify_243 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTL, LT_EXPR);
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
		      tree res = generic_simplify_242 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRT, LT_EXPR);
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
			    tree res = generic_simplify_243 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRT, LT_EXPR);
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
		      tree res = generic_simplify_242 (loc, type, _p0, _p1, captures, CFN_SQRT, LT_EXPR);
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
			    tree res = generic_simplify_243 (loc, type, _p0, _p1, captures, CFN_SQRT, LT_EXPR);
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
		  if (integer_zerop (_q41))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
			if (TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
			  {
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1677;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1677;
			    {
			      tree _r;
			      _r =  constant_boolean_node (false, type);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 974, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1677:;
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
if (uniform_integer_cst_p (_p1))
  {
    {
      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _p1 };
      tree res = generic_simplify_338 (loc, type, _p0, _p1, captures, LT_EXPR);
      if (res) return res;
    }
  }
  switch (TREE_CODE (_p0))
    {
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
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
			  if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
			    {
			      {
				tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q40, _q31 };
				tree res = generic_simplify_339 (loc, type, _p0, _p1, captures, LT_EXPR, GT_EXPR);
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
	tree _q30 = TREE_OPERAND (_p1, 0);
	switch (TREE_CODE (_q30))
	  {
	  case PLUS_EXPR:
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
				tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q30, _q40, _p0, _q41 };
				tree res = generic_simplify_339 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
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
    case PLUS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    switch (TREE_CODE (_q31))
	      {
	      case INTEGER_CST:
	        {
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p1, _p1, _q30, _p0, _q31 };
		    tree res = generic_simplify_334 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
		    if (res) return res;
		  }
	          break;
	        }
	      default:;
	      }
	  }
        break;
      }
    case MINUS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p1, _p0, _q31 };
	      tree res = generic_simplify_340 (loc, type, _p0, _p1, captures, GT_EXPR);
	      if (res) return res;
	    }
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
	switch (TREE_CODE (_q20))
	  {
	  case MINUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_minus_onep (_q21))
		{
		  if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
			tree res = generic_simplify_341 (loc, type, _p0, _p1, captures, LT_EXPR);
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
    case REALPART_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	switch (TREE_CODE (_q30))
	  {
	  case CALL_EXPR:
	    switch (get_call_combined_fn (_q30))
	      {
	      case CFN_SUB_OVERFLOW:
	        if (call_expr_nargs (_q30) == 2)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _p0, _q41 };
			  tree res = generic_simplify_342 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
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
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case REALPART_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case CALL_EXPR:
	    switch (get_call_combined_fn (_q20))
	      {
	      case CFN_ADD_OVERFLOW:
	        if (call_expr_nargs (_q20) == 2)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    tree _q31 = CALL_EXPR_ARG (_q20, 1);
		    if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q31 };
			  tree res = generic_simplify_343 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q31, _q30 };
			  tree res = generic_simplify_343 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
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
        break;
      }
    case TRUNC_DIV_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_all_onesp (_q20))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _p1 };
	      tree res = generic_simplify_344 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
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
	  case REAL_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case REAL_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      tree res = generic_simplify_345 (loc, type, _p0, _p1, captures, LT_EXPR, GT_EXPR);
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
		      tree res = generic_simplify_346 (loc, type, _p0, _p1, captures, PLUS_EXPR, MINUS_EXPR, LT_EXPR, GT_EXPR);
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
		      tree res = generic_simplify_346 (loc, type, _p0, _p1, captures, MINUS_EXPR, PLUS_EXPR, LT_EXPR, GT_EXPR);
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
		      tree res = generic_simplify_274 (loc, type, _p0, _p1, captures, LT_EXPR, GT_EXPR);
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
		      tree res = generic_simplify_347 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR, CFN_BUILT_IN_CTZ);
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
		      tree res = generic_simplify_347 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR, CFN_BUILT_IN_CTZL);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
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
		      tree res = generic_simplify_347 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR, CFN_BUILT_IN_CTZIMAX);
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
		      tree res = generic_simplify_347 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR, CFN_CTZ);
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
		      tree res = generic_simplify_348 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
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
		      tree res = generic_simplify_347 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR, CFN_BUILT_IN_CTZLL);
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
generic_simplify_GT_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
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
	      if (real_zerop (_p1))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		    tree res = generic_simplify_322 (loc, type, _p0, _p1, captures, GT_EXPR, LT_EXPR);
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
    case TRUNC_MOD_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _p0 };
	      tree res = generic_simplify_323 (loc, type, _p0, _p1, captures, LT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
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
		    tree res = generic_simplify_324 (loc, type, _p0, _p1, captures, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
		    tree res = generic_simplify_324 (loc, type, _p0, _p1, captures, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
		    tree res = generic_simplify_324 (loc, type, _p0, _p1, captures, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
		    tree res = generic_simplify_324 (loc, type, _p0, _p1, captures, GT_EXPR);
		    if (res) return res;
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
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_minus_onep (_q21))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case INTEGER_CST:
	        {
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		    tree res = generic_simplify_349 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
		    if (res) return res;
		  }
	          break;
	        }
	      default:;
	      }
	  }
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
		    tree res = generic_simplify_325 (loc, type, _p0, _p1, captures, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
		    tree res = generic_simplify_325 (loc, type, _p0, _p1, captures, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
		    tree res = generic_simplify_325 (loc, type, _p0, _p1, captures, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
		    tree res = generic_simplify_325 (loc, type, _p0, _p1, captures, GT_EXPR);
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
		case PLUS_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    tree _q51 = TREE_OPERAND (_p1, 1);
		    if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
			  tree res = generic_simplify_326 (loc, type, _p0, _p1, captures, GT_EXPR);
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
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_327 (loc, type, _p0, _p1, captures, GT_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
	      tree res = generic_simplify_327 (loc, type, _p0, _p1, captures, GT_EXPR);
	      if (res) return res;
	    }
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
				      tree res = generic_simplify_196 (loc, type, _p0, _p1, captures, GT_EXPR);
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
			  tree res = generic_simplify_196 (loc, type, _p0, _p1, captures, GT_EXPR);
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
		      tree res = generic_simplify_199 (loc, type, _p0, _p1, captures, GT_EXPR);
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
		    tree res = generic_simplify_328 (loc, type, _p0, _p1, captures, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
		    tree res = generic_simplify_329 (loc, type, _p0, _p1, captures, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
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
		    tree res = generic_simplify_203 (loc, type, _p0, _p1, captures, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q51 };
		    tree res = generic_simplify_204 (loc, type, _p0, _p1, captures, GT_EXPR);
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
    case PLUS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q30, _p0 };
	      tree res = generic_simplify_327 (loc, type, _p0, _p1, captures, LT_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q31, _p0 };
	      tree res = generic_simplify_327 (loc, type, _p0, _p1, captures, LT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case MINUS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q31 };
	      tree res = generic_simplify_202 (loc, type, _p0, _p1, captures, GT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case MINUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p0, _q21 };
	      tree res = generic_simplify_202 (loc, type, _p0, _p1, captures, LT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case BIT_AND_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (tree_expr_nonnegative_p (_q20))
	  {
	    if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		  tree res = generic_simplify_330 (loc, type, _p0, _p1, captures, GT_EXPR, BIT_AND_EXPR);
		  if (res) return res;
		}
	      }
	  }
	if (tree_expr_nonnegative_p (_q21))
	  {
	    if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
		  tree res = generic_simplify_330 (loc, type, _p0, _p1, captures, GT_EXPR, BIT_AND_EXPR);
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
    case BIT_IOR_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    if (tree_expr_nonnegative_p (_q30))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
		  tree res = generic_simplify_330 (loc, type, _p0, _p1, captures, LT_EXPR, BIT_IOR_EXPR);
		  if (res) return res;
		}
	      }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    if (tree_expr_nonnegative_p (_q31))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
		  tree res = generic_simplify_330 (loc, type, _p0, _p1, captures, LT_EXPR, BIT_IOR_EXPR);
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
    case MAX_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MAX_EXPR, GT_EXPR, LT_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MAX_EXPR, GT_EXPR, LT_EXPR);
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
		      tree res = generic_simplify_331 (loc, type, _p0, _p1, captures, MAX_EXPR, GT_EXPR, BIT_IOR_EXPR);
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
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MIN_EXPR, LT_EXPR, GT_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MIN_EXPR, LT_EXPR, GT_EXPR);
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
	      tree res = generic_simplify_332 (loc, type, _p0, _p1, captures, MAX_EXPR, GT_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	      tree res = generic_simplify_332 (loc, type, _p0, _p1, captures, MAX_EXPR, GT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case MIN_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_332 (loc, type, _p0, _p1, captures, MIN_EXPR, LT_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_332 (loc, type, _p0, _p1, captures, MIN_EXPR, LT_EXPR);
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
		      tree res = generic_simplify_331 (loc, type, _p0, _p1, captures, MIN_EXPR, GT_EXPR, BIT_AND_EXPR);
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
 && (TREE_CODE_CLASS (GT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1681;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, GT_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1681;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, GT_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1681;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1681:;
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
 && (TREE_CODE_CLASS (GT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1682;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1682;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, GT_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1682;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, GT_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1682;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1682:;
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
 && (TREE_CODE_CLASS (GT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1683;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1683;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, GT_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1683;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, GT_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1683;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1683:;
	    }
	}
        break;
      }
    default:;
    }
if (uniform_integer_cst_p (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      tree res = generic_simplify_350 (loc, type, _p0, _p1, captures, GT_EXPR, GE_EXPR);
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
	      if (integer_zerop (_p1))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
		    tree res = generic_simplify_232 (loc, type, _p0, _p1, captures, GT_EXPR, LT_EXPR);
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
	tree res = generic_simplify_284 (loc, type, _p0, _p1, captures, GT_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p0))
    {
    case BIT_NOT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_p1))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      {
		tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _p1, _q40 };
		tree res = generic_simplify_236 (loc, type, _p0, _p1, captures, GT_EXPR);
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
		    tree res = generic_simplify_236 (loc, type, _p0, _p1, captures, GT_EXPR);
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
	    tree res = generic_simplify_237 (loc, type, _p0, _p1, captures, GT_EXPR, LT_EXPR);
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
		    tree res = generic_simplify_238 (loc, type, _p0, _p1, captures, GT_EXPR);
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
			tree res = generic_simplify_238 (loc, type, _p0, _p1, captures, GT_EXPR);
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
		tree res = generic_simplify_237 (loc, type, _p0, _p1, captures, GT_EXPR, LT_EXPR);
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
	  tree res = generic_simplify_239 (loc, type, _p0, _p1, captures, GT_EXPR);
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
		      tree res = generic_simplify_240 (loc, type, _p0, _p1, captures, PLUS_EXPR, GT_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
	  case INTEGER_CST:
	    {
	      if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q20, _q21 };
		    tree res = generic_simplify_334 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
		    if (res) return res;
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
		      tree res = generic_simplify_240 (loc, type, _p0, _p1, captures, MINUS_EXPR, GT_EXPR);
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
		      tree res = generic_simplify_241 (loc, type, _p0, _p1, captures, GT_EXPR);
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
		tree res = generic_simplify_244 (loc, type, _p0, _p1, captures, GT_EXPR, GT_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case REAL_CST:
	    {
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		tree res = generic_simplify_245 (loc, type, _p0, _p1, captures, GT_EXPR, GE_EXPR);
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
		      tree res = generic_simplify_335 (loc, type, _p0, _p1, captures, GT_EXPR);
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
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
			    tree res = generic_simplify_336 (loc, type, _p0, _p1, captures, GT_EXPR, LT_EXPR);
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
	switch (TREE_CODE (_p1))
	  {
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
		tree res = generic_simplify_249 (loc, type, _p0, _p1, captures, GT_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _p1 };
	  tree res = generic_simplify_250 (loc, type, _p0, _p1, captures, GT_EXPR);
	  if (res) return res;
	}
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
			    tree res = generic_simplify_267 (loc, type, _p0, _p1, captures, GT_EXPR);
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
		      tree res = generic_simplify_267 (loc, type, _p0, _p1, captures, GT_EXPR);
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
	if (uniform_integer_cst_p (_p1))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
	      tree res = generic_simplify_337 (loc, type, _p0, _p1, captures, GT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case BIT_AND_EXPR:
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
		      tree res = generic_simplify_351 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
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
		tree res = generic_simplify_247 (loc, type, _p0, _p1, captures, GT_EXPR, LT_EXPR);
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
	    tree res = generic_simplify_248 (loc, type, _p0, _p1, captures, GT_EXPR, LT_EXPR);
	    if (res) return res;
	  }
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
		      tree res = generic_simplify_268 (loc, type, _p0, _p1, captures, GT_EXPR);
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
		tree res = generic_simplify_268 (loc, type, _p0, _p1, captures, GT_EXPR);
		if (res) return res;
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
		      tree res = generic_simplify_242 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTF, GT_EXPR);
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
			    tree res = generic_simplify_243 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTF, GT_EXPR);
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
		      tree res = generic_simplify_242 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTL, GT_EXPR);
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
			    tree res = generic_simplify_243 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTL, GT_EXPR);
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
		      tree res = generic_simplify_242 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRT, GT_EXPR);
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
			    tree res = generic_simplify_243 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRT, GT_EXPR);
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
		      tree res = generic_simplify_242 (loc, type, _p0, _p1, captures, CFN_SQRT, GT_EXPR);
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
			    tree res = generic_simplify_243 (loc, type, _p0, _p1, captures, CFN_SQRT, GT_EXPR);
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
  if (uniform_integer_cst_p (_p1))
    {
      {
	tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _p1 };
	tree res = generic_simplify_338 (loc, type, _p0, _p1, captures, GT_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p0))
    {
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case NE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_zerop (_q31))
		{
		  if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		    {
		      {
			tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
			if (TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
			  {
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1684;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1684;
			    {
			      tree _r;
			      _r =  constant_boolean_node (false, type);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 974, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1684:;
			  }
		      }
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
		    switch (TREE_CODE (_q31))
		      {
		      case INTEGER_CST:
		        {
			  if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
			    {
			      {
				tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q40, _q31 };
				tree res = generic_simplify_339 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
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
	tree _q30 = TREE_OPERAND (_p1, 0);
	switch (TREE_CODE (_q30))
	  {
	  case PLUS_EXPR:
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
				tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q30, _q40, _p0, _q41 };
				tree res = generic_simplify_339 (loc, type, _p0, _p1, captures, LT_EXPR, GT_EXPR);
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
    case PLUS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    switch (TREE_CODE (_q31))
	      {
	      case INTEGER_CST:
	        {
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p1, _p1, _q30, _p0, _q31 };
		    tree res = generic_simplify_334 (loc, type, _p0, _p1, captures, LT_EXPR, GT_EXPR);
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
  switch (TREE_CODE (_p0))
    {
    case MINUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_340 (loc, type, _p0, _p1, captures, GT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case PLUS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	switch (TREE_CODE (_q30))
	  {
	  case MINUS_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  if (integer_minus_onep (_q31))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q41 };
			tree res = generic_simplify_352 (loc, type, _p0, _p1, captures, LT_EXPR);
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
  switch (TREE_CODE (_p0))
    {
    case REALPART_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case CALL_EXPR:
	    switch (get_call_combined_fn (_q20))
	      {
	      case CFN_SUB_OVERFLOW:
	        if (call_expr_nargs (_q20) == 2)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    tree _q31 = CALL_EXPR_ARG (_q20, 1);
		    if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q31 };
			  tree res = generic_simplify_353 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
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
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case REALPART_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
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
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _p0, _q41 };
			  tree res = generic_simplify_354 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _p0, _q40 };
			  tree res = generic_simplify_354 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
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
        break;
      }
    case TRUNC_DIV_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if (integer_all_onesp (_q30))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q31, _p0 };
	      tree res = generic_simplify_355 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case MULT_EXPR:
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
		      tree res = generic_simplify_345 (loc, type, _p0, _p1, captures, GT_EXPR, LT_EXPR);
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
		      tree res = generic_simplify_346 (loc, type, _p0, _p1, captures, PLUS_EXPR, MINUS_EXPR, GT_EXPR, LT_EXPR);
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
		      tree res = generic_simplify_346 (loc, type, _p0, _p1, captures, MINUS_EXPR, PLUS_EXPR, GT_EXPR, LT_EXPR);
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
		      tree res = generic_simplify_274 (loc, type, _p0, _p1, captures, GT_EXPR, LT_EXPR);
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
		      tree res = generic_simplify_347 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR, CFN_BUILT_IN_CTZ);
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
		      tree res = generic_simplify_356 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR, EQ_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_FFS);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
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
		      tree res = generic_simplify_356 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR, EQ_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_FFSIMAX);
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
		    tree res = generic_simplify_279 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR, CFN_BUILT_IN_POPCOUNT);
		    if (res) return res;
		  }
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
		      tree res = generic_simplify_347 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR, CFN_BUILT_IN_CTZL);
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
		      tree res = generic_simplify_356 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR, EQ_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_FFSL);
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
		    tree res = generic_simplify_279 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR, CFN_BUILT_IN_POPCOUNTLL);
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
		      tree res = generic_simplify_347 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR, CFN_BUILT_IN_CTZIMAX);
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
		    tree res = generic_simplify_279 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR, CFN_POPCOUNT);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_CTZ:
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
		      tree res = generic_simplify_348 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_347 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR, CFN_CTZ);
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
		      tree res = generic_simplify_356 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR, EQ_EXPR, BIT_AND_EXPR, CFN_FFS);
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
		    tree res = generic_simplify_279 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR, CFN_BUILT_IN_POPCOUNTIMAX);
		    if (res) return res;
		  }
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
		    tree res = generic_simplify_279 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR, CFN_BUILT_IN_POPCOUNTL);
		    if (res) return res;
		  }
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
		      tree res = generic_simplify_347 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR, CFN_BUILT_IN_CTZLL);
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
		      tree res = generic_simplify_356 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR, EQ_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_FFSLL);
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
generic_simplify_BIT_AND_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case LSHIFT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case LSHIFT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_63 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
		    if (res) return res;
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
	switch (TREE_CODE (_p1))
	  {
	  case RSHIFT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_64 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
		    if (res) return res;
		  }
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
	switch (TREE_CODE (_p1))
	  {
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      {
		tree _q40_pops[1];
		if (tree_maybe_bit_not (_q40, _q40_pops))
		  {
		    tree _q50 = _q40_pops[0];
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q50 };
		      tree res = generic_simplify_388 (loc, type, _p0, _p1, captures);
		      if (res) return res;
		    }
		  }
	      }
	      break;
	    }
          default:;
          }
	{
	  tree _q20_pops[1];
	  if (tree_maybe_bit_not (_q20, _q20_pops))
	    {
	      tree _q30 = _q20_pops[0];
	      switch (TREE_CODE (_p1))
	        {
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q50, _q30 };
		      tree res = generic_simplify_388 (loc, type, _p0, _p1, captures);
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
  {
    tree _p1_pops[1];
    if (tree_maybe_bit_not (_p1, _p1_pops))
      {
	tree _q30 = _p1_pops[0];
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	  tree res = generic_simplify_388 (loc, type, _p0, _p1, captures);
	  if (res) return res;
	}
      }
  }
  {
    tree _p0_pops[1];
    if (tree_maybe_bit_not (_p0, _p0_pops))
      {
	tree _q20 = _p0_pops[0];
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p1, _q20 };
	  tree res = generic_simplify_388 (loc, type, _p0, _p1, captures);
	  if (res) return res;
	}
      }
  }
  switch (TREE_CODE (_p1))
    {
    case PLUS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	switch (TREE_CODE (_q30))
	  {
	  case LSHIFT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if (integer_onep (_q40))
		{
		  if (integer_minus_onep (_q31))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q41 };
			if (TYPE_UNSIGNED (type)
)
			  {
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1716;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1716;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      {
				tree _o1[1], _r1;
				{
				  tree _o2[2], _r2;
				  _o2[0] =  build_all_ones_cst (type);
				  _o2[1] = captures[3];
				  _r2 = fold_build2_loc (loc, LSHIFT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				  _o1[0] = _r2;
				}
				_r1 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
				res_op1 = _r1;
			      }
			      tree _r;
			      _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 984, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1716:;
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
    case PLUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  case LSHIFT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_onep (_q30))
		{
		  if (integer_minus_onep (_q21))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p0, _q20, _q31 };
			if (TYPE_UNSIGNED (type)
)
			  {
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1717;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1717;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      {
				tree _o1[1], _r1;
				{
				  tree _o2[2], _r2;
				  _o2[0] =  build_all_ones_cst (type);
				  _o2[1] = captures[3];
				  _r2 = fold_build2_loc (loc, LSHIFT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				  _o1[0] = _r2;
				}
				_r1 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
				res_op1 = _r1;
			      }
			      tree _r;
			      _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 984, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1717:;
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
	if (integer_zerop (_q21))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case EQ_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if (integer_zerop (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
			tree res = generic_simplify_65 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, EQ_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	  }
	if (integer_all_onesp (_q21))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case EQ_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if (integer_all_onesp (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
			tree res = generic_simplify_66 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, EQ_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	  }
	switch (TREE_CODE (_p1))
	  {
	  case BIT_IOR_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			tree res = generic_simplify_389 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			tree res = generic_simplify_389 (loc, type, _p0, _p1, captures);
			if (res) return res;
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
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_390 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_390 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
        break;
      }
    case BIT_IOR_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      switch (TREE_CODE (_q50))
	        {
		case BIT_XOR_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    tree _q61 = TREE_OPERAND (_q50, 1);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			      tree res = generic_simplify_391 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			      tree res = generic_simplify_391 (loc, type, _p0, _p1, captures);
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
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			tree res = generic_simplify_389 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			tree res = generic_simplify_389 (loc, type, _p0, _p1, captures);
			if (res) return res;
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
	    tree _q50 = _p1_pops[0];
	    switch (TREE_CODE (_q50))
	      {
	      case EQ_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_q50, 0);
		  tree _q61 = TREE_OPERAND (_q50, 1);
		  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		    {
		      if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			    tree res = generic_simplify_389 (loc, type, _p0, _p1, captures);
			    if (res) return res;
			  }
		        }
		    }
		  if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		    {
		      if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			    tree res = generic_simplify_389 (loc, type, _p0, _p1, captures);
			    if (res) return res;
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
    case BIT_NOT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case BIT_XOR_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_IOR_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			if ((_q61 == _q31 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q31, 0) && types_match (_q61, _q31)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
			      tree res = generic_simplify_391 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q60 == _q31 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q31, 0) && types_match (_q60, _q31)))
		      {
			if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q31, _q30 };
			      tree res = generic_simplify_391 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    break;
		  }
	        default:;
	        }
	      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
		    tree res = generic_simplify_392 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q31, _q30 };
		    tree res = generic_simplify_392 (loc, type, _p0, _p1, captures);
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
  if (tree_nop_convert (_p0, _p0_pops))
    {
      tree _q20 = _p0_pops[0];
      switch (TREE_CODE (_q20))
        {
	case EQ_EXPR:
	  {
	    tree _q30 = TREE_OPERAND (_q20, 0);
	    tree _q31 = TREE_OPERAND (_q20, 1);
	    switch (TREE_CODE (_p1))
	      {
	      case BIT_IOR_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p1, 0);
		  tree _q61 = TREE_OPERAND (_p1, 1);
		  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		    {
		      if ((_q61 == _q31 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q31, 0) && types_match (_q61, _q31)))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
			    tree res = generic_simplify_389 (loc, type, _p0, _p1, captures);
			    if (res) return res;
			  }
		        }
		    }
		  if ((_q60 == _q31 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q31, 0) && types_match (_q60, _q31)))
		    {
		      if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
			{
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q31, _q30 };
			    tree res = generic_simplify_389 (loc, type, _p0, _p1, captures);
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
    }
}
  switch (TREE_CODE (_p1))
    {
    case BIT_NOT_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	switch (TREE_CODE (_q30))
	  {
	  case BIT_XOR_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q41 };
		    tree res = generic_simplify_392 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q40 };
		    tree res = generic_simplify_392 (loc, type, _p0, _p1, captures);
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
  tree _p1_pops[1];
  if (tree_nop_convert (_p1, _p1_pops))
    {
      tree _q30 = _p1_pops[0];
      switch (TREE_CODE (_q30))
        {
	case EQ_EXPR:
	  {
	    tree _q40 = TREE_OPERAND (_q30, 0);
	    tree _q41 = TREE_OPERAND (_q30, 1);
	    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q41 };
		  tree res = generic_simplify_390 (loc, type, _p0, _p1, captures);
		  if (res) return res;
		}
	      }
	    if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q40 };
		  tree res = generic_simplify_390 (loc, type, _p0, _p1, captures);
		  if (res) return res;
		}
	      }
	    break;
	  }
        default:;
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
	  case EQ_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
		    tree res = generic_simplify_390 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q31, _q30 };
		    tree res = generic_simplify_390 (loc, type, _p0, _p1, captures);
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
    case EQ_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
	      tree res = generic_simplify_390 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	      tree res = generic_simplify_390 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
        break;
      }
    case BIT_XOR_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q31 };
	  tree res = generic_simplify_393 (loc, type, _p0, _p1, captures);
	  if (res) return res;
	}
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q31, _q30 };
	  tree res = generic_simplify_393 (loc, type, _p0, _p1, captures);
	  if (res) return res;
	}
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case BIT_XOR_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q21 };
	  tree res = generic_simplify_393 (loc, type, _p0, _p1, captures);
	  if (res) return res;
	}
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q21, _q20 };
	  tree res = generic_simplify_393 (loc, type, _p0, _p1, captures);
	  if (res) return res;
	}
	switch (TREE_CODE (_p1))
	  {
	  case BIT_XOR_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q50))
	        {
		case BIT_XOR_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    tree _q61 = TREE_OPERAND (_q50, 1);
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
			  {
			    {
			      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q61 };
			      tree res = generic_simplify_394 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
		      {
			if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
			  {
			    {
			      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q60 };
			      tree res = generic_simplify_394 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			  {
			    {
			      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q61 };
			      tree res = generic_simplify_394 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
		      {
			if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			  {
			    {
			      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q60 };
			      tree res = generic_simplify_394 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    break;
		  }
	        default:;
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  switch (TREE_CODE (_q51))
		    {
		    case BIT_XOR_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_q51, 0);
			tree _q71 = TREE_OPERAND (_q51, 1);
			if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
			  {
			    {
			      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q71 };
			      tree res = generic_simplify_394 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
			if ((_q71 == _q21 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q21, 0) && types_match (_q71, _q21)))
			  {
			    {
			      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q70 };
			      tree res = generic_simplify_394 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  switch (TREE_CODE (_q51))
		    {
		    case BIT_XOR_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_q51, 0);
			tree _q71 = TREE_OPERAND (_q51, 1);
			if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
			  {
			    {
			      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q71 };
			      tree res = generic_simplify_394 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
			if ((_q71 == _q20 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q20, 0) && types_match (_q71, _q20)))
			  {
			    {
			      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q70 };
			      tree res = generic_simplify_394 (loc, type, _p0, _p1, captures);
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
          default:;
          }
	switch (TREE_CODE (_q20))
	  {
	  case BIT_XOR_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_XOR_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
			  {
			    {
			      tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q30, _p0, _q20, _q31 };
			      tree res = generic_simplify_395 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
			if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
			  {
			    {
			      tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q31, _p0, _q20, _q30 };
			      tree res = generic_simplify_395 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
		      {
			if ((_q71 == _q21 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q21, 0) && types_match (_q71, _q21)))
			  {
			    {
			      tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q30, _p0, _q20, _q31 };
			      tree res = generic_simplify_395 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
		      {
			if ((_q71 == _q21 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q21, 0) && types_match (_q71, _q21)))
			  {
			    {
			      tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q31, _p0, _q20, _q30 };
			      tree res = generic_simplify_395 (loc, type, _p0, _p1, captures);
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
	switch (TREE_CODE (_q21))
	  {
	  case BIT_XOR_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_XOR_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
		      {
			if ((_q71 == _q40 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q40, 0) && types_match (_q71, _q40)))
			  {
			    {
			      tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q40, _p0, _q21, _q41 };
			      tree res = generic_simplify_395 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
			if ((_q71 == _q41 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q41, 0) && types_match (_q71, _q41)))
			  {
			    {
			      tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q41, _p0, _q21, _q40 };
			      tree res = generic_simplify_395 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
		      {
			if ((_q71 == _q20 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q20, 0) && types_match (_q71, _q20)))
			  {
			    {
			      tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q40, _p0, _q21, _q41 };
			      tree res = generic_simplify_395 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q70 == _q41 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q41, 0) && types_match (_q70, _q41)))
		      {
			if ((_q71 == _q20 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q20, 0) && types_match (_q71, _q20)))
			  {
			    {
			      tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q41, _p0, _q21, _q40 };
			      tree res = generic_simplify_395 (loc, type, _p0, _p1, captures);
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
        break;
      }
    case BIT_NOT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case SSA_NAME:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		      if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && wi::bit_and_not (get_nonzero_bits (captures[0]), wi::to_wide (captures[1])) == 0
)
			{
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1718;
			  {
			    tree res_op0;
			    res_op0 = captures[0];
			    tree res_op1;
			    res_op1 = captures[1];
			    tree _r;
			    _r = fold_build2_loc (loc, BIT_XOR_EXPR, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 985, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1718:;
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
	  case BIT_AND_EXPR:
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
			    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1 };
			    tree res = generic_simplify_396 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, PLUS_EXPR);
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
			    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1 };
			    tree res = generic_simplify_396 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, PLUS_EXPR);
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
			    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1 };
			    tree res = generic_simplify_396 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, PLUS_EXPR);
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
	switch (TREE_CODE (_q21))
	  {
	  case BIT_AND_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_q41))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q40, _q41, _p1 };
			    tree res = generic_simplify_397 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, PLUS_EXPR);
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
	  case BIT_IOR_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_q41))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q40, _q41, _p1 };
			    tree res = generic_simplify_397 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, PLUS_EXPR);
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
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_q41))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q40, _q41, _p1 };
			    tree res = generic_simplify_397 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, PLUS_EXPR);
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
	switch (TREE_CODE (_p1))
	  {
	  case INTEGER_CST:
	    {
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
		tree res = generic_simplify_398 (loc, type, _p0, _p1, captures, PLUS_EXPR);
		if (res) return res;
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
	  case BIT_AND_EXPR:
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
			    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1 };
			    tree res = generic_simplify_396 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, MINUS_EXPR);
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
			    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1 };
			    tree res = generic_simplify_396 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, MINUS_EXPR);
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
			    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1 };
			    tree res = generic_simplify_396 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, MINUS_EXPR);
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
	switch (TREE_CODE (_q21))
	  {
	  case BIT_AND_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_q41))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q40, _q41, _p1 };
			    tree res = generic_simplify_397 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, MINUS_EXPR);
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
	  case BIT_IOR_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_q41))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q40, _q41, _p1 };
			    tree res = generic_simplify_397 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, MINUS_EXPR);
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
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_q41))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q40, _q41, _p1 };
			    tree res = generic_simplify_397 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, MINUS_EXPR);
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
	switch (TREE_CODE (_p1))
	  {
	  case INTEGER_CST:
	    {
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
		tree res = generic_simplify_398 (loc, type, _p0, _p1, captures, MINUS_EXPR);
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
	switch (TREE_CODE (_q20))
	  {
	  case BIT_AND_EXPR:
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
			    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1 };
			    tree res = generic_simplify_399 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
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
			    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1 };
			    tree res = generic_simplify_399 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
			    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1 };
			    tree res = generic_simplify_399 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
      if (tree_zero_one_valued_p (_q20))
	{
	  {
	    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
	    tree res = generic_simplify_400 (loc, type, _p0, _p1, captures);
	    if (res) return res;
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
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1719;
	{
	  tree _r;
	  _r = captures[1];
	  if (TREE_SIDE_EFFECTS (captures[0]))
	    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 986, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1719:;
      }
    }
  if (integer_all_onesp (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1720;
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1720;
	{
	  tree res_op0;
	  res_op0 = captures[0];
	  tree _r;
	  _r = non_lvalue_loc (loc, res_op0);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 987, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1720:;
      }
    }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_83 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case BIT_NOT_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	switch (TREE_CODE (_q30))
	  {
	  case BIT_AND_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q41 };
		    tree res = generic_simplify_84 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q40 };
		    tree res = generic_simplify_84 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
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
    case BIT_NOT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case BIT_AND_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q20, _q31 };
		    tree res = generic_simplify_84 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q31, _q20, _q30 };
		    tree res = generic_simplify_84 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      switch (TREE_CODE (_p1))
	        {
		case BIT_IOR_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			if ((_q61 == _q31 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q31, 0) && types_match (_q61, _q31)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
			      tree res = generic_simplify_401 (loc, type, _p0, _p1, captures);
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
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      switch (TREE_CODE (_q40))
	        {
		case BIT_NOT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q50 };
		      tree res = generic_simplify_88 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
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
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
		tree res = generic_simplify_88 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
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
	switch (TREE_CODE (_p1))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      switch (TREE_CODE (_q50))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    tree _q61 = TREE_OPERAND (_q50, 1);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			      tree res = generic_simplify_401 (loc, type, _p0, _p1, captures);
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
	  case BIT_XOR_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
		    tree res = generic_simplify_402 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
		    tree res = generic_simplify_402 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
		    tree res = generic_simplify_402 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
		    tree res = generic_simplify_402 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q20))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_IOR_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			switch (TREE_CODE (_q61))
			  {
			  case BIT_NOT_EXPR:
			    {
			      tree _q80 = TREE_OPERAND (_q61, 0);
			      if ((_q80 == _q21 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q21, 0) && types_match (_q80, _q21)))
				{
				  {
				    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q30, _q21, _p1 };
				    tree res = generic_simplify_403 (loc, type, _p0, _p1, captures);
				    if (res) return res;
				  }
			        }
			      break;
			    }
		          default:;
		          }
		      }
		    switch (TREE_CODE (_q60))
		      {
		      case BIT_NOT_EXPR:
		        {
			  tree _q70 = TREE_OPERAND (_q60, 0);
			  if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
			    {
			      if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
				{
				  {
				    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q30, _q21, _p1 };
				    tree res = generic_simplify_403 (loc, type, _p0, _p1, captures);
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
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q21))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_IOR_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
		      {
			switch (TREE_CODE (_q61))
			  {
			  case BIT_NOT_EXPR:
			    {
			      tree _q80 = TREE_OPERAND (_q61, 0);
			      if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
				{
				  {
				    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q40, _q20, _p1 };
				    tree res = generic_simplify_403 (loc, type, _p0, _p1, captures);
				    if (res) return res;
				  }
			        }
			      break;
			    }
		          default:;
		          }
		      }
		    switch (TREE_CODE (_q60))
		      {
		      case BIT_NOT_EXPR:
		        {
			  tree _q70 = TREE_OPERAND (_q60, 0);
			  if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
			    {
			      if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
				{
				  {
				    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q40, _q20, _p1 };
				    tree res = generic_simplify_403 (loc, type, _p0, _p1, captures);
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
	switch (TREE_CODE (_p1))
	  {
	  case BIT_IOR_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q20, _q21 };
		    tree res = generic_simplify_402 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q21, _q20 };
		    tree res = generic_simplify_402 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q51, _q20, _q21 };
		    tree res = generic_simplify_402 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q51, _q21, _q20 };
		    tree res = generic_simplify_402 (loc, type, _p0, _p1, captures);
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
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_160 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_XOR_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
	      tree res = generic_simplify_160 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_XOR_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_p1))
	        {
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    switch (TREE_CODE (_q50))
		      {
		      case BIT_NOT_EXPR:
		        {
			  tree _q60 = TREE_OPERAND (_q50, 0);
			  {
			    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q60 };
			    tree res = generic_simplify_88 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
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
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q50 };
		      tree res = generic_simplify_88 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
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
  switch (TREE_CODE (_p1))
    {
    case BIT_XOR_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q30, _p0 };
	      tree res = generic_simplify_160 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_XOR_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q31, _p0 };
	      tree res = generic_simplify_160 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_XOR_EXPR);
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
	  case BIT_IOR_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    switch (TREE_CODE (_q60))
		      {
		      case BIT_IOR_EXPR:
		        {
			  tree _q70 = TREE_OPERAND (_q60, 0);
			  tree _q71 = TREE_OPERAND (_q60, 1);
			  if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || operand_equal_p (_q70, _q30, 0))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q71, _q30 };
				tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
				if (res) return res;
			      }
			    }
			  if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || operand_equal_p (_q71, _q30, 0))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q70, _q30 };
				tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
				if (res) return res;
			      }
			    }
			  if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || operand_equal_p (_q70, _q31, 0))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q71, _q31 };
				tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
				if (res) return res;
			      }
			    }
			  if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || operand_equal_p (_q71, _q31, 0))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q70, _q31 };
				tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
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
	  case BIT_AND_EXPR:
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
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q30, _q31, _q31 };
			  tree res = generic_simplify_74 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || operand_equal_p (_q60, _q30, 0))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q31, _q30, _q30 };
			  tree res = generic_simplify_74 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
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
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q30, _q31, _q31 };
		    tree res = generic_simplify_74 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q30, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q31, _q30, _q30 };
		    tree res = generic_simplify_74 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
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
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      switch (TREE_CODE (_q40))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || operand_equal_p (_q51, _q20, 0))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q50, _q51, _q20 };
			  tree res = generic_simplify_91 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q20, 0))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q51, _q50, _q20 };
			  tree res = generic_simplify_91 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
	  case BIT_AND_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      tree _q41 = TREE_OPERAND (_p1, 1);
	      if ((_q41 == _q20 && ! TREE_SIDE_EFFECTS (_q41)) || operand_equal_p (_q41, _q20, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q40, _q41, _q20 };
		    tree res = generic_simplify_91 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || operand_equal_p (_q40, _q20, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q41, _q40, _q20 };
		    tree res = generic_simplify_91 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
		    if (res) return res;
		  }
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
	switch (TREE_CODE (_p1))
	  {
	  case BIT_IOR_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q20, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q20 };
		    tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || operand_equal_p (_q51, _q20, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q20 };
		    tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q21, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q21 };
		    tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || operand_equal_p (_q51, _q21, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q21 };
		    tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
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
	  case BIT_AND_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || operand_equal_p (_q41, _p0, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q40, _q41, _p0 };
		    tree res = generic_simplify_91 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || operand_equal_p (_q40, _p0, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q41, _q40, _p0 };
		    tree res = generic_simplify_91 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
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
    case BIT_AND_EXPR:
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
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q21 };
		    tree res = generic_simplify_74 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q20, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _q20 };
		    tree res = generic_simplify_74 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
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
	      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q21 };
	      tree res = generic_simplify_74 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q20, 0))
	  {
	    {
	      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _q20 };
	      tree res = generic_simplify_74 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
	      if (res) return res;
	    }
	  }
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
	  tree res = generic_simplify_404 (loc, type, _p0, _p1, captures);
	  if (res) return res;
	}
	switch (TREE_CODE (_p1))
	  {
	  CASE_CONVERT:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      switch (TREE_CODE (_q50))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    tree _q61 = TREE_OPERAND (_q50, 1);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21, _p1, _q50, _q61 };
			  tree res = generic_simplify_93 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21, _p1, _q50, _q60 };
			  tree res = generic_simplify_93 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q20, _p1, _q50, _q61 };
			  tree res = generic_simplify_93 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q20, _p1, _q50, _q60 };
			  tree res = generic_simplify_93 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      break;
	    }
	  case BIT_AND_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21, _p1, _p1, _q51 };
		    tree res = generic_simplify_94 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21, _p1, _p1, _q50 };
		    tree res = generic_simplify_94 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q20, _p1, _p1, _q51 };
		    tree res = generic_simplify_94 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q20, _p1, _p1, _q50 };
		    tree res = generic_simplify_94 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
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
    case BIT_AND_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || operand_equal_p (_q31, _p0, 0))
	  {
	    {
	      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q30, _q31, _p0 };
	      tree res = generic_simplify_91 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || operand_equal_p (_q30, _p0, 0))
	  {
	    {
	      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q31, _q30, _p0 };
	      tree res = generic_simplify_91 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
	      if (res) return res;
	    }
	  }
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p0 };
	  tree res = generic_simplify_404 (loc, type, _p0, _p1, captures);
	  if (res) return res;
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
	      switch (TREE_CODE (_p1))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    switch (TREE_CODE (_q60))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q70 = TREE_OPERAND (_q60, 0);
			  tree _q71 = TREE_OPERAND (_q60, 1);
			  if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
			    {
			      {
				tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1, _q60, _q71 };
				tree res = generic_simplify_95 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
				if (res) return res;
			      }
			    }
			  if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
			    {
			      {
				tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1, _q60, _q70 };
				tree res = generic_simplify_95 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
				if (res) return res;
			      }
			    }
			  if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
			    {
			      {
				tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q30, _p1, _q60, _q71 };
				tree res = generic_simplify_95 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
				if (res) return res;
			      }
			    }
			  if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
			    {
			      {
				tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q30, _p1, _q60, _q70 };
				tree res = generic_simplify_95 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
				if (res) return res;
			      }
			    }
		          break;
		        }
		      default:;
		      }
		    break;
		  }
		case BIT_AND_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1, _p1, _q61 };
			  tree res = generic_simplify_96 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1, _p1, _q60 };
			  tree res = generic_simplify_96 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q31 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q31, 0) && types_match (_q60, _q31)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q30, _p1, _p1, _q61 };
			  tree res = generic_simplify_96 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q31 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q31, 0) && types_match (_q61, _q31)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q30, _p1, _p1, _q60 };
			  tree res = generic_simplify_96 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
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
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
		tree res = generic_simplify_97 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _p1 };
	  tree res = generic_simplify_98 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
	  if (res) return res;
	}
        break;
      }
    case BIT_IOR_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_99 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_99 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
	      if (res) return res;
	    }
	  }
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
	  tree res = generic_simplify_100 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
	  if (res) return res;
	}
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
	  tree res = generic_simplify_100 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
	  if (res) return res;
	}
	switch (TREE_CODE (_p1))
	  {
	  case BIT_AND_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_101 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_101 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_101 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_101 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case BIT_NOT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      switch (TREE_CODE (_q50))
	        {
		case BIT_IOR_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    tree _q61 = TREE_OPERAND (_q50, 1);
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q61 };
			  tree res = generic_simplify_102 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q60 };
			  tree res = generic_simplify_102 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _p1, _q61 };
			  tree res = generic_simplify_102 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _p1, _q60 };
			  tree res = generic_simplify_102 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
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
    case BIT_IOR_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
	      tree res = generic_simplify_99 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	      tree res = generic_simplify_99 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
	      if (res) return res;
	    }
	  }
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p0 };
	  tree res = generic_simplify_100 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
	  if (res) return res;
	}
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _q31, _q30, _p0 };
	  tree res = generic_simplify_100 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
	  if (res) return res;
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
	switch (TREE_CODE (_p1))
	  {
	  case BIT_IOR_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q51, _p0, _q21 };
		    tree res = generic_simplify_105 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q51, _p0, _q20 };
		    tree res = generic_simplify_105 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q50, _p0, _q21 };
		    tree res = generic_simplify_105 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q50, _p0, _q20 };
		    tree res = generic_simplify_105 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
		    if (res) return res;
		  }
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
	switch (TREE_CODE (_q20))
	  {
	  case BIT_IOR_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_IOR_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q30, _p0, _q31 };
			  tree res = generic_simplify_106 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q31 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q31, 0) && types_match (_q61, _q31)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q31, _p0, _q30 };
			  tree res = generic_simplify_106 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _q61, _q30, _p0, _q31 };
			  tree res = generic_simplify_106 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q31 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q31, 0) && types_match (_q60, _q31)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _q61, _q31, _p0, _q30 };
			  tree res = generic_simplify_106 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
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
		    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
		    tree res = generic_simplify_107 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q31, _q30 };
		    tree res = generic_simplify_107 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
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
    case BIT_NOT_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	switch (TREE_CODE (_q30))
	  {
	  case BIT_IOR_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q41 };
		    tree res = generic_simplify_107 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q40 };
		    tree res = generic_simplify_107 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR);
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
    case BIT_IOR_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (CONSTANT_CLASS_P (_q21))
	  {
	    if (CONSTANT_CLASS_P (_p1))
	      {
		{
		  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1721;
		  {
		    if (! tree_invariant_p (captures[2])) goto next_after_fail1721;
		    tree res_op0;
		    {
		      tree _o1[2], _r1;
		      _o1[0] = captures[0];
		      _o1[1] = unshare_expr (captures[2]);
		      _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      res_op0 = _r1;
		    }
		    tree res_op1;
		    {
		      tree _o1[2], _r1;
		      _o1[0] = captures[1];
		      _o1[1] = captures[2];
		      _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      if (EXPR_P (_r1))
		        goto next_after_fail1721;
		      res_op1 = _r1;
		    }
		    tree _r;
		    _r = fold_build2_loc (loc, BIT_IOR_EXPR, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 988, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1721:;
		}
	      }
	  }
        break;
      }
    case BIT_AND_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (CONSTANT_CLASS_P (_q21))
	  {
	    if (CONSTANT_CLASS_P (_p1))
	      {
		{
		  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		  tree res = generic_simplify_113 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
		  if (res) return res;
		}
	      }
	  }
        break;
      }
    default:;
    }
  {
    tree _p1_pops[1];
    if (tree_logical_inverted_value (_p1, _p1_pops))
      {
	tree _q30 = _p1_pops[0];
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	      tree res = generic_simplify_405 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
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
	      if (tree_zero_one_valued_p (_q30))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _p1 };
		    tree res = generic_simplify_400 (loc, type, _p0, _p1, captures);
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
	  {
	    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	    tree res = generic_simplify_405 (loc, type, _p0, _p1, captures);
	    if (res) return res;
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
	  case NEGATE_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      if (tree_zero_one_valued_p (_q40))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q40, _p0 };
		    tree res = generic_simplify_400 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
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
	if (tree_zero_one_valued_p (_q30))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _p0 };
	      tree res = generic_simplify_400 (loc, type, _p0, _p1, captures);
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
	switch (TREE_CODE (_q20))
	  {
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_q30))
	        {
		case LT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p1 };
		      tree res = generic_simplify_406 (loc, type, _p0, _p1, captures, LT_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case LE_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p1 };
		      tree res = generic_simplify_406 (loc, type, _p0, _p1, captures, LE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case EQ_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p1 };
		      tree res = generic_simplify_406 (loc, type, _p0, _p1, captures, EQ_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case NE_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p1 };
		      tree res = generic_simplify_406 (loc, type, _p0, _p1, captures, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case GE_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p1 };
		      tree res = generic_simplify_406 (loc, type, _p0, _p1, captures, GE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case GT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p1 };
		      tree res = generic_simplify_406 (loc, type, _p0, _p1, captures, GT_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case UNORDERED_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p1 };
		      tree res = generic_simplify_406 (loc, type, _p0, _p1, captures, UNORDERED_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case ORDERED_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p1 };
		      tree res = generic_simplify_406 (loc, type, _p0, _p1, captures, ORDERED_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case UNLT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p1 };
		      tree res = generic_simplify_406 (loc, type, _p0, _p1, captures, UNLT_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case UNLE_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p1 };
		      tree res = generic_simplify_406 (loc, type, _p0, _p1, captures, UNLE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case UNGT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p1 };
		      tree res = generic_simplify_406 (loc, type, _p0, _p1, captures, UNGT_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case UNGE_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p1 };
		      tree res = generic_simplify_406 (loc, type, _p0, _p1, captures, UNGE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case UNEQ_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p1 };
		      tree res = generic_simplify_406 (loc, type, _p0, _p1, captures, UNEQ_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case LTGT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _p1 };
		      tree res = generic_simplify_406 (loc, type, _p0, _p1, captures, LTGT_EXPR);
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
  switch (TREE_CODE (_p1))
    {
    case NEGATE_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
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
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _p0 };
		      tree res = generic_simplify_407 (loc, type, _p0, _p1, captures, LT_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case LE_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _p0 };
		      tree res = generic_simplify_407 (loc, type, _p0, _p1, captures, LE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case EQ_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _p0 };
		      tree res = generic_simplify_407 (loc, type, _p0, _p1, captures, EQ_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case NE_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _p0 };
		      tree res = generic_simplify_407 (loc, type, _p0, _p1, captures, NE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case GE_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _p0 };
		      tree res = generic_simplify_407 (loc, type, _p0, _p1, captures, GE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case GT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _p0 };
		      tree res = generic_simplify_407 (loc, type, _p0, _p1, captures, GT_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case UNORDERED_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _p0 };
		      tree res = generic_simplify_407 (loc, type, _p0, _p1, captures, UNORDERED_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case ORDERED_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _p0 };
		      tree res = generic_simplify_407 (loc, type, _p0, _p1, captures, ORDERED_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case UNLT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _p0 };
		      tree res = generic_simplify_407 (loc, type, _p0, _p1, captures, UNLT_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case UNLE_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _p0 };
		      tree res = generic_simplify_407 (loc, type, _p0, _p1, captures, UNLE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case UNGT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _p0 };
		      tree res = generic_simplify_407 (loc, type, _p0, _p1, captures, UNGT_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case UNGE_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _p0 };
		      tree res = generic_simplify_407 (loc, type, _p0, _p1, captures, UNGE_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case UNEQ_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _p0 };
		      tree res = generic_simplify_407 (loc, type, _p0, _p1, captures, UNEQ_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case LTGT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _p0 };
		      tree res = generic_simplify_407 (loc, type, _p0, _p1, captures, LTGT_EXPR);
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
		tree res = generic_simplify_408 (loc, type, _p0, _p1, captures);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q20))
	  {
	  case LSHIFT_EXPR:
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
			    case INTEGER_CST:
			      {
				{
				  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q40, _q31, _p1 };
				  tree res = generic_simplify_409 (loc, type, _p0, _p1, captures, LSHIFT_EXPR);
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
	      switch (TREE_CODE (_q31))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q30, _q31, _p1 };
			    tree res = generic_simplify_410 (loc, type, _p0, _p1, captures, LSHIFT_EXPR);
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
	  case RSHIFT_EXPR:
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
			    case INTEGER_CST:
			      {
				{
				  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q40, _q31, _p1 };
				  tree res = generic_simplify_409 (loc, type, _p0, _p1, captures, RSHIFT_EXPR);
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
		    switch (TREE_CODE (_q40))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_q40))
		          {
			  case CFN_BUILT_IN_BSWAP128:
			    if (call_expr_nargs (_q40) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_q40, 0);
				switch (TREE_CODE (_q31))
				  {
				  case INTEGER_CST:
				    {
				      switch (TREE_CODE (_p1))
				        {
					case INTEGER_CST:
					  {
					    {
					      tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q40, _q50, _q31, _p1 };
					      tree res = generic_simplify_411 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP128);
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
			      }
			    break;
			  case CFN_BUILT_IN_BSWAP16:
			    if (call_expr_nargs (_q40) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_q40, 0);
				switch (TREE_CODE (_q31))
				  {
				  case INTEGER_CST:
				    {
				      switch (TREE_CODE (_p1))
				        {
					case INTEGER_CST:
					  {
					    {
					      tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q40, _q50, _q31, _p1 };
					      tree res = generic_simplify_411 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP16);
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
			      }
			    break;
			  case CFN_BUILT_IN_BSWAP32:
			    if (call_expr_nargs (_q40) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_q40, 0);
				switch (TREE_CODE (_q31))
				  {
				  case INTEGER_CST:
				    {
				      switch (TREE_CODE (_p1))
				        {
					case INTEGER_CST:
					  {
					    {
					      tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q40, _q50, _q31, _p1 };
					      tree res = generic_simplify_411 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP32);
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
			      }
			    break;
			  case CFN_BUILT_IN_BSWAP64:
			    if (call_expr_nargs (_q40) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_q40, 0);
				switch (TREE_CODE (_q31))
				  {
				  case INTEGER_CST:
				    {
				      switch (TREE_CODE (_p1))
				        {
					case INTEGER_CST:
					  {
					    {
					      tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q40, _q50, _q31, _p1 };
					      tree res = generic_simplify_411 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP64);
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
	      switch (TREE_CODE (_q31))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q30, _q31, _p1 };
			    tree res = generic_simplify_410 (loc, type, _p0, _p1, captures, RSHIFT_EXPR);
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
	      switch (TREE_CODE (_q30))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_q30))
		    {
		    case CFN_BUILT_IN_BSWAP128:
		      if (call_expr_nargs (_q30) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_q30, 0);
			  switch (TREE_CODE (_q31))
			    {
			    case INTEGER_CST:
			      {
				switch (TREE_CODE (_p1))
				  {
				  case INTEGER_CST:
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q31, _p1 };
					tree res = generic_simplify_411 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP128);
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
		        }
		      break;
		    case CFN_BUILT_IN_BSWAP16:
		      if (call_expr_nargs (_q30) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_q30, 0);
			  switch (TREE_CODE (_q31))
			    {
			    case INTEGER_CST:
			      {
				switch (TREE_CODE (_p1))
				  {
				  case INTEGER_CST:
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q31, _p1 };
					tree res = generic_simplify_411 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP16);
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
		        }
		      break;
		    case CFN_BUILT_IN_BSWAP32:
		      if (call_expr_nargs (_q30) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_q30, 0);
			  switch (TREE_CODE (_q31))
			    {
			    case INTEGER_CST:
			      {
				switch (TREE_CODE (_p1))
				  {
				  case INTEGER_CST:
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q31, _p1 };
					tree res = generic_simplify_411 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP32);
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
		        }
		      break;
		    case CFN_BUILT_IN_BSWAP64:
		      if (call_expr_nargs (_q30) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_q30, 0);
			  switch (TREE_CODE (_q31))
			    {
			    case INTEGER_CST:
			      {
				switch (TREE_CODE (_p1))
				  {
				  case INTEGER_CST:
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q31, _p1 };
					tree res = generic_simplify_411 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP64);
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
		        }
		      break;
		    default:;
		    }
		  break;
	        default:;
	        }
	      break;
	    }
	  case CALL_EXPR:
	    switch (get_call_combined_fn (_q20))
	      {
	      case CFN_BUILT_IN_BSWAP128:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _p1 };
			    tree res = generic_simplify_412 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP128);
			    if (res) return res;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_BSWAP16:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _p1 };
			    tree res = generic_simplify_412 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP16);
			    if (res) return res;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_BSWAP32:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _p1 };
			    tree res = generic_simplify_412 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP32);
			    if (res) return res;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_BSWAP64:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _p1 };
			    tree res = generic_simplify_412 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP64);
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
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case INTEGER_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  tree res = generic_simplify_408 (loc, type, _p0, _p1, captures);
	  if (res) return res;
	}
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case GT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case EQ_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if (tree_min_value (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			tree res = generic_simplify_413 (loc, type, _p0, _p1, captures, EQ_EXPR);
			if (res) return res;
		      }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if (tree_max_value (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
			tree res = generic_simplify_414 (loc, type, _p0, _p1, captures, EQ_EXPR);
			if (res) return res;
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
		  if (tree_min_value (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			tree res = generic_simplify_413 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if (tree_max_value (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
			tree res = generic_simplify_414 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
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
		case EQ_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q61, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
		case NE_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q61, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
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
	switch (TREE_CODE (_q21))
	  {
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      switch (TREE_CODE (_p1))
	        {
		case EQ_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q61, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
		case NE_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q61, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
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
	  case EQ_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q51, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
		    if (res) return res;
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
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q51, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
		    if (res) return res;
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
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LT_EXPR, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GT_EXPR, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LT_EXPR, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GT_EXPR, LT_EXPR);
		    if (res) return res;
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
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GE_EXPR, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LE_EXPR, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case GT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GT_EXPR, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LT_EXPR, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GT_EXPR, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LT_EXPR, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case GE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GE_EXPR, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LE_EXPR, LT_EXPR);
		    if (res) return res;
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
	switch (TREE_CODE (_p1))
	  {
	  case EQ_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if (tree_min_value (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
			tree res = generic_simplify_413 (loc, type, _p0, _p1, captures, EQ_EXPR);
			if (res) return res;
		      }
		    }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if (tree_max_value (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			tree res = generic_simplify_414 (loc, type, _p0, _p1, captures, EQ_EXPR);
			if (res) return res;
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
		  if (tree_min_value (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
			tree res = generic_simplify_413 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
		    }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if (tree_max_value (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			tree res = generic_simplify_414 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
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
		case EQ_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q61, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
		case NE_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q61, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
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
	switch (TREE_CODE (_q21))
	  {
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      switch (TREE_CODE (_p1))
	        {
		case EQ_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q61, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
		case NE_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q61, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
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
	  case EQ_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q51, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
		    if (res) return res;
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
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q51, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
		    if (res) return res;
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
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LT_EXPR, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GT_EXPR, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LT_EXPR, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GT_EXPR, GT_EXPR);
		    if (res) return res;
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
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LE_EXPR, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GE_EXPR, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case GT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GT_EXPR, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LT_EXPR, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GT_EXPR, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LT_EXPR, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case GE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LE_EXPR, LT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GE_EXPR, GT_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
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
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (tree_min_value (_q21))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case GT_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q51 };
			tree res = generic_simplify_418 (loc, type, _p0, _p1, captures, EQ_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      case LT_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q50 };
			tree res = generic_simplify_418 (loc, type, _p0, _p1, captures, EQ_EXPR);
			if (res) return res;
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
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q51, _p0 };
			tree res = generic_simplify_419 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      case GE_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q50, _p0 };
			tree res = generic_simplify_419 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	  }
	if (tree_max_value (_q21))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case LT_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q51 };
			tree res = generic_simplify_420 (loc, type, _p0, _p1, captures, EQ_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      case GT_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q50 };
			tree res = generic_simplify_420 (loc, type, _p0, _p1, captures, EQ_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      case GE_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q51, _p0 };
			tree res = generic_simplify_421 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      case LE_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q50, _p0 };
			tree res = generic_simplify_421 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	  }
	switch (TREE_CODE (_p1))
	  {
	  case EQ_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q50))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q51))
	        {
		CASE_CONVERT:
		  {
		    tree _q70 = TREE_OPERAND (_q51, 0);
		    if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
			  if (res) return res;
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
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q50))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q51))
	        {
		CASE_CONVERT:
		  {
		    tree _q70 = TREE_OPERAND (_q51, 0);
		    if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
			  if (res) return res;
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
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q50))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q51))
	        {
		CASE_CONVERT:
		  {
		    tree _q70 = TREE_OPERAND (_q51, 0);
		    if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
			  if (res) return res;
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
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q50))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q51))
	        {
		CASE_CONVERT:
		  {
		    tree _q70 = TREE_OPERAND (_q51, 0);
		    if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
			  if (res) return res;
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
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q50))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q51))
	        {
		CASE_CONVERT:
		  {
		    tree _q70 = TREE_OPERAND (_q51, 0);
		    if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
			  if (res) return res;
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
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q50))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q51))
	        {
		CASE_CONVERT:
		  {
		    tree _q70 = TREE_OPERAND (_q51, 0);
		    if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
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
		case EQ_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q61, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
		case NE_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q61, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
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
	switch (TREE_CODE (_q21))
	  {
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      switch (TREE_CODE (_p1))
	        {
		case EQ_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q61, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
		case NE_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q61, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
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
	  case EQ_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q51, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
		    if (res) return res;
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
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q51, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
		    if (res) return res;
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
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case GT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
		    if (res) return res;
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
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case GE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
		    if (res) return res;
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
	if (tree_min_value (_q21))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case GT_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q51 };
			tree res = generic_simplify_418 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      case LT_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q50 };
			tree res = generic_simplify_418 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      case GE_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q50 };
			tree res = generic_simplify_424 (loc, type, _p0, _p1, captures);
			if (res) return res;
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
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q51 };
			tree res = generic_simplify_424 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	  }
	if (tree_max_value (_q21))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case LT_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q51 };
			tree res = generic_simplify_420 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      case GT_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q50 };
			tree res = generic_simplify_420 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	  }
	switch (TREE_CODE (_p1))
	  {
	  case EQ_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q50))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q51))
	        {
		CASE_CONVERT:
		  {
		    tree _q70 = TREE_OPERAND (_q51, 0);
		    if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
			  if (res) return res;
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
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q50))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q51))
	        {
		CASE_CONVERT:
		  {
		    tree _q70 = TREE_OPERAND (_q51, 0);
		    if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
			  if (res) return res;
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
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q50))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q51))
	        {
		CASE_CONVERT:
		  {
		    tree _q70 = TREE_OPERAND (_q51, 0);
		    if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
			  if (res) return res;
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
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q50))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q51))
	        {
		CASE_CONVERT:
		  {
		    tree _q70 = TREE_OPERAND (_q51, 0);
		    if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
			  if (res) return res;
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
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q50))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q51))
	        {
		CASE_CONVERT:
		  {
		    tree _q70 = TREE_OPERAND (_q51, 0);
		    if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
			  if (res) return res;
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
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q50))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q51))
	        {
		CASE_CONVERT:
		  {
		    tree _q70 = TREE_OPERAND (_q51, 0);
		    if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_422 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
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
		case EQ_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q61, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
		case NE_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q61, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
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
	switch (TREE_CODE (_q21))
	  {
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      switch (TREE_CODE (_p1))
	        {
		case EQ_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q61, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
		case NE_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q61, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
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
	  case EQ_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q51, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
		    if (res) return res;
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
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q51, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		    if (res) return res;
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
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case GT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
		    if (res) return res;
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
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case GE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_423 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
		    if (res) return res;
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
	  case EQ_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if (tree_min_value (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			tree res = generic_simplify_425 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if (tree_max_value (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			tree res = generic_simplify_426 (loc, type, _p0, _p1, captures);
			if (res) return res;
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
		  if (tree_min_value (_q51))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q20, _p0, _q21 };
			tree res = generic_simplify_427 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
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
		case EQ_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q61, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
		case NE_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q61, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
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
	switch (TREE_CODE (_q21))
	  {
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      switch (TREE_CODE (_p1))
	        {
		case EQ_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q61, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
		case NE_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q61, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
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
	  case EQ_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q51, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
		    if (res) return res;
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
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q51, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
		    if (res) return res;
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
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LT_EXPR, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GT_EXPR, GE_EXPR);
		    if (res) return res;
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
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LE_EXPR, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GE_EXPR, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LE_EXPR, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GE_EXPR, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case GT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LT_EXPR, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GT_EXPR, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case GE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GE_EXPR, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LE_EXPR, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GE_EXPR, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LE_EXPR, GE_EXPR);
		    if (res) return res;
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
	  case EQ_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if (tree_min_value (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			tree res = generic_simplify_425 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if (tree_max_value (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			tree res = generic_simplify_426 (loc, type, _p0, _p1, captures);
			if (res) return res;
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
		  if (tree_min_value (_q51))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q21, _p0, _q20 };
			tree res = generic_simplify_427 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
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
		case EQ_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q61, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
		case NE_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q61, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
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
	switch (TREE_CODE (_q21))
	  {
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      switch (TREE_CODE (_p1))
	        {
		case EQ_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q61, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
		case NE_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q61, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_415 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
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
	  case EQ_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q51, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
		    if (res) return res;
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
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q51, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_416 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
		    if (res) return res;
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
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GT_EXPR, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LT_EXPR, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
		    if (res) return res;
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
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LE_EXPR, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GE_EXPR, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LE_EXPR, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GE_EXPR, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case GT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GT_EXPR, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LT_EXPR, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case GE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GE_EXPR, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LE_EXPR, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, GE_EXPR, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_417 (loc, type, _p0, _p1, captures, LE_EXPR, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
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
	      if (integer_onep (_p1))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		    if ((wi::to_wide (captures[0]) & 1) != 0
)
		      {
			if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1722;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1722;
			{
			  tree res_op0;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[1];
			    _o1[1] =  build_zero_cst (TREE_TYPE (captures[1]));
			    _r1 = fold_build2_loc (loc, EQ_EXPR, boolean_type_node, _o1[0], _o1[1]);
			    res_op0 = _r1;
			  }
			  tree _r;
			  _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
			  if (TREE_SIDE_EFFECTS (captures[0]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 989, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1722:;
		      }
		    else
		      {
			if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1723;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1723;
			{
			  tree _r;
			  _r =  build_zero_cst (type);
			  if (TREE_SIDE_EFFECTS (captures[0]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
			  if (TREE_SIDE_EFFECTS (captures[1]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 990, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1723:;
		      }
		  }
	        }
	      break;
	    }
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_q21))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q30, _q21, _p1 };
			    tree res = generic_simplify_428 (loc, type, _p0, _p1, captures, LSHIFT_EXPR);
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
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q20, _q21, _p1 };
		      tree res = generic_simplify_429 (loc, type, _p0, _p1, captures, LSHIFT_EXPR);
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
    case RSHIFT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_q21))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q30, _q21, _p1 };
			    tree res = generic_simplify_428 (loc, type, _p0, _p1, captures, RSHIFT_EXPR);
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
	      switch (TREE_CODE (_q30))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_q30))
		    {
		    case CFN_BUILT_IN_BSWAP128:
		      if (call_expr_nargs (_q30) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_q30, 0);
			  switch (TREE_CODE (_q21))
			    {
			    case INTEGER_CST:
			      {
				switch (TREE_CODE (_p1))
				  {
				  case INTEGER_CST:
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q21, _p1 };
					tree res = generic_simplify_411 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP128);
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
		        }
		      break;
		    case CFN_BUILT_IN_BSWAP16:
		      if (call_expr_nargs (_q30) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_q30, 0);
			  switch (TREE_CODE (_q21))
			    {
			    case INTEGER_CST:
			      {
				switch (TREE_CODE (_p1))
				  {
				  case INTEGER_CST:
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q21, _p1 };
					tree res = generic_simplify_411 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP16);
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
		        }
		      break;
		    case CFN_BUILT_IN_BSWAP32:
		      if (call_expr_nargs (_q30) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_q30, 0);
			  switch (TREE_CODE (_q21))
			    {
			    case INTEGER_CST:
			      {
				switch (TREE_CODE (_p1))
				  {
				  case INTEGER_CST:
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q21, _p1 };
					tree res = generic_simplify_411 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP32);
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
		        }
		      break;
		    case CFN_BUILT_IN_BSWAP64:
		      if (call_expr_nargs (_q30) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_q30, 0);
			  switch (TREE_CODE (_q21))
			    {
			    case INTEGER_CST:
			      {
				switch (TREE_CODE (_p1))
				  {
				  case INTEGER_CST:
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q21, _p1 };
					tree res = generic_simplify_411 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP64);
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
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q20, _q21, _p1 };
		      tree res = generic_simplify_429 (loc, type, _p0, _p1, captures, RSHIFT_EXPR);
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
	  case CALL_EXPR:
	    switch (get_call_combined_fn (_q20))
	      {
	      case CFN_BUILT_IN_BSWAP128:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    switch (TREE_CODE (_q21))
		      {
		      case INTEGER_CST:
		        {
			  switch (TREE_CODE (_p1))
			    {
			    case INTEGER_CST:
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _p1 };
				  tree res = generic_simplify_411 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP128);
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
	          }
	        break;
	      case CFN_BUILT_IN_BSWAP16:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    switch (TREE_CODE (_q21))
		      {
		      case INTEGER_CST:
		        {
			  switch (TREE_CODE (_p1))
			    {
			    case INTEGER_CST:
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _p1 };
				  tree res = generic_simplify_411 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP16);
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
	          }
	        break;
	      case CFN_BUILT_IN_BSWAP32:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    switch (TREE_CODE (_q21))
		      {
		      case INTEGER_CST:
		        {
			  switch (TREE_CODE (_p1))
			    {
			    case INTEGER_CST:
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _p1 };
				  tree res = generic_simplify_411 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP32);
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
	          }
	        break;
	      case CFN_BUILT_IN_BSWAP64:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    switch (TREE_CODE (_q21))
		      {
		      case INTEGER_CST:
		        {
			  switch (TREE_CODE (_p1))
			    {
			    case INTEGER_CST:
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _p1 };
				  tree res = generic_simplify_411 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP64);
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
	          }
	        break;
	      default:;
	      }
	    break;
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
 && (TREE_CODE_CLASS (BIT_AND_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1724;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1724;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1724;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1724:;
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
 && (TREE_CODE_CLASS (BIT_AND_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1725;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1725;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1725;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1725;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1725:;
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
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_412 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP128);
		      if (res) return res;
		    }
		    break;
		  }
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
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_412 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP16);
		      if (res) return res;
		    }
		    break;
		  }
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
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_412 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP32);
		      if (res) return res;
		    }
		    break;
		  }
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
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		      tree res = generic_simplify_412 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP64);
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
 && (TREE_CODE_CLASS (BIT_AND_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1726;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1726;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1726;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1726;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1726:;
	    }
	}
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case ORDERED_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
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
			tree res = generic_simplify_430 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	      break;
	    }
          default:;
          }
	if ((_q21 == _q20 && ! TREE_SIDE_EFFECTS (_q21)) || (operand_equal_p (_q21, _q20, 0) && types_match (_q21, _q20)))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case ORDERED_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if ((_q51 == _q50 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q50, 0) && types_match (_q51, _q50)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q50 };
			if (types_match (captures[0], captures[1])
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1727;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, ORDERED_EXPR, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 991, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1727:;
			  }
		      }
		    }
		  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _q51 };
			tree res = generic_simplify_431 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _q50 };
			tree res = generic_simplify_431 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	  }
	switch (TREE_CODE (_p1))
	  {
	  case ORDERED_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p0, _q21 };
			tree res = generic_simplify_432 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q21, _p0, _q20 };
			tree res = generic_simplify_432 (loc, type, _p0, _p1, captures);
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
    case NE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case ORDERED_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			tree res = generic_simplify_430 (loc, type, _p0, _p1, captures);
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
    case MINUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_q21))
	        {
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_q21, 0);
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1 };
			    tree res = generic_simplify_433 (loc, type, _p0, _p1, captures, MINUS_EXPR);
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
    case PLUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_q21))
	        {
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_q21, 0);
		    switch (TREE_CODE (_p1))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1 };
			    tree res = generic_simplify_433 (loc, type, _p0, _p1, captures, PLUS_EXPR);
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
    case LT_EXPR:
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
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, LT_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, GT_EXPR, MAX_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case GT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, LT_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, GT_EXPR, MAX_EXPR);
		    if (res) return res;
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
	  case LE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, LE_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, GE_EXPR, MAX_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case GE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, LE_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, GE_EXPR, MAX_EXPR);
		    if (res) return res;
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
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, GT_EXPR, MAX_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, LT_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case LT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, GT_EXPR, MAX_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, LT_EXPR, MIN_EXPR);
		    if (res) return res;
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
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, GE_EXPR, MAX_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, LE_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case LE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, GE_EXPR, MAX_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, LE_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
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
	if (integer_onep (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
	      if (!TYPE_OVERFLOW_SANITIZED (type)
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1728;
		  {
		    tree res_op0;
		    res_op0 = captures[0];
		    tree res_op1;
		    res_op1 = captures[1];
		    tree _r;
		    _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 992, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1728:;
		}
	    }
	  }
        break;
      }
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
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1729;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[0];
				    _o1[1] = captures[2];
				    _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
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
next_after_fail1729:;
			      }
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
    case CALL_EXPR:
      switch (get_call_combined_fn (_p0))
        {
	case CFN_BUILT_IN_POPCOUNT:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      if (integer_onep (_p1))
		{
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    tree res = generic_simplify_434 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNT, CFN_BUILT_IN_PARITY);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_POPCOUNTLL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      if (integer_onep (_p1))
		{
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    tree res = generic_simplify_434 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNTLL, CFN_BUILT_IN_PARITYLL);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_POPCOUNT:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      if (integer_onep (_p1))
		{
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    tree res = generic_simplify_434 (loc, type, _p0, _p1, captures, CFN_POPCOUNT, CFN_PARITY);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_POPCOUNTIMAX:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      if (integer_onep (_p1))
		{
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    tree res = generic_simplify_434 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNTIMAX, CFN_BUILT_IN_PARITYIMAX);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_BUILT_IN_POPCOUNTL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      if (integer_onep (_p1))
		{
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    tree res = generic_simplify_434 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNTL, CFN_BUILT_IN_PARITYL);
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
  return NULL_TREE;
}
#pragma GCC diagnostic pop
