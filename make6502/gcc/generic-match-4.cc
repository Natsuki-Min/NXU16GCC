/* Generated automatically by the program `genmatch' from
   a IL pattern matching and simplification description.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-function"

#include "generic-match-auto.h"

bool
tree_negate_expr_p (tree t)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  switch (TREE_CODE (t))
    {
    case INTEGER_CST:
      {
	{
	  if ((INTEGRAL_TYPE_P (type)
 && TYPE_UNSIGNED (type))
 || (!TYPE_OVERFLOW_SANITIZED (type)
 && may_negate_without_overflow_p (t))
)
	    {
	      {
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 14, __FILE__, __LINE__, false);
		return true;
	      }
	    }
	}
        break;
      }
    case FIXED_CST:
      {
	{
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 15, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case NEGATE_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	  if (!TYPE_OVERFLOW_SANITIZED (type)
)
	    {
	      {
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 16, __FILE__, __LINE__, false);
		return true;
	      }
	    }
	}
        break;
      }
    case REAL_CST:
      {
	{
	  if (REAL_VALUE_NEGATIVE (TREE_REAL_CST (t))
)
	    {
	      {
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 17, __FILE__, __LINE__, false);
		return true;
	      }
	    }
	}
        break;
      }
    case VECTOR_CST:
      {
	{
	  if (FLOAT_TYPE_P (TREE_TYPE (type)) || TYPE_OVERFLOW_WRAPS (type)
)
	    {
	      {
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 18, __FILE__, __LINE__, false);
		return true;
	      }
	    }
	}
        break;
      }
    case MINUS_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if ((ANY_INTEGRAL_TYPE_P (type) && TYPE_OVERFLOW_WRAPS (type))
 || (FLOAT_TYPE_P (type)
 && !HONOR_SIGN_DEPENDENT_ROUNDING (type)
 && !HONOR_SIGNED_ZEROS (type))
)
	    {
	      {
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 19, __FILE__, __LINE__, false);
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
tree_usadd_right_part_1 (tree t, tree *res_ops)
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
		case LT_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    switch (TREE_CODE (_q30))
		      {
		      case PLUS_EXPR:
		        {
			  tree _q40 = TREE_OPERAND (_q30, 0);
			  tree _q41 = TREE_OPERAND (_q30, 1);
			  if ((_q31 == _q40 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _q40, 0) && types_match (_q31, _q40)))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q41 };
				if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
				  {
				    {
				      res_ops[0] = captures[0];
				      res_ops[1] = captures[1];
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 42, __FILE__, __LINE__, false);
				      return true;
				    }
				  }
			      }
			    }
			  if ((_q31 == _q41 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _q41, 0) && types_match (_q31, _q41)))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q41, _q40 };
				if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
				  {
				    {
				      res_ops[0] = captures[0];
				      res_ops[1] = captures[1];
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 42, __FILE__, __LINE__, false);
				      return true;
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
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    switch (TREE_CODE (_q31))
		      {
		      case PLUS_EXPR:
		        {
			  tree _q50 = TREE_OPERAND (_q31, 0);
			  tree _q51 = TREE_OPERAND (_q31, 1);
			  if ((_q50 == _q30 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q30, 0) && types_match (_q50, _q30)))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q51 };
				if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
				  {
				    {
				      res_ops[0] = captures[0];
				      res_ops[1] = captures[1];
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 43, __FILE__, __LINE__, false);
				      return true;
				    }
				  }
			      }
			    }
			  if ((_q51 == _q30 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q30, 0) && types_match (_q51, _q30)))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q50 };
				if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
				  {
				    {
				      res_ops[0] = captures[0];
				      res_ops[1] = captures[1];
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 43, __FILE__, __LINE__, false);
				      return true;
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

bool
tree_cond_expr_convert_p (tree t, tree *res_ops)
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
	  case LT_EXPR:
	  case LE_EXPR:
	  case EQ_EXPR:
	  case NE_EXPR:
	  case GE_EXPR:
	  case GT_EXPR:
	    {
	      tree _q20 = TREE_OPERAND (_p0, 0);
	      tree _q21 = TREE_OPERAND (_p0, 1);
	      switch (TREE_CODE (_p1))
	        {
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    switch (TREE_CODE (_p2))
		      {
		      CASE_CONVERT:
		        {
			  tree _q70 = TREE_OPERAND (_p2, 0);
			  {
			    tree captures[7] ATTRIBUTE_UNUSED = { _q20, _q50, _q70, _p0, _q21, _p1, _p2 };
			    if (INTEGRAL_TYPE_P (type)
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[2]))
 && TYPE_PRECISION (type) != TYPE_PRECISION (TREE_TYPE (captures[0]))
 && TYPE_PRECISION (TREE_TYPE (captures[0]))
 == TYPE_PRECISION (TREE_TYPE (captures[1]))
 && TYPE_PRECISION (TREE_TYPE (captures[0]))
 == TYPE_PRECISION (TREE_TYPE (captures[2]))
 && (TYPE_PRECISION (TREE_TYPE (captures[0])) > TYPE_PRECISION (type)
 || (TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 == TYPE_UNSIGNED (TREE_TYPE (captures[2]))))
 && single_use (captures[5])
 && single_use (captures[6])
)
			      {
				{
				  res_ops[0] = captures[0];
				  res_ops[1] = captures[1];
				  res_ops[2] = captures[2];
				  res_ops[3] = captures[3];
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 86, __FILE__, __LINE__, false);
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
generic_simplify_3 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (outer_op),
 const enum tree_code ARG_UNUSED (inner_op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (real_zerop (captures[2])
 && real_zerop (captures[3])
 && !HONOR_SIGN_DEPENDENT_ROUNDING (type)
)
    {
      {
 bool inner_plus = ((inner_op == PLUS_EXPR)
 ^ REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (captures[2])));
 bool outer_plus
 = ((outer_op == PLUS_EXPR)
 ^ REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (captures[3])));
	  if (outer_plus && !inner_plus
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail221;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		res_op1 = captures[3];
		tree _r;
		_r = fold_build2_loc (loc, outer_op, type, res_op0, res_op1);
		if (TREE_SIDE_EFFECTS (captures[2]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 91, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail221:;
	    }
	  else
	    {
	      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail222;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail222;
	      {
		tree _r;
		_r = captures[0];
		if (TREE_SIDE_EFFECTS (captures[3]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 92, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail222:;
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_8 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail227;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 = captures[1];
    tree _r;
    _r = fold_build2_loc (loc, BIT_IOR_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 97, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail227:;
  return NULL_TREE;
}

tree
generic_simplify_10 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[1]))
 && tree_nop_conversion_p (type, TREE_TYPE (captures[4]))
 && (wi::to_wide (captures[2]) & wi::to_wide (captures[5])) == 0
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail229;
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail229;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail229;
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
	_r = fold_build2_loc (loc, BIT_IOR_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 99, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail229:;
    }
  return NULL_TREE;
}

tree
generic_simplify_13 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_SATURATING (type)
)
    {
      if (tree_nop_conversion_p (type, TREE_TYPE (captures[1]))
 && !TYPE_OVERFLOW_SANITIZED (type)
)
	{
	  {
 tree t1 = type;
 if (INTEGRAL_TYPE_P (type)
 && TYPE_OVERFLOW_WRAPS (type) != TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[1])))
 t1 = TYPE_OVERFLOW_WRAPS (type) ? type : TREE_TYPE (captures[1]);
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail232;
	      {
		tree res_op0;
		{
		  tree _o1[2], _r1;
		  {
		    tree _o2[1], _r2;
		    _o2[0] = captures[0];
		    if (TREE_TYPE (_o2[0]) != t1)
		      {
			_r2 = fold_build1_loc (loc, NOP_EXPR, t1, _o2[0]);
		      }
		    else
		      _r2 = _o2[0];
		    _o1[0] = _r2;
		  }
		  {
		    tree _o2[1], _r2;
		    _o2[0] = captures[1];
		    if (TREE_TYPE (_o2[0]) != t1)
		      {
			_r2 = fold_build1_loc (loc, NOP_EXPR, t1, _o2[0]);
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
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 102, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail232:;
	  }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_18 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail245;
	  {
	    tree res_op0;
	    res_op0 = captures[2];
	    tree res_op1;
	    res_op1 = captures[1];
	    tree _r;
	    _r = fold_build2_loc (loc, PLUS_EXPR, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 115, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail245:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_19 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail246;
	  {
	    tree res_op0;
	    res_op0 = captures[2];
	    tree res_op1;
	    res_op1 = captures[1];
	    tree _r;
	    _r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 116, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail246:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_21 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
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
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_PRECISION (type) <= TYPE_PRECISION (TREE_TYPE (captures[0]))
 && types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[1]))
 && !TYPE_OVERFLOW_TRAPS (type)
 && !TYPE_OVERFLOW_SANITIZED (type)
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail248;
	      {
		tree res_op0;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[1];
		  _r1 = fold_build2_loc (loc, op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail248;
		  res_op0 = _r1;
		}
		tree _r;
		_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 118, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail248:;
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_24 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (plusminus))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_SATURATING (type)
 && (!FLOAT_TYPE_P (type) || flag_associative_math)
)
    {
      if (!ANY_INTEGRAL_TYPE_P (type)
 || TYPE_OVERFLOW_WRAPS (type)
 || (INTEGRAL_TYPE_P (type)
 && tree_expr_nonzero_p (captures[1])
 && expr_not_equal_to (captures[1], wi::minus_one (TYPE_PRECISION (type))))
)
	{
	  if (single_use (captures[0]) || single_use (captures[3])
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail251;
	      {
		tree res_op0;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, plusminus, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  res_op0 = _r1;
		}
		tree res_op1;
		res_op1 = captures[1];
		tree _r;
		_r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 121, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail251:;
	    }
	  else
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail252;
	      {
		tree res_op0;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, plusminus, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  res_op0 = _r1;
		}
		tree res_op1;
		res_op1 = captures[1];
		tree _r;
		_r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 122, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail252:;
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_29 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (div),
 const enum tree_code ARG_UNUSED (mod))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail257;
  {
    tree _r;
    _r = captures[0];
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 127, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail257:;
  return NULL_TREE;
}

tree
generic_simplify_33 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (POPCOUNT))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && (wi::bit_and (widest_int::from (tree_nonzero_bits (captures[1]), UNSIGNED),
 widest_int::from (tree_nonzero_bits (captures[3]), UNSIGNED))
 == 0)
)
    {
      {
 tree utype = TREE_TYPE (captures[1]);
 if (TYPE_PRECISION (utype) < TYPE_PRECISION (TREE_TYPE (captures[3])))
 utype = TREE_TYPE (captures[3]);
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail261;
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
		_o2[0] = captures[3];
		if (TREE_TYPE (_o2[0]) != utype)
		  {
		    _r2 = fold_build1_loc (loc, NOP_EXPR, utype, _o2[0]);
		  }
		else
		  _r2 = _o2[0];
		_o1[1] = _r2;
	      }
	      _r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = maybe_build_call_expr_loc (loc, POPCOUNT, type, 1, res_op0);
	    if (!_r)
	      goto next_after_fail261;
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 131, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail261:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_41 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail269;
  {
    tree res_op0;
    res_op0 = unshare_expr (captures[2]);
    tree res_op1;
    {
      tree _o1[2], _r1;
      _o1[0] = captures[1];
      _o1[1] = captures[2];
      _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      res_op1 = _r1;
    }
    tree _r;
    _r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 139, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail269:;
  return NULL_TREE;
}

tree
generic_simplify_47 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_SATURATING (type)
)
    {
      if (tree_nop_conversion_p (type, TREE_TYPE (captures[1]))
 && !TYPE_OVERFLOW_SANITIZED (type)
)
	{
	  {
 tree t1 = type;
 if (INTEGRAL_TYPE_P (type)
 && TYPE_OVERFLOW_WRAPS (type) != TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[1])))
 t1 = TYPE_OVERFLOW_WRAPS (type) ? type : TREE_TYPE (captures[1]);
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail275;
	      {
		tree res_op0;
		{
		  tree _o1[2], _r1;
		  {
		    tree _o2[1], _r2;
		    _o2[0] = captures[0];
		    if (TREE_TYPE (_o2[0]) != t1)
		      {
			_r2 = fold_build1_loc (loc, NOP_EXPR, t1, _o2[0]);
		      }
		    else
		      _r2 = _o2[0];
		    _o1[0] = _r2;
		  }
		  {
		    tree _o2[1], _r2;
		    _o2[0] = captures[1];
		    if (TREE_TYPE (_o2[0]) != t1)
		      {
			_r2 = fold_build1_loc (loc, NOP_EXPR, t1, _o2[0]);
		      }
		    else
		      _r2 = _o2[0];
		    _o1[1] = _r2;
		  }
		  _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  res_op0 = _r1;
		}
		tree _r;
		_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 144, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail275:;
	  }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_55 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (element_precision (type) <= element_precision (TREE_TYPE (captures[1]))
 || (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[1])))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail287;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree _r;
		_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		if (TREE_SIDE_EFFECTS (captures[2]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 156, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail287:;
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_61 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (log1),
 const enum tree_code ARG_UNUSED (log2),
 const combined_fn ARG_UNUSED (popcount))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail295;
  {
    tree res_op0;
    {
      tree _o1[2], _r1;
      _o1[0] = captures[2];
      _o1[1] = captures[4];
      _r1 = fold_build2_loc (loc, log2, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      res_op0 = _r1;
    }
    tree _r;
    _r = maybe_build_call_expr_loc (loc, popcount, type, 1, res_op0);
    if (!_r)
      goto next_after_fail295;
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 164, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail295:;
  return NULL_TREE;
}

tree
generic_simplify_65 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail299;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail299;
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
	  _r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree res_op1;
	res_op1 = captures[1];
	tree _r;
	_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 168, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail299:;
    }
  return NULL_TREE;
}

tree
generic_simplify_74 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail308;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail308;
  {
    tree _r;
    _r = captures[0];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 176, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail308:;
  return NULL_TREE;
}

tree
generic_simplify_81 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_PRECISION (TREE_TYPE (captures[0])) == 1
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail315;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail315;
      {
	tree _r;
	_r = captures[2];
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 178, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail315:;
    }
  return NULL_TREE;
}

tree
generic_simplify_89 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail323;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail323;
  {
    tree _r;
    _r = captures[0];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 183, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail323:;
  return NULL_TREE;
}

tree
generic_simplify_94 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[3]))
 && tree_nop_conversion_p (type, TREE_TYPE (captures[6]))
)
    {
      if (single_use (captures[4]) && single_use (captures[5])
)
	{
	  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail329;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail329;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree res_op1;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[6];
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
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 189, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail329:;
	}
      else
	{
	  if (single_use (captures[0]) && single_use (captures[1])
)
	    {
	      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail330;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail330;
	      {
		tree res_op0;
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[3];
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
		_r = fold_build2_loc (loc, op, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 190, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail330:;
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_103 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (bitwise_equal_p (captures[4], captures[7])
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail341;
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
next_after_fail341:;
    }
  return NULL_TREE;
}

tree
generic_simplify_113 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!CONSTANT_CLASS_P (captures[0])
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail352;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  _o1[1] = captures[2];
	  _r1 = fold_build2_loc (loc, bitop, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  if (EXPR_P (_r1))
	    goto next_after_fail352;
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, bitop, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 202, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail352:;
    }
  else
    {
      {
 tree cst1 = const_binop (bitop, type, captures[0], captures[2]);
	  if (cst1
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail353;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail353;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail353;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		res_op1 =  cst1;
		tree _r;
		_r = fold_build2_loc (loc, bitop, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 203, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail353:;
	    }
	  else
	    {
	      {
 tree cst2 = const_binop (bitop, type, captures[1], captures[2]);
		  if (cst2
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail354;
		      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail354;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail354;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail354;
		      {
			tree res_op0;
			res_op0 = captures[0];
			tree res_op1;
			res_op1 =  cst2;
			tree _r;
			_r = fold_build2_loc (loc, bitop, type, res_op0, res_op1);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 204, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail354:;
		    }
	      }
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_127 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail390;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail390;
  {
    tree _r;
    _r = captures[2];
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 214, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail390:;
  return NULL_TREE;
}

tree
generic_simplify_131 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail395;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail395;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail395;
	      {
		tree _r;
		_r = captures[3];
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 217, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail395:;
	    }
	  else
	    {
	      if (code1 == NE_EXPR && val && allbits
)
		{
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail396;
		  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail396;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail396;
		  {
		    tree _r;
		    _r =  constant_boolean_node (true, type);
		    if (TREE_SIDE_EFFECTS (captures[1]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 218, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail396:;
		}
	      else
		{
		  if (code1 == NE_EXPR && !val && allbits
)
		    {
		      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail397;
		      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail397;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail397;
		      {
			tree _r;
			_r = captures[0];
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 219, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail397:;
		    }
		  else
		    {
		      if (code1 == EQ_EXPR
 && code2 == GT_EXPR
 && cmp == 0
 && allbits
)
			{
			  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail398;
			  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail398;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail398;
			  {
			    tree res_op0;
			    res_op0 = captures[4];
			    tree res_op1;
			    res_op1 = captures[5];
			    tree _r;
			    _r = fold_build2_loc (loc, GE_EXPR, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 220, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail398:;
			}
		      else
			{
			  if (code1 == EQ_EXPR
 && code2 == LT_EXPR
 && cmp == 0
 && allbits
)
			    {
			      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail399;
			      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail399;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail399;
			      {
				tree res_op0;
				res_op0 = captures[4];
				tree res_op1;
				res_op1 = captures[5];
				tree _r;
				_r = fold_build2_loc (loc, LE_EXPR, type, res_op0, res_op1);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 221, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail399:;
			    }
			  else
			    {
			      if (code1 == EQ_EXPR
 && code2 == GE_EXPR
 && one_before
 && allbits
)
				{
				  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail400;
				  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail400;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail400;
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
				    _r = fold_build2_loc (loc, GE_EXPR, type, res_op0, res_op1);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 222, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail400:;
				}
			      else
				{
				  if (code1 == EQ_EXPR
 && code2 == LE_EXPR
 && one_after
 && allbits
)
				    {
				      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail401;
				      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail401;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail401;
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
					_r = fold_build2_loc (loc, LE_EXPR, type, res_op0, res_op1);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 223, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail401:;
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
generic_simplify_158 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail435;
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
	  tree _o1[1], _r1;
	  _o1[0] = captures[1];
	  _r1 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, BIT_XOR_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 254, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail435:;
    }
  return NULL_TREE;
}

tree
generic_simplify_167 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (PARITY))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (types_match (TREE_TYPE (captures[1]), TREE_TYPE (captures[3]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail445;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  _o1[1] = captures[3];
	  _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree _r;
	_r = maybe_build_call_expr_loc (loc, PARITY, type, 1, res_op0);
	if (!_r)
	  goto next_after_fail445;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 263, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail445:;
    }
  else
    {
      if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[3]))
)
	{
	  {
 tree utype = TREE_TYPE (captures[1]);
 if (TYPE_PRECISION (utype) < TYPE_PRECISION (TREE_TYPE (captures[3])))
 utype = TREE_TYPE (captures[3]);
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail446;
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
		    _o2[0] = captures[3];
		    if (TREE_TYPE (_o2[0]) != utype)
		      {
			_r2 = fold_build1_loc (loc, NOP_EXPR, utype, _o2[0]);
		      }
		    else
		      _r2 = _o2[0];
		    _o1[1] = _r2;
		  }
		  _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  res_op0 = _r1;
		}
		tree _r;
		_r = maybe_build_call_expr_loc (loc, PARITY, type, 1, res_op0);
		if (!_r)
		  goto next_after_fail446;
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 264, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail446:;
	  }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_184 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_CODE (captures[3]) != INTEGER_CST
 && single_use (captures[0])
 && !integer_zerop (captures[2]) && !integer_minus_onep (captures[2])
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail466;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  _o1[1] = captures[3];
	  _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree res_op1;
	res_op1 = captures[2];
	tree _r;
	_r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 278, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail466:;
    }
  return NULL_TREE;
}

tree
generic_simplify_191 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (mod))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
    {
      {
 tree utype = unsigned_type_for (TREE_TYPE (captures[0]));
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail473;
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
		tree _o2[1], _r2;
		_o2[0] = captures[1];
		if (TREE_TYPE (_o2[0]) != utype)
		  {
		    _r2 = fold_build1_loc (loc, NOP_EXPR, utype, _o2[0]);
		  }
		else
		  _r2 = _o2[0];
		_o1[1] = _r2;
	      }
	      _r1 = fold_build2_loc (loc, mod, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      res_op0 = _r1;
	    }
	    tree res_op1;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[2];
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
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 285, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail473:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_202 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))
 || ((op == EQ_EXPR || op == NE_EXPR)
 && TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[0]))))
 && (CONSTANT_CLASS_P (captures[2]) || single_use (captures[1]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail500;
      {
	tree res_op0;
	res_op0 = captures[2];
	tree res_op1;
	res_op1 =  build_zero_cst (TREE_TYPE (captures[2]));
	tree _r;
	_r = fold_build2_loc (loc, op, type, res_op0, res_op1);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 311, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail500:;
    }
  return NULL_TREE;
}

tree
generic_simplify_210 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (wi::bit_and_not (wi::to_wide (captures[1]), get_nonzero_bits (captures[0])) != 0
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail508;
      {
	tree _r;
	_r =  constant_boolean_node (cmp == NE_EXPR, type);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 318, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail508:;
    }
  return NULL_TREE;
}

tree
generic_simplify_215 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 int c1 = wi::clz (wi::to_wide (captures[0]));
 int c2 = wi::clz (wi::to_wide (captures[2]));
      if (c1 < c2
)
	{
	  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail515;
	  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail515;
	  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail515;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail515;
	  {
	    tree _r;
	    _r =  constant_boolean_node (cmp == NE_EXPR ? false : true, type);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 325, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail515:;
	}
      else
	{
	  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail516;
	  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail516;
	  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail516;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail516;
	  {
	    tree res_op0;
	    res_op0 = captures[1];
	    tree res_op1;
	    res_op1 =  build_int_cst (TREE_TYPE (captures[1]), c1 - c2);
	    tree _r;
	    _r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 326, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail516:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_223 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (rotate),
 const enum tree_code ARG_UNUSED (invrot),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail533;
  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail533;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail533;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 =  const_binop (invrot, TREE_TYPE (captures[0]), captures[2], captures[1]);
    tree _r;
    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 343, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail533:;
  return NULL_TREE;
}

tree
generic_simplify_228 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp),
 const enum tree_code ARG_UNUSED (ncmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (types_match (type, TREE_TYPE (captures[0]))
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail538;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail538;
      {
	tree res_op0;
	res_op0 = captures[1];
	tree res_op1;
	res_op1 = captures[2];
	tree _r;
	_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 348, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail538:;
    }
  return NULL_TREE;
}

tree
generic_simplify_232 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (scmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (captures[2])
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail543;
      {
	tree res_op0;
	res_op0 = captures[2];
	tree res_op1;
	res_op1 = captures[3];
	tree _r;
	_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 352, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail543:;
    }
  else
    {
      if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[1]))
 && single_use (captures[0])
)
	{
	  if (tree_int_cst_sgn (captures[2]) < 0
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail544;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		res_op1 = captures[3];
		tree _r;
		_r = fold_build2_loc (loc, scmp, type, res_op0, res_op1);
		if (TREE_SIDE_EFFECTS (captures[2]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 353, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail544:;
	    }
	  else
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail545;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		res_op1 = captures[3];
		tree _r;
		_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		if (TREE_SIDE_EFFECTS (captures[2]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 354, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail545:;
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_241 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_associative_math
)
    {
      {
 tree tem = const_binop (MINUS_EXPR, TREE_TYPE (captures[1]), captures[0], captures[2]);
	  if (tem && !TREE_OVERFLOW (tem)
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail571;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail571;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail571;
	      {
		tree res_op0;
		res_op0 =  tem;
		tree res_op1;
		res_op1 = captures[1];
		tree _r;
		_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 379, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail571:;
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_246 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!integer_zerop (captures[1])
)
    {
      if (wi::to_wide (captures[2]) == 0
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail601;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree res_op1;
	    res_op1 = captures[2];
	    tree _r;
	    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 409, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail601:;
	}
      else
	{
	  if (TREE_CODE (captures[1]) == INTEGER_CST
)
	    {
	      {
 wi::overflow_type ovf;
 wide_int prod = wi::mul (wi::to_wide (captures[2]), wi::to_wide (captures[1]),
 TYPE_SIGN (TREE_TYPE (captures[1])), &ovf);
		  if (ovf
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail602;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail602;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail602;
		      {
			tree _r;
			_r =  constant_boolean_node (cmp == NE_EXPR, type);
			if (TREE_SIDE_EFFECTS (captures[0]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 410, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail602:;
		    }
		  else
		    {
		      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail603;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail603;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail603;
		      {
			tree res_op0;
			res_op0 = captures[0];
			tree res_op1;
			res_op1 =  wide_int_to_tree (TREE_TYPE (captures[0]), prod);
			tree _r;
			_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 411, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail603:;
		    }
	      }
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_252 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 poly_int64 off; tree base;
 tree addr = (TREE_CODE (captures[0]) == SSA_NAME
 ? gimple_assign_rhs1 (SSA_NAME_DEF_STMT (captures[0])) : captures[0]);
      if (SSA_NAME_IS_DEFAULT_DEF (captures[1])
 && TREE_CODE (SSA_NAME_VAR (captures[1])) == PARM_DECL
 && (base = get_base_address (TREE_OPERAND (addr, 0)))
 && TREE_CODE (base) == VAR_DECL
 && auto_var_in_fn_p (base, current_function_decl)
)
	{
	  if (cmp == NE_EXPR
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail621;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail621;
	      {
		tree _r;
		_r =  constant_boolean_node (true, type);
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 422, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail621:;
	    }
	  else
	    {
	      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail622;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail622;
	      {
		tree _r;
		_r =  constant_boolean_node (false, type);
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 423, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail622:;
	    }
	}
      else
	{
	  if ((base = get_addr_base_and_unit_offset (TREE_OPERAND (addr, 0), &off))
 && TREE_CODE (base) == MEM_REF
 && TREE_OPERAND (base, 0) == captures[1]
)
	    {
	      {
 off += mem_ref_offset (base).force_shwi ();
		  if (known_ne (off, 0)
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail623;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail623;
		      {
			tree _r;
			_r =  constant_boolean_node (cmp == NE_EXPR, type);
			if (TREE_SIDE_EFFECTS (captures[1]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 424, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail623:;
		    }
		  else
		    {
		      if (known_eq (off, 0)
)
			{
			  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail624;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail624;
			  {
			    tree _r;
			    _r =  constant_boolean_node (cmp == EQ_EXPR, type);
			    if (TREE_SIDE_EFFECTS (captures[1]))
			      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 425, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail624:;
			}
		    }
	      }
	    }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_259 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_expr_nonzero_p (captures[0])
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail631;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail631;
      {
	tree _r;
	_r =  constant_boolean_node (cmp == NE_EXPR, type);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 432, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail631:;
    }
  return NULL_TREE;
}

tree
generic_simplify_263 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail635;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail635;
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
next_after_fail635:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_269 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (neeq))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (POINTER_TYPE_P (TREE_TYPE (captures[0]))
 && ptrs_compare_unequal (captures[0], captures[1])
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail655;
      {
	tree _r;
	_r =  constant_boolean_node (neeq != EQ_EXPR, type);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 446, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail655:;
    }
  return NULL_TREE;
}

tree
generic_simplify_271 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (inner),
 const enum tree_code ARG_UNUSED (outer))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 bool cst1 = integer_onep (captures[1]);
 bool cst0 = integer_zerop (captures[1]);
 bool innereq = inner == EQ_EXPR;
 bool outereq = outer == EQ_EXPR;
      if (innereq ? cst0 : cst1
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail657;
	  {
	    tree _r;
	    _r =  constant_boolean_node (!outereq, type);
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 448, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail657:;
	}
      else
	{
	  if (innereq ? cst1 : cst0
)
	    {
	      {
 tree utype = unsigned_type_for (TREE_TYPE (captures[0]));
 tree ucst1 = build_one_cst (utype);
		  if (!outereq
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail658;
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
			res_op1 =  ucst1;
			tree _r;
			_r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
			if (TREE_SIDE_EFFECTS (captures[1]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 449, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail658:;
		    }
		  else
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail659;
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
			res_op1 =  ucst1;
			tree _r;
			_r = fold_build2_loc (loc, LE_EXPR, type, res_op0, res_op1);
			if (TREE_SIDE_EFFECTS (captures[1]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 450, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail659:;
		    }
	      }
	    }
	  else
	    {
	      {
 tree value = build_int_cst (TREE_TYPE (captures[0]), !innereq);
		  if (outereq
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail660;
		      {
			tree res_op0;
			res_op0 = captures[0];
			tree res_op1;
			res_op1 =  value;
			tree _r;
			_r = fold_build2_loc (loc, EQ_EXPR, type, res_op0, res_op1);
			if (TREE_SIDE_EFFECTS (captures[1]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 451, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail660:;
		    }
		  else
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail661;
		      {
			tree res_op0;
			res_op0 = captures[0];
			tree res_op1;
			res_op1 =  value;
			tree _r;
			_r = fold_build2_loc (loc, NE_EXPR, type, res_op0, res_op1);
			if (TREE_SIDE_EFFECTS (captures[1]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 452, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail661:;
		    }
	      }
	    }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_281 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (VECTOR_MODE_P (TYPE_MODE (TREE_TYPE (captures[0])))
 && can_compare_p (cmp == EQ_EXPR ? EQ : NE, TYPE_MODE (TREE_TYPE (captures[0])),
 ccp_jump)
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail686;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail686;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 =  build_zero_cst (TREE_TYPE (captures[0]));
	tree _r;
	_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 477, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail686:;
    }
  return NULL_TREE;
}

tree
generic_simplify_283 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp),
 const enum tree_code ARG_UNUSED (ncmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (types_match (type, TREE_TYPE (captures[0]))
)
    {
      {
 enum tree_code ic = invert_tree_comparison
 (cmp, HONOR_NANS (captures[1]));
	  if (ic == icmp
)
	    {
	      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail688;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail688;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		res_op1 = captures[2];
		tree _r;
		_r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 479, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail688:;
	    }
	  else
	    {
	      if (ic == ncmp
)
		{
		  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail689;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail689;
		  {
		    tree res_op0;
		    res_op0 = captures[1];
		    tree res_op1;
		    res_op1 = captures[2];
		    tree _r;
		    _r = fold_build2_loc (loc, ncmp, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 480, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail689:;
		}
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_292 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((TYPE_UNSIGNED (type) || tree_expr_nonnegative_p (captures[0]))
 && (!VECTOR_TYPE_P (type)
 || target_supports_op_p (type, RSHIFT_EXPR, optab_vector)
 || target_supports_op_p (type, RSHIFT_EXPR, optab_scalar))
 && (useless_type_conversion_p (type, TREE_TYPE (captures[2]))
 || (element_precision (type) >= element_precision (TREE_TYPE (captures[2]))
 && (TYPE_UNSIGNED (TREE_TYPE (captures[2]))
 || (element_precision (type)
 == element_precision (TREE_TYPE (captures[2])))
 || (INTEGRAL_TYPE_P (type)
 && (tree_nonzero_bits (captures[0])
 & wi::mask (element_precision (TREE_TYPE (captures[2])) - 1,
 true,
 element_precision (type))) == 0))))
)
    {
      if (!VECTOR_TYPE_P (type)
 && useless_type_conversion_p (TREE_TYPE (captures[1]), TREE_TYPE (captures[2]))
 && element_precision (TREE_TYPE (captures[1])) < element_precision (type)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail699;
	  {
	    tree res_op0;
	    {
	      tree _o1[2], _r1;
	      _o1[0] = captures[1];
	      _o1[1] = captures[3];
	      _r1 = fold_build2_loc (loc, RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	    if (TREE_SIDE_EFFECTS (captures[2]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 488, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail699:;
	}
      else
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail700;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree res_op1;
	    res_op1 = captures[3];
	    tree _r;
	    _r = fold_build2_loc (loc, RSHIFT_EXPR, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[2]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 489, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail700:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_307 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (mod))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!integer_zerop (captures[0])
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail716;
      {
	tree _r;
	_r =  build_zero_cst (type);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 505, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail716:;
    }
  return NULL_TREE;
}

tree
generic_simplify_315 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (SIN),
 const combined_fn ARG_UNUSED (TAN),
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail726;
	  {
	    tree res_op0;
	    res_op0 = captures[1];
	    tree _r;
	    _r = maybe_build_call_expr_loc (loc, COS, type, 1, res_op0);
	    if (!_r)
	      goto next_after_fail726;
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 515, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail726:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_322 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op),
 const enum tree_code ARG_UNUSED (neg_op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations
)
    {
      if (!HONOR_SIGNED_ZEROS (captures[1]) && !HONOR_INFINITIES (captures[1])
)
	{
	  if (real_less (&dconst0, TREE_REAL_CST_PTR (captures[0]))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail733;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		res_op1 = captures[2];
		tree _r;
		_r = fold_build2_loc (loc, op, type, res_op0, res_op1);
		if (TREE_SIDE_EFFECTS (captures[0]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 522, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail733:;
	    }
	  else
	    {
	      if (real_less (TREE_REAL_CST_PTR (captures[0]), &dconst0)
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail734;
		  {
		    tree res_op0;
		    res_op0 = captures[1];
		    tree res_op1;
		    res_op1 = captures[2];
		    tree _r;
		    _r = fold_build2_loc (loc, neg_op, type, res_op0, res_op1);
		    if (TREE_SIDE_EFFECTS (captures[0]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 523, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail734:;
		}
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_333 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (acmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree cst = uniform_integer_cst_p (captures[1]);
      if (tree_int_cst_sgn (cst) == 1
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail747;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree res_op1;
	    res_op1 =  build_uniform_cst (TREE_TYPE (captures[1]),
 wide_int_to_tree (TREE_TYPE (cst),
 wi::to_wide (cst) - 1));
	    tree _r;
	    _r = fold_build2_loc (loc, acmp, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 536, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail747:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_341 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail787;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail787;
      {
	tree res_op0;
	res_op0 = captures[1];
	tree res_op1;
	res_op1 = captures[0];
	tree _r;
	_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 559, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail787:;
    }
  return NULL_TREE;
}

tree
generic_simplify_345 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (neg_cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations
)
    {
      {
 tree tem = const_binop (RDIV_EXPR, type, captures[2], captures[1]);
	  if (tem
 && !(REAL_VALUE_ISINF (TREE_REAL_CST (tem))
 || (real_zerop (tem) && !real_zerop (captures[1])))
)
	    {
	      if (real_less (&dconst0, TREE_REAL_CST_PTR (captures[1]))
)
		{
		  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail791;
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail791;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail791;
		  {
		    tree res_op0;
		    res_op0 = captures[0];
		    tree res_op1;
		    res_op1 =  tem;
		    tree _r;
		    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 563, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail791:;
		}
	      else
		{
		  if (real_less (TREE_REAL_CST_PTR (captures[1]), &dconst0)
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail792;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail792;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail792;
		      {
			tree res_op0;
			res_op0 = captures[0];
			tree res_op1;
			res_op1 =  tem;
			tree _r;
			_r = fold_build2_loc (loc, neg_cmp, type, res_op0, res_op1);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 564, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail792:;
		    }
		}
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_352 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail805;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail805;
      {
	tree res_op0;
	res_op0 = captures[1];
	tree res_op1;
	res_op1 = captures[0];
	tree _r;
	_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 559, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail805:;
    }
  return NULL_TREE;
}

tree
generic_simplify_357 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && !TYPE_UNSIGNED (type)
 && !TYPE_OVERFLOW_TRAPS (type)
 && tree_nop_conversion_p (type, TREE_TYPE (captures[1]))
 && (expr_not_equal_to (captures[0], wi::to_wide (TYPE_MIN_VALUE (type)))
 || expr_not_equal_to (captures[1], wi::minus_one (TYPE_PRECISION
 (TREE_TYPE (captures[1])))))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail814;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[1];
	  if (TREE_TYPE (_o1[0]) != type)
	    {
	      _r1 = fold_build1_loc (loc, NOP_EXPR, type, _o1[0]);
	    }
	  else
	    _r1 = _o1[0];
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, TRUNC_MOD_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 582, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail814:;
    }
  return NULL_TREE;
}

tree
generic_simplify_362 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (shiftrotate))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail819;
  {
    tree _r;
    _r = captures[0];
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 587, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail819:;
  return NULL_TREE;
}

tree
generic_simplify_368 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (wi::ltu_p (wi::to_wide (captures[1]), element_precision (type))
)
    {
      if (TYPE_UNSIGNED (type)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail828;
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
	      _o1[0] =  build_minus_one_cst (type);
	      _o1[1] = captures[1];
	      _r1 = fold_build2_loc (loc, RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      res_op1 = _r1;
	    }
	    tree _r;
	    _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 594, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail828:;
	}
      else
	{
	  if (INTEGRAL_TYPE_P (type)
)
	    {
	      {
 int width = element_precision (type) - tree_to_uhwi (captures[1]);
 tree stype = NULL_TREE;
 if (width <= MAX_FIXED_MODE_SIZE)
 stype = build_nonstandard_integer_type (width, 0);
		  if (stype && (width == 1 || type_has_mode_precision_p (stype))
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail829;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail829;
		      {
			tree res_op0;
			{
			  tree _o1[1], _r1;
			  _o1[0] = captures[0];
			  if (TREE_TYPE (_o1[0]) != stype)
			    {
			      _r1 = fold_build1_loc (loc, NOP_EXPR, stype, _o1[0]);
			    }
			  else
			    _r1 = _o1[0];
			  res_op0 = _r1;
			}
			tree _r;
			_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 595, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail829:;
		    }
	      }
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_374 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail842;
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
next_after_fail842:;
	}
      else
	{
	  {
 tree utype = unsigned_type_for (TREE_TYPE (captures[2]));
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail843;
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
next_after_fail843:;
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
			  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail844;
			  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail844;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail844;
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
next_after_fail844:;
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
				  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail845;
				  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail845;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail845;
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
next_after_fail845:;
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
generic_simplify_400 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TREE_CODE (TREE_TYPE (captures[0])) != BOOLEAN_TYPE
 && (!TYPE_UNSIGNED (TREE_TYPE (captures[0]))
 || TYPE_PRECISION (type) <= TYPE_PRECISION (TREE_TYPE (captures[0])))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail877;
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
	res_op1 = captures[1];
	tree _r;
	_r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 630, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail877:;
    }
  return NULL_TREE;
}

tree
generic_simplify_408 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (POINTER_TYPE_P (TREE_TYPE (captures[0]))
 && tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
    {
      {
 unsigned int align;
 unsigned HOST_WIDE_INT bitpos;
 get_pointer_alignment_1 (captures[0], &align, &bitpos);
	  if (wi::ltu_p (wi::to_wide (captures[1]), align / BITS_PER_UNIT)
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail885;
	      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail885;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail885;
	      {
		tree _r;
		_r =  wide_int_to_tree (type, (wi::to_wide (captures[1])
 & (bitpos / BITS_PER_UNIT)));
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 637, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail885:;
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_417 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
		  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail913;
		  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail913;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail913;
		  {
		    tree _r;
		    _r = captures[0];
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 655, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail913:;
		}
	      else
		{
		  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail914;
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail914;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail914;
		  {
		    tree _r;
		    _r = captures[3];
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 656, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail914:;
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
		      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail915;
		      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail915;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail915;
		      {
			tree _r;
			_r = captures[0];
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 657, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail915:;
		    }
		  else
		    {
		      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail916;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail916;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail916;
		      {
			tree _r;
			_r = captures[3];
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 658, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail916:;
		    }
		}
	      else
		{
		  if (cmp == 0
 && ((code1 == LE_EXPR && code2 == GE_EXPR)
 || (code1 == GE_EXPR && code2 == LE_EXPR))
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail917;
		      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail917;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail917;
		      {
			tree res_op0;
			res_op0 = captures[1];
			tree res_op1;
			res_op1 = captures[2];
			tree _r;
			_r = fold_build2_loc (loc, EQ_EXPR, type, res_op0, res_op1);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 659, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail917:;
		    }
		  else
		    {
		      if (cmp <= 0
 && (code1 == LT_EXPR || code1 == LE_EXPR)
 && (code2 == GT_EXPR || code2 == GE_EXPR)
)
			{
			  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail918;
			  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail918;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail918;
			  {
			    tree _r;
			    _r =  constant_boolean_node (false, type);
			    if (TREE_SIDE_EFFECTS (captures[1]))
			      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 660, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail918:;
			}
		      else
			{
			  if (cmp >= 0
 && (code1 == GT_EXPR || code1 == GE_EXPR)
 && (code2 == LT_EXPR || code2 == LE_EXPR)
)
			    {
			      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail919;
			      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail919;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail919;
			      {
				tree _r;
				_r =  constant_boolean_node (false, type);
				if (TREE_SIDE_EFFECTS (captures[1]))
				  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 661, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail919:;
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
generic_simplify_434 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (POPCOUNT),
 const combined_fn ARG_UNUSED (PARITY))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail955;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail955;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree _r;
    _r = maybe_build_call_expr_loc (loc, PARITY, type, 1, res_op0);
    if (!_r)
      goto next_after_fail955;
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 669, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail955:;
  return NULL_TREE;
}

tree
generic_simplify_436 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail957;
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
    _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 671, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail957:;
  return NULL_TREE;
}

tree
generic_simplify_438 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail959;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 = captures[1];
    tree _r;
    _r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 673, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail959:;
  return NULL_TREE;
}

tree
generic_simplify_442 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail963;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[0];
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[1];
	    _r2 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
	    _o1[1] = _r2;
	  }
	  _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree _r;
	_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 677, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail963:;
    }
  return NULL_TREE;
}

tree
generic_simplify_445 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (rotate))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((element_precision (TREE_TYPE (captures[2]))
 <= element_precision (TREE_TYPE (captures[3]))
 || !TYPE_UNSIGNED (TREE_TYPE (captures[3])))
 && (element_precision (type) <= element_precision (TREE_TYPE (captures[2]))
 || !TYPE_UNSIGNED (TREE_TYPE (captures[2])))
)
    {
      {
 tree rotate_type = TREE_TYPE (captures[2]);
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail966;
	  {
	    tree res_op0;
	    {
	      tree _o1[2], _r1;
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[3];
		if (TREE_TYPE (_o2[0]) != rotate_type)
		  {
		    _r2 = fold_build1_loc (loc, NOP_EXPR, rotate_type, _o2[0]);
		  }
		else
		  _r2 = _o2[0];
		_o1[0] = _r2;
	      }
	      _o1[1] = captures[4];
	      _r1 = fold_build2_loc (loc, rotate, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 679, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail966:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_455 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (minmax))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail978;
  {
    tree _r;
    _r = captures[0];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 690, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail978:;
  return NULL_TREE;
}

tree
generic_simplify_462 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
		  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail991;
		  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail991;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail991;
		  {
		    tree _r;
		    _r = captures[0];
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 694, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail991:;
		}
	      else
		{
		  if (known_gt (off0, off1)
)
		    {
		      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail992;
		      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail992;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail992;
		      {
			tree _r;
			_r = captures[2];
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 695, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail992:;
		    }
		}
	    }
	  else
	    {
	      if (known_ge (off0, off1)
)
		{
		  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail993;
		  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail993;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail993;
		  {
		    tree _r;
		    _r = captures[0];
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 696, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail993:;
		}
	      else
		{
		  if (known_lt (off0, off1)
)
		    {
		      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail994;
		      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail994;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail994;
		      {
			tree _r;
			_r = captures[2];
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 697, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail994:;
		    }
		}
	    }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_479 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1019;
  if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1019;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1019;
  {
    tree _r;
    _r = captures[2];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 718, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1019:;
  return NULL_TREE;
}

tree
generic_simplify_483 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
)
    {
      if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1023;
      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1023;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1023;
      {
	tree _r;
	_r = captures[2];
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 722, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1023:;
    }
  return NULL_TREE;
}

tree
generic_simplify_489 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1029;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1029;
  {
    tree _r;
    _r = captures[2];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 728, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1029:;
  return NULL_TREE;
}

tree
generic_simplify_493 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (func))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1034;
  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1034;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1034;
  {
    tree _r;
    _r = captures[2];
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 733, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1034:;
  return NULL_TREE;
}

tree
generic_simplify_499 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_MAX_VALUE (TREE_TYPE (captures[1]))
 && types_match (TREE_TYPE (captures[1]), TREE_TYPE (TREE_TYPE (captures[0])))
 && int_fits_type_p (captures[2], TREE_TYPE (captures[1]))
)
    {
      if (TYPE_UNSIGNED (TREE_TYPE (captures[1]))
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1042;
	  {
	    tree res_op0;
	    {
	      tree _o1[2], _r1;
	      _o1[0] = captures[1];
	      {
		tree _o2[2], _r2;
		_o2[0] =  TYPE_MAX_VALUE (TREE_TYPE (captures[1]));
		_o2[1] = captures[2];
		_r2 = fold_build2_loc (loc, TRUNC_DIV_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
		if (EXPR_P (_r2))
		  goto next_after_fail1042;
		_o1[1] = _r2;
	      }
	      _r1 = fold_build2_loc (loc, GT_EXPR, boolean_type_node, _o1[0], _o1[1]);
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 740, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1042:;
	}
      else
	{
	  if (TYPE_MIN_VALUE (TREE_TYPE (captures[1]))
)
	    {
	      if (integer_minus_onep (captures[2])
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1043;
		  {
		    tree res_op0;
		    {
		      tree _o1[2], _r1;
		      _o1[0] = captures[1];
		      _o1[1] =  TYPE_MIN_VALUE (TREE_TYPE (captures[1]));
		      _r1 = fold_build2_loc (loc, EQ_EXPR, boolean_type_node, _o1[0], _o1[1]);
		      res_op0 = _r1;
		    }
		    tree _r;
		    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		    if (TREE_SIDE_EFFECTS (captures[2]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 741, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1043:;
		}
	      else
		{
		  {
 tree div = fold_convert (TREE_TYPE (captures[1]), captures[2]);
 tree lo = int_const_binop (TRUNC_DIV_EXPR,
 TYPE_MIN_VALUE (TREE_TYPE (captures[1])), div);
 tree hi = int_const_binop (TRUNC_DIV_EXPR,
 TYPE_MAX_VALUE (TREE_TYPE (captures[1])), div);
 tree etype = range_check_type (TREE_TYPE (captures[1]));
 if (etype)
 {
 if (wi::neg_p (wi::to_wide (div)))
 std::swap (lo, hi);
 lo = fold_convert (etype, lo);
 hi = fold_convert (etype, hi);
 hi = int_const_binop (MINUS_EXPR, hi, lo);
 }
		      if (etype
)
			{
			  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1044;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1044;
			  {
			    tree res_op0;
			    {
			      tree _o1[2], _r1;
			      {
				tree _o2[2], _r2;
				{
				  tree _o3[1], _r3;
				  _o3[0] = captures[1];
				  if (TREE_TYPE (_o3[0]) != etype)
				    {
				      _r3 = fold_build1_loc (loc, NOP_EXPR, etype, _o3[0]);
				    }
				  else
				    _r3 = _o3[0];
				  _o2[0] = _r3;
				}
				_o2[1] =  lo;
				_r2 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				_o1[0] = _r2;
			      }
			      _o1[1] =  hi;
			      _r1 = fold_build2_loc (loc, GT_EXPR, boolean_type_node, _o1[0], _o1[1]);
			      res_op0 = _r1;
			    }
			    tree _r;
			    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 742, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1044:;
			}
		  }
		}
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_527 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (tans),
 const combined_fn ARG_UNUSED (atans))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1072;
      {
	tree _r;
	_r = captures[0];
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 765, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1072:;
    }
  return NULL_TREE;
}

tree
generic_simplify_531 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (fns))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1076;
  {
    tree _r;
    _r = captures[0];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 769, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1076:;
  return NULL_TREE;
}

tree
generic_simplify_537 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (froms),
 const combined_fn ARG_UNUSED (tos))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (optimize && canonicalize_math_p ()
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1082;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree _r;
	_r = maybe_build_call_expr_loc (loc, tos, type, 1, res_op0);
	if (!_r)
	  goto next_after_fail1082;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 775, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1082:;
    }
  return NULL_TREE;
}

tree
generic_simplify_545 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (SIGNBIT))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!HONOR_SIGNED_ZEROS (captures[0])
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1090;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[0];
	  _o1[1] =  build_real (TREE_TYPE (captures[0]), dconst0);
	  _r1 = fold_build2_loc (loc, LT_EXPR, boolean_type_node, _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree _r;
	_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 783, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1090:;
    }
  return NULL_TREE;
}

tree
generic_simplify_554 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (canonicalize_math_after_vectorization_p ()
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1099;
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
	  goto next_after_fail1099;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 792, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1099:;
    }
  return NULL_TREE;
}

tree
generic_simplify_563 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (pfun))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
)
    {
      {
 wide_int nz = tree_nonzero_bits (captures[0]);
	  if (nz == 1
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1108;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1108;
	      {
		tree res_op0;
		res_op0 = captures[0];
		tree _r;
		_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 796, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1108:;
	    }
	  else
	    {
	      if (wi::popcount (nz) == 1
)
		{
		  {
 tree utype = unsigned_type_for (TREE_TYPE (captures[0]));
		      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1109;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1109;
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
			  _o1[1] =  build_int_cst (integer_type_node,
 wi::ctz (nz));
			  _r1 = fold_build2_loc (loc, RSHIFT_EXPR, utype, _o1[0], _o1[1]);
			  res_op0 = _r1;
			}
			tree _r;
			_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 797, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1109:;
		  }
		}
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_ABSU_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case ABSU_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q30 };
		if (tree_nop_conversion_p (TREE_TYPE (captures[0]), TREE_TYPE (captures[1]))
)
		  {
		    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1155;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1155;
		    {
		      tree _r;
		      _r = captures[1];
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 833, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1155:;
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
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1156;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree _r;
	    _r = fold_build1_loc (loc, ABSU_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 834, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1156:;
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1157;
	  {
	    tree res_op0;
	    res_op0 = captures[1];
	    tree res_op1;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[2];
	      _r1 = fold_build1_loc (loc, ABSU_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	      if (EXPR_P (_r1))
	        goto next_after_fail1157;
	      res_op1 = _r1;
	    }
	    tree res_op2;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[3];
	      _r1 = fold_build1_loc (loc, ABSU_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	      if (EXPR_P (_r1))
	        goto next_after_fail1157;
	      res_op2 = _r1;
	    }
	    tree _r;
	    _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 808, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1157:;
	}
        break;
      }
    default:;
    }
if (tree_expr_nonnegative_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1158;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree _r;
	_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 835, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1158:;
    }
  }
  return NULL_TREE;
}

tree
generic_simplify_PAREN_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case PAREN_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
	  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1208;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1208;
	  {
	    tree _r;
	    _r = captures[0];
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 852, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1208:;
	}
        break;
      }
    default:;
    }
if (CONSTANT_CLASS_P (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1209;
      {
	tree _r;
	_r = captures[0];
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 853, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1209:;
    }
  }
  return NULL_TREE;
}

tree
generic_simplify_REALPART_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case COMPLEX_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1210;
	  {
	    tree _r;
	    _r = captures[0];
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 854, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1210:;
	}
        break;
      }
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case CONJ_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q30 };
		if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1211;
		{
		  tree res_op0;
		  {
		    tree _o1[1], _r1;
		    _o1[0] = captures[2];
		    _r1 = fold_build1_loc (loc, REALPART_EXPR, TREE_TYPE (TREE_TYPE (_o1[0])), _o1[0]);
		    res_op0 = _r1;
		  }
		  tree _r;
		  _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 855, __FILE__, __LINE__, true);
		  return _r;
		}
next_after_fail1211:;
	      }
	      break;
	    }
	  case PLUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31 };
		const enum tree_code op = PLUS_EXPR;
		if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1212;
		{
		  tree res_op0;
		  {
		    tree _o1[2], _r1;
		    {
		      tree _o2[1], _r2;
		      _o2[0] = captures[2];
		      _r2 = fold_build1_loc (loc, REALPART_EXPR, TREE_TYPE (TREE_TYPE (_o2[0])), _o2[0]);
		      _o1[0] = _r2;
		    }
		    {
		      tree _o2[1], _r2;
		      _o2[0] = captures[3];
		      _r2 = fold_build1_loc (loc, REALPART_EXPR, TREE_TYPE (TREE_TYPE (_o2[0])), _o2[0]);
		      _o1[1] = _r2;
		    }
		    _r1 = fold_build2_loc (loc, op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		    res_op0 = _r1;
		  }
		  tree _r;
		  _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 856, __FILE__, __LINE__, true);
		  return _r;
		}
next_after_fail1212:;
	      }
	      break;
	    }
	  case MINUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31 };
		const enum tree_code op = MINUS_EXPR;
		if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1213;
		{
		  tree res_op0;
		  {
		    tree _o1[2], _r1;
		    {
		      tree _o2[1], _r2;
		      _o2[0] = captures[2];
		      _r2 = fold_build1_loc (loc, REALPART_EXPR, TREE_TYPE (TREE_TYPE (_o2[0])), _o2[0]);
		      _o1[0] = _r2;
		    }
		    {
		      tree _o2[1], _r2;
		      _o2[0] = captures[3];
		      _r2 = fold_build1_loc (loc, REALPART_EXPR, TREE_TYPE (TREE_TYPE (_o2[0])), _o2[0]);
		      _o1[1] = _r2;
		    }
		    _r1 = fold_build2_loc (loc, op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		    res_op0 = _r1;
		  }
		  tree _r;
		  _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 856, __FILE__, __LINE__, true);
		  return _r;
		}
next_after_fail1213:;
	      }
	      break;
	    }
	  case CALL_EXPR:
	    switch (get_call_combined_fn (_q20))
	      {
	      case CFN_BUILT_IN_CEXPIF:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q30 };
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1214;
		      {
			tree res_op0;
			{
			  tree _o1[1], _r1;
			  _o1[0] = captures[2];
			  _r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_COSF, TREE_TYPE (_o1[0]), 1, _o1[0]);
			  if (!_r1)
			    goto next_after_fail1214;
			  res_op0 = _r1;
			}
			tree _r;
			_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 857, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1214:;
		    }
	          }
	        break;
	      case CFN_BUILT_IN_CEXPIL:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q30 };
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1215;
		      {
			tree res_op0;
			{
			  tree _o1[1], _r1;
			  _o1[0] = captures[2];
			  _r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_COSL, TREE_TYPE (_o1[0]), 1, _o1[0]);
			  if (!_r1)
			    goto next_after_fail1215;
			  res_op0 = _r1;
			}
			tree _r;
			_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 857, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1215:;
		    }
	          }
	        break;
	      case CFN_BUILT_IN_CEXPI:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q30 };
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1216;
		      {
			tree res_op0;
			{
			  tree _o1[1], _r1;
			  _o1[0] = captures[2];
			  _r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_COS, TREE_TYPE (_o1[0]), 1, _o1[0]);
			  if (!_r1)
			    goto next_after_fail1216;
			  res_op0 = _r1;
			}
			tree _r;
			_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 857, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1216:;
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
    case CONJ_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _q20 };
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1217;
	  {
	    tree res_op0;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[2];
	      _r1 = fold_build1_loc (loc, REALPART_EXPR, TREE_TYPE (TREE_TYPE (_o1[0])), _o1[0]);
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 855, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1217:;
	}
        break;
      }
    case PLUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21 };
	  const enum tree_code op = PLUS_EXPR;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1218;
	  {
	    tree res_op0;
	    {
	      tree _o1[2], _r1;
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[2];
		_r2 = fold_build1_loc (loc, REALPART_EXPR, TREE_TYPE (TREE_TYPE (_o2[0])), _o2[0]);
		_o1[0] = _r2;
	      }
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[3];
		_r2 = fold_build1_loc (loc, REALPART_EXPR, TREE_TYPE (TREE_TYPE (_o2[0])), _o2[0]);
		_o1[1] = _r2;
	      }
	      _r1 = fold_build2_loc (loc, op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 856, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1218:;
	}
        break;
      }
    case MINUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21 };
	  const enum tree_code op = MINUS_EXPR;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1219;
	  {
	    tree res_op0;
	    {
	      tree _o1[2], _r1;
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[2];
		_r2 = fold_build1_loc (loc, REALPART_EXPR, TREE_TYPE (TREE_TYPE (_o2[0])), _o2[0]);
		_o1[0] = _r2;
	      }
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[3];
		_r2 = fold_build1_loc (loc, REALPART_EXPR, TREE_TYPE (TREE_TYPE (_o2[0])), _o2[0]);
		_o1[1] = _r2;
	      }
	      _r1 = fold_build2_loc (loc, op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 856, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1219:;
	}
        break;
      }
    case CALL_EXPR:
      switch (get_call_combined_fn (_p0))
        {
	case CFN_BUILT_IN_CEXPIF:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _q20 };
		if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1220;
		{
		  tree res_op0;
		  {
		    tree _o1[1], _r1;
		    _o1[0] = captures[2];
		    _r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_COSF, TREE_TYPE (_o1[0]), 1, _o1[0]);
		    if (!_r1)
		      goto next_after_fail1220;
		    res_op0 = _r1;
		  }
		  tree _r;
		  _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 857, __FILE__, __LINE__, true);
		  return _r;
		}
next_after_fail1220:;
	      }
	    }
	  break;
	case CFN_BUILT_IN_CEXPIL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _q20 };
		if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1221;
		{
		  tree res_op0;
		  {
		    tree _o1[1], _r1;
		    _o1[0] = captures[2];
		    _r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_COSL, TREE_TYPE (_o1[0]), 1, _o1[0]);
		    if (!_r1)
		      goto next_after_fail1221;
		    res_op0 = _r1;
		  }
		  tree _r;
		  _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 857, __FILE__, __LINE__, true);
		  return _r;
		}
next_after_fail1221:;
	      }
	    }
	  break;
	case CFN_BUILT_IN_CEXPI:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p0, _q20 };
		if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1222;
		{
		  tree res_op0;
		  {
		    tree _o1[1], _r1;
		    _o1[0] = captures[2];
		    _r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_COS, TREE_TYPE (_o1[0]), 1, _o1[0]);
		    if (!_r1)
		      goto next_after_fail1222;
		    res_op0 = _r1;
		  }
		  tree _r;
		  _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 857, __FILE__, __LINE__, true);
		  return _r;
		}
next_after_fail1222:;
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
generic_simplify_BIT_XOR_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1325;
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1325;
	{
	  tree res_op0;
	  res_op0 = captures[0];
	  tree _r;
	  _r = non_lvalue_loc (loc, res_op0);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 862, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1325:;
      }
    }
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
		    tree res = generic_simplify_63 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
		    tree res = generic_simplify_64 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case BIT_AND_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q51 };
		tree res = generic_simplify_6 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		if (res) return res;
	      }
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q50 };
		tree res = generic_simplify_6 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		if (res) return res;
	      }
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q51 };
		tree res = generic_simplify_6 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		if (res) return res;
	      }
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q50 };
		tree res = generic_simplify_6 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
	      switch (TREE_CODE (_p1))
	        {
		case BIT_NOT_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q40 };
			  tree res = generic_simplify_145 (loc, type, _p0, _p1, captures);
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
	  case BIT_NOT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_NOT_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q30 };
			  tree res = generic_simplify_145 (loc, type, _p0, _p1, captures);
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
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q21 };
		    tree res = generic_simplify_146 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
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
	      if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q40, _q20 };
		    tree res = generic_simplify_146 (loc, type, _p0, _p1, captures);
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
	switch (TREE_CODE (_p1))
	  {
	  case BIT_AND_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      tree _q41 = TREE_OPERAND (_p1, 1);
	      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
		{
		  switch (TREE_CODE (_q41))
		    {
		    case BIT_NOT_EXPR:
		      {
			tree _q60 = TREE_OPERAND (_q41, 0);
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q60 };
			  tree res = generic_simplify_145 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		        break;
		      }
		    default:;
		    }
	        }
	      switch (TREE_CODE (_q40))
	        {
		case BIT_NOT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    if ((_q41 == _q20 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _q20, 0) && types_match (_q41, _q20)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q50 };
			  tree res = generic_simplify_145 (loc, type, _p0, _p1, captures);
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
    case BIT_AND_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	switch (TREE_CODE (_q30))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p1, _p0, _q31 };
		    tree res = generic_simplify_146 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
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
	      if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p1, _p0, _q30 };
		    tree res = generic_simplify_146 (loc, type, _p0, _p1, captures);
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
	switch (TREE_CODE (_q20))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      {
		tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _p1 };
		tree res = generic_simplify_147 (loc, type, _p0, _p1, captures);
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
		tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q40, _q20, _p1 };
		tree res = generic_simplify_147 (loc, type, _p0, _p1, captures);
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
    case BIT_IOR_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	switch (TREE_CODE (_q30))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      {
		tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q30, _q40, _q31, _p0 };
		tree res = generic_simplify_147 (loc, type, _p0, _p1, captures);
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
		tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q31, _q50, _q30, _p0 };
		tree res = generic_simplify_147 (loc, type, _p0, _p1, captures);
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
		tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _p1 };
		tree res = generic_simplify_148 (loc, type, _p0, _p1, captures);
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
		tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q40, _q20, _p1 };
		tree res = generic_simplify_148 (loc, type, _p0, _p1, captures);
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
    case BIT_AND_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	switch (TREE_CODE (_q30))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      {
		tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q30, _q40, _q31, _p0 };
		tree res = generic_simplify_148 (loc, type, _p0, _p1, captures);
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
		tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q31, _q50, _q30, _p0 };
		tree res = generic_simplify_148 (loc, type, _p0, _p1, captures);
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
		      tree res = generic_simplify_82 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
		      tree res = generic_simplify_82 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
	  tree res = generic_simplify_82 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
	  tree res = generic_simplify_82 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
	  if (res) return res;
	}
      }
  }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1326;
	{
	  tree _r;
	  _r =  build_zero_cst (type);
	  if (TREE_SIDE_EFFECTS (captures[0]))
	    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 912, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1326:;
      }
    }
  if (integer_all_onesp (_p1))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1327;
	{
	  tree res_op0;
	  res_op0 = captures[0];
	  tree _r;
	  _r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
	  if (TREE_SIDE_EFFECTS (captures[1]))
	    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 913, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1327:;
      }
    }
  switch (TREE_CODE (_p0))
    {
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
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  switch (TREE_CODE (_q51))
		    {
		    case BIT_NOT_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_q51, 0);
			if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			      tree res = generic_simplify_149 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
	        }
	      switch (TREE_CODE (_q50))
	        {
		case BIT_NOT_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			      tree res = generic_simplify_149 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			      tree res = generic_simplify_149 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    break;
		  }
	        default:;
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  switch (TREE_CODE (_q51))
		    {
		    case BIT_NOT_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_q51, 0);
			if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			      tree res = generic_simplify_149 (loc, type, _p0, _p1, captures);
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
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
			      tree res = generic_simplify_149 (loc, type, _p0, _p1, captures);
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
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q30 };
			      tree res = generic_simplify_149 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q30 };
			      tree res = generic_simplify_149 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
			switch (TREE_CODE (_q61))
			  {
			  case BIT_NOT_EXPR:
			    {
			      tree _q80 = TREE_OPERAND (_q61, 0);
			      if ((_q80 == _q21 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q21, 0) && types_match (_q80, _q21)))
				{
				  {
				    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q21 };
				    tree res = generic_simplify_150 (loc, type, _p0, _p1, captures);
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
				    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q21 };
				    tree res = generic_simplify_150 (loc, type, _p0, _p1, captures);
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
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q30, _q21, _p1 };
			      tree res = generic_simplify_151 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    break;
		  }
	        default:;
	        }
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q21, _p1 };
		tree res = generic_simplify_152 (loc, type, _p0, _p1, captures);
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
	      switch (TREE_CODE (_p1))
	        {
		case BIT_IOR_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
		      {
			if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
			      tree res = generic_simplify_149 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    break;
		  }
		case BIT_XOR_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
		      {
			if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q40, _q20, _p1 };
			      tree res = generic_simplify_151 (loc, type, _p0, _p1, captures);
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
	  case BIT_AND_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			tree res = generic_simplify_153 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	      break;
	    }
	  case BIT_XOR_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			tree res = generic_simplify_154 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
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
				    tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
				    tree res = generic_simplify_150 (loc, type, _p0, _p1, captures);
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
				    tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
				    tree res = generic_simplify_150 (loc, type, _p0, _p1, captures);
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
	switch (TREE_CODE (_p1))
	  {
	  CASE_CONVERT:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q20, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q20 };
		    tree res = generic_simplify_155 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q21, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q21 };
		    tree res = generic_simplify_155 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q20, 0))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q20 };
	      tree res = generic_simplify_155 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q21, 0))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q21 };
	      tree res = generic_simplify_155 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	{
	  tree _q20_pops[1];
	  if (tree_nop_convert (_q20, _q20_pops))
	    {
	      tree _q30 = _q20_pops[0];
	      switch (TREE_CODE (_q30))
	        {
		case BIT_NOT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q21, _p1 };
		      tree res = generic_simplify_152 (loc, type, _p0, _p1, captures);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	}
	switch (TREE_CODE (_q21))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q20, _p1 };
		tree res = generic_simplify_152 (loc, type, _p0, _p1, captures);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
      {
	tree _q21_pops[1];
	if (tree_nop_convert (_q21, _q21_pops))
	  {
	    tree _q40 = _q21_pops[0];
	    switch (TREE_CODE (_q40))
	      {
	      case BIT_NOT_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_q40, 0);
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q20, _p1 };
		    tree res = generic_simplify_152 (loc, type, _p0, _p1, captures);
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
	      switch (TREE_CODE (_q50))
	        {
		case BIT_NOT_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q20, _q21, _p0 };
			      tree res = generic_simplify_151 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    break;
		  }
	        default:;
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  switch (TREE_CODE (_q51))
		    {
		    case BIT_NOT_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_q51, 0);
			if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q20, _q21, _p0 };
			      tree res = generic_simplify_151 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			tree res = generic_simplify_154 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	      break;
	    }
	  case BIT_AND_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			tree res = generic_simplify_8 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
			if (res) return res;
		      }
		    }
	        }
	      break;
	    }
	  CASE_CONVERT:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q21, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q21 };
		    tree res = generic_simplify_156 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q20, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q20 };
		    tree res = generic_simplify_156 (loc, type, _p0, _p1, captures);
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
	      tree res = generic_simplify_156 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q20, 0))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q20 };
	      tree res = generic_simplify_156 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
        break;
      }
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
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			tree res = generic_simplify_153 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	      break;
	    }
	  case BIT_XOR_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			tree res = generic_simplify_8 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
			if (res) return res;
		      }
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
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    switch (TREE_CODE (_q50))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q60 = TREE_OPERAND (_q50, 0);
			  tree _q61 = TREE_OPERAND (_q50, 1);
			  switch (TREE_CODE (_q61))
			    {
			    case INTEGER_CST:
			      {
				{
				  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q50, _q60, _q61 };
				  tree res = generic_simplify_10 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
		case BIT_AND_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    tree _q51 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q51))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50, _q51 };
			    tree res = generic_simplify_10 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
		      CASE_CONVERT:
		        {
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  switch (TREE_CODE (_q60))
			    {
			    case BIT_AND_EXPR:
			      {
				tree _q70 = TREE_OPERAND (_q60, 0);
				tree _q71 = TREE_OPERAND (_q60, 1);
				switch (TREE_CODE (_q71))
				  {
				  case INTEGER_CST:
				    {
				      {
					tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q60, _q70, _q71 };
					tree res = generic_simplify_10 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
		      case BIT_AND_EXPR:
		        {
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  tree _q61 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q61))
			    {
			    case INTEGER_CST:
			      {
				{
				  tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _p1, _q60, _q61 };
				  tree res = generic_simplify_10 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
	  case BIT_IOR_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || operand_equal_p (_q60, _q30, 0))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q30 };
			  tree res = generic_simplify_155 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q31 && ! TREE_SIDE_EFFECTS (_q60)) || operand_equal_p (_q60, _q31, 0))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q31, _q30, _q31 };
			  tree res = generic_simplify_155 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q30, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q30 };
		    tree res = generic_simplify_155 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q31, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q31, _q30, _q31 };
		    tree res = generic_simplify_155 (loc, type, _p0, _p1, captures);
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
		case BIT_IOR_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q20, 0))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q51, _q20 };
			  tree res = generic_simplify_155 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || operand_equal_p (_q51, _q20, 0))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q51, _q50, _q20 };
			  tree res = generic_simplify_155 (loc, type, _p0, _p1, captures);
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
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      tree _q41 = TREE_OPERAND (_p1, 1);
	      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || operand_equal_p (_q40, _q20, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _q20 };
		    tree res = generic_simplify_155 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q41 == _q20 && ! TREE_SIDE_EFFECTS (_q41)) || operand_equal_p (_q41, _q20, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _q20 };
		    tree res = generic_simplify_155 (loc, type, _p0, _p1, captures);
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
	  case BIT_IOR_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || operand_equal_p (_q40, _p0, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _p0 };
		    tree res = generic_simplify_155 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || operand_equal_p (_q41, _p0, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _p0 };
		    tree res = generic_simplify_155 (loc, type, _p0, _p1, captures);
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
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || operand_equal_p (_q30, _p0, 0))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p0 };
	      tree res = generic_simplify_155 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || operand_equal_p (_q31, _p0, 0))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _q31, _q30, _p0 };
	      tree res = generic_simplify_155 (loc, type, _p0, _p1, captures);
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
	  case BIT_IOR_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree _q30_pops[1];
		if (tree_nop_convert (_q30, _q30_pops))
		  {
		    tree _q40 = _q30_pops[0];
		    switch (TREE_CODE (_q40))
		      {
		      case BIT_NOT_EXPR:
		        {
			  tree _q50 = TREE_OPERAND (_q40, 0);
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q31, _p1 };
			    tree res = generic_simplify_152 (loc, type, _p0, _p1, captures);
			    if (res) return res;
			  }
		          break;
		        }
		      default:;
		      }
		  }
	      }
	      {
		tree _q31_pops[1];
		if (tree_nop_convert (_q31, _q31_pops))
		  {
		    tree _q50 = _q31_pops[0];
		    switch (TREE_CODE (_q50))
		      {
		      case BIT_NOT_EXPR:
		        {
			  tree _q60 = TREE_OPERAND (_q50, 0);
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q60, _q30, _p1 };
			    tree res = generic_simplify_152 (loc, type, _p0, _p1, captures);
			    if (res) return res;
			  }
		          break;
		        }
		      default:;
		      }
		  }
	      }
	      switch (TREE_CODE (_q30))
	        {
		case BIT_NOT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q31, _p1 };
		      tree res = generic_simplify_152 (loc, type, _p0, _p1, captures);
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
		      tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q30, _p1 };
		      tree res = generic_simplify_152 (loc, type, _p0, _p1, captures);
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
  }
  switch (TREE_CODE (_p1))
    {
    case BIT_IOR_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	{
	  tree _q30_pops[1];
	  if (tree_nop_convert (_q30, _q30_pops))
	    {
	      tree _q40 = _q30_pops[0];
	      switch (TREE_CODE (_q40))
	        {
		case BIT_NOT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q31, _p0 };
		      tree res = generic_simplify_152 (loc, type, _p0, _p1, captures);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	}
	{
	  tree _q31_pops[1];
	  if (tree_nop_convert (_q31, _q31_pops))
	    {
	      tree _q50 = _q31_pops[0];
	      switch (TREE_CODE (_q50))
	        {
		case BIT_NOT_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q60, _q30, _p0 };
		      tree res = generic_simplify_152 (loc, type, _p0, _p1, captures);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	}
	switch (TREE_CODE (_q30))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q31, _p0 };
		tree res = generic_simplify_152 (loc, type, _p0, _p1, captures);
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
		tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q30, _p0 };
		tree res = generic_simplify_152 (loc, type, _p0, _p1, captures);
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
	    {
	      tree _q40_pops[1];
	      if (tree_nop_convert (_q40, _q40_pops))
	        {
		  tree _q50 = _q40_pops[0];
		  switch (TREE_CODE (_q50))
		    {
		    case BIT_NOT_EXPR:
		      {
			tree _q60 = TREE_OPERAND (_q50, 0);
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q60, _q41, _p0 };
			  tree res = generic_simplify_152 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		        break;
		      }
		    default:;
		    }
	        }
	    }
	    {
	      tree _q41_pops[1];
	      if (tree_nop_convert (_q41, _q41_pops))
	        {
		  tree _q60 = _q41_pops[0];
		  switch (TREE_CODE (_q60))
		    {
		    case BIT_NOT_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_q60, 0);
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q70, _q40, _p0 };
			  tree res = generic_simplify_152 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		        break;
		      }
		    default:;
		    }
	        }
	    }
	    switch (TREE_CODE (_q40))
	      {
	      case BIT_NOT_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_q40, 0);
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q41, _p0 };
		    tree res = generic_simplify_152 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	          break;
	        }
	      default:;
	      }
	    switch (TREE_CODE (_q41))
	      {
	      case BIT_NOT_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_q41, 0);
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q60, _q40, _p0 };
		    tree res = generic_simplify_152 (loc, type, _p0, _p1, captures);
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
}
  switch (TREE_CODE (_p0))
    {
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
			    tree res = generic_simplify_157 (loc, type, _p0, _p1, captures);
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
		      tree res = generic_simplify_157 (loc, type, _p0, _p1, captures);
		      if (res) return res;
		    }
		    break;
		  }
		case INTEGER_CST:
		  {
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _p1 };
		      tree res = generic_simplify_158 (loc, type, _p0, _p1, captures);
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
    case BIT_NOT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
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
		      tree res = generic_simplify_157 (loc, type, _p0, _p1, captures);
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
		tree res = generic_simplify_157 (loc, type, _p0, _p1, captures);
		if (res) return res;
	      }
	      break;
	    }
	  case INTEGER_CST:
	    {
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		tree res = generic_simplify_158 (loc, type, _p0, _p1, captures);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _p1 };
	  tree res = generic_simplify_159 (loc, type, _p0, _p1, captures);
	  if (res) return res;
	}
        break;
      }
    case BIT_AND_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_160 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, BIT_AND_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
	      tree res = generic_simplify_160 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, BIT_AND_EXPR);
	      if (res) return res;
	    }
	  }
	switch (TREE_CODE (_p1))
	  {
	  case BIT_AND_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q20, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q20 };
		    tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || operand_equal_p (_q51, _q20, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q20 };
		    tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q21, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q21 };
		    tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || operand_equal_p (_q51, _q21, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q21 };
		    tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, BIT_AND_EXPR);
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
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q30, _p0 };
	      tree res = generic_simplify_160 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, BIT_AND_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q31, _p0 };
	      tree res = generic_simplify_160 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, BIT_AND_EXPR);
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
			  if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || operand_equal_p (_q70, _q30, 0))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q71, _q30 };
				tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, BIT_AND_EXPR);
				if (res) return res;
			      }
			    }
			  if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || operand_equal_p (_q71, _q30, 0))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q70, _q30 };
				tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, BIT_AND_EXPR);
				if (res) return res;
			      }
			    }
			  if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || operand_equal_p (_q70, _q31, 0))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q71, _q31 };
				tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, BIT_AND_EXPR);
				if (res) return res;
			      }
			    }
			  if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || operand_equal_p (_q71, _q31, 0))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q70, _q31 };
				tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, BIT_AND_EXPR);
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
	  case BIT_XOR_EXPR:
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
			  tree res = generic_simplify_156 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || operand_equal_p (_q60, _q30, 0))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q31, _q30, _q30 };
			  tree res = generic_simplify_156 (loc, type, _p0, _p1, captures);
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
		    tree res = generic_simplify_156 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q30, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q31, _q30, _q30 };
		    tree res = generic_simplify_156 (loc, type, _p0, _p1, captures);
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
		case BIT_XOR_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || operand_equal_p (_q51, _q20, 0))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q51, _q20 };
			  tree res = generic_simplify_156 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q20, 0))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q51, _q50, _q20 };
			  tree res = generic_simplify_156 (loc, type, _p0, _p1, captures);
			  if (res) return res;
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
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      tree _q41 = TREE_OPERAND (_p1, 1);
	      if ((_q41 == _q20 && ! TREE_SIDE_EFFECTS (_q41)) || operand_equal_p (_q41, _q20, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _q20 };
		    tree res = generic_simplify_156 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || operand_equal_p (_q40, _q20, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _q20 };
		    tree res = generic_simplify_156 (loc, type, _p0, _p1, captures);
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
	  case BIT_XOR_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || operand_equal_p (_q41, _p0, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _p0 };
		    tree res = generic_simplify_156 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || operand_equal_p (_q40, _p0, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _p0 };
		    tree res = generic_simplify_156 (loc, type, _p0, _p1, captures);
		    if (res) return res;
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
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || operand_equal_p (_q31, _p0, 0))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p0 };
	      tree res = generic_simplify_156 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || operand_equal_p (_q30, _p0, 0))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _q31, _q30, _p0 };
	      tree res = generic_simplify_156 (loc, type, _p0, _p1, captures);
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
	      switch (TREE_CODE (_p1))
	        {
		CASE_CONVERT:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    switch (TREE_CODE (_q60))
		      {
		      case BIT_XOR_EXPR:
		        {
			  tree _q70 = TREE_OPERAND (_q60, 0);
			  tree _q71 = TREE_OPERAND (_q60, 1);
			  if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q71 };
				tree res = generic_simplify_161 (loc, type, _p0, _p1, captures);
				if (res) return res;
			      }
			    }
			  if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q70 };
				tree res = generic_simplify_161 (loc, type, _p0, _p1, captures);
				if (res) return res;
			      }
			    }
			  if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q31, _q30, _q71 };
				tree res = generic_simplify_161 (loc, type, _p0, _p1, captures);
				if (res) return res;
			      }
			    }
			  if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q31, _q30, _q70 };
				tree res = generic_simplify_161 (loc, type, _p0, _p1, captures);
				if (res) return res;
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
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q61 };
			  tree res = generic_simplify_161 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q60 };
			  tree res = generic_simplify_161 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q31 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q31, 0) && types_match (_q60, _q31)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q31, _q30, _q61 };
			  tree res = generic_simplify_161 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q31 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q31, 0) && types_match (_q61, _q31)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q31, _q30, _q60 };
			  tree res = generic_simplify_161 (loc, type, _p0, _p1, captures);
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
		tree res = generic_simplify_97 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _p1 };
	  tree res = generic_simplify_98 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
	  if (res) return res;
	}
        break;
      }
    case BIT_XOR_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  CASE_CONVERT:
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
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q61 };
			  tree res = generic_simplify_161 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q60 };
			  tree res = generic_simplify_161 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q61 };
			  tree res = generic_simplify_161 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q60 };
			  tree res = generic_simplify_161 (loc, type, _p0, _p1, captures);
			  if (res) return res;
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
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
		    tree res = generic_simplify_161 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
		    tree res = generic_simplify_161 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
		    tree res = generic_simplify_161 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
		    tree res = generic_simplify_161 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	if (CONSTANT_CLASS_P (_q21))
	  {
	    if (CONSTANT_CLASS_P (_p1))
	      {
		{
		  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		  tree res = generic_simplify_113 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		  if (res) return res;
		}
	      }
	  }
        break;
      }
    case COND_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	tree _q22 = TREE_OPERAND (_p0, 2);
	if (integer_zerop (_q22))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case COND_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p1, 0);
		  tree _q61 = TREE_OPERAND (_p1, 1);
		  tree _q62 = TREE_OPERAND (_p1, 2);
		  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		    {
		      if (integer_zerop (_q61))
			{
			  {
			    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q62 };
			    const enum tree_code op = BIT_XOR_EXPR;
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1328;
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1328;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1328;
			    {
			      tree res_op0;
			      res_op0 = captures[1];
			      tree res_op1;
			      res_op1 = captures[2];
			      tree res_op2;
			      res_op2 = captures[4];
			      tree _r;
			      _r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 864, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1328:;
			  }
		        }
		    }
	          break;
	        }
	      default:;
	      }
	  }
	if (integer_zerop (_q21))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case COND_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p1, 0);
		  tree _q61 = TREE_OPERAND (_p1, 1);
		  tree _q62 = TREE_OPERAND (_p1, 2);
		  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		    {
		      if (integer_zerop (_q62))
			{
			  {
			    tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q20, _q61, _p0, _q22 };
			    const enum tree_code op = BIT_XOR_EXPR;
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1329;
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1329;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1329;
			    {
			      tree res_op0;
			      res_op0 = captures[1];
			      tree res_op1;
			      res_op1 = captures[2];
			      tree res_op2;
			      res_op2 = captures[4];
			      tree _r;
			      _r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 864, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1329:;
			  }
		        }
		    }
	          break;
	        }
	      default:;
	      }
	  }
	switch (TREE_CODE (_q20))
	  {
	  case LT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case GE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (zerop (_q82))
					{
					  {
					    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q80, _q81 };
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, LT_EXPR, GE_EXPR);
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
	      break;
	    }
	  case LE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case GT_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (zerop (_q82))
					{
					  {
					    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q80, _q81 };
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, LE_EXPR, GT_EXPR);
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
	      break;
	    }
	  case EQ_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case NE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (zerop (_q82))
					{
					  {
					    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q80, _q81 };
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, EQ_EXPR, NE_EXPR);
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
	      break;
	    }
	  case NE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case EQ_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (zerop (_q82))
					{
					  {
					    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q80, _q81 };
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, NE_EXPR, EQ_EXPR);
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
	      break;
	    }
	  case GE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case LT_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (zerop (_q82))
					{
					  {
					    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q80, _q81 };
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, GE_EXPR, LT_EXPR);
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
	      break;
	    }
	  case GT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case LE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (zerop (_q82))
					{
					  {
					    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q80, _q81 };
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, GT_EXPR, LE_EXPR);
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
	      break;
	    }
	  case UNORDERED_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case ORDERED_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (zerop (_q82))
					{
					  {
					    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q80, _q81 };
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, UNORDERED_EXPR, ORDERED_EXPR);
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
	      break;
	    }
	  case ORDERED_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case UNORDERED_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (zerop (_q82))
					{
					  {
					    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q80, _q81 };
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, ORDERED_EXPR, UNORDERED_EXPR);
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
	      break;
	    }
	  case UNLT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case GE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (zerop (_q82))
					{
					  {
					    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q80, _q81 };
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, UNLT_EXPR, GE_EXPR);
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
	      break;
	    }
	  case UNLE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case GT_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (zerop (_q82))
					{
					  {
					    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q80, _q81 };
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, UNLE_EXPR, GT_EXPR);
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
	      break;
	    }
	  case UNGT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case LE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (zerop (_q82))
					{
					  {
					    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q80, _q81 };
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, UNGT_EXPR, LE_EXPR);
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
	      break;
	    }
	  case UNGE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case LT_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (zerop (_q82))
					{
					  {
					    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q80, _q81 };
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, UNGE_EXPR, LT_EXPR);
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
	      break;
	    }
	  case UNEQ_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case LTGT_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (zerop (_q82))
					{
					  {
					    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q80, _q81 };
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, UNEQ_EXPR, LTGT_EXPR);
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
	      break;
	    }
	  case LTGT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case UNEQ_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (zerop (_q82))
					{
					  {
					    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q80, _q81 };
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, LTGT_EXPR, UNEQ_EXPR);
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
	if (integer_zerop (_q22))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case VEC_COND_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p1, 0);
		  tree _q61 = TREE_OPERAND (_p1, 1);
		  tree _q62 = TREE_OPERAND (_p1, 2);
		  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		    {
		      if (integer_zerop (_q61))
			{
			  {
			    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q62 };
			    const enum tree_code op = BIT_XOR_EXPR;
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1330;
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1330;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1330;
			    {
			      tree res_op0;
			      res_op0 = captures[1];
			      tree res_op1;
			      res_op1 = captures[2];
			      tree res_op2;
			      res_op2 = captures[4];
			      tree _r;
			      _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 864, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1330:;
			  }
		        }
		    }
	          break;
	        }
	      default:;
	      }
	  }
	if (integer_zerop (_q21))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case VEC_COND_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p1, 0);
		  tree _q61 = TREE_OPERAND (_p1, 1);
		  tree _q62 = TREE_OPERAND (_p1, 2);
		  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		    {
		      if (integer_zerop (_q62))
			{
			  {
			    tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q20, _q61, _p0, _q22 };
			    const enum tree_code op = BIT_XOR_EXPR;
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1331;
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1331;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1331;
			    {
			      tree res_op0;
			      res_op0 = captures[1];
			      tree res_op1;
			      res_op1 = captures[2];
			      tree res_op2;
			      res_op2 = captures[4];
			      tree _r;
			      _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 864, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1331:;
			  }
		        }
		    }
	          break;
	        }
	      default:;
	      }
	  }
	switch (TREE_CODE (_q20))
	  {
	  case LT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case VEC_COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case GE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (integer_zerop (_q82))
					{
					  {
					    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1, _q80, _q81 };
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, LT_EXPR, GE_EXPR);
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
	      break;
	    }
	  case LE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case VEC_COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case GT_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (integer_zerop (_q82))
					{
					  {
					    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1, _q80, _q81 };
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, LE_EXPR, GT_EXPR);
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
	      break;
	    }
	  case EQ_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case VEC_COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case NE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (integer_zerop (_q82))
					{
					  {
					    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1, _q80, _q81 };
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, EQ_EXPR, NE_EXPR);
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
	      break;
	    }
	  case NE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case VEC_COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case EQ_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (integer_zerop (_q82))
					{
					  {
					    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1, _q80, _q81 };
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, NE_EXPR, EQ_EXPR);
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
	      break;
	    }
	  case GE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case VEC_COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case LT_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (integer_zerop (_q82))
					{
					  {
					    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1, _q80, _q81 };
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, GE_EXPR, LT_EXPR);
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
	      break;
	    }
	  case GT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case VEC_COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case LE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (integer_zerop (_q82))
					{
					  {
					    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1, _q80, _q81 };
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, GT_EXPR, LE_EXPR);
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
	      break;
	    }
	  case UNORDERED_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case VEC_COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case ORDERED_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (integer_zerop (_q82))
					{
					  {
					    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1, _q80, _q81 };
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, UNORDERED_EXPR, ORDERED_EXPR);
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
	      break;
	    }
	  case ORDERED_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case VEC_COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case UNORDERED_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (integer_zerop (_q82))
					{
					  {
					    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1, _q80, _q81 };
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, ORDERED_EXPR, UNORDERED_EXPR);
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
	      break;
	    }
	  case UNLT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case VEC_COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case GE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (integer_zerop (_q82))
					{
					  {
					    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1, _q80, _q81 };
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, UNLT_EXPR, GE_EXPR);
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
	      break;
	    }
	  case UNLE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case VEC_COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case GT_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (integer_zerop (_q82))
					{
					  {
					    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1, _q80, _q81 };
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, UNLE_EXPR, GT_EXPR);
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
	      break;
	    }
	  case UNGT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case VEC_COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case LE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (integer_zerop (_q82))
					{
					  {
					    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1, _q80, _q81 };
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, UNGT_EXPR, LE_EXPR);
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
	      break;
	    }
	  case UNGE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case VEC_COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case LT_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (integer_zerop (_q82))
					{
					  {
					    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1, _q80, _q81 };
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, UNGE_EXPR, LT_EXPR);
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
	      break;
	    }
	  case UNEQ_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case VEC_COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case LTGT_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (integer_zerop (_q82))
					{
					  {
					    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1, _q80, _q81 };
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, UNEQ_EXPR, LTGT_EXPR);
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
	      break;
	    }
	  case LTGT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_zerop (_q22))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case VEC_COND_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p1, 0);
			tree _q81 = TREE_OPERAND (_p1, 1);
			tree _q82 = TREE_OPERAND (_p1, 2);
			switch (TREE_CODE (_q80))
			  {
			  case UNEQ_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_q80, 0);
			      tree _q91 = TREE_OPERAND (_q80, 1);
			      if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				{
				  if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				    {
				      if (integer_zerop (_q82))
					{
					  {
					    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1, _q80, _q81 };
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, LTGT_EXPR, UNEQ_EXPR);
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
	if (tree_truth_valued_p (_q20))
	  {
	    if (tree_truth_valued_p (_q21))
	      {
		switch (TREE_CODE (_p1))
		  {
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
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1332;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[0];
				    _o1[1] = captures[1];
				    _r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 914, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1332:;
			      }
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1333;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[0];
				    _o1[1] = captures[1];
				    _r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 914, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1333:;
			      }
			    }
		        }
		      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
			{
			  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1334;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[0];
				    _o1[1] = captures[1];
				    _r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 914, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1334:;
			      }
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1335;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[0];
				    _o1[1] = captures[1];
				    _r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 914, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1335:;
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
    case EQ_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case BIT_AND_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if (tree_truth_valued_p (_q50))
		    {
		      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			{
			  if (tree_truth_valued_p (_q51))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1336;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[0];
				    _o1[1] = captures[1];
				    _r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 914, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1336:;
			      }
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1337;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[0];
				    _o1[1] = captures[1];
				    _r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 914, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1337:;
			      }
			    }
		        }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if (tree_truth_valued_p (_q50))
		    {
		      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
			{
			  if (tree_truth_valued_p (_q51))
			    {
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1338;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[0];
				    _o1[1] = captures[1];
				    _r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 914, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1338:;
			      }
			      {
				tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1339;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[0];
				    _o1[1] = captures[1];
				    _r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 914, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1339:;
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
		tree res = generic_simplify_114 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
	case BIT_NOT_EXPR:
	  {
	    tree _q30 = TREE_OPERAND (_q20, 0);
	    {
	      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _p1 };
	      tree res = generic_simplify_162 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	    break;
	  }
	case MULT_EXPR:
	  {
	    tree _q30 = TREE_OPERAND (_q20, 0);
	    tree _q31 = TREE_OPERAND (_q20, 1);
	    {
	      tree _q30_pops[1];
	      if (tree_nop_convert (_q30, _q30_pops))
	        {
		  tree _q40 = _q30_pops[0];
		  switch (TREE_CODE (_q31))
		    {
		    case INTEGER_CST:
		      {
			switch (TREE_CODE (_p1))
			  {
			  case LSHIFT_EXPR:
			    {
			      tree _q70 = TREE_OPERAND (_p1, 0);
			      tree _q71 = TREE_OPERAND (_p1, 1);
			      if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
				{
				  switch (TREE_CODE (_q71))
				    {
				    case INTEGER_CST:
				      {
					{
					  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q40, _q31, _p1, _q71 };
					  tree res = generic_simplify_111 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
		      {
			tree _p1_pops[1];
			if (tree_nop_convert (_p1, _p1_pops))
			  {
			    tree _q70 = _p1_pops[0];
			    switch (TREE_CODE (_q70))
			      {
			      case MULT_EXPR:
			        {
				  tree _q80 = TREE_OPERAND (_q70, 0);
				  tree _q81 = TREE_OPERAND (_q70, 1);
				  {
				    tree _q80_pops[1];
				    if (tree_nop_convert (_q80, _q80_pops))
				      {
					tree _q90 = _q80_pops[0];
					switch (TREE_CODE (_q81))
					  {
					  case INTEGER_CST:
					    {
					      {
						tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q40, _q31, _p1, _q70, _q90, _q81 };
						tree res = generic_simplify_110 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
			  }
		      }
			if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q40, _q31 };
			      tree res = generic_simplify_112 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
		tree res = generic_simplify_114 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		if (res) return res;
	      }
	    }
        }
    }
  }
  switch (TREE_CODE (_p1))
    {
    case BIT_NOT_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _q30, _p0 };
	  tree res = generic_simplify_159 (loc, type, _p0, _p1, captures);
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
      tree _q30 = _p1_pops[0];
      switch (TREE_CODE (_q30))
        {
	case BIT_NOT_EXPR:
	  {
	    tree _q40 = TREE_OPERAND (_q30, 0);
	    {
	      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q30, _q40, _p0 };
	      tree res = generic_simplify_162 (loc, type, _p0, _p1, captures);
	      if (res) return res;
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
		      tree res = generic_simplify_118 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_XOR_EXPR);
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1340;
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
next_after_fail1340:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1341;
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
next_after_fail1341:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1342;
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
next_after_fail1342:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1343;
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
next_after_fail1343:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1344;
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
next_after_fail1344:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1345;
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
next_after_fail1345:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1346;
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
next_after_fail1346:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1347;
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
next_after_fail1347:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1348;
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
next_after_fail1348:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1349;
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
next_after_fail1349:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1350;
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
next_after_fail1350:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1351;
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
next_after_fail1351:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1352;
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
next_after_fail1352:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1353;
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
next_after_fail1353:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1354;
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
next_after_fail1354:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1355;
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
next_after_fail1355:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1356;
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
next_after_fail1356:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1357;
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
next_after_fail1357:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1358;
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
next_after_fail1358:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1359;
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
next_after_fail1359:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1360;
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
next_after_fail1360:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1361;
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
next_after_fail1361:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1362;
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
next_after_fail1362:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1363;
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
next_after_fail1363:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1364;
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
next_after_fail1364:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1365;
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
next_after_fail1365:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1366;
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
next_after_fail1366:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1367;
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
next_after_fail1367:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1368;
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
next_after_fail1368:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1369;
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
next_after_fail1369:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1370;
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
next_after_fail1370:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1371;
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
next_after_fail1371:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1372;
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
next_after_fail1372:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1373;
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
next_after_fail1373:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1374;
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
next_after_fail1374:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1375;
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
next_after_fail1375:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1376;
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
next_after_fail1376:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1377;
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
next_after_fail1377:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1378;
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
next_after_fail1378:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1379;
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
next_after_fail1379:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1380;
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
next_after_fail1380:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1381;
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
next_after_fail1381:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1382;
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
next_after_fail1382:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1383;
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
next_after_fail1383:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1384;
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
next_after_fail1384:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1385;
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
next_after_fail1385:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1386;
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
next_after_fail1386:;
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1387;
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
next_after_fail1387:;
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
		case MULT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    tree _q51 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q51))
		      {
		      case INTEGER_CST:
		        {
			  {
			    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21, _p1, _p1, _q50, _q51 };
			    tree res = generic_simplify_137 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
			    if (res) return res;
			  }
		          break;
		        }
		      default:;
		      }
		    break;
		  }
		case LSHIFT_EXPR:
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
				tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21, _p1, _q51 };
				tree res = generic_simplify_138 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
	      if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21 };
		    tree res = generic_simplify_139 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
				tree captures[6] ATTRIBUTE_UNUSED = { _p1, _p1, _q20, _q51, _p0, _q21 };
				tree res = generic_simplify_138 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
	    {
	      tree _p1_pops[1];
	      if (tree_nop_convert (_p1, _p1_pops))
	        {
		  tree _q50 = _p1_pops[0];
		  switch (TREE_CODE (_q50))
		    {
		    case MULT_EXPR:
		      {
			tree _q60 = TREE_OPERAND (_q50, 0);
			tree _q61 = TREE_OPERAND (_q50, 1);
			{
			  tree _q60_pops[1];
			  if (tree_nop_convert (_q60, _q60_pops))
			    {
			      tree _q70 = _q60_pops[0];
			      if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
				{
				  switch (TREE_CODE (_q61))
				    {
				    case INTEGER_CST:
				      {
					{
					  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q50, _q20, _q61, _p0, _q21 };
					  tree res = generic_simplify_111 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
					  if (res) return res;
					}
				        break;
				      }
				    default:;
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
  switch (TREE_CODE (_p1))
    {
    case MULT_EXPR:
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
		    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p1, _p0, _q31 };
		    tree res = generic_simplify_139 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
{
  tree _p1_pops[1];
  if (tree_nop_convert (_p1, _p1_pops))
    {
      tree _q30 = _p1_pops[0];
      switch (TREE_CODE (_q30))
        {
	case MULT_EXPR:
	  {
	    tree _q40 = TREE_OPERAND (_q30, 0);
	    tree _q41 = TREE_OPERAND (_q30, 1);
	    {
	      tree _q40_pops[1];
	      if (tree_nop_convert (_q40, _q40_pops))
	        {
		  tree _q50 = _q40_pops[0];
		  if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
		    {
		      switch (TREE_CODE (_q41))
		        {
			case INTEGER_CST:
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q30, _p0, _q41 };
			      tree res = generic_simplify_112 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
			      if (res) return res;
			    }
			    break;
			  }
		        default:;
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
  switch (TREE_CODE (_p0))
    {
    case LSHIFT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case LSHIFT_EXPR:
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
				tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
				tree res = generic_simplify_140 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
	      if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		    tree res = generic_simplify_141 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
    case LSHIFT_EXPR:
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
		    tree captures[3] ATTRIBUTE_UNUSED = { _p1, _p0, _q31 };
		    tree res = generic_simplify_141 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		    if (res) return res;
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
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	switch (TREE_CODE (_q30))
	  {
	  case BIT_XOR_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  if (tree_zero_one_valued_p (_q31))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q41, _q31 };
			tree res = generic_simplify_163 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	      if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
		{
		  if (tree_zero_one_valued_p (_q31))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q40, _q31 };
			tree res = generic_simplify_163 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	      break;
	    }
          default:;
          }
      if (tree_zero_one_valued_p (_q30))
	{
	  switch (TREE_CODE (_q31))
	    {
	    case BIT_XOR_EXPR:
	      {
		tree _q50 = TREE_OPERAND (_q31, 0);
		tree _q51 = TREE_OPERAND (_q31, 1);
		if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q51, _q30 };
		      tree res = generic_simplify_163 (loc, type, _p0, _p1, captures);
		      if (res) return res;
		    }
		  }
		if ((_q51 == _p0 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _p0, 0) && types_match (_q51, _p0)))
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q50, _q30 };
		      tree res = generic_simplify_163 (loc, type, _p0, _p1, captures);
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
  switch (TREE_CODE (_p0))
    {
    case MULT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  case BIT_XOR_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (tree_zero_one_valued_p (_q21))
		{
		  if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q21 };
			tree res = generic_simplify_163 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
		  if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q31, _q30, _q21 };
			tree res = generic_simplify_163 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	      break;
	    }
          default:;
          }
      if (tree_zero_one_valued_p (_q20))
	{
	  switch (TREE_CODE (_q21))
	    {
	    case BIT_XOR_EXPR:
	      {
		tree _q40 = TREE_OPERAND (_q21, 0);
		tree _q41 = TREE_OPERAND (_q21, 1);
		if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _q20 };
		      tree res = generic_simplify_163 (loc, type, _p0, _p1, captures);
		      if (res) return res;
		    }
		  }
		if ((_p1 == _q41 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q41, 0) && types_match (_p1, _q41)))
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _q20 };
		      tree res = generic_simplify_163 (loc, type, _p0, _p1, captures);
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
 && (TREE_CODE_CLASS (BIT_XOR_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1388;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1388;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1388;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1388:;
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
 && (TREE_CODE_CLASS (BIT_XOR_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1389;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1389;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1389;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1389;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1389:;
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
 && (TREE_CODE_CLASS (BIT_XOR_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1390;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1390;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1390;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1390;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1390:;
	    }
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
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_164 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR, UNGE_EXPR);
	      if (res) return res;
	    }
	  }
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
			tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q50 };
			tree res = generic_simplify_165 (loc, type, _p0, _p1, captures, LT_EXPR);
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
			tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q50 };
			tree res = generic_simplify_166 (loc, type, _p0, _p1, captures);
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
    case LE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_164 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR, UNGT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case EQ_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_164 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR, NE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case NE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_164 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR, EQ_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case GE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_164 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR, UNLT_EXPR);
	      if (res) return res;
	    }
	  }
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
			tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q50 };
			tree res = generic_simplify_165 (loc, type, _p0, _p1, captures, GE_EXPR);
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
			tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q50, _p0, _q20 };
			tree res = generic_simplify_166 (loc, type, _p0, _p1, captures);
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
    case GT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_164 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR, UNLE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case UNORDERED_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_164 (loc, type, _p0, _p1, captures, UNORDERED_EXPR, ORDERED_EXPR, ORDERED_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case ORDERED_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_164 (loc, type, _p0, _p1, captures, ORDERED_EXPR, UNORDERED_EXPR, UNORDERED_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case UNLT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_164 (loc, type, _p0, _p1, captures, UNLT_EXPR, GE_EXPR, GE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case UNLE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_164 (loc, type, _p0, _p1, captures, UNLE_EXPR, GT_EXPR, GT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case UNGT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_164 (loc, type, _p0, _p1, captures, UNGT_EXPR, LE_EXPR, LE_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case UNGE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_164 (loc, type, _p0, _p1, captures, UNGE_EXPR, LT_EXPR, LT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case UNEQ_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_164 (loc, type, _p0, _p1, captures, UNEQ_EXPR, LTGT_EXPR, LTGT_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    case LTGT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_truep (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_164 (loc, type, _p0, _p1, captures, LTGT_EXPR, UNEQ_EXPR, UNEQ_EXPR);
	      if (res) return res;
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
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1391;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[0];
				    _o1[1] = captures[2];
				    _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
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
next_after_fail1391:;
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
	case CFN_PARITY:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_PARITY:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_167 (loc, type, _p0, _p1, captures, CFN_PARITY);
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
	case CFN_BUILT_IN_PARITYL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_PARITYL:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_167 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_PARITYL);
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
	case CFN_BUILT_IN_PARITYIMAX:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_PARITYIMAX:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_167 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_PARITYIMAX);
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
	case CFN_BUILT_IN_PARITY:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_PARITY:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_167 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_PARITY);
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
	case CFN_BUILT_IN_PARITYLL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_PARITYLL:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_167 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_PARITYLL);
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
  return NULL_TREE;
}

tree
generic_simplify_BIT_INSERT_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1, tree _p2)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p1))
    {
    case BIT_FIELD_REF:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	tree _q32 = TREE_OPERAND (_p1, 2);
	{
	  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _p2 };
	  if (VECTOR_TYPE_P (type)
 && (VECTOR_MODE_P (TYPE_MODE (type))
 || optimize_vectors_before_lowering_p ())
 && types_match (captures[0], captures[2])
 && types_match (TREE_TYPE (TREE_TYPE (captures[0])), TREE_TYPE (captures[1]))
 && TYPE_VECTOR_SUBPARTS (type).is_constant ()
 && multiple_p (wi::to_poly_offset (captures[4]),
 wi::to_poly_offset (TYPE_SIZE (TREE_TYPE (type))))
)
	    {
	      {
 unsigned HOST_WIDE_INT elsz
 = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (TREE_TYPE (captures[2]))));
 poly_uint64 relt = exact_div (tree_to_poly_uint64 (captures[4]), elsz);
 poly_uint64 ielt = exact_div (tree_to_poly_uint64 (captures[5]), elsz);
 unsigned nunits = TYPE_VECTOR_SUBPARTS (type).to_constant ();
 vec_perm_builder builder;
 builder.new_vector (nunits, nunits, 1);
 for (unsigned i = 0; i < nunits; ++i)
 builder.quick_push (known_eq (ielt, i) ? nunits + relt : i);
 vec_perm_indices sel (builder, 2, nunits);
		  if (!VECTOR_MODE_P (TYPE_MODE (type))
 || can_vec_perm_const_p (TYPE_MODE (type), TYPE_MODE (type), sel, false)
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1959;
		      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail1959;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1959;
		      {
			tree res_op0;
			res_op0 = captures[0];
			tree res_op1;
			res_op1 = captures[2];
			tree res_op2;
			res_op2 =  vec_perm_indices_to_tree
 (build_vector_type (ssizetype, nunits), sel);
			tree _r;
			_r = fold_build3_loc (loc, VEC_PERM_EXPR, type, res_op0, res_op1, res_op2);
			if (TREE_SIDE_EFFECTS (captures[3]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1064, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1959:;
		    }
	      }
	    }
	}
        break;
      }
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_VEC_PERM_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1, tree _p2)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case PLUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case MINUS_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_p2))
	        {
		case VECTOR_CST:
		  {
		    {
		      tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q51, _p2 };
		      tree res = generic_simplify_571 (loc, type, _p0, _p1, _p2, captures, PLUS_EXPR, MINUS_EXPR);
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
	  case PLUS_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_p2))
	        {
		case VECTOR_CST:
		  {
		    {
		      tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q51, _p2 };
		      tree res = generic_simplify_571 (loc, type, _p0, _p1, _p2, captures, MINUS_EXPR, PLUS_EXPR);
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
  switch (TREE_CODE (_p2))
    {
    case VECTOR_CST:
      {
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _p2 };
	  {
 tree op0 = captures[0], op1 = captures[1], op2 = captures[2];
 machine_mode result_mode = TYPE_MODE (type);
 machine_mode op_mode = TYPE_MODE (TREE_TYPE (op0));
 vec_perm_builder builder;
	      if (tree_to_vec_perm_builder (&builder, op2)
)
		{
		  {
 poly_uint64 nelts = TYPE_VECTOR_SUBPARTS (type);
 bool single_arg = (op0 == op1);
 vec_perm_indices sel (builder, single_arg ? 1 : 2, nelts);
		      if (sel.series_p (0, 1, 0, 1)
)
			{
			  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1960;
			  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1960;
			  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1960;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1960;
			  {
			    tree _r;
			    _r =  op0;
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1065, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1960:;
			}
		      else
			{
			  if (sel.series_p (0, 1, nelts, 1)
)
			    {
			      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1961;
			      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1961;
			      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1961;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1961;
			      {
				tree _r;
				_r =  op1;
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1066, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail1961:;
			    }
			  else
			    {
			      {
 if (!single_arg)
 {
 if (sel.all_from_input_p (0))
 op1 = op0;
 else if (sel.all_from_input_p (1))
 {
 op0 = op1;
 sel.rotate_inputs (1);
 }
 else if (known_ge (poly_uint64 (sel[0]), nelts))
 {
 std::swap (op0, op1);
 sel.rotate_inputs (1);
 }
 }
 gassign *def;
 tree cop0 = op0, cop1 = op1;
 if (TREE_CODE (op0) == SSA_NAME
 && (def = dyn_cast <gassign *> (SSA_NAME_DEF_STMT (op0)))
 && gimple_assign_rhs_code (def) == CONSTRUCTOR)
 cop0 = gimple_assign_rhs1 (def);
 if (TREE_CODE (op1) == SSA_NAME
 && (def = dyn_cast <gassign *> (SSA_NAME_DEF_STMT (op1)))
 && gimple_assign_rhs_code (def) == CONSTRUCTOR)
 cop1 = gimple_assign_rhs1 (def);
 tree t;
				  if ((TREE_CODE (cop0) == VECTOR_CST
 || TREE_CODE (cop0) == CONSTRUCTOR)
 && (TREE_CODE (cop1) == VECTOR_CST
 || TREE_CODE (cop1) == CONSTRUCTOR)
 && (t = fold_vec_perm (type, cop0, cop1, sel))
)
				    {
				      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1962;
				      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1962;
				      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1962;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1962;
				      {
					tree _r;
					_r =  t;
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1067, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail1962:;
				    }
				  else
				    {
				      {
 bool changed = (op0 == op1 && !single_arg);
 tree ins = NULL_TREE;
 unsigned at = 0;
 if (op_mode != BLKmode
 && (TREE_CODE (cop0) == VECTOR_CST
 || TREE_CODE (cop0) == CONSTRUCTOR
 || TREE_CODE (cop1) == VECTOR_CST
 || TREE_CODE (cop1) == CONSTRUCTOR))
 {
 bool insert_first_p = sel.series_p (1, 1, nelts + 1, 1);
 if (insert_first_p)
 {
 at = 0;
 if ((ins = fold_read_from_vector (cop0, sel[0])))
 op0 = op1;
 }
 if (!insert_first_p || (!ins && maybe_eq (nelts, 2u)))
 {
 unsigned int encoded_nelts = sel.encoding ().encoded_nelts ();
 for (at = 0; at < encoded_nelts; ++at)
 if (maybe_ne (sel[at], at))
 break;
 if (at < encoded_nelts
 && (known_eq (at + 1, nelts)
 || sel.series_p (at + 1, 1, at + 1, 1)))
 {
 if (known_lt (poly_uint64 (sel[at]), nelts))
 ins = fold_read_from_vector (cop0, sel[at]);
 else
 ins = fold_read_from_vector (cop1, sel[at] - nelts);
 }
 }
 }
 if (!ins && sel.encoding () != builder)
 {
 tree oldop2 = op2;
 if (sel.ninputs () == 2
 || can_vec_perm_const_p (result_mode, op_mode, sel, false))
 op2 = vec_perm_indices_to_tree (TREE_TYPE (op2), sel);
 else
 {
 vec_perm_indices sel2 (builder, 2, nelts);
 if (can_vec_perm_const_p (result_mode, op_mode, sel2, false))
 op2 = vec_perm_indices_to_tree (TREE_TYPE (op2), sel2);
 else
 op2 = vec_perm_indices_to_tree (TREE_TYPE (op2), sel);
 }
 if (!operand_equal_p (op2, oldop2, 0))
 changed = true;
 }
					  if (ins
)
					    {
					      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1963;
					      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1963;
					      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1963;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1963;
					      {
						tree res_op0;
						res_op0 =  op0;
						tree res_op1;
						res_op1 =  ins;
						tree res_op2;
						res_op2 =  bitsize_int (at * vector_element_bits (type));
						tree _r;
						_r = fold_build3_loc (loc, BIT_INSERT_EXPR, type, res_op0, res_op1, res_op2);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1068, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail1963:;
					    }
					  else
					    {
					      if (changed
)
						{
						  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1964;
						  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1964;
						  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1964;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1964;
						  {
						    tree res_op0;
						    res_op0 =  op0;
						    tree res_op1;
						    res_op1 =  op1;
						    tree res_op2;
						    res_op2 =  op2;
						    tree _r;
						    _r = fold_build3_loc (loc, VEC_PERM_EXPR, type, res_op0, res_op1, res_op2);
						    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1069, __FILE__, __LINE__, true);
						    return _r;
						  }
next_after_fail1964:;
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
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case VIEW_CONVERT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case VEC_PERM_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      tree _q32 = TREE_OPERAND (_q20, 2);
	      switch (TREE_CODE (_q32))
	        {
		case VECTOR_CST:
		  {
		    if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		      {
			switch (TREE_CODE (_p2))
			  {
			  case VECTOR_CST:
			    {
			      {
				tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q32, _p2 };
				if (TYPE_VECTOR_SUBPARTS (type).is_constant ()
)
				  {
				    {
 machine_mode result_mode = TYPE_MODE (type);
 machine_mode op_mode = TYPE_MODE (TREE_TYPE (captures[2]));
 int nelts = TYPE_VECTOR_SUBPARTS (type).to_constant ();
 vec_perm_builder builder0;
 vec_perm_builder builder1;
 vec_perm_builder builder2 (nelts, nelts, 1);
					if (tree_to_vec_perm_builder (&builder0, captures[4])
 && tree_to_vec_perm_builder (&builder1, captures[5])
 && TYPE_SIZE (TREE_TYPE (TREE_TYPE (captures[0])))
 == TYPE_SIZE (TREE_TYPE (TREE_TYPE (captures[1])))
)
					  {
					    {
 vec_perm_indices sel0 (builder0, 2, nelts);
 vec_perm_indices sel1 (builder1, 1, nelts);
 for (int i = 0; i < nelts; i++)
 builder2.quick_push (sel0[sel1[i].to_constant ()]);
 vec_perm_indices sel2 (builder2, 2, nelts);
 tree op0 = NULL_TREE;
 if (can_vec_perm_const_p (result_mode, op_mode, sel2, false)
 || (single_use (captures[0])
 ? (!can_vec_perm_const_p (result_mode, op_mode, sel0, false)
 || !can_vec_perm_const_p (result_mode, op_mode, sel1, false))
 : !can_vec_perm_const_p (result_mode, op_mode, sel1, false)))
 op0 = vec_perm_indices_to_tree (TREE_TYPE (captures[5]), sel2);
						if (op0
)
						  {
						    if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1965;
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1965;
						    {
						      tree res_op0;
						      {
							tree _o1[3], _r1;
							_o1[0] = captures[2];
							_o1[1] = captures[3];
							_o1[2] =  op0;
							_r1 = fold_build3_loc (loc, VEC_PERM_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1], _o1[2]);
							res_op0 = _r1;
						      }
						      tree _r;
						      _r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
						      if (TREE_SIDE_EFFECTS (captures[4]))
							_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[4]), _r);
						      if (TREE_SIDE_EFFECTS (captures[5]))
							_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[5]), _r);
						      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1070, __FILE__, __LINE__, true);
						      return _r;
						    }
next_after_fail1965:;
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
    case VEC_PERM_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	tree _q22 = TREE_OPERAND (_p0, 2);
	switch (TREE_CODE (_q22))
	  {
	  case VECTOR_CST:
	    {
	      if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case VECTOR_CST:
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21, _q22, _p2 };
			  if (TYPE_VECTOR_SUBPARTS (type).is_constant ()
)
			    {
			      {
 machine_mode result_mode = TYPE_MODE (type);
 machine_mode op_mode = TYPE_MODE (TREE_TYPE (captures[2]));
 int nelts = TYPE_VECTOR_SUBPARTS (type).to_constant ();
 vec_perm_builder builder0;
 vec_perm_builder builder1;
 vec_perm_builder builder2 (nelts, nelts, 1);
				  if (tree_to_vec_perm_builder (&builder0, captures[4])
 && tree_to_vec_perm_builder (&builder1, captures[5])
 && TYPE_SIZE (TREE_TYPE (TREE_TYPE (captures[0])))
 == TYPE_SIZE (TREE_TYPE (TREE_TYPE (captures[1])))
)
				    {
				      {
 vec_perm_indices sel0 (builder0, 2, nelts);
 vec_perm_indices sel1 (builder1, 1, nelts);
 for (int i = 0; i < nelts; i++)
 builder2.quick_push (sel0[sel1[i].to_constant ()]);
 vec_perm_indices sel2 (builder2, 2, nelts);
 tree op0 = NULL_TREE;
 if (can_vec_perm_const_p (result_mode, op_mode, sel2, false)
 || (single_use (captures[0])
 ? (!can_vec_perm_const_p (result_mode, op_mode, sel0, false)
 || !can_vec_perm_const_p (result_mode, op_mode, sel1, false))
 : !can_vec_perm_const_p (result_mode, op_mode, sel1, false)))
 op0 = vec_perm_indices_to_tree (TREE_TYPE (captures[5]), sel2);
					  if (op0
)
					    {
					      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1966;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1966;
					      {
						tree res_op0;
						{
						  tree _o1[3], _r1;
						  _o1[0] = captures[2];
						  _o1[1] = captures[3];
						  _o1[2] =  op0;
						  _r1 = fold_build3_loc (loc, VEC_PERM_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1], _o1[2]);
						  res_op0 = _r1;
						}
						tree _r;
						_r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
						if (TREE_SIDE_EFFECTS (captures[4]))
						  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[4]), _r);
						if (TREE_SIDE_EFFECTS (captures[5]))
						  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[5]), _r);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1070, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail1966:;
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
	      break;
	    }
          default:;
          }
        break;
      }
    default:;
    }
if (tree_vec_same_elem_p (_p0))
  {
    if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p2 };
	  if (types_match (type, TREE_TYPE (captures[0]))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1967;
	      {
		tree _r;
		_r = captures[0];
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1071, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1967:;
	    }
	  else
	    {
	      {
 tree elem = uniform_vector_p (captures[0]);
		  if (elem
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1968;
		      {
			tree _r;
			_r =  build_vector_from_val (type, elem);
			if (TREE_SIDE_EFFECTS (captures[0]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
			if (TREE_SIDE_EFFECTS (captures[1]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1072, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1968:;
		    }
	      }
	    }
	}
      }
  }
  switch (TREE_CODE (_p1))
    {
    case VEC_PERM_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	tree _q32 = TREE_OPERAND (_p1, 2);
	switch (TREE_CODE (_q32))
	  {
	  case VECTOR_CST:
	    {
	      switch (TREE_CODE (_p2))
	        {
		case VECTOR_CST:
		  {
		    {
		      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _p2 };
		      if (TYPE_VECTOR_SUBPARTS (type).is_constant ()
)
			{
			  {
 machine_mode result_mode = TYPE_MODE (type);
 machine_mode op_mode = TYPE_MODE (TREE_TYPE (captures[2]));
 int nelts = TYPE_VECTOR_SUBPARTS (type).to_constant ();
 vec_perm_builder builder0;
 vec_perm_builder builder1;
 vec_perm_builder builder2 (nelts, nelts, 2);
			      if (tree_to_vec_perm_builder (&builder0, captures[4])
 && tree_to_vec_perm_builder (&builder1, captures[5])
)
				{
				  {
 vec_perm_indices sel0 (builder0, 2, nelts);
 vec_perm_indices sel1 (builder1, 2, nelts);
 bool use_1 = false, use_2 = false;
 for (int i = 0; i < nelts; i++)
 {
 if (known_lt ((poly_uint64)sel1[i], sel1.nelts_per_input ()))
 builder2.quick_push (sel1[i]);
 else
 {
 poly_uint64 j = sel0[(sel1[i] - sel1.nelts_per_input ())
 .to_constant ()];
 if (known_lt (j, sel0.nelts_per_input ()))
 use_1 = true;
 else
 {
 use_2 = true;
 j -= sel0.nelts_per_input ();
 }
 builder2.quick_push (j + sel1.nelts_per_input ());
 }
 }
				      if (use_1 ^ use_2
)
					{
					  {
 vec_perm_indices sel2 (builder2, 2, nelts);
 tree op0 = NULL_TREE;
 if (can_vec_perm_const_p (result_mode, op_mode, sel2, false)
 || (single_use (captures[1])
 ? (!can_vec_perm_const_p (result_mode, op_mode, sel0, false)
 || !can_vec_perm_const_p (result_mode, op_mode, sel1, false))
 : !can_vec_perm_const_p (result_mode, op_mode, sel1, false)))
 op0 = vec_perm_indices_to_tree (TREE_TYPE (captures[5]), sel2);
					      if (op0
)
						{
						  if (use_1
)
						    {
						      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1969;
						      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1969;
						      {
							tree res_op0;
							res_op0 = captures[0];
							tree res_op1;
							res_op1 = captures[2];
							tree res_op2;
							res_op2 =  op0;
							tree _r;
							_r = fold_build3_loc (loc, VEC_PERM_EXPR, type, res_op0, res_op1, res_op2);
							if (TREE_SIDE_EFFECTS (captures[3]))
							  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
							if (TREE_SIDE_EFFECTS (captures[4]))
							  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[4]), _r);
							if (TREE_SIDE_EFFECTS (captures[5]))
							  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[5]), _r);
							if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1073, __FILE__, __LINE__, true);
							return _r;
						      }
next_after_fail1969:;
						    }
						  else
						    {
						      if (use_2
)
							{
							  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1970;
							  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1970;
							  {
							    tree res_op0;
							    res_op0 = captures[0];
							    tree res_op1;
							    res_op1 = captures[3];
							    tree res_op2;
							    res_op2 =  op0;
							    tree _r;
							    _r = fold_build3_loc (loc, VEC_PERM_EXPR, type, res_op0, res_op1, res_op2);
							    if (TREE_SIDE_EFFECTS (captures[2]))
							      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
							    if (TREE_SIDE_EFFECTS (captures[4]))
							      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[4]), _r);
							    if (TREE_SIDE_EFFECTS (captures[5]))
							      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[5]), _r);
							    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1074, __FILE__, __LINE__, true);
							    return _r;
							  }
next_after_fail1970:;
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
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case VEC_PERM_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	tree _q22 = TREE_OPERAND (_p0, 2);
	switch (TREE_CODE (_q22))
	  {
	  case VECTOR_CST:
	    {
	      switch (TREE_CODE (_p2))
	        {
		case VECTOR_CST:
		  {
		    {
		      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q22, _p1, _p2 };
		      if (TYPE_VECTOR_SUBPARTS (type).is_constant ()
)
			{
			  {
 machine_mode result_mode = TYPE_MODE (type);
 machine_mode op_mode = TYPE_MODE (TREE_TYPE (captures[1]));
 int nelts = TYPE_VECTOR_SUBPARTS (type).to_constant ();
 vec_perm_builder builder0;
 vec_perm_builder builder1;
 vec_perm_builder builder2 (nelts, nelts, 2);
			      if (tree_to_vec_perm_builder (&builder0, captures[3])
 && tree_to_vec_perm_builder (&builder1, captures[5])
)
				{
				  {
 vec_perm_indices sel0 (builder0, 2, nelts);
 vec_perm_indices sel1 (builder1, 2, nelts);
 bool use_1 = false, use_2 = false;
 for (int i = 0; i < nelts; i++)
 {
 if (known_ge ((poly_uint64)sel1[i], sel1.nelts_per_input ()))
 builder2.quick_push (sel1[i]);
 else
 {
 poly_uint64 j = sel0[sel1[i].to_constant ()];
 if (known_lt (j, sel0.nelts_per_input ()))
 use_1 = true;
 else
 {
 use_2 = true;
 j -= sel0.nelts_per_input ();
 }
 builder2.quick_push (j);
 }
 }
				      if (use_1 ^ use_2
)
					{
					  {
 vec_perm_indices sel2 (builder2, 2, nelts);
 tree op0 = NULL_TREE;
 if (can_vec_perm_const_p (result_mode, op_mode, sel2, false)
 || (single_use (captures[0])
 ? (!can_vec_perm_const_p (result_mode, op_mode, sel0, false)
 || !can_vec_perm_const_p (result_mode, op_mode, sel1, false))
 : !can_vec_perm_const_p (result_mode, op_mode, sel1, false)))
 op0 = vec_perm_indices_to_tree (TREE_TYPE (captures[5]), sel2);
					      if (op0
)
						{
						  if (use_1
)
						    {
						      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1971;
						      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1971;
						      {
							tree res_op0;
							res_op0 = captures[1];
							tree res_op1;
							res_op1 = captures[4];
							tree res_op2;
							res_op2 =  op0;
							tree _r;
							_r = fold_build3_loc (loc, VEC_PERM_EXPR, type, res_op0, res_op1, res_op2);
							if (TREE_SIDE_EFFECTS (captures[2]))
							  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
							if (TREE_SIDE_EFFECTS (captures[3]))
							  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
							if (TREE_SIDE_EFFECTS (captures[5]))
							  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[5]), _r);
							if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1075, __FILE__, __LINE__, true);
							return _r;
						      }
next_after_fail1971:;
						    }
						  else
						    {
						      if (use_2
)
							{
							  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1972;
							  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1972;
							  {
							    tree res_op0;
							    res_op0 = captures[2];
							    tree res_op1;
							    res_op1 = captures[4];
							    tree res_op2;
							    res_op2 =  op0;
							    tree _r;
							    _r = fold_build3_loc (loc, VEC_PERM_EXPR, type, res_op0, res_op1, res_op2);
							    if (TREE_SIDE_EFFECTS (captures[1]))
							      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
							    if (TREE_SIDE_EFFECTS (captures[3]))
							      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
							    if (TREE_SIDE_EFFECTS (captures[5]))
							      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[5]), _r);
							    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1076, __FILE__, __LINE__, true);
							    return _r;
							  }
next_after_fail1972:;
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
        break;
      }
    default:;
    }
  return NULL_TREE;
}
#pragma GCC diagnostic pop
