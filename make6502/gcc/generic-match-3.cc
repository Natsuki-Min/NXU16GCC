/* Generated automatically by the program `genmatch' from
   a IL pattern matching and simplification description.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-function"

#include "generic-match-auto.h"

bool
tree_truth_valued_p (tree t)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  {
    tree captures[1] ATTRIBUTE_UNUSED = { t };
    if (INTEGRAL_TYPE_P (type) && TYPE_PRECISION (type) == 1
)
      {
	{
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 20, __FILE__, __LINE__, false);
	  return true;
	}
      }
  }
  switch (TREE_CODE (t))
    {
    case LT_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 21, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case LE_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 21, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case EQ_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 21, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case NE_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 21, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case GE_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 21, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case GT_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 21, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case UNORDERED_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 21, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case ORDERED_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 21, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case UNLT_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 21, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case UNLE_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 21, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case UNGT_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 21, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case UNGE_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 21, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case UNEQ_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 21, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case LTGT_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 21, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case TRUTH_AND_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 21, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case TRUTH_ANDIF_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 21, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case TRUTH_OR_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 21, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case TRUTH_ORIF_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 21, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case TRUTH_XOR_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 21, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case TRUTH_NOT_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 22, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    default:;
    }
  return false;
}

tree
generic_simplify_1 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (COPYSIGN_ALL))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail219;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree _r;
    _r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 89, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail219:;
  return NULL_TREE;
}

tree
generic_simplify_2 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (fold_real_zero_addition_p (type, captures[0], captures[1], 0)
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail220;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree _r;
	_r = non_lvalue_loc (loc, res_op0);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 90, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail220:;
    }
  return NULL_TREE;
}

tree
generic_simplify_4 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (type) && TYPE_OVERFLOW_UNDEFINED (type)
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail223;
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail223;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail223;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree _r;
	_r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 93, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail223:;
    }
  return NULL_TREE;
}

tree
generic_simplify_6 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 bool wascmp0, wascmp1;
      if (bitwise_inverted_equal_p (captures[1], captures[3], wascmp0)
 && bitwise_inverted_equal_p (captures[0], captures[2], wascmp1)
 && ((!wascmp0 && !wascmp1)
 || element_precision (type) == 1)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail225;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree res_op1;
	    res_op1 = captures[3];
	    tree _r;
	    _r = fold_build2_loc (loc, BIT_XOR_EXPR, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (TREE_SIDE_EFFECTS (captures[2]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 95, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail225:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_9 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail228;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 = captures[1];
    tree _r;
    _r = fold_build2_loc (loc, PLUS_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 98, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail228:;
  return NULL_TREE;
}

tree
generic_simplify_12 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail231;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree _r;
	    _r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 101, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail231:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_15 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (invert_tree_comparison (cmp, HONOR_NANS (captures[2])) == icmp
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail234;
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail234;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail234;
      {
	tree res_op0;
	res_op0 = captures[1];
	tree res_op1;
	res_op1 = captures[4];
	tree res_op2;
	res_op2 = captures[7];
	tree _r;
	_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 104, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail234:;
    }
  return NULL_TREE;
}

tree
generic_simplify_16 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (inner_op),
 const enum tree_code ARG_UNUSED (neg_inner_op),
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
	      if (!CONSTANT_CLASS_P (captures[0])
)
		{
		  if (outer_op == PLUS_EXPR
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail235;
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
			  _o1[0] = captures[2];
			  {
			    tree _o2[1], _r2;
			    _o2[0] = captures[1];
			    _r2 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, _o2[0]);
			    _o1[1] = _r2;
			  }
			  _r1 = fold_build2_loc (loc, inner_op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			  if (EXPR_P (_r1))
			    goto next_after_fail235;
			  res_op1 = _r1;
			}
			tree _r;
			_r = fold_build2_loc (loc, PLUS_EXPR, type, res_op0, res_op1);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 105, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail235:;
		    }
		  else
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail236;
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
			  _o1[0] = captures[2];
			  {
			    tree _o2[1], _r2;
			    _o2[0] = captures[1];
			    _r2 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, _o2[0]);
			    _o1[1] = _r2;
			  }
			  _r1 = fold_build2_loc (loc, neg_inner_op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			  if (EXPR_P (_r1))
			    goto next_after_fail236;
			  res_op1 = _r1;
			}
			tree _r;
			_r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 106, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail236:;
		    }
		}
	    }
	  else
	    {
	      if (!ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 || TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[0]))
)
		{
		  if (outer_op == PLUS_EXPR
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail237;
		      {
			tree res_op0;
			{
			  tree _o1[2], _r1;
			  _o1[0] = captures[0];
			  {
			    tree _o2[2], _r2;
			    {
			      tree _o3[1], _r3;
			      _o3[0] = captures[2];
			      _r3 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, TREE_TYPE (_o1[0]), _o3[0]);
			      _o2[0] = _r3;
			    }
			    _o2[1] = captures[1];
			    _r2 = fold_build2_loc (loc, inner_op, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
			    if (EXPR_P (_r2))
			      goto next_after_fail237;
			    _o1[1] = _r2;
			  }
			  _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			  res_op0 = _r1;
			}
			tree _r;
			_r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 107, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail237:;
		    }
		  else
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail238;
		      {
			tree res_op0;
			{
			  tree _o1[2], _r1;
			  _o1[0] = captures[0];
			  {
			    tree _o2[2], _r2;
			    {
			      tree _o3[1], _r3;
			      _o3[0] = captures[2];
			      _r3 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, TREE_TYPE (_o1[0]), _o3[0]);
			      _o2[0] = _r3;
			    }
			    _o2[1] = captures[1];
			    _r2 = fold_build2_loc (loc, neg_inner_op, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
			    if (EXPR_P (_r2))
			      goto next_after_fail238;
			    _o1[1] = _r2;
			  }
			  _r1 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			  res_op0 = _r1;
			}
			tree _r;
			_r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 108, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail238:;
		    }
		}
	      else
		{
		  if (types_match (type, captures[0]) && !TYPE_OVERFLOW_SANITIZED (type)
)
		    {
		      {
 tree cst = const_binop (outer_op == inner_op
 ? PLUS_EXPR : MINUS_EXPR,
 type, captures[1], captures[2]);
			  if (cst
)
			    {
			      if (INTEGRAL_TYPE_P (type) && !TREE_OVERFLOW (cst)
)
				{
				  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail239;
				  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail239;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail239;
				  {
				    tree res_op0;
				    res_op0 = captures[0];
				    tree res_op1;
				    res_op1 =  cst;
				    tree _r;
				    _r = fold_build2_loc (loc, inner_op, type, res_op0, res_op1);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 109, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail239:;
				}
			      else
				{
				  if (INTEGRAL_TYPE_P (type)
 && wi::to_wide (cst) == wi::min_value (type)
)
				    {
				      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail240;
				      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail240;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail240;
				      {
					tree res_op0;
					res_op0 = captures[0];
					tree res_op1;
					res_op1 =  wide_int_to_tree (type, wi::to_wide (cst));
					tree _r;
					_r = fold_build2_loc (loc, neg_inner_op, type, res_op0, res_op1);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 110, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail240:;
				    }
				  else
				    {
				      {
 tree utype = unsigned_type_for (type);
					  if (utype
)
					    {
					      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail241;
					      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail241;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail241;
					      {
						tree res_op0;
						{
						  tree _o1[2], _r1;
						  {
						    tree _o2[1], _r2;
						    _o2[0] = captures[0];
						    _r2 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, utype, _o2[0]);
						    _o1[0] = _r2;
						  }
						  {
						    tree _o2[1], _r2;
						    _o2[0] =  TREE_OVERFLOW (cst)
 ? drop_tree_overflow (cst) : cst;
						    _r2 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, utype, _o2[0]);
						    _o1[1] = _r2;
						  }
						  _r1 = fold_build2_loc (loc, inner_op, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						  res_op0 = _r1;
						}
						tree _r;
						_r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 111, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail241:;
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
generic_simplify_39 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail267;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail267;
      {
	tree res_op0;
	res_op0 = unshare_expr (captures[1]);
	tree res_op1;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[4];
	  _o1[1] = captures[1];
	  _r1 = fold_build2_loc (loc, TRUNC_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 137, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail267:;
    }
  return NULL_TREE;
}

tree
generic_simplify_44 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail272;
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
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 142, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail272:;
  return NULL_TREE;
}

tree
generic_simplify_49 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (!ANY_INTEGRAL_TYPE_P (type)
 || TYPE_OVERFLOW_WRAPS (type)
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail277;
	      {
		tree res_op0;
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[1];
		  _r1 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, _o1[0]);
		  res_op0 = _r1;
		}
		tree _r;
		_r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
		if (TREE_SIDE_EFFECTS (captures[0]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 146, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail277:;
	    }
	  else
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail278;
	      {
		tree res_op0;
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[1];
		  _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
		  res_op0 = _r1;
		}
		tree _r;
		_r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
		if (TREE_SIDE_EFFECTS (captures[0]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 147, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail278:;
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_59 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail293;
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
	_r = fold_build2_loc (loc, PLUS_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 162, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail293:;
    }
  return NULL_TREE;
}

tree
generic_simplify_64 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail298;
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
	_r = fold_build2_loc (loc, RSHIFT_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 167, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail298:;
    }
  return NULL_TREE;
}

tree
generic_simplify_71 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail305;
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
    _r = fold_build2_loc (loc, BIT_IOR_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 173, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail305:;
  return NULL_TREE;
}

tree
generic_simplify_77 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_PRECISION (TREE_TYPE (captures[0])) == 1
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail311;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail311;
      {
	tree _r;
	_r = captures[2];
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 178, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail311:;
    }
  return NULL_TREE;
}

tree
generic_simplify_85 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail319;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail319;
  {
    tree _r;
    _r = captures[0];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 183, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail319:;
  return NULL_TREE;
}

tree
generic_simplify_91 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail325;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail325;
  {
    tree _r;
    _r = captures[0];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 176, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail325:;
  return NULL_TREE;
}

tree
generic_simplify_97 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail335;
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
next_after_fail335:;
    }
  return NULL_TREE;
}

tree
generic_simplify_107 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop),
 const enum tree_code ARG_UNUSED (rbitop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (bitop == BIT_AND_EXPR
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail345;
      {
	tree _r;
	_r =  build_zero_cst (type);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 197, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail345:;
    }
  else
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail346;
      {
	tree _r;
	_r =  build_minus_one_cst (type);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 198, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail346:;
    }
  return NULL_TREE;
}

tree
generic_simplify_115 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && invert_tree_comparison (cmp, HONOR_NANS (captures[1])) == icmp
 && canonicalize_math_after_vectorization_p ()
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail356;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail356;
      {
	tree res_op0;
	{
	  tree _o1[3], _r1;
	  _o1[0] = captures[0];
	  _o1[1] = captures[3];
	  _o1[2] = captures[5];
	  _r1 = fold_build3_loc (loc, COND_EXPR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2]);
	  res_op0 = _r1;
	}
	tree res_op1;
	res_op1 =  build_one_cst (type);
	tree _r;
	_r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 206, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail356:;
    }
  return NULL_TREE;
}

tree
generic_simplify_122 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail367;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail367;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail367;
	      {
		tree _r;
		_r = captures[3];
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 217, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail367:;
	    }
	  else
	    {
	      if (code1 == NE_EXPR && val && allbits
)
		{
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail368;
		  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail368;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail368;
		  {
		    tree _r;
		    _r =  constant_boolean_node (true, type);
		    if (TREE_SIDE_EFFECTS (captures[1]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 218, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail368:;
		}
	      else
		{
		  if (code1 == NE_EXPR && !val && allbits
)
		    {
		      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail369;
		      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail369;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail369;
		      {
			tree _r;
			_r = captures[0];
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 219, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail369:;
		    }
		  else
		    {
		      if (code1 == EQ_EXPR
 && code2 == GT_EXPR
 && cmp == 0
 && allbits
)
			{
			  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail370;
			  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail370;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail370;
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
next_after_fail370:;
			}
		      else
			{
			  if (code1 == EQ_EXPR
 && code2 == LT_EXPR
 && cmp == 0
 && allbits
)
			    {
			      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail371;
			      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail371;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail371;
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
next_after_fail371:;
			    }
			  else
			    {
			      if (code1 == EQ_EXPR
 && code2 == GE_EXPR
 && one_before
 && allbits
)
				{
				  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail372;
				  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail372;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail372;
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
next_after_fail372:;
				}
			      else
				{
				  if (code1 == EQ_EXPR
 && code2 == LE_EXPR
 && one_after
 && allbits
)
				    {
				      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail373;
				      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail373;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail373;
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
next_after_fail373:;
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
generic_simplify_146 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail423;
  {
    tree res_op0;
    res_op0 = captures[1];
    tree res_op1;
    res_op1 = captures[2];
    tree _r;
    _r = fold_build2_loc (loc, BIT_IOR_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 242, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail423:;
  return NULL_TREE;
}

tree
generic_simplify_152 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (bitwise_equal_p (captures[0], captures[2])
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail429;
      {
	tree res_op0;
	{
	  tree _o1[1], _r1;
	  {
	    tree _o2[2], _r2;
	    _o2[0] = captures[0];
	    {
	      tree _o3[1], _r3;
	      _o3[0] = captures[1];
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
	  _r1 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	  res_op0 = _r1;
	}
	tree _r;
	_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	if (TREE_SIDE_EFFECTS (captures[2]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 248, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail429:;
    }
  return NULL_TREE;
}

tree
generic_simplify_161 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[1]))
 && tree_nop_conversion_p (type, TREE_TYPE (captures[2]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail438;
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
	_r = fold_build2_loc (loc, BIT_XOR_EXPR, type, res_op0, res_op1);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 257, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail438:;
    }
  return NULL_TREE;
}

tree
generic_simplify_170 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (COPYSIGN_ALL))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail449;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail449;
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
next_after_fail449:;
    }
  return NULL_TREE;
}

tree
generic_simplify_177 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail456;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail456;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree _r;
	    _r = fold_build1_loc (loc, ABSU_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 271, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail456:;
	}
      else
	{
	  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail457;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail457;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree _r;
	    _r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 272, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail457:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_185 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (SQRT_ALL))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations
)
    {
      if (!tree_expr_maybe_signaling_nan_p (captures[1])
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail467;
	  {
	    tree _r;
	    _r = captures[1];
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 279, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail467:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_192 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail474;
  {
    tree _r;
    _r =  constant_boolean_node (op == NE_EXPR ? true : false, type);
    if (TREE_SIDE_EFFECTS (captures[0]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 286, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail474:;
  return NULL_TREE;
}

tree
generic_simplify_198 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[2]))
 && (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[2]))
 || TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[2])))
 && (CONSTANT_CLASS_P (captures[2]) || (single_use (captures[1]) && single_use (captures[0])))
 && tree_nop_conversion_p (TREE_TYPE (captures[0]), TREE_TYPE (captures[1]))
 && tree_nop_conversion_p (TREE_TYPE (captures[0]), TREE_TYPE (captures[3]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail483;
      {
	tree res_op0;
	res_op0 = captures[2];
	tree res_op1;
	res_op1 =  build_zero_cst (TREE_TYPE (captures[2]));
	tree _r;
	_r = fold_build2_loc (loc, op, type, res_op0, res_op1);
	if (TREE_SIDE_EFFECTS (captures[3]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 294, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail483:;
    }
  return NULL_TREE;
}

tree
generic_simplify_205 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (TREE_TYPE (captures[1]), TREE_TYPE (captures[2]))
 && tree_nop_conversion_p (TREE_TYPE (captures[0]), TREE_TYPE (captures[2]))
 && (CONSTANT_CLASS_P (captures[3]) || (single_use (captures[1]) && single_use (captures[0])))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail503;
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
next_after_fail503:;
    }
  return NULL_TREE;
}

tree
generic_simplify_211 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp1),
 const enum tree_code ARG_UNUSED (cmp2),
 const enum tree_code ARG_UNUSED (rcmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0])) || POINTER_TYPE_P (TREE_TYPE (captures[0]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail509;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 = captures[1];
	tree _r;
	_r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 319, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail509:;
    }
  return NULL_TREE;
}

tree
generic_simplify_217 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail519;
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail519;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail519;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 =  wide_int_to_tree (TREE_TYPE (captures[0]),
 wi::exact_log2 (wi::to_wide (captures[1])));
	tree _r;
	_r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 329, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail519:;
    }
  return NULL_TREE;
}

tree
generic_simplify_220 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail529;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail529;
      {
	tree res_op0;
	res_op0 = captures[1];
	tree res_op1;
	res_op1 =  build_zero_cst (TREE_TYPE (captures[1]));
	tree _r;
	_r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 339, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail529:;
    }
  return NULL_TREE;
}

tree
generic_simplify_225 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_CODE (TREE_TYPE (captures[1])) != COMPLEX_TYPE
 && (!VECTOR_MODE_P (TYPE_MODE (TREE_TYPE (captures[1])))
 || !target_supports_op_p (TREE_TYPE (captures[1]), TRUNC_DIV_EXPR,
 optab_vector)
 || target_supports_op_p (TREE_TYPE (captures[1]), TRUNC_MOD_EXPR,
 optab_vector))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail535;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  _o1[1] = captures[2];
	  _r1 = fold_build2_loc (loc, TRUNC_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree res_op1;
	res_op1 =  build_zero_cst (TREE_TYPE (captures[1]));
	tree _r;
	_r = fold_build2_loc (loc, EQ_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 345, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail535:;
    }
  return NULL_TREE;
}

tree
generic_simplify_231 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (single_use (captures[0])
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail542;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail542;
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
next_after_fail542:;
    }
  return NULL_TREE;
}

tree
generic_simplify_236 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (single_use (captures[1]) && single_use (captures[3])
)
    {
      {
 tree otype = TREE_TYPE (captures[0]);
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail552;
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
next_after_fail552:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_240 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations
)
    {
      {
 tree tem = const_binop (op == PLUS_EXPR ? MINUS_EXPR : PLUS_EXPR,
 TREE_TYPE (captures[1]), captures[2], captures[1]);
	  if (tem && !TREE_OVERFLOW (tem)
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail570;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail570;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail570;
	      {
		tree res_op0;
		res_op0 = captures[0];
		tree res_op1;
		res_op1 =  tem;
		tree _r;
		_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 378, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail570:;
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_244 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (SCALAR_FLOAT_TYPE_P (TREE_TYPE (captures[0]))
 && ! DECIMAL_FLOAT_TYPE_P (TREE_TYPE (captures[0]))
)
    {
      {
 format_helper fmt (REAL_MODE_FORMAT (TYPE_MODE (TREE_TYPE (captures[0]))));
 tree type1 = TREE_TYPE (captures[1]);
 bool type1_signed_p = TYPE_SIGN (type1) == SIGNED;
 tree type2 = TREE_TYPE (captures[2]);
 bool type2_signed_p = TYPE_SIGN (type2) == SIGNED;
	  if (fmt.can_represent_integral_type_p (type1)
 && fmt.can_represent_integral_type_p (type2)
)
	    {
	      if (cmp == ORDERED_EXPR || cmp == UNORDERED_EXPR
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail592;
		  {
		    tree _r;
		    _r =  constant_boolean_node (cmp == ORDERED_EXPR, type);
		    if (TREE_SIDE_EFFECTS (captures[1]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		    if (TREE_SIDE_EFFECTS (captures[2]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 400, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail592:;
		}
	      else
		{
		  if (TYPE_PRECISION (type1) > TYPE_PRECISION (type2)
 && type1_signed_p >= type2_signed_p
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail593;
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
			_r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 401, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail593:;
		    }
		  else
		    {
		      if (TYPE_PRECISION (type1) < TYPE_PRECISION (type2)
 && type1_signed_p <= type2_signed_p
)
			{
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail594;
			  {
			    tree res_op0;
			    {
			      tree _o1[1], _r1;
			      _o1[0] = captures[1];
			      if (TREE_TYPE (_o1[0]) != type2)
				{
				  _r1 = fold_build1_loc (loc, NOP_EXPR, type2, _o1[0]);
				}
			      else
			        _r1 = _o1[0];
			      res_op0 = _r1;
			    }
			    tree res_op1;
			    res_op1 = captures[2];
			    tree _r;
			    _r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 402, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail594:;
			}
		      else
			{
			  if (TYPE_PRECISION (type1) == TYPE_PRECISION (type2)
 && type1_signed_p == type2_signed_p
)
			    {
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail595;
			      {
				tree res_op0;
				res_op0 = captures[1];
				tree res_op1;
				res_op1 = captures[2];
				tree _r;
				_r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 403, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail595:;
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
generic_simplify_254 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail626;
  {
    tree res_op0;
    res_op0 = captures[1];
    tree res_op1;
    res_op1 =  build_zero_cst (TREE_TYPE (captures[1]));
    tree _r;
    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
    if (TREE_SIDE_EFFECTS (captures[0]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 427, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail626:;
  return NULL_TREE;
}

tree
generic_simplify_256 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[2]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_PRECISION (TREE_TYPE (captures[1])) == TYPE_PRECISION (TREE_TYPE (captures[2]))
 && TYPE_PRECISION (TREE_TYPE (captures[0])) > TYPE_PRECISION (TREE_TYPE (captures[1]))
 && !wi::neg_p (wi::to_wide (captures[3]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail628;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[2];
	  {
	    tree _o2[1], _r2;
	    {
	      tree _o3[1], _r3;
	      _o3[0] = captures[3];
	      _r3 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o3[0]), _o3[0]);
	      _o2[0] = _r3;
	    }
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
	res_op1 =  build_zero_cst (TREE_TYPE (captures[2]));
	tree _r;
	_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 429, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail628:;
    }
  return NULL_TREE;
}

tree
generic_simplify_261 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail633;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail633;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 =  build_zero_cst (TREE_TYPE (captures[1]));
    tree _r;
    _r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 434, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail633:;
  return NULL_TREE;
}

tree
generic_simplify_265 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail637;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail637;
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
next_after_fail637:;
    }
  return NULL_TREE;
}

tree
generic_simplify_268 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 poly_int64 off0, off1;
 tree base0, base1;
 int equal = address_compare (cmp, TREE_TYPE (captures[0]), captures[1], captures[2], base0, base1,
 off0, off1,
1
);
      if (equal == 1
)
	{
	  if (cmp == EQ_EXPR && (known_eq (off0, off1) || known_ne (off0, off1))
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail647;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail647;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail647;
	      {
		tree _r;
		_r =  constant_boolean_node (known_eq (off0, off1), type);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 438, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail647:;
	    }
	  else
	    {
	      if (cmp == NE_EXPR && (known_eq (off0, off1) || known_ne (off0, off1))
)
		{
		  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail648;
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail648;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail648;
		  {
		    tree _r;
		    _r =  constant_boolean_node (known_ne (off0, off1), type);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 439, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail648:;
		}
	      else
		{
		  if (cmp == LT_EXPR && (known_lt (off0, off1) || known_ge (off0, off1))
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail649;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail649;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail649;
		      {
			tree _r;
			_r =  constant_boolean_node (known_lt (off0, off1), type);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 440, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail649:;
		    }
		  else
		    {
		      if (cmp == LE_EXPR && (known_le (off0, off1) || known_gt (off0, off1))
)
			{
			  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail650;
			  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail650;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail650;
			  {
			    tree _r;
			    _r =  constant_boolean_node (known_le (off0, off1), type);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 441, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail650:;
			}
		      else
			{
			  if (cmp == GE_EXPR && (known_ge (off0, off1) || known_lt (off0, off1))
)
			    {
			      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail651;
			      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail651;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail651;
			      {
				tree _r;
				_r =  constant_boolean_node (known_ge (off0, off1), type);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 442, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail651:;
			    }
			  else
			    {
			      if (cmp == GT_EXPR && (known_gt (off0, off1) || known_le (off0, off1))
)
				{
				  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail652;
				  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail652;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail652;
				  {
				    tree _r;
				    _r =  constant_boolean_node (known_gt (off0, off1), type);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 443, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail652:;
				}
			    }
			}
		    }
		}
	    }
	}
      else
	{
	  if (equal == 0
)
	    {
	      if (cmp == EQ_EXPR
)
		{
		  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail653;
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail653;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail653;
		  {
		    tree _r;
		    _r =  constant_boolean_node (false, type);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 444, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail653:;
		}
	      else
		{
		  if (cmp == NE_EXPR
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail654;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail654;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail654;
		      {
			tree _r;
			_r =  constant_boolean_node (true, type);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 445, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail654:;
		    }
		}
	    }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_280 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const combined_fn ARG_UNUSED (ffs))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 int prec = TYPE_PRECISION (TREE_TYPE (captures[1]));
      if (integer_zerop (captures[2])
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail683;
	  {
	    tree res_op0;
	    res_op0 = captures[1];
	    tree res_op1;
	    res_op1 =  build_zero_cst (TREE_TYPE (captures[1]));
	    tree _r;
	    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[2]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 474, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail683:;
	}
      else
	{
	  if (tree_int_cst_sgn (captures[2]) < 0 || wi::to_widest (captures[2]) > prec
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail684;
	      {
		tree _r;
		_r =  constant_boolean_node (cmp == NE_EXPR ? true : false, type);
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (TREE_SIDE_EFFECTS (captures[2]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 475, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail684:;
	    }
	  else
	    {
	      if (single_use (captures[0])
)
		{
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail685;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail685;
		  {
		    tree res_op0;
		    {
		      tree _o1[2], _r1;
		      _o1[0] = captures[1];
		      _o1[1] =  wide_int_to_tree (TREE_TYPE (captures[1]),
 wi::mask (tree_to_uhwi (captures[2]),
 false, prec));
		      _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      res_op0 = _r1;
		    }
		    tree res_op1;
		    res_op1 =  wide_int_to_tree (TREE_TYPE (captures[1]),
 wi::shifted_mask (tree_to_uhwi (captures[2]) - 1, 1,
 false, prec));
		    tree _r;
		    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 476, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail685:;
		}
	    }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_295 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (div))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && ssa_name_has_boolean_range (captures[1])
 && !flag_non_call_exceptions
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail703;
      {
	tree _r;
	_r = captures[0];
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 492, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail703:;
    }
  return NULL_TREE;
}

tree
generic_simplify_300 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (div))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_pow2p (captures[2])
 && tree_int_cst_sgn (captures[2]) > 0
 && tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
 && wi::to_wide (captures[2]) + wi::to_wide (captures[1]) == 0
)
    {
      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail709;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail709;
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
	res_op1 =  build_int_cst (integer_type_node,
 wi::exact_log2 (wi::to_wide (captures[2])));
	tree _r;
	_r = fold_build2_loc (loc, RSHIFT_EXPR, type, res_op0, res_op1);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 498, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail709:;
    }
  return NULL_TREE;
}

tree
generic_simplify_305 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (mod))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail714;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail714;
  {
    tree _r;
    _r =  build_zero_cst (type);
    if (TREE_SIDE_EFFECTS (captures[0]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 503, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail714:;
  return NULL_TREE;
}

tree
generic_simplify_313 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (SQRT))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail724;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree _r;
	_r = maybe_build_call_expr_loc (loc, SQRT, type, 1, res_op0);
	if (!_r)
	  goto next_after_fail724;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 513, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail724:;
    }
  return NULL_TREE;
}

tree
generic_simplify_318 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (COS),
 const combined_fn ARG_UNUSED (SIN),
 const combined_fn ARG_UNUSED (TAN))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail729;
      {
	tree res_op0;
	res_op0 =  build_one_cst (type);
	tree res_op1;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[1];
	  _r1 = maybe_build_call_expr_loc (loc, TAN, TREE_TYPE (_o1[0]), 1, _o1[0]);
	  if (!_r1)
	    goto next_after_fail729;
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, RDIV_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 518, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail729:;
    }
  return NULL_TREE;
}

tree
generic_simplify_326 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 && wi::only_sign_bit_p (wi::to_wide (captures[1]))
)
    {
      {
 tree stype = signed_type_for (TREE_TYPE (captures[1]));
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail739;
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
	    tree res_op1;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[2];
	      if (TREE_TYPE (_o1[0]) != stype)
		{
		  _r1 = fold_build1_loc (loc, NOP_EXPR, stype, _o1[0]);
		}
	      else
	        _r1 = _o1[0];
	      res_op1 = _r1;
	    }
	    tree _r;
	    _r = fold_build2_loc (loc, op, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 528, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail739:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_337 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((INTEGRAL_TYPE_P (TREE_TYPE (captures[2]))
 || POINTER_TYPE_P (TREE_TYPE (captures[2]))
 || VECTOR_INTEGER_TYPE_P (TREE_TYPE (captures[2])))
 && tree_nop_conversion_p (TREE_TYPE (captures[0]), TREE_TYPE (captures[1]))
)
    {
      {
 tree cst = uniform_integer_cst_p (captures[2]);
 tree arg1_type = TREE_TYPE (cst);
 unsigned int prec = TYPE_PRECISION (arg1_type);
 wide_int max = wi::max_value (arg1_type);
 wide_int signed_max = wi::max_value (prec, SIGNED);
 wide_int min = wi::min_value (arg1_type);
	  if (wi::to_wide (cst) == max
)
	    {
	      if (cmp == GT_EXPR
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail753;
		  {
		    tree _r;
		    _r =  constant_boolean_node (false, type);
		    if (TREE_SIDE_EFFECTS (captures[1]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		    if (TREE_SIDE_EFFECTS (captures[2]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 542, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail753:;
		}
	      else
		{
		  if (cmp == GE_EXPR
)
		    {
		      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail754;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail754;
		      {
			tree res_op0;
			res_op0 = captures[0];
			tree res_op1;
			res_op1 = captures[2];
			tree _r;
			_r = fold_build2_loc (loc, EQ_EXPR, type, res_op0, res_op1);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 543, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail754:;
		    }
		  else
		    {
		      if (cmp == LE_EXPR
)
			{
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail755;
			  {
			    tree _r;
			    _r =  constant_boolean_node (true, type);
			    if (TREE_SIDE_EFFECTS (captures[1]))
			      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			    if (TREE_SIDE_EFFECTS (captures[2]))
			      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 544, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail755:;
			}
		      else
			{
			  if (cmp == LT_EXPR
)
			    {
			      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail756;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail756;
			      {
				tree res_op0;
				res_op0 = captures[0];
				tree res_op1;
				res_op1 = captures[2];
				tree _r;
				_r = fold_build2_loc (loc, NE_EXPR, type, res_op0, res_op1);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 545, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail756:;
			    }
			}
		    }
		}
	    }
	  else
	    {
	      if (wi::to_wide (cst) == min
)
		{
		  if (cmp == LT_EXPR
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail757;
		      {
			tree _r;
			_r =  constant_boolean_node (false, type);
			if (TREE_SIDE_EFFECTS (captures[1]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			if (TREE_SIDE_EFFECTS (captures[2]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 546, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail757:;
		    }
		  else
		    {
		      if (cmp == LE_EXPR
)
			{
			  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail758;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail758;
			  {
			    tree res_op0;
			    res_op0 = captures[0];
			    tree res_op1;
			    res_op1 = captures[2];
			    tree _r;
			    _r = fold_build2_loc (loc, EQ_EXPR, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 547, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail758:;
			}
		      else
			{
			  if (cmp == GE_EXPR
)
			    {
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail759;
			      {
				tree _r;
				_r =  constant_boolean_node (true, type);
				if (TREE_SIDE_EFFECTS (captures[1]))
				  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
				if (TREE_SIDE_EFFECTS (captures[2]))
				  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 548, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail759:;
			    }
			  else
			    {
			      if (cmp == GT_EXPR
)
				{
				  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail760;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail760;
				  {
				    tree res_op0;
				    res_op0 = captures[0];
				    tree res_op1;
				    res_op1 = captures[2];
				    tree _r;
				    _r = fold_build2_loc (loc, NE_EXPR, type, res_op0, res_op1);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 549, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail760:;
				}
			    }
			}
		    }
		}
	      else
		{
		  if (wi::to_wide (cst) == max - 1
)
		    {
		      if (cmp == GT_EXPR
)
			{
			  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail761;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail761;
			  {
			    tree res_op0;
			    res_op0 = captures[0];
			    tree res_op1;
			    res_op1 =  build_uniform_cst (TREE_TYPE (captures[2]),
 wide_int_to_tree (TREE_TYPE (cst),
 wi::to_wide (cst)
 + 1));
			    tree _r;
			    _r = fold_build2_loc (loc, EQ_EXPR, type, res_op0, res_op1);
			    if (TREE_SIDE_EFFECTS (captures[2]))
			      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 550, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail761:;
			}
		      else
			{
			  if (cmp == LE_EXPR
)
			    {
			      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail762;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail762;
			      {
				tree res_op0;
				res_op0 = captures[0];
				tree res_op1;
				res_op1 =  build_uniform_cst (TREE_TYPE (captures[2]),
 wide_int_to_tree (TREE_TYPE (cst),
 wi::to_wide (cst)
 + 1));
				tree _r;
				_r = fold_build2_loc (loc, NE_EXPR, type, res_op0, res_op1);
				if (TREE_SIDE_EFFECTS (captures[2]))
				  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 551, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail762:;
			    }
			}
		    }
		  else
		    {
		      if (wi::to_wide (cst) == min + 1
)
			{
			  if (cmp == GE_EXPR
)
			    {
			      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail763;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail763;
			      {
				tree res_op0;
				res_op0 = captures[0];
				tree res_op1;
				res_op1 =  build_uniform_cst (TREE_TYPE (captures[2]),
 wide_int_to_tree (TREE_TYPE (cst),
 wi::to_wide (cst)
 - 1));
				tree _r;
				_r = fold_build2_loc (loc, NE_EXPR, type, res_op0, res_op1);
				if (TREE_SIDE_EFFECTS (captures[2]))
				  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 552, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail763:;
			    }
			  else
			    {
			      if (cmp == LT_EXPR
)
				{
				  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail764;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail764;
				  {
				    tree res_op0;
				    res_op0 = captures[0];
				    tree res_op1;
				    res_op1 =  build_uniform_cst (TREE_TYPE (captures[2]),
 wide_int_to_tree (TREE_TYPE (cst),
 wi::to_wide (cst)
 - 1));
				    tree _r;
				    _r = fold_build2_loc (loc, EQ_EXPR, type, res_op0, res_op1);
				    if (TREE_SIDE_EFFECTS (captures[2]))
				      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 553, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail764:;
				}
			    }
			}
		      else
			{
			  if (wi::to_wide (cst) == signed_max
 && TYPE_UNSIGNED (arg1_type)
 && TYPE_MODE (arg1_type) != BLKmode
 && prec == GET_MODE_PRECISION (SCALAR_INT_TYPE_MODE (arg1_type))
 && INTEGRAL_TYPE_P (arg1_type)
)
			    {
			      if (cmp == LE_EXPR || cmp == GT_EXPR
)
				{
				  {
 tree st = signed_type_for (TREE_TYPE (captures[2]));
				      if (cst == captures[2] && cmp == LE_EXPR
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail765;
					  {
					    tree res_op0;
					    {
					      tree _o1[1], _r1;
					      _o1[0] = captures[1];
					      if (TREE_TYPE (_o1[0]) != st)
						{
						  _r1 = fold_build1_loc (loc, NOP_EXPR, st, _o1[0]);
						}
					      else
					        _r1 = _o1[0];
					      res_op0 = _r1;
					    }
					    tree res_op1;
					    res_op1 =  build_zero_cst (st);
					    tree _r;
					    _r = fold_build2_loc (loc, GE_EXPR, type, res_op0, res_op1);
					    if (TREE_SIDE_EFFECTS (captures[2]))
					      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 554, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail765:;
					}
				      else
					{
					  if (cst == captures[2] && cmp == GT_EXPR
)
					    {
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail766;
					      {
						tree res_op0;
						{
						  tree _o1[1], _r1;
						  _o1[0] = captures[1];
						  if (TREE_TYPE (_o1[0]) != st)
						    {
						      _r1 = fold_build1_loc (loc, NOP_EXPR, st, _o1[0]);
						    }
						  else
						    _r1 = _o1[0];
						  res_op0 = _r1;
						}
						tree res_op1;
						res_op1 =  build_zero_cst (st);
						tree _r;
						_r = fold_build2_loc (loc, LT_EXPR, type, res_op0, res_op1);
						if (TREE_SIDE_EFFECTS (captures[2]))
						  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 555, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail766:;
					    }
					  else
					    {
					      if (cmp == LE_EXPR
)
						{
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail767;
						  {
						    tree res_op0;
						    {
						      tree _o1[1], _r1;
						      _o1[0] = captures[1];
						      _r1 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, st, _o1[0]);
						      res_op0 = _r1;
						    }
						    tree res_op1;
						    res_op1 =  build_zero_cst (st);
						    tree _r;
						    _r = fold_build2_loc (loc, GE_EXPR, type, res_op0, res_op1);
						    if (TREE_SIDE_EFFECTS (captures[2]))
						      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
						    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 556, __FILE__, __LINE__, true);
						    return _r;
						  }
next_after_fail767:;
						}
					      else
						{
						  if (cmp == GT_EXPR
)
						    {
						      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail768;
						      {
							tree res_op0;
							{
							  tree _o1[1], _r1;
							  _o1[0] = captures[1];
							  _r1 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, st, _o1[0]);
							  res_op0 = _r1;
							}
							tree res_op1;
							res_op1 =  build_zero_cst (st);
							tree _r;
							_r = fold_build2_loc (loc, LT_EXPR, type, res_op0, res_op1);
							if (TREE_SIDE_EFFECTS (captures[2]))
							  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
							if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 557, __FILE__, __LINE__, true);
							return _r;
						      }
next_after_fail768:;
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
generic_simplify_388 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 bool wascmp;
      if (types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[1]))
 && bitwise_inverted_equal_p (captures[0], captures[1], wascmp)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail865;
	  {
	    tree _r;
	    _r =  wascmp ? constant_boolean_node (false, type) : build_zero_cst (type);
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 619, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail865:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_394 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail871;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail871;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    {
      tree _o1[1], _r1;
      _o1[0] = captures[5];
      _r1 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
      res_op1 = _r1;
    }
    tree _r;
    _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 625, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail871:;
  return NULL_TREE;
}

tree
generic_simplify_399 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree pmop[2];
 tree utype = fold_bit_and_mask (TREE_TYPE (captures[1]), captures[4], NEGATE_EXPR, captures[1],
 bitop, captures[2], captures[3], NULL_TREE, ERROR_MARK,
 NULL_TREE, NULL_TREE, pmop);
      if (utype
)
	{
	  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail876;
	  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail876;
	  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail876;
	  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail876;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail876;
	  {
	    tree res_op0;
	    {
	      tree _o1[2], _r1;
	      {
		tree _o2[1], _r2;
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
		_r2 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
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
	      _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 629, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail876:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_414 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (eqne))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (eqne == EQ_EXPR
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail897;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail897;
      {
	tree _r;
	_r =  constant_boolean_node (false, type);
	if (TREE_SIDE_EFFECTS (captures[2]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 646, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail897:;
    }
  else
    {
      if (eqne == NE_EXPR
)
	{
	  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail898;
	  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail898;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail898;
	  {
	    tree _r;
	    _r = captures[0];
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 647, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail898:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_419 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail922;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail922;
  {
    tree _r;
    _r = captures[2];
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 662, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail922:;
  return NULL_TREE;
}

tree
generic_simplify_421 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail925;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail925;
  {
    tree _r;
    _r = captures[2];
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 663, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail925:;
  return NULL_TREE;
}

tree
generic_simplify_423 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail933;
	      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail933;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail933;
	      {
		tree _r;
		_r = captures[0];
		if (TREE_SIDE_EFFECTS (captures[4]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[4]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 648, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail933:;
	    }
	  else
	    {
	      if (code1 == EQ_EXPR && !val
)
		{
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail934;
		  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail934;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail934;
		  {
		    tree _r;
		    _r =  constant_boolean_node (false, type);
		    if (TREE_SIDE_EFFECTS (captures[4]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[4]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 649, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail934:;
		}
	      else
		{
		  if (code1 == NE_EXPR && !val && allbits
)
		    {
		      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail935;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail935;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail935;
		      {
			tree _r;
			_r = captures[3];
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 650, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail935:;
		    }
		  else
		    {
		      if (code1 == NE_EXPR
 && code2 == GE_EXPR
 && cmp == 0
 && allbits
)
			{
			  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail936;
			  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail936;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail936;
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
next_after_fail936:;
			}
		      else
			{
			  if (code1 == NE_EXPR
 && code2 == LE_EXPR
 && cmp == 0
 && allbits
)
			    {
			      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail937;
			      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail937;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail937;
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
next_after_fail937:;
			    }
			  else
			    {
			      if (code1 == NE_EXPR
 && code2 == GT_EXPR
 && one_after
 && allbits
)
				{
				  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail938;
				  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail938;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail938;
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
next_after_fail938:;
				}
			      else
				{
				  if (code1 == NE_EXPR
 && code2 == LT_EXPR
 && one_before
 && allbits
)
				    {
				      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail939;
				      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail939;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail939;
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
next_after_fail939:;
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
generic_simplify_454 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (fmas))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (canonicalize_math_after_vectorization_p ()
)
    {
      if (single_use (captures[0])
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail977;
	  {
	    tree res_op0;
	    res_op0 = captures[1];
	    tree res_op1;
	    res_op1 = captures[2];
	    tree res_op2;
	    res_op2 = captures[3];
	    tree _r;
	    _r = maybe_build_call_expr_loc (loc, CFN_FNMS, type, 3, res_op0, res_op1, res_op2);
	    if (!_r)
	      goto next_after_fail977;
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 689, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail977:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_464 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (minmax),
 const enum tree_code ARG_UNUSED (maxmin))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail999;
  {
    tree res_op0;
    {
      tree _o1[2], _r1;
      _o1[0] = captures[1];
      _o1[1] = captures[3];
      _r1 = fold_build2_loc (loc, maxmin, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      res_op0 = _r1;
    }
    tree _r;
    _r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 698, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail999:;
  return NULL_TREE;
}

tree
generic_simplify_471 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (FMAX_ALL))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_finite_math_only
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1006;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 = captures[1];
	tree _r;
	_r = fold_build2_loc (loc, MAX_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 705, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1006:;
    }
  return NULL_TREE;
}

tree
generic_simplify_475 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (minmax))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree_code code = minmax_from_comparison (cmp, captures[0], captures[1], captures[0], captures[4]);
      if ((cmp == LT_EXPR || cmp == LE_EXPR)
 && code == MIN_EXPR
 && integer_nonzerop (fold_build2 (LE_EXPR, boolean_type_node, captures[3], captures[4]))
)
	{
	  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1014;
	  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1014;
	  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1014;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1014;
	  {
	    tree res_op0;
	    res_op0 = captures[2];
	    tree res_op1;
	    res_op1 = captures[4];
	    tree _r;
	    _r = fold_build2_loc (loc, MIN_EXPR, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 713, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1014:;
	}
      else
	{
	  if ((cmp == GT_EXPR || cmp == GE_EXPR)
 && code == MAX_EXPR
 && integer_nonzerop (fold_build2 (GE_EXPR, boolean_type_node, captures[3], captures[4]))
)
	    {
	      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1015;
	      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1015;
	      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1015;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1015;
	      {
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		res_op1 = captures[4];
		tree _r;
		_r = fold_build2_loc (loc, MAX_EXPR, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 714, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1015:;
	    }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_488 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1028;
  if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1028;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1028;
  {
    tree _r;
    _r = captures[2];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 727, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1028:;
  return NULL_TREE;
}

tree
generic_simplify_494 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (func))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (wi::to_widest (captures[4]) == TYPE_PRECISION (TREE_TYPE (captures[3])) - 1
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1035;
      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1035;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1035;
      {
	tree _r;
	_r = captures[2];
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 734, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1035:;
    }
  return NULL_TREE;
}

tree
generic_simplify_500 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (TREE_TYPE (captures[0]), type)
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1045;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree _r;
	_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 743, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1045:;
    }
  return NULL_TREE;
}

tree
generic_simplify_507 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1052;
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1052;
  if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1052;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1052;
  {
    tree _r;
    _r = captures[3];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 729, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1052:;
  return NULL_TREE;
}

tree
generic_simplify_513 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (cond_len_op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree op_type = TREE_TYPE (captures[3]);
      if (element_precision (type) == element_precision (op_type)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1058;
	  {
	    tree res_op0;
	    {
	      tree _o1[6], _r1;
	      _o1[0] = captures[0];
	      _o1[1] = captures[1];
	      _o1[2] = captures[2];
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[6];
		_r2 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, op_type, _o2[0]);
		_o1[3] = _r2;
	      }
	      _o1[4] = captures[4];
	      _o1[5] = captures[5];
	      _r1 = maybe_build_call_expr_loc (loc, cond_len_op, TREE_TYPE (_o1[1]), 6, _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
	      if (!_r1)
	        goto next_after_fail1058;
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
	    if (TREE_SIDE_EFFECTS (captures[3]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 751, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1058:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_520 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1065;
  {
    tree res_op0;
    res_op0 = unshare_expr (captures[0]);
    tree res_op1;
    res_op1 = captures[0];
    tree _r;
    _r = fold_build2_loc (loc, UNORDERED_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 758, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1065:;
  return NULL_TREE;
}

tree
generic_simplify_525 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1070;
	  {
	    tree res_op0;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[1];
	      _r1 = maybe_build_call_expr_loc (loc, logs, TREE_TYPE (_o1[0]), 1, _o1[0]);
	      if (!_r1)
	        goto next_after_fail1070;
	      if (EXPR_P (_r1))
	        goto next_after_fail1070;
	      res_op0 = _r1;
	    }
	    tree res_op1;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[2];
	      _r1 = maybe_build_call_expr_loc (loc, logs, TREE_TYPE (_o1[0]), 1, _o1[0]);
	      if (!_r1)
	        goto next_after_fail1070;
	      res_op1 = _r1;
	    }
	    tree _r;
	    _r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 763, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1070:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_538 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (froms),
 const combined_fn ARG_UNUSED (tos))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (optimize && canonicalize_math_p ()
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1083;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree _r;
	_r = maybe_build_call_expr_loc (loc, tos, type, 1, res_op0);
	if (!_r)
	  goto next_after_fail1083;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 776, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1083:;
    }
  return NULL_TREE;
}

tree
generic_simplify_546 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (fmas))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (canonicalize_math_after_vectorization_p ()
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1091;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 = captures[1];
	tree res_op2;
	res_op2 = captures[2];
	tree _r;
	_r = maybe_build_call_expr_loc (loc, CFN_FNMA, type, 3, res_op0, res_op1, res_op2);
	if (!_r)
	  goto next_after_fail1091;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 784, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1091:;
    }
  return NULL_TREE;
}

tree
generic_simplify_555 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (ctz))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree t = TREE_TYPE (captures[0]);
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1100;
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
	  goto next_after_fail1100;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 793, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1100:;
  }
  return NULL_TREE;
}

tree
generic_simplify_564 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (PARITY))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1110;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree _r;
    _r = maybe_build_call_expr_loc (loc, PARITY, type, 1, res_op0);
    if (!_r)
      goto next_after_fail1110;
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 798, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1110:;
  return NULL_TREE;
}

tree
generic_simplify_568 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (rot),
 const combined_fn ARG_UNUSED (parity))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && (
0
 || !TREE_SIDE_EFFECTS (captures[3]))
 && TYPE_PRECISION (TREE_TYPE (captures[0]))
 >= TYPE_PRECISION (TREE_TYPE (captures[1]))
)
    {
      {
 tree type0 = TREE_TYPE (captures[0]);
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1114;
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
	    _r = maybe_build_call_expr_loc (loc, parity, type, 1, res_op0);
	    if (!_r)
	      goto next_after_fail1114;
	    if (TREE_SIDE_EFFECTS (captures[3]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 800, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1114:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_FLOAT_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0)
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
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1178;
	{
	  tree _r;
	  _r = captures[0];
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 815, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1178:;
      }
  }
  switch (TREE_CODE (_p0))
    {
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
	  {
 tree inside_type = TREE_TYPE (captures[1]);
 tree inter_type = TREE_TYPE (captures[0]);
 int inside_int = INTEGRAL_TYPE_P (inside_type);
 int inside_ptr = POINTER_TYPE_P (inside_type);
 int inside_float = FLOAT_TYPE_P (inside_type);
 int inside_vec = VECTOR_TYPE_P (inside_type);
 unsigned int inside_prec = element_precision (inside_type);
 int inside_unsignedp = TYPE_UNSIGNED (inside_type);
 int inter_int = INTEGRAL_TYPE_P (inter_type);
 int inter_ptr = POINTER_TYPE_P (inter_type);
 int inter_float = FLOAT_TYPE_P (inter_type);
 int inter_vec = VECTOR_TYPE_P (inter_type);
 unsigned int inter_prec = element_precision (inter_type);
 int inter_unsignedp = TYPE_UNSIGNED (inter_type);
 int final_int = INTEGRAL_TYPE_P (type);
 int final_ptr = POINTER_TYPE_P (type);
 int final_float = FLOAT_TYPE_P (type);
 int final_vec = VECTOR_TYPE_P (type);
 unsigned int final_prec = element_precision (type);
 int final_unsignedp = TYPE_UNSIGNED (type);
	      if (((
0
 && useless_type_conversion_p (type, inside_type))
 || (
1
 && TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (inside_type)))
 && (((inter_int || inter_ptr) && final_int)
 || (inter_float && final_float))
 && inter_prec >= final_prec
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1179;
		  {
		    tree res_op0;
		    res_op0 = captures[1];
		    tree _r;
		    _r = fold_build1_loc (loc, FLOAT_EXPR, type, res_op0);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 816, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1179:;
		}
	      else
		{
		  if (((inter_int && inside_int) || (inter_float && inside_float))
 && (final_int || final_float)
 && inter_prec >= inside_prec
 && (inter_float || inter_unsignedp == inside_unsignedp)
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1180;
		      {
			tree res_op0;
			res_op0 = captures[1];
			tree _r;
			_r = fold_build1_loc (loc, FLOAT_EXPR, type, res_op0);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 817, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1180:;
		    }
		  else
		    {
		      if (inside_int && inter_int && final_int
 && ((inside_prec < inter_prec && inter_prec < final_prec
 && inside_unsignedp && !inter_unsignedp)
 || final_prec == inter_prec
 || (inside_prec < inter_prec && inter_prec > final_prec
 && !inside_unsignedp && inter_unsignedp))
)
			{
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1181;
			  {
			    tree res_op0;
			    res_op0 = captures[1];
			    tree _r;
			    _r = fold_build1_loc (loc, FLOAT_EXPR, type, res_op0);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 818, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1181:;
			}
		      else
			{
			  if (! inside_float && ! inter_float && ! final_float
 && ! inside_vec && ! inter_vec && ! final_vec
 && (inter_prec >= inside_prec || inter_prec >= final_prec)
 && ! (inside_int && inter_int
 && inter_unsignedp != inside_unsignedp
 && inter_prec < final_prec)
 && ((inter_unsignedp && inter_prec > inside_prec)
 == (final_unsignedp && final_prec > inter_prec))
 && ! (inside_ptr && inter_prec != final_prec)
 && ! (final_ptr && inside_prec != inter_prec)
)
			    {
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1182;
			      {
				tree res_op0;
				res_op0 = captures[1];
				tree _r;
				_r = fold_build1_loc (loc, FLOAT_EXPR, type, res_op0);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 819, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail1182:;
			    }
			  else
			    {
			      if (inside_int && inter_int && final_int
 && final_prec <= inside_prec
 && inter_prec >= inside_prec
)
				{
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1183;
				  {
				    tree res_op0;
				    res_op0 = captures[1];
				    tree _r;
				    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 820, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail1183:;
				}
			      else
				{
				  if (0
 && final_int && inter_int && inside_int
 && final_prec == inside_prec
 && final_prec > inter_prec
 && inter_unsignedp
)
				    {
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1184;
				      {
					tree res_op0;
					{
					  tree _o1[2], _r1;
					  _o1[0] = captures[1];
					  _o1[1] =  wide_int_to_tree
 (inside_type,
 wi::mask (inter_prec, false,
 TYPE_PRECISION (inside_type)));
					  _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					  res_op0 = _r1;
					}
					tree _r;
					_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 821, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail1184:;
				    }
				  else
				    {
				      if (0
 && inside_int && inter_float && final_int &&
 (unsigned) significand_size (TYPE_MODE (inter_type))
 >= inside_prec - !inside_unsignedp
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1185;
					  {
					    tree res_op0;
					    res_op0 = captures[1];
					    tree _r;
					    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 822, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1185:;
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
    case FLOAT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
	  {
 tree inside_type = TREE_TYPE (captures[1]);
 tree inter_type = TREE_TYPE (captures[0]);
 int inside_int = INTEGRAL_TYPE_P (inside_type);
 int inside_ptr = POINTER_TYPE_P (inside_type);
 int inside_float = FLOAT_TYPE_P (inside_type);
 int inside_vec = VECTOR_TYPE_P (inside_type);
 unsigned int inside_prec = element_precision (inside_type);
 int inside_unsignedp = TYPE_UNSIGNED (inside_type);
 int inter_int = INTEGRAL_TYPE_P (inter_type);
 int inter_ptr = POINTER_TYPE_P (inter_type);
 int inter_float = FLOAT_TYPE_P (inter_type);
 int inter_vec = VECTOR_TYPE_P (inter_type);
 unsigned int inter_prec = element_precision (inter_type);
 int inter_unsignedp = TYPE_UNSIGNED (inter_type);
 int final_int = INTEGRAL_TYPE_P (type);
 int final_ptr = POINTER_TYPE_P (type);
 int final_float = FLOAT_TYPE_P (type);
 int final_vec = VECTOR_TYPE_P (type);
 unsigned int final_prec = element_precision (type);
 int final_unsignedp = TYPE_UNSIGNED (type);
	      if (((
0
 && useless_type_conversion_p (type, inside_type))
 || (
1
 && TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (inside_type)))
 && (((inter_int || inter_ptr) && final_int)
 || (inter_float && final_float))
 && inter_prec >= final_prec
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1186;
		  {
		    tree res_op0;
		    res_op0 = captures[1];
		    tree _r;
		    _r = fold_build1_loc (loc, FLOAT_EXPR, type, res_op0);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 816, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1186:;
		}
	      else
		{
		  if (((inter_int && inside_int) || (inter_float && inside_float))
 && (final_int || final_float)
 && inter_prec >= inside_prec
 && (inter_float || inter_unsignedp == inside_unsignedp)
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1187;
		      {
			tree res_op0;
			res_op0 = captures[1];
			tree _r;
			_r = fold_build1_loc (loc, FLOAT_EXPR, type, res_op0);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 817, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1187:;
		    }
		  else
		    {
		      if (inside_int && inter_int && final_int
 && ((inside_prec < inter_prec && inter_prec < final_prec
 && inside_unsignedp && !inter_unsignedp)
 || final_prec == inter_prec
 || (inside_prec < inter_prec && inter_prec > final_prec
 && !inside_unsignedp && inter_unsignedp))
)
			{
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1188;
			  {
			    tree res_op0;
			    res_op0 = captures[1];
			    tree _r;
			    _r = fold_build1_loc (loc, FLOAT_EXPR, type, res_op0);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 818, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1188:;
			}
		      else
			{
			  if (! inside_float && ! inter_float && ! final_float
 && ! inside_vec && ! inter_vec && ! final_vec
 && (inter_prec >= inside_prec || inter_prec >= final_prec)
 && ! (inside_int && inter_int
 && inter_unsignedp != inside_unsignedp
 && inter_prec < final_prec)
 && ((inter_unsignedp && inter_prec > inside_prec)
 == (final_unsignedp && final_prec > inter_prec))
 && ! (inside_ptr && inter_prec != final_prec)
 && ! (final_ptr && inside_prec != inter_prec)
)
			    {
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1189;
			      {
				tree res_op0;
				res_op0 = captures[1];
				tree _r;
				_r = fold_build1_loc (loc, FLOAT_EXPR, type, res_op0);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 819, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail1189:;
			    }
			  else
			    {
			      if (inside_int && inter_int && final_int
 && final_prec <= inside_prec
 && inter_prec >= inside_prec
)
				{
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1190;
				  {
				    tree res_op0;
				    res_op0 = captures[1];
				    tree _r;
				    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 820, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail1190:;
				}
			      else
				{
				  if (0
 && final_int && inter_int && inside_int
 && final_prec == inside_prec
 && final_prec > inter_prec
 && inter_unsignedp
)
				    {
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1191;
				      {
					tree res_op0;
					{
					  tree _o1[2], _r1;
					  _o1[0] = captures[1];
					  _o1[1] =  wide_int_to_tree
 (inside_type,
 wi::mask (inter_prec, false,
 TYPE_PRECISION (inside_type)));
					  _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					  res_op0 = _r1;
					}
					tree _r;
					_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 821, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail1191:;
				    }
				  else
				    {
				      if (0
 && inside_int && inter_float && final_int &&
 (unsigned) significand_size (TYPE_MODE (inter_type))
 >= inside_prec - !inside_unsignedp
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1192;
					  {
					    tree res_op0;
					    res_op0 = captures[1];
					    tree _r;
					    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 822, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1192:;
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
  return NULL_TREE;
}

tree
generic_simplify_MINUS_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1260;
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1260;
	{
	  tree res_op0;
	  res_op0 = captures[0];
	  tree _r;
	  _r = non_lvalue_loc (loc, res_op0);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 862, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1260:;
      }
    }
  if (real_zerop (_p1))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (fold_real_zero_addition_p (type, captures[0], captures[1], 1)
)
	  {
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1261;
	    {
	      tree res_op0;
	      res_op0 = captures[0];
	      tree _r;
	      _r = non_lvalue_loc (loc, res_op0);
	      if (TREE_SIDE_EFFECTS (captures[1]))
		_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 878, __FILE__, __LINE__, true);
	      return _r;
	    }
next_after_fail1261:;
	  }
      }
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
		      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
		      tree res = generic_simplify_3 (loc, type, _p0, _p1, captures, MINUS_EXPR, PLUS_EXPR);
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
		      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
		      tree res = generic_simplify_3 (loc, type, _p0, _p1, captures, MINUS_EXPR, MINUS_EXPR);
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
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (!FLOAT_TYPE_P (type)
 || (!tree_expr_maybe_nan_p (captures[0])
 && !tree_expr_maybe_infinite_p (captures[0])
 && (!HONOR_SIGN_DEPENDENT_ROUNDING (type)
 || !HONOR_SIGNED_ZEROS (type)))
)
	  {
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1262;
	    {
	      tree _r;
	      _r =  build_zero_cst (type);
	      if (TREE_SIDE_EFFECTS (captures[0]))
		_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 879, __FILE__, __LINE__, true);
	      return _r;
	    }
next_after_fail1262:;
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
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      switch (TREE_CODE (_q40))
	        {
		case MULT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_q50))
		      {
		      case TRUNC_DIV_EXPR:
		        {
			  tree _q60 = TREE_OPERAND (_q50, 0);
			  tree _q61 = TREE_OPERAND (_q50, 1);
			  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || operand_equal_p (_q60, _q20, 0))
			    {
			      if ((_q51 == _q61 && ! TREE_SIDE_EFFECTS (_q51)) || operand_equal_p (_q51, _q61, 0))
				{
				  {
				    tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q61, _q20, _q61 };
				    tree res = generic_simplify_38 (loc, type, _p0, _p1, captures);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q51))
		      {
		      case TRUNC_DIV_EXPR:
		        {
			  tree _q70 = TREE_OPERAND (_q51, 0);
			  tree _q71 = TREE_OPERAND (_q51, 1);
			  if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || operand_equal_p (_q70, _q20, 0))
			    {
			      if ((_q71 == _q50 && ! TREE_SIDE_EFFECTS (_q71)) || operand_equal_p (_q71, _q50, 0))
				{
				  {
				    tree captures[4] ATTRIBUTE_UNUSED = { _q70, _q71, _q20, _q50 };
				    tree res = generic_simplify_38 (loc, type, _p0, _p1, captures);
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
	  case MULT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      tree _q41 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q40))
	        {
		case TRUNC_DIV_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q20, 0))
		      {
			if ((_q41 == _q51 && ! TREE_SIDE_EFFECTS (_q41)) || operand_equal_p (_q41, _q51, 0))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q50, _q51, _q20, _q51 };
			      tree res = generic_simplify_38 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q41))
	        {
		case TRUNC_DIV_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q41, 0);
		    tree _q61 = TREE_OPERAND (_q41, 1);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || operand_equal_p (_q60, _q20, 0))
		      {
			if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || operand_equal_p (_q61, _q40, 0))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q61, _q20, _q40 };
			      tree res = generic_simplify_38 (loc, type, _p0, _p1, captures);
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
	tree _q30 = TREE_OPERAND (_p1, 0);
	switch (TREE_CODE (_q30))
	  {
	  case MULT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      switch (TREE_CODE (_q40))
	        {
		case TRUNC_DIV_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _p0, 0))
		      {
			if ((_q41 == _q51 && ! TREE_SIDE_EFFECTS (_q41)) || operand_equal_p (_q41, _q51, 0))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q50, _q51, _p0, _q51 };
			      tree res = generic_simplify_38 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q41))
	        {
		case TRUNC_DIV_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q41, 0);
		    tree _q61 = TREE_OPERAND (_q41, 1);
		    if ((_q60 == _p0 && ! TREE_SIDE_EFFECTS (_q60)) || operand_equal_p (_q60, _p0, 0))
		      {
			if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || operand_equal_p (_q61, _q40, 0))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q61, _p0, _q40 };
			      tree res = generic_simplify_38 (loc, type, _p0, _p1, captures);
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
    case MULT_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	switch (TREE_CODE (_q30))
	  {
	  case TRUNC_DIV_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || operand_equal_p (_q40, _p0, 0))
		{
		  if ((_q31 == _q41 && ! TREE_SIDE_EFFECTS (_q31)) || operand_equal_p (_q31, _q41, 0))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q41, _p0, _q41 };
			tree res = generic_simplify_38 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q31))
	  {
	  case TRUNC_DIV_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_q31, 0);
	      tree _q51 = TREE_OPERAND (_q31, 1);
	      if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _p0, 0))
		{
		  if ((_q51 == _q30 && ! TREE_SIDE_EFFECTS (_q51)) || operand_equal_p (_q51, _q30, 0))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _q50, _q51, _p0, _q30 };
			tree res = generic_simplify_38 (loc, type, _p0, _p1, captures);
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
    case MULT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case PLUS_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_q40))
	        {
		case TRUNC_DIV_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		      {
			if (integer_onep (_q41))
			  {
			    if ((_p1 == _q50 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q50, 0) && types_match (_p1, _q50)))
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q40, _q50 };
				  tree res = generic_simplify_39 (loc, type, _p0, _p1, captures);
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
	switch (TREE_CODE (_q20))
	  {
	  case PLUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_q30))
	        {
		case TRUNC_DIV_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    if (integer_onep (_q31))
		      {
			if ((_q21 == _q41 && ! TREE_SIDE_EFFECTS (_q21)) || (operand_equal_p (_q21, _q41, 0) && types_match (_q21, _q41)))
			  {
			    if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
			      {
				{
				  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q41, _q20, _q30, _q40 };
				  tree res = generic_simplify_39 (loc, type, _p0, _p1, captures);
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
		    tree res = generic_simplify_5 (loc, type, _p0, _p1, captures, MINUS_EXPR);
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
	switch (TREE_CODE (_q21))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q40, _p1 };
			      tree res = generic_simplify_40 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
		      {
			if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q40, _p1 };
			      tree res = generic_simplify_40 (loc, type, _p0, _p1, captures);
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
		case BIT_AND_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q30, _p1 };
			      tree res = generic_simplify_40 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q30, _p1 };
			      tree res = generic_simplify_40 (loc, type, _p0, _p1, captures);
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
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
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
				if (~wi::to_wide (captures[2]) == wi::to_wide (captures[4])
)
				  {
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1263;
				    {
				      if (! tree_invariant_p (captures[4])) goto next_after_fail1263;
				      tree res_op0;
				      {
					tree _o1[2], _r1;
					_o1[0] = captures[1];
					_o1[1] = unshare_expr (captures[4]);
					_r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					res_op0 = _r1;
				      }
				      tree res_op1;
				      res_op1 = captures[4];
				      tree _r;
				      _r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
				      if (TREE_SIDE_EFFECTS (captures[2]))
					_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 880, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1263:;
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
	switch (TREE_CODE (_p1))
	  {
	  case BIT_AND_EXPR:
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
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
			      tree res = generic_simplify_41 (loc, type, _p0, _p1, captures);
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
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
			      tree res = generic_simplify_41 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1 };
			      tree res = generic_simplify_41 (loc, type, _p0, _p1, captures);
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
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1 };
			      tree res = generic_simplify_41 (loc, type, _p0, _p1, captures);
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
	  case PLUS_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q50))
	        {
		case BIT_IOR_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    tree _q61 = TREE_OPERAND (_q50, 1);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
			  {
			    if (integer_onep (_q51))
			      {
				{
				  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				  tree res = generic_simplify_42 (loc, type, _p0, _p1, captures);
				  if (res) return res;
				}
			      }
			  }
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			  {
			    if (integer_onep (_q51))
			      {
				{
				  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				  tree res = generic_simplify_42 (loc, type, _p0, _p1, captures);
				  if (res) return res;
				}
			      }
			  }
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
		    case BIT_IOR_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_q60, 0);
			tree _q71 = TREE_OPERAND (_q60, 1);
			if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
			  {
			    if ((_q71 == _q21 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q21, 0) && types_match (_q71, _q21)))
			      {
				if (integer_onep (_q51))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				      tree res = generic_simplify_42 (loc, type, _p0, _p1, captures);
				      if (res) return res;
				    }
				  }
			      }
			  }
			if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
			  {
			    if ((_q71 == _q20 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q20, 0) && types_match (_q71, _q20)))
			      {
				if (integer_onep (_q51))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				      tree res = generic_simplify_42 (loc, type, _p0, _p1, captures);
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
          default:;
          }
      {
	tree _p1_pops[1];
	if (tree_nop_convert (_p1, _p1_pops))
	  {
	    tree _q50 = _p1_pops[0];
	    switch (TREE_CODE (_q50))
	      {
	      case PLUS_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_q50, 0);
		  tree _q61 = TREE_OPERAND (_q50, 1);
		  switch (TREE_CODE (_q60))
		    {
		    case BIT_IOR_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_q60, 0);
			tree _q71 = TREE_OPERAND (_q60, 1);
			if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
			  {
			    if ((_q71 == _q21 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q21, 0) && types_match (_q71, _q21)))
			      {
				if (integer_onep (_q61))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
				      tree res = generic_simplify_42 (loc, type, _p0, _p1, captures);
				      if (res) return res;
				    }
				  }
			      }
			  }
			if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
			  {
			    if ((_q71 == _q20 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q20, 0) && types_match (_q71, _q20)))
			      {
				if (integer_onep (_q61))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
				      tree res = generic_simplify_42 (loc, type, _p0, _p1, captures);
				      if (res) return res;
				    }
				  }
			      }
			  }
		        break;
		      }
		    default:;
		    }
		{
		  tree _q60_pops[1];
		  if (tree_nop_convert (_q60, _q60_pops))
		    {
		      tree _q70 = _q60_pops[0];
		      switch (TREE_CODE (_q70))
		        {
			case BIT_IOR_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_q70, 0);
			    tree _q81 = TREE_OPERAND (_q70, 1);
			    if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
			      {
				if ((_q81 == _q21 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q21, 0) && types_match (_q81, _q21)))
				  {
				    if (integer_onep (_q61))
				      {
					{
					  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
					  tree res = generic_simplify_42 (loc, type, _p0, _p1, captures);
					  if (res) return res;
					}
				      }
				  }
			      }
			    if ((_q80 == _q21 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q21, 0) && types_match (_q80, _q21)))
			      {
				if ((_q81 == _q20 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q20, 0) && types_match (_q81, _q20)))
				  {
				    if (integer_onep (_q61))
				      {
					{
					  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
					  tree res = generic_simplify_42 (loc, type, _p0, _p1, captures);
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
	      default:;
	      }
	  }
      }
        break;
      }
    case BIT_NOT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_p1))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
		if (!TYPE_OVERFLOW_SANITIZED (type)
)
		  {
		    {
 tree utype = unsigned_type_for (type);
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1264;
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
			      _o2[0] = captures[0];
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
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 881, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1264:;
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
			if (!TYPE_OVERFLOW_SANITIZED (type) && !TYPE_OVERFLOW_TRAPS (type)
 && !TYPE_SATURATING (type)
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1265;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 882, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1265:;
			  }
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
			if (!TYPE_OVERFLOW_SANITIZED (type) && !TYPE_OVERFLOW_TRAPS (type)
 && !TYPE_SATURATING (type)
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1266;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, BIT_IOR_EXPR, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 883, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1266:;
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
	  case BIT_AND_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_all_onesp (_q21))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case BIT_IOR_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_p1, 0);
			tree _q71 = TREE_OPERAND (_p1, 1);
			if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
			  {
			    if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
			      {
				{
				  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q31 };
				  tree res = generic_simplify_43 (loc, type, _p0, _p1, captures);
				  if (res) return res;
				}
			      }
			  }
			if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
			  {
			    if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
			      {
				{
				  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q31, _q30 };
				  tree res = generic_simplify_43 (loc, type, _p0, _p1, captures);
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
		      tree _q70 = _p1_pops[0];
		      switch (TREE_CODE (_q70))
		        {
			case BIT_IOR_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_q70, 0);
			    tree _q81 = TREE_OPERAND (_q70, 1);
			    if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
			      {
				if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q31 };
				      tree res = generic_simplify_43 (loc, type, _p0, _p1, captures);
				      if (res) return res;
				    }
				  }
			      }
			    if ((_q80 == _q31 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q31, 0) && types_match (_q80, _q31)))
			      {
				if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q31, _q30 };
				      tree res = generic_simplify_43 (loc, type, _p0, _p1, captures);
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
	      case BIT_AND_EXPR:
	        {
		  tree _q40 = TREE_OPERAND (_q30, 0);
		  tree _q41 = TREE_OPERAND (_q30, 1);
		  if (integer_all_onesp (_q21))
		    {
		      switch (TREE_CODE (_p1))
		        {
			case BIT_IOR_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_p1, 0);
			    tree _q81 = TREE_OPERAND (_p1, 1);
			    if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
			      {
				if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q40, _q41 };
				      tree res = generic_simplify_43 (loc, type, _p0, _p1, captures);
				      if (res) return res;
				    }
				  }
			      }
			    if ((_q80 == _q41 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q41, 0) && types_match (_q80, _q41)))
			      {
				if ((_q81 == _q40 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q40, 0) && types_match (_q81, _q40)))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q41, _q40 };
				      tree res = generic_simplify_43 (loc, type, _p0, _p1, captures);
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
			  tree _q80 = _p1_pops[0];
			  switch (TREE_CODE (_q80))
			    {
			    case BIT_IOR_EXPR:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				tree _q91 = TREE_OPERAND (_q80, 1);
				if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
				  {
				    if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				      {
					{
					  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q40, _q41 };
					  tree res = generic_simplify_43 (loc, type, _p0, _p1, captures);
					  if (res) return res;
					}
				      }
				  }
				if ((_q90 == _q41 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q41, 0) && types_match (_q90, _q41)))
				  {
				    if ((_q91 == _q40 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q40, 0) && types_match (_q91, _q40)))
				      {
					{
					  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q41, _q40 };
					  tree res = generic_simplify_43 (loc, type, _p0, _p1, captures);
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
		    }
	          break;
	        }
	      default:;
	      }
	  }
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
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_44 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
	      tree res = generic_simplify_44 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	switch (TREE_CODE (_p1))
	  {
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1267;
			{
			  tree res_op0;
			  res_op0 = captures[0];
			  tree res_op1;
			  res_op1 = captures[1];
			  tree _r;
			  _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 884, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1267:;
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1268;
			{
			  tree res_op0;
			  res_op0 = captures[0];
			  tree res_op1;
			  res_op1 = captures[1];
			  tree _r;
			  _r = fold_build2_loc (loc, BIT_XOR_EXPR, type, res_op0, res_op1);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 885, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1268:;
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
  if (tree_nop_convert (_p0, _p0_pops))
    {
      tree _q20 = _p0_pops[0];
      switch (TREE_CODE (_q20))
        {
	case PLUS_EXPR:
	  {
	    tree _q30 = TREE_OPERAND (_q20, 0);
	    tree _q31 = TREE_OPERAND (_q20, 1);
	    switch (TREE_CODE (_q30))
	      {
	      case BIT_AND_EXPR:
	        {
		  tree _q40 = TREE_OPERAND (_q30, 0);
		  tree _q41 = TREE_OPERAND (_q30, 1);
		  if (integer_all_onesp (_q31))
		    {
		      switch (TREE_CODE (_p1))
		        {
			case BIT_IOR_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_p1, 0);
			    tree _q81 = TREE_OPERAND (_p1, 1);
			    if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
			      {
				if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q40, _q41 };
				      tree res = generic_simplify_43 (loc, type, _p0, _p1, captures);
				      if (res) return res;
				    }
				  }
			      }
			    if ((_q80 == _q41 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q41, 0) && types_match (_q80, _q41)))
			      {
				if ((_q81 == _q40 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q40, 0) && types_match (_q81, _q40)))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q41, _q40 };
				      tree res = generic_simplify_43 (loc, type, _p0, _p1, captures);
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
			  tree _q80 = _p1_pops[0];
			  switch (TREE_CODE (_q80))
			    {
			    case BIT_IOR_EXPR:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				tree _q91 = TREE_OPERAND (_q80, 1);
				if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
				  {
				    if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				      {
					{
					  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q40, _q41 };
					  tree res = generic_simplify_43 (loc, type, _p0, _p1, captures);
					  if (res) return res;
					}
				      }
				  }
				if ((_q90 == _q41 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q41, 0) && types_match (_q90, _q41)))
				  {
				    if ((_q91 == _q40 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q40, 0) && types_match (_q91, _q40)))
				      {
					{
					  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q41, _q40 };
					  tree res = generic_simplify_43 (loc, type, _p0, _p1, captures);
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
		  case BIT_AND_EXPR:
		    {
		      tree _q50 = TREE_OPERAND (_q40, 0);
		      tree _q51 = TREE_OPERAND (_q40, 1);
		      if (integer_all_onesp (_q31))
			{
			  switch (TREE_CODE (_p1))
			    {
			    case BIT_IOR_EXPR:
			      {
				tree _q90 = TREE_OPERAND (_p1, 0);
				tree _q91 = TREE_OPERAND (_p1, 1);
				if ((_q90 == _q50 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q50, 0) && types_match (_q90, _q50)))
				  {
				    if ((_q91 == _q51 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q51, 0) && types_match (_q91, _q51)))
				      {
					{
					  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q50, _q51 };
					  tree res = generic_simplify_43 (loc, type, _p0, _p1, captures);
					  if (res) return res;
					}
				      }
				  }
				if ((_q90 == _q51 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q51, 0) && types_match (_q90, _q51)))
				  {
				    if ((_q91 == _q50 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q50, 0) && types_match (_q91, _q50)))
				      {
					{
					  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q51, _q50 };
					  tree res = generic_simplify_43 (loc, type, _p0, _p1, captures);
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
			      tree _q90 = _p1_pops[0];
			      switch (TREE_CODE (_q90))
			        {
				case BIT_IOR_EXPR:
				  {
				    tree _q100 = TREE_OPERAND (_q90, 0);
				    tree _q101 = TREE_OPERAND (_q90, 1);
				    if ((_q100 == _q50 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q50, 0) && types_match (_q100, _q50)))
				      {
					if ((_q101 == _q51 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q51, 0) && types_match (_q101, _q51)))
					  {
					    {
					      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q50, _q51 };
					      tree res = generic_simplify_43 (loc, type, _p0, _p1, captures);
					      if (res) return res;
					    }
					  }
				      }
				    if ((_q100 == _q51 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q51, 0) && types_match (_q100, _q51)))
				      {
					if ((_q101 == _q50 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q50, 0) && types_match (_q101, _q50)))
					  {
					    {
					      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q51, _q50 };
					      tree res = generic_simplify_43 (loc, type, _p0, _p1, captures);
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
		        }
		      break;
		    }
	          default:;
	          }
	      }
	  }
	    break;
	  }
	case BIT_AND_EXPR:
	  {
	    tree _q30 = TREE_OPERAND (_q20, 0);
	    tree _q31 = TREE_OPERAND (_q20, 1);
	    switch (TREE_CODE (_p1))
	      {
	      case PLUS_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p1, 0);
		  tree _q61 = TREE_OPERAND (_p1, 1);
		  switch (TREE_CODE (_q60))
		    {
		    case BIT_IOR_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_q60, 0);
			tree _q71 = TREE_OPERAND (_q60, 1);
			if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
			  {
			    if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
			      {
				if (integer_onep (_q61))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
				      tree res = generic_simplify_42 (loc, type, _p0, _p1, captures);
				      if (res) return res;
				    }
				  }
			      }
			  }
			if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
			  {
			    if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
			      {
				if (integer_onep (_q61))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
				      tree res = generic_simplify_42 (loc, type, _p0, _p1, captures);
				      if (res) return res;
				    }
				  }
			      }
			  }
		        break;
		      }
		    default:;
		    }
		{
		  tree _q60_pops[1];
		  if (tree_nop_convert (_q60, _q60_pops))
		    {
		      tree _q70 = _q60_pops[0];
		      switch (TREE_CODE (_q70))
		        {
			case BIT_IOR_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_q70, 0);
			    tree _q81 = TREE_OPERAND (_q70, 1);
			    if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
			      {
				if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
				  {
				    if (integer_onep (_q61))
				      {
					{
					  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
					  tree res = generic_simplify_42 (loc, type, _p0, _p1, captures);
					  if (res) return res;
					}
				      }
				  }
			      }
			    if ((_q80 == _q31 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q31, 0) && types_match (_q80, _q31)))
			      {
				if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
				  {
				    if (integer_onep (_q61))
				      {
					{
					  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
					  tree res = generic_simplify_42 (loc, type, _p0, _p1, captures);
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
	      default:;
	      }
	  {
	    tree _p1_pops[1];
	    if (tree_nop_convert (_p1, _p1_pops))
	      {
		tree _q60 = _p1_pops[0];
		switch (TREE_CODE (_q60))
		  {
		  case PLUS_EXPR:
		    {
		      tree _q70 = TREE_OPERAND (_q60, 0);
		      tree _q71 = TREE_OPERAND (_q60, 1);
		      switch (TREE_CODE (_q70))
		        {
			case BIT_IOR_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_q70, 0);
			    tree _q81 = TREE_OPERAND (_q70, 1);
			    if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
			      {
				if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
				  {
				    if (integer_onep (_q71))
				      {
					{
					  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q60 };
					  tree res = generic_simplify_42 (loc, type, _p0, _p1, captures);
					  if (res) return res;
					}
				      }
				  }
			      }
			    if ((_q80 == _q31 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q31, 0) && types_match (_q80, _q31)))
			      {
				if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
				  {
				    if (integer_onep (_q71))
				      {
					{
					  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q60 };
					  tree res = generic_simplify_42 (loc, type, _p0, _p1, captures);
					  if (res) return res;
					}
				      }
				  }
			      }
			    break;
			  }
		        default:;
		        }
		    {
		      tree _q70_pops[1];
		      if (tree_nop_convert (_q70, _q70_pops))
		        {
			  tree _q80 = _q70_pops[0];
			  switch (TREE_CODE (_q80))
			    {
			    case BIT_IOR_EXPR:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				tree _q91 = TREE_OPERAND (_q80, 1);
				if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
				  {
				    if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				      {
					if (integer_onep (_q71))
					  {
					    {
					      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q60 };
					      tree res = generic_simplify_42 (loc, type, _p0, _p1, captures);
					      if (res) return res;
					    }
					  }
				      }
				  }
				if ((_q90 == _q31 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q31, 0) && types_match (_q90, _q31)))
				  {
				    if ((_q91 == _q30 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q30, 0) && types_match (_q91, _q30)))
				      {
					if (integer_onep (_q71))
					  {
					    {
					      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q60 };
					      tree res = generic_simplify_42 (loc, type, _p0, _p1, captures);
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
  if (tree_negate_expr_p (_p1))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (!FIXED_POINT_TYPE_P (type)
)
	  {
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1269;
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
	      _r = fold_build2_loc (loc, PLUS_EXPR, type, res_op0, res_op1);
	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 886, __FILE__, __LINE__, true);
	      return _r;
	    }
next_after_fail1269:;
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
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      switch (TREE_CODE (_q40))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q20, 0))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _q50, _p1, _q40, _q51, _q20 };
			  tree res = generic_simplify_45 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || operand_equal_p (_q51, _q20, 0))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _q51, _p1, _q40, _q50, _q20 };
			  tree res = generic_simplify_45 (loc, type, _p0, _p1, captures);
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
	      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || operand_equal_p (_q40, _q20, 0))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q40, _p1, _p1, _q41, _q20 };
		    tree res = generic_simplify_46 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q41 == _q20 && ! TREE_SIDE_EFFECTS (_q41)) || operand_equal_p (_q41, _q20, 0))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q41, _p1, _p1, _q40, _q20 };
		    tree res = generic_simplify_46 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
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
		CASE_CONVERT:
		  {
		    tree _q40 = TREE_OPERAND (_p1, 0);
		    switch (TREE_CODE (_q40))
		      {
		      case POINTER_PLUS_EXPR:
		        {
			  tree _q50 = TREE_OPERAND (_q40, 0);
			  tree _q51 = TREE_OPERAND (_q40, 1);
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q50, _q51 };
			    if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
			      {
				{
 poly_int64 diff;
				    if (ptr_difference_const (captures[0], captures[1], &diff)
)
				      {
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1270;
					{
					  tree res_op0;
					  res_op0 =  build_int_cst_type (type, diff);
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
					  if (TREE_SIDE_EFFECTS (captures[0]))
					    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
					  if (TREE_SIDE_EFFECTS (captures[1]))
					    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 887, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1270:;
				      }
				}
			      }
			  }
		          break;
		        }
		      default:;
		      }
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
		      if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
			{
			  {
 poly_int64 diff;
			      if (ptr_difference_const (captures[0], captures[1], &diff)
)
				{
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1271;
				  {
				    tree _r;
				    _r =  build_int_cst_type (type, diff);
				    if (TREE_SIDE_EFFECTS (captures[0]))
				      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
				    if (TREE_SIDE_EFFECTS (captures[1]))
				      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 888, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail1271:;
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
	  case POINTER_PLUS_EXPR:
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
		      case ADDR_EXPR:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q60 };
			    if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
			      {
				{
 poly_int64 diff;
				    if (ptr_difference_const (captures[0], captures[2], &diff)
)
				      {
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1272;
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
					  res_op1 =  build_int_cst_type (type, diff);
					  tree _r;
					  _r = fold_build2_loc (loc, PLUS_EXPR, type, res_op0, res_op1);
					  if (TREE_SIDE_EFFECTS (captures[0]))
					    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
					  if (TREE_SIDE_EFFECTS (captures[2]))
					    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 889, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1272:;
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
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
		      if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
			{
			  {
 poly_int64 diff;
			      if (ptr_difference_const (captures[0], captures[1], &diff)
)
				{
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1273;
				  {
				    tree _r;
				    _r =  build_int_cst_type (type, diff);
				    if (TREE_SIDE_EFFECTS (captures[0]))
				      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
				    if (TREE_SIDE_EFFECTS (captures[1]))
				      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 890, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail1273:;
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
if (integer_onep (_p0))
  {
    switch (TREE_CODE (_p1))
      {
      case SSA_NAME:
        {
	  {
	    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	    if (INTEGRAL_TYPE_P (type)
 && ssa_name_has_boolean_range (captures[1])
)
	      {
		if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1274;
		{
		  tree res_op0;
		  res_op0 = captures[1];
		  tree res_op1;
		  res_op1 = captures[0];
		  tree _r;
		  _r = fold_build2_loc (loc, BIT_XOR_EXPR, type, res_op0, res_op1);
		  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 891, __FILE__, __LINE__, true);
		  return _r;
		}
next_after_fail1274:;
	      }
	  }
          break;
        }
      default:;
      }
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
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || operand_equal_p (_q40, _p0, 0))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q40, _p1, _q30, _q41, _p0 };
		    tree res = generic_simplify_45 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || operand_equal_p (_q41, _p0, 0))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q41, _p1, _q30, _q40, _p0 };
		    tree res = generic_simplify_45 (loc, type, _p0, _p1, captures);
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
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || operand_equal_p (_q30, _p0, 0))
	  {
	    {
	      tree captures[5] ATTRIBUTE_UNUSED = { _q30, _p1, _p1, _q31, _p0 };
	      tree res = generic_simplify_46 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || operand_equal_p (_q31, _p0, 0))
	  {
	    {
	      tree captures[5] ATTRIBUTE_UNUSED = { _q31, _p1, _p1, _q30, _p0 };
	      tree res = generic_simplify_46 (loc, type, _p0, _p1, captures);
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1275;
			{
			  tree res_op0;
			  res_op0 = captures[0];
			  tree res_op1;
			  res_op1 = captures[2];
			  tree _r;
			  _r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
			  if (TREE_SIDE_EFFECTS (captures[1]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 892, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1275:;
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1276;
			{
			  tree res_op0;
			  res_op0 = captures[0];
			  tree res_op1;
			  res_op1 = captures[2];
			  tree _r;
			  _r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
			  if (TREE_SIDE_EFFECTS (captures[1]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 892, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1276:;
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1277;
			{
			  tree res_op0;
			  res_op0 = captures[0];
			  tree res_op1;
			  res_op1 = captures[2];
			  tree _r;
			  _r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
			  if (TREE_SIDE_EFFECTS (captures[1]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 892, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1277:;
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1278;
			{
			  tree res_op0;
			  res_op0 = captures[0];
			  tree res_op1;
			  res_op1 = captures[2];
			  tree _r;
			  _r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
			  if (TREE_SIDE_EFFECTS (captures[1]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 892, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1278:;
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1279;
			{
			  tree res_op0;
			  res_op0 = captures[0];
			  tree res_op1;
			  res_op1 = captures[2];
			  tree _r;
			  _r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
			  if (TREE_SIDE_EFFECTS (captures[1]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 893, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1279:;
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
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1280;
			{
			  tree res_op0;
			  res_op0 = captures[2];
			  tree res_op1;
			  res_op1 = captures[1];
			  tree _r;
			  _r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
			  if (TREE_SIDE_EFFECTS (captures[0]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 894, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1280:;
		      }
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
		    if (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))
 && !TYPE_OVERFLOW_SANITIZED (TREE_TYPE (captures[2]))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1281;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  res_op1 = captures[3];
			  tree _r;
			  _r = fold_build2_loc (loc, POINTER_DIFF_EXPR, type, res_op0, res_op1);
			  if (TREE_SIDE_EFFECTS (captures[2]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 895, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1281:;
		      }
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q51 };
		    if (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))
 && !TYPE_OVERFLOW_SANITIZED (TREE_TYPE (captures[1]))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1282;
			{
			  tree res_op0;
			  res_op0 = captures[3];
			  tree res_op1;
			  res_op1 = captures[2];
			  tree _r;
			  _r = fold_build2_loc (loc, POINTER_DIFF_EXPR, type, res_op0, res_op1);
			  if (TREE_SIDE_EFFECTS (captures[1]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 896, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1282:;
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
	tree _q30 = TREE_OPERAND (_p1, 0);
	switch (TREE_CODE (_q30))
	  {
	  case NEGATE_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q40 };
		tree res = generic_simplify_47 (loc, type, _p0, _p1, captures);
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
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	  tree res = generic_simplify_47 (loc, type, _p0, _p1, captures);
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
	{
	  tree _q20_pops[1];
	  if (tree_nop_convert (_q20, _q20_pops))
	    {
	      tree _q30 = _q20_pops[0];
	      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q21 };
		    tree res = generic_simplify_48 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	    }
	}
	{
	  tree _q21_pops[1];
	  if (tree_nop_convert (_q21, _q21_pops))
	    {
	      tree _q40 = _q21_pops[0];
	      if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
		    tree res = generic_simplify_48 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	    }
	}
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_48 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_48 (loc, type, _p0, _p1, captures);
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
	  tree _q20_pops[1];
	  if (tree_nop_convert (_q20, _q20_pops))
	    {
	      tree _q30 = _q20_pops[0];
	      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q21 };
		    tree res = generic_simplify_49 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	    }
	}
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_49 (loc, type, _p0, _p1, captures);
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
	case PLUS_EXPR:
	  {
	    tree _q30 = TREE_OPERAND (_q20, 0);
	    tree _q31 = TREE_OPERAND (_q20, 1);
	    {
	      tree _q30_pops[1];
	      if (tree_nop_convert (_q30, _q30_pops))
	        {
		  tree _q40 = _q30_pops[0];
		  if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q31 };
			tree res = generic_simplify_48 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	    }
	    {
	      tree _q31_pops[1];
	      if (tree_nop_convert (_q31, _q31_pops))
	        {
		  tree _q50 = _q31_pops[0];
		  if ((_p1 == _q50 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q50, 0) && types_match (_p1, _q50)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q50, _q30 };
			tree res = generic_simplify_48 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	    }
	    if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
		  tree res = generic_simplify_48 (loc, type, _p0, _p1, captures);
		  if (res) return res;
		}
	      }
	    if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _q31, _q30 };
		  tree res = generic_simplify_48 (loc, type, _p0, _p1, captures);
		  if (res) return res;
		}
	      }
	    break;
	  }
	case MINUS_EXPR:
	  {
	    tree _q30 = TREE_OPERAND (_q20, 0);
	    tree _q31 = TREE_OPERAND (_q20, 1);
	    {
	      tree _q30_pops[1];
	      if (tree_nop_convert (_q30, _q30_pops))
	        {
		  tree _q40 = _q30_pops[0];
		  if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q31 };
			tree res = generic_simplify_49 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	    }
	    if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
		  tree res = generic_simplify_49 (loc, type, _p0, _p1, captures);
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
    case PLUS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	{
	  tree _q30_pops[1];
	  if (tree_nop_convert (_q30, _q30_pops))
	    {
	      tree _q40 = _q30_pops[0];
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
		    tree res = generic_simplify_50 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	    }
	}
	{
	  tree _q31_pops[1];
	  if (tree_nop_convert (_q31, _q31_pops))
	    {
	      tree _q50 = _q31_pops[0];
	      if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
		    tree res = generic_simplify_50 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	    }
	}
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
	      tree res = generic_simplify_50 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	      tree res = generic_simplify_50 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
        break;
      }
    case MINUS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	{
	  tree _q30_pops[1];
	  if (tree_nop_convert (_q30, _q30_pops))
	    {
	      tree _q40 = _q30_pops[0];
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
		    tree res = generic_simplify_51 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	    }
	}
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
	      tree res = generic_simplify_51 (loc, type, _p0, _p1, captures);
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
	    {
	      tree _q40_pops[1];
	      if (tree_nop_convert (_q40, _q40_pops))
	        {
		  tree _q50 = _q40_pops[0];
		  if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q41 };
			tree res = generic_simplify_50 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	    }
	    {
	      tree _q41_pops[1];
	      if (tree_nop_convert (_q41, _q41_pops))
	        {
		  tree _q60 = _q41_pops[0];
		  if ((_q60 == _p0 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p0, 0) && types_match (_q60, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q40 };
			tree res = generic_simplify_50 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	    }
	    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q41 };
		  tree res = generic_simplify_50 (loc, type, _p0, _p1, captures);
		  if (res) return res;
		}
	      }
	    if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q40 };
		  tree res = generic_simplify_50 (loc, type, _p0, _p1, captures);
		  if (res) return res;
		}
	      }
	    break;
	  }
	case MINUS_EXPR:
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
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q41 };
			tree res = generic_simplify_51 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	    }
	    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q41 };
		  tree res = generic_simplify_51 (loc, type, _p0, _p1, captures);
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
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
		    tree res = generic_simplify_52 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
		    tree res = generic_simplify_52 (loc, type, _p0, _p1, captures);
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
		  tree res = generic_simplify_16 (loc, type, _p0, _p1, captures, PLUS_EXPR, MINUS_EXPR, MINUS_EXPR);
		  if (res) return res;
		}
	      }
	  }
        break;
      }
    case MINUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (CONSTANT_CLASS_P (_q21))
	  {
	    if (CONSTANT_CLASS_P (_p1))
	      {
		{
		  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		  tree res = generic_simplify_16 (loc, type, _p0, _p1, captures, MINUS_EXPR, PLUS_EXPR, MINUS_EXPR);
		  if (res) return res;
		}
	      }
	  }
	if (CONSTANT_CLASS_P (_q20))
	  {
	    if (CONSTANT_CLASS_P (_p1))
	      {
		{
		  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		  tree res = generic_simplify_17 (loc, type, _p0, _p1, captures, MINUS_EXPR);
		  if (res) return res;
		}
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
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
		tree res = generic_simplify_21 (loc, type, _p0, _p1, captures, MINUS_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q20))
	  {
	  case NEGATE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      if (integer_each_onep (_p1))
		{
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
		    tree res = generic_simplify_54 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case PLUS_EXPR:
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
			  tree res = generic_simplify_55 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q31 && ! TREE_SIDE_EFFECTS (_q60)) || operand_equal_p (_q60, _q31, 0))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q31, _q30, _q31 };
			  tree res = generic_simplify_55 (loc, type, _p0, _p1, captures);
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
		    tree res = generic_simplify_55 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q31, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q31, _q30, _q31 };
		    tree res = generic_simplify_55 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case POINTER_PLUS_EXPR:
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
			  if (!TYPE_SATURATING (type)
)
			    {
			      if ((!FLOAT_TYPE_P (type) || flag_associative_math)
 && !FIXED_POINT_TYPE_P (type)
)
				{
				  if (element_precision (type) <= element_precision (TREE_TYPE (captures[1]))
 || (POINTER_TYPE_P (TREE_TYPE (captures[0]))
 && TREE_CODE (captures[1]) == INTEGER_CST
 && tree_int_cst_sign_bit (captures[1]) == 0)
)
				    {
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1283;
				      {
					tree res_op0;
					res_op0 = captures[1];
					tree _r;
					_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
					if (TREE_SIDE_EFFECTS (captures[2]))
					  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 897, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail1283:;
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
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      switch (TREE_CODE (_q40))
	        {
		case PLUS_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q20, 0))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q51, _q20 };
			  tree res = generic_simplify_56 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || operand_equal_p (_q51, _q20, 0))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q51, _q50, _q20 };
			  tree res = generic_simplify_56 (loc, type, _p0, _p1, captures);
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
    case NEGATE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	if (integer_each_onep (_p1))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	      tree res = generic_simplify_54 (loc, type, _p0, _p1, captures);
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
	case PLUS_EXPR:
	  {
	    tree _q30 = TREE_OPERAND (_q20, 0);
	    tree _q31 = TREE_OPERAND (_q20, 1);
	    if (CONSTANT_CLASS_P (_q31))
	      {
		if (CONSTANT_CLASS_P (_p1))
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
		      tree res = generic_simplify_16 (loc, type, _p0, _p1, captures, PLUS_EXPR, MINUS_EXPR, MINUS_EXPR);
		      if (res) return res;
		    }
		  }
	      }
	    break;
	  }
	case MINUS_EXPR:
	  {
	    tree _q30 = TREE_OPERAND (_q20, 0);
	    tree _q31 = TREE_OPERAND (_q20, 1);
	    if (CONSTANT_CLASS_P (_q31))
	      {
		if (CONSTANT_CLASS_P (_p1))
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
		      tree res = generic_simplify_16 (loc, type, _p0, _p1, captures, MINUS_EXPR, PLUS_EXPR, MINUS_EXPR);
		      if (res) return res;
		    }
		  }
	      }
	    if (CONSTANT_CLASS_P (_q30))
	      {
		if (CONSTANT_CLASS_P (_p1))
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
		      tree res = generic_simplify_17 (loc, type, _p0, _p1, captures, MINUS_EXPR);
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
if (CONSTANT_CLASS_P (_p0))
  {
    switch (TREE_CODE (_p1))
      {
      case MINUS_EXPR:
        {
	  tree _q30 = TREE_OPERAND (_p1, 0);
	  tree _q31 = TREE_OPERAND (_p1, 1);
	  if (CONSTANT_CLASS_P (_q30))
	    {
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q31 };
		tree res = generic_simplify_53 (loc, type, _p0, _p1, captures);
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
	  case MINUS_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if (CONSTANT_CLASS_P (_q40))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q40, _q41 };
		    tree res = generic_simplify_53 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
      }
  }
  }
if (integer_all_onesp (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p1 };
      if (!TYPE_SATURATING (type)
)
	{
	  if ((!FLOAT_TYPE_P (type) || flag_associative_math)
 && !FIXED_POINT_TYPE_P (type)
)
	    {
	      if (TREE_CODE (type) != COMPLEX_TYPE
)
		{
		  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1284;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1284;
		  {
		    tree res_op0;
		    res_op0 = captures[0];
		    tree _r;
		    _r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 898, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1284:;
		}
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
	  case PLUS_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || operand_equal_p (_q40, _p0, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _p0 };
		    tree res = generic_simplify_56 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || operand_equal_p (_q41, _p0, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _p0 };
		    tree res = generic_simplify_56 (loc, type, _p0, _p1, captures);
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
	switch (TREE_CODE (_p1))
	  {
	  CASE_CONVERT:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      switch (TREE_CODE (_q40))
	        {
		case POINTER_PLUS_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q20, 0))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q51, _q20 };
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
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1285;
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
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 899, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1285:;
				      }
				    }
				  else
				    {
				      if (element_precision (type) <= element_precision (TREE_TYPE (captures[1]))
 || (POINTER_TYPE_P (TREE_TYPE (captures[0]))
 && TREE_CODE (captures[1]) == INTEGER_CST
 && tree_int_cst_sign_bit (captures[1]) == 0)
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1286;
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
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 900, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1286:;
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
	switch (TREE_CODE (_q20))
	  {
	  case PLUS_EXPR:
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
		      case PLUS_EXPR:
		        {
			  tree _q70 = TREE_OPERAND (_q60, 0);
			  tree _q71 = TREE_OPERAND (_q60, 1);
			  if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || operand_equal_p (_q70, _q30, 0))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q71, _q30 };
				tree res = generic_simplify_57 (loc, type, _p0, _p1, captures);
				if (res) return res;
			      }
			    }
			  if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || operand_equal_p (_q71, _q30, 0))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q70, _q30 };
				tree res = generic_simplify_57 (loc, type, _p0, _p1, captures);
				if (res) return res;
			      }
			    }
			  if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || operand_equal_p (_q70, _q31, 0))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q71, _q31 };
				tree res = generic_simplify_57 (loc, type, _p0, _p1, captures);
				if (res) return res;
			      }
			    }
			  if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || operand_equal_p (_q71, _q31, 0))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q70, _q31 };
				tree res = generic_simplify_57 (loc, type, _p0, _p1, captures);
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
	  case POINTER_PLUS_EXPR:
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
		      case POINTER_PLUS_EXPR:
		        {
			  tree _q70 = TREE_OPERAND (_q60, 0);
			  tree _q71 = TREE_OPERAND (_q60, 1);
			  if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || operand_equal_p (_q70, _q30, 0))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q71, _q30 };
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
)
					  {
					    {
 tree utype = unsigned_type_for (type);
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1287;
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
						  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 901, __FILE__, __LINE__, true);
						  return _r;
						}
next_after_fail1287:;
					    }
					  }
					else
					  {
					    if (element_precision (type) <= element_precision (TREE_TYPE (captures[1]))
 || (POINTER_TYPE_P (TREE_TYPE (captures[0]))
 && TREE_CODE (captures[1]) == INTEGER_CST
 && tree_int_cst_sign_bit (captures[1]) == 0
 && TREE_CODE (captures[2]) == INTEGER_CST
 && tree_int_cst_sign_bit (captures[2]) == 0)
)
					      {
						if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1288;
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
						  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 902, __FILE__, __LINE__, true);
						  return _r;
						}
next_after_fail1288:;
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
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_24 (loc, type, _p0, _p1, captures, MINUS_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_24 (loc, type, _p0, _p1, captures, MINUS_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_24 (loc, type, _p0, _p1, captures, MINUS_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_24 (loc, type, _p0, _p1, captures, MINUS_EXPR);
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
    case MULT_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q31 };
	      tree res = generic_simplify_25 (loc, type, _p0, _p1, captures, MINUS_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
	      tree res = generic_simplify_25 (loc, type, _p0, _p1, captures, MINUS_EXPR);
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
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_26 (loc, type, _p0, _p1, captures, MINUS_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
	      tree res = generic_simplify_26 (loc, type, _p0, _p1, captures, MINUS_EXPR);
	      if (res) return res;
	    }
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
	switch (TREE_CODE (_q30))
	  {
	  case MINUS_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  if (tree_zero_one_valued_p (_q31))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q41, _q31 };
			tree res = generic_simplify_58 (loc, type, _p0, _p1, captures);
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
	    case MINUS_EXPR:
	      {
		tree _q50 = TREE_OPERAND (_q31, 0);
		tree _q51 = TREE_OPERAND (_q31, 1);
		if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q51, _q30 };
		      tree res = generic_simplify_58 (loc, type, _p0, _p1, captures);
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
 && (TREE_CODE_CLASS (MINUS_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1289;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1289;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1289;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1289:;
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
 && (TREE_CODE_CLASS (MINUS_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1290;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1290;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1290;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1290;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1290:;
	    }
	}
        break;
      }
    default:;
    }
if (integer_zerop (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p1 };
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1291;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1291;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree _r;
	_r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 903, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1291:;
    }
  }
if (real_zerop (_p0))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (fold_real_zero_addition_p (type, captures[1], captures[0], 0)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1292;
	  {
	    tree res_op0;
	    res_op0 = captures[1];
	    tree _r;
	    _r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 904, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1292:;
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
 && (TREE_CODE_CLASS (MINUS_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1293;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1293;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1293;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1293;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1293:;
	    }
	}
	if (integer_each_onep (_q31))
	  {
	    if (integer_zerop (_q32))
	      {
		{
		  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32 };
		  tree res = generic_simplify_59 (loc, type, _p0, _p1, captures);
		  if (res) return res;
		}
	      }
	  }
        break;
      }
    case VIEW_CONVERT_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	switch (TREE_CODE (_q30))
	  {
	  case VEC_COND_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      tree _q42 = TREE_OPERAND (_q30, 2);
	      if (integer_each_onep (_q41))
		{
		  if (integer_zerop (_q42))
		    {
		      {
			tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q30, _q40, _q41, _q42 };
			tree res = generic_simplify_59 (loc, type, _p0, _p1, captures);
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
    case RDIV_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case RDIV_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
		    tree res = generic_simplify_31 (loc, type, _p0, _p1, captures, MINUS_EXPR);
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
	switch (TREE_CODE (_q20))
	  {
	  case CALL_EXPR:
	    switch (get_call_combined_fn (_q20))
	      {
	      case CFN_BUILT_IN_POPCOUNT:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    switch (TREE_CODE (_q21))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_q21))
		          {
			  case CFN_BUILT_IN_POPCOUNT:
			    if (call_expr_nargs (_q21) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_q21, 0);
				switch (TREE_CODE (_p1))
				  {
				  case CALL_EXPR:
				    switch (get_call_combined_fn (_p1))
				      {
				      case CFN_BUILT_IN_POPCOUNT:
				        if (call_expr_nargs (_p1) == 1)
    {
					    tree _q70 = CALL_EXPR_ARG (_p1, 0);
					    switch (TREE_CODE (_q70))
					      {
					      case BIT_AND_EXPR:
					        {
						  tree _q80 = TREE_OPERAND (_q70, 0);
						  tree _q81 = TREE_OPERAND (_q70, 1);
						  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
						    {
						      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
							    tree res = generic_simplify_61 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNT);
							    if (res) return res;
							  }
						        }
						    }
						  if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
						    {
						      if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
							    tree res = generic_simplify_61 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNT);
							    if (res) return res;
							  }
						        }
						    }
					          break;
					        }
					      case BIT_IOR_EXPR:
					        {
						  tree _q80 = TREE_OPERAND (_q70, 0);
						  tree _q81 = TREE_OPERAND (_q70, 1);
						  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
						    {
						      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
							    tree res = generic_simplify_61 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNT);
							    if (res) return res;
							  }
						        }
						    }
						  if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
						    {
						      if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
							    tree res = generic_simplify_61 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNT);
							    if (res) return res;
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
			  default:;
		          }
		        break;
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_POPCOUNTLL:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    switch (TREE_CODE (_q21))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_q21))
		          {
			  case CFN_BUILT_IN_POPCOUNTLL:
			    if (call_expr_nargs (_q21) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_q21, 0);
				switch (TREE_CODE (_p1))
				  {
				  case CALL_EXPR:
				    switch (get_call_combined_fn (_p1))
				      {
				      case CFN_BUILT_IN_POPCOUNTLL:
				        if (call_expr_nargs (_p1) == 1)
    {
					    tree _q70 = CALL_EXPR_ARG (_p1, 0);
					    switch (TREE_CODE (_q70))
					      {
					      case BIT_AND_EXPR:
					        {
						  tree _q80 = TREE_OPERAND (_q70, 0);
						  tree _q81 = TREE_OPERAND (_q70, 1);
						  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
						    {
						      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
							    tree res = generic_simplify_61 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTLL);
							    if (res) return res;
							  }
						        }
						    }
						  if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
						    {
						      if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
							    tree res = generic_simplify_61 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTLL);
							    if (res) return res;
							  }
						        }
						    }
					          break;
					        }
					      case BIT_IOR_EXPR:
					        {
						  tree _q80 = TREE_OPERAND (_q70, 0);
						  tree _q81 = TREE_OPERAND (_q70, 1);
						  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
						    {
						      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
							    tree res = generic_simplify_61 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTLL);
							    if (res) return res;
							  }
						        }
						    }
						  if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
						    {
						      if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
							    tree res = generic_simplify_61 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTLL);
							    if (res) return res;
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
			  default:;
		          }
		        break;
		      default:;
		      }
	          }
	        break;
	      case CFN_POPCOUNT:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    switch (TREE_CODE (_q21))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_q21))
		          {
			  case CFN_POPCOUNT:
			    if (call_expr_nargs (_q21) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_q21, 0);
				switch (TREE_CODE (_p1))
				  {
				  case CALL_EXPR:
				    switch (get_call_combined_fn (_p1))
				      {
				      case CFN_POPCOUNT:
				        if (call_expr_nargs (_p1) == 1)
    {
					    tree _q70 = CALL_EXPR_ARG (_p1, 0);
					    switch (TREE_CODE (_q70))
					      {
					      case BIT_AND_EXPR:
					        {
						  tree _q80 = TREE_OPERAND (_q70, 0);
						  tree _q81 = TREE_OPERAND (_q70, 1);
						  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
						    {
						      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
							    tree res = generic_simplify_61 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_POPCOUNT);
							    if (res) return res;
							  }
						        }
						    }
						  if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
						    {
						      if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
							    tree res = generic_simplify_61 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_POPCOUNT);
							    if (res) return res;
							  }
						        }
						    }
					          break;
					        }
					      case BIT_IOR_EXPR:
					        {
						  tree _q80 = TREE_OPERAND (_q70, 0);
						  tree _q81 = TREE_OPERAND (_q70, 1);
						  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
						    {
						      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
							    tree res = generic_simplify_61 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_POPCOUNT);
							    if (res) return res;
							  }
						        }
						    }
						  if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
						    {
						      if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
							    tree res = generic_simplify_61 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_POPCOUNT);
							    if (res) return res;
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
			  default:;
		          }
		        break;
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_POPCOUNTIMAX:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    switch (TREE_CODE (_q21))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_q21))
		          {
			  case CFN_BUILT_IN_POPCOUNTIMAX:
			    if (call_expr_nargs (_q21) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_q21, 0);
				switch (TREE_CODE (_p1))
				  {
				  case CALL_EXPR:
				    switch (get_call_combined_fn (_p1))
				      {
				      case CFN_BUILT_IN_POPCOUNTIMAX:
				        if (call_expr_nargs (_p1) == 1)
    {
					    tree _q70 = CALL_EXPR_ARG (_p1, 0);
					    switch (TREE_CODE (_q70))
					      {
					      case BIT_AND_EXPR:
					        {
						  tree _q80 = TREE_OPERAND (_q70, 0);
						  tree _q81 = TREE_OPERAND (_q70, 1);
						  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
						    {
						      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
							    tree res = generic_simplify_61 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTIMAX);
							    if (res) return res;
							  }
						        }
						    }
						  if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
						    {
						      if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
							    tree res = generic_simplify_61 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTIMAX);
							    if (res) return res;
							  }
						        }
						    }
					          break;
					        }
					      case BIT_IOR_EXPR:
					        {
						  tree _q80 = TREE_OPERAND (_q70, 0);
						  tree _q81 = TREE_OPERAND (_q70, 1);
						  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
						    {
						      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
							    tree res = generic_simplify_61 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTIMAX);
							    if (res) return res;
							  }
						        }
						    }
						  if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
						    {
						      if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
							    tree res = generic_simplify_61 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTIMAX);
							    if (res) return res;
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
			  default:;
		          }
		        break;
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_POPCOUNTL:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    switch (TREE_CODE (_q21))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_q21))
		          {
			  case CFN_BUILT_IN_POPCOUNTL:
			    if (call_expr_nargs (_q21) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_q21, 0);
				switch (TREE_CODE (_p1))
				  {
				  case CALL_EXPR:
				    switch (get_call_combined_fn (_p1))
				      {
				      case CFN_BUILT_IN_POPCOUNTL:
				        if (call_expr_nargs (_p1) == 1)
    {
					    tree _q70 = CALL_EXPR_ARG (_p1, 0);
					    switch (TREE_CODE (_q70))
					      {
					      case BIT_AND_EXPR:
					        {
						  tree _q80 = TREE_OPERAND (_q70, 0);
						  tree _q81 = TREE_OPERAND (_q70, 1);
						  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
						    {
						      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
							    tree res = generic_simplify_61 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTL);
							    if (res) return res;
							  }
						        }
						    }
						  if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
						    {
						      if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
							    tree res = generic_simplify_61 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTL);
							    if (res) return res;
							  }
						        }
						    }
					          break;
					        }
					      case BIT_IOR_EXPR:
					        {
						  tree _q80 = TREE_OPERAND (_q70, 0);
						  tree _q81 = TREE_OPERAND (_q70, 1);
						  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
						    {
						      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
							    tree res = generic_simplify_61 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTL);
							    if (res) return res;
							  }
						        }
						    }
						  if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
						    {
						      if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _p1, _q70 };
							    tree res = generic_simplify_61 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTL);
							    if (res) return res;
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
        break;
      }
    case VEC_PERM_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	tree _q22 = TREE_OPERAND (_p0, 2);
	switch (TREE_CODE (_q20))
	  {
	  case MULT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (tree_vec_same_elem_p (_q31))
		{
		  if ((_q21 == _q20 && ! TREE_SIDE_EFFECTS (_q21)) || (operand_equal_p (_q21, _q20, 0) && types_match (_q21, _q20)))
		    {
		      {
			tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q22, _p1 };
			tree res = generic_simplify_62 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	      if (tree_vec_same_elem_p (_q30))
		{
		  if ((_q21 == _q20 && ! TREE_SIDE_EFFECTS (_q21)) || (operand_equal_p (_q21, _q20, 0) && types_match (_q21, _q20)))
		    {
		      {
			tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q30, _q22, _p1 };
			tree res = generic_simplify_62 (loc, type, _p0, _p1, captures);
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
    case CALL_EXPR:
      switch (get_call_combined_fn (_p0))
        {
	case CFN_BUILT_IN_LOG:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_LOG:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_60 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_LOG);
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
	case CFN_BUILT_IN_LOG2:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_LOG2:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_60 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_LOG2);
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
	case CFN_BUILT_IN_LOGF:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_LOGF:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_60 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_LOGF);
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
	case CFN_BUILT_IN_LOGL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_LOGL:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_60 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_LOGL);
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
	case CFN_BUILT_IN_LOG10F:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_LOG10F:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_60 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_LOG10F);
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
	case CFN_BUILT_IN_LOG10L:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_LOG10L:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_60 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_LOG10L);
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
	case CFN_LOG:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_LOG:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_60 (loc, type, _p0, _p1, captures, CFN_LOG);
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
	case CFN_LOG2:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_LOG2:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_60 (loc, type, _p0, _p1, captures, CFN_LOG2);
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
	case CFN_LOG10:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_LOG10:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_60 (loc, type, _p0, _p1, captures, CFN_LOG10);
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
	case CFN_BUILT_IN_LOG10:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_LOG10:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_60 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_LOG10);
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
	case CFN_BUILT_IN_LOG2F:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_LOG2F:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_60 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_LOG2F);
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
	case CFN_BUILT_IN_LOG2L:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_LOG2L:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_60 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_LOG2L);
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
    case BIT_AND_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	switch (TREE_CODE (_q30))
	  {
	  case MAX_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  switch (TREE_CODE (_q41))
		    {
		    case INTEGER_CST:
		      {
			switch (TREE_CODE (_q31))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q41, _q31 };
				{
 auto i = wi::neg (wi::to_wide (captures[2]));
				    if (wi::popcount (i) == 1
 && (wi::to_wide (captures[1])) == (i - 1)
)
				      {
					if (TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
					  {
					    if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1294;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1294;
					    {
					      tree res_op0;
					      res_op0 = captures[0];
					      tree res_op1;
					      res_op1 = captures[1];
					      tree _r;
					      _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
					      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 905, __FILE__, __LINE__, true);
					      return _r;
					    }
next_after_fail1294:;
					  }
					else
					  {
					    if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1295;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1295;
					    {
					      if (! tree_invariant_p (captures[0])) goto next_after_fail1295;
					      if (! tree_invariant_p (captures[1])) goto next_after_fail1295;
					      tree res_op0;
					      {
						tree _o1[2], _r1;
						_o1[0] = unshare_expr (captures[0]);
						_o1[1] = unshare_expr (captures[1]);
						_r1 = fold_build2_loc (loc, LE_EXPR, boolean_type_node, _o1[0], _o1[1]);
						res_op0 = _r1;
					      }
					      tree res_op1;
					      res_op1 = unshare_expr (captures[0]);
					      tree res_op2;
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[0];
						_o1[1] = captures[1];
						_r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						res_op2 = _r1;
					      }
					      tree _r;
					      _r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
					      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 906, __FILE__, __LINE__, true);
					      return _r;
					    }
next_after_fail1295:;
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
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1296;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[0];
				    _o1[1] = captures[2];
				    _r1 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
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
next_after_fail1296:;
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
					tree res = generic_simplify_37 (loc, type, _p0, _p1, captures, MINUS_EXPR);
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
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_RSHIFT_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p1))
    {
    case TRUNC_MOD_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	{
	  tree _q31_pops[1];
	  if (tree_power_of_two_cand (_q31, _q31_pops))
	    {
	      tree _q50 = _q31_pops[0];
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q30, _q31, _q50 };
		tree res = generic_simplify_358 (loc, type, _p0, _p1, captures, RSHIFT_EXPR, TRUNC_MOD_EXPR);
		if (res) return res;
	      }
	    }
	}
        break;
      }
    case FLOOR_MOD_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	{
	  tree _q31_pops[1];
	  if (tree_power_of_two_cand (_q31, _q31_pops))
	    {
	      tree _q50 = _q31_pops[0];
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q30, _q31, _q50 };
		tree res = generic_simplify_358 (loc, type, _p0, _p1, captures, RSHIFT_EXPR, FLOOR_MOD_EXPR);
		if (res) return res;
	      }
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
      tree res = generic_simplify_359 (loc, type, _p0, _p1, captures, RSHIFT_EXPR);
      if (res) return res;
    }
  }
  switch (TREE_CODE (_p0))
    {
    case NEGATE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_p1))
	  {
	  case INTEGER_CST:
	    {
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		if (!TYPE_UNSIGNED (type)
 && TYPE_OVERFLOW_UNDEFINED (type)
)
		  {
		    {
 tree stype = TREE_TYPE (captures[2]);
 tree bt = truth_type_for (type);
 tree zeros = build_zero_cst (type);
 tree INTEGER_CST = NULL_TREE;
			if (INTEGRAL_TYPE_P (type)
 && canonicalize_math_after_vectorization_p ()
 && wi::eq_p (wi::to_wide (captures[2]), TYPE_PRECISION (type) - 1)
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1706;
			    {
			      tree res_op0;
			      {
				tree _o1[1], _r1;
				{
				  tree _o2[2], _r2;
				  _o2[0] = captures[1];
				  _o2[1] =  zeros;
				  _r2 = fold_build2_loc (loc, GT_EXPR, boolean_type_node, _o2[0], _o2[1]);
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
			      _r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
			      if (TREE_SIDE_EFFECTS (captures[2]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 980, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1706:;
			  }
			else
			  {
			    if (VECTOR_INTEGER_TYPE_P (type)
 && TYPE_MODE (bt) == TYPE_MODE (type)
 && expand_vec_cmp_expr_p (type, bt, GT_EXPR)
 && (INTEGER_CST = uniform_integer_cst_p (captures[2])) != NULL
 && wi::eq_p (wi::to_wide (INTEGER_CST), element_precision (type) - 1)
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1707;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] =  zeros;
				    _r1 = fold_build2_loc (loc, GT_EXPR, bt, _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
				  if (TREE_SIDE_EFFECTS (captures[2]))
				    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 981, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1707:;
			      }
			  }
		    }
		  }
	      }
	      break;
	    }
	  case VECTOR_CST:
	    {
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		if (!TYPE_UNSIGNED (type)
 && TYPE_OVERFLOW_UNDEFINED (type)
)
		  {
		    {
 tree stype = TREE_TYPE (captures[2]);
 tree bt = truth_type_for (type);
 tree zeros = build_zero_cst (type);
 tree VECTOR_CST = NULL_TREE;
			if (INTEGRAL_TYPE_P (type)
 && canonicalize_math_after_vectorization_p ()
 && wi::eq_p (wi::to_wide (captures[2]), TYPE_PRECISION (type) - 1)
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1708;
			    {
			      tree res_op0;
			      {
				tree _o1[1], _r1;
				{
				  tree _o2[2], _r2;
				  _o2[0] = captures[1];
				  _o2[1] =  zeros;
				  _r2 = fold_build2_loc (loc, GT_EXPR, boolean_type_node, _o2[0], _o2[1]);
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
			      _r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
			      if (TREE_SIDE_EFFECTS (captures[2]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 980, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1708:;
			  }
			else
			  {
			    if (VECTOR_INTEGER_TYPE_P (type)
 && TYPE_MODE (bt) == TYPE_MODE (type)
 && expand_vec_cmp_expr_p (type, bt, GT_EXPR)
 && (VECTOR_CST = uniform_integer_cst_p (captures[2])) != NULL
 && wi::eq_p (wi::to_wide (VECTOR_CST), element_precision (type) - 1)
)
			      {
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1709;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[1];
				    _o1[1] =  zeros;
				    _r1 = fold_build2_loc (loc, GT_EXPR, bt, _o1[0], _o1[1]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
				  if (TREE_SIDE_EFFECTS (captures[2]))
				    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 981, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1709:;
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
    case LSHIFT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q21, 0))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _q21 };
		    tree res = generic_simplify_368 (loc, type, _p0, _p1, captures);
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
	case LSHIFT_EXPR:
	  {
	    tree _q30 = TREE_OPERAND (_q20, 0);
	    tree _q31 = TREE_OPERAND (_q20, 1);
	    switch (TREE_CODE (_q31))
	      {
	      case INTEGER_CST:
	        {
		  if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q31, 0))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q30, _p1, _q31 };
			tree res = generic_simplify_368 (loc, type, _p0, _p1, captures);
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
    }
}
if (integer_all_onesp (_p0))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (!TYPE_UNSIGNED (type)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1710;
	  {
	    tree _r;
	    _r = captures[0];
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 982, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1710:;
	}
    }
  }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1711;
	{
	  tree _r;
	  _r =  build_zero_cst (type);
	  if (TREE_SIDE_EFFECTS (captures[0]))
	    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 983, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1711:;
      }
    }
  if (integer_zerop (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_361 (loc, type, _p0, _p1, captures, RSHIFT_EXPR);
	if (res) return res;
      }
    }
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	tree res = generic_simplify_362 (loc, type, _p0, _p1, captures, RSHIFT_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case VECTOR_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  tree res = generic_simplify_363 (loc, type, _p0, _p1, captures, RSHIFT_EXPR);
	  if (res) return res;
	}
        break;
      }
    case CONSTRUCTOR:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  tree res = generic_simplify_364 (loc, type, _p0, _p1, captures, RSHIFT_EXPR);
	  if (res) return res;
	}
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case RSHIFT_EXPR:
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
		      tree res = generic_simplify_365 (loc, type, _p0, _p1, captures, RSHIFT_EXPR);
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
			    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1 };
			    tree res = generic_simplify_366 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, RSHIFT_EXPR);
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
			    tree res = generic_simplify_366 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, RSHIFT_EXPR);
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
			    tree res = generic_simplify_366 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, RSHIFT_EXPR);
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
			    tree res = generic_simplify_369 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP128);
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
			    tree res = generic_simplify_369 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP16);
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
			    tree res = generic_simplify_369 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP32);
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
			    tree res = generic_simplify_369 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP64);
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
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21, _p1 };
		      tree res = generic_simplify_367 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, RSHIFT_EXPR);
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
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21, _p1 };
		      tree res = generic_simplify_367 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, RSHIFT_EXPR);
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
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21, _p1 };
		      tree res = generic_simplify_367 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, RSHIFT_EXPR);
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
 && (TREE_CODE_CLASS (RSHIFT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1712;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1712;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1712;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1712:;
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
 && (TREE_CODE_CLASS (RSHIFT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1713;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1713;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1713;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1713;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1713:;
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
		      tree res = generic_simplify_369 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP128);
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
		      tree res = generic_simplify_369 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP16);
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
		      tree res = generic_simplify_369 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP32);
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
		      tree res = generic_simplify_369 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_BSWAP64);
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
 && (TREE_CODE_CLASS (RSHIFT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1714;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1714;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1714;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1714;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1714:;
	    }
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
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1715;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[0];
				    _o1[1] = captures[2];
				    _r1 = fold_build2_loc (loc, RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
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
next_after_fail1715:;
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
  return NULL_TREE;
}

tree
generic_simplify_UNLT_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
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
 && (TREE_CODE_CLASS (UNLT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1762;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, UNLT_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1762;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, UNLT_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1762;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1762:;
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
 && (TREE_CODE_CLASS (UNLT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1763;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1763;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, UNLT_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1763;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, UNLT_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1763;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1763:;
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
 && (TREE_CODE_CLASS (UNLT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1764;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1764;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, UNLT_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1764;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, UNLT_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1764;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1764:;
	    }
	}
        break;
      }
    default:;
    }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_520 (loc, type, _p0, _p1, captures, UNLT_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p0))
    {
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
		tree res = generic_simplify_244 (loc, type, _p0, _p1, captures, UNLT_EXPR, LT_EXPR);
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
		tree res = generic_simplify_247 (loc, type, _p0, _p1, captures, UNLT_EXPR, UNGT_EXPR);
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
	    tree res = generic_simplify_248 (loc, type, _p0, _p1, captures, UNLT_EXPR, UNGT_EXPR);
	    if (res) return res;
	  }
        }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  tree res = generic_simplify_519 (loc, type, _p0, _p1, captures, UNLT_EXPR);
	  if (res) return res;
	}
        break;
      }
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_UNLE_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
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
 && (TREE_CODE_CLASS (UNLE_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1765;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, UNLE_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1765;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, UNLE_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1765;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1765:;
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
 && (TREE_CODE_CLASS (UNLE_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1766;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1766;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, UNLE_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1766;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, UNLE_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1766;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1766:;
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
 && (TREE_CODE_CLASS (UNLE_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1767;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1767;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, UNLE_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1767;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, UNLE_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1767;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1767:;
	    }
	}
        break;
      }
    default:;
    }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_521 (loc, type, _p0, _p1, captures, UNLE_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p0))
    {
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
		tree res = generic_simplify_244 (loc, type, _p0, _p1, captures, UNLE_EXPR, LE_EXPR);
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
		tree res = generic_simplify_247 (loc, type, _p0, _p1, captures, UNLE_EXPR, UNGE_EXPR);
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
	    tree res = generic_simplify_248 (loc, type, _p0, _p1, captures, UNLE_EXPR, UNGE_EXPR);
	    if (res) return res;
	  }
        }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  tree res = generic_simplify_519 (loc, type, _p0, _p1, captures, UNLE_EXPR);
	  if (res) return res;
	}
        break;
      }
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_LTGT_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
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
 && (TREE_CODE_CLASS (LTGT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1777;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, LTGT_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1777;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, LTGT_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1777;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1777:;
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
 && (TREE_CODE_CLASS (LTGT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1778;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1778;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, LTGT_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1778;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, LTGT_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1778;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1778:;
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
 && (TREE_CODE_CLASS (LTGT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1779;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1779;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, LTGT_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1779;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, LTGT_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1779;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1779:;
	    }
	}
        break;
      }
    default:;
    }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (!flag_trapping_math || !tree_expr_maybe_nan_p (captures[0])
)
	  {
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1780;
	    {
	      tree _r;
	      _r =  constant_boolean_node (false, type);
	      if (TREE_SIDE_EFFECTS (captures[0]))
		_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1008, __FILE__, __LINE__, true);
	      return _r;
	    }
next_after_fail1780:;
	  }
      }
    }
  switch (TREE_CODE (_p0))
    {
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
		tree res = generic_simplify_244 (loc, type, _p0, _p1, captures, LTGT_EXPR, NE_EXPR);
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
		tree res = generic_simplify_247 (loc, type, _p0, _p1, captures, LTGT_EXPR, LTGT_EXPR);
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
	    tree res = generic_simplify_248 (loc, type, _p0, _p1, captures, LTGT_EXPR, LTGT_EXPR);
	    if (res) return res;
	  }
        }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  tree res = generic_simplify_519 (loc, type, _p0, _p1, captures, LTGT_EXPR);
	  if (res) return res;
	}
        break;
      }
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_COND_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1, tree _p2)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case LE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (integer_zerop (_q21))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		    {
		      if ((_p2 == _q21 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q21, 0) && types_match (_p2, _q21)))
			{
			  if (integer_zerop (_p2))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				if (ANY_INTEGRAL_TYPE_P (type) && TYPE_OVERFLOW_UNDEFINED (type)
)
				  {
				    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1813;
				    if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1813;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1813;
				    {
				      tree res_op0;
				      res_op0 = captures[2];
				      tree res_op1;
				      res_op1 = captures[1];
				      tree _r;
				      _r = fold_build2_loc (loc, MAX_EXPR, type, res_op0, res_op1);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1009, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1813:;
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
		      CASE_CONVERT:
		        {
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
			    {
			      switch (TREE_CODE (_p2))
			        {
				case INTEGER_CST:
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q21, _p2 };
				      tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, LE_EXPR);
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
		    if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		      {
			switch (TREE_CODE (_p2))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q21, _p2 };
				tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, LE_EXPR);
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
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  tree _q61 = TREE_OPERAND (_p1, 1);
			  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
			    {
			      switch (TREE_CODE (_q61))
			        {
				case INTEGER_CST:
				  {
				    switch (TREE_CODE (_p2))
				      {
				      case INTEGER_CST:
				        {
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q21, _q61, _p2 };
					    tree res = generic_simplify_473 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, PLUS_EXPR);
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
		        }
		      case MINUS_EXPR:
		        {
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  tree _q61 = TREE_OPERAND (_p1, 1);
			  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
			    {
			      switch (TREE_CODE (_q61))
			        {
				case INTEGER_CST:
				  {
				    switch (TREE_CODE (_p2))
				      {
				      case INTEGER_CST:
				        {
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q21, _q61, _p2 };
					    tree res = generic_simplify_473 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MINUS_EXPR);
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
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			switch (TREE_CODE (_p2))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p2 };
				tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, LE_EXPR);
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
		  switch (TREE_CODE (_p2))
		    {
		    case INTEGER_CST:
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p2 };
			  tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, LE_EXPR);
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
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			switch (TREE_CODE (_q51))
			  {
			  case INTEGER_CST:
			    {
			      switch (TREE_CODE (_p2))
			        {
				case INTEGER_CST:
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _p2 };
				      tree res = generic_simplify_473 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, PLUS_EXPR);
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
		  }
		case MINUS_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    tree _q51 = TREE_OPERAND (_p1, 1);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			switch (TREE_CODE (_q51))
			  {
			  case INTEGER_CST:
			    {
			      switch (TREE_CODE (_p2))
			        {
				case INTEGER_CST:
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _p2 };
				      tree res = generic_simplify_473 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MINUS_EXPR);
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
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MIN_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MIN_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MIN_EXPR);
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
		  switch (TREE_CODE (_p2))
		    {
		    case MIN_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q21, _q20, _p1, _q51, _p2 };
		    tree res = generic_simplify_475 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MIN_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q21, _q20, _p1, _q50, _p2 };
		    tree res = generic_simplify_475 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case MAX_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MAX_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q51, _p2 };
		    tree res = generic_simplify_475 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MAX_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MAX_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MAX_EXPR);
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
		  switch (TREE_CODE (_p2))
		    {
		    case MAX_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MAX_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q50, _p2 };
		    tree res = generic_simplify_475 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MAX_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	if (tree_expr_nonnegative_p (_q20))
	  {
	    if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	      {
		switch (TREE_CODE (_p2))
		  {
		  case ABS_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_p2, 0);
		      if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p2 };
			    tree res = generic_simplify_476 (loc, type, _p0, _p1, _p2, captures, GE_EXPR);
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
    case EQ_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (tree_zero_one_valued_p (_q20))
	  {
	    if (integer_zerop (_q21))
	      {
		switch (TREE_CODE (_p2))
		  {
		  case BIT_XOR_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_p2, 0);
		      tree _q61 = TREE_OPERAND (_p2, 1);
		      if ((_q61 == _p1 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _p1, 0) && types_match (_q61, _p1)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _q60 };
			    tree res = generic_simplify_477 (loc, type, _p0, _p1, _p2, captures, BIT_XOR_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _q61 };
			    tree res = generic_simplify_477 (loc, type, _p0, _p1, _p2, captures, BIT_XOR_EXPR);
			    if (res) return res;
			  }
		        }
		      break;
		    }
		  case BIT_IOR_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_p2, 0);
		      tree _q61 = TREE_OPERAND (_p2, 1);
		      if ((_q61 == _p1 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _p1, 0) && types_match (_q61, _p1)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _q60 };
			    tree res = generic_simplify_477 (loc, type, _p0, _p1, _p2, captures, BIT_IOR_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _q61 };
			    tree res = generic_simplify_477 (loc, type, _p0, _p1, _p2, captures, BIT_IOR_EXPR);
			    if (res) return res;
			  }
		        }
		      break;
		    }
		  case PLUS_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_p2, 0);
		      tree _q61 = TREE_OPERAND (_p2, 1);
		      if ((_q61 == _p1 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _p1, 0) && types_match (_q61, _p1)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _q60 };
			    tree res = generic_simplify_477 (loc, type, _p0, _p1, _p2, captures, PLUS_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _q61 };
			    tree res = generic_simplify_477 (loc, type, _p0, _p1, _p2, captures, PLUS_EXPR);
			    if (res) return res;
			  }
		        }
		      break;
		    }
	          default:;
	          }
	      }
	  }
	if (integer_zerop (_q21))
	  {
	    switch (TREE_CODE (_p2))
	      {
	      case PLUS_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  tree _q61 = TREE_OPERAND (_p2, 1);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_478 (loc, type, _p0, _p1, _p2, captures, PLUS_EXPR);
			    if (res) return res;
			  }
		        }
		    }
		  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		    {
		      if ((_q61 == _p1 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _p1, 0) && types_match (_q61, _p1)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_478 (loc, type, _p0, _p1, _p2, captures, PLUS_EXPR);
			    if (res) return res;
			  }
		        }
		    }
	          break;
	        }
	      case BIT_IOR_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  tree _q61 = TREE_OPERAND (_p2, 1);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_478 (loc, type, _p0, _p1, _p2, captures, BIT_IOR_EXPR);
			    if (res) return res;
			  }
		        }
		    }
		  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		    {
		      if ((_q61 == _p1 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _p1, 0) && types_match (_q61, _p1)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_478 (loc, type, _p0, _p1, _p2, captures, BIT_IOR_EXPR);
			    if (res) return res;
			  }
		        }
		    }
	          break;
	        }
	      case BIT_XOR_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  tree _q61 = TREE_OPERAND (_p2, 1);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_478 (loc, type, _p0, _p1, _p2, captures, BIT_XOR_EXPR);
			    if (res) return res;
			  }
		        }
		    }
		  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		    {
		      if ((_q61 == _p1 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _p1, 0) && types_match (_q61, _p1)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_478 (loc, type, _p0, _p1, _p2, captures, BIT_XOR_EXPR);
			    if (res) return res;
			  }
		        }
		    }
	          break;
	        }
	      case LROTATE_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  tree _q61 = TREE_OPERAND (_p2, 1);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_479 (loc, type, _p0, _p1, _p2, captures);
			    if (res) return res;
			  }
		        }
		    }
	          break;
	        }
	      case RROTATE_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  tree _q61 = TREE_OPERAND (_p2, 1);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_479 (loc, type, _p0, _p1, _p2, captures);
			    if (res) return res;
			  }
		        }
		    }
	          break;
	        }
	      case LSHIFT_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  tree _q61 = TREE_OPERAND (_p2, 1);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_479 (loc, type, _p0, _p1, _p2, captures);
			    if (res) return res;
			  }
		        }
		    }
	          break;
	        }
	      case RSHIFT_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  tree _q61 = TREE_OPERAND (_p2, 1);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_479 (loc, type, _p0, _p1, _p2, captures);
			    if (res) return res;
			  }
		        }
		    }
	          break;
	        }
	      case MINUS_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  tree _q61 = TREE_OPERAND (_p2, 1);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_479 (loc, type, _p0, _p1, _p2, captures);
			    if (res) return res;
			  }
		        }
		    }
	          break;
	        }
	      case POINTER_PLUS_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  tree _q61 = TREE_OPERAND (_p2, 1);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_479 (loc, type, _p0, _p1, _p2, captures);
			    if (res) return res;
			  }
		        }
		    }
	          break;
	        }
	      default:;
	      }
	  }
	if (integer_onep (_q21))
	  {
	    switch (TREE_CODE (_p2))
	      {
	      case TRUNC_DIV_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  tree _q61 = TREE_OPERAND (_p2, 1);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_480 (loc, type, _p0, _p1, _p2, captures, TRUNC_DIV_EXPR);
			    if (res) return res;
			  }
		        }
		    }
	          break;
	        }
	      case CEIL_DIV_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  tree _q61 = TREE_OPERAND (_p2, 1);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_480 (loc, type, _p0, _p1, _p2, captures, CEIL_DIV_EXPR);
			    if (res) return res;
			  }
		        }
		    }
	          break;
	        }
	      case FLOOR_DIV_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  tree _q61 = TREE_OPERAND (_p2, 1);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_480 (loc, type, _p0, _p1, _p2, captures, FLOOR_DIV_EXPR);
			    if (res) return res;
			  }
		        }
		    }
	          break;
	        }
	      case ROUND_DIV_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  tree _q61 = TREE_OPERAND (_p2, 1);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_480 (loc, type, _p0, _p1, _p2, captures, ROUND_DIV_EXPR);
			    if (res) return res;
			  }
		        }
		    }
	          break;
	        }
	      case EXACT_DIV_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  tree _q61 = TREE_OPERAND (_p2, 1);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_480 (loc, type, _p0, _p1, _p2, captures, EXACT_DIV_EXPR);
			    if (res) return res;
			  }
		        }
		    }
	          break;
	        }
	      case MULT_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  tree _q61 = TREE_OPERAND (_p2, 1);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_481 (loc, type, _p0, _p1, _p2, captures, MULT_EXPR);
			    if (res) return res;
			  }
		        }
		    }
		  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		    {
		      if ((_q61 == _p1 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _p1, 0) && types_match (_q61, _p1)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_481 (loc, type, _p0, _p1, _p2, captures, MULT_EXPR);
			    if (res) return res;
			  }
		        }
		    }
	          break;
	        }
	      default:;
	      }
	  }
	if (integer_all_onesp (_q21))
	  {
	    switch (TREE_CODE (_p2))
	      {
	      case BIT_AND_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  tree _q61 = TREE_OPERAND (_p2, 1);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_482 (loc, type, _p0, _p1, _p2, captures, BIT_AND_EXPR);
			    if (res) return res;
			  }
		        }
		    }
		  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		    {
		      if ((_q61 == _p1 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _p1, 0) && types_match (_q61, _p1)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			    tree res = generic_simplify_482 (loc, type, _p0, _p1, _p2, captures, BIT_AND_EXPR);
			    if (res) return res;
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
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_q21))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      CASE_CONVERT:
		        {
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
			    {
			      switch (TREE_CODE (_p2))
			        {
				case INTEGER_CST:
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q21, _p2 };
				      tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, EQ_EXPR);
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
		    if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		      {
			switch (TREE_CODE (_p2))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q21, _p2 };
				tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, EQ_EXPR);
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
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			switch (TREE_CODE (_p2))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p2 };
				tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, EQ_EXPR);
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
		  switch (TREE_CODE (_p2))
		    {
		    case INTEGER_CST:
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p2 };
			  tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, EQ_EXPR);
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
      if (tree_expr_nonnegative_p (_q21))
	{
	  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	    {
	      switch (TREE_CODE (_p2))
	        {
		case ABS_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p2, 0);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p2 };
			  tree res = generic_simplify_476 (loc, type, _p0, _p1, _p2, captures, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	    }
        }
	if (tree_expr_nonnegative_p (_q20))
	  {
	    if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	      {
		switch (TREE_CODE (_p2))
		  {
		  case ABS_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_p2, 0);
		      if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p2 };
			    tree res = generic_simplify_476 (loc, type, _p0, _p1, _p2, captures, EQ_EXPR);
			    if (res) return res;
			  }
		        }
		      break;
		    }
	          default:;
	          }
	      }
	    if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	      {
		switch (TREE_CODE (_p2))
		  {
		  case ABS_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_p2, 0);
		      if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p2 };
			    tree res = generic_simplify_483 (loc, type, _p0, _p1, _p2, captures);
			    if (res) return res;
			  }
		        }
		      break;
		    }
	          default:;
	          }
	      }
	  }
	if (tree_expr_nonnegative_p (_q21))
	  {
	    if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	      {
		switch (TREE_CODE (_p2))
		  {
		  case ABS_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_p2, 0);
		      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p2 };
			    tree res = generic_simplify_483 (loc, type, _p0, _p1, _p2, captures);
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
    case NE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (tree_zero_one_valued_p (_q20))
	  {
	    if (integer_zerop (_q21))
	      {
		switch (TREE_CODE (_p1))
		  {
		  case BIT_XOR_EXPR:
		    {
		      tree _q50 = TREE_OPERAND (_p1, 0);
		      tree _q51 = TREE_OPERAND (_p1, 1);
		      if ((_p2 == _q51 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q51, 0) && types_match (_p2, _q51)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q50, _q51 };
			    tree res = generic_simplify_484 (loc, type, _p0, _p1, _p2, captures, BIT_XOR_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_p2 == _q50 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q50, 0) && types_match (_p2, _q50)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q51, _q50 };
			    tree res = generic_simplify_484 (loc, type, _p0, _p1, _p2, captures, BIT_XOR_EXPR);
			    if (res) return res;
			  }
		        }
		      break;
		    }
		  case BIT_IOR_EXPR:
		    {
		      tree _q50 = TREE_OPERAND (_p1, 0);
		      tree _q51 = TREE_OPERAND (_p1, 1);
		      if ((_p2 == _q51 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q51, 0) && types_match (_p2, _q51)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q50, _q51 };
			    tree res = generic_simplify_484 (loc, type, _p0, _p1, _p2, captures, BIT_IOR_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_p2 == _q50 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q50, 0) && types_match (_p2, _q50)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q51, _q50 };
			    tree res = generic_simplify_484 (loc, type, _p0, _p1, _p2, captures, BIT_IOR_EXPR);
			    if (res) return res;
			  }
		        }
		      break;
		    }
		  case PLUS_EXPR:
		    {
		      tree _q50 = TREE_OPERAND (_p1, 0);
		      tree _q51 = TREE_OPERAND (_p1, 1);
		      if ((_p2 == _q51 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q51, 0) && types_match (_p2, _q51)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q50, _q51 };
			    tree res = generic_simplify_484 (loc, type, _p0, _p1, _p2, captures, PLUS_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_p2 == _q50 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q50, 0) && types_match (_p2, _q50)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q51, _q50 };
			    tree res = generic_simplify_484 (loc, type, _p0, _p1, _p2, captures, PLUS_EXPR);
			    if (res) return res;
			  }
		        }
		      break;
		    }
	          default:;
	          }
	      }
	  }
	if (integer_zerop (_q21))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case TRUNC_DIV_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if (integer_zerop (_p2))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _q20, _p1, _q50, _q51 };
			tree res = generic_simplify_485 (loc, type, _p0, _p1, _p2, captures, TRUNC_DIV_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      case CEIL_DIV_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if (integer_zerop (_p2))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _q20, _p1, _q50, _q51 };
			tree res = generic_simplify_485 (loc, type, _p0, _p1, _p2, captures, CEIL_DIV_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      case FLOOR_DIV_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if (integer_zerop (_p2))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _q20, _p1, _q50, _q51 };
			tree res = generic_simplify_485 (loc, type, _p0, _p1, _p2, captures, FLOOR_DIV_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      case ROUND_DIV_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if (integer_zerop (_p2))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _q20, _p1, _q50, _q51 };
			tree res = generic_simplify_485 (loc, type, _p0, _p1, _p2, captures, ROUND_DIV_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      case EXACT_DIV_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if (integer_zerop (_p2))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _q20, _p1, _q50, _q51 };
			tree res = generic_simplify_485 (loc, type, _p0, _p1, _p2, captures, EXACT_DIV_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      case MULT_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if (integer_zerop (_p2))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _q20, _p1, _q50, _q51 };
			tree res = generic_simplify_486 (loc, type, _p0, _p1, _p2, captures, MULT_EXPR);
			if (res) return res;
		      }
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _q20, _p1, _q51, _q50 };
			tree res = generic_simplify_486 (loc, type, _p0, _p1, _p2, captures, MULT_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      case BIT_AND_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if (integer_zerop (_p2))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _q20, _p1, _q50, _q51 };
			tree res = generic_simplify_486 (loc, type, _p0, _p1, _p2, captures, BIT_AND_EXPR);
			if (res) return res;
		      }
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _q20, _p1, _q51, _q50 };
			tree res = generic_simplify_486 (loc, type, _p0, _p1, _p2, captures, BIT_AND_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	  }
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
		      CASE_CONVERT:
		        {
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
			    {
			      switch (TREE_CODE (_p2))
			        {
				case INTEGER_CST:
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q21, _p2 };
				      tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, NE_EXPR);
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
		    if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		      {
			switch (TREE_CODE (_p2))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q21, _p2 };
				tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, NE_EXPR);
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
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			switch (TREE_CODE (_p2))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p2 };
				tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, NE_EXPR);
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
		  switch (TREE_CODE (_p2))
		    {
		    case INTEGER_CST:
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p2 };
			  tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, NE_EXPR);
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
	switch (TREE_CODE (_p1))
	  {
	  case MINUS_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      if (integer_zerop (_p2))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    tree res = generic_simplify_487 (loc, type, _p0, _p1, _p2, captures);
			    if (res) return res;
			  }
		        }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      if (integer_zerop (_p2))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_487 (loc, type, _p0, _p1, _p2, captures);
			    if (res) return res;
			  }
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
		      if (integer_zerop (_p2))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    tree res = generic_simplify_488 (loc, type, _p0, _p1, _p2, captures);
			    if (res) return res;
			  }
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_488 (loc, type, _p0, _p1, _p2, captures);
			    if (res) return res;
			  }
		        }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      if (integer_zerop (_p2))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    tree res = generic_simplify_488 (loc, type, _p0, _p1, _p2, captures);
			    if (res) return res;
			  }
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_488 (loc, type, _p0, _p1, _p2, captures);
			    if (res) return res;
			  }
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
		      if ((_p2 == _q20 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q20, 0) && types_match (_p2, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    tree res = generic_simplify_489 (loc, type, _p0, _p1, _p2, captures, BIT_AND_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_p2 == _q21 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q21, 0) && types_match (_p2, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_489 (loc, type, _p0, _p1, _p2, captures, BIT_AND_EXPR);
			    if (res) return res;
			  }
		        }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      if ((_p2 == _q20 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q20, 0) && types_match (_p2, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    tree res = generic_simplify_489 (loc, type, _p0, _p1, _p2, captures, BIT_AND_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_p2 == _q21 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q21, 0) && types_match (_p2, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_489 (loc, type, _p0, _p1, _p2, captures, BIT_AND_EXPR);
			    if (res) return res;
			  }
		        }
		    }
	        }
	      break;
	    }
	  case BIT_IOR_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      if ((_p2 == _q20 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q20, 0) && types_match (_p2, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    tree res = generic_simplify_489 (loc, type, _p0, _p1, _p2, captures, BIT_IOR_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_p2 == _q21 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q21, 0) && types_match (_p2, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_489 (loc, type, _p0, _p1, _p2, captures, BIT_IOR_EXPR);
			    if (res) return res;
			  }
		        }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      if ((_p2 == _q20 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q20, 0) && types_match (_p2, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    tree res = generic_simplify_489 (loc, type, _p0, _p1, _p2, captures, BIT_IOR_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_p2 == _q21 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q21, 0) && types_match (_p2, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_489 (loc, type, _p0, _p1, _p2, captures, BIT_IOR_EXPR);
			    if (res) return res;
			  }
		        }
		    }
	        }
	      break;
	    }
	  case MIN_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      if ((_p2 == _q20 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q20, 0) && types_match (_p2, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    tree res = generic_simplify_489 (loc, type, _p0, _p1, _p2, captures, MIN_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_p2 == _q21 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q21, 0) && types_match (_p2, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_489 (loc, type, _p0, _p1, _p2, captures, MIN_EXPR);
			    if (res) return res;
			  }
		        }
		    }
		  switch (TREE_CODE (_p2))
		    {
		    case MIN_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, NE_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, NE_EXPR, MIN_EXPR);
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
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      if ((_p2 == _q20 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q20, 0) && types_match (_p2, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    tree res = generic_simplify_489 (loc, type, _p0, _p1, _p2, captures, MIN_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_p2 == _q21 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q21, 0) && types_match (_p2, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_489 (loc, type, _p0, _p1, _p2, captures, MIN_EXPR);
			    if (res) return res;
			  }
		        }
		    }
		  switch (TREE_CODE (_p2))
		    {
		    case MIN_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, NE_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, NE_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MIN_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, NE_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, NE_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MIN_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, NE_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, NE_EXPR, MIN_EXPR);
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
	  case MAX_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      if ((_p2 == _q20 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q20, 0) && types_match (_p2, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    tree res = generic_simplify_489 (loc, type, _p0, _p1, _p2, captures, MAX_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_p2 == _q21 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q21, 0) && types_match (_p2, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_489 (loc, type, _p0, _p1, _p2, captures, MAX_EXPR);
			    if (res) return res;
			  }
		        }
		    }
		  switch (TREE_CODE (_p2))
		    {
		    case MAX_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, NE_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, NE_EXPR, MAX_EXPR);
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
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      if ((_p2 == _q20 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q20, 0) && types_match (_p2, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    tree res = generic_simplify_489 (loc, type, _p0, _p1, _p2, captures, MAX_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_p2 == _q21 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q21, 0) && types_match (_p2, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_489 (loc, type, _p0, _p1, _p2, captures, MAX_EXPR);
			    if (res) return res;
			  }
		        }
		    }
		  switch (TREE_CODE (_p2))
		    {
		    case MAX_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, NE_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, NE_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MAX_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, NE_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, NE_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MAX_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, NE_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, NE_EXPR, MAX_EXPR);
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
	  case MULT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      switch (TREE_CODE (_p2))
		        {
			case MULT_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_p2, 0);
			    tree _q81 = TREE_OPERAND (_p2, 1);
			    if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
			      {
				if ((_q81 == _q20 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q20, 0) && types_match (_q81, _q20)))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				      const enum tree_code op = MULT_EXPR;
				      if (ANY_INTEGRAL_TYPE_P (type)
)
					{
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1814;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1814;
					  {
					    tree _r;
					    _r = captures[2];
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1010, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1814:;
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
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      switch (TREE_CODE (_p2))
		        {
			case MULT_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_p2, 0);
			    tree _q81 = TREE_OPERAND (_p2, 1);
			    if ((_q80 == _q21 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q21, 0) && types_match (_q80, _q21)))
			      {
				if ((_q81 == _q21 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q21, 0) && types_match (_q81, _q21)))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
				      const enum tree_code op = MULT_EXPR;
				      if (ANY_INTEGRAL_TYPE_P (type)
)
					{
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1815;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1815;
					  {
					    tree _r;
					    _r = captures[2];
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1010, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1815:;
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
	  case PLUS_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      switch (TREE_CODE (_p2))
		        {
			case PLUS_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_p2, 0);
			    tree _q81 = TREE_OPERAND (_p2, 1);
			    if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
			      {
				if ((_q81 == _q20 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q20, 0) && types_match (_q81, _q20)))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
				      const enum tree_code op = PLUS_EXPR;
				      if (ANY_INTEGRAL_TYPE_P (type)
)
					{
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1816;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1816;
					  {
					    tree _r;
					    _r = captures[2];
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1010, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1816:;
					}
				    }
				  }
			      }
			    break;
			  }
			case MULT_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_p2, 0);
			    tree _q81 = TREE_OPERAND (_p2, 1);
			    if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
			      {
				if (uniform_integer_cst_p (_q81))
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q81 };
				      if (wi::to_wide (uniform_integer_cst_p (captures[3])) == 2
)
					{
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1817;
					  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1817;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1817;
					  {
					    tree _r;
					    _r = captures[2];
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1011, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1817:;
					}
				    }
				  }
			      }
			    if ((_q80 == _q21 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q21, 0) && types_match (_q80, _q21)))
			      {
				if (uniform_integer_cst_p (_q81))
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _p1, _q81 };
				      if (wi::to_wide (uniform_integer_cst_p (captures[3])) == 2
)
					{
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1818;
					  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1818;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1818;
					  {
					    tree _r;
					    _r = captures[2];
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1011, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1818:;
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
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      switch (TREE_CODE (_p2))
		        {
			case PLUS_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_p2, 0);
			    tree _q81 = TREE_OPERAND (_p2, 1);
			    if ((_q80 == _q21 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q21, 0) && types_match (_q80, _q21)))
			      {
				if ((_q81 == _q21 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q21, 0) && types_match (_q81, _q21)))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
				      const enum tree_code op = PLUS_EXPR;
				      if (ANY_INTEGRAL_TYPE_P (type)
)
					{
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1819;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1819;
					  {
					    tree _r;
					    _r = captures[2];
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1010, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1819:;
					}
				    }
				  }
			      }
			    break;
			  }
			case MULT_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_p2, 0);
			    tree _q81 = TREE_OPERAND (_p2, 1);
			    if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
			      {
				if (uniform_integer_cst_p (_q81))
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q81 };
				      if (wi::to_wide (uniform_integer_cst_p (captures[3])) == 2
)
					{
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1820;
					  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1820;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1820;
					  {
					    tree _r;
					    _r = captures[2];
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1011, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1820:;
					}
				    }
				  }
			      }
			    if ((_q80 == _q21 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q21, 0) && types_match (_q80, _q21)))
			      {
				if (uniform_integer_cst_p (_q81))
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _p1, _q81 };
				      if (wi::to_wide (uniform_integer_cst_p (captures[3])) == 2
)
					{
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1821;
					  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1821;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1821;
					  {
					    tree _r;
					    _r = captures[2];
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1011, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1821:;
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
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_p1))
	        {
		case NEGATE_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			switch (TREE_CODE (_p2))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				if (!TYPE_SATURATING (type)
 && (TYPE_OVERFLOW_WRAPS (type)
 || !wi::only_sign_bit_p (wi::to_wide (captures[1])))
 && wi::eq_p (wi::neg (wi::to_wide (captures[1])), wi::to_wide (captures[3]))
)
				  {
				    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1822;
				    if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1822;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1822;
				    {
				      tree _r;
				      _r = captures[2];
				      if (TREE_SIDE_EFFECTS (captures[1]))
					_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1012, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1822:;
				  }
			      }
			      break;
			    }
		          default:;
		          }
		      }
		    break;
		  }
		case BIT_NOT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			switch (TREE_CODE (_p2))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				if (wi::eq_p (wi::bit_not (wi::to_wide (captures[1])), wi::to_wide (captures[3]))
)
				  {
				    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1823;
				    if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1823;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1823;
				    {
				      tree _r;
				      _r = captures[2];
				      if (TREE_SIDE_EFFECTS (captures[1]))
					_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1013, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1823:;
				  }
			      }
			      break;
			    }
		          default:;
		          }
		      }
		    break;
		  }
		case ABS_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			switch (TREE_CODE (_p2))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				if (wi::abs (wi::to_wide (captures[1])) == wi::to_wide (captures[3])
)
				  {
				    if (ABS_EXPR != ABSU_EXPR && wi::only_sign_bit_p (wi::to_wide (captures[1]))
)
				      {
					{
 tree utype = unsigned_type_for (TREE_TYPE (captures[0]));
					    if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1824;
					    if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1824;
					    if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1824;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1824;
					    {
					      tree res_op0;
					      {
						tree _o1[1], _r1;
						_o1[0] = captures[0];
						_r1 = fold_build1_loc (loc, ABSU_EXPR, utype, _o1[0]);
						res_op0 = _r1;
					      }
					      tree _r;
					      _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
					      if (TREE_SIDE_EFFECTS (captures[1]))
						_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
					      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1014, __FILE__, __LINE__, true);
					      return _r;
					    }
next_after_fail1824:;
					}
				      }
				    else
				      {
					if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1825;
					if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1825;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1825;
					{
					  tree _r;
					  _r = captures[2];
					  if (TREE_SIDE_EFFECTS (captures[1]))
					    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1015, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1825:;
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
		case ABSU_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			switch (TREE_CODE (_p2))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				if (wi::abs (wi::to_wide (captures[1])) == wi::to_wide (captures[3])
)
				  {
				    if (ABSU_EXPR != ABSU_EXPR && wi::only_sign_bit_p (wi::to_wide (captures[1]))
)
				      {
					{
 tree utype = unsigned_type_for (TREE_TYPE (captures[0]));
					    if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1826;
					    if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1826;
					    if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1826;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1826;
					    {
					      tree res_op0;
					      {
						tree _o1[1], _r1;
						_o1[0] = captures[0];
						_r1 = fold_build1_loc (loc, ABSU_EXPR, utype, _o1[0]);
						res_op0 = _r1;
					      }
					      tree _r;
					      _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
					      if (TREE_SIDE_EFFECTS (captures[1]))
						_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
					      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1014, __FILE__, __LINE__, true);
					      return _r;
					    }
next_after_fail1826:;
					}
				      }
				    else
				      {
					if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1827;
					if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1827;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1827;
					{
					  tree _r;
					  _r = captures[2];
					  if (TREE_SIDE_EFFECTS (captures[1]))
					    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1015, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1827:;
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
    case INTEGER_CST:
      {
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _p2 };
	  if (integer_zerop (captures[0])
)
	    {
	      if (!VOID_TYPE_P (TREE_TYPE (captures[2])) || VOID_TYPE_P (type)
)
		{
		  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1828;
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1828;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1828;
		  {
		    tree _r;
		    _r = captures[2];
		    if (TREE_SIDE_EFFECTS (captures[0]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1016, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1828:;
		}
	    }
	  else
	    {
	      if (!VOID_TYPE_P (TREE_TYPE (captures[1])) || VOID_TYPE_P (type)
)
		{
		  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1829;
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1829;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1829;
		  {
		    tree _r;
		    _r = captures[1];
		    if (TREE_SIDE_EFFECTS (captures[0]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1017, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1829:;
		}
	    }
	}
        break;
      }
    case LT_EXPR:
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
		      CASE_CONVERT:
		        {
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
			    {
			      switch (TREE_CODE (_p2))
			        {
				case INTEGER_CST:
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q21, _p2 };
				      tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, LT_EXPR);
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
		    if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		      {
			switch (TREE_CODE (_p2))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q21, _p2 };
				tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, LT_EXPR);
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
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  tree _q61 = TREE_OPERAND (_p1, 1);
			  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
			    {
			      switch (TREE_CODE (_q61))
			        {
				case INTEGER_CST:
				  {
				    switch (TREE_CODE (_p2))
				      {
				      case INTEGER_CST:
				        {
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q21, _q61, _p2 };
					    tree res = generic_simplify_473 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, PLUS_EXPR);
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
		        }
		      case MINUS_EXPR:
		        {
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  tree _q61 = TREE_OPERAND (_p1, 1);
			  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
			    {
			      switch (TREE_CODE (_q61))
			        {
				case INTEGER_CST:
				  {
				    switch (TREE_CODE (_p2))
				      {
				      case INTEGER_CST:
				        {
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q21, _q61, _p2 };
					    tree res = generic_simplify_473 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MINUS_EXPR);
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
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			switch (TREE_CODE (_p2))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p2 };
				tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, LT_EXPR);
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
		  switch (TREE_CODE (_p2))
		    {
		    case INTEGER_CST:
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p2 };
			  tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, LT_EXPR);
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
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			switch (TREE_CODE (_q51))
			  {
			  case INTEGER_CST:
			    {
			      switch (TREE_CODE (_p2))
			        {
				case INTEGER_CST:
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _p2 };
				      tree res = generic_simplify_473 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, PLUS_EXPR);
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
		  }
		case MINUS_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    tree _q51 = TREE_OPERAND (_p1, 1);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			switch (TREE_CODE (_q51))
			  {
			  case INTEGER_CST:
			    {
			      switch (TREE_CODE (_p2))
			        {
				case INTEGER_CST:
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _p2 };
				      tree res = generic_simplify_473 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MINUS_EXPR);
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
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MIN_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MIN_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MIN_EXPR);
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
		  switch (TREE_CODE (_p2))
		    {
		    case MIN_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q21, _q20, _p1, _q51, _p2 };
		    tree res = generic_simplify_475 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MIN_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q21, _q20, _p1, _q50, _p2 };
		    tree res = generic_simplify_475 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case MAX_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MAX_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q51, _p2 };
		    tree res = generic_simplify_475 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MAX_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MAX_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MAX_EXPR);
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
		  switch (TREE_CODE (_p2))
		    {
		    case MAX_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MAX_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q50, _p2 };
		    tree res = generic_simplify_475 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MAX_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	if (tree_expr_nonnegative_p (_q20))
	  {
	    if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	      {
		switch (TREE_CODE (_p2))
		  {
		  case ABS_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_p2, 0);
		      if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p2 };
			    tree res = generic_simplify_476 (loc, type, _p0, _p1, _p2, captures, GT_EXPR);
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
    case GT_EXPR:
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
		      CASE_CONVERT:
		        {
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
			    {
			      switch (TREE_CODE (_p2))
			        {
				case INTEGER_CST:
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q21, _p2 };
				      tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, GT_EXPR);
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
		    if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		      {
			switch (TREE_CODE (_p2))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q21, _p2 };
				tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, GT_EXPR);
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
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  tree _q61 = TREE_OPERAND (_p1, 1);
			  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
			    {
			      switch (TREE_CODE (_q61))
			        {
				case INTEGER_CST:
				  {
				    switch (TREE_CODE (_p2))
				      {
				      case INTEGER_CST:
				        {
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q21, _q61, _p2 };
					    tree res = generic_simplify_473 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, PLUS_EXPR);
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
		        }
		      case MINUS_EXPR:
		        {
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  tree _q61 = TREE_OPERAND (_p1, 1);
			  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
			    {
			      switch (TREE_CODE (_q61))
			        {
				case INTEGER_CST:
				  {
				    switch (TREE_CODE (_p2))
				      {
				      case INTEGER_CST:
				        {
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q21, _q61, _p2 };
					    tree res = generic_simplify_473 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MINUS_EXPR);
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
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			switch (TREE_CODE (_p2))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p2 };
				tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, GT_EXPR);
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
		  switch (TREE_CODE (_p2))
		    {
		    case INTEGER_CST:
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p2 };
			  tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, GT_EXPR);
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
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			switch (TREE_CODE (_q51))
			  {
			  case INTEGER_CST:
			    {
			      switch (TREE_CODE (_p2))
			        {
				case INTEGER_CST:
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _p2 };
				      tree res = generic_simplify_473 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, PLUS_EXPR);
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
		  }
		case MINUS_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    tree _q51 = TREE_OPERAND (_p1, 1);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			switch (TREE_CODE (_q51))
			  {
			  case INTEGER_CST:
			    {
			      switch (TREE_CODE (_p2))
			        {
				case INTEGER_CST:
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _p2 };
				      tree res = generic_simplify_473 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MINUS_EXPR);
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
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MIN_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q51, _p2 };
		    tree res = generic_simplify_475 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MIN_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MIN_EXPR);
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
		  switch (TREE_CODE (_p2))
		    {
		    case MIN_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MIN_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q50, _p2 };
		    tree res = generic_simplify_475 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case MAX_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MAX_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MAX_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GT_EXPR, MAX_EXPR);
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
		  switch (TREE_CODE (_p2))
		    {
		    case MAX_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q21, _q20, _p1, _q51, _p2 };
		    tree res = generic_simplify_475 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MAX_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MAX_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q21, _q20, _p1, _q50, _p2 };
		    tree res = generic_simplify_475 (loc, type, _p0, _p1, _p2, captures, LT_EXPR, MAX_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	if (tree_expr_nonnegative_p (_q21))
	  {
	    if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	      {
		switch (TREE_CODE (_p2))
		  {
		  case ABS_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_p2, 0);
		      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p2 };
			    tree res = generic_simplify_476 (loc, type, _p0, _p1, _p2, captures, GT_EXPR);
			    if (res) return res;
			  }
		        }
		      break;
		    }
	          default:;
	          }
	      }
	  }
	switch (TREE_CODE (_q20))
	  {
	  case PLUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_onep (_q31))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_p1, 0);
			if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
			  {
			    if (integer_onep (_p2))
			      {
				{
				  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q21, _p2 };
				  if (TYPE_UNSIGNED (type)
)
				    {
				      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1830;
				      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1830;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1830;
				      {
					tree res_op0;
					{
					  tree _o1[2], _r1;
					  _o1[0] = unshare_expr (captures[0]);
					  _o1[1] = captures[1];
					  _r1 = fold_build2_loc (loc, GE_EXPR, boolean_type_node, _o1[0], _o1[1]);
					  res_op0 = _r1;
					}
					tree res_op1;
					{
					  tree _o1[1], _r1;
					  _o1[0] = captures[0];
					  _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					  res_op1 = _r1;
					}
					tree res_op2;
					res_op2 = captures[2];
					tree _r;
					_r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1018, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail1830:;
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
    case GE_EXPR:
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
		      CASE_CONVERT:
		        {
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
			    {
			      switch (TREE_CODE (_p2))
			        {
				case INTEGER_CST:
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q21, _p2 };
				      tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, GE_EXPR);
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
		    if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		      {
			switch (TREE_CODE (_p2))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q21, _p2 };
				tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, GE_EXPR);
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
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  tree _q61 = TREE_OPERAND (_p1, 1);
			  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
			    {
			      switch (TREE_CODE (_q61))
			        {
				case INTEGER_CST:
				  {
				    switch (TREE_CODE (_p2))
				      {
				      case INTEGER_CST:
				        {
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q21, _q61, _p2 };
					    tree res = generic_simplify_473 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, PLUS_EXPR);
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
		        }
		      case MINUS_EXPR:
		        {
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  tree _q61 = TREE_OPERAND (_p1, 1);
			  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
			    {
			      switch (TREE_CODE (_q61))
			        {
				case INTEGER_CST:
				  {
				    switch (TREE_CODE (_p2))
				      {
				      case INTEGER_CST:
				        {
					  {
					    tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q21, _q61, _p2 };
					    tree res = generic_simplify_473 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MINUS_EXPR);
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
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			switch (TREE_CODE (_p2))
			  {
			  case INTEGER_CST:
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p2 };
				tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, GE_EXPR);
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
		  switch (TREE_CODE (_p2))
		    {
		    case INTEGER_CST:
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p2 };
			  tree res = generic_simplify_472 (loc, type, _p0, _p1, _p2, captures, GE_EXPR);
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
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			switch (TREE_CODE (_q51))
			  {
			  case INTEGER_CST:
			    {
			      switch (TREE_CODE (_p2))
			        {
				case INTEGER_CST:
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _p2 };
				      tree res = generic_simplify_473 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, PLUS_EXPR);
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
		  }
		case MINUS_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    tree _q51 = TREE_OPERAND (_p1, 1);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			switch (TREE_CODE (_q51))
			  {
			  case INTEGER_CST:
			    {
			      switch (TREE_CODE (_p2))
			        {
				case INTEGER_CST:
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _p2 };
				      tree res = generic_simplify_473 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MINUS_EXPR);
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
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MIN_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q51, _p2 };
		    tree res = generic_simplify_475 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MIN_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MIN_EXPR);
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
		  switch (TREE_CODE (_p2))
		    {
		    case MIN_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MIN_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MIN_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q50, _p2 };
		    tree res = generic_simplify_475 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case MAX_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MAX_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MAX_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, GE_EXPR, MAX_EXPR);
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
		  switch (TREE_CODE (_p2))
		    {
		    case MAX_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q21, _q20, _p1, _q51, _p2 };
		    tree res = generic_simplify_475 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MAX_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case MAX_EXPR:
		      {
			tree _q80 = TREE_OPERAND (_p2, 0);
			tree _q81 = TREE_OPERAND (_p2, 1);
			if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q80 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
			if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q81 };
			      tree res = generic_simplify_474 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MAX_EXPR);
			      if (res) return res;
			    }
			  }
		        break;
		      }
		    default:;
		    }
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _q21, _q20, _p1, _q50, _p2 };
		    tree res = generic_simplify_475 (loc, type, _p0, _p1, _p2, captures, LE_EXPR, MAX_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
	if (tree_expr_nonnegative_p (_q21))
	  {
	    if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	      {
		switch (TREE_CODE (_p2))
		  {
		  case ABS_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_p2, 0);
		      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p2 };
			    tree res = generic_simplify_476 (loc, type, _p0, _p1, _p2, captures, GE_EXPR);
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
    case COND_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	tree _q32 = TREE_OPERAND (_p1, 2);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _p2 };
	      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1831;
	      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1831;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1831;
	      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1831;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1831;
	      {
		tree res_op0;
		res_op0 = captures[0];
		tree res_op1;
		res_op1 = captures[1];
		tree res_op2;
		res_op2 = captures[3];
		tree _r;
		_r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1019, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1831:;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p2))
    {
    case COND_EXPR:
      {
	tree _q40 = TREE_OPERAND (_p2, 0);
	tree _q41 = TREE_OPERAND (_p2, 1);
	tree _q42 = TREE_OPERAND (_p2, 2);
	if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
	  {
	    {
	      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _q41, _q42 };
	      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1832;
	      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1832;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1832;
	      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1832;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1832;
	      {
		tree res_op0;
		res_op0 = captures[0];
		tree res_op1;
		res_op1 = captures[1];
		tree res_op2;
		res_op2 = captures[3];
		tree _r;
		_r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1020, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1832:;
	    }
	  }
	{
	  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42 };
	  if (inverse_conditions_p (captures[0], captures[2])
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1833;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1833;
	      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1833;
	      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1833;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1833;
	      {
		tree res_op0;
		res_op0 = captures[0];
		tree res_op1;
		res_op1 = captures[1];
		tree res_op2;
		res_op2 = captures[3];
		tree _r;
		_r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1021, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1833:;
	    }
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
	{
	  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q30, _q31, _q32, _p2 };
	  if (inverse_conditions_p (captures[0], captures[1])
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1834;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1834;
	      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1834;
	      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1834;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1834;
	      {
		tree res_op0;
		res_op0 = captures[0];
		tree res_op1;
		res_op1 = captures[3];
		tree res_op2;
		res_op2 = captures[4];
		tree _r;
		_r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1022, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1834:;
	    }
	}
        break;
      }
    default:;
    }
  if ((_p2 == _p1 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _p1, 0) && types_match (_p2, _p1)))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1835;
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1835;
	{
	  tree _r;
	  _r = captures[1];
	  if (TREE_SIDE_EFFECTS (captures[0]))
	    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1023, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1835:;
      }
    }
  switch (TREE_CODE (_p0))
    {
    case EQ_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (zerop (_q21))
	  {
	    switch (TREE_CODE (_p2))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			const enum tree_code cmp = EQ_EXPR;
			if (!HONOR_SIGNED_ZEROS (type)
 && bitwise_equal_p (captures[0], captures[1])
)
			  {
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1836;
			    if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1836;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1836;
			    {
			      tree _r;
			      _r = captures[2];
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1024, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1836:;
			  }
		      }
		    }
	          break;
	        }
	      default:;
	      }
	    if (zerop (_p1))
	      {
		switch (TREE_CODE (_p2))
		  {
		  case NEGATE_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_p2, 0);
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p2, _q60 };
			const enum tree_code cmp = EQ_EXPR;
			if (!HONOR_SIGNED_ZEROS (type)
 && bitwise_equal_p (captures[0], captures[2])
)
			  {
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1837;
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1837;
			    if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1837;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1837;
			    {
			      tree _r;
			      _r = captures[1];
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1025, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1837:;
			  }
		      }
		      break;
		    }
	          default:;
	          }
	      }
	  }
	switch (TREE_CODE (_q20))
	  {
	  case MINUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q21))
		{
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      switch (TREE_CODE (_p2))
		        {
			case MINUS_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_p2, 0);
			    tree _q81 = TREE_OPERAND (_p2, 1);
			    if ((_q80 == _q31 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q31, 0) && types_match (_q80, _q31)))
			      {
				if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _p2 };
				      const enum tree_code cmp = EQ_EXPR;
				      if (!HONOR_SIGNED_ZEROS (type)
)
					{
					  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1838;
					  if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1838;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1838;
					  {
					    tree _r;
					    _r = captures[3];
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1026, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1838:;
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
	      if (integer_zerop (_q21))
		{
		  if (integer_zerop (_p1))
		    {
		      switch (TREE_CODE (_p2))
		        {
			case MINUS_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_p2, 0);
			    tree _q81 = TREE_OPERAND (_p2, 1);
			    if ((_q80 == _q31 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q31, 0) && types_match (_q80, _q31)))
			      {
				if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _p2 };
				      tree res = generic_simplify_490 (loc, type, _p0, _p1, _p2, captures, EQ_EXPR);
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
	  case CALL_EXPR:
	    switch (get_call_combined_fn (_q20))
	      {
	      case CFN_BUILT_IN_SIGNBITF:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    if (integer_zerop (_q21))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case NEGATE_EXPR:
			    {
			      tree _q60 = TREE_OPERAND (_p1, 0);
			      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
				{
				  if ((_p2 == _q30 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q30, 0) && types_match (_p2, _q30)))
				    {
				      {
					tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					tree res = generic_simplify_491 (loc, type, _p0, _p1, _p2, captures, EQ_EXPR, CFN_BUILT_IN_SIGNBITF);
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
	      case CFN_BUILT_IN_SIGNBITL:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    if (integer_zerop (_q21))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case NEGATE_EXPR:
			    {
			      tree _q60 = TREE_OPERAND (_p1, 0);
			      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
				{
				  if ((_p2 == _q30 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q30, 0) && types_match (_p2, _q30)))
				    {
				      {
					tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					tree res = generic_simplify_491 (loc, type, _p0, _p1, _p2, captures, EQ_EXPR, CFN_BUILT_IN_SIGNBITL);
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
	      case CFN_SIGNBIT:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    if (integer_zerop (_q21))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case NEGATE_EXPR:
			    {
			      tree _q60 = TREE_OPERAND (_p1, 0);
			      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
				{
				  if ((_p2 == _q30 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q30, 0) && types_match (_p2, _q30)))
				    {
				      {
					tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					tree res = generic_simplify_491 (loc, type, _p0, _p1, _p2, captures, EQ_EXPR, CFN_SIGNBIT);
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
	      case CFN_BUILT_IN_SIGNBIT:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    if (integer_zerop (_q21))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case NEGATE_EXPR:
			    {
			      tree _q60 = TREE_OPERAND (_p1, 0);
			      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
				{
				  if ((_p2 == _q30 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q30, 0) && types_match (_p2, _q30)))
				    {
				      {
					tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					tree res = generic_simplify_491 (loc, type, _p0, _p1, _p2, captures, EQ_EXPR, CFN_BUILT_IN_SIGNBIT);
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
	      default:;
	      }
	    break;
          default:;
          }
        break;
      }
    case UNEQ_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (zerop (_q21))
	  {
	    switch (TREE_CODE (_p2))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
			const enum tree_code cmp = UNEQ_EXPR;
			if (!HONOR_SIGNED_ZEROS (type)
 && bitwise_equal_p (captures[0], captures[1])
)
			  {
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1839;
			    if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1839;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1839;
			    {
			      tree _r;
			      _r = captures[2];
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1024, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1839:;
			  }
		      }
		    }
	          break;
	        }
	      default:;
	      }
	    if (zerop (_p1))
	      {
		switch (TREE_CODE (_p2))
		  {
		  case NEGATE_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_p2, 0);
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p2, _q60 };
			const enum tree_code cmp = UNEQ_EXPR;
			if (!HONOR_SIGNED_ZEROS (type)
 && bitwise_equal_p (captures[0], captures[2])
)
			  {
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1840;
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1840;
			    if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1840;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1840;
			    {
			      tree _r;
			      _r = captures[1];
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1025, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1840:;
			  }
		      }
		      break;
		    }
	          default:;
	          }
	      }
	  }
	switch (TREE_CODE (_q20))
	  {
	  case MINUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q21))
		{
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      switch (TREE_CODE (_p2))
		        {
			case MINUS_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_p2, 0);
			    tree _q81 = TREE_OPERAND (_p2, 1);
			    if ((_q80 == _q31 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q31, 0) && types_match (_q80, _q31)))
			      {
				if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _p2 };
				      const enum tree_code cmp = UNEQ_EXPR;
				      if (!HONOR_SIGNED_ZEROS (type)
)
					{
					  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1841;
					  if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1841;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1841;
					  {
					    tree _r;
					    _r = captures[3];
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1026, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1841:;
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
	      if (integer_zerop (_q21))
		{
		  if (integer_zerop (_p1))
		    {
		      switch (TREE_CODE (_p2))
		        {
			case MINUS_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_p2, 0);
			    tree _q81 = TREE_OPERAND (_p2, 1);
			    if ((_q80 == _q31 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q31, 0) && types_match (_q80, _q31)))
			      {
				if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _p2 };
				      tree res = generic_simplify_490 (loc, type, _p0, _p1, _p2, captures, UNEQ_EXPR);
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
          default:;
          }
        break;
      }
    case NE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (zerop (_q21))
	  {
	    switch (TREE_CODE (_p2))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			const enum tree_code cmp = NE_EXPR;
			if (!HONOR_SIGNED_ZEROS (type)
 && bitwise_equal_p (captures[0], captures[1])
)
			  {
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1842;
			    if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1842;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1842;
			    {
			      tree _r;
			      _r = captures[1];
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1027, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1842:;
			  }
		      }
		    }
	          break;
	        }
	      default:;
	      }
	  if (integer_zerop (_p2))
	    {
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		const enum tree_code cmp = NE_EXPR;
		if (!HONOR_SIGNED_ZEROS (type)
 && bitwise_equal_p (captures[0], captures[1])
)
		  {
		    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1843;
		    if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1843;
		    if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1843;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1843;
		    {
		      tree _r;
		      _r = captures[1];
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1028, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1843:;
		  }
	      }
	    }
	  }
	switch (TREE_CODE (_q20))
	  {
	  case MINUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q21))
		{
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      switch (TREE_CODE (_p2))
		        {
			case MINUS_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_p2, 0);
			    tree _q81 = TREE_OPERAND (_p2, 1);
			    if ((_q80 == _q31 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q31, 0) && types_match (_q80, _q31)))
			      {
				if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q31 };
				      const enum tree_code cmp = NE_EXPR;
				      if (!HONOR_SIGNED_ZEROS (type)
)
					{
					  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1844;
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1844;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1844;
					  {
					    tree _r;
					    _r = captures[0];
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1029, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1844:;
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
	      if (integer_zerop (_q21))
		{
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      if (integer_zerop (_p2))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q31 };
			    tree res = generic_simplify_492 (loc, type, _p0, _p1, _p2, captures, NE_EXPR);
			    if (res) return res;
			  }
		        }
		    }
	        }
	      break;
	    }
	  case BIT_AND_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_pow2p (_q31))
		{
		  if (integer_zerop (_q21))
		    {
		      switch (TREE_CODE (_p1))
		        {
			case INTEGER_CST:
			  {
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
				  if (!POINTER_TYPE_P (type) && integer_pow2p (captures[2])
)
				    {
				      {
 int shift = (wi::exact_log2 (wi::to_wide (captures[2]))
 - wi::exact_log2 (wi::to_wide (captures[1])));
					  if (shift > 0
)
					    {
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1845;
					      if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1845;
					      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1845;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1845;
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
						  _o1[1] =  build_int_cst (integer_type_node, shift);
						  _r1 = fold_build2_loc (loc, LSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						  res_op0 = _r1;
						}
						tree res_op1;
						res_op1 = captures[2];
						tree _r;
						_r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1030, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail1845:;
					    }
					  else
					    {
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1846;
					      if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1846;
					      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1846;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1846;
					      {
						tree res_op0;
						{
						  tree _o1[1], _r1;
						  {
						    tree _o2[2], _r2;
						    _o2[0] = captures[0];
						    _o2[1] =  build_int_cst (integer_type_node, -shift);
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
						res_op1 = captures[2];
						tree _r;
						_r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1031, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail1846:;
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
	  case CALL_EXPR:
	    switch (get_call_combined_fn (_q20))
	      {
	      case CFN_BUILT_IN_SIGNBITF:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    if (integer_zerop (_q21))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case NEGATE_EXPR:
			    {
			      tree _q60 = TREE_OPERAND (_p1, 0);
			      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
				{
				  if ((_p2 == _q30 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q30, 0) && types_match (_p2, _q30)))
				    {
				      {
					tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					tree res = generic_simplify_491 (loc, type, _p0, _p1, _p2, captures, NE_EXPR, CFN_BUILT_IN_SIGNBITF);
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
	      case CFN_BUILT_IN_SIGNBITL:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    if (integer_zerop (_q21))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case NEGATE_EXPR:
			    {
			      tree _q60 = TREE_OPERAND (_p1, 0);
			      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
				{
				  if ((_p2 == _q30 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q30, 0) && types_match (_p2, _q30)))
				    {
				      {
					tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					tree res = generic_simplify_491 (loc, type, _p0, _p1, _p2, captures, NE_EXPR, CFN_BUILT_IN_SIGNBITL);
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
	      case CFN_SIGNBIT:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    if (integer_zerop (_q21))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case NEGATE_EXPR:
			    {
			      tree _q60 = TREE_OPERAND (_p1, 0);
			      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
				{
				  if ((_p2 == _q30 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q30, 0) && types_match (_p2, _q30)))
				    {
				      {
					tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					tree res = generic_simplify_491 (loc, type, _p0, _p1, _p2, captures, NE_EXPR, CFN_SIGNBIT);
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
	      case CFN_BUILT_IN_SIGNBIT:
	        if (call_expr_nargs (_q20) == 1)
    {
		    tree _q30 = CALL_EXPR_ARG (_q20, 0);
		    if (integer_zerop (_q21))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case NEGATE_EXPR:
			    {
			      tree _q60 = TREE_OPERAND (_p1, 0);
			      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
				{
				  if ((_p2 == _q30 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q30, 0) && types_match (_p2, _q30)))
				    {
				      {
					tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
					tree res = generic_simplify_491 (loc, type, _p0, _p1, _p2, captures, NE_EXPR, CFN_BUILT_IN_SIGNBIT);
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
	      default:;
	      }
	    break;
          default:;
          }
	if (integer_zerop (_q21))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case CALL_EXPR:
	        switch (get_call_combined_fn (_p1))
	          {
		  case CFN_PARITY:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  if (integer_zerop (_p2))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
					tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_PARITY);
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
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				  tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_PARITY);
				  if (res) return res;
				}
			      }
			  }
		      }
		    break;
		  case CFN_BUILT_IN_FFS:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  if (integer_zerop (_p2))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
					tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_FFS);
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
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				  tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_FFS);
				  if (res) return res;
				}
			      }
			  }
		      }
		    break;
		  case CFN_BUILT_IN_FFSIMAX:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  if (integer_zerop (_p2))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
					tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_FFSIMAX);
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
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				  tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_FFSIMAX);
				  if (res) return res;
				}
			      }
			  }
		      }
		    break;
		  case CFN_BUILT_IN_POPCOUNT:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  if (integer_zerop (_p2))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
					tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_POPCOUNT);
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
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				  tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_POPCOUNT);
				  if (res) return res;
				}
			      }
			  }
		      }
		    break;
		  case CFN_BUILT_IN_PARITYL:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  if (integer_zerop (_p2))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
					tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_PARITYL);
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
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				  tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_PARITYL);
				  if (res) return res;
				}
			      }
			  }
		      }
		    break;
		  case CFN_BUILT_IN_FFSL:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  if (integer_zerop (_p2))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
					tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_FFSL);
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
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				  tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_FFSL);
				  if (res) return res;
				}
			      }
			  }
		      }
		    break;
		  case CFN_BUILT_IN_POPCOUNTLL:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  if (integer_zerop (_p2))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
					tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_POPCOUNTLL);
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
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				  tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_POPCOUNTLL);
				  if (res) return res;
				}
			      }
			  }
		      }
		    break;
		  case CFN_POPCOUNT:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  if (integer_zerop (_p2))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
					tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_POPCOUNT);
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
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				  tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_POPCOUNT);
				  if (res) return res;
				}
			      }
			  }
		      }
		    break;
		  case CFN_BUILT_IN_PARITYIMAX:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  if (integer_zerop (_p2))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
					tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_PARITYIMAX);
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
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				  tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_PARITYIMAX);
				  if (res) return res;
				}
			      }
			  }
		      }
		    break;
		  case CFN_BUILT_IN_CLRSBL:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  switch (TREE_CODE (_p2))
				    {
				    case INTEGER_CST:
				      {
					{
					  tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q50, _p2 };
					  tree res = generic_simplify_494 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_CLRSBL);
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
			if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
			  {
			    switch (TREE_CODE (_p2))
			      {
			      case INTEGER_CST:
			        {
				  {
				    tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q50, _p2 };
				    tree res = generic_simplify_495 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_CLRSBL);
				    if (res) return res;
				  }
			          break;
			        }
			      default:;
			      }
			  }
		      }
		    break;
		  case CFN_FFS:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  if (integer_zerop (_p2))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
					tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_FFS);
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
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				  tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_FFS);
				  if (res) return res;
				}
			      }
			  }
		      }
		    break;
		  case CFN_BUILT_IN_PARITY:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  if (integer_zerop (_p2))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
					tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_PARITY);
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
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				  tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_PARITY);
				  if (res) return res;
				}
			      }
			  }
		      }
		    break;
		  case CFN_BUILT_IN_POPCOUNTIMAX:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  if (integer_zerop (_p2))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
					tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_POPCOUNTIMAX);
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
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				  tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_POPCOUNTIMAX);
				  if (res) return res;
				}
			      }
			  }
		      }
		    break;
		  case CFN_BUILT_IN_CLRSBIMAX:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  switch (TREE_CODE (_p2))
				    {
				    case INTEGER_CST:
				      {
					{
					  tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q50, _p2 };
					  tree res = generic_simplify_494 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_CLRSBIMAX);
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
			if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
			  {
			    switch (TREE_CODE (_p2))
			      {
			      case INTEGER_CST:
			        {
				  {
				    tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q50, _p2 };
				    tree res = generic_simplify_495 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_CLRSBIMAX);
				    if (res) return res;
				  }
			          break;
			        }
			      default:;
			      }
			  }
		      }
		    break;
		  case CFN_BUILT_IN_CLRSBLL:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  switch (TREE_CODE (_p2))
				    {
				    case INTEGER_CST:
				      {
					{
					  tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q50, _p2 };
					  tree res = generic_simplify_494 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_CLRSBLL);
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
			if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
			  {
			    switch (TREE_CODE (_p2))
			      {
			      case INTEGER_CST:
			        {
				  {
				    tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q50, _p2 };
				    tree res = generic_simplify_495 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_CLRSBLL);
				    if (res) return res;
				  }
			          break;
			        }
			      default:;
			      }
			  }
		      }
		    break;
		  case CFN_CLRSB:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  switch (TREE_CODE (_p2))
				    {
				    case INTEGER_CST:
				      {
					{
					  tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q50, _p2 };
					  tree res = generic_simplify_494 (loc, type, _p0, _p1, _p2, captures, CFN_CLRSB);
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
			if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
			  {
			    switch (TREE_CODE (_p2))
			      {
			      case INTEGER_CST:
			        {
				  {
				    tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q50, _p2 };
				    tree res = generic_simplify_495 (loc, type, _p0, _p1, _p2, captures, CFN_CLRSB);
				    if (res) return res;
				  }
			          break;
			        }
			      default:;
			      }
			  }
		      }
		    break;
		  case CFN_BUILT_IN_BSWAP128:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  if (integer_zerop (_p2))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
					tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_BSWAP128);
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
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				  tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_BSWAP128);
				  if (res) return res;
				}
			      }
			  }
		      }
		    break;
		  case CFN_BUILT_IN_POPCOUNTL:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  if (integer_zerop (_p2))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
					tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_POPCOUNTL);
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
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				  tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_POPCOUNTL);
				  if (res) return res;
				}
			      }
			  }
		      }
		    break;
		  case CFN_BUILT_IN_CLRSB:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  switch (TREE_CODE (_p2))
				    {
				    case INTEGER_CST:
				      {
					{
					  tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q50, _p2 };
					  tree res = generic_simplify_494 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_CLRSB);
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
			if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
			  {
			    switch (TREE_CODE (_p2))
			      {
			      case INTEGER_CST:
			        {
				  {
				    tree captures[5] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q50, _p2 };
				    tree res = generic_simplify_495 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_CLRSB);
				    if (res) return res;
				  }
			          break;
			        }
			      default:;
			      }
			  }
		      }
		    break;
		  case CFN_BUILT_IN_FFSLL:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  if (integer_zerop (_p2))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
					tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_FFSLL);
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
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				  tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_FFSLL);
				  if (res) return res;
				}
			      }
			  }
		      }
		    break;
		  case CFN_BUILT_IN_PARITYLL:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  if (integer_zerop (_p2))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
					tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_PARITYLL);
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
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				  tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_PARITYLL);
				  if (res) return res;
				}
			      }
			  }
		      }
		    break;
		  case CFN_BUILT_IN_BSWAP16:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  if (integer_zerop (_p2))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
					tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_BSWAP16);
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
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				  tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_BSWAP16);
				  if (res) return res;
				}
			      }
			  }
		      }
		    break;
		  case CFN_BUILT_IN_BSWAP32:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  if (integer_zerop (_p2))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
					tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_BSWAP32);
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
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				  tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_BSWAP32);
				  if (res) return res;
				}
			      }
			  }
		      }
		    break;
		  case CFN_BUILT_IN_BSWAP64:
		    if (call_expr_nargs (_p1) == 1)
    {
			tree _q50 = CALL_EXPR_ARG (_p1, 0);
			switch (TREE_CODE (_q50))
			  {
			  CASE_CONVERT:
			    {
			      tree _q60 = TREE_OPERAND (_q50, 0);
			      if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
				{
				  if (integer_zerop (_p2))
				    {
				      {
					tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
					tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_BSWAP64);
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
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _p2 };
				  tree res = generic_simplify_493 (loc, type, _p0, _p1, _p2, captures, CFN_BUILT_IN_BSWAP64);
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
	  }
        break;
      }
    case LTGT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (zerop (_q21))
	  {
	    switch (TREE_CODE (_p2))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			const enum tree_code cmp = LTGT_EXPR;
			if (!HONOR_SIGNED_ZEROS (type)
 && bitwise_equal_p (captures[0], captures[1])
)
			  {
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1847;
			    if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1847;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1847;
			    {
			      tree _r;
			      _r = captures[1];
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1027, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1847:;
			  }
		      }
		    }
	          break;
	        }
	      default:;
	      }
	  if (integer_zerop (_p2))
	    {
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		const enum tree_code cmp = LTGT_EXPR;
		if (!HONOR_SIGNED_ZEROS (type)
 && bitwise_equal_p (captures[0], captures[1])
)
		  {
		    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1848;
		    if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1848;
		    if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1848;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1848;
		    {
		      tree _r;
		      _r = captures[1];
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1028, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1848:;
		  }
	      }
	    }
	  }
	switch (TREE_CODE (_q20))
	  {
	  case MINUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q21))
		{
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      switch (TREE_CODE (_p2))
		        {
			case MINUS_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_p2, 0);
			    tree _q81 = TREE_OPERAND (_p2, 1);
			    if ((_q80 == _q31 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q31, 0) && types_match (_q80, _q31)))
			      {
				if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q31 };
				      const enum tree_code cmp = LTGT_EXPR;
				      if (!HONOR_SIGNED_ZEROS (type)
)
					{
					  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1849;
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1849;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1849;
					  {
					    tree _r;
					    _r = captures[0];
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1029, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1849:;
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
	      if (integer_zerop (_q21))
		{
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      if (integer_zerop (_p2))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q31 };
			    tree res = generic_simplify_492 (loc, type, _p0, _p1, _p2, captures, LTGT_EXPR);
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
    case GE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      if (zerop (_q21))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_p2, 0);
			if ((_q70 == _p1 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _p1, 0) && types_match (_q70, _p1)))
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _p1 };
			      const enum tree_code cmp = GE_EXPR;
			      if (!HONOR_SIGNED_ZEROS (TREE_TYPE (captures[1]))
 && !TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 && element_precision (captures[1])
 <= element_precision (captures[0])
 && bitwise_equal_p (captures[1], captures[2])
)
				{
				  if (TYPE_UNSIGNED (TREE_TYPE (captures[2]))
)
				    {
				      {
 tree stype = signed_type_for (TREE_TYPE (captures[2]));
					  if (types_match (captures[0], stype)
)
					    {
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1850;
					      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1850;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1850;
					      {
						tree res_op0;
						res_op0 = captures[0];
						tree _r;
						_r = fold_build1_loc (loc, ABSU_EXPR, type, res_op0);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1032, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail1850:;
					    }
					  else
					    {
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1851;
					      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1851;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1851;
					      {
						tree res_op0;
						{
						  tree _o1[1], _r1;
						  _o1[0] = captures[2];
						  if (TREE_TYPE (_o1[0]) != stype)
						    {
						      _r1 = fold_build1_loc (loc, NOP_EXPR, stype, _o1[0]);
						    }
						  else
						    _r1 = _o1[0];
						  res_op0 = _r1;
						}
						tree _r;
						_r = fold_build1_loc (loc, ABSU_EXPR, type, res_op0);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1033, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail1851:;
					    }
				      }
				    }
				  else
				    {
				      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1852;
				      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1852;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1852;
				      {
					tree res_op0;
					res_op0 = captures[2];
					tree _r;
					_r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1034, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail1852:;
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
	if (zerop (_q21))
	  {
	    switch (TREE_CODE (_p2))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q20, _p1 };
			const enum tree_code cmp = GE_EXPR;
			if (!HONOR_SIGNED_ZEROS (TREE_TYPE (captures[1]))
 && !TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 && element_precision (captures[1])
 <= element_precision (captures[0])
 && bitwise_equal_p (captures[1], captures[2])
)
			  {
			    if (TYPE_UNSIGNED (TREE_TYPE (captures[2]))
)
			      {
				{
 tree stype = signed_type_for (TREE_TYPE (captures[2]));
				    if (types_match (captures[0], stype)
)
				      {
					if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1853;
					if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1853;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1853;
					{
					  tree res_op0;
					  res_op0 = captures[0];
					  tree _r;
					  _r = fold_build1_loc (loc, ABSU_EXPR, type, res_op0);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1032, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1853:;
				      }
				    else
				      {
					if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1854;
					if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1854;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1854;
					{
					  tree res_op0;
					  {
					    tree _o1[1], _r1;
					    _o1[0] = captures[2];
					    if (TREE_TYPE (_o1[0]) != stype)
					      {
						_r1 = fold_build1_loc (loc, NOP_EXPR, stype, _o1[0]);
					      }
					    else
					      _r1 = _o1[0];
					    res_op0 = _r1;
					  }
					  tree _r;
					  _r = fold_build1_loc (loc, ABSU_EXPR, type, res_op0);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1033, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1854:;
				      }
				}
			      }
			    else
			      {
				if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1855;
				if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1855;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1855;
				{
				  tree res_op0;
				  res_op0 = captures[2];
				  tree _r;
				  _r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1034, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1855:;
			      }
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
	  case MINUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q21))
		{
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      switch (TREE_CODE (_p2))
		        {
			case MINUS_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_p2, 0);
			    tree _q81 = TREE_OPERAND (_p2, 1);
			    if ((_q80 == _q31 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q31, 0) && types_match (_q80, _q31)))
			      {
				if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q31 };
				      const enum tree_code cmp = GE_EXPR;
				      if (!HONOR_SIGNED_ZEROS (type)
 && !TYPE_UNSIGNED (type)
)
					{
					  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1856;
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1856;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1856;
					  {
					    tree res_op0;
					    res_op0 = captures[0];
					    tree _r;
					    _r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1035, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1856:;
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
	if (integer_zerop (_q21))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case BIT_NOT_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  if ((_p2 == _q50 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q50, 0) && types_match (_p2, _q50)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q50 };
			tree res = generic_simplify_496 (loc, type, _p0, _p1, _p2, captures, GE_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	    switch (TREE_CODE (_p2))
	      {
	      case BIT_NOT_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			tree res = generic_simplify_497 (loc, type, _p0, _p1, _p2, captures, GE_EXPR);
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
	switch (TREE_CODE (_q20))
	  {
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      if (zerop (_q21))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_p2, 0);
			if ((_q70 == _p1 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _p1, 0) && types_match (_q70, _p1)))
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _p1 };
			      const enum tree_code cmp = GT_EXPR;
			      if (!HONOR_SIGNED_ZEROS (TREE_TYPE (captures[1]))
 && !TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 && element_precision (captures[1])
 <= element_precision (captures[0])
 && bitwise_equal_p (captures[1], captures[2])
)
				{
				  if (TYPE_UNSIGNED (TREE_TYPE (captures[2]))
)
				    {
				      {
 tree stype = signed_type_for (TREE_TYPE (captures[2]));
					  if (types_match (captures[0], stype)
)
					    {
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1857;
					      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1857;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1857;
					      {
						tree res_op0;
						res_op0 = captures[0];
						tree _r;
						_r = fold_build1_loc (loc, ABSU_EXPR, type, res_op0);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1032, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail1857:;
					    }
					  else
					    {
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1858;
					      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1858;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1858;
					      {
						tree res_op0;
						{
						  tree _o1[1], _r1;
						  _o1[0] = captures[2];
						  if (TREE_TYPE (_o1[0]) != stype)
						    {
						      _r1 = fold_build1_loc (loc, NOP_EXPR, stype, _o1[0]);
						    }
						  else
						    _r1 = _o1[0];
						  res_op0 = _r1;
						}
						tree _r;
						_r = fold_build1_loc (loc, ABSU_EXPR, type, res_op0);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1033, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail1858:;
					    }
				      }
				    }
				  else
				    {
				      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1859;
				      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1859;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1859;
				      {
					tree res_op0;
					res_op0 = captures[2];
					tree _r;
					_r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1034, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail1859:;
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
	if (zerop (_q21))
	  {
	    switch (TREE_CODE (_p2))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q20, _p1 };
			const enum tree_code cmp = GT_EXPR;
			if (!HONOR_SIGNED_ZEROS (TREE_TYPE (captures[1]))
 && !TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 && element_precision (captures[1])
 <= element_precision (captures[0])
 && bitwise_equal_p (captures[1], captures[2])
)
			  {
			    if (TYPE_UNSIGNED (TREE_TYPE (captures[2]))
)
			      {
				{
 tree stype = signed_type_for (TREE_TYPE (captures[2]));
				    if (types_match (captures[0], stype)
)
				      {
					if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1860;
					if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1860;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1860;
					{
					  tree res_op0;
					  res_op0 = captures[0];
					  tree _r;
					  _r = fold_build1_loc (loc, ABSU_EXPR, type, res_op0);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1032, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1860:;
				      }
				    else
				      {
					if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1861;
					if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1861;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1861;
					{
					  tree res_op0;
					  {
					    tree _o1[1], _r1;
					    _o1[0] = captures[2];
					    if (TREE_TYPE (_o1[0]) != stype)
					      {
						_r1 = fold_build1_loc (loc, NOP_EXPR, stype, _o1[0]);
					      }
					    else
					      _r1 = _o1[0];
					    res_op0 = _r1;
					  }
					  tree _r;
					  _r = fold_build1_loc (loc, ABSU_EXPR, type, res_op0);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1033, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1861:;
				      }
				}
			      }
			    else
			      {
				if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1862;
				if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1862;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1862;
				{
				  tree res_op0;
				  res_op0 = captures[2];
				  tree _r;
				  _r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1034, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1862:;
			      }
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
	  case MINUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q21))
		{
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      switch (TREE_CODE (_p2))
		        {
			case MINUS_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_p2, 0);
			    tree _q81 = TREE_OPERAND (_p2, 1);
			    if ((_q80 == _q31 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q31, 0) && types_match (_q80, _q31)))
			      {
				if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q31 };
				      const enum tree_code cmp = GT_EXPR;
				      if (!HONOR_SIGNED_ZEROS (type)
 && !TYPE_UNSIGNED (type)
)
					{
					  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1863;
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1863;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1863;
					  {
					    tree res_op0;
					    res_op0 = captures[0];
					    tree _r;
					    _r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1035, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1863:;
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
    case LE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      if (zerop (_q21))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_p2, 0);
			if ((_q70 == _p1 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _p1, 0) && types_match (_q70, _p1)))
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _p1 };
			      const enum tree_code cmp = LE_EXPR;
			      if (!HONOR_SIGNED_ZEROS (TREE_TYPE (captures[1]))
 && !TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 && element_precision (captures[1])
 <= element_precision (captures[0])
 && bitwise_equal_p (captures[1], captures[2])
)
				{
				  if ((ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[2]))
 && !TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[2])))
 || TYPE_UNSIGNED (TREE_TYPE (captures[2]))
)
				    {
				      {
 tree stype = signed_type_for (TREE_TYPE (captures[2]));
 tree utype = unsigned_type_for (TREE_TYPE (captures[2]));
					  if (types_match (captures[0], stype)
)
					    {
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1864;
					      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1864;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1864;
					      {
						tree res_op0;
						{
						  tree _o1[1], _r1;
						  {
						    tree _o2[1], _r2;
						    _o2[0] = captures[0];
						    _r2 = fold_build1_loc (loc, ABSU_EXPR, utype, _o2[0]);
						    _o1[0] = _r2;
						  }
						  _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
						  res_op0 = _r1;
						}
						tree _r;
						_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1036, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail1864:;
					    }
					  else
					    {
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1865;
					      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1865;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1865;
					      {
						tree res_op0;
						{
						  tree _o1[1], _r1;
						  {
						    tree _o2[1], _r2;
						    {
						      tree _o3[1], _r3;
						      _o3[0] = captures[2];
						      if (TREE_TYPE (_o3[0]) != stype)
							{
							  _r3 = fold_build1_loc (loc, NOP_EXPR, stype, _o3[0]);
							}
						      else
						        _r3 = _o3[0];
						      _o2[0] = _r3;
						    }
						    _r2 = fold_build1_loc (loc, ABSU_EXPR, utype, _o2[0]);
						    _o1[0] = _r2;
						  }
						  _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
						  res_op0 = _r1;
						}
						tree _r;
						_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1037, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail1865:;
					    }
				      }
				    }
				  else
				    {
				      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1866;
				      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1866;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1866;
				      {
					tree res_op0;
					{
					  tree _o1[1], _r1;
					  {
					    tree _o2[1], _r2;
					    _o2[0] = captures[2];
					    _r2 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
					    _o1[0] = _r2;
					  }
					  _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					  res_op0 = _r1;
					}
					tree _r;
					_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1038, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail1866:;
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
	if (zerop (_q21))
	  {
	    switch (TREE_CODE (_p2))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q20, _p1 };
			const enum tree_code cmp = LE_EXPR;
			if (!HONOR_SIGNED_ZEROS (TREE_TYPE (captures[1]))
 && !TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 && element_precision (captures[1])
 <= element_precision (captures[0])
 && bitwise_equal_p (captures[1], captures[2])
)
			  {
			    if ((ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[2]))
 && !TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[2])))
 || TYPE_UNSIGNED (TREE_TYPE (captures[2]))
)
			      {
				{
 tree stype = signed_type_for (TREE_TYPE (captures[2]));
 tree utype = unsigned_type_for (TREE_TYPE (captures[2]));
				    if (types_match (captures[0], stype)
)
				      {
					if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1867;
					if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1867;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1867;
					{
					  tree res_op0;
					  {
					    tree _o1[1], _r1;
					    {
					      tree _o2[1], _r2;
					      _o2[0] = captures[0];
					      _r2 = fold_build1_loc (loc, ABSU_EXPR, utype, _o2[0]);
					      _o1[0] = _r2;
					    }
					    _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					    res_op0 = _r1;
					  }
					  tree _r;
					  _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1036, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1867:;
				      }
				    else
				      {
					if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1868;
					if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1868;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1868;
					{
					  tree res_op0;
					  {
					    tree _o1[1], _r1;
					    {
					      tree _o2[1], _r2;
					      {
						tree _o3[1], _r3;
						_o3[0] = captures[2];
						if (TREE_TYPE (_o3[0]) != stype)
						  {
						    _r3 = fold_build1_loc (loc, NOP_EXPR, stype, _o3[0]);
						  }
						else
						  _r3 = _o3[0];
						_o2[0] = _r3;
					      }
					      _r2 = fold_build1_loc (loc, ABSU_EXPR, utype, _o2[0]);
					      _o1[0] = _r2;
					    }
					    _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					    res_op0 = _r1;
					  }
					  tree _r;
					  _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1037, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1868:;
				      }
				}
			      }
			    else
			      {
				if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1869;
				if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1869;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1869;
				{
				  tree res_op0;
				  {
				    tree _o1[1], _r1;
				    {
				      tree _o2[1], _r2;
				      _o2[0] = captures[2];
				      _r2 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				      _o1[0] = _r2;
				    }
				    _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1038, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1869:;
			      }
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
	  case MINUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q21))
		{
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      switch (TREE_CODE (_p2))
		        {
			case MINUS_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_p2, 0);
			    tree _q81 = TREE_OPERAND (_p2, 1);
			    if ((_q80 == _q31 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q31, 0) && types_match (_q80, _q31)))
			      {
				if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q31 };
				      const enum tree_code cmp = LE_EXPR;
				      if (!HONOR_SIGNED_ZEROS (type)
 && !TYPE_UNSIGNED (type)
)
					{
					  if (ANY_INTEGRAL_TYPE_P (type)
 && !TYPE_OVERFLOW_WRAPS (type)
)
					    {
					      {
 tree utype = unsigned_type_for (type);
						  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1870;
						  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1870;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1870;
						  {
						    tree res_op0;
						    {
						      tree _o1[1], _r1;
						      {
							tree _o2[1], _r2;
							_o2[0] = captures[0];
							_r2 = fold_build1_loc (loc, ABSU_EXPR, utype, _o2[0]);
							_o1[0] = _r2;
						      }
						      _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
						      res_op0 = _r1;
						    }
						    tree _r;
						    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
						    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1039, __FILE__, __LINE__, true);
						    return _r;
						  }
next_after_fail1870:;
					      }
					    }
					  else
					    {
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1871;
					      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1871;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1871;
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
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1040, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail1871:;
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
    case LT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      if (zerop (_q21))
		{
		  switch (TREE_CODE (_p2))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_p2, 0);
			if ((_q70 == _p1 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _p1, 0) && types_match (_q70, _p1)))
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _p1 };
			      const enum tree_code cmp = LT_EXPR;
			      if (!HONOR_SIGNED_ZEROS (TREE_TYPE (captures[1]))
 && !TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 && element_precision (captures[1])
 <= element_precision (captures[0])
 && bitwise_equal_p (captures[1], captures[2])
)
				{
				  if ((ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[2]))
 && !TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[2])))
 || TYPE_UNSIGNED (TREE_TYPE (captures[2]))
)
				    {
				      {
 tree stype = signed_type_for (TREE_TYPE (captures[2]));
 tree utype = unsigned_type_for (TREE_TYPE (captures[2]));
					  if (types_match (captures[0], stype)
)
					    {
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1872;
					      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1872;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1872;
					      {
						tree res_op0;
						{
						  tree _o1[1], _r1;
						  {
						    tree _o2[1], _r2;
						    _o2[0] = captures[0];
						    _r2 = fold_build1_loc (loc, ABSU_EXPR, utype, _o2[0]);
						    _o1[0] = _r2;
						  }
						  _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
						  res_op0 = _r1;
						}
						tree _r;
						_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1036, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail1872:;
					    }
					  else
					    {
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1873;
					      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1873;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1873;
					      {
						tree res_op0;
						{
						  tree _o1[1], _r1;
						  {
						    tree _o2[1], _r2;
						    {
						      tree _o3[1], _r3;
						      _o3[0] = captures[2];
						      if (TREE_TYPE (_o3[0]) != stype)
							{
							  _r3 = fold_build1_loc (loc, NOP_EXPR, stype, _o3[0]);
							}
						      else
						        _r3 = _o3[0];
						      _o2[0] = _r3;
						    }
						    _r2 = fold_build1_loc (loc, ABSU_EXPR, utype, _o2[0]);
						    _o1[0] = _r2;
						  }
						  _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
						  res_op0 = _r1;
						}
						tree _r;
						_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1037, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail1873:;
					    }
				      }
				    }
				  else
				    {
				      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1874;
				      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1874;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1874;
				      {
					tree res_op0;
					{
					  tree _o1[1], _r1;
					  {
					    tree _o2[1], _r2;
					    _o2[0] = captures[2];
					    _r2 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
					    _o1[0] = _r2;
					  }
					  _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					  res_op0 = _r1;
					}
					tree _r;
					_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1038, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail1874:;
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
	if (zerop (_q21))
	  {
	    switch (TREE_CODE (_p2))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q20, _p1 };
			const enum tree_code cmp = LT_EXPR;
			if (!HONOR_SIGNED_ZEROS (TREE_TYPE (captures[1]))
 && !TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 && element_precision (captures[1])
 <= element_precision (captures[0])
 && bitwise_equal_p (captures[1], captures[2])
)
			  {
			    if ((ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[2]))
 && !TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[2])))
 || TYPE_UNSIGNED (TREE_TYPE (captures[2]))
)
			      {
				{
 tree stype = signed_type_for (TREE_TYPE (captures[2]));
 tree utype = unsigned_type_for (TREE_TYPE (captures[2]));
				    if (types_match (captures[0], stype)
)
				      {
					if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1875;
					if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1875;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1875;
					{
					  tree res_op0;
					  {
					    tree _o1[1], _r1;
					    {
					      tree _o2[1], _r2;
					      _o2[0] = captures[0];
					      _r2 = fold_build1_loc (loc, ABSU_EXPR, utype, _o2[0]);
					      _o1[0] = _r2;
					    }
					    _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					    res_op0 = _r1;
					  }
					  tree _r;
					  _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1036, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1875:;
				      }
				    else
				      {
					if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1876;
					if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1876;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1876;
					{
					  tree res_op0;
					  {
					    tree _o1[1], _r1;
					    {
					      tree _o2[1], _r2;
					      {
						tree _o3[1], _r3;
						_o3[0] = captures[2];
						if (TREE_TYPE (_o3[0]) != stype)
						  {
						    _r3 = fold_build1_loc (loc, NOP_EXPR, stype, _o3[0]);
						  }
						else
						  _r3 = _o3[0];
						_o2[0] = _r3;
					      }
					      _r2 = fold_build1_loc (loc, ABSU_EXPR, utype, _o2[0]);
					      _o1[0] = _r2;
					    }
					    _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
					    res_op0 = _r1;
					  }
					  tree _r;
					  _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1037, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1876:;
				      }
				}
			      }
			    else
			      {
				if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1877;
				if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1877;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1877;
				{
				  tree res_op0;
				  {
				    tree _o1[1], _r1;
				    {
				      tree _o2[1], _r2;
				      _o2[0] = captures[2];
				      _r2 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
				      _o1[0] = _r2;
				    }
				    _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
				    res_op0 = _r1;
				  }
				  tree _r;
				  _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1038, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1877:;
			      }
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
	  case MINUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (zerop (_q21))
		{
		  if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		    {
		      switch (TREE_CODE (_p2))
		        {
			case MINUS_EXPR:
			  {
			    tree _q80 = TREE_OPERAND (_p2, 0);
			    tree _q81 = TREE_OPERAND (_p2, 1);
			    if ((_q80 == _q31 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q31, 0) && types_match (_q80, _q31)))
			      {
				if ((_q81 == _q30 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q30, 0) && types_match (_q81, _q30)))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q31 };
				      const enum tree_code cmp = LT_EXPR;
				      if (!HONOR_SIGNED_ZEROS (type)
 && !TYPE_UNSIGNED (type)
)
					{
					  if (ANY_INTEGRAL_TYPE_P (type)
 && !TYPE_OVERFLOW_WRAPS (type)
)
					    {
					      {
 tree utype = unsigned_type_for (type);
						  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1878;
						  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1878;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1878;
						  {
						    tree res_op0;
						    {
						      tree _o1[1], _r1;
						      {
							tree _o2[1], _r2;
							_o2[0] = captures[0];
							_r2 = fold_build1_loc (loc, ABSU_EXPR, utype, _o2[0]);
							_o1[0] = _r2;
						      }
						      _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
						      res_op0 = _r1;
						    }
						    tree _r;
						    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
						    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1039, __FILE__, __LINE__, true);
						    return _r;
						  }
next_after_fail1878:;
					      }
					    }
					  else
					    {
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1879;
					      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1879;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1879;
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
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1040, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail1879:;
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
	if (integer_zerop (_q21))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case BIT_NOT_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  if ((_p2 == _q50 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q50, 0) && types_match (_p2, _q50)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q50 };
			tree res = generic_simplify_496 (loc, type, _p0, _p1, _p2, captures, LT_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	    switch (TREE_CODE (_p2))
	      {
	      case BIT_NOT_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p2, 0);
		  if ((_q60 == _p1 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p1, 0) && types_match (_q60, _p1)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			tree res = generic_simplify_497 (loc, type, _p0, _p1, _p2, captures, LT_EXPR);
			if (res) return res;
		      }
		    }
	          break;
	        }
	      default:;
	      }
	    switch (TREE_CODE (_p1))
	      {
	      case INTEGER_CST:
	        {
		  if (integer_zerop (_p2))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			if (integer_pow2p (captures[1])
 && !TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
			  {
			    {
 int shift = element_precision (captures[0]) - wi::exact_log2 (wi::to_wide (captures[1])) - 1;
				if (shift >= 0
)
				  {
				    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1880;
				    if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1880;
				    if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1880;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1880;
				    {
				      tree res_op0;
				      {
					tree _o1[1], _r1;
					{
					  tree _o2[2], _r2;
					  _o2[0] = captures[0];
					  _o2[1] =  build_int_cst (integer_type_node, shift);
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
				      _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1041, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1880:;
				  }
				else
				  {
				    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1881;
				    if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1881;
				    if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1881;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1881;
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
				      _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1042, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1881:;
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
{
  tree _p0_pops[1];
  if (tree_logical_inverted_value (_p0, _p0_pops))
    {
      tree _q20 = _p0_pops[0];
      if (tree_truth_valued_p (_q20))
	{
	  {
	    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
	    if (COND_EXPR == VEC_COND_EXPR
 || TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
	      {
		if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1882;
		if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1882;
		if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1882;
		{
		  tree res_op0;
		  res_op0 = captures[0];
		  tree res_op1;
		  res_op1 = captures[2];
		  tree res_op2;
		  res_op2 = captures[1];
		  tree _r;
		  _r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
		  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1043, __FILE__, __LINE__, true);
		  return _r;
		}
next_after_fail1882:;
	      }
	  }
        }
    }
}
  return NULL_TREE;
}
#pragma GCC diagnostic pop
