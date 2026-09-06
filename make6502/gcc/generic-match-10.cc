/* Generated automatically by the program `genmatch' from
   a IL pattern matching and simplification description.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-function"

#include "generic-match-auto.h"

bool
tree_nop_convert (tree t, tree *res_ops)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  switch (TREE_CODE (t))
    {
    CASE_CONVERT:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	  if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
	    {
	      {
		res_ops[0] = captures[0];
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 0, __FILE__, __LINE__, false);
		return true;
	      }
	    }
	}
        break;
      }
    case VIEW_CONVERT_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	  if (VECTOR_TYPE_P (type) && VECTOR_TYPE_P (TREE_TYPE (captures[0]))
 && known_eq (TYPE_VECTOR_SUBPARTS (type),
 TYPE_VECTOR_SUBPARTS (TREE_TYPE (captures[0])))
 && tree_nop_conversion_p (TREE_TYPE (type), TREE_TYPE (TREE_TYPE (captures[0])))
)
	    {
	      {
		res_ops[0] = captures[0];
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1, __FILE__, __LINE__, false);
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
tree_min_value (tree t)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  if (uniform_integer_cst_p (t))
    {
      {
	{
 tree int_cst = uniform_integer_cst_p (t);
 tree inner_type = TREE_TYPE (int_cst);
	    if ((INTEGRAL_TYPE_P (inner_type)
 || POINTER_TYPE_P (inner_type))
 && wi::eq_p (wi::to_wide (int_cst), wi::min_value (inner_type))
)
	      {
		{
		  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 38, __FILE__, __LINE__, false);
		  return true;
		}
	      }
	}
      }
    }
  return false;
}

bool
tree_usadd_left_part_2 (tree t, tree *res_ops)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  switch (TREE_CODE (t))
    {
    case REALPART_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	switch (TREE_CODE (_p0))
	  {
	  case CALL_EXPR:
	    switch (get_call_combined_fn (_p0))
	      {
	      case CFN_ADD_OVERFLOW:
	        if (call_expr_nargs (_p0) == 2)
    {
		    tree _q20 = CALL_EXPR_ARG (_p0, 0);
		    tree _q21 = CALL_EXPR_ARG (_p0, 1);
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		      if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
			{
			  {
			    res_ops[0] = captures[0];
			    res_ops[1] = captures[1];
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 41, __FILE__, __LINE__, false);
			    return true;
			  }
			}
		    }
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
		      if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
			{
			  {
			    res_ops[0] = captures[0];
			    res_ops[1] = captures[1];
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 41, __FILE__, __LINE__, false);
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
  return false;
}

bool
tree_compositional_complex (tree t)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  switch (TREE_CODE (t))
    {
    CASE_CONVERT:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	switch (TREE_CODE (_p0))
	  {
	  case COMPLEX_EXPR:
	    {
	      tree _q20 = TREE_OPERAND (_p0, 0);
	      tree _q21 = TREE_OPERAND (_p0, 1);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		{
		  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 79, __FILE__, __LINE__, false);
		  return true;
		}
	      }
	      break;
	    }
          default:;
          }
        break;
      }
    case COMPLEX_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 79, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    default:;
    }
  return false;
}

bool
tree_double_value_p (tree t)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  {
    tree captures[1] ATTRIBUTE_UNUSED = { t };
    if (TYPE_MAIN_VARIANT (TREE_TYPE (captures[0])) == double_type_node
)
      {
	{
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 80, __FILE__, __LINE__, false);
	  return true;
	}
      }
  }
  return false;
}

bool
tree_float_value_p (tree t)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  {
    tree captures[1] ATTRIBUTE_UNUSED = { t };
    if (TYPE_MAIN_VARIANT (TREE_TYPE (captures[0])) == float_type_node
)
      {
	{
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 81, __FILE__, __LINE__, false);
	  return true;
	}
      }
  }
  return false;
}

bool
tree_vec_same_elem_p (tree t)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  switch (TREE_CODE (t))
    {
    case VEC_DUPLICATE_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 82, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case CONSTRUCTOR:
      {
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { t };
	  if (TREE_CODE (captures[0]) == SSA_NAME
 && uniform_vector_p (gimple_assign_rhs1 (SSA_NAME_DEF_STMT (captures[0])))
)
	    {
	      {
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 83, __FILE__, __LINE__, false);
		return true;
	      }
	    }
	}
        break;
      }
    default:;
    }
  {
    tree captures[1] ATTRIBUTE_UNUSED = { t };
    if (uniform_vector_p (captures[0])
)
      {
	{
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 84, __FILE__, __LINE__, false);
	  return true;
	}
      }
  }
  return false;
}

bool
tree_bitwise_induction_p (tree t, tree *res_ops)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  switch (TREE_CODE (t))
    {
    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
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
		    switch (TREE_CODE (_q30))
		      {
		      CASE_CONVERT:
		        {
			  tree _q40 = TREE_OPERAND (_q30, 0);
			  switch (TREE_CODE (_q40))
			    {
			    case LSHIFT_EXPR:
			      {
				tree _q50 = TREE_OPERAND (_q40, 0);
				tree _q51 = TREE_OPERAND (_q40, 1);
				if (integer_onep (_q50))
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q51, _p1, _q50 };
				      {
					res_ops[0] = captures[0];
					res_ops[1] = captures[1];
					res_ops[2] = captures[2];
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 87, __FILE__, __LINE__, false);
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
		      case LSHIFT_EXPR:
		        {
			  tree _q40 = TREE_OPERAND (_q30, 0);
			  tree _q41 = TREE_OPERAND (_q30, 1);
			  if (integer_onep (_q40))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q41, _p1, _q40 };
				{
				  res_ops[0] = captures[0];
				  res_ops[1] = captures[1];
				  res_ops[2] = captures[2];
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 87, __FILE__, __LINE__, false);
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
		CASE_CONVERT:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    switch (TREE_CODE (_q30))
		      {
		      case LSHIFT_EXPR:
		        {
			  tree _q40 = TREE_OPERAND (_q30, 0);
			  tree _q41 = TREE_OPERAND (_q30, 1);
			  if (integer_onep (_q40))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q41, _p1, _q40 };
				{
				  res_ops[0] = captures[0];
				  res_ops[1] = captures[1];
				  res_ops[2] = captures[2];
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 87, __FILE__, __LINE__, false);
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
		case LSHIFT_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    if (integer_onep (_q30))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q31, _p1, _q30 };
			  {
			    res_ops[0] = captures[0];
			    res_ops[1] = captures[1];
			    res_ops[2] = captures[2];
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 87, __FILE__, __LINE__, false);
			    return true;
			  }
			}
		      }
		    break;
		  }
	        default:;
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
		case BIT_NOT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    switch (TREE_CODE (_q40))
		      {
		      CASE_CONVERT:
		        {
			  tree _q50 = TREE_OPERAND (_q40, 0);
			  switch (TREE_CODE (_q50))
			    {
			    case LSHIFT_EXPR:
			      {
				tree _q60 = TREE_OPERAND (_q50, 0);
				tree _q61 = TREE_OPERAND (_q50, 1);
				if (integer_onep (_q60))
				  {
				    {
				      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q61, _p0, _q60 };
				      {
					res_ops[0] = captures[0];
					res_ops[1] = captures[1];
					res_ops[2] = captures[2];
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 87, __FILE__, __LINE__, false);
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
		      case LSHIFT_EXPR:
		        {
			  tree _q50 = TREE_OPERAND (_q40, 0);
			  tree _q51 = TREE_OPERAND (_q40, 1);
			  if (integer_onep (_q50))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q51, _p0, _q50 };
				{
				  res_ops[0] = captures[0];
				  res_ops[1] = captures[1];
				  res_ops[2] = captures[2];
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 87, __FILE__, __LINE__, false);
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
		CASE_CONVERT:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    switch (TREE_CODE (_q40))
		      {
		      case LSHIFT_EXPR:
		        {
			  tree _q50 = TREE_OPERAND (_q40, 0);
			  tree _q51 = TREE_OPERAND (_q40, 1);
			  if (integer_onep (_q50))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q51, _p0, _q50 };
				{
				  res_ops[0] = captures[0];
				  res_ops[1] = captures[1];
				  res_ops[2] = captures[2];
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 87, __FILE__, __LINE__, false);
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
		case LSHIFT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    if (integer_onep (_q40))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q41, _p0, _q40 };
			  {
			    res_ops[0] = captures[0];
			    res_ops[1] = captures[1];
			    res_ops[2] = captures[2];
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 87, __FILE__, __LINE__, false);
			    return true;
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
	  case BIT_NOT_EXPR:
	    {
	      tree _q20 = TREE_OPERAND (_p0, 0);
	      switch (TREE_CODE (_q20))
	        {
		CASE_CONVERT:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    switch (TREE_CODE (_q30))
		      {
		      case LSHIFT_EXPR:
		        {
			  tree _q40 = TREE_OPERAND (_q30, 0);
			  tree _q41 = TREE_OPERAND (_q30, 1);
			  if (integer_onep (_q40))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q41, _p1, _q40 };
				{
				  res_ops[0] = captures[0];
				  res_ops[1] = captures[1];
				  res_ops[2] = captures[2];
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 87, __FILE__, __LINE__, false);
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
		case LSHIFT_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    if (integer_onep (_q30))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q31, _p1, _q30 };
			  {
			    res_ops[0] = captures[0];
			    res_ops[1] = captures[1];
			    res_ops[2] = captures[2];
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 87, __FILE__, __LINE__, false);
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
	switch (TREE_CODE (_p1))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_p1, 0);
	      switch (TREE_CODE (_q30))
	        {
		CASE_CONVERT:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    switch (TREE_CODE (_q40))
		      {
		      case LSHIFT_EXPR:
		        {
			  tree _q50 = TREE_OPERAND (_q40, 0);
			  tree _q51 = TREE_OPERAND (_q40, 1);
			  if (integer_onep (_q50))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q51, _p0, _q50 };
				{
				  res_ops[0] = captures[0];
				  res_ops[1] = captures[1];
				  res_ops[2] = captures[2];
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 87, __FILE__, __LINE__, false);
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
		case LSHIFT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    if (integer_onep (_q40))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q41, _p0, _q40 };
			  {
			    res_ops[0] = captures[0];
			    res_ops[1] = captures[1];
			    res_ops[2] = captures[2];
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 87, __FILE__, __LINE__, false);
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
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q31, _p1, _q30 };
			  {
			    res_ops[0] = captures[0];
			    res_ops[1] = captures[1];
			    res_ops[2] = captures[2];
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 87, __FILE__, __LINE__, false);
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
			  tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q41, _p0, _q40 };
			  {
			    res_ops[0] = captures[0];
			    res_ops[1] = captures[1];
			    res_ops[2] = captures[2];
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 87, __FILE__, __LINE__, false);
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
	switch (TREE_CODE (_p0))
	  {
	  case LSHIFT_EXPR:
	    {
	      tree _q20 = TREE_OPERAND (_p0, 0);
	      tree _q21 = TREE_OPERAND (_p0, 1);
	      if (integer_onep (_q20))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _p1, _q20 };
		    {
		      res_ops[0] = captures[0];
		      res_ops[1] = captures[1];
		      res_ops[2] = captures[2];
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 87, __FILE__, __LINE__, false);
		      return true;
		    }
		  }
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
		    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q31, _p0, _q30 };
		    {
		      res_ops[0] = captures[0];
		      res_ops[1] = captures[1];
		      res_ops[2] = captures[2];
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 87, __FILE__, __LINE__, false);
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
    case BIT_NOT_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	switch (TREE_CODE (_p0))
	  {
	  case BIT_XOR_EXPR:
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
		      case LSHIFT_EXPR:
		        {
			  tree _q40 = TREE_OPERAND (_q30, 0);
			  tree _q41 = TREE_OPERAND (_q30, 1);
			  if (integer_onep (_q40))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q41, _q21, _q40 };
				{
				  res_ops[0] = captures[0];
				  res_ops[1] = captures[1];
				  res_ops[2] = captures[2];
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 88, __FILE__, __LINE__, false);
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
		case LSHIFT_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    if (integer_onep (_q30))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q31, _q21, _q30 };
			  {
			    res_ops[0] = captures[0];
			    res_ops[1] = captures[1];
			    res_ops[2] = captures[2];
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 88, __FILE__, __LINE__, false);
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
      {
	tree _p0_pops[1];
	if (tree_nop_convert (_p0, _p0_pops))
	  {
	    tree _q20 = _p0_pops[0];
	    switch (TREE_CODE (_q20))
	      {
	      case BIT_XOR_EXPR:
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
			  case LSHIFT_EXPR:
			    {
			      tree _q50 = TREE_OPERAND (_q40, 0);
			      tree _q51 = TREE_OPERAND (_q40, 1);
			      if (integer_onep (_q50))
				{
				  {
				    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q51, _q31, _q50 };
				    {
				      res_ops[0] = captures[0];
				      res_ops[1] = captures[1];
				      res_ops[2] = captures[2];
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 88, __FILE__, __LINE__, false);
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
		    case LSHIFT_EXPR:
		      {
			tree _q40 = TREE_OPERAND (_q30, 0);
			tree _q41 = TREE_OPERAND (_q30, 1);
			if (integer_onep (_q40))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q41, _q31, _q40 };
			      {
				res_ops[0] = captures[0];
				res_ops[1] = captures[1];
				res_ops[2] = captures[2];
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 88, __FILE__, __LINE__, false);
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
	  }
      }
        break;
      }
    default:;
    }
  return false;
}

tree
generic_simplify_46 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
 && tree_nop_conversion_p (type, TREE_TYPE (captures[3]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail274;
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
next_after_fail274:;
    }
  return NULL_TREE;
}

tree
generic_simplify_53 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (!ANY_INTEGRAL_TYPE_P (type) || TYPE_OVERFLOW_WRAPS (type)
)
	    {
	      if (!CONSTANT_CLASS_P (captures[2])
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail283;
		  {
		    tree res_op0;
		    {
		      tree _o1[1], _r1;
		      _o1[0] = captures[2];
		      _r1 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, _o1[0]);
		      res_op0 = _r1;
		    }
		    tree res_op1;
		    {
		      tree _o1[2], _r1;
		      _o1[0] = captures[0];
		      {
			tree _o2[1], _r2;
			_o2[0] = captures[1];
			_r2 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, _o2[0]);
			_o1[1] = _r2;
		      }
		      _r1 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      if (EXPR_P (_r1))
		        goto next_after_fail283;
		      res_op1 = _r1;
		    }
		    tree _r;
		    _r = fold_build2_loc (loc, PLUS_EXPR, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 152, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail283:;
		}
	    }
	  else
	    {
	      if (!ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[2]))
 || TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[2]))
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail284;
		  {
		    tree res_op0;
		    {
		      tree _o1[2], _r1;
		      _o1[0] = captures[2];
		      {
			tree _o2[2], _r2;
			{
			  tree _o3[1], _r3;
			  _o3[0] = captures[0];
			  _r3 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, TREE_TYPE (_o1[0]), _o3[0]);
			  _o2[0] = _r3;
			}
			_o2[1] = captures[1];
			_r2 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
			if (EXPR_P (_r2))
			  goto next_after_fail284;
			_o1[1] = _r2;
		      }
		      _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      res_op0 = _r1;
		    }
		    tree _r;
		    _r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 153, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail284:;
		}
	      else
		{
		  if (types_match (type, captures[2]) && !TYPE_OVERFLOW_SANITIZED (type)
)
		    {
		      {
 tree cst = const_binop (MINUS_EXPR, type, captures[0], captures[1]);
			  if (cst && !TREE_OVERFLOW (cst)
)
			    {
			      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail285;
			      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail285;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail285;
			      {
				tree res_op0;
				res_op0 =  cst;
				tree res_op1;
				res_op1 = captures[2];
				tree _r;
				_r = fold_build2_loc (loc, PLUS_EXPR, type, res_op0, res_op1);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 154, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail285:;
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
generic_simplify_67 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail301;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 = captures[1];
    tree _r;
    _r = fold_build2_loc (loc, BIT_IOR_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 170, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail301:;
  return NULL_TREE;
}

tree
generic_simplify_75 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail309;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail309;
  {
    tree _r;
    _r = captures[0];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 175, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail309:;
  return NULL_TREE;
}

tree
generic_simplify_82 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 bool wascmp;
      if (types_match (TREE_TYPE (captures[0]), TREE_TYPE (captures[1]))
 && bitwise_inverted_equal_p (captures[0], captures[1], wascmp)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail316;
	  {
	    tree res_op0;
	    res_op0 =  wascmp
 ? constant_boolean_node (true, type)
 : build_all_ones_cst (TREE_TYPE (captures[0]));
	    tree _r;
	    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 180, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail316:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_93 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail327;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail327;
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
next_after_fail327:;
	}
      else
	{
	  if (single_use (captures[0]) && single_use (captures[1])
)
	    {
	      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail328;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail328;
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
next_after_fail328:;
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_102 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop),
 const enum tree_code ARG_UNUSED (rbitop))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail340;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail340;
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
next_after_fail340:;
  return NULL_TREE;
}

tree
generic_simplify_109 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (bitwise_equal_p (captures[4], captures[7])
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail348;
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
next_after_fail348:;
    }
  return NULL_TREE;
}

tree
generic_simplify_118 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (inner_op),
 const enum tree_code ARG_UNUSED (outer_op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 bool fail = false;
 wide_int zero_mask_not;
 wide_int C0;
 wide_int cst_emit;
 if (TREE_CODE (captures[1]) == SSA_NAME)
 zero_mask_not = get_nonzero_bits (captures[1]);
 else
 fail = true;
 if (inner_op == BIT_XOR_EXPR)
 {
 C0 = wi::bit_and_not (wi::to_wide (captures[2]), wi::to_wide (captures[3]));
 cst_emit = C0 | wi::to_wide (captures[3]);
 }
 else
 {
 C0 = wi::to_wide (captures[2]);
 cst_emit = C0 ^ wi::to_wide (captures[3]);
 }
      if (!fail && (C0 & zero_mask_not) == 0
)
	{
	  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail362;
	  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail362;
	  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail362;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail362;
	  {
	    tree res_op0;
	    res_op0 = captures[1];
	    tree res_op1;
	    res_op1 =  wide_int_to_tree (type, cst_emit);
	    tree _r;
	    _r = fold_build2_loc (loc, outer_op, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 212, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail362:;
	}
      else
	{
	  if (!fail && (wi::to_wide (captures[3]) & zero_mask_not) == 0
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail363;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail363;
	      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail363;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail363;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		res_op1 =  wide_int_to_tree (type, cst_emit);
		tree _r;
		_r = fold_build2_loc (loc, inner_op, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 213, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail363:;
	    }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_130 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (eqne))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (eqne == EQ_EXPR
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail393;
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail393;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail393;
      {
	tree _r;
	_r = captures[0];
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 234, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail393:;
    }
  else
    {
      if (eqne == NE_EXPR
)
	{
	  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail394;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail394;
	  {
	    tree _r;
	    _r =  constant_boolean_node (true, type);
	    if (TREE_SIDE_EFFECTS (captures[2]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 235, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail394:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_135 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail412;
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail412;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail412;
  {
    tree _r;
    _r = captures[0];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 236, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail412:;
  return NULL_TREE;
}

tree
generic_simplify_137 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail414;
	  if (TREE_SIDE_EFFECTS (captures[7])) goto next_after_fail414;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail414;
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
next_after_fail414:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_141 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && tree_int_cst_sgn (captures[2]) > 0
 && (tree_nonzero_bits (captures[0]) & tree_nonzero_bits (captures[1])) == 0
)
    {
      {
 tree t = type;
 if (!TYPE_OVERFLOW_WRAPS (t))
 t = unsigned_type_for (t);
 wide_int wone = wi::one (TYPE_PRECISION (t));
 wide_int c = wi::add (wi::lshift (wone, wi::to_wide (captures[2])), wone);
	  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail418;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail418;
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
	      _o1[1] =  wide_int_to_tree (t, c);
	      _r1 = fold_build2_loc (loc, MULT_EXPR, t, _o1[0], _o1[1]);
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 238, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail418:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_147 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail424;
  {
    if (! tree_invariant_p (captures[3])) goto next_after_fail424;
    tree res_op0;
    {
      tree _o1[2], _r1;
      _o1[0] = captures[2];
      _o1[1] = unshare_expr (captures[3]);
      _r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      res_op0 = _r1;
    }
    tree res_op1;
    {
      tree _o1[2], _r1;
      {
	tree _o2[1], _r2;
	_o2[0] = captures[4];
	_r2 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
	if (EXPR_P (_r2))
	  goto next_after_fail424;
	_o1[0] = _r2;
      }
      _o1[1] = captures[3];
      _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      if (EXPR_P (_r1))
        goto next_after_fail424;
      res_op1 = _r1;
    }
    tree _r;
    _r = fold_build2_loc (loc, BIT_XOR_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 243, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail424:;
  return NULL_TREE;
}

tree
generic_simplify_157 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (element_precision (type) <= element_precision (TREE_TYPE (captures[0]))
 && element_precision (type) <= element_precision (TREE_TYPE (captures[1]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail434;
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
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 253, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail434:;
    }
  return NULL_TREE;
}

tree
generic_simplify_168 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (COPYSIGN_ALL))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail447;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail447;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree _r;
	_r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 265, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail447:;
    }
  return NULL_TREE;
}

tree
generic_simplify_172 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail451;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail451;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree _r;
    _r = non_lvalue_loc (loc, res_op0);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 267, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail451:;
  return NULL_TREE;
}

tree
generic_simplify_179 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
 && tree_nop_conversion_p (type, TREE_TYPE (captures[1]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail460;
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
	  {
	    tree _o2[1], _r2;
	    _o2[0] = captures[1];
	    _r2 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
	    _o1[0] = _r2;
	  }
	  if (TREE_TYPE (_o1[0]) != type)
	    {
	      _r1 = fold_build1_loc (loc, NOP_EXPR, type, _o1[0]);
	    }
	  else
	    _r1 = _o1[0];
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 273, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail460:;
    }
  return NULL_TREE;
}

tree
generic_simplify_190 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && !TYPE_OVERFLOW_WRAPS (TREE_TYPE(captures[0]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail472;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 =  build_zero_cst (TREE_TYPE(captures[0]));
	tree _r;
	_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 284, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail472:;
    }
  return NULL_TREE;
}

tree
generic_simplify_194 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))
 && tree_expr_nonzero_p (captures[1])
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail476;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 = captures[2];
	tree _r;
	_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 288, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail476:;
    }
  return NULL_TREE;
}

tree
generic_simplify_200 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail498;
  {
    tree res_op0;
    res_op0 = captures[1];
    tree res_op1;
    res_op1 = captures[2];
    tree _r;
    _r = fold_build2_loc (loc, op, type, res_op0, res_op1);
    if (TREE_SIDE_EFFECTS (captures[0]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 309, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail498:;
  return NULL_TREE;
}

tree
generic_simplify_206 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (neeq))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 poly_int64 diff; tree inner_type = TREE_TYPE (captures[2]);
      if (ptr_difference_const (captures[0], captures[1], &diff)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail504;
	  {
	    tree res_op0;
	    res_op0 =  build_int_cst_type (inner_type, diff);
	    tree res_op1;
	    res_op1 = captures[2];
	    tree _r;
	    _r = fold_build2_loc (loc, neeq, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 314, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail504:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_212 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (minmax),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (out))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail510;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 = captures[1];
	tree _r;
	_r = fold_build2_loc (loc, out, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 320, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail510:;
    }
  return NULL_TREE;
}

tree
generic_simplify_218 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_int_cst_sgn (captures[0]) > 0
)
    {
      {
 int c1 = wi::clz (wi::to_wide (captures[0]));
 int c2 = wi::clz (wi::to_wide (captures[2]));
	  if (c1 > c2
)
	    {
	      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail520;
	      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail520;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail520;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail520;
	      {
		tree _r;
		_r =  constant_boolean_node (cmp == NE_EXPR ? false : true, type);
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 330, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail520:;
	    }
	  else
	    {
	      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail521;
	      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail521;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail521;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail521;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		res_op1 =  build_int_cst (TREE_TYPE (captures[1]), c2 - c1);
		tree _r;
		_r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 331, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail521:;
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_224 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (rotate),
 const enum tree_code ARG_UNUSED (invrot),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (captures[2]) || integer_all_onesp (captures[2])
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail534;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 = captures[2];
	tree _r;
	_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 344, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail534:;
    }
  return NULL_TREE;
}

tree
generic_simplify_229 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail539;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail539;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		res_op1 = captures[2];
		tree _r;
		_r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 349, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail539:;
	    }
	  else
	    {
	      if (ic == ncmp
)
		{
		  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail540;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail540;
		  {
		    tree res_op0;
		    res_op0 = captures[1];
		    tree res_op1;
		    res_op1 = captures[2];
		    tree _r;
		    _r = fold_build2_loc (loc, ncmp, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 350, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail540:;
		}
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_237 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (scmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (single_use (captures[0])
 && (TREE_CODE (captures[2]) == INTEGER_CST || TREE_CODE (captures[2]) == VECTOR_CST)
)
    {
      {
 tree otype = TREE_TYPE (captures[2]);
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail553;
	  {
	    tree res_op0;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[1];
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
	      _r1 = fold_build1_loc (loc, BIT_NOT_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	      res_op1 = _r1;
	    }
	    tree _r;
	    _r = fold_build2_loc (loc, scmp, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 362, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail553:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_242 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (sq),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations && ! flag_errno_math
)
    {
      if (REAL_VALUE_NEGATIVE (TREE_REAL_CST (captures[1]))
)
	{
	  if (cmp == EQ_EXPR || cmp == LT_EXPR || cmp == LE_EXPR
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail572;
	      {
		tree _r;
		_r =  constant_boolean_node (false, type);
		if (TREE_SIDE_EFFECTS (captures[0]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 380, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail572:;
	    }
	  else
	    {
	      if (cmp == NE_EXPR || !HONOR_NANS (captures[0])
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail573;
		  {
		    tree _r;
		    _r =  constant_boolean_node (true, type);
		    if (TREE_SIDE_EFFECTS (captures[0]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
		    if (TREE_SIDE_EFFECTS (captures[1]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 381, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail573:;
		}
	      else
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail574;
		  {
		    tree res_op0;
		    res_op0 = captures[0];
		    tree res_op1;
		    res_op1 =  build_real (TREE_TYPE (captures[0]), dconst0);
		    tree _r;
		    _r = fold_build2_loc (loc, GE_EXPR, type, res_op0, res_op1);
		    if (TREE_SIDE_EFFECTS (captures[1]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 382, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail574:;
		}
	    }
	}
      else
	{
	  if (real_equal (TREE_REAL_CST_PTR (captures[1]), &dconst0)
)
	    {
	      if (cmp == LT_EXPR
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail575;
		  {
		    tree _r;
		    _r =  constant_boolean_node (false, type);
		    if (TREE_SIDE_EFFECTS (captures[0]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
		    if (TREE_SIDE_EFFECTS (captures[1]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 383, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail575:;
		}
	      else
		{
		  if (cmp == GE_EXPR && !HONOR_NANS (captures[0])
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail576;
		      {
			tree _r;
			_r =  constant_boolean_node (true, type);
			if (TREE_SIDE_EFFECTS (captures[0]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
			if (TREE_SIDE_EFFECTS (captures[1]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 384, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail576:;
		    }
		  else
		    {
		      if (cmp == LE_EXPR
)
			{
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail577;
			  {
			    tree res_op0;
			    res_op0 = captures[0];
			    tree res_op1;
			    res_op1 = captures[1];
			    tree _r;
			    _r = fold_build2_loc (loc, EQ_EXPR, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 385, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail577:;
			}
		      else
			{
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail578;
			  {
			    tree res_op0;
			    res_op0 = captures[0];
			    tree res_op1;
			    res_op1 = captures[1];
			    tree _r;
			    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 386, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail578:;
			}
		    }
		}
	    }
	  else
	    {
	      if ((cmp == LT_EXPR
 || cmp == LE_EXPR
 || cmp == GT_EXPR
 || cmp == GE_EXPR)
 && !REAL_VALUE_ISNAN (TREE_REAL_CST (captures[1]))
 && !HONOR_SIGN_DEPENDENT_ROUNDING (TREE_TYPE (captures[0]))
)
		{
		  {
 REAL_VALUE_TYPE c2;
 enum tree_code ncmp = cmp;
 const real_format *fmt
 = REAL_MODE_FORMAT (TYPE_MODE (TREE_TYPE (captures[0])));
 real_arithmetic (&c2, MULT_EXPR,
 &TREE_REAL_CST (captures[1]), &TREE_REAL_CST (captures[1]));
 real_convert (&c2, fmt, &c2);
 if (!REAL_VALUE_ISINF (c2))
 {
 tree c3 = fold_const_call (CFN_SQRT, TREE_TYPE (captures[0]),
 build_real (TREE_TYPE (captures[0]), c2));
 if (c3 == NULL_TREE || TREE_CODE (c3) != REAL_CST)
 ncmp = ERROR_MARK;
 else if ((cmp == LT_EXPR || cmp == GE_EXPR)
 && real_less (&TREE_REAL_CST (c3), &TREE_REAL_CST (captures[1])))
 ncmp = cmp == LT_EXPR ? LE_EXPR : GT_EXPR;
 else if ((cmp == LE_EXPR || cmp == GT_EXPR)
 && real_less (&TREE_REAL_CST (captures[1]), &TREE_REAL_CST (c3)))
 ncmp = cmp == LE_EXPR ? LT_EXPR : GE_EXPR;
 else
 {
 REAL_VALUE_TYPE c2alt, tow;
 if (cmp == LT_EXPR || cmp == GE_EXPR)
 tow = dconst0;
 else
 tow = dconstinf;
 real_nextafter (&c2alt, fmt, &c2, &tow);
 real_convert (&c2alt, fmt, &c2alt);
 if (REAL_VALUE_ISINF (c2alt))
 ncmp = ERROR_MARK;
 else
 {
 c3 = fold_const_call (CFN_SQRT, TREE_TYPE (captures[0]),
 build_real (TREE_TYPE (captures[0]), c2alt));
 if (c3 == NULL_TREE || TREE_CODE (c3) != REAL_CST)
 ncmp = ERROR_MARK;
 else if (real_equal (&TREE_REAL_CST (c3),
 &TREE_REAL_CST (captures[1])))
 c2 = c2alt;
 }
 }
 }
		      if (cmp == GT_EXPR || cmp == GE_EXPR
)
			{
			  if (REAL_VALUE_ISINF (c2)
)
			    {
			      if (HONOR_INFINITIES (captures[0])
)
				{
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail579;
				  {
				    tree res_op0;
				    res_op0 = captures[0];
				    tree res_op1;
				    res_op1 =  build_real (TREE_TYPE (captures[0]), c2);
				    tree _r;
				    _r = fold_build2_loc (loc, EQ_EXPR, type, res_op0, res_op1);
				    if (TREE_SIDE_EFFECTS (captures[1]))
				      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 387, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail579:;
				}
			      else
				{
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail580;
				  {
				    tree _r;
				    _r =  constant_boolean_node (false, type);
				    if (TREE_SIDE_EFFECTS (captures[0]))
				      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
				    if (TREE_SIDE_EFFECTS (captures[1]))
				      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 388, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail580:;
				}
			    }
			  else
			    {
			      if (ncmp != ERROR_MARK
)
				{
				  if (ncmp == GE_EXPR
)
				    {
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail581;
				      {
					tree res_op0;
					res_op0 = captures[0];
					tree res_op1;
					res_op1 =  build_real (TREE_TYPE (captures[0]), c2);
					tree _r;
					_r = fold_build2_loc (loc, GE_EXPR, type, res_op0, res_op1);
					if (TREE_SIDE_EFFECTS (captures[1]))
					  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 389, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail581:;
				    }
				  else
				    {
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail582;
				      {
					tree res_op0;
					res_op0 = captures[0];
					tree res_op1;
					res_op1 =  build_real (TREE_TYPE (captures[0]), c2);
					tree _r;
					_r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
					if (TREE_SIDE_EFFECTS (captures[1]))
					  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 390, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail582:;
				    }
				}
			    }
			}
		      else
			{
			  if (REAL_VALUE_ISINF (c2)
)
			    {
			      if (! HONOR_NANS (captures[0]) && ! HONOR_INFINITIES (captures[0])
)
				{
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail583;
				  {
				    tree _r;
				    _r =  constant_boolean_node (true, type);
				    if (TREE_SIDE_EFFECTS (captures[0]))
				      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
				    if (TREE_SIDE_EFFECTS (captures[1]))
				      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 391, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail583:;
				}
			      else
				{
				  if (! HONOR_NANS (captures[0])
)
				    {
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail584;
				      {
					tree res_op0;
					res_op0 = captures[0];
					tree res_op1;
					res_op1 =  build_real (TREE_TYPE (captures[0]), c2);
					tree _r;
					_r = fold_build2_loc (loc, NE_EXPR, type, res_op0, res_op1);
					if (TREE_SIDE_EFFECTS (captures[1]))
					  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 392, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail584:;
				    }
				  else
				    {
				      if (! HONOR_INFINITIES (captures[0])
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail585;
					  {
					    tree res_op0;
					    res_op0 = captures[0];
					    tree res_op1;
					    res_op1 =  build_real (TREE_TYPE (captures[0]), dconst0);
					    tree _r;
					    _r = fold_build2_loc (loc, GE_EXPR, type, res_op0, res_op1);
					    if (TREE_SIDE_EFFECTS (captures[1]))
					      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 393, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail585:;
					}
				      else
					{
					  if (1
)
					    {
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail586;
					      {
						if (! tree_invariant_p (captures[0])) goto next_after_fail586;
						tree res_op0;
						{
						  tree _o1[2], _r1;
						  _o1[0] = unshare_expr (captures[0]);
						  _o1[1] =  build_real (TREE_TYPE (captures[0]), dconst0);
						  _r1 = fold_build2_loc (loc, GE_EXPR, type, _o1[0], _o1[1]);
						  res_op0 = _r1;
						}
						tree res_op1;
						{
						  tree _o1[2], _r1;
						  _o1[0] = captures[0];
						  _o1[1] =  build_real (TREE_TYPE (captures[0]), c2);
						  _r1 = fold_build2_loc (loc, NE_EXPR, type, _o1[0], _o1[1]);
						  res_op1 = _r1;
						}
						tree _r;
						_r = fold_build2_loc (loc, TRUTH_ANDIF_EXPR, type, res_op0, res_op1);
						if (TREE_SIDE_EFFECTS (captures[1]))
						  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 394, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail586:;
					    }
					}
				    }
				}
			    }
			  else
			    {
			      if (ncmp != ERROR_MARK && ! HONOR_NANS (captures[0])
)
				{
				  if (ncmp == LT_EXPR
)
				    {
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail587;
				      {
					tree res_op0;
					res_op0 = captures[0];
					tree res_op1;
					res_op1 =  build_real (TREE_TYPE (captures[0]), c2);
					tree _r;
					_r = fold_build2_loc (loc, LT_EXPR, type, res_op0, res_op1);
					if (TREE_SIDE_EFFECTS (captures[1]))
					  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 395, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail587:;
				    }
				  else
				    {
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail588;
				      {
					tree res_op0;
					res_op0 = captures[0];
					tree res_op1;
					res_op1 =  build_real (TREE_TYPE (captures[0]), c2);
					tree _r;
					_r = fold_build2_loc (loc, LE_EXPR, type, res_op0, res_op1);
					if (TREE_SIDE_EFFECTS (captures[1]))
					  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 396, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail588:;
				    }
				}
			      else
				{
				  if (ncmp != ERROR_MARK &&
1
)
				    {
				      if (ncmp == LT_EXPR
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail589;
					  {
					    if (! tree_invariant_p (captures[0])) goto next_after_fail589;
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      _o1[0] = unshare_expr (captures[0]);
					      _o1[1] =  build_real (TREE_TYPE (captures[0]), dconst0);
					      _r1 = fold_build2_loc (loc, GE_EXPR, type, _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree res_op1;
					    {
					      tree _o1[2], _r1;
					      _o1[0] = captures[0];
					      _o1[1] =  build_real (TREE_TYPE (captures[0]), c2);
					      _r1 = fold_build2_loc (loc, LT_EXPR, type, _o1[0], _o1[1]);
					      res_op1 = _r1;
					    }
					    tree _r;
					    _r = fold_build2_loc (loc, TRUTH_ANDIF_EXPR, type, res_op0, res_op1);
					    if (TREE_SIDE_EFFECTS (captures[1]))
					      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 397, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail589:;
					}
				      else
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail590;
					  {
					    if (! tree_invariant_p (captures[0])) goto next_after_fail590;
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      _o1[0] = unshare_expr (captures[0]);
					      _o1[1] =  build_real (TREE_TYPE (captures[0]), dconst0);
					      _r1 = fold_build2_loc (loc, GE_EXPR, type, _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree res_op1;
					    {
					      tree _o1[2], _r1;
					      _o1[0] = captures[0];
					      _o1[1] =  build_real (TREE_TYPE (captures[0]), c2);
					      _r1 = fold_build2_loc (loc, LE_EXPR, type, _o1[0], _o1[1]);
					      res_op1 = _r1;
					    }
					    tree _r;
					    _r = fold_build2_loc (loc, TRUTH_ANDIF_EXPR, type, res_op0, res_op1);
					    if (TREE_SIDE_EFFECTS (captures[1]))
					      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 398, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail590:;
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
generic_simplify_294 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (div))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_UNSIGNED (type)
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail702;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree _r;
	_r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 491, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail702:;
    }
  return NULL_TREE;
}

tree
generic_simplify_299 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (div))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 wi::overflow_type overflow;
 wide_int mul = wi::mul (wi::to_wide (captures[2]), wi::to_wide (captures[3]),
 TYPE_SIGN (type), &overflow);
      if (div == EXACT_DIV_EXPR
 || optimize_successive_divisions_p (captures[3], captures[0])
)
	{
	  if (!overflow
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail707;
	      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail707;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail707;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		res_op1 =  wide_int_to_tree (type, mul);
		tree _r;
		_r = fold_build2_loc (loc, div, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 496, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail707:;
	    }
	  else
	    {
	      if (TYPE_UNSIGNED (type)
 || mul != wi::min_value (TYPE_PRECISION (type), SIGNED)
)
		{
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail708;
		  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail708;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail708;
		  {
		    tree _r;
		    _r =  build_zero_cst (type);
		    if (TREE_SIDE_EFFECTS (captures[1]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 497, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail708:;
		}
	    }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_310 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (mod))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((TYPE_UNSIGNED (type) || tree_expr_nonnegative_p (captures[0]))
 && INTEGRAL_TYPE_P (type)
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && (TYPE_PRECISION (type) <= TYPE_PRECISION (TREE_TYPE (captures[1]))
 || TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 || !TYPE_UNSIGNED (type))
 && integer_pow2p (captures[2]) && tree_int_cst_sgn (captures[2]) > 0
)
    {
      {
 tree utype = TREE_TYPE (captures[1]);
 if (!TYPE_OVERFLOW_WRAPS (utype))
 utype = unsigned_type_for (utype);
	  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail719;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail719;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree res_op1;
	    {
	      tree _o1[1], _r1;
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
		_o2[1] =  build_one_cst (utype);
		_r2 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
		_o1[0] = _r2;
	      }
	      if (TREE_TYPE (_o1[0]) != type)
		{
		  _r1 = fold_build1_loc (loc, NOP_EXPR, type, _o1[0]);
		}
	      else
	        _r1 = _o1[0];
	      res_op1 = _r1;
	    }
	    tree _r;
	    _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 508, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail719:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_324 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))
)
    {
      if (tree_expr_nonnegative_p (captures[1]) && tree_expr_nonzero_p (captures[1])
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail736;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree res_op1;
	    res_op1 = captures[2];
	    tree _r;
	    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 525, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail736:;
	}
      else
	{
	  if (TREE_CODE (captures[1]) == INTEGER_CST
 && wi::neg_p (wi::to_wide (captures[1]), TYPE_SIGN (TREE_TYPE (captures[1])))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail737;
	      {
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		res_op1 = captures[0];
		tree _r;
		_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 526, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail737:;
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_338 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail769;
		  {
		    tree _r;
		    _r =  constant_boolean_node (false, type);
		    if (TREE_SIDE_EFFECTS (captures[0]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
		    if (TREE_SIDE_EFFECTS (captures[2]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 542, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail769:;
		}
	      else
		{
		  if (cmp == GE_EXPR
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail770;
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
next_after_fail770:;
		    }
		  else
		    {
		      if (cmp == LE_EXPR
)
			{
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail771;
			  {
			    tree _r;
			    _r =  constant_boolean_node (true, type);
			    if (TREE_SIDE_EFFECTS (captures[0]))
			      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
			    if (TREE_SIDE_EFFECTS (captures[2]))
			      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 544, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail771:;
			}
		      else
			{
			  if (cmp == LT_EXPR
)
			    {
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail772;
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
next_after_fail772:;
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
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail773;
		      {
			tree _r;
			_r =  constant_boolean_node (false, type);
			if (TREE_SIDE_EFFECTS (captures[0]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
			if (TREE_SIDE_EFFECTS (captures[2]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 546, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail773:;
		    }
		  else
		    {
		      if (cmp == LE_EXPR
)
			{
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail774;
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
next_after_fail774:;
			}
		      else
			{
			  if (cmp == GE_EXPR
)
			    {
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail775;
			      {
				tree _r;
				_r =  constant_boolean_node (true, type);
				if (TREE_SIDE_EFFECTS (captures[0]))
				  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
				if (TREE_SIDE_EFFECTS (captures[2]))
				  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 548, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail775:;
			    }
			  else
			    {
			      if (cmp == GT_EXPR
)
				{
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail776;
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
next_after_fail776:;
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail777;
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
next_after_fail777:;
			}
		      else
			{
			  if (cmp == LE_EXPR
)
			    {
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail778;
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
next_after_fail778:;
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
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail779;
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
next_after_fail779:;
			    }
			  else
			    {
			      if (cmp == LT_EXPR
)
				{
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail780;
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
next_after_fail780:;
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
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail781;
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
next_after_fail781:;
					}
				      else
					{
					  if (cst == captures[2] && cmp == GT_EXPR
)
					    {
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail782;
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
next_after_fail782:;
					    }
					  else
					    {
					      if (cmp == LE_EXPR
)
						{
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail783;
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
next_after_fail783:;
						}
					      else
						{
						  if (cmp == GT_EXPR
)
						    {
						      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail784;
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
next_after_fail784:;
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
generic_simplify_385 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (copysigns),
 const combined_fn ARG_UNUSED (hypots))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail862;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 = captures[1];
    tree _r;
    _r = maybe_build_call_expr_loc (loc, hypots, type, 2, res_op0, res_op1);
    if (!_r)
      goto next_after_fail862;
    if (TREE_SIDE_EFFECTS (captures[2]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 616, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail862:;
  return NULL_TREE;
}

tree
generic_simplify_391 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail868;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 = captures[1];
    tree _r;
    _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 622, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail868:;
  return NULL_TREE;
}

tree
generic_simplify_395 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail872;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail872;
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
next_after_fail872:;
  return NULL_TREE;
}

tree
generic_simplify_401 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail878;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 = captures[1];
    tree _r;
    _r = fold_build2_loc (loc, BIT_XOR_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 631, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail878:;
  return NULL_TREE;
}

tree
generic_simplify_405 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail882;
  {
    tree _r;
    _r =  build_zero_cst (type);
    if (TREE_SIDE_EFFECTS (captures[0]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 635, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail882:;
  return NULL_TREE;
}

tree
generic_simplify_409 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail886;
	      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail886;
	      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail886;
	      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail886;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail886;
	      {
		tree _r;
		_r =  build_int_cst (type, 0);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 638, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail886:;
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
					  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail887;
					  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail887;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail887;
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
next_after_fail887:;
					}
				      else
					{
					  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail888;
					  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail888;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail888;
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
next_after_fail888:;
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
generic_simplify_429 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail947;
	      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail947;
	      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail947;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail947;
	      {
		tree _r;
		_r =  build_int_cst (type, 0);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 638, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail947:;
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
					  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail948;
					  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail948;
					  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail948;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail948;
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
next_after_fail948:;
					}
				      else
					{
					  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail949;
					  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail949;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail949;
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
next_after_fail949:;
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
generic_simplify_451 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
 && wi::to_wide (captures[1]) == element_precision (type) - 1
)
    {
      {
 tree stype = TREE_TYPE (captures[0]);
 tree ntype = TYPE_UNSIGNED (stype) ? signed_type_for (stype)
 : unsigned_type_for (stype);
	  if (VECTOR_TYPE_P (type)
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail973;
	      {
		tree res_op0;
		{
		  tree _o1[2], _r1;
		  {
		    tree _o2[1], _r2;
		    _o2[0] = captures[0];
		    _r2 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, ntype, _o2[0]);
		    _o1[0] = _r2;
		  }
		  _o1[1] = captures[1];
		  _r1 = fold_build2_loc (loc, RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  res_op0 = _r1;
		}
		tree _r;
		_r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 685, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail973:;
	    }
	  else
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail974;
	      {
		tree res_op0;
		{
		  tree _o1[2], _r1;
		  {
		    tree _o2[1], _r2;
		    _o2[0] = captures[0];
		    if (TREE_TYPE (_o2[0]) != ntype)
		      {
			_r2 = fold_build1_loc (loc, NOP_EXPR, ntype, _o2[0]);
		      }
		    else
		      _r2 = _o2[0];
		    _o1[0] = _r2;
		  }
		  _o1[1] = captures[1];
		  _r1 = fold_build2_loc (loc, RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  res_op0 = _r1;
		}
		tree _r;
		_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 686, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail974:;
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_468 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (minmax))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!tree_expr_maybe_signaling_nan_p (captures[0])
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1003;
      {
	tree _r;
	_r = captures[0];
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 702, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1003:;
    }
  return NULL_TREE;
}

tree
generic_simplify_473 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree from_type = TREE_TYPE (captures[0]), to_type = TREE_TYPE (captures[1]);
      if (types_match (from_type, to_type)
 || (TYPE_UNSIGNED (from_type)
 && !TYPE_UNSIGNED (to_type)
 && TYPE_PRECISION (from_type) == TYPE_PRECISION (to_type)
 && integer_zerop (captures[1])
 && (cmp == LT_EXPR || cmp == GE_EXPR))
)
	{
	  {
 wi::overflow_type overflow = wi::OVF_NONE;
 enum tree_code code, cmp_code = cmp;
 wide_int real_c1;
 wide_int c1 = wi::to_wide (captures[1]);
 wide_int c2 = wi::to_wide (captures[2]);
 wide_int c3 = wi::to_wide (captures[3]);
 signop sgn = TYPE_SIGN (from_type);
 if (!types_match (from_type, to_type))
 {
 if (cmp_code == LT_EXPR)
 cmp_code = GT_EXPR;
 if (cmp_code == GE_EXPR)
 cmp_code = LE_EXPR;
 c1 = wi::max_value (to_type);
 }
 if (op == PLUS_EXPR)
 real_c1 = wi::sub (c3, c2, sgn, &overflow);
 else
 real_c1 = wi::add (c3, c2, sgn, &overflow);
 code = cmp_code;
 if (!overflow || !TYPE_OVERFLOW_UNDEFINED (from_type))
 {
 if (!wi::cmp (wi::sub (real_c1, 1, sgn, &overflow), c1, sgn)
 && !overflow)
 {
 if (cmp_code == LE_EXPR)
 code = LT_EXPR;
 if (cmp_code == GT_EXPR)
 code = GE_EXPR;
 }
 if (!wi::cmp (wi::add (real_c1, 1, sgn, &overflow), c1, sgn)
 && !overflow)
 {
 if (cmp_code == LT_EXPR)
 code = LE_EXPR;
 if (cmp_code == GE_EXPR)
 code = GT_EXPR;
 }
 if (code != cmp_code || !wi::cmp (real_c1, c1, sgn))
 {
 if (cmp_code == LT_EXPR || cmp_code == LE_EXPR)
 code = MIN_EXPR;
 if (cmp_code == GT_EXPR || cmp_code == GE_EXPR)
 code = MAX_EXPR;
 }
 }
	      if (code == MAX_EXPR
)
		{
		  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1010;
		  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1010;
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1010;
		  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1010;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1010;
		  {
		    tree res_op0;
		    {
		      tree _o1[2], _r1;
		      _o1[0] = captures[0];
		      _o1[1] =  wide_int_to_tree (from_type, real_c1);
		      _r1 = fold_build2_loc (loc, MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		      res_op0 = _r1;
		    }
		    tree res_op1;
		    res_op1 =  wide_int_to_tree (from_type, c2);
		    tree _r;
		    _r = fold_build2_loc (loc, op, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 709, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1010:;
		}
	      else
		{
		  if (code == MIN_EXPR
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1011;
		      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1011;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1011;
		      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1011;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1011;
		      {
			tree res_op0;
			{
			  tree _o1[2], _r1;
			  _o1[0] = captures[0];
			  _o1[1] =  wide_int_to_tree (from_type, real_c1);
			  _r1 = fold_build2_loc (loc, MIN_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			  res_op0 = _r1;
			}
			tree res_op1;
			res_op1 =  wide_int_to_tree (from_type, c2);
			tree _r;
			_r = fold_build2_loc (loc, op, type, res_op0, res_op1);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 710, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1011:;
		    }
		}
	  }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_501 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (bswap))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1046;
  {
    tree _r;
    _r = captures[0];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 744, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1046:;
  return NULL_TREE;
}

tree
generic_simplify_505 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1050;
  if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1050;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1050;
  {
    tree _r;
    _r = captures[2];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 727, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1050:;
  return NULL_TREE;
}

tree
generic_simplify_511 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (cond_op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree op_type = TREE_TYPE (captures[4]);
      if (element_precision (type) == element_precision (op_type)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1056;
	  {
	    tree res_op0;
	    {
	      tree _o1[5], _r1;
	      _o1[0] = captures[0];
	      _o1[1] = captures[1];
	      _o1[2] = captures[2];
	      _o1[3] = captures[3];
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[5];
		_r2 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, op_type, _o2[0]);
		_o1[4] = _r2;
	      }
	      _r1 = maybe_build_call_expr_loc (loc, cond_op, TREE_TYPE (_o1[1]), 5, _o1[0], _o1[1], _o1[2], _o1[3], _o1[4]);
	      if (!_r1)
	        goto next_after_fail1056;
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
	    if (TREE_SIDE_EFFECTS (captures[4]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[4]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 749, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1056:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_518 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (cond_op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1063;
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
    tree res_op4;
    res_op4 = captures[6];
    tree _r;
    _r = maybe_build_call_expr_loc (loc, cond_op, type, 5, res_op0, res_op1, res_op2, res_op3, res_op4);
    if (!_r)
      goto next_after_fail1063;
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 756, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1063:;
  return NULL_TREE;
}

tree
generic_simplify_530 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (fns))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1075;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree _r;
    _r = maybe_build_call_expr_loc (loc, fns, type, 1, res_op0);
    if (!_r)
      goto next_after_fail1075;
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 768, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1075:;
  return NULL_TREE;
}

tree
generic_simplify_533 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (froms),
 const combined_fn ARG_UNUSED (tos))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (optimize && canonicalize_math_p ()
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1078;
      {
	tree res_op0;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  _r1 = maybe_build_call_expr_loc (loc, tos, TREE_TYPE (_o1[0]), 1, _o1[0]);
	  if (!_r1)
	    goto next_after_fail1078;
	  res_op0 = _r1;
	}
	tree _r;
	_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 771, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1078:;
    }
  return NULL_TREE;
}

tree
generic_simplify_542 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (ifn),
 const combined_fn ARG_UNUSED (lfn),
 const combined_fn ARG_UNUSED (llfn))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (canonicalize_math_p ()
)
    {
      if (TYPE_PRECISION (long_long_integer_type_node)
 == TYPE_PRECISION (long_integer_type_node)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1087;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree _r;
	    _r = maybe_build_call_expr_loc (loc, lfn, type, 1, res_op0);
	    if (!_r)
	      goto next_after_fail1087;
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 780, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1087:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_553 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (canonicalize_math_after_vectorization_p ()
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1098;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 = captures[1];
	tree res_op2;
	res_op2 = captures[2];
	tree _r;
	_r = maybe_build_call_expr_loc (loc, CFN_FMS, type, 3, res_op0, res_op1, res_op2);
	if (!_r)
	  goto next_after_fail1098;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 791, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1098:;
    }
  return NULL_TREE;
}

tree
generic_simplify_562 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1107;
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
		  goto next_after_fail1107;
		if (TREE_SIDE_EFFECTS (captures[3]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 795, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1107:;
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_ABS_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    CASE_CONVERT:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	  if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && !TYPE_UNSIGNED (TREE_TYPE (captures[0]))
 && element_precision (type) > element_precision (TREE_TYPE (captures[0]))
)
	    {
	      {
 tree utype = unsigned_type_for (TREE_TYPE (captures[0]));
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1119;
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
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 805, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1119:;
	      }
	    }
	}
        break;
      }
    case ABS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q20 };
	  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1120;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1120;
	  {
	    tree _r;
	    _r = captures[0];
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 806, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1120:;
	}
        break;
      }
    case NEGATE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1121;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree _r;
	    _r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 807, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1121:;
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1122;
	  {
	    tree res_op0;
	    res_op0 = captures[1];
	    tree res_op1;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[2];
	      _r1 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	      if (EXPR_P (_r1))
	        goto next_after_fail1122;
	      res_op1 = _r1;
	    }
	    tree res_op2;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[3];
	      _r1 = fold_build1_loc (loc, ABS_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	      if (EXPR_P (_r1))
	        goto next_after_fail1122;
	      res_op2 = _r1;
	    }
	    tree _r;
	    _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 808, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1122:;
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
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		tree res = generic_simplify_1 (loc, type, _p0, captures, CFN_BUILT_IN_COPYSIGNF16);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF32:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		tree res = generic_simplify_1 (loc, type, _p0, captures, CFN_BUILT_IN_COPYSIGNF32);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF64:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		tree res = generic_simplify_1 (loc, type, _p0, captures, CFN_BUILT_IN_COPYSIGNF64);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF128X:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		tree res = generic_simplify_1 (loc, type, _p0, captures, CFN_BUILT_IN_COPYSIGNF128X);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF128:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		tree res = generic_simplify_1 (loc, type, _p0, captures, CFN_BUILT_IN_COPYSIGNF128);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF32X:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		tree res = generic_simplify_1 (loc, type, _p0, captures, CFN_BUILT_IN_COPYSIGNF32X);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF64X:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		tree res = generic_simplify_1 (loc, type, _p0, captures, CFN_BUILT_IN_COPYSIGNF64X);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGN:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		tree res = generic_simplify_1 (loc, type, _p0, captures, CFN_BUILT_IN_COPYSIGN);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNF:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		tree res = generic_simplify_1 (loc, type, _p0, captures, CFN_BUILT_IN_COPYSIGNF);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_BUILT_IN_COPYSIGNL:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		tree res = generic_simplify_1 (loc, type, _p0, captures, CFN_BUILT_IN_COPYSIGNL);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COPYSIGN:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		tree res = generic_simplify_1 (loc, type, _p0, captures, CFN_COPYSIGN);
		if (res) return res;
	      }
	    }
	  break;
	default:;
        }
      break;
    default:;
    }
if (tree_expr_nonnegative_p (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1123;
      {
	tree _r;
	_r = captures[0];
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 809, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1123:;
    }
  }
  return NULL_TREE;
}

tree
generic_simplify (location_t loc, enum tree_code code, const tree type ATTRIBUTE_UNUSED, tree _p0)
{
  switch (code)
    {
    case ABS_EXPR:
      return generic_simplify_ABS_EXPR (loc, code, type, _p0);
    CASE_CONVERT:
      return generic_simplify_CONVERT_EXPR (loc, code, type, _p0);
    case BIT_NOT_EXPR:
      return generic_simplify_BIT_NOT_EXPR (loc, code, type, _p0);
    case ABSU_EXPR:
      return generic_simplify_ABSU_EXPR (loc, code, type, _p0);
    case NEGATE_EXPR:
      return generic_simplify_NEGATE_EXPR (loc, code, type, _p0);
    case VIEW_CONVERT_EXPR:
      return generic_simplify_VIEW_CONVERT_EXPR (loc, code, type, _p0);
    case FLOAT_EXPR:
      return generic_simplify_FLOAT_EXPR (loc, code, type, _p0);
    case FIX_TRUNC_EXPR:
      return generic_simplify_FIX_TRUNC_EXPR (loc, code, type, _p0);
    case PAREN_EXPR:
      return generic_simplify_PAREN_EXPR (loc, code, type, _p0);
    case REALPART_EXPR:
      return generic_simplify_REALPART_EXPR (loc, code, type, _p0);
    case IMAGPART_EXPR:
      return generic_simplify_IMAGPART_EXPR (loc, code, type, _p0);
    case CONJ_EXPR:
      return generic_simplify_CONJ_EXPR (loc, code, type, _p0);
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_PLUS_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1238;
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1238;
	{
	  tree res_op0;
	  res_op0 = captures[0];
	  tree _r;
	  _r = non_lvalue_loc (loc, res_op0);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 862, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1238:;
      }
    }
  if (real_zerop (_p1))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	tree res = generic_simplify_2 (loc, type, _p0, _p1, captures);
	if (res) return res;
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
		      tree res = generic_simplify_3 (loc, type, _p0, _p1, captures, PLUS_EXPR, PLUS_EXPR);
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
		      tree res = generic_simplify_3 (loc, type, _p0, _p1, captures, PLUS_EXPR, MINUS_EXPR);
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
	if (integer_zerop (_q21))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case MAX_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  switch (TREE_CODE (_q50))
		    {
		    case NEGATE_EXPR:
		      {
			tree _q60 = TREE_OPERAND (_q50, 0);
			if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
			  {
			    if (integer_zerop (_q51))
			      {
				{
				  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
				  tree res = generic_simplify_4 (loc, type, _p0, _p1, captures);
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
	switch (TREE_CODE (_q20))
	  {
	  case NEGATE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      if (integer_zerop (_q21))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case MAX_EXPR:
		      {
			tree _q60 = TREE_OPERAND (_p1, 0);
			tree _q61 = TREE_OPERAND (_p1, 1);
			if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
			  {
			    if (integer_zerop (_q61))
			      {
				{
				  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
				  tree res = generic_simplify_4 (loc, type, _p0, _p1, captures);
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
	    }
          default:;
          }
        break;
      }
    case MULT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  case PLUS_EXPR:
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
 wi::overflow_type ovf1;
 wi::overflow_type ovf2;
 wide_int mul = wi::mul (wi::to_wide (captures[3]), wi::to_wide (captures[4]),
 TYPE_SIGN (type), &ovf1);
 wide_int add = wi::add (mul, wi::to_wide (captures[5]),
 TYPE_SIGN (type), &ovf2);
 if (TYPE_OVERFLOW_UNDEFINED (type))
 {
 }
 else
 overflowed = false;
				      if (!overflowed
)
					{
					  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1239;
					  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1239;
					  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail1239;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1239;
					  {
					    tree res_op0;
					    {
					      tree _o1[2], _r1;
					      _o1[0] = captures[2];
					      _o1[1] = captures[4];
					      _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					      res_op0 = _r1;
					    }
					    tree res_op1;
					    res_op1 =  wide_int_to_tree (type, add);
					    tree _r;
					    _r = fold_build2_loc (loc, PLUS_EXPR, type, res_op0, res_op1);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 863, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1239:;
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
		    tree res = generic_simplify_5 (loc, type, _p0, _p1, captures, PLUS_EXPR);
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
		tree res = generic_simplify_6 (loc, type, _p0, _p1, captures, PLUS_EXPR);
		if (res) return res;
	      }
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q50 };
		tree res = generic_simplify_6 (loc, type, _p0, _p1, captures, PLUS_EXPR);
		if (res) return res;
	      }
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q51 };
		tree res = generic_simplify_6 (loc, type, _p0, _p1, captures, PLUS_EXPR);
		if (res) return res;
	      }
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q50 };
		tree res = generic_simplify_6 (loc, type, _p0, _p1, captures, PLUS_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	if (integer_onep (_q21))
	  {
	    if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	      {
		{
		  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p0, _q21 };
		  tree res = generic_simplify_7 (loc, type, _p0, _p1, captures);
		  if (res) return res;
		}
	      }
	  }
        break;
      }
    default:;
    }
if (real_zerop (_p0))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p1, _p0 };
      tree res = generic_simplify_2 (loc, type, _p0, _p1, captures);
      if (res) return res;
    }
  }
  switch (TREE_CODE (_p1))
    {
    case BIT_AND_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    if (integer_onep (_q31))
	      {
		{
		  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q31 };
		  tree res = generic_simplify_7 (loc, type, _p0, _p1, captures);
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
    case BIT_AND_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
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
			tree res = generic_simplify_8 (loc, type, _p0, _p1, captures, PLUS_EXPR);
			if (res) return res;
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
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			tree res = generic_simplify_9 (loc, type, _p0, _p1, captures);
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
				  tree res = generic_simplify_10 (loc, type, _p0, _p1, captures, PLUS_EXPR);
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
			    tree res = generic_simplify_10 (loc, type, _p0, _p1, captures, PLUS_EXPR);
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
			tree res = generic_simplify_8 (loc, type, _p0, _p1, captures, PLUS_EXPR);
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
    case BIT_IOR_EXPR:
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
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			tree res = generic_simplify_9 (loc, type, _p0, _p1, captures);
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
	  case BIT_AND_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_q21))
	        {
		case BIT_IOR_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q21, 0);
		    tree _q61 = TREE_OPERAND (_q21, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			if ((_q61 == _q31 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q31, 0) && types_match (_q61, _q31)))
			  {
			    if (integer_all_onesp (_p1))
			      {
				{
				  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q31 };
				  tree res = generic_simplify_11 (loc, type, _p0, _p1, captures);
				  if (res) return res;
				}
			      }
			  }
		      }
		    if ((_q60 == _q31 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q31, 0) && types_match (_q60, _q31)))
		      {
			if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
			  {
			    if (integer_all_onesp (_p1))
			      {
				{
				  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q31, _q30 };
				  tree res = generic_simplify_11 (loc, type, _p0, _p1, captures);
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
	      case BIT_AND_EXPR:
	        {
		  tree _q40 = TREE_OPERAND (_q30, 0);
		  tree _q41 = TREE_OPERAND (_q30, 1);
		  {
		    tree _q21_pops[1];
		    if (tree_nop_convert (_q21, _q21_pops))
		      {
			tree _q70 = _q21_pops[0];
			switch (TREE_CODE (_q70))
			  {
			  case BIT_IOR_EXPR:
			    {
			      tree _q80 = TREE_OPERAND (_q70, 0);
			      tree _q81 = TREE_OPERAND (_q70, 1);
			      if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
				{
				  if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
				    {
				      if (integer_all_onesp (_p1))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q40, _q41 };
					    tree res = generic_simplify_11 (loc, type, _p0, _p1, captures);
					    if (res) return res;
					  }
				        }
				    }
			        }
			      if ((_q80 == _q41 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q41, 0) && types_match (_q80, _q41)))
				{
				  if ((_q81 == _q40 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q40, 0) && types_match (_q81, _q40)))
				    {
				      if (integer_all_onesp (_p1))
					{
					  {
					    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q41, _q40 };
					    tree res = generic_simplify_11 (loc, type, _p0, _p1, captures);
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
	{
	  tree _q21_pops[1];
	  if (tree_nop_convert (_q21, _q21_pops))
	    {
	      tree _q40 = _q21_pops[0];
	      if ((_p1 == _q40 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q40, 0) && types_match (_p1, _q40)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
		    tree res = generic_simplify_12 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	    }
	}
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_12 (loc, type, _p0, _p1, captures);
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
					tree res = generic_simplify_10 (loc, type, _p0, _p1, captures, PLUS_EXPR);
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
				  tree res = generic_simplify_10 (loc, type, _p0, _p1, captures, PLUS_EXPR);
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
	  case NEGATE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _p1, _q30 };
		tree res = generic_simplify_13 (loc, type, _p0, _p1, captures);
		if (res) return res;
	      }
	      break;
	    }
          default:;
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
			    const enum tree_code op = PLUS_EXPR;
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1240;
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1240;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1240;
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
next_after_fail1240:;
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
			    const enum tree_code op = PLUS_EXPR;
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1241;
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1241;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1241;
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
next_after_fail1241:;
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, PLUS_EXPR, LT_EXPR, GE_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, PLUS_EXPR, LE_EXPR, GT_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, PLUS_EXPR, EQ_EXPR, NE_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, PLUS_EXPR, NE_EXPR, EQ_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, PLUS_EXPR, GE_EXPR, LT_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, PLUS_EXPR, GT_EXPR, LE_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, PLUS_EXPR, UNORDERED_EXPR, ORDERED_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, PLUS_EXPR, ORDERED_EXPR, UNORDERED_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, PLUS_EXPR, UNLT_EXPR, GE_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, PLUS_EXPR, UNLE_EXPR, GT_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, PLUS_EXPR, UNGT_EXPR, LE_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, PLUS_EXPR, UNGE_EXPR, LT_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, PLUS_EXPR, UNEQ_EXPR, LTGT_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, PLUS_EXPR, LTGT_EXPR, UNEQ_EXPR);
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
			    const enum tree_code op = PLUS_EXPR;
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1242;
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1242;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1242;
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
next_after_fail1242:;
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
			    const enum tree_code op = PLUS_EXPR;
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1243;
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1243;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1243;
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
next_after_fail1243:;
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, PLUS_EXPR, LT_EXPR, GE_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, PLUS_EXPR, LE_EXPR, GT_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, PLUS_EXPR, EQ_EXPR, NE_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, PLUS_EXPR, NE_EXPR, EQ_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, PLUS_EXPR, GE_EXPR, LT_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, PLUS_EXPR, GT_EXPR, LE_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, PLUS_EXPR, UNORDERED_EXPR, ORDERED_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, PLUS_EXPR, ORDERED_EXPR, UNORDERED_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, PLUS_EXPR, UNLT_EXPR, GE_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, PLUS_EXPR, UNLE_EXPR, GT_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, PLUS_EXPR, UNGT_EXPR, LE_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, PLUS_EXPR, UNGE_EXPR, LT_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, PLUS_EXPR, UNEQ_EXPR, LTGT_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, PLUS_EXPR, LTGT_EXPR, UNEQ_EXPR);
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
    default:;
    }
{
  tree _p0_pops[1];
  if (tree_nop_convert (_p0, _p0_pops))
    {
      tree _q20 = _p0_pops[0];
      switch (TREE_CODE (_q20))
        {
	case MINUS_EXPR:
	  {
	    tree _q30 = TREE_OPERAND (_q20, 0);
	    tree _q31 = TREE_OPERAND (_q20, 1);
	    switch (TREE_CODE (_q30))
	      {
	      case BIT_AND_EXPR:
	        {
		  tree _q40 = TREE_OPERAND (_q30, 0);
		  tree _q41 = TREE_OPERAND (_q30, 1);
		  switch (TREE_CODE (_q31))
		    {
		    case BIT_IOR_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_q31, 0);
			tree _q71 = TREE_OPERAND (_q31, 1);
			if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
			  {
			    if ((_q71 == _q41 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q41, 0) && types_match (_q71, _q41)))
			      {
				if (integer_all_onesp (_p1))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q40, _q41 };
				      tree res = generic_simplify_11 (loc, type, _p0, _p1, captures);
				      if (res) return res;
				    }
				  }
			      }
			  }
			if ((_q70 == _q41 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q41, 0) && types_match (_q70, _q41)))
			  {
			    if ((_q71 == _q40 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q40, 0) && types_match (_q71, _q40)))
			      {
				if (integer_all_onesp (_p1))
				  {
				    {
				      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q41, _q40 };
				      tree res = generic_simplify_11 (loc, type, _p0, _p1, captures);
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
		  case BIT_AND_EXPR:
		    {
		      tree _q50 = TREE_OPERAND (_q40, 0);
		      tree _q51 = TREE_OPERAND (_q40, 1);
		      {
			tree _q31_pops[1];
			if (tree_nop_convert (_q31, _q31_pops))
			  {
			    tree _q80 = _q31_pops[0];
			    switch (TREE_CODE (_q80))
			      {
			      case BIT_IOR_EXPR:
			        {
				  tree _q90 = TREE_OPERAND (_q80, 0);
				  tree _q91 = TREE_OPERAND (_q80, 1);
				  if ((_q90 == _q50 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q50, 0) && types_match (_q90, _q50)))
				    {
				      if ((_q91 == _q51 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q51, 0) && types_match (_q91, _q51)))
					{
					  if (integer_all_onesp (_p1))
					    {
					      {
						tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q50, _q51 };
						tree res = generic_simplify_11 (loc, type, _p0, _p1, captures);
						if (res) return res;
					      }
					    }
				        }
				    }
				  if ((_q90 == _q51 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q51, 0) && types_match (_q90, _q51)))
				    {
				      if ((_q91 == _q50 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q50, 0) && types_match (_q91, _q50)))
					{
					  if (integer_all_onesp (_p1))
					    {
					      {
						tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q51, _q50 };
						tree res = generic_simplify_11 (loc, type, _p0, _p1, captures);
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
		tree res = generic_simplify_13 (loc, type, _p0, _p1, captures);
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
	  tree res = generic_simplify_13 (loc, type, _p0, _p1, captures);
	  if (res) return res;
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
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p1, _q20 };
	  tree res = generic_simplify_13 (loc, type, _p0, _p1, captures);
	  if (res) return res;
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
	case MINUS_EXPR:
	  {
	    tree _q30 = TREE_OPERAND (_q20, 0);
	    tree _q31 = TREE_OPERAND (_q20, 1);
	    {
	      tree _q31_pops[1];
	      if (tree_nop_convert (_q31, _q31_pops))
	        {
		  tree _q50 = _q31_pops[0];
		  if ((_p1 == _q50 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q50, 0) && types_match (_p1, _q50)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q50 };
			tree res = generic_simplify_12 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	    }
	    if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
		  tree res = generic_simplify_12 (loc, type, _p0, _p1, captures);
		  if (res) return res;
		}
	      }
	    if (CONSTANT_CLASS_P (_q31))
	      {
		if (CONSTANT_CLASS_P (_p1))
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
		      tree res = generic_simplify_16 (loc, type, _p0, _p1, captures, MINUS_EXPR, PLUS_EXPR, PLUS_EXPR);
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
		      tree res = generic_simplify_17 (loc, type, _p0, _p1, captures, PLUS_EXPR);
		      if (res) return res;
		    }
		  }
	      }
	    break;
	  }
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
		      tree res = generic_simplify_16 (loc, type, _p0, _p1, captures, PLUS_EXPR, MINUS_EXPR, PLUS_EXPR);
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
  switch (TREE_CODE (_p1))
    {
    case MINUS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	{
	  tree _q31_pops[1];
	  if (tree_nop_convert (_q31, _q31_pops))
	    {
	      tree _q50 = _q31_pops[0];
	      if ((_q50 == _p0 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _p0, 0) && types_match (_q50, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _p0 };
		    tree res = generic_simplify_12 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	    }
	}
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _p0 };
	      tree res = generic_simplify_12 (loc, type, _p0, _p1, captures);
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
	    {
	      tree _q41_pops[1];
	      if (tree_nop_convert (_q41, _q41_pops))
	        {
		  tree _q60 = _q41_pops[0];
		  if ((_q60 == _p0 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _p0, 0) && types_match (_q60, _p0)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q40, _p0 };
			tree res = generic_simplify_12 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	    }
	    if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
	      {
		{
		  tree captures[2] ATTRIBUTE_UNUSED = { _q40, _p0 };
		  tree res = generic_simplify_12 (loc, type, _p0, _p1, captures);
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
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
		    tree res = generic_simplify_18 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
		    tree res = generic_simplify_18 (loc, type, _p0, _p1, captures);
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
		  tree res = generic_simplify_16 (loc, type, _p0, _p1, captures, PLUS_EXPR, MINUS_EXPR, PLUS_EXPR);
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
	switch (TREE_CODE (_p1))
	  {
	  case PLUS_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q51, _q20 };
		    tree res = generic_simplify_18 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q50, _q20 };
		    tree res = generic_simplify_18 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      break;
	    }
	  case MINUS_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
		    tree res = generic_simplify_19 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q51, _q20 };
		    tree res = generic_simplify_19 (loc, type, _p0, _p1, captures);
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
		  tree res = generic_simplify_16 (loc, type, _p0, _p1, captures, MINUS_EXPR, PLUS_EXPR, PLUS_EXPR);
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
		  tree res = generic_simplify_17 (loc, type, _p0, _p1, captures, PLUS_EXPR);
		  if (res) return res;
		}
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
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
		    tree res = generic_simplify_20 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q51, _q20 };
		    tree res = generic_simplify_20 (loc, type, _p0, _p1, captures);
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
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
		tree res = generic_simplify_21 (loc, type, _p0, _p1, captures, PLUS_EXPR);
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
	      switch (TREE_CODE (_p1))
	        {
		CASE_CONVERT:
		  {
		    tree _q50 = TREE_OPERAND (_p1, 0);
		    if ((_q50 == _q30 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q30, 0) && types_match (_q50, _q30)))
		      {
			{
			  tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
			  tree res = generic_simplify_22 (loc, type, _p0, _p1, captures);
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
		case BIT_NOT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			{
			  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			  tree res = generic_simplify_22 (loc, type, _p0, _p1, captures);
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
    case BIT_NOT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	      tree res = generic_simplify_22 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	if (integer_each_onep (_p1))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	      tree res = generic_simplify_23 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
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
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	      tree res = generic_simplify_22 (loc, type, _p0, _p1, captures);
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
	  case BIT_NOT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      if (integer_each_onep (_p1))
		{
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q30 };
		    tree res = generic_simplify_23 (loc, type, _p0, _p1, captures);
		    if (res) return res;
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
		    tree res = generic_simplify_24 (loc, type, _p0, _p1, captures, PLUS_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_24 (loc, type, _p0, _p1, captures, PLUS_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_24 (loc, type, _p0, _p1, captures, PLUS_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_24 (loc, type, _p0, _p1, captures, PLUS_EXPR);
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
	      tree res = generic_simplify_25 (loc, type, _p0, _p1, captures, PLUS_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
	      tree res = generic_simplify_25 (loc, type, _p0, _p1, captures, PLUS_EXPR);
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
	      tree res = generic_simplify_26 (loc, type, _p0, _p1, captures, PLUS_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
	      tree res = generic_simplify_26 (loc, type, _p0, _p1, captures, PLUS_EXPR);
	      if (res) return res;
	    }
	  }
	if (integer_nonzerop (_q20))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case INTEGER_CST:
	        {
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
		    tree res = generic_simplify_27 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	          break;
	        }
	      default:;
	      }
	  }
	if (integer_nonzerop (_q21))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case INTEGER_CST:
	        {
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1 };
		    tree res = generic_simplify_27 (loc, type, _p0, _p1, captures);
		    if (res) return res;
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
	      if (tree_zero_one_valued_p (_q21))
		{
		  if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q31, _q30, _q21 };
			tree res = generic_simplify_28 (loc, type, _p0, _p1, captures);
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
	    case MINUS_EXPR:
	      {
		tree _q40 = TREE_OPERAND (_q21, 0);
		tree _q41 = TREE_OPERAND (_q21, 1);
		if ((_p1 == _q41 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q41, 0) && types_match (_p1, _q41)))
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _q20 };
		      tree res = generic_simplify_28 (loc, type, _p0, _p1, captures);
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
	      if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
		{
		  if (tree_zero_one_valued_p (_q31))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q40, _q31 };
			tree res = generic_simplify_28 (loc, type, _p0, _p1, captures);
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
		if ((_q51 == _p0 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _p0, 0) && types_match (_q51, _p0)))
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q50, _q30 };
		      tree res = generic_simplify_28 (loc, type, _p0, _p1, captures);
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
	  case TRUNC_DIV_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if ((_q21 == _q31 && ! TREE_SIDE_EFFECTS (_q21)) || (operand_equal_p (_q21, _q31, 0) && types_match (_q21, _q31)))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case TRUNC_MOD_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_p1, 0);
			tree _q71 = TREE_OPERAND (_p1, 1);
			if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
			  {
			    if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
				  tree res = generic_simplify_29 (loc, type, _p0, _p1, captures, TRUNC_DIV_EXPR, TRUNC_MOD_EXPR);
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
	    }
	  case CEIL_DIV_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if ((_q21 == _q31 && ! TREE_SIDE_EFFECTS (_q21)) || (operand_equal_p (_q21, _q31, 0) && types_match (_q21, _q31)))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case CEIL_MOD_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_p1, 0);
			tree _q71 = TREE_OPERAND (_p1, 1);
			if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
			  {
			    if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
				  tree res = generic_simplify_29 (loc, type, _p0, _p1, captures, CEIL_DIV_EXPR, CEIL_MOD_EXPR);
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
	    }
	  case FLOOR_DIV_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if ((_q21 == _q31 && ! TREE_SIDE_EFFECTS (_q21)) || (operand_equal_p (_q21, _q31, 0) && types_match (_q21, _q31)))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case FLOOR_MOD_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_p1, 0);
			tree _q71 = TREE_OPERAND (_p1, 1);
			if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
			  {
			    if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
				  tree res = generic_simplify_29 (loc, type, _p0, _p1, captures, FLOOR_DIV_EXPR, FLOOR_MOD_EXPR);
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
	    }
	  case ROUND_DIV_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if ((_q21 == _q31 && ! TREE_SIDE_EFFECTS (_q21)) || (operand_equal_p (_q21, _q31, 0) && types_match (_q21, _q31)))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case ROUND_MOD_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_p1, 0);
			tree _q71 = TREE_OPERAND (_p1, 1);
			if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
			  {
			    if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
				  tree res = generic_simplify_29 (loc, type, _p0, _p1, captures, ROUND_DIV_EXPR, ROUND_MOD_EXPR);
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
	    }
          default:;
          }
	switch (TREE_CODE (_q21))
	  {
	  case TRUNC_DIV_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      if ((_q41 == _q20 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _q20, 0) && types_match (_q41, _q20)))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case TRUNC_MOD_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_p1, 0);
			tree _q71 = TREE_OPERAND (_p1, 1);
			if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
			  {
			    if ((_q71 == _q20 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q20, 0) && types_match (_q71, _q20)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
				  tree res = generic_simplify_29 (loc, type, _p0, _p1, captures, TRUNC_DIV_EXPR, TRUNC_MOD_EXPR);
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
	    }
	  case CEIL_DIV_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      if ((_q41 == _q20 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _q20, 0) && types_match (_q41, _q20)))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case CEIL_MOD_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_p1, 0);
			tree _q71 = TREE_OPERAND (_p1, 1);
			if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
			  {
			    if ((_q71 == _q20 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q20, 0) && types_match (_q71, _q20)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
				  tree res = generic_simplify_29 (loc, type, _p0, _p1, captures, CEIL_DIV_EXPR, CEIL_MOD_EXPR);
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
	    }
	  case FLOOR_DIV_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      if ((_q41 == _q20 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _q20, 0) && types_match (_q41, _q20)))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case FLOOR_MOD_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_p1, 0);
			tree _q71 = TREE_OPERAND (_p1, 1);
			if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
			  {
			    if ((_q71 == _q20 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q20, 0) && types_match (_q71, _q20)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
				  tree res = generic_simplify_29 (loc, type, _p0, _p1, captures, FLOOR_DIV_EXPR, FLOOR_MOD_EXPR);
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
	    }
	  case ROUND_DIV_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      if ((_q41 == _q20 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _q20, 0) && types_match (_q41, _q20)))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case ROUND_MOD_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_p1, 0);
			tree _q71 = TREE_OPERAND (_p1, 1);
			if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
			  {
			    if ((_q71 == _q20 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q20, 0) && types_match (_q71, _q20)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q40, _q20 };
				  tree res = generic_simplify_29 (loc, type, _p0, _p1, captures, ROUND_DIV_EXPR, ROUND_MOD_EXPR);
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
	    }
          default:;
          }
        break;
      }
    case TRUNC_MOD_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case MULT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q50))
	        {
		case TRUNC_DIV_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    tree _q61 = TREE_OPERAND (_q50, 1);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
			  {
			    if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
				  tree res = generic_simplify_29 (loc, type, _p0, _p1, captures, TRUNC_DIV_EXPR, TRUNC_MOD_EXPR);
				  if (res) return res;
				}
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
		    case TRUNC_DIV_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_q51, 0);
			tree _q71 = TREE_OPERAND (_q51, 1);
			if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
			  {
			    if ((_q71 == _q21 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q21, 0) && types_match (_q71, _q21)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
				  tree res = generic_simplify_29 (loc, type, _p0, _p1, captures, TRUNC_DIV_EXPR, TRUNC_MOD_EXPR);
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
	    }
          default:;
          }
        break;
      }
    case CEIL_MOD_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case MULT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q50))
	        {
		case CEIL_DIV_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    tree _q61 = TREE_OPERAND (_q50, 1);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
			  {
			    if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
				  tree res = generic_simplify_29 (loc, type, _p0, _p1, captures, CEIL_DIV_EXPR, CEIL_MOD_EXPR);
				  if (res) return res;
				}
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
		    case CEIL_DIV_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_q51, 0);
			tree _q71 = TREE_OPERAND (_q51, 1);
			if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
			  {
			    if ((_q71 == _q21 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q21, 0) && types_match (_q71, _q21)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
				  tree res = generic_simplify_29 (loc, type, _p0, _p1, captures, CEIL_DIV_EXPR, CEIL_MOD_EXPR);
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
	    }
          default:;
          }
        break;
      }
    case FLOOR_MOD_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case MULT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q50))
	        {
		case FLOOR_DIV_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    tree _q61 = TREE_OPERAND (_q50, 1);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
			  {
			    if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
				  tree res = generic_simplify_29 (loc, type, _p0, _p1, captures, FLOOR_DIV_EXPR, FLOOR_MOD_EXPR);
				  if (res) return res;
				}
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
		    case FLOOR_DIV_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_q51, 0);
			tree _q71 = TREE_OPERAND (_q51, 1);
			if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
			  {
			    if ((_q71 == _q21 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q21, 0) && types_match (_q71, _q21)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
				  tree res = generic_simplify_29 (loc, type, _p0, _p1, captures, FLOOR_DIV_EXPR, FLOOR_MOD_EXPR);
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
	    }
          default:;
          }
        break;
      }
    case ROUND_MOD_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case MULT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q50))
	        {
		case ROUND_DIV_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    tree _q61 = TREE_OPERAND (_q50, 1);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
			  {
			    if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
				  tree res = generic_simplify_29 (loc, type, _p0, _p1, captures, ROUND_DIV_EXPR, ROUND_MOD_EXPR);
				  if (res) return res;
				}
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
		    case ROUND_DIV_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_q51, 0);
			tree _q71 = TREE_OPERAND (_q51, 1);
			if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
			  {
			    if ((_q71 == _q21 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q21, 0) && types_match (_q71, _q21)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
				  tree res = generic_simplify_29 (loc, type, _p0, _p1, captures, ROUND_DIV_EXPR, ROUND_MOD_EXPR);
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
	    }
          default:;
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
	  if (REAL_VALUE_NEGATIVE (TREE_REAL_CST (captures[1]))
)
	    {
	      {
 tree tem = const_unop (NEGATE_EXPR, type, captures[1]);
		  if (!TREE_OVERFLOW (tem) || !flag_trapping_math
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1244;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1244;
		      {
			tree res_op0;
			res_op0 = captures[0];
			tree res_op1;
			res_op1 =  tem;
			tree _r;
			_r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 865, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1244:;
		    }
	      }
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
	if (SCALAR_FLOAT_TYPE_P (type)
)
	  {
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1245;
	    {
	      tree res_op0;
	      res_op0 = captures[0];
	      tree res_op1;
	      res_op1 =  build_real (type, dconst2);
	      tree _r;
	      _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 866, __FILE__, __LINE__, true);
	      return _r;
	    }
next_after_fail1245:;
	  }
	else
	  {
	    if (INTEGRAL_TYPE_P (type)
)
	      {
		if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1246;
		{
		  tree res_op0;
		  res_op0 = captures[0];
		  tree res_op1;
		  res_op1 =  build_int_cst (type, 2);
		  tree _r;
		  _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 867, __FILE__, __LINE__, true);
		  return _r;
		}
next_after_fail1246:;
	      }
	  }
      }
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
 && (TREE_CODE_CLASS (PLUS_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1247;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1247;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1247;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1247:;
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
 && (TREE_CODE_CLASS (PLUS_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1248;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1248;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1248;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1248;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1248:;
	    }
	}
	if (integer_each_onep (_q21))
	  {
	    if (integer_zerop (_q22))
	      {
		{
		  tree captures[5] ATTRIBUTE_UNUSED = { _p1, _p0, _q20, _q21, _q22 };
		  tree res = generic_simplify_30 (loc, type, _p0, _p1, captures);
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
	{
	  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32 };
	  if (VECTOR_TYPE_P (type)
 && (TREE_CODE_CLASS (PLUS_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1249;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1249;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1249;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1249;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1249:;
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
			tree res = generic_simplify_30 (loc, type, _p0, _p1, captures);
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
    case VIEW_CONVERT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_q20))
	  {
	  case VEC_COND_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      tree _q32 = TREE_OPERAND (_q20, 2);
	      if (integer_each_onep (_q31))
		{
		  if (integer_zerop (_q32))
		    {
		      {
			tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q20, _q30, _q31, _q32 };
			tree res = generic_simplify_30 (loc, type, _p0, _p1, captures);
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
    case VEC_COND_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	tree _q32 = TREE_OPERAND (_p1, 2);
	if (integer_each_onep (_q31))
	  {
	    if (integer_zerop (_q32))
	      {
		{
		  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32 };
		  tree res = generic_simplify_30 (loc, type, _p0, _p1, captures);
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
		    tree res = generic_simplify_31 (loc, type, _p0, _p1, captures, PLUS_EXPR);
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
				switch (TREE_CODE (_q50))
				  {
				  case BIT_AND_EXPR:
				    {
				      tree _q60 = TREE_OPERAND (_q50, 0);
				      tree _q61 = TREE_OPERAND (_q50, 1);
				      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case CALL_EXPR:
					      switch (get_call_combined_fn (_p1))
					        {
						case CFN_BUILT_IN_POPCOUNT:
						  if (call_expr_nargs (_p1) == 1)
    {
						      tree _q90 = CALL_EXPR_ARG (_p1, 0);
						      if ((_q90 == _q61 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q61, 0) && types_match (_q90, _q61)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _q61, _p1 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNT);
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
				      if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case CALL_EXPR:
					      switch (get_call_combined_fn (_p1))
					        {
						case CFN_BUILT_IN_POPCOUNT:
						  if (call_expr_nargs (_p1) == 1)
    {
						      tree _q90 = CALL_EXPR_ARG (_p1, 0);
						      if ((_q90 == _q60 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q60, 0) && types_match (_q90, _q60)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _q60, _p1 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNT);
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
				    }
				  case BIT_IOR_EXPR:
				    {
				      tree _q60 = TREE_OPERAND (_q50, 0);
				      tree _q61 = TREE_OPERAND (_q50, 1);
				      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case CALL_EXPR:
					      switch (get_call_combined_fn (_p1))
					        {
						case CFN_BUILT_IN_POPCOUNT:
						  if (call_expr_nargs (_p1) == 1)
    {
						      tree _q90 = CALL_EXPR_ARG (_p1, 0);
						      if ((_q90 == _q61 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q61, 0) && types_match (_q90, _q61)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _q61, _p1 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNT);
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
				      if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case CALL_EXPR:
					      switch (get_call_combined_fn (_p1))
					        {
						case CFN_BUILT_IN_POPCOUNT:
						  if (call_expr_nargs (_p1) == 1)
    {
						      tree _q90 = CALL_EXPR_ARG (_p1, 0);
						      if ((_q90 == _q60 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q60, 0) && types_match (_q90, _q60)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _q60, _p1 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNT);
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
				switch (TREE_CODE (_q50))
				  {
				  case BIT_AND_EXPR:
				    {
				      tree _q60 = TREE_OPERAND (_q50, 0);
				      tree _q61 = TREE_OPERAND (_q50, 1);
				      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case CALL_EXPR:
					      switch (get_call_combined_fn (_p1))
					        {
						case CFN_BUILT_IN_POPCOUNTLL:
						  if (call_expr_nargs (_p1) == 1)
    {
						      tree _q90 = CALL_EXPR_ARG (_p1, 0);
						      if ((_q90 == _q61 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q61, 0) && types_match (_q90, _q61)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _q61, _p1 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTLL);
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
				      if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case CALL_EXPR:
					      switch (get_call_combined_fn (_p1))
					        {
						case CFN_BUILT_IN_POPCOUNTLL:
						  if (call_expr_nargs (_p1) == 1)
    {
						      tree _q90 = CALL_EXPR_ARG (_p1, 0);
						      if ((_q90 == _q60 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q60, 0) && types_match (_q90, _q60)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _q60, _p1 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTLL);
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
				    }
				  case BIT_IOR_EXPR:
				    {
				      tree _q60 = TREE_OPERAND (_q50, 0);
				      tree _q61 = TREE_OPERAND (_q50, 1);
				      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case CALL_EXPR:
					      switch (get_call_combined_fn (_p1))
					        {
						case CFN_BUILT_IN_POPCOUNTLL:
						  if (call_expr_nargs (_p1) == 1)
    {
						      tree _q90 = CALL_EXPR_ARG (_p1, 0);
						      if ((_q90 == _q61 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q61, 0) && types_match (_q90, _q61)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _q61, _p1 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTLL);
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
				      if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case CALL_EXPR:
					      switch (get_call_combined_fn (_p1))
					        {
						case CFN_BUILT_IN_POPCOUNTLL:
						  if (call_expr_nargs (_p1) == 1)
    {
						      tree _q90 = CALL_EXPR_ARG (_p1, 0);
						      if ((_q90 == _q60 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q60, 0) && types_match (_q90, _q60)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _q60, _p1 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTLL);
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
				switch (TREE_CODE (_q50))
				  {
				  case BIT_AND_EXPR:
				    {
				      tree _q60 = TREE_OPERAND (_q50, 0);
				      tree _q61 = TREE_OPERAND (_q50, 1);
				      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case CALL_EXPR:
					      switch (get_call_combined_fn (_p1))
					        {
						case CFN_POPCOUNT:
						  if (call_expr_nargs (_p1) == 1)
    {
						      tree _q90 = CALL_EXPR_ARG (_p1, 0);
						      if ((_q90 == _q61 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q61, 0) && types_match (_q90, _q61)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _q61, _p1 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_POPCOUNT);
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
				      if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case CALL_EXPR:
					      switch (get_call_combined_fn (_p1))
					        {
						case CFN_POPCOUNT:
						  if (call_expr_nargs (_p1) == 1)
    {
						      tree _q90 = CALL_EXPR_ARG (_p1, 0);
						      if ((_q90 == _q60 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q60, 0) && types_match (_q90, _q60)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _q60, _p1 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_POPCOUNT);
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
				    }
				  case BIT_IOR_EXPR:
				    {
				      tree _q60 = TREE_OPERAND (_q50, 0);
				      tree _q61 = TREE_OPERAND (_q50, 1);
				      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case CALL_EXPR:
					      switch (get_call_combined_fn (_p1))
					        {
						case CFN_POPCOUNT:
						  if (call_expr_nargs (_p1) == 1)
    {
						      tree _q90 = CALL_EXPR_ARG (_p1, 0);
						      if ((_q90 == _q61 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q61, 0) && types_match (_q90, _q61)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _q61, _p1 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_POPCOUNT);
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
				      if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case CALL_EXPR:
					      switch (get_call_combined_fn (_p1))
					        {
						case CFN_POPCOUNT:
						  if (call_expr_nargs (_p1) == 1)
    {
						      tree _q90 = CALL_EXPR_ARG (_p1, 0);
						      if ((_q90 == _q60 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q60, 0) && types_match (_q90, _q60)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _q60, _p1 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_POPCOUNT);
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
				switch (TREE_CODE (_q50))
				  {
				  case BIT_AND_EXPR:
				    {
				      tree _q60 = TREE_OPERAND (_q50, 0);
				      tree _q61 = TREE_OPERAND (_q50, 1);
				      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case CALL_EXPR:
					      switch (get_call_combined_fn (_p1))
					        {
						case CFN_BUILT_IN_POPCOUNTIMAX:
						  if (call_expr_nargs (_p1) == 1)
    {
						      tree _q90 = CALL_EXPR_ARG (_p1, 0);
						      if ((_q90 == _q61 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q61, 0) && types_match (_q90, _q61)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _q61, _p1 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTIMAX);
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
				      if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case CALL_EXPR:
					      switch (get_call_combined_fn (_p1))
					        {
						case CFN_BUILT_IN_POPCOUNTIMAX:
						  if (call_expr_nargs (_p1) == 1)
    {
						      tree _q90 = CALL_EXPR_ARG (_p1, 0);
						      if ((_q90 == _q60 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q60, 0) && types_match (_q90, _q60)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _q60, _p1 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTIMAX);
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
				    }
				  case BIT_IOR_EXPR:
				    {
				      tree _q60 = TREE_OPERAND (_q50, 0);
				      tree _q61 = TREE_OPERAND (_q50, 1);
				      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case CALL_EXPR:
					      switch (get_call_combined_fn (_p1))
					        {
						case CFN_BUILT_IN_POPCOUNTIMAX:
						  if (call_expr_nargs (_p1) == 1)
    {
						      tree _q90 = CALL_EXPR_ARG (_p1, 0);
						      if ((_q90 == _q61 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q61, 0) && types_match (_q90, _q61)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _q61, _p1 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTIMAX);
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
				      if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case CALL_EXPR:
					      switch (get_call_combined_fn (_p1))
					        {
						case CFN_BUILT_IN_POPCOUNTIMAX:
						  if (call_expr_nargs (_p1) == 1)
    {
						      tree _q90 = CALL_EXPR_ARG (_p1, 0);
						      if ((_q90 == _q60 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q60, 0) && types_match (_q90, _q60)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _q60, _p1 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTIMAX);
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
				switch (TREE_CODE (_q50))
				  {
				  case BIT_AND_EXPR:
				    {
				      tree _q60 = TREE_OPERAND (_q50, 0);
				      tree _q61 = TREE_OPERAND (_q50, 1);
				      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case CALL_EXPR:
					      switch (get_call_combined_fn (_p1))
					        {
						case CFN_BUILT_IN_POPCOUNTL:
						  if (call_expr_nargs (_p1) == 1)
    {
						      tree _q90 = CALL_EXPR_ARG (_p1, 0);
						      if ((_q90 == _q61 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q61, 0) && types_match (_q90, _q61)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _q61, _p1 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTL);
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
				      if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case CALL_EXPR:
					      switch (get_call_combined_fn (_p1))
					        {
						case CFN_BUILT_IN_POPCOUNTL:
						  if (call_expr_nargs (_p1) == 1)
    {
						      tree _q90 = CALL_EXPR_ARG (_p1, 0);
						      if ((_q90 == _q60 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q60, 0) && types_match (_q90, _q60)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _q60, _p1 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTL);
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
				    }
				  case BIT_IOR_EXPR:
				    {
				      tree _q60 = TREE_OPERAND (_q50, 0);
				      tree _q61 = TREE_OPERAND (_q50, 1);
				      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case CALL_EXPR:
					      switch (get_call_combined_fn (_p1))
					        {
						case CFN_BUILT_IN_POPCOUNTL:
						  if (call_expr_nargs (_p1) == 1)
    {
						      tree _q90 = CALL_EXPR_ARG (_p1, 0);
						      if ((_q90 == _q61 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q61, 0) && types_match (_q90, _q61)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _q61, _p1 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTL);
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
				      if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
					{
					  switch (TREE_CODE (_p1))
					    {
					    case CALL_EXPR:
					      switch (get_call_combined_fn (_p1))
					        {
						case CFN_BUILT_IN_POPCOUNTL:
						  if (call_expr_nargs (_p1) == 1)
    {
						      tree _q90 = CALL_EXPR_ARG (_p1, 0);
						      if ((_q90 == _q60 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q60, 0) && types_match (_q90, _q60)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50, _q60, _p1 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTL);
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
			tree res = generic_simplify_36 (loc, type, _p0, _p1, captures);
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
			tree res = generic_simplify_36 (loc, type, _p0, _p1, captures);
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
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1250;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[0];
				    _o1[1] = captures[2];
				    _r1 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
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
next_after_fail1250:;
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
					tree res = generic_simplify_37 (loc, type, _p0, _p1, captures, PLUS_EXPR);
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
			    tree res = generic_simplify_32 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_LOG);
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
	case CFN_BUILT_IN_POPCOUNT:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_POPCOUNT:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_33 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNT);
			    if (res) return res;
			  }
		        }
		      break;
		    default:;
		    }
		  break;
	        default:;
	        }
	      switch (TREE_CODE (_q20))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_p1))
		          {
			  case CFN_BUILT_IN_POPCOUNT:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q60 = CALL_EXPR_ARG (_p1, 0);
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
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1, _q60 };
						tree res = generic_simplify_34 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNT);
						if (res) return res;
					      }
					    }
				        }
				      if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
					{
					  if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1, _q60 };
						tree res = generic_simplify_34 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNT);
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
		    break;
		  }
		case BIT_IOR_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_p1))
		          {
			  case CFN_BUILT_IN_POPCOUNT:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q60 = CALL_EXPR_ARG (_p1, 0);
				switch (TREE_CODE (_q60))
				  {
				  case BIT_AND_EXPR:
				    {
				      tree _q70 = TREE_OPERAND (_q60, 0);
				      tree _q71 = TREE_OPERAND (_q60, 1);
				      if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
					{
					  if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q60, _q30, _q31, _p0, _q20 };
						tree res = generic_simplify_34 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNT);
						if (res) return res;
					      }
					    }
				        }
				      if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
					{
					  if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q60, _q31, _q30, _p0, _q20 };
						tree res = generic_simplify_34 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNT);
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
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_p1))
	        {
		case MINUS_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_p1, 0);
		    tree _q41 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q40))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_q40))
		          {
			  case CFN_BUILT_IN_POPCOUNT:
			    if (call_expr_nargs (_q40) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_q40, 0);
				switch (TREE_CODE (_q41))
				  {
				  case CALL_EXPR:
				    switch (get_call_combined_fn (_q41))
				      {
				      case CFN_BUILT_IN_POPCOUNT:
				        if (call_expr_nargs (_q41) == 1)
    {
					    tree _q70 = CALL_EXPR_ARG (_q41, 0);
					    switch (TREE_CODE (_q70))
					      {
					      case BIT_AND_EXPR:
					        {
						  tree _q80 = TREE_OPERAND (_q70, 0);
						  tree _q81 = TREE_OPERAND (_q70, 1);
						  if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
						    {
						      if ((_q81 == _q20 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q20, 0) && types_match (_q81, _q20)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q41, _q70, _q20, _p0 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNT);
							    if (res) return res;
							  }
						        }
						    }
						  if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
						    {
						      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q41, _q70, _q20, _p0 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNT);
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
						  if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
						    {
						      if ((_q81 == _q20 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q20, 0) && types_match (_q81, _q20)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q41, _q70, _q20, _p0 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNT);
							    if (res) return res;
							  }
						        }
						    }
						  if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
						    {
						      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q41, _q70, _q20, _p0 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNT);
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
		    break;
		  }
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
			    tree res = generic_simplify_32 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_LOG2);
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
			    tree res = generic_simplify_32 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_LOGF);
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
			    tree res = generic_simplify_32 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_LOGL);
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
	case CFN_BUILT_IN_POPCOUNTLL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_POPCOUNTLL:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_33 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNTLL);
			    if (res) return res;
			  }
		        }
		      break;
		    default:;
		    }
		  break;
	        default:;
	        }
	      switch (TREE_CODE (_q20))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_p1))
		          {
			  case CFN_BUILT_IN_POPCOUNTLL:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q60 = CALL_EXPR_ARG (_p1, 0);
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
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1, _q60 };
						tree res = generic_simplify_34 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNTLL);
						if (res) return res;
					      }
					    }
				        }
				      if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
					{
					  if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1, _q60 };
						tree res = generic_simplify_34 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNTLL);
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
		    break;
		  }
		case BIT_IOR_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_p1))
		          {
			  case CFN_BUILT_IN_POPCOUNTLL:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q60 = CALL_EXPR_ARG (_p1, 0);
				switch (TREE_CODE (_q60))
				  {
				  case BIT_AND_EXPR:
				    {
				      tree _q70 = TREE_OPERAND (_q60, 0);
				      tree _q71 = TREE_OPERAND (_q60, 1);
				      if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
					{
					  if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q60, _q30, _q31, _p0, _q20 };
						tree res = generic_simplify_34 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNTLL);
						if (res) return res;
					      }
					    }
				        }
				      if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
					{
					  if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q60, _q31, _q30, _p0, _q20 };
						tree res = generic_simplify_34 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNTLL);
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
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_p1))
	        {
		case MINUS_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_p1, 0);
		    tree _q41 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q40))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_q40))
		          {
			  case CFN_BUILT_IN_POPCOUNTLL:
			    if (call_expr_nargs (_q40) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_q40, 0);
				switch (TREE_CODE (_q41))
				  {
				  case CALL_EXPR:
				    switch (get_call_combined_fn (_q41))
				      {
				      case CFN_BUILT_IN_POPCOUNTLL:
				        if (call_expr_nargs (_q41) == 1)
    {
					    tree _q70 = CALL_EXPR_ARG (_q41, 0);
					    switch (TREE_CODE (_q70))
					      {
					      case BIT_AND_EXPR:
					        {
						  tree _q80 = TREE_OPERAND (_q70, 0);
						  tree _q81 = TREE_OPERAND (_q70, 1);
						  if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
						    {
						      if ((_q81 == _q20 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q20, 0) && types_match (_q81, _q20)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q41, _q70, _q20, _p0 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTLL);
							    if (res) return res;
							  }
						        }
						    }
						  if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
						    {
						      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q41, _q70, _q20, _p0 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTLL);
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
						  if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
						    {
						      if ((_q81 == _q20 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q20, 0) && types_match (_q81, _q20)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q41, _q70, _q20, _p0 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTLL);
							    if (res) return res;
							  }
						        }
						    }
						  if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
						    {
						      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q41, _q70, _q20, _p0 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTLL);
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
		    break;
		  }
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
			    tree res = generic_simplify_32 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_LOG10F);
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
			    tree res = generic_simplify_32 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_LOG10L);
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
	case CFN_POPCOUNT:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_POPCOUNT:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_33 (loc, type, _p0, _p1, captures, CFN_POPCOUNT);
			    if (res) return res;
			  }
		        }
		      break;
		    default:;
		    }
		  break;
	        default:;
	        }
	      switch (TREE_CODE (_q20))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_p1))
		          {
			  case CFN_POPCOUNT:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q60 = CALL_EXPR_ARG (_p1, 0);
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
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1, _q60 };
						tree res = generic_simplify_34 (loc, type, _p0, _p1, captures, CFN_POPCOUNT);
						if (res) return res;
					      }
					    }
				        }
				      if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
					{
					  if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1, _q60 };
						tree res = generic_simplify_34 (loc, type, _p0, _p1, captures, CFN_POPCOUNT);
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
		    break;
		  }
		case BIT_IOR_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_p1))
		          {
			  case CFN_POPCOUNT:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q60 = CALL_EXPR_ARG (_p1, 0);
				switch (TREE_CODE (_q60))
				  {
				  case BIT_AND_EXPR:
				    {
				      tree _q70 = TREE_OPERAND (_q60, 0);
				      tree _q71 = TREE_OPERAND (_q60, 1);
				      if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
					{
					  if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q60, _q30, _q31, _p0, _q20 };
						tree res = generic_simplify_34 (loc, type, _p0, _p1, captures, CFN_POPCOUNT);
						if (res) return res;
					      }
					    }
				        }
				      if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
					{
					  if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q60, _q31, _q30, _p0, _q20 };
						tree res = generic_simplify_34 (loc, type, _p0, _p1, captures, CFN_POPCOUNT);
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
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_p1))
	        {
		case MINUS_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_p1, 0);
		    tree _q41 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q40))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_q40))
		          {
			  case CFN_POPCOUNT:
			    if (call_expr_nargs (_q40) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_q40, 0);
				switch (TREE_CODE (_q41))
				  {
				  case CALL_EXPR:
				    switch (get_call_combined_fn (_q41))
				      {
				      case CFN_POPCOUNT:
				        if (call_expr_nargs (_q41) == 1)
    {
					    tree _q70 = CALL_EXPR_ARG (_q41, 0);
					    switch (TREE_CODE (_q70))
					      {
					      case BIT_AND_EXPR:
					        {
						  tree _q80 = TREE_OPERAND (_q70, 0);
						  tree _q81 = TREE_OPERAND (_q70, 1);
						  if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
						    {
						      if ((_q81 == _q20 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q20, 0) && types_match (_q81, _q20)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q41, _q70, _q20, _p0 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_POPCOUNT);
							    if (res) return res;
							  }
						        }
						    }
						  if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
						    {
						      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q41, _q70, _q20, _p0 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_POPCOUNT);
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
						  if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
						    {
						      if ((_q81 == _q20 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q20, 0) && types_match (_q81, _q20)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q41, _q70, _q20, _p0 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_POPCOUNT);
							    if (res) return res;
							  }
						        }
						    }
						  if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
						    {
						      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q41, _q70, _q20, _p0 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_POPCOUNT);
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
		    break;
		  }
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
			    tree res = generic_simplify_32 (loc, type, _p0, _p1, captures, CFN_LOG);
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
	case CFN_BUILT_IN_POPCOUNTIMAX:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_POPCOUNTIMAX:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_33 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNTIMAX);
			    if (res) return res;
			  }
		        }
		      break;
		    default:;
		    }
		  break;
	        default:;
	        }
	      switch (TREE_CODE (_q20))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_p1))
		          {
			  case CFN_BUILT_IN_POPCOUNTIMAX:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q60 = CALL_EXPR_ARG (_p1, 0);
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
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1, _q60 };
						tree res = generic_simplify_34 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNTIMAX);
						if (res) return res;
					      }
					    }
				        }
				      if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
					{
					  if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1, _q60 };
						tree res = generic_simplify_34 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNTIMAX);
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
		    break;
		  }
		case BIT_IOR_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_p1))
		          {
			  case CFN_BUILT_IN_POPCOUNTIMAX:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q60 = CALL_EXPR_ARG (_p1, 0);
				switch (TREE_CODE (_q60))
				  {
				  case BIT_AND_EXPR:
				    {
				      tree _q70 = TREE_OPERAND (_q60, 0);
				      tree _q71 = TREE_OPERAND (_q60, 1);
				      if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
					{
					  if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q60, _q30, _q31, _p0, _q20 };
						tree res = generic_simplify_34 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNTIMAX);
						if (res) return res;
					      }
					    }
				        }
				      if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
					{
					  if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q60, _q31, _q30, _p0, _q20 };
						tree res = generic_simplify_34 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNTIMAX);
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
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_p1))
	        {
		case MINUS_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_p1, 0);
		    tree _q41 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q40))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_q40))
		          {
			  case CFN_BUILT_IN_POPCOUNTIMAX:
			    if (call_expr_nargs (_q40) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_q40, 0);
				switch (TREE_CODE (_q41))
				  {
				  case CALL_EXPR:
				    switch (get_call_combined_fn (_q41))
				      {
				      case CFN_BUILT_IN_POPCOUNTIMAX:
				        if (call_expr_nargs (_q41) == 1)
    {
					    tree _q70 = CALL_EXPR_ARG (_q41, 0);
					    switch (TREE_CODE (_q70))
					      {
					      case BIT_AND_EXPR:
					        {
						  tree _q80 = TREE_OPERAND (_q70, 0);
						  tree _q81 = TREE_OPERAND (_q70, 1);
						  if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
						    {
						      if ((_q81 == _q20 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q20, 0) && types_match (_q81, _q20)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q41, _q70, _q20, _p0 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTIMAX);
							    if (res) return res;
							  }
						        }
						    }
						  if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
						    {
						      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q41, _q70, _q20, _p0 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTIMAX);
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
						  if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
						    {
						      if ((_q81 == _q20 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q20, 0) && types_match (_q81, _q20)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q41, _q70, _q20, _p0 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTIMAX);
							    if (res) return res;
							  }
						        }
						    }
						  if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
						    {
						      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q41, _q70, _q20, _p0 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTIMAX);
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
		    break;
		  }
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
			    tree res = generic_simplify_32 (loc, type, _p0, _p1, captures, CFN_LOG2);
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
	case CFN_BUILT_IN_POPCOUNTL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_POPCOUNTL:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  {
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_33 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNTL);
			    if (res) return res;
			  }
		        }
		      break;
		    default:;
		    }
		  break;
	        default:;
	        }
	      switch (TREE_CODE (_q20))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_p1))
		          {
			  case CFN_BUILT_IN_POPCOUNTL:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q60 = CALL_EXPR_ARG (_p1, 0);
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
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1, _q60 };
						tree res = generic_simplify_34 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNTL);
						if (res) return res;
					      }
					    }
				        }
				      if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
					{
					  if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1, _q60 };
						tree res = generic_simplify_34 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNTL);
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
		    break;
		  }
		case BIT_IOR_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_p1))
		          {
			  case CFN_BUILT_IN_POPCOUNTL:
			    if (call_expr_nargs (_p1) == 1)
    {
				tree _q60 = CALL_EXPR_ARG (_p1, 0);
				switch (TREE_CODE (_q60))
				  {
				  case BIT_AND_EXPR:
				    {
				      tree _q70 = TREE_OPERAND (_q60, 0);
				      tree _q71 = TREE_OPERAND (_q60, 1);
				      if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
					{
					  if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q60, _q30, _q31, _p0, _q20 };
						tree res = generic_simplify_34 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNTL);
						if (res) return res;
					      }
					    }
				        }
				      if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
					{
					  if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q60, _q31, _q30, _p0, _q20 };
						tree res = generic_simplify_34 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POPCOUNTL);
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
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_p1))
	        {
		case MINUS_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_p1, 0);
		    tree _q41 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q40))
		      {
		      case CALL_EXPR:
		        switch (get_call_combined_fn (_q40))
		          {
			  case CFN_BUILT_IN_POPCOUNTL:
			    if (call_expr_nargs (_q40) == 1)
    {
				tree _q50 = CALL_EXPR_ARG (_q40, 0);
				switch (TREE_CODE (_q41))
				  {
				  case CALL_EXPR:
				    switch (get_call_combined_fn (_q41))
				      {
				      case CFN_BUILT_IN_POPCOUNTL:
				        if (call_expr_nargs (_q41) == 1)
    {
					    tree _q70 = CALL_EXPR_ARG (_q41, 0);
					    switch (TREE_CODE (_q70))
					      {
					      case BIT_AND_EXPR:
					        {
						  tree _q80 = TREE_OPERAND (_q70, 0);
						  tree _q81 = TREE_OPERAND (_q70, 1);
						  if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
						    {
						      if ((_q81 == _q20 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q20, 0) && types_match (_q81, _q20)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q41, _q70, _q20, _p0 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTL);
							    if (res) return res;
							  }
						        }
						    }
						  if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
						    {
						      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q41, _q70, _q20, _p0 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_POPCOUNTL);
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
						  if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
						    {
						      if ((_q81 == _q20 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q20, 0) && types_match (_q81, _q20)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q41, _q70, _q20, _p0 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTL);
							    if (res) return res;
							  }
						        }
						    }
						  if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
						    {
						      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
							{
							  {
							    tree captures[7] ATTRIBUTE_UNUSED = { _p1, _q40, _q50, _q41, _q70, _q20, _p0 };
							    tree res = generic_simplify_35 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR, CFN_BUILT_IN_POPCOUNTL);
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
		    break;
		  }
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
			    tree res = generic_simplify_32 (loc, type, _p0, _p1, captures, CFN_LOG10);
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
			    tree res = generic_simplify_32 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_LOG10);
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
			    tree res = generic_simplify_32 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_LOG2F);
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
			    tree res = generic_simplify_32 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_LOG2L);
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
    case VEC_PERM_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	tree _q32 = TREE_OPERAND (_p1, 2);
	switch (TREE_CODE (_q30))
	  {
	  case MULT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if (tree_vec_same_elem_p (_q41))
		{
		  if ((_q31 == _q30 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _q30, 0) && types_match (_q31, _q30)))
		    {
		      {
			tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q40, _q41, _q32, _p0 };
			tree res = generic_simplify_36 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	      if (tree_vec_same_elem_p (_q40))
		{
		  if ((_q31 == _q30 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _q30, 0) && types_match (_q31, _q30)))
		    {
		      {
			tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q41, _q40, _q32, _p0 };
			tree res = generic_simplify_36 (loc, type, _p0, _p1, captures);
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
  return NULL_TREE;
}

tree
generic_simplify_UNEQ_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
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
 && (TREE_CODE_CLASS (UNEQ_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1774;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, UNEQ_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1774;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, UNEQ_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1774;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1774:;
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
 && (TREE_CODE_CLASS (UNEQ_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1775;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1775;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, UNEQ_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1775;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, UNEQ_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1775;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1775:;
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
 && (TREE_CODE_CLASS (UNEQ_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1776;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1776;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, UNEQ_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1776;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, UNEQ_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1776;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1776:;
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
	tree res = generic_simplify_521 (loc, type, _p0, _p1, captures, UNEQ_EXPR);
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
		tree res = generic_simplify_244 (loc, type, _p0, _p1, captures, UNEQ_EXPR, EQ_EXPR);
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
		tree res = generic_simplify_247 (loc, type, _p0, _p1, captures, UNEQ_EXPR, UNEQ_EXPR);
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
	    tree res = generic_simplify_248 (loc, type, _p0, _p1, captures, UNEQ_EXPR, UNEQ_EXPR);
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
	  tree res = generic_simplify_519 (loc, type, _p0, _p1, captures, UNEQ_EXPR);
	  if (res) return res;
	}
        break;
      }
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify (location_t loc, enum tree_code code, const tree type ATTRIBUTE_UNUSED, tree _p0, tree _p1)
{
  switch (code)
    {
    case PLUS_EXPR:
      return generic_simplify_PLUS_EXPR (loc, code, type, _p0, _p1);
    case POINTER_PLUS_EXPR:
      return generic_simplify_POINTER_PLUS_EXPR (loc, code, type, _p0, _p1);
    case MINUS_EXPR:
      return generic_simplify_MINUS_EXPR (loc, code, type, _p0, _p1);
    case BIT_IOR_EXPR:
      return generic_simplify_BIT_IOR_EXPR (loc, code, type, _p0, _p1);
    case BIT_XOR_EXPR:
      return generic_simplify_BIT_XOR_EXPR (loc, code, type, _p0, _p1);
    case POINTER_DIFF_EXPR:
      return generic_simplify_POINTER_DIFF_EXPR (loc, code, type, _p0, _p1);
    case MULT_EXPR:
      return generic_simplify_MULT_EXPR (loc, code, type, _p0, _p1);
    case EQ_EXPR:
      return generic_simplify_EQ_EXPR (loc, code, type, _p0, _p1);
    case NE_EXPR:
      return generic_simplify_NE_EXPR (loc, code, type, _p0, _p1);
    case TRUNC_DIV_EXPR:
      return generic_simplify_TRUNC_DIV_EXPR (loc, code, type, _p0, _p1);
    case CEIL_DIV_EXPR:
      return generic_simplify_CEIL_DIV_EXPR (loc, code, type, _p0, _p1);
    case FLOOR_DIV_EXPR:
      return generic_simplify_FLOOR_DIV_EXPR (loc, code, type, _p0, _p1);
    case ROUND_DIV_EXPR:
      return generic_simplify_ROUND_DIV_EXPR (loc, code, type, _p0, _p1);
    case EXACT_DIV_EXPR:
      return generic_simplify_EXACT_DIV_EXPR (loc, code, type, _p0, _p1);
    case FLOOR_MOD_EXPR:
      return generic_simplify_FLOOR_MOD_EXPR (loc, code, type, _p0, _p1);
    case RDIV_EXPR:
      return generic_simplify_RDIV_EXPR (loc, code, type, _p0, _p1);
    case LT_EXPR:
      return generic_simplify_LT_EXPR (loc, code, type, _p0, _p1);
    case LE_EXPR:
      return generic_simplify_LE_EXPR (loc, code, type, _p0, _p1);
    case GT_EXPR:
      return generic_simplify_GT_EXPR (loc, code, type, _p0, _p1);
    case GE_EXPR:
      return generic_simplify_GE_EXPR (loc, code, type, _p0, _p1);
    case CEIL_MOD_EXPR:
      return generic_simplify_CEIL_MOD_EXPR (loc, code, type, _p0, _p1);
    case ROUND_MOD_EXPR:
      return generic_simplify_ROUND_MOD_EXPR (loc, code, type, _p0, _p1);
    case TRUNC_MOD_EXPR:
      return generic_simplify_TRUNC_MOD_EXPR (loc, code, type, _p0, _p1);
    case LSHIFT_EXPR:
      return generic_simplify_LSHIFT_EXPR (loc, code, type, _p0, _p1);
    case RSHIFT_EXPR:
      return generic_simplify_RSHIFT_EXPR (loc, code, type, _p0, _p1);
    case BIT_AND_EXPR:
      return generic_simplify_BIT_AND_EXPR (loc, code, type, _p0, _p1);
    case MIN_EXPR:
      return generic_simplify_MIN_EXPR (loc, code, type, _p0, _p1);
    case MAX_EXPR:
      return generic_simplify_MAX_EXPR (loc, code, type, _p0, _p1);
    case LROTATE_EXPR:
      return generic_simplify_LROTATE_EXPR (loc, code, type, _p0, _p1);
    case RROTATE_EXPR:
      return generic_simplify_RROTATE_EXPR (loc, code, type, _p0, _p1);
    case COMPLEX_EXPR:
      return generic_simplify_COMPLEX_EXPR (loc, code, type, _p0, _p1);
    case UNORDERED_EXPR:
      return generic_simplify_UNORDERED_EXPR (loc, code, type, _p0, _p1);
    case ORDERED_EXPR:
      return generic_simplify_ORDERED_EXPR (loc, code, type, _p0, _p1);
    case UNLT_EXPR:
      return generic_simplify_UNLT_EXPR (loc, code, type, _p0, _p1);
    case UNLE_EXPR:
      return generic_simplify_UNLE_EXPR (loc, code, type, _p0, _p1);
    case UNGT_EXPR:
      return generic_simplify_UNGT_EXPR (loc, code, type, _p0, _p1);
    case UNGE_EXPR:
      return generic_simplify_UNGE_EXPR (loc, code, type, _p0, _p1);
    case UNEQ_EXPR:
      return generic_simplify_UNEQ_EXPR (loc, code, type, _p0, _p1);
    case LTGT_EXPR:
      return generic_simplify_LTGT_EXPR (loc, code, type, _p0, _p1);
    case TRUTH_ORIF_EXPR:
      return generic_simplify_TRUTH_ORIF_EXPR (loc, code, type, _p0, _p1);
    case TRUTH_OR_EXPR:
      return generic_simplify_TRUTH_OR_EXPR (loc, code, type, _p0, _p1);
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_VEC_COND_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1, tree _p2)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case VECTOR_CST:
      {
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _p2 };
	  if (integer_all_onesp (captures[0])
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1883;
	      {
		tree _r;
		_r = captures[1];
		if (TREE_SIDE_EFFECTS (captures[0]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
		if (TREE_SIDE_EFFECTS (captures[2]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1044, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1883:;
	    }
	  else
	    {
	      if (integer_zerop (captures[0])
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1884;
		  {
		    tree _r;
		    _r = captures[2];
		    if (TREE_SIDE_EFFECTS (captures[0]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
		    if (TREE_SIDE_EFFECTS (captures[1]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1045, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1884:;
		}
	    }
	}
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case VECTOR_CST:
      {
	switch (TREE_CODE (_p2))
	  {
	  case VECTOR_CST:
	    {
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _p2 };
		if (VECTOR_BOOLEAN_TYPE_P (type)
 && types_match (type, TREE_TYPE (captures[0]))
)
		  {
		    if (integer_zerop (captures[1]) && integer_all_onesp (captures[2])
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1885;
			{
			  tree res_op0;
			  res_op0 = captures[0];
			  tree _r;
			  _r = fold_build1_loc (loc, BIT_NOT_EXPR, type, res_op0);
			  if (TREE_SIDE_EFFECTS (captures[1]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			  if (TREE_SIDE_EFFECTS (captures[2]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1046, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1885:;
		      }
		    else
		      {
			if (integer_all_onesp (captures[1]) && integer_zerop (captures[2])
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1886;
			    {
			      tree _r;
			      _r = captures[0];
			      if (TREE_SIDE_EFFECTS (captures[1]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			      if (TREE_SIDE_EFFECTS (captures[2]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1047, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1886:;
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
    case NE_EXPR:
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
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      if (integer_zerop (_p2))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    tree res = generic_simplify_504 (loc, type, _p0, _p1, _p2, captures);
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
			    tree res = generic_simplify_504 (loc, type, _p0, _p1, _p2, captures);
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
			    tree res = generic_simplify_505 (loc, type, _p0, _p1, _p2, captures);
			    if (res) return res;
			  }
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_505 (loc, type, _p0, _p1, _p2, captures);
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
			    tree res = generic_simplify_505 (loc, type, _p0, _p1, _p2, captures);
			    if (res) return res;
			  }
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_505 (loc, type, _p0, _p1, _p2, captures);
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
			    tree res = generic_simplify_506 (loc, type, _p0, _p1, _p2, captures, BIT_AND_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_p2 == _q21 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q21, 0) && types_match (_p2, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_506 (loc, type, _p0, _p1, _p2, captures, BIT_AND_EXPR);
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
			    tree res = generic_simplify_506 (loc, type, _p0, _p1, _p2, captures, BIT_AND_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_p2 == _q21 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q21, 0) && types_match (_p2, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_506 (loc, type, _p0, _p1, _p2, captures, BIT_AND_EXPR);
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
			    tree res = generic_simplify_506 (loc, type, _p0, _p1, _p2, captures, BIT_IOR_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_p2 == _q21 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q21, 0) && types_match (_p2, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_506 (loc, type, _p0, _p1, _p2, captures, BIT_IOR_EXPR);
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
			    tree res = generic_simplify_506 (loc, type, _p0, _p1, _p2, captures, BIT_IOR_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_p2 == _q21 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q21, 0) && types_match (_p2, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_506 (loc, type, _p0, _p1, _p2, captures, BIT_IOR_EXPR);
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
			    tree res = generic_simplify_506 (loc, type, _p0, _p1, _p2, captures, MIN_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_p2 == _q21 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q21, 0) && types_match (_p2, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_506 (loc, type, _p0, _p1, _p2, captures, MIN_EXPR);
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
			    tree res = generic_simplify_506 (loc, type, _p0, _p1, _p2, captures, MIN_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_p2 == _q21 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q21, 0) && types_match (_p2, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_506 (loc, type, _p0, _p1, _p2, captures, MIN_EXPR);
			    if (res) return res;
			  }
		        }
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
			    tree res = generic_simplify_506 (loc, type, _p0, _p1, _p2, captures, MAX_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_p2 == _q21 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q21, 0) && types_match (_p2, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_506 (loc, type, _p0, _p1, _p2, captures, MAX_EXPR);
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
			    tree res = generic_simplify_506 (loc, type, _p0, _p1, _p2, captures, MAX_EXPR);
			    if (res) return res;
			  }
		        }
		      if ((_p2 == _q21 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q21, 0) && types_match (_p2, _q21)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_506 (loc, type, _p0, _p1, _p2, captures, MAX_EXPR);
			    if (res) return res;
			  }
		        }
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
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1887;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1887;
					  {
					    tree _r;
					    _r = captures[2];
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1010, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1887:;
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
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1888;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1888;
					  {
					    tree _r;
					    _r = captures[2];
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1010, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1888:;
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
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1889;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1889;
					  {
					    tree _r;
					    _r = captures[2];
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1010, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1889:;
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
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1890;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1890;
					  {
					    tree _r;
					    _r = captures[2];
					    if (TREE_SIDE_EFFECTS (captures[3]))
					      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1011, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1890:;
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
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1891;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1891;
					  {
					    tree _r;
					    _r = captures[2];
					    if (TREE_SIDE_EFFECTS (captures[3]))
					      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1011, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1891:;
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
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1892;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1892;
					  {
					    tree _r;
					    _r = captures[2];
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1010, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1892:;
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
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1893;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1893;
					  {
					    tree _r;
					    _r = captures[2];
					    if (TREE_SIDE_EFFECTS (captures[3]))
					      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1011, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1893:;
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
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1894;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1894;
					  {
					    tree _r;
					    _r = captures[2];
					    if (TREE_SIDE_EFFECTS (captures[3]))
					      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1011, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1894:;
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
    case VEC_COND_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	tree _q32 = TREE_OPERAND (_p1, 2);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _p2 };
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1895;
	      {
		tree res_op0;
		res_op0 = captures[0];
		tree res_op1;
		res_op1 = captures[1];
		tree res_op2;
		res_op2 = captures[3];
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (TREE_SIDE_EFFECTS (captures[2]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1019, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1895:;
	    }
	  }
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p2))
    {
    case VEC_COND_EXPR:
      {
	tree _q40 = TREE_OPERAND (_p2, 0);
	tree _q41 = TREE_OPERAND (_p2, 1);
	tree _q42 = TREE_OPERAND (_p2, 2);
	if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
	  {
	    {
	      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _q41, _q42 };
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1896;
	      {
		tree res_op0;
		res_op0 = captures[0];
		tree res_op1;
		res_op1 = captures[1];
		tree res_op2;
		res_op2 = captures[3];
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (TREE_SIDE_EFFECTS (captures[2]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1020, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1896:;
	    }
	  }
	{
	  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42 };
	  if (inverse_conditions_p (captures[0], captures[2])
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1897;
	      {
		tree res_op0;
		res_op0 = captures[0];
		tree res_op1;
		res_op1 = captures[1];
		tree res_op2;
		res_op2 = captures[3];
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (TREE_SIDE_EFFECTS (captures[2]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		if (TREE_SIDE_EFFECTS (captures[4]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[4]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1021, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1897:;
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
	  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q30, _q31, _q32, _p2 };
	  if (inverse_conditions_p (captures[0], captures[1])
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1898;
	      {
		tree res_op0;
		res_op0 = captures[0];
		tree res_op1;
		res_op1 = captures[3];
		tree res_op2;
		res_op2 = captures[4];
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (TREE_SIDE_EFFECTS (captures[2]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1022, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1898:;
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
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1899;
	{
	  tree _r;
	  _r = captures[1];
	  if (TREE_SIDE_EFFECTS (captures[0]))
	    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1023, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1899:;
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
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1900;
			    if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1900;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1900;
			    {
			      tree _r;
			      _r = captures[2];
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1024, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1900:;
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
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1901;
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1901;
			    if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1901;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1901;
			    {
			      tree _r;
			      _r = captures[1];
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1025, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1901:;
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
					  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1902;
					  if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1902;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1902;
					  {
					    tree _r;
					    _r = captures[3];
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1026, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1902:;
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
				      tree res = generic_simplify_507 (loc, type, _p0, _p1, _p2, captures, EQ_EXPR);
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
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1903;
			    if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1903;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1903;
			    {
			      tree _r;
			      _r = captures[2];
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1024, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1903:;
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
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1904;
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1904;
			    if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1904;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1904;
			    {
			      tree _r;
			      _r = captures[1];
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1025, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1904:;
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
					  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1905;
					  if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1905;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1905;
					  {
					    tree _r;
					    _r = captures[3];
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1026, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1905:;
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
				      tree res = generic_simplify_507 (loc, type, _p0, _p1, _p2, captures, UNEQ_EXPR);
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
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1906;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1906;
			    {
			      tree _r;
			      _r = captures[1];
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1027, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1906:;
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
		    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1907;
		    if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1907;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1907;
		    {
		      tree _r;
		      _r = captures[1];
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1028, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1907:;
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
					  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1908;
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1908;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1908;
					  {
					    tree _r;
					    _r = captures[0];
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1029, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1908:;
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
			    tree res = generic_simplify_508 (loc, type, _p0, _p1, _p2, captures, NE_EXPR);
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
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1909;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1909;
			    {
			      tree _r;
			      _r = captures[1];
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1027, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1909:;
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
		    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1910;
		    if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1910;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1910;
		    {
		      tree _r;
		      _r = captures[1];
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1028, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1910:;
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
					  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1911;
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1911;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1911;
					  {
					    tree _r;
					    _r = captures[0];
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1029, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1911:;
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
			    tree res = generic_simplify_508 (loc, type, _p0, _p1, _p2, captures, LTGT_EXPR);
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
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1912;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1912;
					      {
						tree res_op0;
						res_op0 = captures[0];
						tree _r;
						_r = fold_build1_loc (loc, ABSU_EXPR, type, res_op0);
						if (TREE_SIDE_EFFECTS (captures[2]))
						  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1032, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail1912:;
					    }
					  else
					    {
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1913;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1913;
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
next_after_fail1913:;
					    }
				      }
				    }
				  else
				    {
				      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1914;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1914;
				      {
					tree res_op0;
					res_op0 = captures[2];
					tree _r;
					_r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1034, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail1914:;
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
					if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1915;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1915;
					{
					  tree res_op0;
					  res_op0 = captures[0];
					  tree _r;
					  _r = fold_build1_loc (loc, ABSU_EXPR, type, res_op0);
					  if (TREE_SIDE_EFFECTS (captures[2]))
					    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1032, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1915:;
				      }
				    else
				      {
					if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1916;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1916;
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
next_after_fail1916:;
				      }
				}
			      }
			    else
			      {
				if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1917;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1917;
				{
				  tree res_op0;
				  res_op0 = captures[2];
				  tree _r;
				  _r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1034, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1917:;
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
					  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1918;
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1918;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1918;
					  {
					    tree res_op0;
					    res_op0 = captures[0];
					    tree _r;
					    _r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1035, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1918:;
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
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1919;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1919;
					      {
						tree res_op0;
						res_op0 = captures[0];
						tree _r;
						_r = fold_build1_loc (loc, ABSU_EXPR, type, res_op0);
						if (TREE_SIDE_EFFECTS (captures[2]))
						  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1032, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail1919:;
					    }
					  else
					    {
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1920;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1920;
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
next_after_fail1920:;
					    }
				      }
				    }
				  else
				    {
				      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1921;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1921;
				      {
					tree res_op0;
					res_op0 = captures[2];
					tree _r;
					_r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1034, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail1921:;
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
					if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1922;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1922;
					{
					  tree res_op0;
					  res_op0 = captures[0];
					  tree _r;
					  _r = fold_build1_loc (loc, ABSU_EXPR, type, res_op0);
					  if (TREE_SIDE_EFFECTS (captures[2]))
					    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1032, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1922:;
				      }
				    else
				      {
					if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1923;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1923;
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
next_after_fail1923:;
				      }
				}
			      }
			    else
			      {
				if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1924;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1924;
				{
				  tree res_op0;
				  res_op0 = captures[2];
				  tree _r;
				  _r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1034, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1924:;
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
					  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1925;
					  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1925;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1925;
					  {
					    tree res_op0;
					    res_op0 = captures[0];
					    tree _r;
					    _r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1035, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1925:;
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
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1926;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1926;
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
						if (TREE_SIDE_EFFECTS (captures[2]))
						  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1036, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail1926:;
					    }
					  else
					    {
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1927;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1927;
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
next_after_fail1927:;
					    }
				      }
				    }
				  else
				    {
				      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1928;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1928;
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
next_after_fail1928:;
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
					if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1929;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1929;
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
					  if (TREE_SIDE_EFFECTS (captures[2]))
					    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1036, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1929:;
				      }
				    else
				      {
					if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1930;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1930;
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
next_after_fail1930:;
				      }
				}
			      }
			    else
			      {
				if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1931;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1931;
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
next_after_fail1931:;
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
						  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1932;
						  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1932;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1932;
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
next_after_fail1932:;
					      }
					    }
					  else
					    {
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1933;
					      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1933;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1933;
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
next_after_fail1933:;
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
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1934;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1934;
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
						if (TREE_SIDE_EFFECTS (captures[2]))
						  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
						if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1036, __FILE__, __LINE__, true);
						return _r;
					      }
next_after_fail1934:;
					    }
					  else
					    {
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1935;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1935;
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
next_after_fail1935:;
					    }
				      }
				    }
				  else
				    {
				      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1936;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1936;
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
next_after_fail1936:;
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
					if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1937;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1937;
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
					  if (TREE_SIDE_EFFECTS (captures[2]))
					    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1036, __FILE__, __LINE__, true);
					  return _r;
					}
next_after_fail1937:;
				      }
				    else
				      {
					if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1938;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1938;
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
next_after_fail1938:;
				      }
				}
			      }
			    else
			      {
				if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1939;
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1939;
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
next_after_fail1939:;
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
						  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1940;
						  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1940;
						  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1940;
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
next_after_fail1940:;
					      }
					    }
					  else
					    {
					      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1941;
					      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1941;
					      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1941;
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
next_after_fail1941:;
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
{
  tree _p0_pops[1];
  if (tree_logical_inverted_value (_p0, _p0_pops))
    {
      tree _q20 = _p0_pops[0];
      if (tree_truth_valued_p (_q20))
	{
	  {
	    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _p2 };
	    if (VEC_COND_EXPR == VEC_COND_EXPR
 || TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
	      {
		if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1942;
		{
		  tree res_op0;
		  res_op0 = captures[0];
		  tree res_op1;
		  res_op1 = captures[2];
		  tree res_op2;
		  res_op2 = captures[1];
		  tree _r;
		  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1043, __FILE__, __LINE__, true);
		  return _r;
		}
next_after_fail1942:;
	      }
	  }
        }
    }
}
  switch (TREE_CODE (_p1))
    {
    case VIEW_CONVERT_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	switch (TREE_CODE (_q30))
	  {
	  case CALL_EXPR:
	    switch (get_call_combined_fn (_q30))
	      {
	      case CFN_COND_ADD:
	        if (call_expr_nargs (_q30) == 4)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _p2 };
			  tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_ADD);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_AND:
	        if (call_expr_nargs (_q30) == 4)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _p2 };
			  tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_AND);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_DIV:
	        if (call_expr_nargs (_q30) == 4)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _p2 };
			  tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_DIV);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_IOR:
	        if (call_expr_nargs (_q30) == 4)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _p2 };
			  tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_IOR);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_MAX:
	        if (call_expr_nargs (_q30) == 4)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _p2 };
			  tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_MAX);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_MIN:
	        if (call_expr_nargs (_q30) == 4)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _p2 };
			  tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_MIN);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_MOD:
	        if (call_expr_nargs (_q30) == 4)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _p2 };
			  tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_MOD);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_MUL:
	        if (call_expr_nargs (_q30) == 4)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _p2 };
			  tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_MUL);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_SHL:
	        if (call_expr_nargs (_q30) == 4)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _p2 };
			  tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_SHL);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_SHR:
	        if (call_expr_nargs (_q30) == 4)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _p2 };
			  tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_SHR);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_SUB:
	        if (call_expr_nargs (_q30) == 4)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _p2 };
			  tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_SUB);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_XOR:
	        if (call_expr_nargs (_q30) == 4)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _p2 };
			  tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_XOR);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_COPYSIGN:
	        if (call_expr_nargs (_q30) == 4)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _p2 };
			  tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_COPYSIGN);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_FMAX:
	        if (call_expr_nargs (_q30) == 4)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _p2 };
			  tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FMAX);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_FMIN:
	        if (call_expr_nargs (_q30) == 4)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _p2 };
			  tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FMIN);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_RDIV:
	        if (call_expr_nargs (_q30) == 4)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _p2 };
			  tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_RDIV);
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
    case CALL_EXPR:
      switch (get_call_combined_fn (_p1))
        {
	case CFN_COND_ADD:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _p2 };
		    tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_ADD);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_AND:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _p2 };
		    tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_AND);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_DIV:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _p2 };
		    tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_DIV);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_IOR:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _p2 };
		    tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_IOR);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_MAX:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _p2 };
		    tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_MAX);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_MIN:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _p2 };
		    tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_MIN);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_MOD:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _p2 };
		    tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_MOD);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_MUL:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _p2 };
		    tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_MUL);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_SHL:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _p2 };
		    tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_SHL);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_SHR:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _p2 };
		    tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_SHR);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_SUB:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _p2 };
		    tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_SUB);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_XOR:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _p2 };
		    tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_XOR);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_COPYSIGN:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _p2 };
		    tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_COPYSIGN);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_FMAX:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _p2 };
		    tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FMAX);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_FMIN:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _p2 };
		    tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FMIN);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_RDIV:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _p2 };
		    tree res = generic_simplify_509 (loc, type, _p0, _p1, _p2, captures, CFN_COND_RDIV);
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
  switch (TREE_CODE (_p2))
    {
    case VIEW_CONVERT_EXPR:
      {
	tree _q40 = TREE_OPERAND (_p2, 0);
	switch (TREE_CODE (_q40))
	  {
	  case CALL_EXPR:
	    switch (get_call_combined_fn (_q40))
	      {
	      case CFN_COND_ADD:
	        if (call_expr_nargs (_q40) == 4)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    {
		      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53 };
		      tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_ADD);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_AND:
	        if (call_expr_nargs (_q40) == 4)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    {
		      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53 };
		      tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_AND);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_DIV:
	        if (call_expr_nargs (_q40) == 4)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    {
		      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53 };
		      tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_DIV);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_IOR:
	        if (call_expr_nargs (_q40) == 4)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    {
		      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53 };
		      tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_IOR);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_MAX:
	        if (call_expr_nargs (_q40) == 4)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    {
		      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53 };
		      tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_MAX);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_MIN:
	        if (call_expr_nargs (_q40) == 4)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    {
		      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53 };
		      tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_MIN);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_MOD:
	        if (call_expr_nargs (_q40) == 4)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    {
		      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53 };
		      tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_MOD);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_MUL:
	        if (call_expr_nargs (_q40) == 4)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    {
		      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53 };
		      tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_MUL);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_SHL:
	        if (call_expr_nargs (_q40) == 4)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    {
		      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53 };
		      tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_SHL);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_SHR:
	        if (call_expr_nargs (_q40) == 4)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    {
		      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53 };
		      tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_SHR);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_SUB:
	        if (call_expr_nargs (_q40) == 4)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    {
		      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53 };
		      tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_SUB);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_XOR:
	        if (call_expr_nargs (_q40) == 4)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    {
		      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53 };
		      tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_XOR);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_COPYSIGN:
	        if (call_expr_nargs (_q40) == 4)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    {
		      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53 };
		      tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_COPYSIGN);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_FMAX:
	        if (call_expr_nargs (_q40) == 4)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    {
		      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53 };
		      tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FMAX);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_FMIN:
	        if (call_expr_nargs (_q40) == 4)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    {
		      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53 };
		      tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FMIN);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_RDIV:
	        if (call_expr_nargs (_q40) == 4)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    {
		      tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53 };
		      tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_RDIV);
		      if (res) return res;
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
      switch (get_call_combined_fn (_p2))
        {
	case CFN_COND_ADD:
	  if (call_expr_nargs (_p2) == 4)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      {
		tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43 };
		tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_ADD);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_AND:
	  if (call_expr_nargs (_p2) == 4)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      {
		tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43 };
		tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_AND);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_DIV:
	  if (call_expr_nargs (_p2) == 4)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      {
		tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43 };
		tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_DIV);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_IOR:
	  if (call_expr_nargs (_p2) == 4)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      {
		tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43 };
		tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_IOR);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_MAX:
	  if (call_expr_nargs (_p2) == 4)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      {
		tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43 };
		tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_MAX);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_MIN:
	  if (call_expr_nargs (_p2) == 4)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      {
		tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43 };
		tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_MIN);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_MOD:
	  if (call_expr_nargs (_p2) == 4)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      {
		tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43 };
		tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_MOD);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_MUL:
	  if (call_expr_nargs (_p2) == 4)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      {
		tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43 };
		tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_MUL);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_SHL:
	  if (call_expr_nargs (_p2) == 4)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      {
		tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43 };
		tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_SHL);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_SHR:
	  if (call_expr_nargs (_p2) == 4)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      {
		tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43 };
		tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_SHR);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_SUB:
	  if (call_expr_nargs (_p2) == 4)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      {
		tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43 };
		tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_SUB);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_XOR:
	  if (call_expr_nargs (_p2) == 4)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      {
		tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43 };
		tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_XOR);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_COPYSIGN:
	  if (call_expr_nargs (_p2) == 4)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      {
		tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43 };
		tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_COPYSIGN);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_FMAX:
	  if (call_expr_nargs (_p2) == 4)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      {
		tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43 };
		tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FMAX);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_FMIN:
	  if (call_expr_nargs (_p2) == 4)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      {
		tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43 };
		tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FMIN);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_RDIV:
	  if (call_expr_nargs (_p2) == 4)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      {
		tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43 };
		tree res = generic_simplify_510 (loc, type, _p0, _p1, _p2, captures, CFN_COND_RDIV);
		if (res) return res;
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
    case VIEW_CONVERT_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	switch (TREE_CODE (_q30))
	  {
	  case CALL_EXPR:
	    switch (get_call_combined_fn (_q30))
	      {
	      case CFN_COND_FMA:
	        if (call_expr_nargs (_q30) == 5)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _p2 };
			  tree res = generic_simplify_511 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FMA);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_FMS:
	        if (call_expr_nargs (_q30) == 5)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _p2 };
			  tree res = generic_simplify_511 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FMS);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_FNMA:
	        if (call_expr_nargs (_q30) == 5)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _p2 };
			  tree res = generic_simplify_511 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FNMA);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_FNMS:
	        if (call_expr_nargs (_q30) == 5)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _p2 };
			  tree res = generic_simplify_511 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FNMS);
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
    case CALL_EXPR:
      switch (get_call_combined_fn (_p1))
        {
	case CFN_COND_FMA:
	  if (call_expr_nargs (_p1) == 5)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _p2 };
		    tree res = generic_simplify_511 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FMA);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_FMS:
	  if (call_expr_nargs (_p1) == 5)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _p2 };
		    tree res = generic_simplify_511 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FMS);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_FNMA:
	  if (call_expr_nargs (_p1) == 5)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _p2 };
		    tree res = generic_simplify_511 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FNMA);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_FNMS:
	  if (call_expr_nargs (_p1) == 5)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _p2 };
		    tree res = generic_simplify_511 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FNMS);
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
  switch (TREE_CODE (_p2))
    {
    case VIEW_CONVERT_EXPR:
      {
	tree _q40 = TREE_OPERAND (_p2, 0);
	switch (TREE_CODE (_q40))
	  {
	  case CALL_EXPR:
	    switch (get_call_combined_fn (_q40))
	      {
	      case CFN_COND_FMA:
	        if (call_expr_nargs (_q40) == 5)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    {
		      tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54 };
		      tree res = generic_simplify_512 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FMA);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_FMS:
	        if (call_expr_nargs (_q40) == 5)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    {
		      tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54 };
		      tree res = generic_simplify_512 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FMS);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_FNMA:
	        if (call_expr_nargs (_q40) == 5)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    {
		      tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54 };
		      tree res = generic_simplify_512 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FNMA);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_FNMS:
	        if (call_expr_nargs (_q40) == 5)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    {
		      tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54 };
		      tree res = generic_simplify_512 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FNMS);
		      if (res) return res;
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
      switch (get_call_combined_fn (_p2))
        {
	case CFN_COND_FMA:
	  if (call_expr_nargs (_p2) == 5)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      {
		tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44 };
		tree res = generic_simplify_512 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FMA);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_FMS:
	  if (call_expr_nargs (_p2) == 5)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      {
		tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44 };
		tree res = generic_simplify_512 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FMS);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_FNMA:
	  if (call_expr_nargs (_p2) == 5)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      {
		tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44 };
		tree res = generic_simplify_512 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FNMA);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_FNMS:
	  if (call_expr_nargs (_p2) == 5)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      {
		tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44 };
		tree res = generic_simplify_512 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FNMS);
		if (res) return res;
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
    case VIEW_CONVERT_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	switch (TREE_CODE (_q30))
	  {
	  case CALL_EXPR:
	    switch (get_call_combined_fn (_q30))
	      {
	      case CFN_COND_LEN_ADD:
	        if (call_expr_nargs (_q30) == 6)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    tree _q45 = CALL_EXPR_ARG (_q30, 5);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _q45, _p2 };
			  tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_ADD);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_LEN_AND:
	        if (call_expr_nargs (_q30) == 6)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    tree _q45 = CALL_EXPR_ARG (_q30, 5);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _q45, _p2 };
			  tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_AND);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_LEN_DIV:
	        if (call_expr_nargs (_q30) == 6)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    tree _q45 = CALL_EXPR_ARG (_q30, 5);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _q45, _p2 };
			  tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_DIV);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_LEN_IOR:
	        if (call_expr_nargs (_q30) == 6)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    tree _q45 = CALL_EXPR_ARG (_q30, 5);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _q45, _p2 };
			  tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_IOR);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_LEN_MAX:
	        if (call_expr_nargs (_q30) == 6)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    tree _q45 = CALL_EXPR_ARG (_q30, 5);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _q45, _p2 };
			  tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_MAX);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_LEN_MIN:
	        if (call_expr_nargs (_q30) == 6)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    tree _q45 = CALL_EXPR_ARG (_q30, 5);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _q45, _p2 };
			  tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_MIN);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_LEN_MOD:
	        if (call_expr_nargs (_q30) == 6)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    tree _q45 = CALL_EXPR_ARG (_q30, 5);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _q45, _p2 };
			  tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_MOD);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_LEN_MUL:
	        if (call_expr_nargs (_q30) == 6)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    tree _q45 = CALL_EXPR_ARG (_q30, 5);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _q45, _p2 };
			  tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_MUL);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_LEN_SHL:
	        if (call_expr_nargs (_q30) == 6)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    tree _q45 = CALL_EXPR_ARG (_q30, 5);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _q45, _p2 };
			  tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_SHL);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_LEN_SHR:
	        if (call_expr_nargs (_q30) == 6)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    tree _q45 = CALL_EXPR_ARG (_q30, 5);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _q45, _p2 };
			  tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_SHR);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_LEN_SUB:
	        if (call_expr_nargs (_q30) == 6)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    tree _q45 = CALL_EXPR_ARG (_q30, 5);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _q45, _p2 };
			  tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_SUB);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_LEN_XOR:
	        if (call_expr_nargs (_q30) == 6)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    tree _q45 = CALL_EXPR_ARG (_q30, 5);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _q45, _p2 };
			  tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_XOR);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_LEN_FMAX:
	        if (call_expr_nargs (_q30) == 6)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    tree _q45 = CALL_EXPR_ARG (_q30, 5);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _q45, _p2 };
			  tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FMAX);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_LEN_FMIN:
	        if (call_expr_nargs (_q30) == 6)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    tree _q45 = CALL_EXPR_ARG (_q30, 5);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _q45, _p2 };
			  tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FMIN);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_LEN_RDIV:
	        if (call_expr_nargs (_q30) == 6)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    tree _q45 = CALL_EXPR_ARG (_q30, 5);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _q45, _p2 };
			  tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_RDIV);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_LEN_COPYSIGN:
	        if (call_expr_nargs (_q30) == 6)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    tree _q45 = CALL_EXPR_ARG (_q30, 5);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _q45, _p2 };
			  tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_COPYSIGN);
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
    case CALL_EXPR:
      switch (get_call_combined_fn (_p1))
        {
	case CFN_COND_LEN_ADD:
	  if (call_expr_nargs (_p1) == 6)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      tree _q35 = CALL_EXPR_ARG (_p1, 5);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _q35, _p2 };
		    tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_ADD);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_LEN_AND:
	  if (call_expr_nargs (_p1) == 6)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      tree _q35 = CALL_EXPR_ARG (_p1, 5);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _q35, _p2 };
		    tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_AND);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_LEN_DIV:
	  if (call_expr_nargs (_p1) == 6)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      tree _q35 = CALL_EXPR_ARG (_p1, 5);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _q35, _p2 };
		    tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_DIV);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_LEN_IOR:
	  if (call_expr_nargs (_p1) == 6)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      tree _q35 = CALL_EXPR_ARG (_p1, 5);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _q35, _p2 };
		    tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_IOR);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_LEN_MAX:
	  if (call_expr_nargs (_p1) == 6)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      tree _q35 = CALL_EXPR_ARG (_p1, 5);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _q35, _p2 };
		    tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_MAX);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_LEN_MIN:
	  if (call_expr_nargs (_p1) == 6)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      tree _q35 = CALL_EXPR_ARG (_p1, 5);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _q35, _p2 };
		    tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_MIN);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_LEN_MOD:
	  if (call_expr_nargs (_p1) == 6)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      tree _q35 = CALL_EXPR_ARG (_p1, 5);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _q35, _p2 };
		    tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_MOD);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_LEN_MUL:
	  if (call_expr_nargs (_p1) == 6)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      tree _q35 = CALL_EXPR_ARG (_p1, 5);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _q35, _p2 };
		    tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_MUL);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_LEN_SHL:
	  if (call_expr_nargs (_p1) == 6)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      tree _q35 = CALL_EXPR_ARG (_p1, 5);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _q35, _p2 };
		    tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_SHL);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_LEN_SHR:
	  if (call_expr_nargs (_p1) == 6)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      tree _q35 = CALL_EXPR_ARG (_p1, 5);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _q35, _p2 };
		    tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_SHR);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_LEN_SUB:
	  if (call_expr_nargs (_p1) == 6)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      tree _q35 = CALL_EXPR_ARG (_p1, 5);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _q35, _p2 };
		    tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_SUB);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_LEN_XOR:
	  if (call_expr_nargs (_p1) == 6)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      tree _q35 = CALL_EXPR_ARG (_p1, 5);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _q35, _p2 };
		    tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_XOR);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_LEN_FMAX:
	  if (call_expr_nargs (_p1) == 6)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      tree _q35 = CALL_EXPR_ARG (_p1, 5);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _q35, _p2 };
		    tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FMAX);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_LEN_FMIN:
	  if (call_expr_nargs (_p1) == 6)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      tree _q35 = CALL_EXPR_ARG (_p1, 5);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _q35, _p2 };
		    tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FMIN);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_LEN_RDIV:
	  if (call_expr_nargs (_p1) == 6)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      tree _q35 = CALL_EXPR_ARG (_p1, 5);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _q35, _p2 };
		    tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_RDIV);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_LEN_COPYSIGN:
	  if (call_expr_nargs (_p1) == 6)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      tree _q35 = CALL_EXPR_ARG (_p1, 5);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _q35, _p2 };
		    tree res = generic_simplify_513 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_COPYSIGN);
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
  switch (TREE_CODE (_p2))
    {
    case VIEW_CONVERT_EXPR:
      {
	tree _q40 = TREE_OPERAND (_p2, 0);
	switch (TREE_CODE (_q40))
	  {
	  case CALL_EXPR:
	    switch (get_call_combined_fn (_q40))
	      {
	      case CFN_COND_LEN_ADD:
	        if (call_expr_nargs (_q40) == 6)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    tree _q55 = CALL_EXPR_ARG (_q40, 5);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54, _q55 };
		      tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_ADD);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_LEN_AND:
	        if (call_expr_nargs (_q40) == 6)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    tree _q55 = CALL_EXPR_ARG (_q40, 5);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54, _q55 };
		      tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_AND);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_LEN_DIV:
	        if (call_expr_nargs (_q40) == 6)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    tree _q55 = CALL_EXPR_ARG (_q40, 5);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54, _q55 };
		      tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_DIV);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_LEN_IOR:
	        if (call_expr_nargs (_q40) == 6)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    tree _q55 = CALL_EXPR_ARG (_q40, 5);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54, _q55 };
		      tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_IOR);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_LEN_MAX:
	        if (call_expr_nargs (_q40) == 6)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    tree _q55 = CALL_EXPR_ARG (_q40, 5);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54, _q55 };
		      tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_MAX);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_LEN_MIN:
	        if (call_expr_nargs (_q40) == 6)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    tree _q55 = CALL_EXPR_ARG (_q40, 5);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54, _q55 };
		      tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_MIN);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_LEN_MOD:
	        if (call_expr_nargs (_q40) == 6)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    tree _q55 = CALL_EXPR_ARG (_q40, 5);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54, _q55 };
		      tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_MOD);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_LEN_MUL:
	        if (call_expr_nargs (_q40) == 6)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    tree _q55 = CALL_EXPR_ARG (_q40, 5);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54, _q55 };
		      tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_MUL);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_LEN_SHL:
	        if (call_expr_nargs (_q40) == 6)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    tree _q55 = CALL_EXPR_ARG (_q40, 5);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54, _q55 };
		      tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_SHL);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_LEN_SHR:
	        if (call_expr_nargs (_q40) == 6)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    tree _q55 = CALL_EXPR_ARG (_q40, 5);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54, _q55 };
		      tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_SHR);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_LEN_SUB:
	        if (call_expr_nargs (_q40) == 6)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    tree _q55 = CALL_EXPR_ARG (_q40, 5);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54, _q55 };
		      tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_SUB);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_LEN_XOR:
	        if (call_expr_nargs (_q40) == 6)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    tree _q55 = CALL_EXPR_ARG (_q40, 5);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54, _q55 };
		      tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_XOR);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_LEN_FMAX:
	        if (call_expr_nargs (_q40) == 6)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    tree _q55 = CALL_EXPR_ARG (_q40, 5);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54, _q55 };
		      tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FMAX);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_LEN_FMIN:
	        if (call_expr_nargs (_q40) == 6)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    tree _q55 = CALL_EXPR_ARG (_q40, 5);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54, _q55 };
		      tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FMIN);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_LEN_RDIV:
	        if (call_expr_nargs (_q40) == 6)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    tree _q55 = CALL_EXPR_ARG (_q40, 5);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54, _q55 };
		      tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_RDIV);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_LEN_COPYSIGN:
	        if (call_expr_nargs (_q40) == 6)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    tree _q55 = CALL_EXPR_ARG (_q40, 5);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54, _q55 };
		      tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_COPYSIGN);
		      if (res) return res;
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
      switch (get_call_combined_fn (_p2))
        {
	case CFN_COND_LEN_ADD:
	  if (call_expr_nargs (_p2) == 6)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      tree _q45 = CALL_EXPR_ARG (_p2, 5);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44, _q45 };
		tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_ADD);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_LEN_AND:
	  if (call_expr_nargs (_p2) == 6)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      tree _q45 = CALL_EXPR_ARG (_p2, 5);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44, _q45 };
		tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_AND);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_LEN_DIV:
	  if (call_expr_nargs (_p2) == 6)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      tree _q45 = CALL_EXPR_ARG (_p2, 5);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44, _q45 };
		tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_DIV);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_LEN_IOR:
	  if (call_expr_nargs (_p2) == 6)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      tree _q45 = CALL_EXPR_ARG (_p2, 5);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44, _q45 };
		tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_IOR);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_LEN_MAX:
	  if (call_expr_nargs (_p2) == 6)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      tree _q45 = CALL_EXPR_ARG (_p2, 5);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44, _q45 };
		tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_MAX);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_LEN_MIN:
	  if (call_expr_nargs (_p2) == 6)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      tree _q45 = CALL_EXPR_ARG (_p2, 5);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44, _q45 };
		tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_MIN);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_LEN_MOD:
	  if (call_expr_nargs (_p2) == 6)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      tree _q45 = CALL_EXPR_ARG (_p2, 5);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44, _q45 };
		tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_MOD);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_LEN_MUL:
	  if (call_expr_nargs (_p2) == 6)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      tree _q45 = CALL_EXPR_ARG (_p2, 5);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44, _q45 };
		tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_MUL);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_LEN_SHL:
	  if (call_expr_nargs (_p2) == 6)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      tree _q45 = CALL_EXPR_ARG (_p2, 5);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44, _q45 };
		tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_SHL);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_LEN_SHR:
	  if (call_expr_nargs (_p2) == 6)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      tree _q45 = CALL_EXPR_ARG (_p2, 5);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44, _q45 };
		tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_SHR);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_LEN_SUB:
	  if (call_expr_nargs (_p2) == 6)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      tree _q45 = CALL_EXPR_ARG (_p2, 5);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44, _q45 };
		tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_SUB);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_LEN_XOR:
	  if (call_expr_nargs (_p2) == 6)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      tree _q45 = CALL_EXPR_ARG (_p2, 5);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44, _q45 };
		tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_XOR);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_LEN_FMAX:
	  if (call_expr_nargs (_p2) == 6)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      tree _q45 = CALL_EXPR_ARG (_p2, 5);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44, _q45 };
		tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FMAX);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_LEN_FMIN:
	  if (call_expr_nargs (_p2) == 6)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      tree _q45 = CALL_EXPR_ARG (_p2, 5);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44, _q45 };
		tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FMIN);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_LEN_RDIV:
	  if (call_expr_nargs (_p2) == 6)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      tree _q45 = CALL_EXPR_ARG (_p2, 5);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44, _q45 };
		tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_RDIV);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_LEN_COPYSIGN:
	  if (call_expr_nargs (_p2) == 6)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      tree _q45 = CALL_EXPR_ARG (_p2, 5);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44, _q45 };
		tree res = generic_simplify_514 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_COPYSIGN);
		if (res) return res;
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
    case VIEW_CONVERT_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	switch (TREE_CODE (_q30))
	  {
	  case CALL_EXPR:
	    switch (get_call_combined_fn (_q30))
	      {
	      case CFN_COND_LEN_FMA:
	        if (call_expr_nargs (_q30) == 7)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    tree _q45 = CALL_EXPR_ARG (_q30, 5);
		    tree _q46 = CALL_EXPR_ARG (_q30, 6);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _q45, _q46, _p2 };
			  tree res = generic_simplify_515 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FMA);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_LEN_FMS:
	        if (call_expr_nargs (_q30) == 7)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    tree _q45 = CALL_EXPR_ARG (_q30, 5);
		    tree _q46 = CALL_EXPR_ARG (_q30, 6);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _q45, _q46, _p2 };
			  tree res = generic_simplify_515 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FMS);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_LEN_FNMA:
	        if (call_expr_nargs (_q30) == 7)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    tree _q45 = CALL_EXPR_ARG (_q30, 5);
		    tree _q46 = CALL_EXPR_ARG (_q30, 6);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _q45, _q46, _p2 };
			  tree res = generic_simplify_515 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FNMA);
			  if (res) return res;
			}
		      }
	          }
	        break;
	      case CFN_COND_LEN_FNMS:
	        if (call_expr_nargs (_q30) == 7)
    {
		    tree _q40 = CALL_EXPR_ARG (_q30, 0);
		    tree _q41 = CALL_EXPR_ARG (_q30, 1);
		    tree _q42 = CALL_EXPR_ARG (_q30, 2);
		    tree _q43 = CALL_EXPR_ARG (_q30, 3);
		    tree _q44 = CALL_EXPR_ARG (_q30, 4);
		    tree _q45 = CALL_EXPR_ARG (_q30, 5);
		    tree _q46 = CALL_EXPR_ARG (_q30, 6);
		    if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		      {
			{
			  tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q41, _q42, _q43, _q44, _q45, _q46, _p2 };
			  tree res = generic_simplify_515 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FNMS);
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
    case CALL_EXPR:
      switch (get_call_combined_fn (_p1))
        {
	case CFN_COND_LEN_FMA:
	  if (call_expr_nargs (_p1) == 7)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      tree _q35 = CALL_EXPR_ARG (_p1, 5);
	      tree _q36 = CALL_EXPR_ARG (_p1, 6);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _q35, _q36, _p2 };
		    tree res = generic_simplify_515 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FMA);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_LEN_FMS:
	  if (call_expr_nargs (_p1) == 7)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      tree _q35 = CALL_EXPR_ARG (_p1, 5);
	      tree _q36 = CALL_EXPR_ARG (_p1, 6);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _q35, _q36, _p2 };
		    tree res = generic_simplify_515 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FMS);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_LEN_FNMA:
	  if (call_expr_nargs (_p1) == 7)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      tree _q35 = CALL_EXPR_ARG (_p1, 5);
	      tree _q36 = CALL_EXPR_ARG (_p1, 6);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _q35, _q36, _p2 };
		    tree res = generic_simplify_515 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FNMA);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_LEN_FNMS:
	  if (call_expr_nargs (_p1) == 7)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      tree _q35 = CALL_EXPR_ARG (_p1, 5);
	      tree _q36 = CALL_EXPR_ARG (_p1, 6);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q31, _q32, _q33, _q34, _q35, _q36, _p2 };
		    tree res = generic_simplify_515 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FNMS);
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
  switch (TREE_CODE (_p2))
    {
    case VIEW_CONVERT_EXPR:
      {
	tree _q40 = TREE_OPERAND (_p2, 0);
	switch (TREE_CODE (_q40))
	  {
	  case CALL_EXPR:
	    switch (get_call_combined_fn (_q40))
	      {
	      case CFN_COND_LEN_FMA:
	        if (call_expr_nargs (_q40) == 7)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    tree _q55 = CALL_EXPR_ARG (_q40, 5);
		    tree _q56 = CALL_EXPR_ARG (_q40, 6);
		    {
		      tree captures[9] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54, _q55, _q56 };
		      tree res = generic_simplify_516 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FMA);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_LEN_FMS:
	        if (call_expr_nargs (_q40) == 7)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    tree _q55 = CALL_EXPR_ARG (_q40, 5);
		    tree _q56 = CALL_EXPR_ARG (_q40, 6);
		    {
		      tree captures[9] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54, _q55, _q56 };
		      tree res = generic_simplify_516 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FMS);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_LEN_FNMA:
	        if (call_expr_nargs (_q40) == 7)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    tree _q55 = CALL_EXPR_ARG (_q40, 5);
		    tree _q56 = CALL_EXPR_ARG (_q40, 6);
		    {
		      tree captures[9] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54, _q55, _q56 };
		      tree res = generic_simplify_516 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FNMA);
		      if (res) return res;
		    }
	          }
	        break;
	      case CFN_COND_LEN_FNMS:
	        if (call_expr_nargs (_q40) == 7)
    {
		    tree _q50 = CALL_EXPR_ARG (_q40, 0);
		    tree _q51 = CALL_EXPR_ARG (_q40, 1);
		    tree _q52 = CALL_EXPR_ARG (_q40, 2);
		    tree _q53 = CALL_EXPR_ARG (_q40, 3);
		    tree _q54 = CALL_EXPR_ARG (_q40, 4);
		    tree _q55 = CALL_EXPR_ARG (_q40, 5);
		    tree _q56 = CALL_EXPR_ARG (_q40, 6);
		    {
		      tree captures[9] ATTRIBUTE_UNUSED = { _p0, _p1, _q50, _q51, _q52, _q53, _q54, _q55, _q56 };
		      tree res = generic_simplify_516 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FNMS);
		      if (res) return res;
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
      switch (get_call_combined_fn (_p2))
        {
	case CFN_COND_LEN_FMA:
	  if (call_expr_nargs (_p2) == 7)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      tree _q45 = CALL_EXPR_ARG (_p2, 5);
	      tree _q46 = CALL_EXPR_ARG (_p2, 6);
	      {
		tree captures[9] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44, _q45, _q46 };
		tree res = generic_simplify_516 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FMA);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_LEN_FMS:
	  if (call_expr_nargs (_p2) == 7)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      tree _q45 = CALL_EXPR_ARG (_p2, 5);
	      tree _q46 = CALL_EXPR_ARG (_p2, 6);
	      {
		tree captures[9] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44, _q45, _q46 };
		tree res = generic_simplify_516 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FMS);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_LEN_FNMA:
	  if (call_expr_nargs (_p2) == 7)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      tree _q45 = CALL_EXPR_ARG (_p2, 5);
	      tree _q46 = CALL_EXPR_ARG (_p2, 6);
	      {
		tree captures[9] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44, _q45, _q46 };
		tree res = generic_simplify_516 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FNMA);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_COND_LEN_FNMS:
	  if (call_expr_nargs (_p2) == 7)
    {
	      tree _q40 = CALL_EXPR_ARG (_p2, 0);
	      tree _q41 = CALL_EXPR_ARG (_p2, 1);
	      tree _q42 = CALL_EXPR_ARG (_p2, 2);
	      tree _q43 = CALL_EXPR_ARG (_p2, 3);
	      tree _q44 = CALL_EXPR_ARG (_p2, 4);
	      tree _q45 = CALL_EXPR_ARG (_p2, 5);
	      tree _q46 = CALL_EXPR_ARG (_p2, 6);
	      {
		tree captures[9] ATTRIBUTE_UNUSED = { _p0, _p1, _q40, _q41, _q42, _q43, _q44, _q45, _q46 };
		tree res = generic_simplify_516 (loc, type, _p0, _p1, _p2, captures, CFN_COND_LEN_FNMS);
		if (res) return res;
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
	case CFN_COND_ADD:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_p2 == _q33 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q33, 0) && types_match (_p2, _q33)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _q33 };
		    tree res = generic_simplify_517 (loc, type, _p0, _p1, _p2, captures, CFN_COND_ADD);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_AND:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_p2 == _q33 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q33, 0) && types_match (_p2, _q33)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _q33 };
		    tree res = generic_simplify_517 (loc, type, _p0, _p1, _p2, captures, CFN_COND_AND);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_DIV:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_p2 == _q33 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q33, 0) && types_match (_p2, _q33)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _q33 };
		    tree res = generic_simplify_517 (loc, type, _p0, _p1, _p2, captures, CFN_COND_DIV);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_FMA:
	  if (call_expr_nargs (_p1) == 5)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      if ((_p2 == _q34 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q34, 0) && types_match (_p2, _q34)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _q33, _q34 };
		    tree res = generic_simplify_518 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FMA);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_FMS:
	  if (call_expr_nargs (_p1) == 5)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      if ((_p2 == _q34 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q34, 0) && types_match (_p2, _q34)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _q33, _q34 };
		    tree res = generic_simplify_518 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FMS);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_IOR:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_p2 == _q33 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q33, 0) && types_match (_p2, _q33)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _q33 };
		    tree res = generic_simplify_517 (loc, type, _p0, _p1, _p2, captures, CFN_COND_IOR);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_MAX:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_p2 == _q33 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q33, 0) && types_match (_p2, _q33)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _q33 };
		    tree res = generic_simplify_517 (loc, type, _p0, _p1, _p2, captures, CFN_COND_MAX);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_MIN:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_p2 == _q33 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q33, 0) && types_match (_p2, _q33)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _q33 };
		    tree res = generic_simplify_517 (loc, type, _p0, _p1, _p2, captures, CFN_COND_MIN);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_MOD:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_p2 == _q33 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q33, 0) && types_match (_p2, _q33)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _q33 };
		    tree res = generic_simplify_517 (loc, type, _p0, _p1, _p2, captures, CFN_COND_MOD);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_MUL:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_p2 == _q33 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q33, 0) && types_match (_p2, _q33)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _q33 };
		    tree res = generic_simplify_517 (loc, type, _p0, _p1, _p2, captures, CFN_COND_MUL);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_SHL:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_p2 == _q33 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q33, 0) && types_match (_p2, _q33)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _q33 };
		    tree res = generic_simplify_517 (loc, type, _p0, _p1, _p2, captures, CFN_COND_SHL);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_SHR:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_p2 == _q33 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q33, 0) && types_match (_p2, _q33)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _q33 };
		    tree res = generic_simplify_517 (loc, type, _p0, _p1, _p2, captures, CFN_COND_SHR);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_SUB:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_p2 == _q33 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q33, 0) && types_match (_p2, _q33)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _q33 };
		    tree res = generic_simplify_517 (loc, type, _p0, _p1, _p2, captures, CFN_COND_SUB);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_XOR:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_p2 == _q33 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q33, 0) && types_match (_p2, _q33)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _q33 };
		    tree res = generic_simplify_517 (loc, type, _p0, _p1, _p2, captures, CFN_COND_XOR);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_COPYSIGN:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_p2 == _q33 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q33, 0) && types_match (_p2, _q33)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _q33 };
		    tree res = generic_simplify_517 (loc, type, _p0, _p1, _p2, captures, CFN_COND_COPYSIGN);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_FMAX:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_p2 == _q33 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q33, 0) && types_match (_p2, _q33)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _q33 };
		    tree res = generic_simplify_517 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FMAX);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_FMIN:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_p2 == _q33 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q33, 0) && types_match (_p2, _q33)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _q33 };
		    tree res = generic_simplify_517 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FMIN);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_FNMA:
	  if (call_expr_nargs (_p1) == 5)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      if ((_p2 == _q34 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q34, 0) && types_match (_p2, _q34)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _q33, _q34 };
		    tree res = generic_simplify_518 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FNMA);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_FNMS:
	  if (call_expr_nargs (_p1) == 5)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      tree _q34 = CALL_EXPR_ARG (_p1, 4);
	      if ((_p2 == _q34 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q34, 0) && types_match (_p2, _q34)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _q33, _q34 };
		    tree res = generic_simplify_518 (loc, type, _p0, _p1, _p2, captures, CFN_COND_FNMS);
		    if (res) return res;
		  }
	        }
	    }
	  break;
	case CFN_COND_RDIV:
	  if (call_expr_nargs (_p1) == 4)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      tree _q32 = CALL_EXPR_ARG (_p1, 2);
	      tree _q33 = CALL_EXPR_ARG (_p1, 3);
	      if ((_p2 == _q33 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q33, 0) && types_match (_p2, _q33)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31, _q32, _q33 };
		    tree res = generic_simplify_517 (loc, type, _p0, _p1, _p2, captures, CFN_COND_RDIV);
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
