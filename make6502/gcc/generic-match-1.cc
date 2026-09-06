/* Generated automatically by the program `genmatch' from
   a IL pattern matching and simplification description.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-function"

#include "generic-match-auto.h"

bool
tree_zero_one_valued_p (tree t)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  {
    tree captures[1] ATTRIBUTE_UNUSED = { t };
    if (INTEGRAL_TYPE_P (type)
 && (TYPE_UNSIGNED (type)
 || TYPE_PRECISION (type) > 1)
 && wi::leu_p (tree_nonzero_bits (captures[0]), 1)
)
      {
	{
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 28, __FILE__, __LINE__, false);
	  return true;
	}
      }
  }
  switch (TREE_CODE (t))
    {
    case BIT_AND_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	if (integer_onep (_p1))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { t, _p0 };
	      if (INTEGRAL_TYPE_P (type)
)
		{
		  {
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 29, __FILE__, __LINE__, false);
		    return true;
		  }
		}
	    }
	  }
	if (integer_onep (_p0))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { t, _p1 };
	      if (INTEGRAL_TYPE_P (type)
)
		{
		  {
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 29, __FILE__, __LINE__, false);
		    return true;
		  }
		}
	    }
	  }
        break;
      }
    CASE_CONVERT:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { t, _p0 };
	  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && (TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 || TYPE_PRECISION (TREE_TYPE (captures[1])) > 1)
 && INTEGRAL_TYPE_P (type)
 && (TYPE_UNSIGNED (type)
 || TYPE_PRECISION (type) > 1)
 && wi::leu_p (tree_nonzero_bits (captures[1]), 1)
)
	    {
	      {
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 30, __FILE__, __LINE__, false);
		return true;
	      }
	    }
	}
        break;
      }
    default:;
    }
if (tree_truth_valued_p (t))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { t };
      if (INTEGRAL_TYPE_P (type)
 && (TYPE_UNSIGNED (type)
 || TYPE_PRECISION (type) > 1)
)
	{
	  {
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 31, __FILE__, __LINE__, false);
	    return true;
	  }
	}
    }
  }
  return false;
}

bool
tree_unsigned_integer_sat_sub (tree t, tree *res_ops)
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
				  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
				  if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
				    {
				      {
					res_ops[0] = captures[0];
					res_ops[1] = captures[1];
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 61, __FILE__, __LINE__, false);
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
	  case GE_EXPR:
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
		      case MINUS_EXPR:
		        {
			  tree _q60 = TREE_OPERAND (_q50, 0);
			  tree _q61 = TREE_OPERAND (_q50, 1);
			  switch (TREE_CODE (_q60))
			    {
			    CASE_CONVERT:
			      {
				tree _q70 = TREE_OPERAND (_q60, 0);
				if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
				  {
				    switch (TREE_CODE (_q61))
				      {
				      CASE_CONVERT:
				        {
					  tree _q90 = TREE_OPERAND (_q61, 0);
					  if ((_q90 == _q21 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q21, 0) && types_match (_q90, _q21)))
					    {
					      if (integer_zerop (_p2))
						{
						  {
						    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
						    if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && TYPE_UNSIGNED (TREE_TYPE (captures[0])) && types_match (captures[0], captures[1])
)
						      {
							{
							  res_ops[0] = captures[0];
							  res_ops[1] = captures[1];
							  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 62, __FILE__, __LINE__, false);
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
				  }
			        break;
			      }
			    default:;
			    }
			  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
			    {
			      if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
				{
				  if (integer_zerop (_p2))
				    {
				      {
					tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && TYPE_UNSIGNED (TREE_TYPE (captures[0])) && types_match (captures[0], captures[1])
)
					  {
					    {
					      res_ops[0] = captures[0];
					      res_ops[1] = captures[1];
					      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 62, __FILE__, __LINE__, false);
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
		case MINUS_EXPR:
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
			      switch (TREE_CODE (_q51))
			        {
				CASE_CONVERT:
				  {
				    tree _q80 = TREE_OPERAND (_q51, 0);
				    if ((_q80 == _q21 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q21, 0) && types_match (_q80, _q21)))
				      {
					if (integer_zerop (_p2))
					  {
					    {
					      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
					      if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && TYPE_UNSIGNED (TREE_TYPE (captures[0])) && types_match (captures[0], captures[1])
)
						{
						  {
						    res_ops[0] = captures[0];
						    res_ops[1] = captures[1];
						    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 62, __FILE__, __LINE__, false);
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
			    }
		          break;
		        }
		      default:;
		      }
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			  {
			    if (integer_zerop (_p2))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
				  if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && TYPE_UNSIGNED (TREE_TYPE (captures[0])) && types_match (captures[0], captures[1])
)
				    {
				      {
					res_ops[0] = captures[0];
					res_ops[1] = captures[1];
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 62, __FILE__, __LINE__, false);
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
			  case CFN_SUB_OVERFLOW:
			    if (call_expr_nargs (_q30) == 2)
    {
				tree _q40 = CALL_EXPR_ARG (_q30, 0);
				tree _q41 = CALL_EXPR_ARG (_q30, 1);
				if (integer_zerop (_q21))
				  {
				    switch (TREE_CODE (_p1))
				      {
				      case REALPART_EXPR:
				        {
					  tree _q80 = TREE_OPERAND (_p1, 0);
					  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
					    {
					      if (integer_zerop (_p2))
						{
						  {
						    tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _q30 };
						    if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
						      {
							{
							  res_ops[0] = captures[0];
							  res_ops[1] = captures[1];
							  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 63, __FILE__, __LINE__, false);
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
			  case CFN_SUB_OVERFLOW:
			    if (call_expr_nargs (_q30) == 2)
    {
				tree _q40 = CALL_EXPR_ARG (_q30, 0);
				tree _q41 = CALL_EXPR_ARG (_q30, 1);
				if (integer_zerop (_q21))
				  {
				    if (integer_zerop (_p1))
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
 && types_match (type, captures[0], captures[1])
)
						      {
							{
							  res_ops[0] = captures[0];
							  res_ops[1] = captures[1];
							  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 64, __FILE__, __LINE__, false);
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
	      switch (TREE_CODE (_q21))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case MINUS_EXPR:
		        {
			  tree _q50 = TREE_OPERAND (_p1, 0);
			  tree _q51 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q50))
			    {
			    case INTEGER_CST:
			      {
				if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
				  {
				    if (integer_zerop (_p2))
				      {
					{
					  tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q20, _q21 };
					  if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[1]) && int_fits_type_p (captures[0], type)
)
					    {
					      {
 unsigned precision = TYPE_PRECISION (type);
 wide_int max = wi::mask (precision, false, precision);
 wide_int c0 = wi::to_wide (captures[0]);
 wide_int c2 = wi::to_wide (captures[2]);
 wide_int c2_add_1 = wi::add (c2, wi::uhwi (1, precision));
 bool equal_p = wi::eq_p (c0, c2);
 bool less_than_1_p = !wi::eq_p (c2, max) && wi::eq_p (c2_add_1, c0);
						  if (equal_p || less_than_1_p
)
						    {
						      {
							res_ops[0] = captures[0];
							res_ops[1] = captures[1];
							if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 65, __FILE__, __LINE__, false);
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
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	switch (TREE_CODE (_p0))
	  {
	  case MINUS_EXPR:
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
		      case GT_EXPR:
		        {
			  tree _q60 = TREE_OPERAND (_q50, 0);
			  tree _q61 = TREE_OPERAND (_q50, 1);
			  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
			    {
			      if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
				{
				  {
				    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
				    if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
				      {
					{
					  res_ops[0] = captures[0];
					  res_ops[1] = captures[1];
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 66, __FILE__, __LINE__, false);
					  return true;
					}
				      }
				  }
			        }
			    }
		          break;
		        }
		      case GE_EXPR:
		        {
			  tree _q60 = TREE_OPERAND (_q50, 0);
			  tree _q61 = TREE_OPERAND (_q50, 1);
			  if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
			    {
			      if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
				{
				  {
				    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
				    if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
				      {
					{
					  res_ops[0] = captures[0];
					  res_ops[1] = captures[1];
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 67, __FILE__, __LINE__, false);
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
	  CASE_CONVERT:
	    {
	      tree _q20 = TREE_OPERAND (_p0, 0);
	      switch (TREE_CODE (_q20))
	        {
		case GT_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case MINUS_EXPR:
		        {
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  tree _q61 = TREE_OPERAND (_p1, 1);
			  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
			    {
			      if ((_q61 == _q31 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q31, 0) && types_match (_q61, _q31)))
				{
				  {
				    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
				    if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
				      {
					{
					  res_ops[0] = captures[0];
					  res_ops[1] = captures[1];
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 66, __FILE__, __LINE__, false);
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
		case GE_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case MINUS_EXPR:
		        {
			  tree _q60 = TREE_OPERAND (_p1, 0);
			  tree _q61 = TREE_OPERAND (_p1, 1);
			  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
			    {
			      if ((_q61 == _q31 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q31, 0) && types_match (_q61, _q31)))
				{
				  {
				    tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
				    if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
				      {
					{
					  res_ops[0] = captures[0];
					  res_ops[1] = captures[1];
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 67, __FILE__, __LINE__, false);
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
			  switch (TREE_CODE (_p1))
			    {
			    case BIT_XOR_EXPR:
			      {
				tree _q60 = TREE_OPERAND (_p1, 0);
				tree _q61 = TREE_OPERAND (_p1, 1);
				switch (TREE_CODE (_q60))
				  {
				  case IMAGPART_EXPR:
				    {
				      tree _q70 = TREE_OPERAND (_q60, 0);
				      if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
					{
					  if (integer_onep (_q61))
					    {
					      {
						tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q20 };
						if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
						  {
						    {
						      res_ops[0] = captures[0];
						      res_ops[1] = captures[1];
						      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 68, __FILE__, __LINE__, false);
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
		        }
		      break;
		    default:;
		    }
		  break;
	        default:;
	        }
	      break;
	    }
	  case BIT_XOR_EXPR:
	    {
	      tree _q20 = TREE_OPERAND (_p0, 0);
	      tree _q21 = TREE_OPERAND (_p0, 1);
	      switch (TREE_CODE (_q20))
	        {
		case IMAGPART_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    if (integer_onep (_q21))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case REALPART_EXPR:
			    {
			      tree _q60 = TREE_OPERAND (_p1, 0);
			      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
				{
				  switch (TREE_CODE (_q60))
				    {
				    case CALL_EXPR:
				      switch (get_call_combined_fn (_q60))
				        {
					case CFN_SUB_OVERFLOW:
					  if (call_expr_nargs (_q60) == 2)
    {
					      tree _q80 = CALL_EXPR_ARG (_q60, 0);
					      tree _q81 = CALL_EXPR_ARG (_q60, 1);
					      {
						tree captures[3] ATTRIBUTE_UNUSED = { _q80, _q81, _q30 };
						if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
						  {
						    {
						      res_ops[0] = captures[0];
						      res_ops[1] = captures[1];
						      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 68, __FILE__, __LINE__, false);
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
    case BIT_AND_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
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
			  switch (TREE_CODE (_p1))
			    {
			    case PLUS_EXPR:
			      {
				tree _q60 = TREE_OPERAND (_p1, 0);
				tree _q61 = TREE_OPERAND (_p1, 1);
				switch (TREE_CODE (_q60))
				  {
				  case IMAGPART_EXPR:
				    {
				      tree _q70 = TREE_OPERAND (_q60, 0);
				      if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
					{
					  if (integer_minus_onep (_q61))
					    {
					      {
						tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _q20 };
						if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
						  {
						    {
						      res_ops[0] = captures[0];
						      res_ops[1] = captures[1];
						      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 69, __FILE__, __LINE__, false);
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
		        }
		      break;
		    default:;
		    }
		  break;
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
		case IMAGPART_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    if (integer_minus_onep (_q21))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case REALPART_EXPR:
			    {
			      tree _q60 = TREE_OPERAND (_p1, 0);
			      if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
				{
				  switch (TREE_CODE (_q60))
				    {
				    case CALL_EXPR:
				      switch (get_call_combined_fn (_q60))
				        {
					case CFN_SUB_OVERFLOW:
					  if (call_expr_nargs (_q60) == 2)
    {
					      tree _q80 = CALL_EXPR_ARG (_q60, 0);
					      tree _q81 = CALL_EXPR_ARG (_q60, 1);
					      {
						tree captures[3] ATTRIBUTE_UNUSED = { _q80, _q81, _q30 };
						if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
						  {
						    {
						      res_ops[0] = captures[0];
						      res_ops[1] = captures[1];
						      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 69, __FILE__, __LINE__, false);
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
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	switch (TREE_CODE (_p0))
	  {
	  case MAX_EXPR:
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
			    if (INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type)
 && types_match (type, captures[1]) && int_fits_type_p (captures[1], type)
)
			      {
				{
 unsigned precision = TYPE_PRECISION (type);
 wide_int c1 = wi::to_wide (captures[1]);
 wide_int c2 = wi::to_wide (captures[2]);
 wide_int sum = wi::add (c1, c2);
				    if (wi::eq_p (sum, wi::uhwi (0, precision))
)
				      {
					{
					  res_ops[0] = captures[0];
					  res_ops[1] = captures[1];
					  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 70, __FILE__, __LINE__, false);
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
generic_simplify_70 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail304;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail304;
  {
    tree _r;
    _r = captures[2];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 171, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail304:;
  return NULL_TREE;
}

tree
generic_simplify_73 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail307;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail307;
  {
    tree _r;
    _r = captures[0];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 175, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail307:;
  return NULL_TREE;
}

tree
generic_simplify_80 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail314;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail314;
  {
    tree _r;
    _r = captures[2];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 177, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail314:;
  return NULL_TREE;
}

tree
generic_simplify_86 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail320;
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
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 184, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail320:;
  return NULL_TREE;
}

tree
generic_simplify_95 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail331;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail331;
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
next_after_fail331:;
	}
      else
	{
	  if (single_use (captures[0]) && single_use (captures[1])
)
	    {
	      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail332;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail332;
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
next_after_fail332:;
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_104 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (bitwise_equal_p (captures[4], captures[7])
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail342;
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
next_after_fail342:;
    }
  return NULL_TREE;
}

tree
generic_simplify_114 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail355;
  {
    tree _r;
    _r =  constant_boolean_node (true, type);
    if (TREE_SIDE_EFFECTS (captures[0]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 205, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail355:;
  return NULL_TREE;
}

tree
generic_simplify_119 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail364;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail364;
  {
    tree _r;
    _r = captures[2];
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 214, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail364:;
  return NULL_TREE;
}

tree
generic_simplify_123 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail374;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail374;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail374;
	      {
		tree _r;
		_r = captures[3];
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 217, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail374:;
	    }
	  else
	    {
	      if (code1 == NE_EXPR && val && allbits
)
		{
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail375;
		  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail375;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail375;
		  {
		    tree _r;
		    _r =  constant_boolean_node (true, type);
		    if (TREE_SIDE_EFFECTS (captures[4]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[4]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 218, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail375:;
		}
	      else
		{
		  if (code1 == NE_EXPR && !val && allbits
)
		    {
		      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail376;
		      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail376;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail376;
		      {
			tree _r;
			_r = captures[0];
			if (TREE_SIDE_EFFECTS (captures[4]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[4]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 219, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail376:;
		    }
		  else
		    {
		      if (code1 == EQ_EXPR
 && code2 == GT_EXPR
 && cmp == 0
 && allbits
)
			{
			  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail377;
			  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail377;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail377;
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
next_after_fail377:;
			}
		      else
			{
			  if (code1 == EQ_EXPR
 && code2 == LT_EXPR
 && cmp == 0
 && allbits
)
			    {
			      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail378;
			      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail378;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail378;
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
next_after_fail378:;
			    }
			  else
			    {
			      if (code1 == EQ_EXPR
 && code2 == GE_EXPR
 && one_before
 && allbits
)
				{
				  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail379;
				  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail379;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail379;
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
next_after_fail379:;
				}
			      else
				{
				  if (code1 == EQ_EXPR
 && code2 == LE_EXPR
 && one_after
 && allbits
)
				    {
				      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail380;
				      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail380;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail380;
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
next_after_fail380:;
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
generic_simplify_148 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail425;
  {
    if (! tree_invariant_p (captures[3])) goto next_after_fail425;
    tree res_op0;
    {
      tree _o1[2], _r1;
      _o1[0] = captures[2];
      _o1[1] = unshare_expr (captures[3]);
      _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      res_op0 = _r1;
    }
    tree res_op1;
    {
      tree _o1[2], _r1;
      _o1[0] = captures[4];
      _o1[1] = captures[3];
      _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      if (EXPR_P (_r1))
        goto next_after_fail425;
      res_op1 = _r1;
    }
    tree _r;
    _r = fold_build2_loc (loc, BIT_XOR_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 244, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail425:;
  return NULL_TREE;
}

tree
generic_simplify_156 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail433;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree _r;
    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
    if (TREE_SIDE_EFFECTS (captures[2]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 252, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail433:;
  return NULL_TREE;
}

tree
generic_simplify_162 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[2]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail439;
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
next_after_fail439:;
    }
  return NULL_TREE;
}

tree
generic_simplify_169 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (COPYSIGN_ALL))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!tree_expr_maybe_nan_p (captures[0]) && !HONOR_SIGNED_ZEROS (type)
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail448;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail448;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree _r;
	_r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 265, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail448:;
    }
  return NULL_TREE;
}

tree
generic_simplify_176 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail455;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail455;
  {
    tree _r;
    _r =  build_zero_cst (type);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 270, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail455:;
  return NULL_TREE;
}

tree
generic_simplify_181 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail462;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail462;
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
next_after_fail462:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_189 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (POW))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail471;
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
	_r = maybe_build_call_expr_loc (loc, POW, type, 2, res_op0, res_op1);
	if (!_r)
	  goto next_after_fail471;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 283, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail471:;
    }
  return NULL_TREE;
}

tree
generic_simplify_197 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail482;
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
next_after_fail482:;
    }
  return NULL_TREE;
}

tree
generic_simplify_204 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_OVERFLOW_SANITIZED (TREE_TYPE (captures[1]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail502;
      {
	tree res_op0;
	res_op0 = captures[3];
	tree res_op1;
	res_op1 = captures[2];
	tree _r;
	_r = fold_build2_loc (loc, op, type, res_op0, res_op1);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 313, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail502:;
    }
  return NULL_TREE;
}

tree
generic_simplify_209 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_INT_CST_LOW (captures[1]) & 1
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail507;
      {
	tree _r;
	_r =  constant_boolean_node (cmp == NE_EXPR, type);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 317, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail507:;
    }
  return NULL_TREE;
}

tree
generic_simplify_214 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (wi::gt_p (wi::to_wide (captures[1]), wi::to_wide (captures[2]),
 TYPE_SIGN (TREE_TYPE (captures[0])))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail513;
      {
	tree _r;
	_r =  constant_boolean_node (cmp == NE_EXPR, type);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (TREE_SIDE_EFFECTS (captures[2]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 323, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail513:;
    }
  else
    {
      if (wi::lt_p (wi::to_wide (captures[1]), wi::to_wide (captures[2]),
 TYPE_SIGN (TREE_TYPE (captures[0])))
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail514;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree res_op1;
	    res_op1 = captures[2];
	    tree _r;
	    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 324, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail514:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_222 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (rotate),
 const enum tree_code ARG_UNUSED (invrot),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail532;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 = captures[2];
    tree _r;
    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 342, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail532:;
  return NULL_TREE;
}

tree
generic_simplify_226 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const combined_fn ARG_UNUSED (bswap))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree ctype = TREE_TYPE (captures[0]);
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail536;
      {
	tree res_op0;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[1];
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
	  _o1[0] = captures[2];
	  if (TREE_TYPE (_o1[0]) != ctype)
	    {
	      _r1 = fold_build1_loc (loc, NOP_EXPR, ctype, _o1[0]);
	    }
	  else
	    _r1 = _o1[0];
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 346, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail536:;
  }
  return NULL_TREE;
}

tree
generic_simplify_233 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))
 && wi::to_wide (captures[1]) != 0
)
    {
      {
 widest_int quot;
	  if (wi::multiple_of_p (wi::to_widest (captures[2]), wi::to_widest (captures[1]),
 TYPE_SIGN (TREE_TYPE (captures[0])), &quot)
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail546;
	      {
		tree res_op0;
		res_op0 = captures[0];
		tree res_op1;
		res_op1 =  wide_int_to_tree (TREE_TYPE (captures[0]), quot);
		tree _r;
		_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (TREE_SIDE_EFFECTS (captures[2]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 355, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail546:;
	    }
	  else
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail547;
	      {
		tree _r;
		_r =  constant_boolean_node (cmp == NE_EXPR, type);
		if (TREE_SIDE_EFFECTS (captures[0]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (TREE_SIDE_EFFECTS (captures[2]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 356, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail547:;
	    }
      }
    }
  else
    {
      if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[0]))
 && (wi::bit_and (wi::to_wide (captures[1]), 1) == 1)
)
	{
	  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail548;
	  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail548;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail548;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree res_op1;
 tree itype = TREE_TYPE (captures[0]);
 int p = TYPE_PRECISION (itype);
 wide_int m = wi::one (p + 1) << p;
 wide_int a = wide_int::from (wi::to_wide (captures[1]), p + 1, UNSIGNED);
 wide_int i = wide_int::from (wi::mod_inv (a, m),
 p, TYPE_SIGN (itype));	    res_op1 = 
 wide_int_to_tree (itype, wi::mul (i, wi::to_wide (captures[2])));
	    tree _r;
	    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 357, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail548:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_245 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (SCALAR_FLOAT_TYPE_P (TREE_TYPE (captures[1]))
 && ! DECIMAL_FLOAT_TYPE_P (TREE_TYPE (captures[1]))
)
    {
      {
 tree itype = TREE_TYPE (captures[0]);
 format_helper fmt (REAL_MODE_FORMAT (TYPE_MODE (TREE_TYPE (captures[1]))));
 const REAL_VALUE_TYPE *cst = TREE_REAL_CST_PTR (captures[1]);
 bool exception_p = real_isnan (cst)
 && flag_trapping_math
 && ((cmp != EQ_EXPR && cmp != NE_EXPR)
 || (cst->signalling
 && HONOR_SNANS (TREE_TYPE (captures[1]))));
	  if (fmt.can_represent_integral_type_p (itype) && ! exception_p
)
	    {
	      {
 signop isign = TYPE_SIGN (itype);
 REAL_VALUE_TYPE imin, imax;
 real_from_integer (&imin, fmt, wi::min_value (itype), isign);
 real_from_integer (&imax, fmt, wi::max_value (itype), isign);
 REAL_VALUE_TYPE icst;
 if (cmp == GT_EXPR || cmp == GE_EXPR)
 real_ceil (&icst, fmt, cst);
 else if (cmp == LT_EXPR || cmp == LE_EXPR)
 real_floor (&icst, fmt, cst);
 else
 real_trunc (&icst, fmt, cst);
 bool cst_int_p = !real_isnan (cst) && real_identical (&icst, cst);
 bool overflow_p = false;
 wide_int icst_val
 = real_to_integer (&icst, &overflow_p, TYPE_PRECISION (itype));
		  if (real_compare (LT_EXPR, cst, &imin)
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail596;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail596;
		      {
			tree _r;
			_r =  constant_boolean_node (cmp == GT_EXPR || cmp == GE_EXPR || cmp == NE_EXPR,
 type);
			if (TREE_SIDE_EFFECTS (captures[0]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 404, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail596:;
		    }
		  else
		    {
		      if (real_compare (GT_EXPR, cst, &imax)
)
			{
			  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail597;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail597;
			  {
			    tree _r;
			    _r =  constant_boolean_node (cmp == LT_EXPR || cmp == LE_EXPR || cmp == NE_EXPR,
 type);
			    if (TREE_SIDE_EFFECTS (captures[0]))
			      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 405, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail597:;
			}
		      else
			{
			  if (cst_int_p
)
			    {
			      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail598;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail598;
			      {
				tree res_op0;
				res_op0 = captures[0];
				tree res_op1;
 gcc_assert (!overflow_p);				res_op1 = 
 wide_int_to_tree (itype, icst_val);
				tree _r;
				_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 406, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail598:;
			    }
			  else
			    {
			      if (cmp == EQ_EXPR || cmp == NE_EXPR
)
				{
				  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail599;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail599;
				  {
				    tree _r;
				    _r =  constant_boolean_node (cmp == NE_EXPR, type);
				    if (TREE_SIDE_EFFECTS (captures[0]))
				      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 407, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail599:;
				}
			      else
				{
				  {
 gcc_checking_assert (!overflow_p);
				      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail600;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail600;
				      {
					tree res_op0;
					res_op0 = captures[0];
					tree res_op1;
					res_op1 =  wide_int_to_tree (itype, icst_val);
					tree _r;
					_r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
					if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 408, __FILE__, __LINE__, true);
					return _r;
				      }
next_after_fail600:;
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
generic_simplify_257 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail629;
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
	  goto next_after_fail629;
	_o1[1] = _r2;
      }
      _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      res_op0 = _r1;
    }
    tree res_op1;
    res_op1 =  build_zero_cst (TREE_TYPE (captures[0]));
    tree _r;
    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 430, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail629:;
  return NULL_TREE;
}

tree
generic_simplify_260 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail632;
  {
    tree res_op0;
    {
      tree _o1[2], _r1;
      {
	tree _o2[2], _r2;
	_o2[0] = captures[1];
	_o2[1] = captures[4];
	_r2 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
	_o1[0] = _r2;
      }
      _o1[1] = captures[2];
      _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
      res_op0 = _r1;
    }
    tree res_op1;
    res_op1 =  build_zero_cst (TREE_TYPE (captures[2]));
    tree _r;
    _r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 433, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail632:;
  return NULL_TREE;
}

tree
generic_simplify_264 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (sgncmp),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (ncmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && !TYPE_UNSIGNED (TREE_TYPE (captures[0]))
 && types_match (captures[0], captures[2])
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail636;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail636;
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
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 436, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail636:;
    }
  return NULL_TREE;
}

tree
generic_simplify_267 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail639;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail639;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail639;
	      {
		tree _r;
		_r =  constant_boolean_node (known_eq (off0, off1), type);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 438, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail639:;
	    }
	  else
	    {
	      if (cmp == NE_EXPR && (known_eq (off0, off1) || known_ne (off0, off1))
)
		{
		  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail640;
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail640;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail640;
		  {
		    tree _r;
		    _r =  constant_boolean_node (known_ne (off0, off1), type);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 439, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail640:;
		}
	      else
		{
		  if (cmp == LT_EXPR && (known_lt (off0, off1) || known_ge (off0, off1))
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail641;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail641;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail641;
		      {
			tree _r;
			_r =  constant_boolean_node (known_lt (off0, off1), type);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 440, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail641:;
		    }
		  else
		    {
		      if (cmp == LE_EXPR && (known_le (off0, off1) || known_gt (off0, off1))
)
			{
			  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail642;
			  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail642;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail642;
			  {
			    tree _r;
			    _r =  constant_boolean_node (known_le (off0, off1), type);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 441, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail642:;
			}
		      else
			{
			  if (cmp == GE_EXPR && (known_ge (off0, off1) || known_lt (off0, off1))
)
			    {
			      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail643;
			      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail643;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail643;
			      {
				tree _r;
				_r =  constant_boolean_node (known_ge (off0, off1), type);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 442, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail643:;
			    }
			  else
			    {
			      if (cmp == GT_EXPR && (known_gt (off0, off1) || known_le (off0, off1))
)
				{
				  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail644;
				  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail644;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail644;
				  {
				    tree _r;
				    _r =  constant_boolean_node (known_gt (off0, off1), type);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 443, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail644:;
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
		  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail645;
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail645;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail645;
		  {
		    tree _r;
		    _r =  constant_boolean_node (false, type);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 444, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail645:;
		}
	      else
		{
		  if (cmp == NE_EXPR
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail646;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail646;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail646;
		      {
			tree _r;
			_r =  constant_boolean_node (true, type);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 445, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail646:;
		    }
		}
	    }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_279 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (rep),
 const combined_fn ARG_UNUSED (popcount))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail682;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail682;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 =  build_zero_cst (TREE_TYPE (captures[0]));
    tree _r;
    _r = fold_build2_loc (loc, rep, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 473, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail682:;
  return NULL_TREE;
}

tree
generic_simplify_282 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp),
 const enum tree_code ARG_UNUSED (ncmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (types_match (type, TREE_TYPE (captures[0]))
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail687;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail687;
      {
	tree res_op0;
	res_op0 = captures[1];
	tree res_op1;
	res_op1 = captures[2];
	tree _r;
	_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 478, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail687:;
    }
  return NULL_TREE;
}

tree
generic_simplify_287 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (copysigns))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail693;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 = captures[2];
    tree _r;
    _r = maybe_build_call_expr_loc (loc, copysigns, type, 2, res_op0, res_op1);
    if (!_r)
      goto next_after_fail693;
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 484, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail693:;
  return NULL_TREE;
}

tree
generic_simplify_291 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail697;
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
next_after_fail697:;
	}
      else
	{
	  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail698;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail698;
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
next_after_fail698:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_308 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (mod))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail717;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail717;
  {
    tree _r;
    _r = captures[0];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 506, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail717:;
  return NULL_TREE;
}

tree
generic_simplify_314 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (SIN),
 const combined_fn ARG_UNUSED (COS),
 const combined_fn ARG_UNUSED (TAN))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail725;
      {
	tree res_op0;
	res_op0 = captures[1];
	tree _r;
	_r = maybe_build_call_expr_loc (loc, TAN, type, 1, res_op0);
	if (!_r)
	  goto next_after_fail725;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 514, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail725:;
    }
  return NULL_TREE;
}

tree
generic_simplify_320 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail731;
	  {
	    tree res_op0;
	    res_op0 = captures[1];
	    tree res_op1;
	    {
	      tree _o1[2], _r1;
	      _o1[0] = captures[2];
	      _o1[1] =  build_one_cst (type);
	      _r1 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      res_op1 = _r1;
	    }
	    tree _r;
	    _r = maybe_build_call_expr_loc (loc, POW, type, 2, res_op0, res_op1);
	    if (!_r)
	      goto next_after_fail731;
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 520, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail731:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_328 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail741;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 = captures[2];
	tree _r;
	_r = fold_build2_loc (loc, op, type, res_op0, res_op1);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 530, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail741:;
    }
  return NULL_TREE;
}

tree
generic_simplify_334 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (out))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[1]))
 && types_match (TREE_TYPE (captures[3]), TREE_TYPE (captures[0]))
 && tree_nop_conversion_p (TREE_TYPE (captures[2]), TREE_TYPE (captures[3]))
 && wi::to_wide (captures[4]) != 0
 && single_use (captures[1])
)
    {
      {
 unsigned int prec = TYPE_PRECISION (TREE_TYPE (captures[3]));
 signop sign = TYPE_SIGN (TREE_TYPE (captures[3]));
	  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail748;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail748;
	  {
	    tree res_op0;
	    res_op0 = captures[3];
	    tree res_op1;
	    res_op1 =  wide_int_to_tree (TREE_TYPE (captures[3]),
 wi::max_value (prec, sign)
 - wi::to_wide (captures[4]));
	    tree _r;
	    _r = fold_build2_loc (loc, out, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 537, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail748:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_343 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (out))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 && types_match (TREE_TYPE (captures[1]), TREE_TYPE (captures[2]))
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail789;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail789;
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
next_after_fail789:;
    }
  return NULL_TREE;
}

tree
generic_simplify_349 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_UNSIGNED (TREE_TYPE (captures[0]))
 && TYPE_PRECISION (TREE_TYPE (captures[0])) > 1
 && (wi::to_wide (captures[2])
 == wi::max_value (TYPE_PRECISION (TREE_TYPE (captures[0])), SIGNED) - 1)
)
    {
      {
 tree stype = signed_type_for (TREE_TYPE (captures[0]));
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail802;
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
	    res_op1 =  build_int_cst (stype, 0);
	    tree _r;
	    _r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (TREE_SIDE_EFFECTS (captures[2]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 574, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail802:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_353 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (out))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TYPE_UNSIGNED (TREE_TYPE (captures[1]))
 && types_match (TREE_TYPE (captures[1]), TREE_TYPE (captures[2]))
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail806;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail806;
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
next_after_fail806:;
    }
  return NULL_TREE;
}

tree
generic_simplify_358 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (shift),
 const enum tree_code ARG_UNUSED (mod))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_pow2p (captures[3]) && tree_int_cst_sgn (captures[3]) > 0
)
    {
      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail815;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail815;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[1];
	  {
	    tree _o2[2], _r2;
	    _o2[0] = captures[2];
	    _o2[1] =  build_int_cst (TREE_TYPE (captures[2]),
 1);
	    _r2 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
	    _o1[1] = _r2;
	  }
	  _r1 = fold_build2_loc (loc, BIT_AND_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op1 = _r1;
	}
	tree _r;
	_r = fold_build2_loc (loc, shift, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 583, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail815:;
    }
  return NULL_TREE;
}

tree
generic_simplify_366 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail826;
	      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail826;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail826;
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
next_after_fail826:;
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_371 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (eqne))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((integer_zerop (captures[1]) || integer_onep (captures[1]))
)
    {
      if ((eqne == EQ_EXPR) ^ integer_zerop (captures[1])
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail835;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree _r;
	    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 601, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail835:;
	}
      else
	{
	  if (types_match (type, TREE_TYPE (captures[0]))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail836;
	      {
		tree res_op0;
		res_op0 = captures[0];
		tree res_op1;
		res_op1 =  build_one_cst (type);
		tree _r;
		_r = fold_build2_loc (loc, BIT_XOR_EXPR, type, res_op0, res_op1);
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 602, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail836:;
	    }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_377 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (type)
 && TYPE_PRECISION (type) == 1
 && !TYPE_OVERFLOW_SANITIZED (TREE_TYPE (captures[0]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail854;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree _r;
	_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 608, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail854:;
    }
  return NULL_TREE;
}

tree
generic_simplify_379 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (coss),
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
 tree t_zero = build_zero_cst (type);
      if (SCALAR_FLOAT_TYPE_P (type)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail856;
	  {
	    if (! tree_invariant_p (captures[1])) goto next_after_fail856;
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
	      _o1[0] =  t_one;
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
		  goto next_after_fail856;
		_o1[1] = _r2;
	      }
	      _r1 = fold_build2_loc (loc, RDIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      res_op1 = _r1;
	    }
	    tree res_op2;
	    {
	      tree _o1[2], _r1;
	      _o1[0] =  t_zero;
	      _o1[1] = captures[1];
	      _r1 = maybe_build_call_expr_loc (loc, copysigns, TREE_TYPE (_o1[0]), 2, _o1[0], _o1[1]);
	      if (!_r1)
	        goto next_after_fail856;
	      res_op2 = _r1;
	    }
	    tree _r;
	    _r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 610, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail856:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_384 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (copysigns),
 const combined_fn ARG_UNUSED (hypots))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail861;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 = captures[2];
    tree _r;
    _r = maybe_build_call_expr_loc (loc, hypots, type, 2, res_op0, res_op1);
    if (!_r)
      goto next_after_fail861;
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 615, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail861:;
  return NULL_TREE;
}

tree
generic_simplify_389 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_PRECISION (TREE_TYPE (captures[0])) == 1
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail866;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 = captures[1];
	tree _r;
	_r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 620, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail866:;
    }
  return NULL_TREE;
}

tree
generic_simplify_393 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 bool wascmp;
      if (bitwise_inverted_equal_p (captures[0], captures[1], wascmp)
 && (!wascmp || element_precision (type) == 1)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail870;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree res_op1;
	    res_op1 = captures[2];
	    tree _r;
	    _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[1]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 624, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail870:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_402 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 bool wascmp;
      if (bitwise_inverted_equal_p (captures[0], captures[2], wascmp)
 && (!wascmp || element_precision (type) == 1)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail879;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree res_op1;
	    res_op1 = captures[1];
	    tree _r;
	    _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
	    if (TREE_SIDE_EFFECTS (captures[2]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 632, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail879:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_406 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail883;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail883;
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
next_after_fail883:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_415 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	      if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail899;
	      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail899;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail899;
	      {
		tree _r;
		_r = captures[0];
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 648, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail899:;
	    }
	  else
	    {
	      if (code1 == EQ_EXPR && !val
)
		{
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail900;
		  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail900;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail900;
		  {
		    tree _r;
		    _r =  constant_boolean_node (false, type);
		    if (TREE_SIDE_EFFECTS (captures[1]))
		      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 649, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail900:;
		}
	      else
		{
		  if (code1 == NE_EXPR && !val && allbits
)
		    {
		      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail901;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail901;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail901;
		      {
			tree _r;
			_r = captures[3];
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 650, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail901:;
		    }
		  else
		    {
		      if (code1 == NE_EXPR
 && code2 == GE_EXPR
 && cmp == 0
 && allbits
)
			{
			  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail902;
			  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail902;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail902;
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
next_after_fail902:;
			}
		      else
			{
			  if (code1 == NE_EXPR
 && code2 == LE_EXPR
 && cmp == 0
 && allbits
)
			    {
			      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail903;
			      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail903;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail903;
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
next_after_fail903:;
			    }
			  else
			    {
			      if (code1 == NE_EXPR
 && code2 == GT_EXPR
 && one_after
 && allbits
)
				{
				  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail904;
				  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail904;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail904;
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
next_after_fail904:;
				}
			      else
				{
				  if (code1 == NE_EXPR
 && code2 == LT_EXPR
 && one_before
 && allbits
)
				    {
				      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail905;
				      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail905;
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail905;
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
next_after_fail905:;
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
generic_simplify_439 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (element_precision (type) <= element_precision (TREE_TYPE (captures[0]))
 || !TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail960;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail960;
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
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 674, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail960:;
    }
  return NULL_TREE;
}

tree
generic_simplify_443 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail964;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[0];
	  _o1[1] = captures[1];
	  _r1 = fold_build2_loc (loc, BIT_XOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree _r;
	_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 678, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail964:;
    }
  return NULL_TREE;
}

tree
generic_simplify_448 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (icmp),
 const enum tree_code ARG_UNUSED (ncmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (VECTOR_TYPE_P (type)
 || (INTEGRAL_TYPE_P (type) && TYPE_PRECISION (type) == 1)
)
    {
      {
 enum tree_code ic = invert_tree_comparison
 (cmp, HONOR_NANS (captures[0]));
	  if (ic == icmp
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail969;
	      {
		tree res_op0;
		res_op0 = captures[0];
		tree res_op1;
		res_op1 = captures[1];
		tree _r;
		_r = fold_build2_loc (loc, icmp, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 681, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail969:;
	    }
	  else
	    {
	      if (ic == ncmp
)
		{
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail970;
		  {
		    tree res_op0;
		    res_op0 = captures[0];
		    tree res_op1;
		    res_op1 = captures[1];
		    tree _r;
		    _r = fold_build2_loc (loc, ncmp, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 682, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail970:;
		}
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_459 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_CODE (type) != COMPLEX_TYPE
 && (! ANY_INTEGRAL_TYPE_P (type)
 || TYPE_OVERFLOW_UNDEFINED (type))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail982;
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
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 693, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail982:;
    }
  return NULL_TREE;
}

tree
generic_simplify_467 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_CODE (type) != COMPLEX_TYPE
 && (! ANY_INTEGRAL_TYPE_P (type)
 || TYPE_OVERFLOW_UNDEFINED (type))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1002;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree _r;
	_r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 701, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1002:;
    }
  return NULL_TREE;
}

tree
generic_simplify_472 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree from_type = TREE_TYPE (captures[0]);
 tree c1_type = TREE_TYPE (captures[1]), c2_type = TREE_TYPE (captures[2]);
 enum tree_code code = ERROR_MARK;
 if (INTEGRAL_TYPE_P (from_type)
 && int_fits_type_p (captures[2], from_type)
 && (types_match (c1_type, from_type)
 || (TYPE_PRECISION (c1_type) > TYPE_PRECISION (from_type)
 && (TYPE_UNSIGNED (from_type)
 || TYPE_SIGN (c1_type) == TYPE_SIGN (from_type))))
 && (types_match (c2_type, from_type)
 || (TYPE_PRECISION (c2_type) > TYPE_PRECISION (from_type)
 && (TYPE_UNSIGNED (from_type)
 || TYPE_SIGN (c2_type) == TYPE_SIGN (from_type)))))
 {
 if (cmp != EQ_EXPR)
 code = minmax_from_comparison (cmp, captures[0], captures[1], captures[0], captures[2]);
 else if (int_fits_type_p (captures[1], from_type))
 code = EQ_EXPR;
 }
      if (code == MAX_EXPR
)
	{
	  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1007;
	  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1007;
	  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1007;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1007;
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
	      _r1 = fold_build2_loc (loc, MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 706, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1007:;
	}
      else
	{
	  if (code == MIN_EXPR
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1008;
	      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1008;
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1008;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1008;
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
		  _r1 = fold_build2_loc (loc, MIN_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  res_op0 = _r1;
		}
		tree _r;
		_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 707, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1008:;
	    }
	  else
	    {
	      if (code == EQ_EXPR
)
		{
		  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1009;
		  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1009;
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1009;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1009;
		  {
		    if (! tree_invariant_p (captures[1])) goto next_after_fail1009;
		    tree res_op0;
		    {
		      tree _o1[3], _r1;
		      {
			tree _o2[2], _r2;
			_o2[0] = captures[0];
			{
			  tree _o3[1], _r3;
			  _o3[0] = unshare_expr (captures[1]);
			  if (TREE_TYPE (_o3[0]) != TREE_TYPE (_o2[0]))
			    {
			      _r3 = fold_build1_loc (loc, NOP_EXPR, TREE_TYPE (_o2[0]), _o3[0]);
			    }
			  else
			    _r3 = _o3[0];
			  _o2[1] = _r3;
			}
			_r2 = fold_build2_loc (loc, EQ_EXPR, boolean_type_node, _o2[0], _o2[1]);
			_o1[0] = _r2;
		      }
		      {
			tree _o2[1], _r2;
			_o2[0] = captures[1];
			if (TREE_TYPE (_o2[0]) != from_type)
			  {
			    _r2 = fold_build1_loc (loc, NOP_EXPR, from_type, _o2[0]);
			  }
			else
			  _r2 = _o2[0];
			_o1[1] = _r2;
		      }
		      {
			tree _o2[1], _r2;
			_o2[0] = captures[2];
			if (TREE_TYPE (_o2[0]) != from_type)
			  {
			    _r2 = fold_build1_loc (loc, NOP_EXPR, from_type, _o2[0]);
			  }
			else
			  _r2 = _o2[0];
			_o1[2] = _r2;
		      }
		      _r1 = fold_build3_loc (loc, COND_EXPR, TREE_TYPE (_o1[1]), _o1[0], _o1[1], _o1[2]);
		      res_op0 = _r1;
		    }
		    tree _r;
		    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 708, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1009:;
		}
	    }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_508 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1053;
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1053;
  if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1053;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1053;
  {
    tree _r;
    _r = captures[0];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 732, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1053:;
  return NULL_TREE;
}

tree
generic_simplify_514 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (cond_len_op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree op_type = TREE_TYPE (captures[5]);
      if (inverse_conditions_p (captures[0], captures[2])
 && element_precision (type) == element_precision (op_type)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1059;
	  {
	    tree res_op0;
	    {
	      tree _o1[6], _r1;
	      _o1[0] = captures[2];
	      _o1[1] = captures[3];
	      _o1[2] = captures[4];
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[1];
		_r2 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, op_type, _o2[0]);
		_o1[3] = _r2;
	      }
	      _o1[4] = captures[6];
	      _o1[5] = captures[7];
	      _r1 = maybe_build_call_expr_loc (loc, cond_len_op, TREE_TYPE (_o1[1]), 6, _o1[0], _o1[1], _o1[2], _o1[3], _o1[4], _o1[5]);
	      if (!_r1)
	        goto next_after_fail1059;
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (TREE_SIDE_EFFECTS (captures[5]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[5]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 752, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1059:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_522 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (ovf))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_PRECISION (TREE_TYPE (captures[0])) > TYPE_PRECISION (TREE_TYPE (captures[1]))
 && (!TYPE_UNSIGNED (TREE_TYPE (captures[0])) || TYPE_UNSIGNED (TREE_TYPE (captures[1])))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1067;
      {
	tree res_op0;
	res_op0 = captures[1];
	tree res_op1;
	res_op1 = captures[2];
	tree _r;
	_r = maybe_build_call_expr_loc (loc, ovf, type, 2, res_op0, res_op1);
	if (!_r)
	  goto next_after_fail1067;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 760, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1067:;
    }
  return NULL_TREE;
}

tree
generic_simplify_532 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (froms),
 const combined_fn ARG_UNUSED (tos))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (optimize && canonicalize_math_p ()
 && targetm.libc_has_function (function_c99_misc, NULL_TREE)
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1077;
      {
	tree res_op0;
	{
	  tree _o1[1], _r1;
	  _o1[0] = captures[0];
	  _r1 = maybe_build_call_expr_loc (loc, tos, TREE_TYPE (_o1[0]), 1, _o1[0]);
	  if (!_r1)
	    goto next_after_fail1077;
	  res_op0 = _r1;
	}
	tree _r;
	_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 770, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1077:;
    }
  return NULL_TREE;
}

tree
generic_simplify_540 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (fns))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (canonicalize_math_p ()
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1085;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree _r;
	_r = fold_build1_loc (loc, FIX_TRUNC_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 778, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1085:;
    }
  return NULL_TREE;
}

tree
generic_simplify_548 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (fmas))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (canonicalize_math_after_vectorization_p ()
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1093;
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
	  goto next_after_fail1093;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 786, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1093:;
    }
  return NULL_TREE;
}

tree
generic_simplify_557 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (ctz))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree t = TREE_TYPE (captures[0]);
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1102;
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
	  goto next_after_fail1102;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 793, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1102:;
  }
  return NULL_TREE;
}

tree
generic_simplify_566 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1112;
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
	      goto next_after_fail1112;
	    if (TREE_SIDE_EFFECTS (captures[3]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 800, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1112:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_CONVERT_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
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
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q30 };
		if (INTEGRAL_TYPE_P (type)
 && tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
 && TYPE_PRECISION (type) > TYPE_PRECISION (TREE_TYPE (captures[2]))
 && TREE_CODE (type) != BOOLEAN_TYPE
)
		  {
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1124;
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
		      tree _r;
		      _r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 810, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1124:;
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
	{
	  tree _q20_pops[1];
	  if (tree_nop_convert (_q20, _q20_pops))
	    {
	      tree _q30 = _q20_pops[0];
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21 };
		tree res = generic_simplify_370 (loc, type, _p0, captures, BIT_AND_EXPR);
		if (res) return res;
	      }
	    }
	}
	{
	  tree _q21_pops[1];
	  if (tree_nop_convert (_q21, _q21_pops))
	    {
	      tree _q40 = _q21_pops[0];
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q40, _q20 };
		tree res = generic_simplify_370 (loc, type, _p0, captures, BIT_AND_EXPR);
		if (res) return res;
	      }
	    }
	}
        break;
      }
    case BIT_IOR_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree _q20_pops[1];
	  if (tree_nop_convert (_q20, _q20_pops))
	    {
	      tree _q30 = _q20_pops[0];
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21 };
		tree res = generic_simplify_370 (loc, type, _p0, captures, BIT_IOR_EXPR);
		if (res) return res;
	      }
	    }
	}
	{
	  tree _q21_pops[1];
	  if (tree_nop_convert (_q21, _q21_pops))
	    {
	      tree _q40 = _q21_pops[0];
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q40, _q20 };
		tree res = generic_simplify_370 (loc, type, _p0, captures, BIT_IOR_EXPR);
		if (res) return res;
	      }
	    }
	}
        break;
      }
    case BIT_XOR_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree _q20_pops[1];
	  if (tree_nop_convert (_q20, _q20_pops))
	    {
	      tree _q30 = _q20_pops[0];
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21 };
		tree res = generic_simplify_370 (loc, type, _p0, captures, BIT_XOR_EXPR);
		if (res) return res;
	      }
	    }
	}
	{
	  tree _q21_pops[1];
	  if (tree_nop_convert (_q21, _q21_pops))
	    {
	      tree _q40 = _q21_pops[0];
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q40, _q20 };
		tree res = generic_simplify_370 (loc, type, _p0, captures, BIT_XOR_EXPR);
		if (res) return res;
	      }
	    }
	}
        break;
      }
    case MULT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (tree_zero_one_valued_p (_q20))
	  {
	    switch (TREE_CODE (_q21))
	      {
	      case INTEGER_CST:
	        {
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		    if (INTEGRAL_TYPE_P (type)
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_PRECISION (type) < TYPE_PRECISION (TREE_TYPE (captures[0]))
 && (TYPE_UNSIGNED (type) || TYPE_PRECISION (type) > 1)
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1125;
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
			  _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 811, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1125:;
		      }
		  }
	          break;
	        }
	      default:;
	      }
	  }
        break;
      }
    case POINTER_DIFF_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		if (POINTER_TYPE_P (type)
)
		  {
		    if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1126;
		    if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1126;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1126;
		    {
		      tree _r;
		      _r =  build_fold_addr_expr_with_type
 (build2 (MEM_REF, char_type_node, captures[0],
 wide_int_to_tree (ptr_type_node, wi::neg (wi::to_wide (captures[1])))),
 type);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 812, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1126:;
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
	if (tree_zero_one_valued_p (_q20))
	  {
	    switch (TREE_CODE (_q21))
	      {
	      case INTEGER_CST:
	        {
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		    tree res = generic_simplify_371 (loc, type, _p0, captures, EQ_EXPR);
		    if (res) return res;
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
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (tree_zero_one_valued_p (_q20))
	  {
	    switch (TREE_CODE (_q21))
	      {
	      case INTEGER_CST:
	        {
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		    tree res = generic_simplify_371 (loc, type, _p0, captures, NE_EXPR);
		    if (res) return res;
		  }
	          break;
	        }
	      default:;
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
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_q21))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q21 };
		      tree res = generic_simplify_372 (loc, type, _p0, captures, MIN_EXPR);
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
	switch (TREE_CODE (_q20))
	  {
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_q21))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q21 };
		      tree res = generic_simplify_372 (loc, type, _p0, captures, MAX_EXPR);
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
    case LSHIFT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		if (INTEGRAL_TYPE_P (type)
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && !integer_zerop (captures[2])
 && TYPE_PRECISION (type) <= TYPE_PRECISION (TREE_TYPE (captures[0]))
)
		  {
		    if (TYPE_PRECISION (type) == TYPE_PRECISION (TREE_TYPE (captures[0]))
 || wi::ltu_p (wi::to_wide (captures[2]), TYPE_PRECISION (type))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1127;
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
			  res_op1 = captures[2];
			  tree _r;
			  _r = fold_build2_loc (loc, LSHIFT_EXPR, type, res_op0, res_op1);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 813, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1127:;
		      }
		    else
		      {
			if (wi::ltu_p (wi::to_wide (captures[2]), TYPE_PRECISION (TREE_TYPE (captures[0])))
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1128;
			    {
			      tree _r;
			      _r =  build_zero_cst (type);
			      if (TREE_SIDE_EFFECTS (captures[1]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			      if (TREE_SIDE_EFFECTS (captures[2]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 814, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1128:;
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
    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
    if ((
0
 && useless_type_conversion_p (type, TREE_TYPE (captures[0])))
 || (
1
 && type == TREE_TYPE (captures[0]))
)
      {
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1129;
	{
	  tree _r;
	  _r = captures[0];
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 815, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1129:;
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
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1130;
		  {
		    tree res_op0;
		    res_op0 = captures[1];
		    tree _r;
		    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 816, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1130:;
		}
	      else
		{
		  if (((inter_int && inside_int) || (inter_float && inside_float))
 && (final_int || final_float)
 && inter_prec >= inside_prec
 && (inter_float || inter_unsignedp == inside_unsignedp)
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1131;
		      {
			tree res_op0;
			res_op0 = captures[1];
			tree _r;
			_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 817, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1131:;
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1132;
			  {
			    tree res_op0;
			    res_op0 = captures[1];
			    tree _r;
			    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 818, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1132:;
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
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1133;
			      {
				tree res_op0;
				res_op0 = captures[1];
				tree _r;
				_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 819, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail1133:;
			    }
			  else
			    {
			      if (inside_int && inter_int && final_int
 && final_prec <= inside_prec
 && inter_prec >= inside_prec
)
				{
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1134;
				  {
				    tree res_op0;
				    res_op0 = captures[1];
				    tree _r;
				    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 820, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail1134:;
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
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1135;
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
next_after_fail1135:;
				    }
				  else
				    {
				      if (0
 && inside_int && inter_float && final_int &&
 (unsigned) significand_size (TYPE_MODE (inter_type))
 >= inside_prec - !inside_unsignedp
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1136;
					  {
					    tree res_op0;
					    res_op0 = captures[1];
					    tree _r;
					    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 822, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1136:;
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
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1137;
		  {
		    tree res_op0;
		    res_op0 = captures[1];
		    tree _r;
		    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 816, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1137:;
		}
	      else
		{
		  if (((inter_int && inside_int) || (inter_float && inside_float))
 && (final_int || final_float)
 && inter_prec >= inside_prec
 && (inter_float || inter_unsignedp == inside_unsignedp)
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1138;
		      {
			tree res_op0;
			res_op0 = captures[1];
			tree _r;
			_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 817, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1138:;
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1139;
			  {
			    tree res_op0;
			    res_op0 = captures[1];
			    tree _r;
			    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 818, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1139:;
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
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1140;
			      {
				tree res_op0;
				res_op0 = captures[1];
				tree _r;
				_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 819, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail1140:;
			    }
			  else
			    {
			      if (inside_int && inter_int && final_int
 && final_prec <= inside_prec
 && inter_prec >= inside_prec
)
				{
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1141;
				  {
				    tree res_op0;
				    res_op0 = captures[1];
				    tree _r;
				    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 820, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail1141:;
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
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1142;
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
next_after_fail1142:;
				    }
				  else
				    {
				      if (0
 && inside_int && inter_float && final_int &&
 (unsigned) significand_size (TYPE_MODE (inter_type))
 >= inside_prec - !inside_unsignedp
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1143;
					  {
					    tree res_op0;
					    res_op0 = captures[1];
					    tree _r;
					    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 822, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1143:;
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
    case BIT_AND_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		if (INTEGRAL_TYPE_P (type)
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_PRECISION (type) <= TYPE_PRECISION (TREE_TYPE (captures[0]))
 && operand_equal_p (captures[1], build_low_bits_mask (TREE_TYPE (captures[1]),
 TYPE_PRECISION (type)), 0)
)
		  {
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1144;
		    {
		      tree res_op0;
		      res_op0 = captures[0];
		      tree _r;
		      _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
		      if (TREE_SIDE_EFFECTS (captures[1]))
			_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 823, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1144:;
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
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q22 };
	  if (VECTOR_TYPE_P (type)
 && types_match (TREE_TYPE (captures[1]), truth_type_for (type))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1145;
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
		      if (EXPR_P (_r1))
		        goto next_after_fail1145;
		    }
		  else
		    _r1 = _o1[0];
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[1], _r1;
		  _o1[0] = captures[3];
		  if (TREE_TYPE (_o1[0]) != type)
		    {
		      _r1 = fold_build1_loc (loc, NOP_EXPR, type, _o1[0]);
		      if (EXPR_P (_r1))
		        goto next_after_fail1145;
		    }
		  else
		    _r1 = _o1[0];
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 824, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1145:;
	    }
	}
        break;
      }
    case COND_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	tree _q22 = TREE_OPERAND (_p0, 2);
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      switch (TREE_CODE (_q22))
	        {
		case INTEGER_CST:
		  {
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q22 };
		      if (INTEGRAL_TYPE_P (type)
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
)
			{
			  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1146;
			  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1146;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1146;
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
			    tree res_op2;
			    {
			      tree _o1[1], _r1;
			      _o1[0] = captures[3];
			      if (TREE_TYPE (_o1[0]) != type)
				{
				  _r1 = fold_build1_loc (loc, NOP_EXPR, type, _o1[0]);
				}
			      else
			        _r1 = _o1[0];
			      res_op2 = _r1;
			    }
			    tree _r;
			    _r = fold_build3_loc (loc, COND_EXPR, type, res_op0, res_op1, res_op2);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 825, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1146:;
			}
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
		    {
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50 };
		      tree res = generic_simplify_373 (loc, type, _p0, captures, PLUS_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      {
		tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q21 };
		tree res = generic_simplify_374 (loc, type, _p0, captures, PLUS_EXPR);
		if (res) return res;
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
	      {
		tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q20, _q21, _q40 };
		tree res = generic_simplify_375 (loc, type, _p0, captures, PLUS_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	{
	  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q20, _q21, _q21 };
	  tree res = generic_simplify_376 (loc, type, _p0, captures, PLUS_EXPR);
	  if (res) return res;
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
		    {
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50 };
		      tree res = generic_simplify_373 (loc, type, _p0, captures, MINUS_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      {
		tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q21 };
		tree res = generic_simplify_374 (loc, type, _p0, captures, MINUS_EXPR);
		if (res) return res;
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
	      {
		tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q20, _q21, _q40 };
		tree res = generic_simplify_375 (loc, type, _p0, captures, MINUS_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	{
	  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q20, _q21, _q21 };
	  tree res = generic_simplify_376 (loc, type, _p0, captures, MINUS_EXPR);
	  if (res) return res;
	}
        break;
      }
    case MULT_EXPR:
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
		    {
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50 };
		      tree res = generic_simplify_373 (loc, type, _p0, captures, MULT_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      {
		tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q21 };
		tree res = generic_simplify_374 (loc, type, _p0, captures, MULT_EXPR);
		if (res) return res;
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
	      {
		tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q20, _q21, _q40 };
		tree res = generic_simplify_375 (loc, type, _p0, captures, MULT_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	{
	  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q20, _q21, _q21 };
	  tree res = generic_simplify_376 (loc, type, _p0, captures, MULT_EXPR);
	  if (res) return res;
	}
        break;
      }
    case RDIV_EXPR:
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
		    {
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q50 };
		      tree res = generic_simplify_373 (loc, type, _p0, captures, RDIV_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      {
		tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q21, _q21 };
		tree res = generic_simplify_374 (loc, type, _p0, captures, RDIV_EXPR);
		if (res) return res;
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
	      {
		tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q20, _q21, _q40 };
		tree res = generic_simplify_375 (loc, type, _p0, captures, RDIV_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	{
	  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q20, _q21, _q21 };
	  tree res = generic_simplify_376 (loc, type, _p0, captures, RDIV_EXPR);
	  if (res) return res;
	}
        break;
      }
    case NEGATE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
	  tree res = generic_simplify_377 (loc, type, _p0, captures);
	  if (res) return res;
	}
        break;
      }
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_CEIL_DIV_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_172 (loc, type, _p0, _p1, captures, CEIL_DIV_EXPR);
	if (res) return res;
      }
    }
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	tree res = generic_simplify_293 (loc, type, _p0, _p1, captures, CEIL_DIV_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  tree res = generic_simplify_295 (loc, type, _p0, _p1, captures, CEIL_DIV_EXPR);
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
      tree res = generic_simplify_294 (loc, type, _p0, _p1, captures, CEIL_DIV_EXPR);
      if (res) return res;
    }
  }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_296 (loc, type, _p0, _p1, captures, CEIL_DIV_EXPR);
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
	      tree res = generic_simplify_297 (loc, type, _p0, _p1, captures, CEIL_DIV_EXPR);
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
	      tree res = generic_simplify_297 (loc, type, _p0, _p1, captures, CEIL_DIV_EXPR);
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
	      tree res = generic_simplify_298 (loc, type, _p0, _p1, captures, CEIL_DIV_EXPR);
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
	      tree res = generic_simplify_298 (loc, type, _p0, _p1, captures, CEIL_DIV_EXPR);
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
			    tree res = generic_simplify_300 (loc, type, _p0, _p1, captures, CEIL_DIV_EXPR);
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
		      tree res = generic_simplify_300 (loc, type, _p0, _p1, captures, CEIL_DIV_EXPR);
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
	      tree res = generic_simplify_301 (loc, type, _p0, _p1, captures, CEIL_DIV_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_301 (loc, type, _p0, _p1, captures, CEIL_DIV_EXPR);
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
				tree res = generic_simplify_302 (loc, type, _p0, _p1, captures, CEIL_DIV_EXPR);
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
 && (TREE_CODE_CLASS (CEIL_DIV_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1624;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, CEIL_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1624;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, CEIL_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1624;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1624:;
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
 && (TREE_CODE_CLASS (CEIL_DIV_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1625;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1625;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, CEIL_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1625;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, CEIL_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1625;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1625:;
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
 && (TREE_CODE_CLASS (CEIL_DIV_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1626;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1626;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, CEIL_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1626;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, CEIL_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1626;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1626:;
	    }
	}
        break;
      }
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_EXACT_DIV_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_172 (loc, type, _p0, _p1, captures, EXACT_DIV_EXPR);
	if (res) return res;
      }
    }
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	tree res = generic_simplify_293 (loc, type, _p0, _p1, captures, EXACT_DIV_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  tree res = generic_simplify_295 (loc, type, _p0, _p1, captures, EXACT_DIV_EXPR);
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
      tree res = generic_simplify_294 (loc, type, _p0, _p1, captures, EXACT_DIV_EXPR);
      if (res) return res;
    }
  }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_296 (loc, type, _p0, _p1, captures, EXACT_DIV_EXPR);
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
	      tree res = generic_simplify_297 (loc, type, _p0, _p1, captures, EXACT_DIV_EXPR);
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
	      tree res = generic_simplify_297 (loc, type, _p0, _p1, captures, EXACT_DIV_EXPR);
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
	      tree res = generic_simplify_298 (loc, type, _p0, _p1, captures, EXACT_DIV_EXPR);
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
	      tree res = generic_simplify_298 (loc, type, _p0, _p1, captures, EXACT_DIV_EXPR);
	      if (res) return res;
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
		case INTEGER_CST:
		  {
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
		      tree res = generic_simplify_299 (loc, type, _p0, _p1, captures, EXACT_DIV_EXPR);
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
			    tree res = generic_simplify_300 (loc, type, _p0, _p1, captures, EXACT_DIV_EXPR);
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
		      tree res = generic_simplify_300 (loc, type, _p0, _p1, captures, EXACT_DIV_EXPR);
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
	      tree res = generic_simplify_301 (loc, type, _p0, _p1, captures, EXACT_DIV_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_301 (loc, type, _p0, _p1, captures, EXACT_DIV_EXPR);
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
				tree res = generic_simplify_302 (loc, type, _p0, _p1, captures, EXACT_DIV_EXPR);
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
  return NULL_TREE;
}

tree
generic_simplify_RDIV_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (FLOAT_TYPE_P (type)
 && ! HONOR_NANS (type)
 && ! HONOR_INFINITIES (type)
)
	  {
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1636;
	    {
	      tree _r;
	      _r =  build_one_cst (type);
	      if (TREE_SIDE_EFFECTS (captures[0]))
		_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 963, __FILE__, __LINE__, true);
	      return _r;
	    }
next_after_fail1636:;
	  }
      }
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
	      tree res = generic_simplify_311 (loc, type, _p0, _p1, captures);
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
	      tree res = generic_simplify_311 (loc, type, _p0, _p1, captures);
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
		case ABS_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		      {
			{
			  tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
			  tree res = generic_simplify_312 (loc, type, _p0, _p1, captures);
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
	  case ABS_EXPR:
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
			  tree res = generic_simplify_312 (loc, type, _p0, _p1, captures);
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
    case ABS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	      tree res = generic_simplify_312 (loc, type, _p0, _p1, captures);
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
	      tree res = generic_simplify_312 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  if (real_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (!tree_expr_maybe_signaling_nan_p (captures[0])
)
	  {
	    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1637;
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1637;
	    {
	      tree res_op0;
	      res_op0 = captures[0];
	      tree _r;
	      _r = non_lvalue_loc (loc, res_op0);
	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 964, __FILE__, __LINE__, true);
	      return _r;
	    }
next_after_fail1637:;
	  }
      }
    }
  if (real_minus_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (!tree_expr_maybe_signaling_nan_p (captures[0])
)
	  {
	    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1638;
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1638;
	    {
	      tree res_op0;
	      res_op0 = captures[0];
	      tree _r;
	      _r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 965, __FILE__, __LINE__, true);
	      return _r;
	    }
next_after_fail1638:;
	  }
      }
    }
  switch (TREE_CODE (_p0))
    {
    case RDIV_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1 };
	  if (flag_reciprocal_math
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1639;
	      {
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  res_op1 = _r1;
		}
		tree _r;
		_r = fold_build2_loc (loc, RDIV_EXPR, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 966, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1639:;
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
	switch (TREE_CODE (_q31))
	  {
	  case REAL_CST:
	    {
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31 };
		if (flag_reciprocal_math
)
		  {
		    {
 tree tem = const_binop (RDIV_EXPR, type, build_one_cst (type), captures[3]);
			if (tem
)
			  {
			    if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1640;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1640;
			    {
			      tree res_op0;
			      {
				tree _o1[2], _r1;
				_o1[0] = captures[0];
				_o1[1] =  tem;
				_r1 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				res_op0 = _r1;
			      }
			      tree res_op1;
			      res_op1 = captures[2];
			      tree _r;
			      _r = fold_build2_loc (loc, RDIV_EXPR, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 967, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1640:;
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
    case RDIV_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31 };
	  if (flag_reciprocal_math
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1641;
	      {
		tree res_op0;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[2];
		  _r1 = fold_build2_loc (loc, RDIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  res_op0 = _r1;
		}
		tree res_op1;
		res_op1 = captures[3];
		tree _r;
		_r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 968, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1641:;
	    }
	}
        break;
      }
    case NEGATE_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1642;
	  {
	    tree res_op0;
	    {
	      tree _o1[1], _r1;
	      _o1[0] = captures[0];
	      _r1 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o1[0]), _o1[0]);
	      res_op0 = _r1;
	    }
	    tree res_op1;
	    res_op1 = captures[1];
	    tree _r;
	    _r = fold_build2_loc (loc, RDIV_EXPR, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 969, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1642:;
	}
        break;
      }
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (optimize
)
	    {
	      if (flag_reciprocal_math
 && !real_zerop (captures[1])
)
		{
		  {
 tree tem = const_binop (RDIV_EXPR, type, build_one_cst (type), captures[1]);
		      if (tem
)
			{
			  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1643;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1643;
			  {
			    tree res_op0;
			    res_op0 = captures[0];
			    tree res_op1;
			    res_op1 =  tem;
			    tree _r;
			    _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 970, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1643:;
			}
		  }
		}
	      else
		{
		  if (REAL_CST != COMPLEX_CST
)
		    {
		      {
 tree inverse = exact_inverse (type, captures[1]);
			  if (inverse
)
			    {
			      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1644;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1644;
			      {
				tree res_op0;
				res_op0 = captures[0];
				tree res_op1;
				res_op1 =  inverse;
				tree _r;
				_r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 971, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail1644:;
			    }
		      }
		    }
		}
	    }
	}
        break;
      }
    case COMPLEX_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (optimize
)
	    {
	      if (flag_reciprocal_math
 && !real_zerop (captures[1])
)
		{
		  {
 tree tem = const_binop (RDIV_EXPR, type, build_one_cst (type), captures[1]);
		      if (tem
)
			{
			  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1645;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1645;
			  {
			    tree res_op0;
			    res_op0 = captures[0];
			    tree res_op1;
			    res_op1 =  tem;
			    tree _r;
			    _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 970, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1645:;
			}
		  }
		}
	      else
		{
		  if (COMPLEX_CST != COMPLEX_CST
)
		    {
		      {
 tree inverse = exact_inverse (type, captures[1]);
			  if (inverse
)
			    {
			      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1646;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1646;
			      {
				tree res_op0;
				res_op0 = captures[0];
				tree res_op1;
				res_op1 =  inverse;
				tree _r;
				_r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 971, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail1646:;
			    }
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
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (optimize
)
	    {
	      if (flag_reciprocal_math
 && !real_zerop (captures[1])
)
		{
		  {
 tree tem = const_binop (RDIV_EXPR, type, build_one_cst (type), captures[1]);
		      if (tem
)
			{
			  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1647;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1647;
			  {
			    tree res_op0;
			    res_op0 = captures[0];
			    tree res_op1;
			    res_op1 =  tem;
			    tree _r;
			    _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 970, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1647:;
			}
		  }
		}
	      else
		{
		  if (VECTOR_CST != COMPLEX_CST
)
		    {
		      {
 tree inverse = exact_inverse (type, captures[1]);
			  if (inverse
)
			    {
			      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1648;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1648;
			      {
				tree res_op0;
				res_op0 = captures[0];
				tree res_op1;
				res_op1 =  inverse;
				tree _r;
				_r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 971, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail1648:;
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
 && (TREE_CODE_CLASS (RDIV_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1649;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, RDIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1649;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, RDIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1649;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1649:;
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
 && (TREE_CODE_CLASS (RDIV_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1650;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1650;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, RDIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1650;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, RDIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1650;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1650:;
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
 && (TREE_CODE_CLASS (RDIV_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1651;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1651;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, RDIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1651;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, RDIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1651;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1651:;
	    }
	}
        break;
      }
    case CALL_EXPR:
      switch (get_call_combined_fn (_p1))
        {
	case CFN_BUILT_IN_SQRTF:
	  if (call_expr_nargs (_p1) == 1)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
		    tree res = generic_simplify_313 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTF);
		    if (res) return res;
		  }
	        }
	      switch (TREE_CODE (_q30))
	        {
		case RDIV_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q40, _q41 };
		      if (flag_unsafe_math_optimizations
)
			{
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1652;
			  {
			    tree res_op0;
			    res_op0 = captures[0];
			    tree res_op1;
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[2], _r2;
				_o2[0] = captures[4];
				_o2[1] = captures[3];
				_r2 = fold_build2_loc (loc, RDIV_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				_o1[0] = _r2;
			      }
			      _r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_SQRTF, TREE_TYPE (_o1[0]), 1, _o1[0]);
			      if (!_r1)
			        goto next_after_fail1652;
			      res_op1 = _r1;
			    }
			    tree _r;
			    _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 972, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1652:;
			}
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_SQRTL:
	  if (call_expr_nargs (_p1) == 1)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
		    tree res = generic_simplify_313 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTL);
		    if (res) return res;
		  }
	        }
	      switch (TREE_CODE (_q30))
	        {
		case RDIV_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q40, _q41 };
		      if (flag_unsafe_math_optimizations
)
			{
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1653;
			  {
			    tree res_op0;
			    res_op0 = captures[0];
			    tree res_op1;
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[2], _r2;
				_o2[0] = captures[4];
				_o2[1] = captures[3];
				_r2 = fold_build2_loc (loc, RDIV_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				_o1[0] = _r2;
			      }
			      _r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_SQRTL, TREE_TYPE (_o1[0]), 1, _o1[0]);
			      if (!_r1)
			        goto next_after_fail1653;
			      res_op1 = _r1;
			    }
			    tree _r;
			    _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 972, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1653:;
			}
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_EXP:
	  if (call_expr_nargs (_p1) == 1)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		if (flag_unsafe_math_optimizations
)
		  {
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1654;
		    {
		      tree res_op0;
		      res_op0 = captures[0];
		      tree res_op1;
		      {
			tree _o1[1], _r1;
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[2];
			  _r2 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
			  _o1[0] = _r2;
			}
			_r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP, TREE_TYPE (_o1[0]), 1, _o1[0]);
			if (!_r1)
			  goto next_after_fail1654;
			res_op1 = _r1;
		      }
		      tree _r;
		      _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1654:;
		  }
	      }
	    }
	  break;
	case CFN_BUILT_IN_EXP10F:
	  if (call_expr_nargs (_p1) == 1)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		if (flag_unsafe_math_optimizations
)
		  {
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1655;
		    {
		      tree res_op0;
		      res_op0 = captures[0];
		      tree res_op1;
		      {
			tree _o1[1], _r1;
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[2];
			  _r2 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
			  _o1[0] = _r2;
			}
			_r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP10F, TREE_TYPE (_o1[0]), 1, _o1[0]);
			if (!_r1)
			  goto next_after_fail1655;
			res_op1 = _r1;
		      }
		      tree _r;
		      _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1655:;
		  }
	      }
	    }
	  break;
	case CFN_BUILT_IN_EXP10L:
	  if (call_expr_nargs (_p1) == 1)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		if (flag_unsafe_math_optimizations
)
		  {
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1656;
		    {
		      tree res_op0;
		      res_op0 = captures[0];
		      tree res_op1;
		      {
			tree _o1[1], _r1;
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[2];
			  _r2 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
			  _o1[0] = _r2;
			}
			_r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP10L, TREE_TYPE (_o1[0]), 1, _o1[0]);
			if (!_r1)
			  goto next_after_fail1656;
			res_op1 = _r1;
		      }
		      tree _r;
		      _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1656:;
		  }
	      }
	    }
	  break;
	case CFN_BUILT_IN_CBRT:
	  if (call_expr_nargs (_p1) == 1)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      switch (TREE_CODE (_q30))
	        {
		case RDIV_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q40, _q41 };
		      if (flag_unsafe_math_optimizations
)
			{
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1657;
			  {
			    tree res_op0;
			    res_op0 = captures[0];
			    tree res_op1;
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[2], _r2;
				_o2[0] = captures[4];
				_o2[1] = captures[3];
				_r2 = fold_build2_loc (loc, RDIV_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				_o1[0] = _r2;
			      }
			      _r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_CBRT, TREE_TYPE (_o1[0]), 1, _o1[0]);
			      if (!_r1)
			        goto next_after_fail1657;
			      res_op1 = _r1;
			    }
			    tree _r;
			    _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 972, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1657:;
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
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		if (flag_unsafe_math_optimizations
)
		  {
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1658;
		    {
		      tree res_op0;
		      res_op0 = captures[0];
		      tree res_op1;
		      {
			tree _o1[1], _r1;
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[2];
			  _r2 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
			  _o1[0] = _r2;
			}
			_r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP2, TREE_TYPE (_o1[0]), 1, _o1[0]);
			if (!_r1)
			  goto next_after_fail1658;
			res_op1 = _r1;
		      }
		      tree _r;
		      _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1658:;
		  }
	      }
	    }
	  break;
	case CFN_BUILT_IN_EXPF:
	  if (call_expr_nargs (_p1) == 1)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		if (flag_unsafe_math_optimizations
)
		  {
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1659;
		    {
		      tree res_op0;
		      res_op0 = captures[0];
		      tree res_op1;
		      {
			tree _o1[1], _r1;
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[2];
			  _r2 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
			  _o1[0] = _r2;
			}
			_r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXPF, TREE_TYPE (_o1[0]), 1, _o1[0]);
			if (!_r1)
			  goto next_after_fail1659;
			res_op1 = _r1;
		      }
		      tree _r;
		      _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1659:;
		  }
	      }
	    }
	  break;
	case CFN_BUILT_IN_EXPL:
	  if (call_expr_nargs (_p1) == 1)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		if (flag_unsafe_math_optimizations
)
		  {
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1660;
		    {
		      tree res_op0;
		      res_op0 = captures[0];
		      tree res_op1;
		      {
			tree _o1[1], _r1;
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[2];
			  _r2 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
			  _o1[0] = _r2;
			}
			_r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXPL, TREE_TYPE (_o1[0]), 1, _o1[0]);
			if (!_r1)
			  goto next_after_fail1660;
			res_op1 = _r1;
		      }
		      tree _r;
		      _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1660:;
		  }
	      }
	    }
	  break;
	case CFN_BUILT_IN_SQRT:
	  if (call_expr_nargs (_p1) == 1)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
		    tree res = generic_simplify_313 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRT);
		    if (res) return res;
		  }
	        }
	      switch (TREE_CODE (_q30))
	        {
		case RDIV_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q40, _q41 };
		      if (flag_unsafe_math_optimizations
)
			{
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1661;
			  {
			    tree res_op0;
			    res_op0 = captures[0];
			    tree res_op1;
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[2], _r2;
				_o2[0] = captures[4];
				_o2[1] = captures[3];
				_r2 = fold_build2_loc (loc, RDIV_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				_o1[0] = _r2;
			      }
			      _r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_SQRT, TREE_TYPE (_o1[0]), 1, _o1[0]);
			      if (!_r1)
			        goto next_after_fail1661;
			      res_op1 = _r1;
			    }
			    tree _r;
			    _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 972, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1661:;
			}
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_EXP:
	  if (call_expr_nargs (_p1) == 1)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		if (flag_unsafe_math_optimizations
)
		  {
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1662;
		    {
		      tree res_op0;
		      res_op0 = captures[0];
		      tree res_op1;
		      {
			tree _o1[1], _r1;
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[2];
			  _r2 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
			  _o1[0] = _r2;
			}
			_r1 = maybe_build_call_expr_loc (loc, CFN_EXP, TREE_TYPE (_o1[0]), 1, _o1[0]);
			if (!_r1)
			  goto next_after_fail1662;
			res_op1 = _r1;
		      }
		      tree _r;
		      _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1662:;
		  }
	      }
	    }
	  break;
	case CFN_BUILT_IN_POW10F:
	  if (call_expr_nargs (_p1) == 1)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		if (flag_unsafe_math_optimizations
)
		  {
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1663;
		    {
		      tree res_op0;
		      res_op0 = captures[0];
		      tree res_op1;
		      {
			tree _o1[1], _r1;
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[2];
			  _r2 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
			  _o1[0] = _r2;
			}
			_r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_POW10F, TREE_TYPE (_o1[0]), 1, _o1[0]);
			if (!_r1)
			  goto next_after_fail1663;
			res_op1 = _r1;
		      }
		      tree _r;
		      _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1663:;
		  }
	      }
	    }
	  break;
	case CFN_BUILT_IN_POW10L:
	  if (call_expr_nargs (_p1) == 1)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		if (flag_unsafe_math_optimizations
)
		  {
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1664;
		    {
		      tree res_op0;
		      res_op0 = captures[0];
		      tree res_op1;
		      {
			tree _o1[1], _r1;
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[2];
			  _r2 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
			  _o1[0] = _r2;
			}
			_r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_POW10L, TREE_TYPE (_o1[0]), 1, _o1[0]);
			if (!_r1)
			  goto next_after_fail1664;
			res_op1 = _r1;
		      }
		      tree _r;
		      _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1664:;
		  }
	      }
	    }
	  break;
	case CFN_EXP2:
	  if (call_expr_nargs (_p1) == 1)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		if (flag_unsafe_math_optimizations
)
		  {
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1665;
		    {
		      tree res_op0;
		      res_op0 = captures[0];
		      tree res_op1;
		      {
			tree _o1[1], _r1;
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[2];
			  _r2 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
			  _o1[0] = _r2;
			}
			_r1 = maybe_build_call_expr_loc (loc, CFN_EXP2, TREE_TYPE (_o1[0]), 1, _o1[0]);
			if (!_r1)
			  goto next_after_fail1665;
			res_op1 = _r1;
		      }
		      tree _r;
		      _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1665:;
		  }
	      }
	    }
	  break;
	case CFN_SQRT:
	  if (call_expr_nargs (_p1) == 1)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
		{
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
		    tree res = generic_simplify_313 (loc, type, _p0, _p1, captures, CFN_SQRT);
		    if (res) return res;
		  }
	        }
	      switch (TREE_CODE (_q30))
	        {
		case RDIV_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q40, _q41 };
		      if (flag_unsafe_math_optimizations
)
			{
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1666;
			  {
			    tree res_op0;
			    res_op0 = captures[0];
			    tree res_op1;
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[2], _r2;
				_o2[0] = captures[4];
				_o2[1] = captures[3];
				_r2 = fold_build2_loc (loc, RDIV_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				_o1[0] = _r2;
			      }
			      _r1 = maybe_build_call_expr_loc (loc, CFN_SQRT, TREE_TYPE (_o1[0]), 1, _o1[0]);
			      if (!_r1)
			        goto next_after_fail1666;
			      res_op1 = _r1;
			    }
			    tree _r;
			    _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 972, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1666:;
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
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		if (flag_unsafe_math_optimizations
)
		  {
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1667;
		    {
		      tree res_op0;
		      res_op0 = captures[0];
		      tree res_op1;
		      {
			tree _o1[1], _r1;
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[2];
			  _r2 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
			  _o1[0] = _r2;
			}
			_r1 = maybe_build_call_expr_loc (loc, CFN_EXP10, TREE_TYPE (_o1[0]), 1, _o1[0]);
			if (!_r1)
			  goto next_after_fail1667;
			res_op1 = _r1;
		      }
		      tree _r;
		      _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1667:;
		  }
	      }
	    }
	  break;
	case CFN_BUILT_IN_CBRTF:
	  if (call_expr_nargs (_p1) == 1)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      switch (TREE_CODE (_q30))
	        {
		case RDIV_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q40, _q41 };
		      if (flag_unsafe_math_optimizations
)
			{
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1668;
			  {
			    tree res_op0;
			    res_op0 = captures[0];
			    tree res_op1;
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[2], _r2;
				_o2[0] = captures[4];
				_o2[1] = captures[3];
				_r2 = fold_build2_loc (loc, RDIV_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				_o1[0] = _r2;
			      }
			      _r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_CBRTF, TREE_TYPE (_o1[0]), 1, _o1[0]);
			      if (!_r1)
			        goto next_after_fail1668;
			      res_op1 = _r1;
			    }
			    tree _r;
			    _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 972, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1668:;
			}
		    }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_CBRTL:
	  if (call_expr_nargs (_p1) == 1)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      switch (TREE_CODE (_q30))
	        {
		case RDIV_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q40, _q41 };
		      if (flag_unsafe_math_optimizations
)
			{
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1669;
			  {
			    tree res_op0;
			    res_op0 = captures[0];
			    tree res_op1;
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[2], _r2;
				_o2[0] = captures[4];
				_o2[1] = captures[3];
				_r2 = fold_build2_loc (loc, RDIV_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				_o1[0] = _r2;
			      }
			      _r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_CBRTL, TREE_TYPE (_o1[0]), 1, _o1[0]);
			      if (!_r1)
			        goto next_after_fail1669;
			      res_op1 = _r1;
			    }
			    tree _r;
			    _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 972, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1669:;
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
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		if (flag_unsafe_math_optimizations
)
		  {
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1670;
		    {
		      tree res_op0;
		      res_op0 = captures[0];
		      tree res_op1;
		      {
			tree _o1[1], _r1;
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[2];
			  _r2 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
			  _o1[0] = _r2;
			}
			_r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP10, TREE_TYPE (_o1[0]), 1, _o1[0]);
			if (!_r1)
			  goto next_after_fail1670;
			res_op1 = _r1;
		      }
		      tree _r;
		      _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1670:;
		  }
	      }
	    }
	  break;
	case CFN_BUILT_IN_EXP2F:
	  if (call_expr_nargs (_p1) == 1)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		if (flag_unsafe_math_optimizations
)
		  {
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1671;
		    {
		      tree res_op0;
		      res_op0 = captures[0];
		      tree res_op1;
		      {
			tree _o1[1], _r1;
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[2];
			  _r2 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
			  _o1[0] = _r2;
			}
			_r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP2F, TREE_TYPE (_o1[0]), 1, _o1[0]);
			if (!_r1)
			  goto next_after_fail1671;
			res_op1 = _r1;
		      }
		      tree _r;
		      _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1671:;
		  }
	      }
	    }
	  break;
	case CFN_BUILT_IN_EXP2L:
	  if (call_expr_nargs (_p1) == 1)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		if (flag_unsafe_math_optimizations
)
		  {
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1672;
		    {
		      tree res_op0;
		      res_op0 = captures[0];
		      tree res_op1;
		      {
			tree _o1[1], _r1;
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[2];
			  _r2 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
			  _o1[0] = _r2;
			}
			_r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_EXP2L, TREE_TYPE (_o1[0]), 1, _o1[0]);
			if (!_r1)
			  goto next_after_fail1672;
			res_op1 = _r1;
		      }
		      tree _r;
		      _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1672:;
		  }
	      }
	    }
	  break;
	case CFN_BUILT_IN_POW10:
	  if (call_expr_nargs (_p1) == 1)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		if (flag_unsafe_math_optimizations
)
		  {
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1673;
		    {
		      tree res_op0;
		      res_op0 = captures[0];
		      tree res_op1;
		      {
			tree _o1[1], _r1;
			{
			  tree _o2[1], _r2;
			  _o2[0] = captures[2];
			  _r2 = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (_o2[0]), _o2[0]);
			  _o1[0] = _r2;
			}
			_r1 = maybe_build_call_expr_loc (loc, CFN_BUILT_IN_POW10, TREE_TYPE (_o1[0]), 1, _o1[0]);
			if (!_r1)
			  goto next_after_fail1673;
			res_op1 = _r1;
		      }
		      tree _r;
		      _r = fold_build2_loc (loc, MULT_EXPR, type, res_op0, res_op1);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 973, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1673:;
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
	case CFN_BUILT_IN_SINHF:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_COSHF:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_316 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SINHF, CFN_BUILT_IN_COSHF, CFN_BUILT_IN_TANHF);
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
	case CFN_BUILT_IN_SINHL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_COSHL:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_316 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SINHL, CFN_BUILT_IN_COSHL, CFN_BUILT_IN_TANHL);
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
	case CFN_BUILT_IN_TANHF:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_SINHF:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_317 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_TANHF, CFN_BUILT_IN_SINHF, CFN_BUILT_IN_COSHF);
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
	case CFN_BUILT_IN_TANHL:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_SINHL:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_317 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_TANHL, CFN_BUILT_IN_SINHL, CFN_BUILT_IN_COSHL);
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
	case CFN_BUILT_IN_COS:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_SIN:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_318 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COS, CFN_BUILT_IN_SIN, CFN_BUILT_IN_TAN);
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
	case CFN_BUILT_IN_POW:
	  if (call_expr_nargs (_p0) == 2)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      tree _q21 = CALL_EXPR_ARG (_p0, 1);
	      switch (TREE_CODE (_q21))
	        {
		case REAL_CST:
		  {
		    if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			  tree res = generic_simplify_320 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POW);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_SIN:
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
				tree res = generic_simplify_314 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SIN, CFN_BUILT_IN_COS, CFN_BUILT_IN_TAN);
				if (res) return res;
			      }
			    }
		        }
		      break;
		    case CFN_BUILT_IN_TAN:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_315 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SIN, CFN_BUILT_IN_TAN, CFN_BUILT_IN_COS);
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
	case CFN_BUILT_IN_TAN:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_SIN:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_319 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_TAN, CFN_BUILT_IN_SIN, CFN_BUILT_IN_COS);
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
	case CFN_BUILT_IN_COSF:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_SINF:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_318 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COSF, CFN_BUILT_IN_SINF, CFN_BUILT_IN_TANF);
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
		    case CFN_BUILT_IN_SINL:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_318 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_COSL, CFN_BUILT_IN_SINL, CFN_BUILT_IN_TANL);
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
	      switch (TREE_CODE (_q21))
	        {
		case REAL_CST:
		  {
		    if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			  tree res = generic_simplify_320 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POWF);
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
	      switch (TREE_CODE (_q21))
	        {
		case REAL_CST:
		  {
		    if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			  tree res = generic_simplify_320 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POWL);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_BUILT_IN_SINF:
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
				tree res = generic_simplify_314 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SINF, CFN_BUILT_IN_COSF, CFN_BUILT_IN_TANF);
				if (res) return res;
			      }
			    }
		        }
		      break;
		    case CFN_BUILT_IN_TANF:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_315 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SINF, CFN_BUILT_IN_TANF, CFN_BUILT_IN_COSF);
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
	case CFN_BUILT_IN_SINH:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_COSH:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_316 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SINH, CFN_BUILT_IN_COSH, CFN_BUILT_IN_TANH);
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
	case CFN_BUILT_IN_SINL:
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
				tree res = generic_simplify_314 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SINL, CFN_BUILT_IN_COSL, CFN_BUILT_IN_TANL);
				if (res) return res;
			      }
			    }
		        }
		      break;
		    case CFN_BUILT_IN_TANL:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_315 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SINL, CFN_BUILT_IN_TANL, CFN_BUILT_IN_COSL);
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
	case CFN_BUILT_IN_TANF:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_SINF:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_319 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_TANF, CFN_BUILT_IN_SINF, CFN_BUILT_IN_COSF);
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
	case CFN_BUILT_IN_TANH:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_BUILT_IN_SINH:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_317 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_TANH, CFN_BUILT_IN_SINH, CFN_BUILT_IN_COSH);
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
		    case CFN_BUILT_IN_SINL:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_319 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_TANL, CFN_BUILT_IN_SINL, CFN_BUILT_IN_COSL);
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
	case CFN_COS:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_SIN:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_318 (loc, type, _p0, _p1, captures, CFN_COS, CFN_SIN, CFN_TAN);
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
	      switch (TREE_CODE (_q21))
	        {
		case REAL_CST:
		  {
		    if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			  tree res = generic_simplify_320 (loc, type, _p0, _p1, captures, CFN_POW);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	    }
	  break;
	case CFN_SIN:
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
				tree res = generic_simplify_314 (loc, type, _p0, _p1, captures, CFN_SIN, CFN_COS, CFN_TAN);
				if (res) return res;
			      }
			    }
		        }
		      break;
		    case CFN_TAN:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_315 (loc, type, _p0, _p1, captures, CFN_SIN, CFN_TAN, CFN_COS);
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
	case CFN_TAN:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_SIN:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_319 (loc, type, _p0, _p1, captures, CFN_TAN, CFN_SIN, CFN_COS);
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
	case CFN_SINH:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_COSH:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_316 (loc, type, _p0, _p1, captures, CFN_SINH, CFN_COSH, CFN_TANH);
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
	case CFN_TANH:
	  if (call_expr_nargs (_p0) == 1)
    {
	      tree _q20 = CALL_EXPR_ARG (_p0, 0);
	      switch (TREE_CODE (_p1))
	        {
		case CALL_EXPR:
		  switch (get_call_combined_fn (_p1))
		    {
		    case CFN_SINH:
		      if (call_expr_nargs (_p1) == 1)
    {
			  tree _q40 = CALL_EXPR_ARG (_p1, 0);
			  if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
				tree res = generic_simplify_317 (loc, type, _p0, _p1, captures, CFN_TANH, CFN_SINH, CFN_COSH);
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
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31 };
		tree res = generic_simplify_321 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POW);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_BUILT_IN_POWF:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31 };
		tree res = generic_simplify_321 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POWF);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_BUILT_IN_POWL:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31 };
		tree res = generic_simplify_321 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_POWL);
		if (res) return res;
	      }
	    }
	  break;
	case CFN_POW:
	  if (call_expr_nargs (_p1) == 2)
    {
	      tree _q30 = CALL_EXPR_ARG (_p1, 0);
	      tree _q31 = CALL_EXPR_ARG (_p1, 1);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31 };
		tree res = generic_simplify_321 (loc, type, _p0, _p1, captures, CFN_POW);
		if (res) return res;
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
generic_simplify_CEIL_MOD_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	tree res = generic_simplify_304 (loc, type, _p0, _p1, captures, CEIL_MOD_EXPR);
	if (res) return res;
      }
    }
  if (integer_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_305 (loc, type, _p0, _p1, captures, CEIL_MOD_EXPR);
	if (res) return res;
      }
    }
  if (integer_minus_onep (_p1))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	tree res = generic_simplify_306 (loc, type, _p0, _p1, captures, CEIL_MOD_EXPR);
	if (res) return res;
      }
    }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_307 (loc, type, _p0, _p1, captures, CEIL_MOD_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p0))
    {
    case CEIL_MOD_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_308 (loc, type, _p0, _p1, captures, CEIL_MOD_EXPR);
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
		      tree res = generic_simplify_309 (loc, type, _p0, _p1, captures, CEIL_MOD_EXPR);
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
 && (TREE_CODE_CLASS (CEIL_MOD_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1688;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, CEIL_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1688;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, CEIL_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1688;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1688:;
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
 && (TREE_CODE_CLASS (CEIL_MOD_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1689;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1689;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, CEIL_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1689;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, CEIL_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1689;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1689:;
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
 && (TREE_CODE_CLASS (CEIL_MOD_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1690;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1690;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, CEIL_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1690;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, CEIL_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1690;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1690:;
	    }
	}
        break;
      }
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_ROUND_MOD_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	tree res = generic_simplify_304 (loc, type, _p0, _p1, captures, ROUND_MOD_EXPR);
	if (res) return res;
      }
    }
  if (integer_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_305 (loc, type, _p0, _p1, captures, ROUND_MOD_EXPR);
	if (res) return res;
      }
    }
  if (integer_minus_onep (_p1))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	tree res = generic_simplify_306 (loc, type, _p0, _p1, captures, ROUND_MOD_EXPR);
	if (res) return res;
      }
    }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_307 (loc, type, _p0, _p1, captures, ROUND_MOD_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p0))
    {
    case ROUND_MOD_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_308 (loc, type, _p0, _p1, captures, ROUND_MOD_EXPR);
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
		      tree res = generic_simplify_309 (loc, type, _p0, _p1, captures, ROUND_MOD_EXPR);
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
 && (TREE_CODE_CLASS (ROUND_MOD_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1691;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, ROUND_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1691;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, ROUND_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1691;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1691:;
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
 && (TREE_CODE_CLASS (ROUND_MOD_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1692;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1692;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, ROUND_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1692;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, ROUND_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1692;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1692:;
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
 && (TREE_CODE_CLASS (ROUND_MOD_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1693;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1693;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, ROUND_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1693;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, ROUND_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1693;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1693:;
	    }
	}
        break;
      }
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_TRUNC_MOD_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	tree res = generic_simplify_304 (loc, type, _p0, _p1, captures, TRUNC_MOD_EXPR);
	if (res) return res;
      }
    }
  if (integer_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_305 (loc, type, _p0, _p1, captures, TRUNC_MOD_EXPR);
	if (res) return res;
      }
    }
  if (integer_minus_onep (_p1))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	tree res = generic_simplify_306 (loc, type, _p0, _p1, captures, TRUNC_MOD_EXPR);
	if (res) return res;
      }
    }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_307 (loc, type, _p0, _p1, captures, TRUNC_MOD_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p0))
    {
    case TRUNC_MOD_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_308 (loc, type, _p0, _p1, captures, TRUNC_MOD_EXPR);
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
		      tree res = generic_simplify_309 (loc, type, _p0, _p1, captures, TRUNC_MOD_EXPR);
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
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (TYPE_SIGN (type) == SIGNED
 && !TREE_OVERFLOW (captures[1])
 && wi::neg_p (wi::to_wide (captures[1]))
 && !TYPE_OVERFLOW_TRAPS (type)
 && !sign_bit_p (captures[1], captures[1])
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1694;
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
		_r = fold_build2_loc (loc, TRUNC_MOD_EXPR, type, res_op0, res_op1);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 975, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1694:;
	    }
	}
        break;
      }
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
		tree res = generic_simplify_357 (loc, type, _p0, _p1, captures);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
      {
	tree _q30_pops[1];
	if (tree_power_of_two_cand (_q30, _q30_pops))
	  {
	    tree _q40 = _q30_pops[0];
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q40 };
	      tree res = generic_simplify_310 (loc, type, _p0, _p1, captures, TRUNC_MOD_EXPR);
	      if (res) return res;
	    }
	  }
      }
        break;
      }
    case NEGATE_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	  tree res = generic_simplify_357 (loc, type, _p0, _p1, captures);
	  if (res) return res;
	}
        break;
      }
    default:;
    }
{
  tree _p1_pops[1];
  if (tree_power_of_two_cand (_p1, _p1_pops))
    {
      tree _q30 = _p1_pops[0];
      {
	tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
	tree res = generic_simplify_310 (loc, type, _p0, _p1, captures, TRUNC_MOD_EXPR);
	if (res) return res;
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
 && (TREE_CODE_CLASS (TRUNC_MOD_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1695;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, TRUNC_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1695;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, TRUNC_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1695;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1695:;
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
 && (TREE_CODE_CLASS (TRUNC_MOD_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1696;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1696;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, TRUNC_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1696;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, TRUNC_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1696;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1696:;
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
 && (TREE_CODE_CLASS (TRUNC_MOD_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1697;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1697;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, TRUNC_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1697;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, TRUNC_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1697;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1697:;
	    }
	}
        break;
      }
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_LSHIFT_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
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
		tree res = generic_simplify_358 (loc, type, _p0, _p1, captures, LSHIFT_EXPR, TRUNC_MOD_EXPR);
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
		tree res = generic_simplify_358 (loc, type, _p0, _p1, captures, LSHIFT_EXPR, FLOOR_MOD_EXPR);
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
      tree res = generic_simplify_359 (loc, type, _p0, _p1, captures, LSHIFT_EXPR);
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
	  case LT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if (integer_zerop (_q31))
		{
		  switch (TREE_CODE (_p1))
		    {
		    case INTEGER_CST:
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p1 };
			  if (TYPE_SIGN (TREE_TYPE (captures[0])) == SIGNED
 && wi::eq_p (wi::to_wide (captures[2]), TYPE_PRECISION (TREE_TYPE (captures[0])) - 1)
)
			    {
			      {
 wide_int wone = wi::one (TYPE_PRECISION (type));
				  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1698;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1698;
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
				    res_op1 =  wide_int_to_tree (type,
 wi::lshift (wone, wi::to_wide (captures[2])));
				    tree _r;
				    _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
				    if (TREE_SIDE_EFFECTS (captures[1]))
				      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 976, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail1698:;
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
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_q30))
	        {
		case RSHIFT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_q41))
		      {
		      case INTEGER_CST:
		        {
			  if ((_p1 == _q41 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q41, 0) && types_match (_p1, _q41)))
			    {
			      {
				tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q40, _q41 };
				if (wi::ltu_p (wi::to_wide (captures[2]), element_precision (type))
 && INTEGRAL_TYPE_P (type)
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_PRECISION (type) == TYPE_PRECISION (TREE_TYPE (captures[1]))
 && (TYPE_PRECISION (TREE_TYPE (captures[0])) >= TYPE_PRECISION (type)
 || wi::geu_p (wi::to_wide (captures[2]),
 TYPE_PRECISION (type)
 - TYPE_PRECISION (TREE_TYPE (captures[0]))))
)
				  {
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1699;
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
					tree _o1[2], _r1;
					_o1[0] =  build_minus_one_cst (type);
					_o1[1] = captures[2];
					_r1 = fold_build2_loc (loc, LSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					res_op1 = _r1;
				      }
				      tree _r;
				      _r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 977, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1699:;
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
    case RSHIFT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
		    tree res = generic_simplify_360 (loc, type, _p0, _p1, captures);
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
	case RSHIFT_EXPR:
	  {
	    tree _q30 = TREE_OPERAND (_q20, 0);
	    tree _q31 = TREE_OPERAND (_q20, 1);
	    switch (TREE_CODE (_q31))
	      {
	      case INTEGER_CST:
	        {
		  if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
			tree res = generic_simplify_360 (loc, type, _p0, _p1, captures);
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
if (integer_onep (_p0))
  {
    switch (TREE_CODE (_p1))
      {
      case MINUS_EXPR:
        {
	  tree _q30 = TREE_OPERAND (_p1, 0);
	  tree _q31 = TREE_OPERAND (_p1, 1);
	  switch (TREE_CODE (_q30))
	    {
	    case INTEGER_CST:
	      {
		{
		  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31 };
		  if (INTEGRAL_TYPE_P (type)
 && wi::eq_p (wi::to_wide (captures[2]), TYPE_PRECISION (type) - 1)
 && single_use (captures[1])
)
		    {
		      if (TYPE_UNSIGNED (type)
)
			{
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1700;
			  {
			    tree res_op0;
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] = captures[2];
			      _r1 = fold_build2_loc (loc, LSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      res_op0 = _r1;
			    }
			    tree res_op1;
			    res_op1 = captures[3];
			    tree _r;
			    _r = fold_build2_loc (loc, RSHIFT_EXPR, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 978, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1700:;
			}
		      else
			{
			  {
 tree utype = unsigned_type_for (type);
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1701;
			      {
				tree res_op0;
				{
				  tree _o1[2], _r1;
				  {
				    tree _o2[2], _r2;
				    {
				      tree _o3[1], _r3;
				      _o3[0] = captures[0];
				      if (TREE_TYPE (_o3[0]) != utype)
					{
					  _r3 = fold_build1_loc (loc, NOP_EXPR, utype, _o3[0]);
					}
				      else
				        _r3 = _o3[0];
				      _o2[0] = _r3;
				    }
				    _o2[1] = captures[2];
				    _r2 = fold_build2_loc (loc, LSHIFT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
				    _o1[0] = _r2;
				  }
				  _o1[1] = captures[3];
				  _r1 = fold_build2_loc (loc, RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				  res_op0 = _r1;
				}
				tree _r;
				_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 979, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail1701:;
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
  if (integer_zerop (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_361 (loc, type, _p0, _p1, captures, LSHIFT_EXPR);
	if (res) return res;
      }
    }
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	tree res = generic_simplify_362 (loc, type, _p0, _p1, captures, LSHIFT_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case VECTOR_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  tree res = generic_simplify_363 (loc, type, _p0, _p1, captures, LSHIFT_EXPR);
	  if (res) return res;
	}
        break;
      }
    case CONSTRUCTOR:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  tree res = generic_simplify_364 (loc, type, _p0, _p1, captures, LSHIFT_EXPR);
	  if (res) return res;
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
		      tree res = generic_simplify_365 (loc, type, _p0, _p1, captures, LSHIFT_EXPR);
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
			    tree res = generic_simplify_366 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, LSHIFT_EXPR);
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
			    tree res = generic_simplify_366 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, LSHIFT_EXPR);
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
			    tree res = generic_simplify_366 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, LSHIFT_EXPR);
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
		      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21, _p1 };
		      tree res = generic_simplify_367 (loc, type, _p0, _p1, captures, BIT_AND_EXPR, LSHIFT_EXPR);
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
		      tree res = generic_simplify_367 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, LSHIFT_EXPR);
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
		      tree res = generic_simplify_367 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, LSHIFT_EXPR);
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
 && (TREE_CODE_CLASS (LSHIFT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1702;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, LSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1702;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, LSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1702;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1702:;
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
 && (TREE_CODE_CLASS (LSHIFT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1703;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1703;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, LSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1703;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, LSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1703;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1703:;
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
 && (TREE_CODE_CLASS (LSHIFT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1704;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1704;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, LSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1704;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, LSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1704;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1704:;
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
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1705;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[0];
				    _o1[1] = captures[2];
				    _r1 = fold_build2_loc (loc, LSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
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
next_after_fail1705:;
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
generic_simplify_MIN_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_455 (loc, type, _p0, _p1, captures, MIN_EXPR);
	if (res) return res;
      }
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
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_456 (loc, type, _p0, _p1, captures, MIN_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
	      tree res = generic_simplify_456 (loc, type, _p0, _p1, captures, MIN_EXPR);
	      if (res) return res;
	    }
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
	      tree captures[3] ATTRIBUTE_UNUSED = { _p1, _p0, _q31 };
	      tree res = generic_simplify_457 (loc, type, _p0, _p1, captures, MIN_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p1, _p0, _q30 };
	      tree res = generic_simplify_457 (loc, type, _p0, _p1, captures, MIN_EXPR);
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
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_458 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_458 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
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
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _p0 };
	      tree res = generic_simplify_458 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q31, _p0 };
	      tree res = generic_simplify_458 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
        break;
      }
    case NEGATE_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	      tree res = generic_simplify_459 (loc, type, _p0, _p1, captures);
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
	      tree res = generic_simplify_459 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (INTEGRAL_TYPE_P (type)
 && TYPE_MIN_VALUE (type)
 && operand_equal_p (captures[1], TYPE_MIN_VALUE (type), OEP_ONLY_CONST)
)
      {
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1730;
	{
	  tree _r;
	  _r = captures[1];
	  if (TREE_SIDE_EFFECTS (captures[0]))
	    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 993, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1730:;
      }
    else
      {
	if (INTEGRAL_TYPE_P (type)
 && TYPE_MAX_VALUE (type)
 && operand_equal_p (captures[1], TYPE_MAX_VALUE (type), OEP_ONLY_CONST)
)
	  {
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1731;
	    {
	      tree _r;
	      _r = captures[0];
	      if (TREE_SIDE_EFFECTS (captures[1]))
		_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 994, __FILE__, __LINE__, true);
	      return _r;
	    }
next_after_fail1731:;
	  }
      }
  }
  switch (TREE_CODE (_p1))
    {
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
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q31 };
		    if (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))
)
		      {
			if (tree_int_cst_sgn (captures[2]) > 0
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1732;
			    {
			      tree _r;
			      _r = captures[0];
			      if (TREE_SIDE_EFFECTS (captures[2]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 995, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1732:;
			  }
			else
			  {
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1733;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1733;
			    {
			      tree _r;
			      _r = captures[1];
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 996, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1733:;
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
  switch (TREE_CODE (_p0))
    {
    case PLUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case INTEGER_CST:
	    {
	      if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p0, _q21 };
		    if (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))
)
		      {
			if (tree_int_cst_sgn (captures[2]) > 0
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1734;
			    {
			      tree _r;
			      _r = captures[0];
			      if (TREE_SIDE_EFFECTS (captures[2]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 995, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1734:;
			  }
			else
			  {
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1735;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1735;
			    {
			      tree _r;
			      _r = captures[1];
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 996, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1735:;
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
			    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
			    tree res = generic_simplify_460 (loc, type, _p0, _p1, captures, MIN_EXPR, LT_EXPR);
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
		      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _p1 };
		      tree res = generic_simplify_461 (loc, type, _p0, _p1, captures, MIN_EXPR, LT_EXPR);
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
		      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _p1, _q30 };
		      tree res = generic_simplify_462 (loc, type, _p0, _p1, captures, MIN_EXPR, LT_EXPR);
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
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _p1, _p1 };
		tree res = generic_simplify_463 (loc, type, _p0, _p1, captures, MIN_EXPR, LT_EXPR);
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
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
		if (FLOAT_TYPE_P (TREE_TYPE (captures[1]))
 || (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[1])))
)
		  {
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1736;
		    {
		      tree res_op0;
		      {
			tree _o1[2], _r1;
			_o1[0] = captures[1];
			_o1[1] = captures[3];
			_r1 = fold_build2_loc (loc, MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			res_op0 = _r1;
		      }
		      tree _r;
		      _r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1736:;
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
	  case BIT_NOT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
		tree res = generic_simplify_464 (loc, type, _p0, _p1, captures, MIN_EXPR, MAX_EXPR);
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
 && (TREE_CODE_CLASS (MIN_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1737;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, MIN_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1737;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, MIN_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1737;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1737:;
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
 && (TREE_CODE_CLASS (MIN_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1738;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1738;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, MIN_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1738;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, MIN_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1738;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1738:;
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
 && (TREE_CODE_CLASS (MIN_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1739;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1739;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, MIN_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1739;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, MIN_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1739;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1739:;
	    }
	}
        break;
      }
    default:;
    }
  if (tree_zero_one_valued_p (_p0))
    {
      if (tree_zero_one_valued_p (_p1))
	{
	  {
	    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	    tree res = generic_simplify_465 (loc, type, _p0, _p1, captures, MIN_EXPR, BIT_AND_EXPR);
	    if (res) return res;
	  }
        }
    }
  return NULL_TREE;
}

tree
generic_simplify_LROTATE_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_all_onesp (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	tree res = generic_simplify_498 (loc, type, _p0, _p1, captures, LROTATE_EXPR);
	if (res) return res;
      }
    }
  if (integer_zerop (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_361 (loc, type, _p0, _p1, captures, LROTATE_EXPR);
	if (res) return res;
      }
    }
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	tree res = generic_simplify_362 (loc, type, _p0, _p1, captures, LROTATE_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case VECTOR_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  tree res = generic_simplify_363 (loc, type, _p0, _p1, captures, LROTATE_EXPR);
	  if (res) return res;
	}
        break;
      }
    case CONSTRUCTOR:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  tree res = generic_simplify_364 (loc, type, _p0, _p1, captures, LROTATE_EXPR);
	  if (res) return res;
	}
        break;
      }
    case INTEGER_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1750;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1750;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree res_op1;
	    res_op1 =  const_binop (MINUS_EXPR, TREE_TYPE (captures[1]),
 build_int_cst (TREE_TYPE (captures[1]),
 element_precision (type)), captures[1]);
	    tree _r;
	    _r = fold_build2_loc (loc, RROTATE_EXPR, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1002, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1750:;
	}
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case LROTATE_EXPR:
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
		      tree res = generic_simplify_365 (loc, type, _p0, _p1, captures, LROTATE_EXPR);
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
  return NULL_TREE;
}

tree
generic_simplify_UNORDERED_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
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
 && (TREE_CODE_CLASS (UNORDERED_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1752;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, UNORDERED_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1752;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, UNORDERED_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1752;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1752:;
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
 && (TREE_CODE_CLASS (UNORDERED_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1753;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1753;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, UNORDERED_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1753;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, UNORDERED_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1753;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1753:;
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
 && (TREE_CODE_CLASS (UNORDERED_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1754;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1754;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, UNORDERED_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1754;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, UNORDERED_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1754;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1754:;
	    }
	}
        break;
      }
    default:;
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
		tree res = generic_simplify_244 (loc, type, _p0, _p1, captures, UNORDERED_EXPR, UNORDERED_EXPR);
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
		tree res = generic_simplify_247 (loc, type, _p0, _p1, captures, UNORDERED_EXPR, UNORDERED_EXPR);
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
	    tree res = generic_simplify_248 (loc, type, _p0, _p1, captures, UNORDERED_EXPR, UNORDERED_EXPR);
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
	  tree res = generic_simplify_519 (loc, type, _p0, _p1, captures, UNORDERED_EXPR);
	  if (res) return res;
	}
        break;
      }
    default:;
    }
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (tree_expr_nan_p (captures[0]) || tree_expr_nan_p (captures[1])
)
      {
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1755;
	{
	  tree _r;
	  _r =  constant_boolean_node (true, type);
	  if (TREE_SIDE_EFFECTS (captures[0]))
	    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	  if (TREE_SIDE_EFFECTS (captures[1]))
	    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1004, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1755:;
      }
    else
      {
	if (!tree_expr_maybe_nan_p (captures[0]) && !tree_expr_maybe_nan_p (captures[1])
)
	  {
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1756;
	    {
	      tree _r;
	      _r =  constant_boolean_node (false, type);
	      if (TREE_SIDE_EFFECTS (captures[0]))
		_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	      if (TREE_SIDE_EFFECTS (captures[1]))
		_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1005, __FILE__, __LINE__, true);
	      return _r;
	    }
next_after_fail1756:;
	  }
      }
  }
  return NULL_TREE;
}

tree
generic_simplify_UNGT_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
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
 && (TREE_CODE_CLASS (UNGT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1768;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, UNGT_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1768;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, UNGT_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1768;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1768:;
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
 && (TREE_CODE_CLASS (UNGT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1769;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1769;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, UNGT_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1769;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, UNGT_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1769;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1769:;
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
 && (TREE_CODE_CLASS (UNGT_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1770;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1770;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, UNGT_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1770;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, UNGT_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1770;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1770:;
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
	tree res = generic_simplify_520 (loc, type, _p0, _p1, captures, UNGT_EXPR);
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
		tree res = generic_simplify_244 (loc, type, _p0, _p1, captures, UNGT_EXPR, GT_EXPR);
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
		tree res = generic_simplify_247 (loc, type, _p0, _p1, captures, UNGT_EXPR, UNLT_EXPR);
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
	    tree res = generic_simplify_248 (loc, type, _p0, _p1, captures, UNGT_EXPR, UNLT_EXPR);
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
	  tree res = generic_simplify_519 (loc, type, _p0, _p1, captures, UNGT_EXPR);
	  if (res) return res;
	}
        break;
      }
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_TRUTH_ORIF_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case LE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q20))
	  {
	  case POINTER_PLUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_q31))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case LE_EXPR:
		        {
			  tree _q70 = TREE_OPERAND (_p1, 0);
			  tree _q71 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q70))
			    {
			    case POINTER_PLUS_EXPR:
			      {
				tree _q80 = TREE_OPERAND (_q70, 0);
				tree _q81 = TREE_OPERAND (_q70, 1);
				if ((_q80 == _q21 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q21, 0) && types_match (_q80, _q21)))
				  {
				    if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
				      {
					if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
					  {
					    {
					      tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1, _q70 };
					      const enum tree_code cmp = LE_EXPR;
					      if (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[2]))
 && TYPE_OVERFLOW_WRAPS (sizetype)
 && TYPE_PRECISION (TREE_TYPE (captures[2])) == TYPE_PRECISION (sizetype)
)
						{
						  {
 offset_int off = wi::to_offset (captures[3]) - (cmp == LE_EXPR ? 1 : 0);
 offset_int rhs = off * 2;
						      if (wi::min_precision (rhs, UNSIGNED) <= TYPE_PRECISION (sizetype)
)
							{
							  {
 bool swap_p = tree_swap_operands_p (captures[2], captures[4]);
 tree rhs_tree = wide_int_to_tree (sizetype, rhs);
							      if (cmp == LT_EXPR
)
								{
								  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1781;
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1781;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1781;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1781;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1781;
								  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail1781;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1781;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1781;
								  {
								    tree res_op0;
								    {
								      tree _o1[1], _r1;
								      {
									tree _o2[2], _r2;
									_o2[0] =  swap_p ? captures[6] : captures[1];
									_o2[1] =  swap_p ? captures[2] : captures[4];
									_r2 = fold_build2_loc (loc, POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
									_o1[0] = _r2;
								      }
								      if (TREE_TYPE (_o1[0]) != sizetype)
									{
									  _r1 = fold_build1_loc (loc, NOP_EXPR, sizetype, _o1[0]);
									}
								      else
								        _r1 = _o1[0];
								      res_op0 = _r1;
								    }
								    tree res_op1;
								    res_op1 =  rhs_tree;
								    tree _r;
								    _r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
								    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 910, __FILE__, __LINE__, true);
								    return _r;
								  }
next_after_fail1781:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1782;
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1782;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1782;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1782;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1782;
								  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail1782;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1782;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1782;
								  {
								    tree res_op0;
								    {
								      tree _o1[1], _r1;
								      {
									tree _o2[2], _r2;
									{
									  tree _o3[2], _r3;
									  _o3[0] =  swap_p ? captures[4] : captures[2];
									  _o3[1] =  wide_int_to_tree (sizetype, off);
									  _r3 = fold_build2_loc (loc, POINTER_PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
									  _o2[0] = _r3;
									}
									_o2[1] =  swap_p ? captures[2] : captures[4];
									_r2 = fold_build2_loc (loc, POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
									_o1[0] = _r2;
								      }
								      if (TREE_TYPE (_o1[0]) != sizetype)
									{
									  _r1 = fold_build1_loc (loc, NOP_EXPR, sizetype, _o1[0]);
									}
								      else
								        _r1 = _o1[0];
								      res_op0 = _r1;
								    }
								    tree res_op1;
								    res_op1 =  rhs_tree;
								    tree _r;
								    _r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
								    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 911, __FILE__, __LINE__, true);
								    return _r;
								  }
next_after_fail1782:;
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
		      case GE_EXPR:
		        {
			  tree _q70 = TREE_OPERAND (_p1, 0);
			  tree _q71 = TREE_OPERAND (_p1, 1);
			  if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
			    {
			      switch (TREE_CODE (_q71))
			        {
				case POINTER_PLUS_EXPR:
				  {
				    tree _q90 = TREE_OPERAND (_q71, 0);
				    tree _q91 = TREE_OPERAND (_q71, 1);
				    if ((_q90 == _q21 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q21, 0) && types_match (_q90, _q21)))
				      {
					if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
					  {
					    {
					      tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1, _q71 };
					      const enum tree_code cmp = LE_EXPR;
					      if (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[2]))
 && TYPE_OVERFLOW_WRAPS (sizetype)
 && TYPE_PRECISION (TREE_TYPE (captures[2])) == TYPE_PRECISION (sizetype)
)
						{
						  {
 offset_int off = wi::to_offset (captures[3]) - (cmp == LE_EXPR ? 1 : 0);
 offset_int rhs = off * 2;
						      if (wi::min_precision (rhs, UNSIGNED) <= TYPE_PRECISION (sizetype)
)
							{
							  {
 bool swap_p = tree_swap_operands_p (captures[2], captures[4]);
 tree rhs_tree = wide_int_to_tree (sizetype, rhs);
							      if (cmp == LT_EXPR
)
								{
								  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1783;
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1783;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1783;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1783;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1783;
								  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail1783;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1783;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1783;
								  {
								    tree res_op0;
								    {
								      tree _o1[1], _r1;
								      {
									tree _o2[2], _r2;
									_o2[0] =  swap_p ? captures[6] : captures[1];
									_o2[1] =  swap_p ? captures[2] : captures[4];
									_r2 = fold_build2_loc (loc, POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
									_o1[0] = _r2;
								      }
								      if (TREE_TYPE (_o1[0]) != sizetype)
									{
									  _r1 = fold_build1_loc (loc, NOP_EXPR, sizetype, _o1[0]);
									}
								      else
								        _r1 = _o1[0];
								      res_op0 = _r1;
								    }
								    tree res_op1;
								    res_op1 =  rhs_tree;
								    tree _r;
								    _r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
								    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 910, __FILE__, __LINE__, true);
								    return _r;
								  }
next_after_fail1783:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1784;
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1784;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1784;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1784;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1784;
								  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail1784;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1784;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1784;
								  {
								    tree res_op0;
								    {
								      tree _o1[1], _r1;
								      {
									tree _o2[2], _r2;
									{
									  tree _o3[2], _r3;
									  _o3[0] =  swap_p ? captures[4] : captures[2];
									  _o3[1] =  wide_int_to_tree (sizetype, off);
									  _r3 = fold_build2_loc (loc, POINTER_PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
									  _o2[0] = _r3;
									}
									_o2[1] =  swap_p ? captures[2] : captures[4];
									_r2 = fold_build2_loc (loc, POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
									_o1[0] = _r2;
								      }
								      if (TREE_TYPE (_o1[0]) != sizetype)
									{
									  _r1 = fold_build1_loc (loc, NOP_EXPR, sizetype, _o1[0]);
									}
								      else
								        _r1 = _o1[0];
								      res_op0 = _r1;
								    }
								    tree res_op1;
								    res_op1 =  rhs_tree;
								    tree _r;
								    _r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
								    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 911, __FILE__, __LINE__, true);
								    return _r;
								  }
next_after_fail1784:;
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
	switch (TREE_CODE (_q20))
	  {
	  case POINTER_PLUS_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_q31))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case LT_EXPR:
		        {
			  tree _q70 = TREE_OPERAND (_p1, 0);
			  tree _q71 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q70))
			    {
			    case POINTER_PLUS_EXPR:
			      {
				tree _q80 = TREE_OPERAND (_q70, 0);
				tree _q81 = TREE_OPERAND (_q70, 1);
				if ((_q80 == _q21 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q21, 0) && types_match (_q80, _q21)))
				  {
				    if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
				      {
					if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
					  {
					    {
					      tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1, _q70 };
					      const enum tree_code cmp = LT_EXPR;
					      if (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[2]))
 && TYPE_OVERFLOW_WRAPS (sizetype)
 && TYPE_PRECISION (TREE_TYPE (captures[2])) == TYPE_PRECISION (sizetype)
)
						{
						  {
 offset_int off = wi::to_offset (captures[3]) - (cmp == LE_EXPR ? 1 : 0);
 offset_int rhs = off * 2;
						      if (wi::min_precision (rhs, UNSIGNED) <= TYPE_PRECISION (sizetype)
)
							{
							  {
 bool swap_p = tree_swap_operands_p (captures[2], captures[4]);
 tree rhs_tree = wide_int_to_tree (sizetype, rhs);
							      if (cmp == LT_EXPR
)
								{
								  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1785;
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1785;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1785;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1785;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1785;
								  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail1785;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1785;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1785;
								  {
								    tree res_op0;
								    {
								      tree _o1[1], _r1;
								      {
									tree _o2[2], _r2;
									_o2[0] =  swap_p ? captures[6] : captures[1];
									_o2[1] =  swap_p ? captures[2] : captures[4];
									_r2 = fold_build2_loc (loc, POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
									_o1[0] = _r2;
								      }
								      if (TREE_TYPE (_o1[0]) != sizetype)
									{
									  _r1 = fold_build1_loc (loc, NOP_EXPR, sizetype, _o1[0]);
									}
								      else
								        _r1 = _o1[0];
								      res_op0 = _r1;
								    }
								    tree res_op1;
								    res_op1 =  rhs_tree;
								    tree _r;
								    _r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
								    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 910, __FILE__, __LINE__, true);
								    return _r;
								  }
next_after_fail1785:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1786;
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1786;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1786;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1786;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1786;
								  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail1786;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1786;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1786;
								  {
								    tree res_op0;
								    {
								      tree _o1[1], _r1;
								      {
									tree _o2[2], _r2;
									{
									  tree _o3[2], _r3;
									  _o3[0] =  swap_p ? captures[4] : captures[2];
									  _o3[1] =  wide_int_to_tree (sizetype, off);
									  _r3 = fold_build2_loc (loc, POINTER_PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
									  _o2[0] = _r3;
									}
									_o2[1] =  swap_p ? captures[2] : captures[4];
									_r2 = fold_build2_loc (loc, POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
									_o1[0] = _r2;
								      }
								      if (TREE_TYPE (_o1[0]) != sizetype)
									{
									  _r1 = fold_build1_loc (loc, NOP_EXPR, sizetype, _o1[0]);
									}
								      else
								        _r1 = _o1[0];
								      res_op0 = _r1;
								    }
								    tree res_op1;
								    res_op1 =  rhs_tree;
								    tree _r;
								    _r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
								    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 911, __FILE__, __LINE__, true);
								    return _r;
								  }
next_after_fail1786:;
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
		      case GT_EXPR:
		        {
			  tree _q70 = TREE_OPERAND (_p1, 0);
			  tree _q71 = TREE_OPERAND (_p1, 1);
			  if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
			    {
			      switch (TREE_CODE (_q71))
			        {
				case POINTER_PLUS_EXPR:
				  {
				    tree _q90 = TREE_OPERAND (_q71, 0);
				    tree _q91 = TREE_OPERAND (_q71, 1);
				    if ((_q90 == _q21 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q21, 0) && types_match (_q90, _q21)))
				      {
					if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
					  {
					    {
					      tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _q21, _p1, _q71 };
					      const enum tree_code cmp = LT_EXPR;
					      if (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[2]))
 && TYPE_OVERFLOW_WRAPS (sizetype)
 && TYPE_PRECISION (TREE_TYPE (captures[2])) == TYPE_PRECISION (sizetype)
)
						{
						  {
 offset_int off = wi::to_offset (captures[3]) - (cmp == LE_EXPR ? 1 : 0);
 offset_int rhs = off * 2;
						      if (wi::min_precision (rhs, UNSIGNED) <= TYPE_PRECISION (sizetype)
)
							{
							  {
 bool swap_p = tree_swap_operands_p (captures[2], captures[4]);
 tree rhs_tree = wide_int_to_tree (sizetype, rhs);
							      if (cmp == LT_EXPR
)
								{
								  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1787;
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1787;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1787;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1787;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1787;
								  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail1787;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1787;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1787;
								  {
								    tree res_op0;
								    {
								      tree _o1[1], _r1;
								      {
									tree _o2[2], _r2;
									_o2[0] =  swap_p ? captures[6] : captures[1];
									_o2[1] =  swap_p ? captures[2] : captures[4];
									_r2 = fold_build2_loc (loc, POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
									_o1[0] = _r2;
								      }
								      if (TREE_TYPE (_o1[0]) != sizetype)
									{
									  _r1 = fold_build1_loc (loc, NOP_EXPR, sizetype, _o1[0]);
									}
								      else
								        _r1 = _o1[0];
								      res_op0 = _r1;
								    }
								    tree res_op1;
								    res_op1 =  rhs_tree;
								    tree _r;
								    _r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
								    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 910, __FILE__, __LINE__, true);
								    return _r;
								  }
next_after_fail1787:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1788;
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1788;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1788;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1788;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1788;
								  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail1788;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1788;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1788;
								  {
								    tree res_op0;
								    {
								      tree _o1[1], _r1;
								      {
									tree _o2[2], _r2;
									{
									  tree _o3[2], _r3;
									  _o3[0] =  swap_p ? captures[4] : captures[2];
									  _o3[1] =  wide_int_to_tree (sizetype, off);
									  _r3 = fold_build2_loc (loc, POINTER_PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
									  _o2[0] = _r3;
									}
									_o2[1] =  swap_p ? captures[2] : captures[4];
									_r2 = fold_build2_loc (loc, POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
									_o1[0] = _r2;
								      }
								      if (TREE_TYPE (_o1[0]) != sizetype)
									{
									  _r1 = fold_build1_loc (loc, NOP_EXPR, sizetype, _o1[0]);
									}
								      else
								        _r1 = _o1[0];
								      res_op0 = _r1;
								    }
								    tree res_op1;
								    res_op1 =  rhs_tree;
								    tree _r;
								    _r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
								    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 911, __FILE__, __LINE__, true);
								    return _r;
								  }
next_after_fail1788:;
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
    case GE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case POINTER_PLUS_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_q41))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case LE_EXPR:
		        {
			  tree _q70 = TREE_OPERAND (_p1, 0);
			  tree _q71 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q70))
			    {
			    case POINTER_PLUS_EXPR:
			      {
				tree _q80 = TREE_OPERAND (_q70, 0);
				tree _q81 = TREE_OPERAND (_q70, 1);
				if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
				  {
				    if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
				      {
					if ((_q71 == _q40 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q40, 0) && types_match (_q71, _q40)))
					  {
					    {
					      tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q21, _q40, _q41, _q20, _p1, _q70 };
					      const enum tree_code cmp = LE_EXPR;
					      if (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[2]))
 && TYPE_OVERFLOW_WRAPS (sizetype)
 && TYPE_PRECISION (TREE_TYPE (captures[2])) == TYPE_PRECISION (sizetype)
)
						{
						  {
 offset_int off = wi::to_offset (captures[3]) - (cmp == LE_EXPR ? 1 : 0);
 offset_int rhs = off * 2;
						      if (wi::min_precision (rhs, UNSIGNED) <= TYPE_PRECISION (sizetype)
)
							{
							  {
 bool swap_p = tree_swap_operands_p (captures[2], captures[4]);
 tree rhs_tree = wide_int_to_tree (sizetype, rhs);
							      if (cmp == LT_EXPR
)
								{
								  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1789;
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1789;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1789;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1789;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1789;
								  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail1789;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1789;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1789;
								  {
								    tree res_op0;
								    {
								      tree _o1[1], _r1;
								      {
									tree _o2[2], _r2;
									_o2[0] =  swap_p ? captures[6] : captures[1];
									_o2[1] =  swap_p ? captures[2] : captures[4];
									_r2 = fold_build2_loc (loc, POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
									_o1[0] = _r2;
								      }
								      if (TREE_TYPE (_o1[0]) != sizetype)
									{
									  _r1 = fold_build1_loc (loc, NOP_EXPR, sizetype, _o1[0]);
									}
								      else
								        _r1 = _o1[0];
								      res_op0 = _r1;
								    }
								    tree res_op1;
								    res_op1 =  rhs_tree;
								    tree _r;
								    _r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
								    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 910, __FILE__, __LINE__, true);
								    return _r;
								  }
next_after_fail1789:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1790;
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1790;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1790;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1790;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1790;
								  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail1790;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1790;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1790;
								  {
								    tree res_op0;
								    {
								      tree _o1[1], _r1;
								      {
									tree _o2[2], _r2;
									{
									  tree _o3[2], _r3;
									  _o3[0] =  swap_p ? captures[4] : captures[2];
									  _o3[1] =  wide_int_to_tree (sizetype, off);
									  _r3 = fold_build2_loc (loc, POINTER_PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
									  _o2[0] = _r3;
									}
									_o2[1] =  swap_p ? captures[2] : captures[4];
									_r2 = fold_build2_loc (loc, POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
									_o1[0] = _r2;
								      }
								      if (TREE_TYPE (_o1[0]) != sizetype)
									{
									  _r1 = fold_build1_loc (loc, NOP_EXPR, sizetype, _o1[0]);
									}
								      else
								        _r1 = _o1[0];
								      res_op0 = _r1;
								    }
								    tree res_op1;
								    res_op1 =  rhs_tree;
								    tree _r;
								    _r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
								    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 911, __FILE__, __LINE__, true);
								    return _r;
								  }
next_after_fail1790:;
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
		      case GE_EXPR:
		        {
			  tree _q70 = TREE_OPERAND (_p1, 0);
			  tree _q71 = TREE_OPERAND (_p1, 1);
			  if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
			    {
			      switch (TREE_CODE (_q71))
			        {
				case POINTER_PLUS_EXPR:
				  {
				    tree _q90 = TREE_OPERAND (_q71, 0);
				    tree _q91 = TREE_OPERAND (_q71, 1);
				    if ((_q90 == _q20 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q20, 0) && types_match (_q90, _q20)))
				      {
					if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
					  {
					    {
					      tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q21, _q40, _q41, _q20, _p1, _q71 };
					      const enum tree_code cmp = LE_EXPR;
					      if (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[2]))
 && TYPE_OVERFLOW_WRAPS (sizetype)
 && TYPE_PRECISION (TREE_TYPE (captures[2])) == TYPE_PRECISION (sizetype)
)
						{
						  {
 offset_int off = wi::to_offset (captures[3]) - (cmp == LE_EXPR ? 1 : 0);
 offset_int rhs = off * 2;
						      if (wi::min_precision (rhs, UNSIGNED) <= TYPE_PRECISION (sizetype)
)
							{
							  {
 bool swap_p = tree_swap_operands_p (captures[2], captures[4]);
 tree rhs_tree = wide_int_to_tree (sizetype, rhs);
							      if (cmp == LT_EXPR
)
								{
								  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1791;
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1791;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1791;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1791;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1791;
								  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail1791;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1791;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1791;
								  {
								    tree res_op0;
								    {
								      tree _o1[1], _r1;
								      {
									tree _o2[2], _r2;
									_o2[0] =  swap_p ? captures[6] : captures[1];
									_o2[1] =  swap_p ? captures[2] : captures[4];
									_r2 = fold_build2_loc (loc, POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
									_o1[0] = _r2;
								      }
								      if (TREE_TYPE (_o1[0]) != sizetype)
									{
									  _r1 = fold_build1_loc (loc, NOP_EXPR, sizetype, _o1[0]);
									}
								      else
								        _r1 = _o1[0];
								      res_op0 = _r1;
								    }
								    tree res_op1;
								    res_op1 =  rhs_tree;
								    tree _r;
								    _r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
								    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 910, __FILE__, __LINE__, true);
								    return _r;
								  }
next_after_fail1791:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1792;
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1792;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1792;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1792;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1792;
								  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail1792;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1792;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1792;
								  {
								    tree res_op0;
								    {
								      tree _o1[1], _r1;
								      {
									tree _o2[2], _r2;
									{
									  tree _o3[2], _r3;
									  _o3[0] =  swap_p ? captures[4] : captures[2];
									  _o3[1] =  wide_int_to_tree (sizetype, off);
									  _r3 = fold_build2_loc (loc, POINTER_PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
									  _o2[0] = _r3;
									}
									_o2[1] =  swap_p ? captures[2] : captures[4];
									_r2 = fold_build2_loc (loc, POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
									_o1[0] = _r2;
								      }
								      if (TREE_TYPE (_o1[0]) != sizetype)
									{
									  _r1 = fold_build1_loc (loc, NOP_EXPR, sizetype, _o1[0]);
									}
								      else
								        _r1 = _o1[0];
								      res_op0 = _r1;
								    }
								    tree res_op1;
								    res_op1 =  rhs_tree;
								    tree _r;
								    _r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
								    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 911, __FILE__, __LINE__, true);
								    return _r;
								  }
next_after_fail1792:;
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
    case GT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_q21))
	  {
	  case POINTER_PLUS_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_q41))
	        {
		case INTEGER_CST:
		  {
		    switch (TREE_CODE (_p1))
		      {
		      case LT_EXPR:
		        {
			  tree _q70 = TREE_OPERAND (_p1, 0);
			  tree _q71 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q70))
			    {
			    case POINTER_PLUS_EXPR:
			      {
				tree _q80 = TREE_OPERAND (_q70, 0);
				tree _q81 = TREE_OPERAND (_q70, 1);
				if ((_q80 == _q20 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q20, 0) && types_match (_q80, _q20)))
				  {
				    if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
				      {
					if ((_q71 == _q40 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q40, 0) && types_match (_q71, _q40)))
					  {
					    {
					      tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q21, _q40, _q41, _q20, _p1, _q70 };
					      const enum tree_code cmp = LT_EXPR;
					      if (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[2]))
 && TYPE_OVERFLOW_WRAPS (sizetype)
 && TYPE_PRECISION (TREE_TYPE (captures[2])) == TYPE_PRECISION (sizetype)
)
						{
						  {
 offset_int off = wi::to_offset (captures[3]) - (cmp == LE_EXPR ? 1 : 0);
 offset_int rhs = off * 2;
						      if (wi::min_precision (rhs, UNSIGNED) <= TYPE_PRECISION (sizetype)
)
							{
							  {
 bool swap_p = tree_swap_operands_p (captures[2], captures[4]);
 tree rhs_tree = wide_int_to_tree (sizetype, rhs);
							      if (cmp == LT_EXPR
)
								{
								  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1793;
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1793;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1793;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1793;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1793;
								  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail1793;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1793;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1793;
								  {
								    tree res_op0;
								    {
								      tree _o1[1], _r1;
								      {
									tree _o2[2], _r2;
									_o2[0] =  swap_p ? captures[6] : captures[1];
									_o2[1] =  swap_p ? captures[2] : captures[4];
									_r2 = fold_build2_loc (loc, POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
									_o1[0] = _r2;
								      }
								      if (TREE_TYPE (_o1[0]) != sizetype)
									{
									  _r1 = fold_build1_loc (loc, NOP_EXPR, sizetype, _o1[0]);
									}
								      else
								        _r1 = _o1[0];
								      res_op0 = _r1;
								    }
								    tree res_op1;
								    res_op1 =  rhs_tree;
								    tree _r;
								    _r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
								    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 910, __FILE__, __LINE__, true);
								    return _r;
								  }
next_after_fail1793:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1794;
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1794;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1794;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1794;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1794;
								  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail1794;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1794;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1794;
								  {
								    tree res_op0;
								    {
								      tree _o1[1], _r1;
								      {
									tree _o2[2], _r2;
									{
									  tree _o3[2], _r3;
									  _o3[0] =  swap_p ? captures[4] : captures[2];
									  _o3[1] =  wide_int_to_tree (sizetype, off);
									  _r3 = fold_build2_loc (loc, POINTER_PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
									  _o2[0] = _r3;
									}
									_o2[1] =  swap_p ? captures[2] : captures[4];
									_r2 = fold_build2_loc (loc, POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
									_o1[0] = _r2;
								      }
								      if (TREE_TYPE (_o1[0]) != sizetype)
									{
									  _r1 = fold_build1_loc (loc, NOP_EXPR, sizetype, _o1[0]);
									}
								      else
								        _r1 = _o1[0];
								      res_op0 = _r1;
								    }
								    tree res_op1;
								    res_op1 =  rhs_tree;
								    tree _r;
								    _r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
								    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 911, __FILE__, __LINE__, true);
								    return _r;
								  }
next_after_fail1794:;
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
		      case GT_EXPR:
		        {
			  tree _q70 = TREE_OPERAND (_p1, 0);
			  tree _q71 = TREE_OPERAND (_p1, 1);
			  if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
			    {
			      switch (TREE_CODE (_q71))
			        {
				case POINTER_PLUS_EXPR:
				  {
				    tree _q90 = TREE_OPERAND (_q71, 0);
				    tree _q91 = TREE_OPERAND (_q71, 1);
				    if ((_q90 == _q20 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q20, 0) && types_match (_q90, _q20)))
				      {
					if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
					  {
					    {
					      tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q21, _q40, _q41, _q20, _p1, _q71 };
					      const enum tree_code cmp = LT_EXPR;
					      if (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[2]))
 && TYPE_OVERFLOW_WRAPS (sizetype)
 && TYPE_PRECISION (TREE_TYPE (captures[2])) == TYPE_PRECISION (sizetype)
)
						{
						  {
 offset_int off = wi::to_offset (captures[3]) - (cmp == LE_EXPR ? 1 : 0);
 offset_int rhs = off * 2;
						      if (wi::min_precision (rhs, UNSIGNED) <= TYPE_PRECISION (sizetype)
)
							{
							  {
 bool swap_p = tree_swap_operands_p (captures[2], captures[4]);
 tree rhs_tree = wide_int_to_tree (sizetype, rhs);
							      if (cmp == LT_EXPR
)
								{
								  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1795;
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1795;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1795;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1795;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1795;
								  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail1795;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1795;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1795;
								  {
								    tree res_op0;
								    {
								      tree _o1[1], _r1;
								      {
									tree _o2[2], _r2;
									_o2[0] =  swap_p ? captures[6] : captures[1];
									_o2[1] =  swap_p ? captures[2] : captures[4];
									_r2 = fold_build2_loc (loc, POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
									_o1[0] = _r2;
								      }
								      if (TREE_TYPE (_o1[0]) != sizetype)
									{
									  _r1 = fold_build1_loc (loc, NOP_EXPR, sizetype, _o1[0]);
									}
								      else
								        _r1 = _o1[0];
								      res_op0 = _r1;
								    }
								    tree res_op1;
								    res_op1 =  rhs_tree;
								    tree _r;
								    _r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
								    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 910, __FILE__, __LINE__, true);
								    return _r;
								  }
next_after_fail1795:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[0])) goto next_after_fail1796;
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1796;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1796;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1796;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1796;
								  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail1796;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1796;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1796;
								  {
								    tree res_op0;
								    {
								      tree _o1[1], _r1;
								      {
									tree _o2[2], _r2;
									{
									  tree _o3[2], _r3;
									  _o3[0] =  swap_p ? captures[4] : captures[2];
									  _o3[1] =  wide_int_to_tree (sizetype, off);
									  _r3 = fold_build2_loc (loc, POINTER_PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
									  _o2[0] = _r3;
									}
									_o2[1] =  swap_p ? captures[2] : captures[4];
									_r2 = fold_build2_loc (loc, POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
									_o1[0] = _r2;
								      }
								      if (TREE_TYPE (_o1[0]) != sizetype)
									{
									  _r1 = fold_build1_loc (loc, NOP_EXPR, sizetype, _o1[0]);
									}
								      else
								        _r1 = _o1[0];
								      res_op0 = _r1;
								    }
								    tree res_op1;
								    res_op1 =  rhs_tree;
								    tree _r;
								    _r = fold_build2_loc (loc, GT_EXPR, type, res_op0, res_op1);
								    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 911, __FILE__, __LINE__, true);
								    return _r;
								  }
next_after_fail1796:;
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
  return NULL_TREE;
}
#pragma GCC diagnostic pop
