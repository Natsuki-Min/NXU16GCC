/* Generated automatically by the program `genmatch' from
   a IL pattern matching and simplification description.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-function"

#include "generic-match-auto.h"

bool
tree_maybe_cmp (tree t, tree *res_ops)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  switch (TREE_CODE (t))
    {
    case LT_EXPR:
    case LE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case GE_EXPR:
    case GT_EXPR:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { t, _p0, _p1 };
	  {
	    res_ops[0] = captures[0];
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 5, __FILE__, __LINE__, false);
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
	  case LT_EXPR:
	  case LE_EXPR:
	  case EQ_EXPR:
	  case NE_EXPR:
	  case GE_EXPR:
	  case GT_EXPR:
	  case UNORDERED_EXPR:
	  case ORDERED_EXPR:
	  case UNLT_EXPR:
	  case UNLE_EXPR:
	  case UNGT_EXPR:
	  case UNGE_EXPR:
	  case UNEQ_EXPR:
	  case LTGT_EXPR:
	    {
	      tree _q20 = TREE_OPERAND (_p0, 0);
	      tree _q21 = TREE_OPERAND (_p0, 1);
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
		if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
		  {
		    {
		      res_ops[0] = captures[0];
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 6, __FILE__, __LINE__, false);
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
    case BIT_XOR_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { t, _p0, _p1 };
	  if (INTEGRAL_TYPE_P (type)
 && TYPE_PRECISION (type) == 1
)
	    {
	      {
		res_ops[0] = captures[0];
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 7, __FILE__, __LINE__, false);
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
tree_signed_integer_sat_add (tree t, tree *res_ops)
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
		      case BIT_XOR_EXPR:
		        {
			  tree _q40 = TREE_OPERAND (_q30, 0);
			  tree _q41 = TREE_OPERAND (_q30, 1);
			  {
			    tree _q41_pops[1];
			    if (tree_nop_convert (_q41, _q41_pops))
			      {
				tree _q60 = _q41_pops[0];
				switch (TREE_CODE (_q60))
				  {
				  case PLUS_EXPR:
				    {
				      tree _q70 = TREE_OPERAND (_q60, 0);
				      tree _q71 = TREE_OPERAND (_q60, 1);
				      {
					tree _q70_pops[1];
					if (tree_nop_convert (_q70, _q70_pops))
					  {
					    tree _q80 = _q70_pops[0];
					    if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
					      {
						{
						  tree _q71_pops[1];
						  if (tree_nop_convert (_q71, _q71_pops))
						    {
						      tree _q100 = _q71_pops[0];
						      switch (TREE_CODE (_q31))
						        {
							case BIT_NOT_EXPR:
							  {
							    tree _q120 = TREE_OPERAND (_q31, 0);
							    switch (TREE_CODE (_q120))
							      {
							      case BIT_XOR_EXPR:
							        {
								  tree _q130 = TREE_OPERAND (_q120, 0);
								  tree _q131 = TREE_OPERAND (_q120, 1);
								  if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
								    {
								      if ((_q131 == _q100 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q100, 0) && types_match (_q131, _q100)))
									{
									  if (integer_zerop (_q21))
									    {
									      switch (TREE_CODE (_p1))
									        {
										case BIT_XOR_EXPR:
										  {
										    tree _q170 = TREE_OPERAND (_p1, 0);
										    tree _q171 = TREE_OPERAND (_p1, 1);
										    switch (TREE_CODE (_q170))
										      {
										      case NEGATE_EXPR:
										        {
											  tree _q180 = TREE_OPERAND (_q170, 0);
											  switch (TREE_CODE (_q180))
											    {
											    CASE_CONVERT:
											      {
												tree _q190 = TREE_OPERAND (_q180, 0);
												switch (TREE_CODE (_q190))
												  {
												  case LT_EXPR:
												    {
												      tree _q200 = TREE_OPERAND (_q190, 0);
												      tree _q201 = TREE_OPERAND (_q190, 1);
												      if ((_q200 == _q40 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q40, 0) && types_match (_q200, _q40)))
													{
													  if (integer_zerop (_q201))
													    {
													      if (tree_max_value (_q171))
														{
														  if ((_p2 == _q41 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q41, 0) && types_match (_p2, _q41)))
														    {
														      {
															tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
															if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															  {
															    {
															      res_ops[0] = captures[0];
															      res_ops[1] = captures[1];
															      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 56, __FILE__, __LINE__, false);
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
										  if (tree_max_value (_q170))
										    {
										      switch (TREE_CODE (_q171))
										        {
											case NEGATE_EXPR:
											  {
											    tree _q190 = TREE_OPERAND (_q171, 0);
											    switch (TREE_CODE (_q190))
											      {
											      CASE_CONVERT:
											        {
												  tree _q200 = TREE_OPERAND (_q190, 0);
												  switch (TREE_CODE (_q200))
												    {
												    case LT_EXPR:
												      {
													tree _q210 = TREE_OPERAND (_q200, 0);
													tree _q211 = TREE_OPERAND (_q200, 1);
													if ((_q210 == _q40 && ! TREE_SIDE_EFFECTS (_q210)) || (operand_equal_p (_q210, _q40, 0) && types_match (_q210, _q40)))
													  {
													    if (integer_zerop (_q211))
													      {
														if ((_p2 == _q41 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q41, 0) && types_match (_p2, _q41)))
														  {
														    {
														      tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
														      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															{
															  {
															    res_ops[0] = captures[0];
															    res_ops[1] = captures[1];
															    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 56, __FILE__, __LINE__, false);
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
											    break;
											  }
										        default:;
										        }
										    }
										    break;
										  }
									        default:;
									        }
									    }
								        }
								    }
								  if ((_q130 == _q100 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q100, 0) && types_match (_q130, _q100)))
								    {
								      if ((_q131 == _q40 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q40, 0) && types_match (_q131, _q40)))
									{
									  if (integer_zerop (_q21))
									    {
									      switch (TREE_CODE (_p1))
									        {
										case BIT_XOR_EXPR:
										  {
										    tree _q170 = TREE_OPERAND (_p1, 0);
										    tree _q171 = TREE_OPERAND (_p1, 1);
										    switch (TREE_CODE (_q170))
										      {
										      case NEGATE_EXPR:
										        {
											  tree _q180 = TREE_OPERAND (_q170, 0);
											  switch (TREE_CODE (_q180))
											    {
											    CASE_CONVERT:
											      {
												tree _q190 = TREE_OPERAND (_q180, 0);
												switch (TREE_CODE (_q190))
												  {
												  case LT_EXPR:
												    {
												      tree _q200 = TREE_OPERAND (_q190, 0);
												      tree _q201 = TREE_OPERAND (_q190, 1);
												      if ((_q200 == _q40 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q40, 0) && types_match (_q200, _q40)))
													{
													  if (integer_zerop (_q201))
													    {
													      if (tree_max_value (_q171))
														{
														  if ((_p2 == _q41 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q41, 0) && types_match (_p2, _q41)))
														    {
														      {
															tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
															if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															  {
															    {
															      res_ops[0] = captures[0];
															      res_ops[1] = captures[1];
															      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 56, __FILE__, __LINE__, false);
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
										  if (tree_max_value (_q170))
										    {
										      switch (TREE_CODE (_q171))
										        {
											case NEGATE_EXPR:
											  {
											    tree _q190 = TREE_OPERAND (_q171, 0);
											    switch (TREE_CODE (_q190))
											      {
											      CASE_CONVERT:
											        {
												  tree _q200 = TREE_OPERAND (_q190, 0);
												  switch (TREE_CODE (_q200))
												    {
												    case LT_EXPR:
												      {
													tree _q210 = TREE_OPERAND (_q200, 0);
													tree _q211 = TREE_OPERAND (_q200, 1);
													if ((_q210 == _q40 && ! TREE_SIDE_EFFECTS (_q210)) || (operand_equal_p (_q210, _q40, 0) && types_match (_q210, _q40)))
													  {
													    if (integer_zerop (_q211))
													      {
														if ((_p2 == _q41 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q41, 0) && types_match (_p2, _q41)))
														  {
														    {
														      tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
														      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															{
															  {
															    res_ops[0] = captures[0];
															    res_ops[1] = captures[1];
															    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 56, __FILE__, __LINE__, false);
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
											    break;
											  }
										        default:;
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
							    break;
							  }
						        default:;
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
			  {
			    tree _q40_pops[1];
			    if (tree_nop_convert (_q40, _q40_pops))
			      {
				tree _q50 = _q40_pops[0];
				switch (TREE_CODE (_q50))
				  {
				  case PLUS_EXPR:
				    {
				      tree _q60 = TREE_OPERAND (_q50, 0);
				      tree _q61 = TREE_OPERAND (_q50, 1);
				      {
					tree _q60_pops[1];
					if (tree_nop_convert (_q60, _q60_pops))
					  {
					    tree _q70 = _q60_pops[0];
					    {
					      tree _q61_pops[1];
					      if (tree_nop_convert (_q61, _q61_pops))
					        {
						  tree _q90 = _q61_pops[0];
						  if ((_q41 == _q70 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _q70, 0) && types_match (_q41, _q70)))
						    {
						      switch (TREE_CODE (_q31))
						        {
							case BIT_NOT_EXPR:
							  {
							    tree _q120 = TREE_OPERAND (_q31, 0);
							    switch (TREE_CODE (_q120))
							      {
							      case BIT_XOR_EXPR:
							        {
								  tree _q130 = TREE_OPERAND (_q120, 0);
								  tree _q131 = TREE_OPERAND (_q120, 1);
								  if ((_q130 == _q70 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q70, 0) && types_match (_q130, _q70)))
								    {
								      if ((_q131 == _q90 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q90, 0) && types_match (_q131, _q90)))
									{
									  if (integer_zerop (_q21))
									    {
									      switch (TREE_CODE (_p1))
									        {
										case BIT_XOR_EXPR:
										  {
										    tree _q170 = TREE_OPERAND (_p1, 0);
										    tree _q171 = TREE_OPERAND (_p1, 1);
										    switch (TREE_CODE (_q170))
										      {
										      case NEGATE_EXPR:
										        {
											  tree _q180 = TREE_OPERAND (_q170, 0);
											  switch (TREE_CODE (_q180))
											    {
											    CASE_CONVERT:
											      {
												tree _q190 = TREE_OPERAND (_q180, 0);
												switch (TREE_CODE (_q190))
												  {
												  case LT_EXPR:
												    {
												      tree _q200 = TREE_OPERAND (_q190, 0);
												      tree _q201 = TREE_OPERAND (_q190, 1);
												      if ((_q200 == _q70 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q70, 0) && types_match (_q200, _q70)))
													{
													  if (integer_zerop (_q201))
													    {
													      if (tree_max_value (_q171))
														{
														  if ((_p2 == _q40 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q40, 0) && types_match (_p2, _q40)))
														    {
														      {
															tree captures[3] ATTRIBUTE_UNUSED = { _q70, _q90, _q40 };
															if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															  {
															    {
															      res_ops[0] = captures[0];
															      res_ops[1] = captures[1];
															      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 56, __FILE__, __LINE__, false);
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
										  if (tree_max_value (_q170))
										    {
										      switch (TREE_CODE (_q171))
										        {
											case NEGATE_EXPR:
											  {
											    tree _q190 = TREE_OPERAND (_q171, 0);
											    switch (TREE_CODE (_q190))
											      {
											      CASE_CONVERT:
											        {
												  tree _q200 = TREE_OPERAND (_q190, 0);
												  switch (TREE_CODE (_q200))
												    {
												    case LT_EXPR:
												      {
													tree _q210 = TREE_OPERAND (_q200, 0);
													tree _q211 = TREE_OPERAND (_q200, 1);
													if ((_q210 == _q70 && ! TREE_SIDE_EFFECTS (_q210)) || (operand_equal_p (_q210, _q70, 0) && types_match (_q210, _q70)))
													  {
													    if (integer_zerop (_q211))
													      {
														if ((_p2 == _q40 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q40, 0) && types_match (_p2, _q40)))
														  {
														    {
														      tree captures[3] ATTRIBUTE_UNUSED = { _q70, _q90, _q40 };
														      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															{
															  {
															    res_ops[0] = captures[0];
															    res_ops[1] = captures[1];
															    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 56, __FILE__, __LINE__, false);
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
											    break;
											  }
										        default:;
										        }
										    }
										    break;
										  }
									        default:;
									        }
									    }
								        }
								    }
								  if ((_q130 == _q90 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q90, 0) && types_match (_q130, _q90)))
								    {
								      if ((_q131 == _q70 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q70, 0) && types_match (_q131, _q70)))
									{
									  if (integer_zerop (_q21))
									    {
									      switch (TREE_CODE (_p1))
									        {
										case BIT_XOR_EXPR:
										  {
										    tree _q170 = TREE_OPERAND (_p1, 0);
										    tree _q171 = TREE_OPERAND (_p1, 1);
										    switch (TREE_CODE (_q170))
										      {
										      case NEGATE_EXPR:
										        {
											  tree _q180 = TREE_OPERAND (_q170, 0);
											  switch (TREE_CODE (_q180))
											    {
											    CASE_CONVERT:
											      {
												tree _q190 = TREE_OPERAND (_q180, 0);
												switch (TREE_CODE (_q190))
												  {
												  case LT_EXPR:
												    {
												      tree _q200 = TREE_OPERAND (_q190, 0);
												      tree _q201 = TREE_OPERAND (_q190, 1);
												      if ((_q200 == _q70 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q70, 0) && types_match (_q200, _q70)))
													{
													  if (integer_zerop (_q201))
													    {
													      if (tree_max_value (_q171))
														{
														  if ((_p2 == _q40 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q40, 0) && types_match (_p2, _q40)))
														    {
														      {
															tree captures[3] ATTRIBUTE_UNUSED = { _q70, _q90, _q40 };
															if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															  {
															    {
															      res_ops[0] = captures[0];
															      res_ops[1] = captures[1];
															      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 56, __FILE__, __LINE__, false);
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
										  if (tree_max_value (_q170))
										    {
										      switch (TREE_CODE (_q171))
										        {
											case NEGATE_EXPR:
											  {
											    tree _q190 = TREE_OPERAND (_q171, 0);
											    switch (TREE_CODE (_q190))
											      {
											      CASE_CONVERT:
											        {
												  tree _q200 = TREE_OPERAND (_q190, 0);
												  switch (TREE_CODE (_q200))
												    {
												    case LT_EXPR:
												      {
													tree _q210 = TREE_OPERAND (_q200, 0);
													tree _q211 = TREE_OPERAND (_q200, 1);
													if ((_q210 == _q70 && ! TREE_SIDE_EFFECTS (_q210)) || (operand_equal_p (_q210, _q70, 0) && types_match (_q210, _q70)))
													  {
													    if (integer_zerop (_q211))
													      {
														if ((_p2 == _q40 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q40, 0) && types_match (_p2, _q40)))
														  {
														    {
														      tree captures[3] ATTRIBUTE_UNUSED = { _q70, _q90, _q40 };
														      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															{
															  {
															    res_ops[0] = captures[0];
															    res_ops[1] = captures[1];
															    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 56, __FILE__, __LINE__, false);
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
											    break;
											  }
										        default:;
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
							    break;
							  }
						        default:;
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
		      case BIT_NOT_EXPR:
		        {
			  tree _q40 = TREE_OPERAND (_q30, 0);
			  switch (TREE_CODE (_q40))
			    {
			    case BIT_XOR_EXPR:
			      {
				tree _q50 = TREE_OPERAND (_q40, 0);
				tree _q51 = TREE_OPERAND (_q40, 1);
				switch (TREE_CODE (_q31))
				  {
				  case BIT_XOR_EXPR:
				    {
				      tree _q80 = TREE_OPERAND (_q31, 0);
				      tree _q81 = TREE_OPERAND (_q31, 1);
				      if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
					{
					  {
					    tree _q81_pops[1];
					    if (tree_nop_convert (_q81, _q81_pops))
					      {
						tree _q100 = _q81_pops[0];
						switch (TREE_CODE (_q100))
						  {
						  case PLUS_EXPR:
						    {
						      tree _q110 = TREE_OPERAND (_q100, 0);
						      tree _q111 = TREE_OPERAND (_q100, 1);
						      {
							tree _q110_pops[1];
							if (tree_nop_convert (_q110, _q110_pops))
							  {
							    tree _q120 = _q110_pops[0];
							    if ((_q120 == _q50 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q50, 0) && types_match (_q120, _q50)))
							      {
								{
								  tree _q111_pops[1];
								  if (tree_nop_convert (_q111, _q111_pops))
								    {
								      tree _q140 = _q111_pops[0];
								      if ((_q140 == _q51 && ! TREE_SIDE_EFFECTS (_q140)) || (operand_equal_p (_q140, _q51, 0) && types_match (_q140, _q51)))
									{
									  if (integer_zerop (_q21))
									    {
									      switch (TREE_CODE (_p1))
									        {
										case BIT_XOR_EXPR:
										  {
										    tree _q170 = TREE_OPERAND (_p1, 0);
										    tree _q171 = TREE_OPERAND (_p1, 1);
										    switch (TREE_CODE (_q170))
										      {
										      case NEGATE_EXPR:
										        {
											  tree _q180 = TREE_OPERAND (_q170, 0);
											  switch (TREE_CODE (_q180))
											    {
											    CASE_CONVERT:
											      {
												tree _q190 = TREE_OPERAND (_q180, 0);
												switch (TREE_CODE (_q190))
												  {
												  case LT_EXPR:
												    {
												      tree _q200 = TREE_OPERAND (_q190, 0);
												      tree _q201 = TREE_OPERAND (_q190, 1);
												      if ((_q200 == _q50 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q50, 0) && types_match (_q200, _q50)))
													{
													  if (integer_zerop (_q201))
													    {
													      if (tree_max_value (_q171))
														{
														  if ((_p2 == _q81 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q81, 0) && types_match (_p2, _q81)))
														    {
														      {
															tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q51, _q81 };
															if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															  {
															    {
															      res_ops[0] = captures[0];
															      res_ops[1] = captures[1];
															      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 56, __FILE__, __LINE__, false);
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
										  if (tree_max_value (_q170))
										    {
										      switch (TREE_CODE (_q171))
										        {
											case NEGATE_EXPR:
											  {
											    tree _q190 = TREE_OPERAND (_q171, 0);
											    switch (TREE_CODE (_q190))
											      {
											      CASE_CONVERT:
											        {
												  tree _q200 = TREE_OPERAND (_q190, 0);
												  switch (TREE_CODE (_q200))
												    {
												    case LT_EXPR:
												      {
													tree _q210 = TREE_OPERAND (_q200, 0);
													tree _q211 = TREE_OPERAND (_q200, 1);
													if ((_q210 == _q50 && ! TREE_SIDE_EFFECTS (_q210)) || (operand_equal_p (_q210, _q50, 0) && types_match (_q210, _q50)))
													  {
													    if (integer_zerop (_q211))
													      {
														if ((_p2 == _q81 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q81, 0) && types_match (_p2, _q81)))
														  {
														    {
														      tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q51, _q81 };
														      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															{
															  {
															    res_ops[0] = captures[0];
															    res_ops[1] = captures[1];
															    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 56, __FILE__, __LINE__, false);
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
											    break;
											  }
										        default:;
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
				      if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
					{
					  {
					    tree _q81_pops[1];
					    if (tree_nop_convert (_q81, _q81_pops))
					      {
						tree _q100 = _q81_pops[0];
						switch (TREE_CODE (_q100))
						  {
						  case PLUS_EXPR:
						    {
						      tree _q110 = TREE_OPERAND (_q100, 0);
						      tree _q111 = TREE_OPERAND (_q100, 1);
						      {
							tree _q110_pops[1];
							if (tree_nop_convert (_q110, _q110_pops))
							  {
							    tree _q120 = _q110_pops[0];
							    if ((_q120 == _q51 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q51, 0) && types_match (_q120, _q51)))
							      {
								{
								  tree _q111_pops[1];
								  if (tree_nop_convert (_q111, _q111_pops))
								    {
								      tree _q140 = _q111_pops[0];
								      if ((_q140 == _q50 && ! TREE_SIDE_EFFECTS (_q140)) || (operand_equal_p (_q140, _q50, 0) && types_match (_q140, _q50)))
									{
									  if (integer_zerop (_q21))
									    {
									      switch (TREE_CODE (_p1))
									        {
										case BIT_XOR_EXPR:
										  {
										    tree _q170 = TREE_OPERAND (_p1, 0);
										    tree _q171 = TREE_OPERAND (_p1, 1);
										    switch (TREE_CODE (_q170))
										      {
										      case NEGATE_EXPR:
										        {
											  tree _q180 = TREE_OPERAND (_q170, 0);
											  switch (TREE_CODE (_q180))
											    {
											    CASE_CONVERT:
											      {
												tree _q190 = TREE_OPERAND (_q180, 0);
												switch (TREE_CODE (_q190))
												  {
												  case LT_EXPR:
												    {
												      tree _q200 = TREE_OPERAND (_q190, 0);
												      tree _q201 = TREE_OPERAND (_q190, 1);
												      if ((_q200 == _q51 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q51, 0) && types_match (_q200, _q51)))
													{
													  if (integer_zerop (_q201))
													    {
													      if (tree_max_value (_q171))
														{
														  if ((_p2 == _q81 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q81, 0) && types_match (_p2, _q81)))
														    {
														      {
															tree captures[3] ATTRIBUTE_UNUSED = { _q51, _q50, _q81 };
															if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															  {
															    {
															      res_ops[0] = captures[0];
															      res_ops[1] = captures[1];
															      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 56, __FILE__, __LINE__, false);
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
										  if (tree_max_value (_q170))
										    {
										      switch (TREE_CODE (_q171))
										        {
											case NEGATE_EXPR:
											  {
											    tree _q190 = TREE_OPERAND (_q171, 0);
											    switch (TREE_CODE (_q190))
											      {
											      CASE_CONVERT:
											        {
												  tree _q200 = TREE_OPERAND (_q190, 0);
												  switch (TREE_CODE (_q200))
												    {
												    case LT_EXPR:
												      {
													tree _q210 = TREE_OPERAND (_q200, 0);
													tree _q211 = TREE_OPERAND (_q200, 1);
													if ((_q210 == _q51 && ! TREE_SIDE_EFFECTS (_q210)) || (operand_equal_p (_q210, _q51, 0) && types_match (_q210, _q51)))
													  {
													    if (integer_zerop (_q211))
													      {
														if ((_p2 == _q81 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q81, 0) && types_match (_p2, _q81)))
														  {
														    {
														      tree captures[3] ATTRIBUTE_UNUSED = { _q51, _q50, _q81 };
														      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															{
															  {
															    res_ops[0] = captures[0];
															    res_ops[1] = captures[1];
															    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 56, __FILE__, __LINE__, false);
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
											    break;
											  }
										        default:;
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
				      {
					tree _q80_pops[1];
					if (tree_nop_convert (_q80, _q80_pops))
					  {
					    tree _q90 = _q80_pops[0];
					    switch (TREE_CODE (_q90))
					      {
					      case PLUS_EXPR:
					        {
						  tree _q100 = TREE_OPERAND (_q90, 0);
						  tree _q101 = TREE_OPERAND (_q90, 1);
						  {
						    tree _q100_pops[1];
						    if (tree_nop_convert (_q100, _q100_pops))
						      {
							tree _q110 = _q100_pops[0];
							if ((_q110 == _q50 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q50, 0) && types_match (_q110, _q50)))
							  {
							    {
							      tree _q101_pops[1];
							      if (tree_nop_convert (_q101, _q101_pops))
							        {
								  tree _q130 = _q101_pops[0];
								  if ((_q130 == _q51 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q51, 0) && types_match (_q130, _q51)))
								    {
								      if ((_q81 == _q50 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q50, 0) && types_match (_q81, _q50)))
									{
									  if (integer_zerop (_q21))
									    {
									      switch (TREE_CODE (_p1))
									        {
										case BIT_XOR_EXPR:
										  {
										    tree _q170 = TREE_OPERAND (_p1, 0);
										    tree _q171 = TREE_OPERAND (_p1, 1);
										    switch (TREE_CODE (_q170))
										      {
										      case NEGATE_EXPR:
										        {
											  tree _q180 = TREE_OPERAND (_q170, 0);
											  switch (TREE_CODE (_q180))
											    {
											    CASE_CONVERT:
											      {
												tree _q190 = TREE_OPERAND (_q180, 0);
												switch (TREE_CODE (_q190))
												  {
												  case LT_EXPR:
												    {
												      tree _q200 = TREE_OPERAND (_q190, 0);
												      tree _q201 = TREE_OPERAND (_q190, 1);
												      if ((_q200 == _q50 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q50, 0) && types_match (_q200, _q50)))
													{
													  if (integer_zerop (_q201))
													    {
													      if (tree_max_value (_q171))
														{
														  if ((_p2 == _q80 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q80, 0) && types_match (_p2, _q80)))
														    {
														      {
															tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q51, _q80 };
															if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															  {
															    {
															      res_ops[0] = captures[0];
															      res_ops[1] = captures[1];
															      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 56, __FILE__, __LINE__, false);
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
										  if (tree_max_value (_q170))
										    {
										      switch (TREE_CODE (_q171))
										        {
											case NEGATE_EXPR:
											  {
											    tree _q190 = TREE_OPERAND (_q171, 0);
											    switch (TREE_CODE (_q190))
											      {
											      CASE_CONVERT:
											        {
												  tree _q200 = TREE_OPERAND (_q190, 0);
												  switch (TREE_CODE (_q200))
												    {
												    case LT_EXPR:
												      {
													tree _q210 = TREE_OPERAND (_q200, 0);
													tree _q211 = TREE_OPERAND (_q200, 1);
													if ((_q210 == _q50 && ! TREE_SIDE_EFFECTS (_q210)) || (operand_equal_p (_q210, _q50, 0) && types_match (_q210, _q50)))
													  {
													    if (integer_zerop (_q211))
													      {
														if ((_p2 == _q80 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q80, 0) && types_match (_p2, _q80)))
														  {
														    {
														      tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q51, _q80 };
														      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															{
															  {
															    res_ops[0] = captures[0];
															    res_ops[1] = captures[1];
															    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 56, __FILE__, __LINE__, false);
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
											    break;
											  }
										        default:;
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
							    }
							  }
							if ((_q110 == _q51 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q51, 0) && types_match (_q110, _q51)))
							  {
							    {
							      tree _q101_pops[1];
							      if (tree_nop_convert (_q101, _q101_pops))
							        {
								  tree _q130 = _q101_pops[0];
								  if ((_q130 == _q50 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q50, 0) && types_match (_q130, _q50)))
								    {
								      if ((_q81 == _q51 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q51, 0) && types_match (_q81, _q51)))
									{
									  if (integer_zerop (_q21))
									    {
									      switch (TREE_CODE (_p1))
									        {
										case BIT_XOR_EXPR:
										  {
										    tree _q170 = TREE_OPERAND (_p1, 0);
										    tree _q171 = TREE_OPERAND (_p1, 1);
										    switch (TREE_CODE (_q170))
										      {
										      case NEGATE_EXPR:
										        {
											  tree _q180 = TREE_OPERAND (_q170, 0);
											  switch (TREE_CODE (_q180))
											    {
											    CASE_CONVERT:
											      {
												tree _q190 = TREE_OPERAND (_q180, 0);
												switch (TREE_CODE (_q190))
												  {
												  case LT_EXPR:
												    {
												      tree _q200 = TREE_OPERAND (_q190, 0);
												      tree _q201 = TREE_OPERAND (_q190, 1);
												      if ((_q200 == _q51 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q51, 0) && types_match (_q200, _q51)))
													{
													  if (integer_zerop (_q201))
													    {
													      if (tree_max_value (_q171))
														{
														  if ((_p2 == _q80 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q80, 0) && types_match (_p2, _q80)))
														    {
														      {
															tree captures[3] ATTRIBUTE_UNUSED = { _q51, _q50, _q80 };
															if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															  {
															    {
															      res_ops[0] = captures[0];
															      res_ops[1] = captures[1];
															      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 56, __FILE__, __LINE__, false);
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
										  if (tree_max_value (_q170))
										    {
										      switch (TREE_CODE (_q171))
										        {
											case NEGATE_EXPR:
											  {
											    tree _q190 = TREE_OPERAND (_q171, 0);
											    switch (TREE_CODE (_q190))
											      {
											      CASE_CONVERT:
											        {
												  tree _q200 = TREE_OPERAND (_q190, 0);
												  switch (TREE_CODE (_q200))
												    {
												    case LT_EXPR:
												      {
													tree _q210 = TREE_OPERAND (_q200, 0);
													tree _q211 = TREE_OPERAND (_q200, 1);
													if ((_q210 == _q51 && ! TREE_SIDE_EFFECTS (_q210)) || (operand_equal_p (_q210, _q51, 0) && types_match (_q210, _q51)))
													  {
													    if (integer_zerop (_q211))
													      {
														if ((_p2 == _q80 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q80, 0) && types_match (_p2, _q80)))
														  {
														    {
														      tree captures[3] ATTRIBUTE_UNUSED = { _q51, _q50, _q80 };
														      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															{
															  {
															    res_ops[0] = captures[0];
															    res_ops[1] = captures[1];
															    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 56, __FILE__, __LINE__, false);
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
											    break;
											  }
										        default:;
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
	  case GE_EXPR:
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
		      case BIT_XOR_EXPR:
		        {
			  tree _q40 = TREE_OPERAND (_q30, 0);
			  tree _q41 = TREE_OPERAND (_q30, 1);
			  {
			    tree _q41_pops[1];
			    if (tree_nop_convert (_q41, _q41_pops))
			      {
				tree _q60 = _q41_pops[0];
				switch (TREE_CODE (_q60))
				  {
				  case PLUS_EXPR:
				    {
				      tree _q70 = TREE_OPERAND (_q60, 0);
				      tree _q71 = TREE_OPERAND (_q60, 1);
				      {
					tree _q70_pops[1];
					if (tree_nop_convert (_q70, _q70_pops))
					  {
					    tree _q80 = _q70_pops[0];
					    if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
					      {
						{
						  tree _q71_pops[1];
						  if (tree_nop_convert (_q71, _q71_pops))
						    {
						      tree _q100 = _q71_pops[0];
						      switch (TREE_CODE (_q31))
						        {
							case BIT_NOT_EXPR:
							  {
							    tree _q120 = TREE_OPERAND (_q31, 0);
							    switch (TREE_CODE (_q120))
							      {
							      case BIT_XOR_EXPR:
							        {
								  tree _q130 = TREE_OPERAND (_q120, 0);
								  tree _q131 = TREE_OPERAND (_q120, 1);
								  if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
								    {
								      if ((_q131 == _q100 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q100, 0) && types_match (_q131, _q100)))
									{
									  if (integer_zerop (_q21))
									    {
									      if ((_p1 == _q41 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q41, 0) && types_match (_p1, _q41)))
										{
										  switch (TREE_CODE (_p2))
										    {
										    case BIT_XOR_EXPR:
										      {
											tree _q180 = TREE_OPERAND (_p2, 0);
											tree _q181 = TREE_OPERAND (_p2, 1);
											switch (TREE_CODE (_q180))
											  {
											  case NEGATE_EXPR:
											    {
											      tree _q190 = TREE_OPERAND (_q180, 0);
											      switch (TREE_CODE (_q190))
											        {
												CASE_CONVERT:
												  {
												    tree _q200 = TREE_OPERAND (_q190, 0);
												    switch (TREE_CODE (_q200))
												      {
												      case LT_EXPR:
												        {
													  tree _q210 = TREE_OPERAND (_q200, 0);
													  tree _q211 = TREE_OPERAND (_q200, 1);
													  if ((_q210 == _q40 && ! TREE_SIDE_EFFECTS (_q210)) || (operand_equal_p (_q210, _q40, 0) && types_match (_q210, _q40)))
													    {
													      if (integer_zerop (_q211))
														{
														  if (tree_max_value (_q181))
														    {
														      {
															tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
															if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															  {
															    {
															      res_ops[0] = captures[0];
															      res_ops[1] = captures[1];
															      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 57, __FILE__, __LINE__, false);
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
											      break;
											    }
										          default:;
										          }
										      if (tree_max_value (_q180))
											{
											  switch (TREE_CODE (_q181))
											    {
											    case NEGATE_EXPR:
											      {
												tree _q200 = TREE_OPERAND (_q181, 0);
												switch (TREE_CODE (_q200))
												  {
												  CASE_CONVERT:
												    {
												      tree _q210 = TREE_OPERAND (_q200, 0);
												      switch (TREE_CODE (_q210))
												        {
													case LT_EXPR:
													  {
													    tree _q220 = TREE_OPERAND (_q210, 0);
													    tree _q221 = TREE_OPERAND (_q210, 1);
													    if ((_q220 == _q40 && ! TREE_SIDE_EFFECTS (_q220)) || (operand_equal_p (_q220, _q40, 0) && types_match (_q220, _q40)))
													      {
														if (integer_zerop (_q221))
														  {
														    {
														      tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
														      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															{
															  {
															    res_ops[0] = captures[0];
															    res_ops[1] = captures[1];
															    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 57, __FILE__, __LINE__, false);
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
										        }
										        break;
										      }
										    default:;
										    }
									        }
									    }
								        }
								    }
								  if ((_q130 == _q100 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q100, 0) && types_match (_q130, _q100)))
								    {
								      if ((_q131 == _q40 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q40, 0) && types_match (_q131, _q40)))
									{
									  if (integer_zerop (_q21))
									    {
									      if ((_p1 == _q41 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q41, 0) && types_match (_p1, _q41)))
										{
										  switch (TREE_CODE (_p2))
										    {
										    case BIT_XOR_EXPR:
										      {
											tree _q180 = TREE_OPERAND (_p2, 0);
											tree _q181 = TREE_OPERAND (_p2, 1);
											switch (TREE_CODE (_q180))
											  {
											  case NEGATE_EXPR:
											    {
											      tree _q190 = TREE_OPERAND (_q180, 0);
											      switch (TREE_CODE (_q190))
											        {
												CASE_CONVERT:
												  {
												    tree _q200 = TREE_OPERAND (_q190, 0);
												    switch (TREE_CODE (_q200))
												      {
												      case LT_EXPR:
												        {
													  tree _q210 = TREE_OPERAND (_q200, 0);
													  tree _q211 = TREE_OPERAND (_q200, 1);
													  if ((_q210 == _q40 && ! TREE_SIDE_EFFECTS (_q210)) || (operand_equal_p (_q210, _q40, 0) && types_match (_q210, _q40)))
													    {
													      if (integer_zerop (_q211))
														{
														  if (tree_max_value (_q181))
														    {
														      {
															tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
															if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															  {
															    {
															      res_ops[0] = captures[0];
															      res_ops[1] = captures[1];
															      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 57, __FILE__, __LINE__, false);
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
											      break;
											    }
										          default:;
										          }
										      if (tree_max_value (_q180))
											{
											  switch (TREE_CODE (_q181))
											    {
											    case NEGATE_EXPR:
											      {
												tree _q200 = TREE_OPERAND (_q181, 0);
												switch (TREE_CODE (_q200))
												  {
												  CASE_CONVERT:
												    {
												      tree _q210 = TREE_OPERAND (_q200, 0);
												      switch (TREE_CODE (_q210))
												        {
													case LT_EXPR:
													  {
													    tree _q220 = TREE_OPERAND (_q210, 0);
													    tree _q221 = TREE_OPERAND (_q210, 1);
													    if ((_q220 == _q40 && ! TREE_SIDE_EFFECTS (_q220)) || (operand_equal_p (_q220, _q40, 0) && types_match (_q220, _q40)))
													      {
														if (integer_zerop (_q221))
														  {
														    {
														      tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
														      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															{
															  {
															    res_ops[0] = captures[0];
															    res_ops[1] = captures[1];
															    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 57, __FILE__, __LINE__, false);
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
										        }
										        break;
										      }
										    default:;
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
			  tree _q40 = TREE_OPERAND (_q30, 0);
			  switch (TREE_CODE (_q40))
			    {
			    case BIT_XOR_EXPR:
			      {
				tree _q50 = TREE_OPERAND (_q40, 0);
				tree _q51 = TREE_OPERAND (_q40, 1);
				switch (TREE_CODE (_q31))
				  {
				  case BIT_XOR_EXPR:
				    {
				      tree _q80 = TREE_OPERAND (_q31, 0);
				      tree _q81 = TREE_OPERAND (_q31, 1);
				      if ((_q80 == _q50 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q50, 0) && types_match (_q80, _q50)))
					{
					  {
					    tree _q81_pops[1];
					    if (tree_nop_convert (_q81, _q81_pops))
					      {
						tree _q100 = _q81_pops[0];
						switch (TREE_CODE (_q100))
						  {
						  case PLUS_EXPR:
						    {
						      tree _q110 = TREE_OPERAND (_q100, 0);
						      tree _q111 = TREE_OPERAND (_q100, 1);
						      {
							tree _q110_pops[1];
							if (tree_nop_convert (_q110, _q110_pops))
							  {
							    tree _q120 = _q110_pops[0];
							    if ((_q120 == _q50 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q50, 0) && types_match (_q120, _q50)))
							      {
								{
								  tree _q111_pops[1];
								  if (tree_nop_convert (_q111, _q111_pops))
								    {
								      tree _q140 = _q111_pops[0];
								      if ((_q140 == _q51 && ! TREE_SIDE_EFFECTS (_q140)) || (operand_equal_p (_q140, _q51, 0) && types_match (_q140, _q51)))
									{
									  if (integer_zerop (_q21))
									    {
									      if ((_p1 == _q81 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q81, 0) && types_match (_p1, _q81)))
										{
										  switch (TREE_CODE (_p2))
										    {
										    case BIT_XOR_EXPR:
										      {
											tree _q180 = TREE_OPERAND (_p2, 0);
											tree _q181 = TREE_OPERAND (_p2, 1);
											switch (TREE_CODE (_q180))
											  {
											  case NEGATE_EXPR:
											    {
											      tree _q190 = TREE_OPERAND (_q180, 0);
											      switch (TREE_CODE (_q190))
											        {
												CASE_CONVERT:
												  {
												    tree _q200 = TREE_OPERAND (_q190, 0);
												    switch (TREE_CODE (_q200))
												      {
												      case LT_EXPR:
												        {
													  tree _q210 = TREE_OPERAND (_q200, 0);
													  tree _q211 = TREE_OPERAND (_q200, 1);
													  if ((_q210 == _q50 && ! TREE_SIDE_EFFECTS (_q210)) || (operand_equal_p (_q210, _q50, 0) && types_match (_q210, _q50)))
													    {
													      if (integer_zerop (_q211))
														{
														  if (tree_max_value (_q181))
														    {
														      {
															tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q51, _q81 };
															if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															  {
															    {
															      res_ops[0] = captures[0];
															      res_ops[1] = captures[1];
															      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 57, __FILE__, __LINE__, false);
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
											      break;
											    }
										          default:;
										          }
										      if (tree_max_value (_q180))
											{
											  switch (TREE_CODE (_q181))
											    {
											    case NEGATE_EXPR:
											      {
												tree _q200 = TREE_OPERAND (_q181, 0);
												switch (TREE_CODE (_q200))
												  {
												  CASE_CONVERT:
												    {
												      tree _q210 = TREE_OPERAND (_q200, 0);
												      switch (TREE_CODE (_q210))
												        {
													case LT_EXPR:
													  {
													    tree _q220 = TREE_OPERAND (_q210, 0);
													    tree _q221 = TREE_OPERAND (_q210, 1);
													    if ((_q220 == _q50 && ! TREE_SIDE_EFFECTS (_q220)) || (operand_equal_p (_q220, _q50, 0) && types_match (_q220, _q50)))
													      {
														if (integer_zerop (_q221))
														  {
														    {
														      tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q51, _q81 };
														      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															{
															  {
															    res_ops[0] = captures[0];
															    res_ops[1] = captures[1];
															    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 57, __FILE__, __LINE__, false);
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
										        }
										        break;
										      }
										    default:;
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
					      }
					  }
				        }
				      if ((_q80 == _q51 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q51, 0) && types_match (_q80, _q51)))
					{
					  {
					    tree _q81_pops[1];
					    if (tree_nop_convert (_q81, _q81_pops))
					      {
						tree _q100 = _q81_pops[0];
						switch (TREE_CODE (_q100))
						  {
						  case PLUS_EXPR:
						    {
						      tree _q110 = TREE_OPERAND (_q100, 0);
						      tree _q111 = TREE_OPERAND (_q100, 1);
						      {
							tree _q110_pops[1];
							if (tree_nop_convert (_q110, _q110_pops))
							  {
							    tree _q120 = _q110_pops[0];
							    if ((_q120 == _q51 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q51, 0) && types_match (_q120, _q51)))
							      {
								{
								  tree _q111_pops[1];
								  if (tree_nop_convert (_q111, _q111_pops))
								    {
								      tree _q140 = _q111_pops[0];
								      if ((_q140 == _q50 && ! TREE_SIDE_EFFECTS (_q140)) || (operand_equal_p (_q140, _q50, 0) && types_match (_q140, _q50)))
									{
									  if (integer_zerop (_q21))
									    {
									      if ((_p1 == _q81 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q81, 0) && types_match (_p1, _q81)))
										{
										  switch (TREE_CODE (_p2))
										    {
										    case BIT_XOR_EXPR:
										      {
											tree _q180 = TREE_OPERAND (_p2, 0);
											tree _q181 = TREE_OPERAND (_p2, 1);
											switch (TREE_CODE (_q180))
											  {
											  case NEGATE_EXPR:
											    {
											      tree _q190 = TREE_OPERAND (_q180, 0);
											      switch (TREE_CODE (_q190))
											        {
												CASE_CONVERT:
												  {
												    tree _q200 = TREE_OPERAND (_q190, 0);
												    switch (TREE_CODE (_q200))
												      {
												      case LT_EXPR:
												        {
													  tree _q210 = TREE_OPERAND (_q200, 0);
													  tree _q211 = TREE_OPERAND (_q200, 1);
													  if ((_q210 == _q51 && ! TREE_SIDE_EFFECTS (_q210)) || (operand_equal_p (_q210, _q51, 0) && types_match (_q210, _q51)))
													    {
													      if (integer_zerop (_q211))
														{
														  if (tree_max_value (_q181))
														    {
														      {
															tree captures[3] ATTRIBUTE_UNUSED = { _q51, _q50, _q81 };
															if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															  {
															    {
															      res_ops[0] = captures[0];
															      res_ops[1] = captures[1];
															      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 57, __FILE__, __LINE__, false);
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
											      break;
											    }
										          default:;
										          }
										      if (tree_max_value (_q180))
											{
											  switch (TREE_CODE (_q181))
											    {
											    case NEGATE_EXPR:
											      {
												tree _q200 = TREE_OPERAND (_q181, 0);
												switch (TREE_CODE (_q200))
												  {
												  CASE_CONVERT:
												    {
												      tree _q210 = TREE_OPERAND (_q200, 0);
												      switch (TREE_CODE (_q210))
												        {
													case LT_EXPR:
													  {
													    tree _q220 = TREE_OPERAND (_q210, 0);
													    tree _q221 = TREE_OPERAND (_q210, 1);
													    if ((_q220 == _q51 && ! TREE_SIDE_EFFECTS (_q220)) || (operand_equal_p (_q220, _q51, 0) && types_match (_q220, _q51)))
													      {
														if (integer_zerop (_q221))
														  {
														    {
														      tree captures[3] ATTRIBUTE_UNUSED = { _q51, _q50, _q81 };
														      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
															{
															  {
															    res_ops[0] = captures[0];
															    res_ops[1] = captures[1];
															    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 57, __FILE__, __LINE__, false);
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
										        }
										        break;
										      }
										    default:;
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
	  case BIT_AND_EXPR:
	    {
	      tree _q20 = TREE_OPERAND (_p0, 0);
	      tree _q21 = TREE_OPERAND (_p0, 1);
	      switch (TREE_CODE (_q20))
	        {
		case LT_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    switch (TREE_CODE (_q30))
		      {
		      case BIT_XOR_EXPR:
		        {
			  tree _q40 = TREE_OPERAND (_q30, 0);
			  tree _q41 = TREE_OPERAND (_q30, 1);
			  {
			    tree _q41_pops[1];
			    if (tree_nop_convert (_q41, _q41_pops))
			      {
				tree _q60 = _q41_pops[0];
				switch (TREE_CODE (_q60))
				  {
				  case PLUS_EXPR:
				    {
				      tree _q70 = TREE_OPERAND (_q60, 0);
				      tree _q71 = TREE_OPERAND (_q60, 1);
				      {
					tree _q70_pops[1];
					if (tree_nop_convert (_q70, _q70_pops))
					  {
					    tree _q80 = _q70_pops[0];
					    if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
					      {
						{
						  tree _q71_pops[1];
						  if (tree_nop_convert (_q71, _q71_pops))
						    {
						      tree _q100 = _q71_pops[0];
						      if (integer_zerop (_q31))
							{
							  switch (TREE_CODE (_q21))
							    {
							    case GE_EXPR:
							      {
								tree _q130 = TREE_OPERAND (_q21, 0);
								tree _q131 = TREE_OPERAND (_q21, 1);
								switch (TREE_CODE (_q130))
								  {
								  case BIT_XOR_EXPR:
								    {
								      tree _q140 = TREE_OPERAND (_q130, 0);
								      tree _q141 = TREE_OPERAND (_q130, 1);
								      if ((_q140 == _q40 && ! TREE_SIDE_EFFECTS (_q140)) || (operand_equal_p (_q140, _q40, 0) && types_match (_q140, _q40)))
									{
									  if ((_q141 == _q100 && ! TREE_SIDE_EFFECTS (_q141)) || (operand_equal_p (_q141, _q100, 0) && types_match (_q141, _q100)))
									    {
									      if (integer_zerop (_q131))
										{
										  switch (TREE_CODE (_p1))
										    {
										    case BIT_XOR_EXPR:
										      {
											tree _q180 = TREE_OPERAND (_p1, 0);
											tree _q181 = TREE_OPERAND (_p1, 1);
											{
											  tree _q180_pops[1];
											  if (tree_nop_convert (_q180, _q180_pops))
											    {
											      tree _q190 = _q180_pops[0];
											      switch (TREE_CODE (_q190))
											        {
												case NEGATE_EXPR:
												  {
												    tree _q200 = TREE_OPERAND (_q190, 0);
												    {
												      tree _q200_pops[1];
												      if (tree_nop_convert (_q200, _q200_pops))
												        {
													  tree _q210 = _q200_pops[0];
													  switch (TREE_CODE (_q210))
													    {
													    CASE_CONVERT:
													      {
														tree _q220 = TREE_OPERAND (_q210, 0);
														switch (TREE_CODE (_q220))
														  {
														  case LT_EXPR:
														    {
														      tree _q230 = TREE_OPERAND (_q220, 0);
														      tree _q231 = TREE_OPERAND (_q220, 1);
														      if ((_q230 == _q40 && ! TREE_SIDE_EFFECTS (_q230)) || (operand_equal_p (_q230, _q40, 0) && types_match (_q230, _q40)))
															{
															  if (integer_zerop (_q231))
															    {
															      if (tree_max_value (_q181))
																{
																  if ((_p2 == _q41 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q41, 0) && types_match (_p2, _q41)))
																    {
																      {
																	tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
																	if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																	  {
																	    {
																	      res_ops[0] = captures[0];
																	      res_ops[1] = captures[1];
																	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 58, __FILE__, __LINE__, false);
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
												        }
												    }
												    break;
												  }
											        default:;
											        }
											    }
											}
											if (tree_max_value (_q180))
											  {
											    {
											      tree _q181_pops[1];
											      if (tree_nop_convert (_q181, _q181_pops))
											        {
												  tree _q200 = _q181_pops[0];
												  switch (TREE_CODE (_q200))
												    {
												    case NEGATE_EXPR:
												      {
													tree _q210 = TREE_OPERAND (_q200, 0);
													{
													  tree _q210_pops[1];
													  if (tree_nop_convert (_q210, _q210_pops))
													    {
													      tree _q220 = _q210_pops[0];
													      switch (TREE_CODE (_q220))
													        {
														CASE_CONVERT:
														  {
														    tree _q230 = TREE_OPERAND (_q220, 0);
														    switch (TREE_CODE (_q230))
														      {
														      case LT_EXPR:
														        {
															  tree _q240 = TREE_OPERAND (_q230, 0);
															  tree _q241 = TREE_OPERAND (_q230, 1);
															  if ((_q240 == _q40 && ! TREE_SIDE_EFFECTS (_q240)) || (operand_equal_p (_q240, _q40, 0) && types_match (_q240, _q40)))
															    {
															      if (integer_zerop (_q241))
																{
																  if ((_p2 == _q41 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q41, 0) && types_match (_p2, _q41)))
																    {
																      {
																	tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
																	if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																	  {
																	    {
																	      res_ops[0] = captures[0];
																	      res_ops[1] = captures[1];
																	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 58, __FILE__, __LINE__, false);
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
								      if ((_q140 == _q100 && ! TREE_SIDE_EFFECTS (_q140)) || (operand_equal_p (_q140, _q100, 0) && types_match (_q140, _q100)))
									{
									  if ((_q141 == _q40 && ! TREE_SIDE_EFFECTS (_q141)) || (operand_equal_p (_q141, _q40, 0) && types_match (_q141, _q40)))
									    {
									      if (integer_zerop (_q131))
										{
										  switch (TREE_CODE (_p1))
										    {
										    case BIT_XOR_EXPR:
										      {
											tree _q180 = TREE_OPERAND (_p1, 0);
											tree _q181 = TREE_OPERAND (_p1, 1);
											{
											  tree _q180_pops[1];
											  if (tree_nop_convert (_q180, _q180_pops))
											    {
											      tree _q190 = _q180_pops[0];
											      switch (TREE_CODE (_q190))
											        {
												case NEGATE_EXPR:
												  {
												    tree _q200 = TREE_OPERAND (_q190, 0);
												    {
												      tree _q200_pops[1];
												      if (tree_nop_convert (_q200, _q200_pops))
												        {
													  tree _q210 = _q200_pops[0];
													  switch (TREE_CODE (_q210))
													    {
													    CASE_CONVERT:
													      {
														tree _q220 = TREE_OPERAND (_q210, 0);
														switch (TREE_CODE (_q220))
														  {
														  case LT_EXPR:
														    {
														      tree _q230 = TREE_OPERAND (_q220, 0);
														      tree _q231 = TREE_OPERAND (_q220, 1);
														      if ((_q230 == _q40 && ! TREE_SIDE_EFFECTS (_q230)) || (operand_equal_p (_q230, _q40, 0) && types_match (_q230, _q40)))
															{
															  if (integer_zerop (_q231))
															    {
															      if (tree_max_value (_q181))
																{
																  if ((_p2 == _q41 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q41, 0) && types_match (_p2, _q41)))
																    {
																      {
																	tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
																	if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																	  {
																	    {
																	      res_ops[0] = captures[0];
																	      res_ops[1] = captures[1];
																	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 58, __FILE__, __LINE__, false);
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
												        }
												    }
												    break;
												  }
											        default:;
											        }
											    }
											}
											if (tree_max_value (_q180))
											  {
											    {
											      tree _q181_pops[1];
											      if (tree_nop_convert (_q181, _q181_pops))
											        {
												  tree _q200 = _q181_pops[0];
												  switch (TREE_CODE (_q200))
												    {
												    case NEGATE_EXPR:
												      {
													tree _q210 = TREE_OPERAND (_q200, 0);
													{
													  tree _q210_pops[1];
													  if (tree_nop_convert (_q210, _q210_pops))
													    {
													      tree _q220 = _q210_pops[0];
													      switch (TREE_CODE (_q220))
													        {
														CASE_CONVERT:
														  {
														    tree _q230 = TREE_OPERAND (_q220, 0);
														    switch (TREE_CODE (_q230))
														      {
														      case LT_EXPR:
														        {
															  tree _q240 = TREE_OPERAND (_q230, 0);
															  tree _q241 = TREE_OPERAND (_q230, 1);
															  if ((_q240 == _q40 && ! TREE_SIDE_EFFECTS (_q240)) || (operand_equal_p (_q240, _q40, 0) && types_match (_q240, _q40)))
															    {
															      if (integer_zerop (_q241))
																{
																  if ((_p2 == _q41 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q41, 0) && types_match (_p2, _q41)))
																    {
																      {
																	tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
																	if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																	  {
																	    {
																	      res_ops[0] = captures[0];
																	      res_ops[1] = captures[1];
																	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 58, __FILE__, __LINE__, false);
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
							        break;
							      }
							    case BIT_NOT_EXPR:
							      {
								tree _q130 = TREE_OPERAND (_q21, 0);
								switch (TREE_CODE (_q130))
								  {
								  case LT_EXPR:
								    {
								      tree _q140 = TREE_OPERAND (_q130, 0);
								      tree _q141 = TREE_OPERAND (_q130, 1);
								      switch (TREE_CODE (_q140))
								        {
									case BIT_XOR_EXPR:
									  {
									    tree _q150 = TREE_OPERAND (_q140, 0);
									    tree _q151 = TREE_OPERAND (_q140, 1);
									    if ((_q150 == _q40 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q40, 0) && types_match (_q150, _q40)))
									      {
										if ((_q151 == _q100 && ! TREE_SIDE_EFFECTS (_q151)) || (operand_equal_p (_q151, _q100, 0) && types_match (_q151, _q100)))
										  {
										    if (integer_zerop (_q141))
										      {
											switch (TREE_CODE (_p1))
											  {
											  case BIT_XOR_EXPR:
											    {
											      tree _q190 = TREE_OPERAND (_p1, 0);
											      tree _q191 = TREE_OPERAND (_p1, 1);
											      {
												tree _q190_pops[1];
												if (tree_nop_convert (_q190, _q190_pops))
												  {
												    tree _q200 = _q190_pops[0];
												    switch (TREE_CODE (_q200))
												      {
												      case NEGATE_EXPR:
												        {
													  tree _q210 = TREE_OPERAND (_q200, 0);
													  {
													    tree _q210_pops[1];
													    if (tree_nop_convert (_q210, _q210_pops))
													      {
														tree _q220 = _q210_pops[0];
														switch (TREE_CODE (_q220))
														  {
														  CASE_CONVERT:
														    {
														      tree _q230 = TREE_OPERAND (_q220, 0);
														      switch (TREE_CODE (_q230))
														        {
															case LT_EXPR:
															  {
															    tree _q240 = TREE_OPERAND (_q230, 0);
															    tree _q241 = TREE_OPERAND (_q230, 1);
															    if ((_q240 == _q40 && ! TREE_SIDE_EFFECTS (_q240)) || (operand_equal_p (_q240, _q40, 0) && types_match (_q240, _q40)))
															      {
																if (integer_zerop (_q241))
																  {
																    if (tree_max_value (_q191))
																      {
																	if ((_p2 == _q41 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q41, 0) && types_match (_p2, _q41)))
																	  {
																	    {
																	      tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
																	      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																		{
																		  {
																		    res_ops[0] = captures[0];
																		    res_ops[1] = captures[1];
																		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 59, __FILE__, __LINE__, false);
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
													      }
													  }
												          break;
												        }
												      default:;
												      }
												  }
											      }
											      if (tree_max_value (_q190))
												{
												  {
												    tree _q191_pops[1];
												    if (tree_nop_convert (_q191, _q191_pops))
												      {
													tree _q210 = _q191_pops[0];
													switch (TREE_CODE (_q210))
													  {
													  case NEGATE_EXPR:
													    {
													      tree _q220 = TREE_OPERAND (_q210, 0);
													      {
														tree _q220_pops[1];
														if (tree_nop_convert (_q220, _q220_pops))
														  {
														    tree _q230 = _q220_pops[0];
														    switch (TREE_CODE (_q230))
														      {
														      CASE_CONVERT:
														        {
															  tree _q240 = TREE_OPERAND (_q230, 0);
															  switch (TREE_CODE (_q240))
															    {
															    case LT_EXPR:
															      {
																tree _q250 = TREE_OPERAND (_q240, 0);
																tree _q251 = TREE_OPERAND (_q240, 1);
																if ((_q250 == _q40 && ! TREE_SIDE_EFFECTS (_q250)) || (operand_equal_p (_q250, _q40, 0) && types_match (_q250, _q40)))
																  {
																    if (integer_zerop (_q251))
																      {
																	if ((_p2 == _q41 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q41, 0) && types_match (_p2, _q41)))
																	  {
																	    {
																	      tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
																	      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																		{
																		  {
																		    res_ops[0] = captures[0];
																		    res_ops[1] = captures[1];
																		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 59, __FILE__, __LINE__, false);
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
									    if ((_q150 == _q100 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q100, 0) && types_match (_q150, _q100)))
									      {
										if ((_q151 == _q40 && ! TREE_SIDE_EFFECTS (_q151)) || (operand_equal_p (_q151, _q40, 0) && types_match (_q151, _q40)))
										  {
										    if (integer_zerop (_q141))
										      {
											switch (TREE_CODE (_p1))
											  {
											  case BIT_XOR_EXPR:
											    {
											      tree _q190 = TREE_OPERAND (_p1, 0);
											      tree _q191 = TREE_OPERAND (_p1, 1);
											      {
												tree _q190_pops[1];
												if (tree_nop_convert (_q190, _q190_pops))
												  {
												    tree _q200 = _q190_pops[0];
												    switch (TREE_CODE (_q200))
												      {
												      case NEGATE_EXPR:
												        {
													  tree _q210 = TREE_OPERAND (_q200, 0);
													  {
													    tree _q210_pops[1];
													    if (tree_nop_convert (_q210, _q210_pops))
													      {
														tree _q220 = _q210_pops[0];
														switch (TREE_CODE (_q220))
														  {
														  CASE_CONVERT:
														    {
														      tree _q230 = TREE_OPERAND (_q220, 0);
														      switch (TREE_CODE (_q230))
														        {
															case LT_EXPR:
															  {
															    tree _q240 = TREE_OPERAND (_q230, 0);
															    tree _q241 = TREE_OPERAND (_q230, 1);
															    if ((_q240 == _q40 && ! TREE_SIDE_EFFECTS (_q240)) || (operand_equal_p (_q240, _q40, 0) && types_match (_q240, _q40)))
															      {
																if (integer_zerop (_q241))
																  {
																    if (tree_max_value (_q191))
																      {
																	if ((_p2 == _q41 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q41, 0) && types_match (_p2, _q41)))
																	  {
																	    {
																	      tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
																	      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																		{
																		  {
																		    res_ops[0] = captures[0];
																		    res_ops[1] = captures[1];
																		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 59, __FILE__, __LINE__, false);
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
													      }
													  }
												          break;
												        }
												      default:;
												      }
												  }
											      }
											      if (tree_max_value (_q190))
												{
												  {
												    tree _q191_pops[1];
												    if (tree_nop_convert (_q191, _q191_pops))
												      {
													tree _q210 = _q191_pops[0];
													switch (TREE_CODE (_q210))
													  {
													  case NEGATE_EXPR:
													    {
													      tree _q220 = TREE_OPERAND (_q210, 0);
													      {
														tree _q220_pops[1];
														if (tree_nop_convert (_q220, _q220_pops))
														  {
														    tree _q230 = _q220_pops[0];
														    switch (TREE_CODE (_q230))
														      {
														      CASE_CONVERT:
														        {
															  tree _q240 = TREE_OPERAND (_q230, 0);
															  switch (TREE_CODE (_q240))
															    {
															    case LT_EXPR:
															      {
																tree _q250 = TREE_OPERAND (_q240, 0);
																tree _q251 = TREE_OPERAND (_q240, 1);
																if ((_q250 == _q40 && ! TREE_SIDE_EFFECTS (_q250)) || (operand_equal_p (_q250, _q40, 0) && types_match (_q250, _q40)))
																  {
																    if (integer_zerop (_q251))
																      {
																	if ((_p2 == _q41 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q41, 0) && types_match (_p2, _q41)))
																	  {
																	    {
																	      tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
																	      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																		{
																		  {
																		    res_ops[0] = captures[0];
																		    res_ops[1] = captures[1];
																		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 59, __FILE__, __LINE__, false);
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
		case GE_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    tree _q31 = TREE_OPERAND (_q20, 1);
		    switch (TREE_CODE (_q30))
		      {
		      case BIT_XOR_EXPR:
		        {
			  tree _q40 = TREE_OPERAND (_q30, 0);
			  tree _q41 = TREE_OPERAND (_q30, 1);
			  if (integer_zerop (_q31))
			    {
			      switch (TREE_CODE (_q21))
			        {
				case LT_EXPR:
				  {
				    tree _q80 = TREE_OPERAND (_q21, 0);
				    tree _q81 = TREE_OPERAND (_q21, 1);
				    switch (TREE_CODE (_q80))
				      {
				      case BIT_XOR_EXPR:
				        {
					  tree _q90 = TREE_OPERAND (_q80, 0);
					  tree _q91 = TREE_OPERAND (_q80, 1);
					  if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
					    {
					      {
						tree _q91_pops[1];
						if (tree_nop_convert (_q91, _q91_pops))
						  {
						    tree _q110 = _q91_pops[0];
						    switch (TREE_CODE (_q110))
						      {
						      case PLUS_EXPR:
						        {
							  tree _q120 = TREE_OPERAND (_q110, 0);
							  tree _q121 = TREE_OPERAND (_q110, 1);
							  {
							    tree _q120_pops[1];
							    if (tree_nop_convert (_q120, _q120_pops))
							      {
								tree _q130 = _q120_pops[0];
								if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
								  {
								    {
								      tree _q121_pops[1];
								      if (tree_nop_convert (_q121, _q121_pops))
								        {
									  tree _q150 = _q121_pops[0];
									  if ((_q150 == _q41 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q41, 0) && types_match (_q150, _q41)))
									    {
									      if (integer_zerop (_q81))
										{
										  switch (TREE_CODE (_p1))
										    {
										    case BIT_XOR_EXPR:
										      {
											tree _q180 = TREE_OPERAND (_p1, 0);
											tree _q181 = TREE_OPERAND (_p1, 1);
											{
											  tree _q180_pops[1];
											  if (tree_nop_convert (_q180, _q180_pops))
											    {
											      tree _q190 = _q180_pops[0];
											      switch (TREE_CODE (_q190))
											        {
												case NEGATE_EXPR:
												  {
												    tree _q200 = TREE_OPERAND (_q190, 0);
												    {
												      tree _q200_pops[1];
												      if (tree_nop_convert (_q200, _q200_pops))
												        {
													  tree _q210 = _q200_pops[0];
													  switch (TREE_CODE (_q210))
													    {
													    CASE_CONVERT:
													      {
														tree _q220 = TREE_OPERAND (_q210, 0);
														switch (TREE_CODE (_q220))
														  {
														  case LT_EXPR:
														    {
														      tree _q230 = TREE_OPERAND (_q220, 0);
														      tree _q231 = TREE_OPERAND (_q220, 1);
														      if ((_q230 == _q40 && ! TREE_SIDE_EFFECTS (_q230)) || (operand_equal_p (_q230, _q40, 0) && types_match (_q230, _q40)))
															{
															  if (integer_zerop (_q231))
															    {
															      if (tree_max_value (_q181))
																{
																  if ((_p2 == _q91 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q91, 0) && types_match (_p2, _q91)))
																    {
																      {
																	tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _q91 };
																	if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																	  {
																	    {
																	      res_ops[0] = captures[0];
																	      res_ops[1] = captures[1];
																	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 58, __FILE__, __LINE__, false);
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
												        }
												    }
												    break;
												  }
											        default:;
											        }
											    }
											}
											if (tree_max_value (_q180))
											  {
											    {
											      tree _q181_pops[1];
											      if (tree_nop_convert (_q181, _q181_pops))
											        {
												  tree _q200 = _q181_pops[0];
												  switch (TREE_CODE (_q200))
												    {
												    case NEGATE_EXPR:
												      {
													tree _q210 = TREE_OPERAND (_q200, 0);
													{
													  tree _q210_pops[1];
													  if (tree_nop_convert (_q210, _q210_pops))
													    {
													      tree _q220 = _q210_pops[0];
													      switch (TREE_CODE (_q220))
													        {
														CASE_CONVERT:
														  {
														    tree _q230 = TREE_OPERAND (_q220, 0);
														    switch (TREE_CODE (_q230))
														      {
														      case LT_EXPR:
														        {
															  tree _q240 = TREE_OPERAND (_q230, 0);
															  tree _q241 = TREE_OPERAND (_q230, 1);
															  if ((_q240 == _q40 && ! TREE_SIDE_EFFECTS (_q240)) || (operand_equal_p (_q240, _q40, 0) && types_match (_q240, _q40)))
															    {
															      if (integer_zerop (_q241))
																{
																  if ((_p2 == _q91 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q91, 0) && types_match (_p2, _q91)))
																    {
																      {
																	tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _q91 };
																	if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																	  {
																	    {
																	      res_ops[0] = captures[0];
																	      res_ops[1] = captures[1];
																	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 58, __FILE__, __LINE__, false);
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
					  if ((_q90 == _q41 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q41, 0) && types_match (_q90, _q41)))
					    {
					      {
						tree _q91_pops[1];
						if (tree_nop_convert (_q91, _q91_pops))
						  {
						    tree _q110 = _q91_pops[0];
						    switch (TREE_CODE (_q110))
						      {
						      case PLUS_EXPR:
						        {
							  tree _q120 = TREE_OPERAND (_q110, 0);
							  tree _q121 = TREE_OPERAND (_q110, 1);
							  {
							    tree _q120_pops[1];
							    if (tree_nop_convert (_q120, _q120_pops))
							      {
								tree _q130 = _q120_pops[0];
								if ((_q130 == _q41 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q41, 0) && types_match (_q130, _q41)))
								  {
								    {
								      tree _q121_pops[1];
								      if (tree_nop_convert (_q121, _q121_pops))
								        {
									  tree _q150 = _q121_pops[0];
									  if ((_q150 == _q40 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q40, 0) && types_match (_q150, _q40)))
									    {
									      if (integer_zerop (_q81))
										{
										  switch (TREE_CODE (_p1))
										    {
										    case BIT_XOR_EXPR:
										      {
											tree _q180 = TREE_OPERAND (_p1, 0);
											tree _q181 = TREE_OPERAND (_p1, 1);
											{
											  tree _q180_pops[1];
											  if (tree_nop_convert (_q180, _q180_pops))
											    {
											      tree _q190 = _q180_pops[0];
											      switch (TREE_CODE (_q190))
											        {
												case NEGATE_EXPR:
												  {
												    tree _q200 = TREE_OPERAND (_q190, 0);
												    {
												      tree _q200_pops[1];
												      if (tree_nop_convert (_q200, _q200_pops))
												        {
													  tree _q210 = _q200_pops[0];
													  switch (TREE_CODE (_q210))
													    {
													    CASE_CONVERT:
													      {
														tree _q220 = TREE_OPERAND (_q210, 0);
														switch (TREE_CODE (_q220))
														  {
														  case LT_EXPR:
														    {
														      tree _q230 = TREE_OPERAND (_q220, 0);
														      tree _q231 = TREE_OPERAND (_q220, 1);
														      if ((_q230 == _q41 && ! TREE_SIDE_EFFECTS (_q230)) || (operand_equal_p (_q230, _q41, 0) && types_match (_q230, _q41)))
															{
															  if (integer_zerop (_q231))
															    {
															      if (tree_max_value (_q181))
																{
																  if ((_p2 == _q91 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q91, 0) && types_match (_p2, _q91)))
																    {
																      {
																	tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _q91 };
																	if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																	  {
																	    {
																	      res_ops[0] = captures[0];
																	      res_ops[1] = captures[1];
																	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 58, __FILE__, __LINE__, false);
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
												        }
												    }
												    break;
												  }
											        default:;
											        }
											    }
											}
											if (tree_max_value (_q180))
											  {
											    {
											      tree _q181_pops[1];
											      if (tree_nop_convert (_q181, _q181_pops))
											        {
												  tree _q200 = _q181_pops[0];
												  switch (TREE_CODE (_q200))
												    {
												    case NEGATE_EXPR:
												      {
													tree _q210 = TREE_OPERAND (_q200, 0);
													{
													  tree _q210_pops[1];
													  if (tree_nop_convert (_q210, _q210_pops))
													    {
													      tree _q220 = _q210_pops[0];
													      switch (TREE_CODE (_q220))
													        {
														CASE_CONVERT:
														  {
														    tree _q230 = TREE_OPERAND (_q220, 0);
														    switch (TREE_CODE (_q230))
														      {
														      case LT_EXPR:
														        {
															  tree _q240 = TREE_OPERAND (_q230, 0);
															  tree _q241 = TREE_OPERAND (_q230, 1);
															  if ((_q240 == _q41 && ! TREE_SIDE_EFFECTS (_q240)) || (operand_equal_p (_q240, _q41, 0) && types_match (_q240, _q41)))
															    {
															      if (integer_zerop (_q241))
																{
																  if ((_p2 == _q91 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q91, 0) && types_match (_p2, _q91)))
																    {
																      {
																	tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _q91 };
																	if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																	  {
																	    {
																	      res_ops[0] = captures[0];
																	      res_ops[1] = captures[1];
																	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 58, __FILE__, __LINE__, false);
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
		case BIT_NOT_EXPR:
		  {
		    tree _q30 = TREE_OPERAND (_q20, 0);
		    switch (TREE_CODE (_q30))
		      {
		      case LT_EXPR:
		        {
			  tree _q40 = TREE_OPERAND (_q30, 0);
			  tree _q41 = TREE_OPERAND (_q30, 1);
			  switch (TREE_CODE (_q40))
			    {
			    case BIT_XOR_EXPR:
			      {
				tree _q50 = TREE_OPERAND (_q40, 0);
				tree _q51 = TREE_OPERAND (_q40, 1);
				if (integer_zerop (_q41))
				  {
				    switch (TREE_CODE (_q21))
				      {
				      case LT_EXPR:
				        {
					  tree _q90 = TREE_OPERAND (_q21, 0);
					  tree _q91 = TREE_OPERAND (_q21, 1);
					  switch (TREE_CODE (_q90))
					    {
					    case BIT_XOR_EXPR:
					      {
						tree _q100 = TREE_OPERAND (_q90, 0);
						tree _q101 = TREE_OPERAND (_q90, 1);
						if ((_q100 == _q50 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q50, 0) && types_match (_q100, _q50)))
						  {
						    {
						      tree _q101_pops[1];
						      if (tree_nop_convert (_q101, _q101_pops))
						        {
							  tree _q120 = _q101_pops[0];
							  switch (TREE_CODE (_q120))
							    {
							    case PLUS_EXPR:
							      {
								tree _q130 = TREE_OPERAND (_q120, 0);
								tree _q131 = TREE_OPERAND (_q120, 1);
								{
								  tree _q130_pops[1];
								  if (tree_nop_convert (_q130, _q130_pops))
								    {
								      tree _q140 = _q130_pops[0];
								      if ((_q140 == _q50 && ! TREE_SIDE_EFFECTS (_q140)) || (operand_equal_p (_q140, _q50, 0) && types_match (_q140, _q50)))
									{
									  {
									    tree _q131_pops[1];
									    if (tree_nop_convert (_q131, _q131_pops))
									      {
										tree _q160 = _q131_pops[0];
										if ((_q160 == _q51 && ! TREE_SIDE_EFFECTS (_q160)) || (operand_equal_p (_q160, _q51, 0) && types_match (_q160, _q51)))
										  {
										    if (integer_zerop (_q91))
										      {
											switch (TREE_CODE (_p1))
											  {
											  case BIT_XOR_EXPR:
											    {
											      tree _q190 = TREE_OPERAND (_p1, 0);
											      tree _q191 = TREE_OPERAND (_p1, 1);
											      {
												tree _q190_pops[1];
												if (tree_nop_convert (_q190, _q190_pops))
												  {
												    tree _q200 = _q190_pops[0];
												    switch (TREE_CODE (_q200))
												      {
												      case NEGATE_EXPR:
												        {
													  tree _q210 = TREE_OPERAND (_q200, 0);
													  {
													    tree _q210_pops[1];
													    if (tree_nop_convert (_q210, _q210_pops))
													      {
														tree _q220 = _q210_pops[0];
														switch (TREE_CODE (_q220))
														  {
														  CASE_CONVERT:
														    {
														      tree _q230 = TREE_OPERAND (_q220, 0);
														      switch (TREE_CODE (_q230))
														        {
															case LT_EXPR:
															  {
															    tree _q240 = TREE_OPERAND (_q230, 0);
															    tree _q241 = TREE_OPERAND (_q230, 1);
															    if ((_q240 == _q50 && ! TREE_SIDE_EFFECTS (_q240)) || (operand_equal_p (_q240, _q50, 0) && types_match (_q240, _q50)))
															      {
																if (integer_zerop (_q241))
																  {
																    if (tree_max_value (_q191))
																      {
																	if ((_p2 == _q101 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q101, 0) && types_match (_p2, _q101)))
																	  {
																	    {
																	      tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q51, _q101 };
																	      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																		{
																		  {
																		    res_ops[0] = captures[0];
																		    res_ops[1] = captures[1];
																		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 59, __FILE__, __LINE__, false);
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
													      }
													  }
												          break;
												        }
												      default:;
												      }
												  }
											      }
											      if (tree_max_value (_q190))
												{
												  {
												    tree _q191_pops[1];
												    if (tree_nop_convert (_q191, _q191_pops))
												      {
													tree _q210 = _q191_pops[0];
													switch (TREE_CODE (_q210))
													  {
													  case NEGATE_EXPR:
													    {
													      tree _q220 = TREE_OPERAND (_q210, 0);
													      {
														tree _q220_pops[1];
														if (tree_nop_convert (_q220, _q220_pops))
														  {
														    tree _q230 = _q220_pops[0];
														    switch (TREE_CODE (_q230))
														      {
														      CASE_CONVERT:
														        {
															  tree _q240 = TREE_OPERAND (_q230, 0);
															  switch (TREE_CODE (_q240))
															    {
															    case LT_EXPR:
															      {
																tree _q250 = TREE_OPERAND (_q240, 0);
																tree _q251 = TREE_OPERAND (_q240, 1);
																if ((_q250 == _q50 && ! TREE_SIDE_EFFECTS (_q250)) || (operand_equal_p (_q250, _q50, 0) && types_match (_q250, _q50)))
																  {
																    if (integer_zerop (_q251))
																      {
																	if ((_p2 == _q101 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q101, 0) && types_match (_p2, _q101)))
																	  {
																	    {
																	      tree captures[3] ATTRIBUTE_UNUSED = { _q50, _q51, _q101 };
																	      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																		{
																		  {
																		    res_ops[0] = captures[0];
																		    res_ops[1] = captures[1];
																		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 59, __FILE__, __LINE__, false);
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
						if ((_q100 == _q51 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q51, 0) && types_match (_q100, _q51)))
						  {
						    {
						      tree _q101_pops[1];
						      if (tree_nop_convert (_q101, _q101_pops))
						        {
							  tree _q120 = _q101_pops[0];
							  switch (TREE_CODE (_q120))
							    {
							    case PLUS_EXPR:
							      {
								tree _q130 = TREE_OPERAND (_q120, 0);
								tree _q131 = TREE_OPERAND (_q120, 1);
								{
								  tree _q130_pops[1];
								  if (tree_nop_convert (_q130, _q130_pops))
								    {
								      tree _q140 = _q130_pops[0];
								      if ((_q140 == _q51 && ! TREE_SIDE_EFFECTS (_q140)) || (operand_equal_p (_q140, _q51, 0) && types_match (_q140, _q51)))
									{
									  {
									    tree _q131_pops[1];
									    if (tree_nop_convert (_q131, _q131_pops))
									      {
										tree _q160 = _q131_pops[0];
										if ((_q160 == _q50 && ! TREE_SIDE_EFFECTS (_q160)) || (operand_equal_p (_q160, _q50, 0) && types_match (_q160, _q50)))
										  {
										    if (integer_zerop (_q91))
										      {
											switch (TREE_CODE (_p1))
											  {
											  case BIT_XOR_EXPR:
											    {
											      tree _q190 = TREE_OPERAND (_p1, 0);
											      tree _q191 = TREE_OPERAND (_p1, 1);
											      {
												tree _q190_pops[1];
												if (tree_nop_convert (_q190, _q190_pops))
												  {
												    tree _q200 = _q190_pops[0];
												    switch (TREE_CODE (_q200))
												      {
												      case NEGATE_EXPR:
												        {
													  tree _q210 = TREE_OPERAND (_q200, 0);
													  {
													    tree _q210_pops[1];
													    if (tree_nop_convert (_q210, _q210_pops))
													      {
														tree _q220 = _q210_pops[0];
														switch (TREE_CODE (_q220))
														  {
														  CASE_CONVERT:
														    {
														      tree _q230 = TREE_OPERAND (_q220, 0);
														      switch (TREE_CODE (_q230))
														        {
															case LT_EXPR:
															  {
															    tree _q240 = TREE_OPERAND (_q230, 0);
															    tree _q241 = TREE_OPERAND (_q230, 1);
															    if ((_q240 == _q51 && ! TREE_SIDE_EFFECTS (_q240)) || (operand_equal_p (_q240, _q51, 0) && types_match (_q240, _q51)))
															      {
																if (integer_zerop (_q241))
																  {
																    if (tree_max_value (_q191))
																      {
																	if ((_p2 == _q101 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q101, 0) && types_match (_p2, _q101)))
																	  {
																	    {
																	      tree captures[3] ATTRIBUTE_UNUSED = { _q51, _q50, _q101 };
																	      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																		{
																		  {
																		    res_ops[0] = captures[0];
																		    res_ops[1] = captures[1];
																		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 59, __FILE__, __LINE__, false);
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
													      }
													  }
												          break;
												        }
												      default:;
												      }
												  }
											      }
											      if (tree_max_value (_q190))
												{
												  {
												    tree _q191_pops[1];
												    if (tree_nop_convert (_q191, _q191_pops))
												      {
													tree _q210 = _q191_pops[0];
													switch (TREE_CODE (_q210))
													  {
													  case NEGATE_EXPR:
													    {
													      tree _q220 = TREE_OPERAND (_q210, 0);
													      {
														tree _q220_pops[1];
														if (tree_nop_convert (_q220, _q220_pops))
														  {
														    tree _q230 = _q220_pops[0];
														    switch (TREE_CODE (_q230))
														      {
														      CASE_CONVERT:
														        {
															  tree _q240 = TREE_OPERAND (_q230, 0);
															  switch (TREE_CODE (_q240))
															    {
															    case LT_EXPR:
															      {
																tree _q250 = TREE_OPERAND (_q240, 0);
																tree _q251 = TREE_OPERAND (_q240, 1);
																if ((_q250 == _q51 && ! TREE_SIDE_EFFECTS (_q250)) || (operand_equal_p (_q250, _q51, 0) && types_match (_q250, _q51)))
																  {
																    if (integer_zerop (_q251))
																      {
																	if ((_p2 == _q101 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q101, 0) && types_match (_p2, _q101)))
																	  {
																	    {
																	      tree captures[3] ATTRIBUTE_UNUSED = { _q51, _q50, _q101 };
																	      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																		{
																		  {
																		    res_ops[0] = captures[0];
																		    res_ops[1] = captures[1];
																		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 59, __FILE__, __LINE__, false);
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
								    if (tree_max_value (_q81))
								      {
									switch (TREE_CODE (_p2))
									  {
									  case REALPART_EXPR:
									    {
									      tree _q150 = TREE_OPERAND (_p2, 0);
									      if ((_q150 == _q30 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q30, 0) && types_match (_q150, _q30)))
										{
										  {
										    tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _q30 };
										    if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
										      {
											{
											  res_ops[0] = captures[0];
											  res_ops[1] = captures[1];
											  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 60, __FILE__, __LINE__, false);
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
							    if ((_q110 == _q41 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q41, 0) && types_match (_q110, _q41)))
							      {
								if (integer_zerop (_q111))
								  {
								    if (tree_max_value (_q81))
								      {
									switch (TREE_CODE (_p2))
									  {
									  case REALPART_EXPR:
									    {
									      tree _q150 = TREE_OPERAND (_p2, 0);
									      if ((_q150 == _q30 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q30, 0) && types_match (_q150, _q30)))
										{
										  {
										    tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _q30 };
										    if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
										      {
											{
											  res_ops[0] = captures[0];
											  res_ops[1] = captures[1];
											  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 60, __FILE__, __LINE__, false);
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
					  tree _q80_pops[1];
					  if (tree_nop_convert (_q80, _q80_pops))
					    {
					      tree _q90 = _q80_pops[0];
					      switch (TREE_CODE (_q90))
					        {
						case NEGATE_EXPR:
						  {
						    tree _q100 = TREE_OPERAND (_q90, 0);
						    {
						      tree _q100_pops[1];
						      if (tree_nop_convert (_q100, _q100_pops))
						        {
							  tree _q110 = _q100_pops[0];
							  switch (TREE_CODE (_q110))
							    {
							    CASE_CONVERT:
							      {
								tree _q120 = TREE_OPERAND (_q110, 0);
								switch (TREE_CODE (_q120))
								  {
								  case LT_EXPR:
								    {
								      tree _q130 = TREE_OPERAND (_q120, 0);
								      tree _q131 = TREE_OPERAND (_q120, 1);
								      if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
									{
									  if (integer_zerop (_q131))
									    {
									      if (tree_max_value (_q81))
										{
										  switch (TREE_CODE (_p2))
										    {
										    case REALPART_EXPR:
										      {
											tree _q170 = TREE_OPERAND (_p2, 0);
											if ((_q170 == _q30 && ! TREE_SIDE_EFFECTS (_q170)) || (operand_equal_p (_q170, _q30, 0) && types_match (_q170, _q30)))
											  {
											    {
											      tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _q30 };
											      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
												{
												  {
												    res_ops[0] = captures[0];
												    res_ops[1] = captures[1];
												    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 60, __FILE__, __LINE__, false);
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
								      if ((_q130 == _q41 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q41, 0) && types_match (_q130, _q41)))
									{
									  if (integer_zerop (_q131))
									    {
									      if (tree_max_value (_q81))
										{
										  switch (TREE_CODE (_p2))
										    {
										    case REALPART_EXPR:
										      {
											tree _q170 = TREE_OPERAND (_p2, 0);
											if ((_q170 == _q30 && ! TREE_SIDE_EFFECTS (_q170)) || (operand_equal_p (_q170, _q30, 0) && types_match (_q170, _q30)))
											  {
											    {
											      tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _q30 };
											      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
												{
												  {
												    res_ops[0] = captures[0];
												    res_ops[1] = captures[1];
												    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 60, __FILE__, __LINE__, false);
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
					if (tree_max_value (_q80))
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
									case REALPART_EXPR:
									  {
									    tree _q150 = TREE_OPERAND (_p2, 0);
									    if ((_q150 == _q30 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q30, 0) && types_match (_q150, _q30)))
									      {
										{
										  tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _q30 };
										  if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
										    {
										      {
											res_ops[0] = captures[0];
											res_ops[1] = captures[1];
											if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 60, __FILE__, __LINE__, false);
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
							      if ((_q120 == _q41 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q41, 0) && types_match (_q120, _q41)))
								{
								  if (integer_zerop (_q121))
								    {
								      switch (TREE_CODE (_p2))
								        {
									case REALPART_EXPR:
									  {
									    tree _q150 = TREE_OPERAND (_p2, 0);
									    if ((_q150 == _q30 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q30, 0) && types_match (_q150, _q30)))
									      {
										{
										  tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _q30 };
										  if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
										    {
										      {
											res_ops[0] = captures[0];
											res_ops[1] = captures[1];
											if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 60, __FILE__, __LINE__, false);
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
						        break;
						      }
						    default:;
						    }
					          break;
					        }
					      default:;
					      }
					  {
					    tree _q81_pops[1];
					    if (tree_nop_convert (_q81, _q81_pops))
					      {
						tree _q100 = _q81_pops[0];
						switch (TREE_CODE (_q100))
						  {
						  case NEGATE_EXPR:
						    {
						      tree _q110 = TREE_OPERAND (_q100, 0);
						      {
							tree _q110_pops[1];
							if (tree_nop_convert (_q110, _q110_pops))
							  {
							    tree _q120 = _q110_pops[0];
							    switch (TREE_CODE (_q120))
							      {
							      CASE_CONVERT:
							        {
								  tree _q130 = TREE_OPERAND (_q120, 0);
								  switch (TREE_CODE (_q130))
								    {
								    case LT_EXPR:
								      {
									tree _q140 = TREE_OPERAND (_q130, 0);
									tree _q141 = TREE_OPERAND (_q130, 1);
									if ((_q140 == _q40 && ! TREE_SIDE_EFFECTS (_q140)) || (operand_equal_p (_q140, _q40, 0) && types_match (_q140, _q40)))
									  {
									    if (integer_zerop (_q141))
									      {
										switch (TREE_CODE (_p2))
										  {
										  case REALPART_EXPR:
										    {
										      tree _q170 = TREE_OPERAND (_p2, 0);
										      if ((_q170 == _q30 && ! TREE_SIDE_EFFECTS (_q170)) || (operand_equal_p (_q170, _q30, 0) && types_match (_q170, _q30)))
											{
											  {
											    tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _q30 };
											    if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
											      {
												{
												  res_ops[0] = captures[0];
												  res_ops[1] = captures[1];
												  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 60, __FILE__, __LINE__, false);
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
									if ((_q140 == _q41 && ! TREE_SIDE_EFFECTS (_q140)) || (operand_equal_p (_q140, _q41, 0) && types_match (_q140, _q41)))
									  {
									    if (integer_zerop (_q141))
									      {
										switch (TREE_CODE (_p2))
										  {
										  case REALPART_EXPR:
										    {
										      tree _q170 = TREE_OPERAND (_p2, 0);
										      if ((_q170 == _q30 && ! TREE_SIDE_EFFECTS (_q170)) || (operand_equal_p (_q170, _q30, 0) && types_match (_q170, _q30)))
											{
											  {
											    tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _q30 };
											    if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
 && types_match (type, captures[0], captures[1])
)
											      {
												{
												  res_ops[0] = captures[0];
												  res_ops[1] = captures[1];
												  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 60, __FILE__, __LINE__, false);
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
          default:;
          }
        break;
      }
    default:;
    }
  return false;
}

tree
generic_simplify_POINTER_DIFF_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1392;
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1392;
	{
	  tree res_op0;
	  res_op0 = captures[0];
	  tree _r;
	  _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 916, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1392:;
      }
    }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _p0, 0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p0 };
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1393;
	{
	  tree _r;
	  _r =  build_zero_cst (type);
	  if (TREE_SIDE_EFFECTS (captures[1]))
	    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 917, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1393:;
      }
    }
  switch (TREE_CODE (_p0))
    {
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
		    if (TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[1]))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1394;
			{
			  tree res_op0;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[1];
			    _o1[1] = captures[2];
			    _r1 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    res_op0 = _r1;
			  }
			  tree _r;
			  _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
			  if (TREE_SIDE_EFFECTS (captures[0]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 918, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1394:;
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
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
		      if (tree_nop_conversion_p (TREE_TYPE(captures[0]), TREE_TYPE (captures[1]))
 && tree_nop_conversion_p (TREE_TYPE(captures[2]), TREE_TYPE (captures[3]))
)
			{
			  {
 poly_int64 diff;
			      if (ptr_difference_const (captures[1], captures[3], &diff)
)
				{
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1395;
				  {
				    tree _r;
				    _r =  build_int_cst_type (type, diff);
				    if (TREE_SIDE_EFFECTS (captures[1]))
				      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
				    if (TREE_SIDE_EFFECTS (captures[3]))
				      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 919, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail1395:;
				}
			  }
			}
		    }
		    break;
		  }
	        default:;
	        }
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _p1 };
		if (tree_nop_conversion_p (TREE_TYPE(captures[0]), TREE_TYPE (captures[1]))
 && tree_nop_conversion_p (TREE_TYPE(captures[2]), TREE_TYPE (captures[3]))
)
		  {
		    {
 poly_int64 diff;
			if (ptr_difference_const (captures[1], captures[3], &diff)
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1396;
			    {
			      tree _r;
			      _r =  build_int_cst_type (type, diff);
			      if (TREE_SIDE_EFFECTS (captures[1]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			      if (TREE_SIDE_EFFECTS (captures[2]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 919, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1396:;
			  }
		    }
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
		case ADDR_EXPR:
		  {
		    {
		      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
		      if (tree_nop_conversion_p (TREE_TYPE(captures[0]), TREE_TYPE (captures[1]))
 && tree_nop_conversion_p (TREE_TYPE(captures[2]), TREE_TYPE (captures[3]))
)
			{
			  {
 poly_int64 diff;
			      if (ptr_difference_const (captures[1], captures[3], &diff)
)
				{
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1397;
				  {
				    tree _r;
				    _r =  build_int_cst_type (type, diff);
				    if (TREE_SIDE_EFFECTS (captures[1]))
				      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
				    if (TREE_SIDE_EFFECTS (captures[3]))
				      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 920, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail1397:;
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
	  case ADDR_EXPR:
	    {
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _p1 };
		if (tree_nop_conversion_p (TREE_TYPE(captures[0]), TREE_TYPE (captures[1]))
 && tree_nop_conversion_p (TREE_TYPE(captures[2]), TREE_TYPE (captures[3]))
)
		  {
		    {
 poly_int64 diff;
			if (ptr_difference_const (captures[1], captures[3], &diff)
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1398;
			    {
			      tree _r;
			      _r =  build_int_cst_type (type, diff);
			      if (TREE_SIDE_EFFECTS (captures[1]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			      if (TREE_SIDE_EFFECTS (captures[2]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 920, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1398:;
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
    case ADDR_EXPR:
      {
	switch (TREE_CODE (_p1))
	  {
	  CASE_CONVERT:
	    {
	      tree _q30 = TREE_OPERAND (_p1, 0);
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _p1, _q30 };
		if (tree_nop_conversion_p (TREE_TYPE(captures[0]), TREE_TYPE (captures[1]))
 && tree_nop_conversion_p (TREE_TYPE(captures[2]), TREE_TYPE (captures[3]))
)
		  {
		    {
 poly_int64 diff;
			if (ptr_difference_const (captures[1], captures[3], &diff)
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1399;
			    {
			      tree _r;
			      _r =  build_int_cst_type (type, diff);
			      if (TREE_SIDE_EFFECTS (captures[0]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
			      if (TREE_SIDE_EFFECTS (captures[3]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 919, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1399:;
			  }
		    }
		  }
	      }
	      break;
	    }
          default:;
          }
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _p1, _p1 };
	  if (tree_nop_conversion_p (TREE_TYPE(captures[0]), TREE_TYPE (captures[1]))
 && tree_nop_conversion_p (TREE_TYPE(captures[2]), TREE_TYPE (captures[3]))
)
	    {
	      {
 poly_int64 diff;
		  if (ptr_difference_const (captures[1], captures[3], &diff)
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1400;
		      {
			tree _r;
			_r =  build_int_cst_type (type, diff);
			if (TREE_SIDE_EFFECTS (captures[0]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
			if (TREE_SIDE_EFFECTS (captures[2]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 919, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1400:;
		    }
	      }
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
	  case ADDR_EXPR:
	    {
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _p1, _q30 };
		if (tree_nop_conversion_p (TREE_TYPE(captures[0]), TREE_TYPE (captures[1]))
 && tree_nop_conversion_p (TREE_TYPE(captures[2]), TREE_TYPE (captures[3]))
)
		  {
		    {
 poly_int64 diff;
			if (ptr_difference_const (captures[1], captures[3], &diff)
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1401;
			    {
			      tree _r;
			      _r =  build_int_cst_type (type, diff);
			      if (TREE_SIDE_EFFECTS (captures[0]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
			      if (TREE_SIDE_EFFECTS (captures[3]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 920, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1401:;
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
    case ADDR_EXPR:
      {
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p0, _p1, _p1 };
	  if (tree_nop_conversion_p (TREE_TYPE(captures[0]), TREE_TYPE (captures[1]))
 && tree_nop_conversion_p (TREE_TYPE(captures[2]), TREE_TYPE (captures[3]))
)
	    {
	      {
 poly_int64 diff;
		  if (ptr_difference_const (captures[1], captures[3], &diff)
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1402;
		      {
			tree _r;
			_r =  build_int_cst_type (type, diff);
			if (TREE_SIDE_EFFECTS (captures[0]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
			if (TREE_SIDE_EFFECTS (captures[2]))
			  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 920, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1402:;
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
    case POINTER_PLUS_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
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
			    {
 poly_int64 diff;
				if (ptr_difference_const (captures[0], captures[2], &diff)
)
				  {
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1403;
				    {
				      tree res_op0;
				      res_op0 =  build_int_cst_type (type, diff);
				      tree res_op1;
				      {
					tree _o1[1], _r1;
					{
					  tree _o2[2], _r2;
					  _o2[0] = captures[1];
					  _o2[1] = captures[3];
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
				      _r = fold_build2_loc (loc, PLUS_EXPR, type, res_op0, res_op1);
				      if (TREE_SIDE_EFFECTS (captures[0]))
					_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
				      if (TREE_SIDE_EFFECTS (captures[2]))
					_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
				      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 921, __FILE__, __LINE__, true);
				      return _r;
				    }
next_after_fail1403:;
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
	  case ADDR_EXPR:
	    {
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		{
 poly_int64 diff;
		    if (ptr_difference_const (captures[0], captures[2], &diff)
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1404;
			{
			  tree res_op0;
			  res_op0 =  build_int_cst_type (type, diff);
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
			  _r = fold_build2_loc (loc, PLUS_EXPR, type, res_op0, res_op1);
			  if (TREE_SIDE_EFFECTS (captures[0]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
			  if (TREE_SIDE_EFFECTS (captures[2]))
			    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 922, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1404:;
		      }
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
	      if (!TYPE_SATURATING (type)
)
		{
		  if ((!FLOAT_TYPE_P (type) || flag_associative_math)
 && !FIXED_POINT_TYPE_P (type)
)
		    {
		      {
 tree stype = signed_type_for (TREE_TYPE (captures[1]));
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1405;
			  {
			    tree res_op0;
			    {
			      tree _o1[1], _r1;
			      _o1[0] = captures[1];
			      _r1 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, stype, _o1[0]);
			      res_op0 = _r1;
			    }
			    tree _r;
			    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
			    if (TREE_SIDE_EFFECTS (captures[2]))
			      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 923, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1405:;
		      }
		    }
		}
	    }
	  }
        break;
      }
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
		{
 poly_int64 diff;
		    if (ptr_difference_const (captures[0], captures[1], &diff)
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1406;
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
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 924, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1406:;
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
    case POINTER_PLUS_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || operand_equal_p (_q30, _p0, 0))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p0 };
	      if (!TYPE_SATURATING (type)
)
		{
		  if ((!FLOAT_TYPE_P (type) || flag_associative_math)
 && !FIXED_POINT_TYPE_P (type)
)
		    {
		      {
 tree stype = signed_type_for (TREE_TYPE (captures[1]));
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1407;
			  {
			    tree res_op0;
			    {
			      tree _o1[1], _r1;
			      {
				tree _o2[1], _r2;
				_o2[0] = captures[1];
				_r2 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, stype, _o2[0]);
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
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 925, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1407:;
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
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
		    if (!TYPE_SATURATING (type)
)
		      {
			if ((!FLOAT_TYPE_P (type) || flag_associative_math)
 && !FIXED_POINT_TYPE_P (type)
)
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1408;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[2];
			      tree _r;
			      _r = fold_build2_loc (loc, POINTER_DIFF_EXPR, type, res_op0, res_op1);
			      if (TREE_SIDE_EFFECTS (captures[1]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 926, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1408:;
			  }
		      }
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q20, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q20 };
		    if (!TYPE_SATURATING (type)
)
		      {
			if ((!FLOAT_TYPE_P (type) || flag_associative_math)
 && !FIXED_POINT_TYPE_P (type)
)
			  {
			    {
 tree stype = signed_type_for (TREE_TYPE (captures[1]));
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1409;
				{
				  tree res_op0;
				  {
				    tree _o1[1], _r1;
				    {
				      tree _o2[1], _r2;
				      _o2[0] = captures[1];
				      _r2 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, stype, _o2[0]);
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
				  {
				    tree _o1[1], _r1;
				    {
				      tree _o2[1], _r2;
				      _o2[0] = captures[2];
				      _r2 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, stype, _o2[0]);
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
				  _r = fold_build2_loc (loc, MINUS_EXPR, type, res_op0, res_op1);
				  if (TREE_SIDE_EFFECTS (captures[3]))
				    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[3]), _r);
				  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 927, __FILE__, __LINE__, true);
				  return _r;
				}
next_after_fail1409:;
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
if (integer_zerop (_p0))
  {
    {
      tree captures[1] ATTRIBUTE_UNUSED = { _p1 };
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1410;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1410;
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
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 928, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1410:;
    }
  }
  return NULL_TREE;
}

tree
generic_simplify_FLOOR_DIV_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_172 (loc, type, _p0, _p1, captures, FLOOR_DIV_EXPR);
	if (res) return res;
      }
    }
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	tree res = generic_simplify_293 (loc, type, _p0, _p1, captures, FLOOR_DIV_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  tree res = generic_simplify_295 (loc, type, _p0, _p1, captures, FLOOR_DIV_EXPR);
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
      tree res = generic_simplify_294 (loc, type, _p0, _p1, captures, FLOOR_DIV_EXPR);
      if (res) return res;
    }
  }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_296 (loc, type, _p0, _p1, captures, FLOOR_DIV_EXPR);
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
	      tree res = generic_simplify_297 (loc, type, _p0, _p1, captures, FLOOR_DIV_EXPR);
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
	      tree res = generic_simplify_297 (loc, type, _p0, _p1, captures, FLOOR_DIV_EXPR);
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
	      tree res = generic_simplify_298 (loc, type, _p0, _p1, captures, FLOOR_DIV_EXPR);
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
	      tree res = generic_simplify_298 (loc, type, _p0, _p1, captures, FLOOR_DIV_EXPR);
	      if (res) return res;
	    }
	  }
        break;
      }
    default:;
    }
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    tree res = generic_simplify_303 (loc, type, _p0, _p1, captures, FLOOR_DIV_EXPR, TRUNC_DIV_EXPR);
    if (res) return res;
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
			    tree res = generic_simplify_300 (loc, type, _p0, _p1, captures, FLOOR_DIV_EXPR);
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
		      tree res = generic_simplify_300 (loc, type, _p0, _p1, captures, FLOOR_DIV_EXPR);
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
	      tree res = generic_simplify_301 (loc, type, _p0, _p1, captures, FLOOR_DIV_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_301 (loc, type, _p0, _p1, captures, FLOOR_DIV_EXPR);
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
				tree res = generic_simplify_302 (loc, type, _p0, _p1, captures, FLOOR_DIV_EXPR);
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
 && (TREE_CODE_CLASS (FLOOR_DIV_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1627;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, FLOOR_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1627;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, FLOOR_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1627;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1627:;
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
 && (TREE_CODE_CLASS (FLOOR_DIV_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1628;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1628;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, FLOOR_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1628;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, FLOOR_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1628;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1628:;
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
 && (TREE_CODE_CLASS (FLOOR_DIV_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1629;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1629;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, FLOOR_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1629;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, FLOOR_DIV_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1629;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1629:;
	    }
	}
        break;
      }
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_FLOOR_MOD_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    tree res = generic_simplify_303 (loc, type, _p0, _p1, captures, FLOOR_MOD_EXPR, TRUNC_MOD_EXPR);
    if (res) return res;
  }
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	tree res = generic_simplify_304 (loc, type, _p0, _p1, captures, FLOOR_MOD_EXPR);
	if (res) return res;
      }
    }
  if (integer_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_305 (loc, type, _p0, _p1, captures, FLOOR_MOD_EXPR);
	if (res) return res;
      }
    }
  if (integer_minus_onep (_p1))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	tree res = generic_simplify_306 (loc, type, _p0, _p1, captures, FLOOR_MOD_EXPR);
	if (res) return res;
      }
    }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_307 (loc, type, _p0, _p1, captures, FLOOR_MOD_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p0))
    {
    case FLOOR_MOD_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_308 (loc, type, _p0, _p1, captures, FLOOR_MOD_EXPR);
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
		      tree res = generic_simplify_309 (loc, type, _p0, _p1, captures, FLOOR_MOD_EXPR);
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
	tree _q30 = TREE_OPERAND (_p1, 0);
	{
	  tree _q30_pops[1];
	  if (tree_power_of_two_cand (_q30, _q30_pops))
	    {
	      tree _q40 = _q30_pops[0];
	      {
		tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q40 };
		tree res = generic_simplify_310 (loc, type, _p0, _p1, captures, FLOOR_MOD_EXPR);
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
  if (tree_power_of_two_cand (_p1, _p1_pops))
    {
      tree _q30 = _p1_pops[0];
      {
	tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
	tree res = generic_simplify_310 (loc, type, _p0, _p1, captures, FLOOR_MOD_EXPR);
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
 && (TREE_CODE_CLASS (FLOOR_MOD_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1633;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, FLOOR_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1633;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, FLOOR_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1633;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1633:;
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
 && (TREE_CODE_CLASS (FLOOR_MOD_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1634;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1634;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, FLOOR_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1634;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, FLOOR_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1634;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1634:;
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
 && (TREE_CODE_CLASS (FLOOR_MOD_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1635;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1635;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, FLOOR_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1635;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, FLOOR_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1635;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1635:;
	    }
	}
        break;
      }
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_LE_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
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
		    tree res = generic_simplify_322 (loc, type, _p0, _p1, captures, LE_EXPR, GE_EXPR);
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
	      tree res = generic_simplify_323 (loc, type, _p0, _p1, captures, GE_EXPR);
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
		    tree res = generic_simplify_324 (loc, type, _p0, _p1, captures, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
		    tree res = generic_simplify_324 (loc, type, _p0, _p1, captures, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
		    tree res = generic_simplify_324 (loc, type, _p0, _p1, captures, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
		    tree res = generic_simplify_324 (loc, type, _p0, _p1, captures, LE_EXPR);
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
		    tree res = generic_simplify_349 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
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
		    tree res = generic_simplify_325 (loc, type, _p0, _p1, captures, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
		    tree res = generic_simplify_325 (loc, type, _p0, _p1, captures, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
		    tree res = generic_simplify_325 (loc, type, _p0, _p1, captures, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
		    tree res = generic_simplify_325 (loc, type, _p0, _p1, captures, LE_EXPR);
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
			  tree res = generic_simplify_326 (loc, type, _p0, _p1, captures, LE_EXPR);
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
	      tree res = generic_simplify_327 (loc, type, _p0, _p1, captures, LE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
	      tree res = generic_simplify_327 (loc, type, _p0, _p1, captures, LE_EXPR);
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
				      tree res = generic_simplify_196 (loc, type, _p0, _p1, captures, LE_EXPR);
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
			  tree res = generic_simplify_196 (loc, type, _p0, _p1, captures, LE_EXPR);
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
		      tree res = generic_simplify_199 (loc, type, _p0, _p1, captures, LE_EXPR);
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
		    tree res = generic_simplify_328 (loc, type, _p0, _p1, captures, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
		    tree res = generic_simplify_329 (loc, type, _p0, _p1, captures, LE_EXPR);
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
		    tree res = generic_simplify_203 (loc, type, _p0, _p1, captures, LE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q51 };
		    tree res = generic_simplify_204 (loc, type, _p0, _p1, captures, LE_EXPR);
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
	      tree res = generic_simplify_327 (loc, type, _p0, _p1, captures, GE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q31, _p0 };
	      tree res = generic_simplify_327 (loc, type, _p0, _p1, captures, GE_EXPR);
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
	      tree res = generic_simplify_202 (loc, type, _p0, _p1, captures, LE_EXPR);
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
	      tree res = generic_simplify_202 (loc, type, _p0, _p1, captures, GE_EXPR);
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
		  tree res = generic_simplify_330 (loc, type, _p0, _p1, captures, LE_EXPR, BIT_AND_EXPR);
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
		  tree res = generic_simplify_330 (loc, type, _p0, _p1, captures, LE_EXPR, BIT_AND_EXPR);
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
		  tree res = generic_simplify_330 (loc, type, _p0, _p1, captures, GE_EXPR, BIT_IOR_EXPR);
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
		  tree res = generic_simplify_330 (loc, type, _p0, _p1, captures, GE_EXPR, BIT_IOR_EXPR);
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
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MAX_EXPR, LE_EXPR, GE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MAX_EXPR, LE_EXPR, GE_EXPR);
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
		      tree res = generic_simplify_331 (loc, type, _p0, _p1, captures, MAX_EXPR, LE_EXPR, BIT_AND_EXPR);
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
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MIN_EXPR, GE_EXPR, LE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MIN_EXPR, GE_EXPR, LE_EXPR);
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
	      tree res = generic_simplify_332 (loc, type, _p0, _p1, captures, MAX_EXPR, LE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	      tree res = generic_simplify_332 (loc, type, _p0, _p1, captures, MAX_EXPR, LE_EXPR);
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
	      tree res = generic_simplify_332 (loc, type, _p0, _p1, captures, MIN_EXPR, GE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_332 (loc, type, _p0, _p1, captures, MIN_EXPR, GE_EXPR);
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
		      tree res = generic_simplify_331 (loc, type, _p0, _p1, captures, MIN_EXPR, LE_EXPR, BIT_IOR_EXPR);
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
 && (TREE_CODE_CLASS (LE_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1678;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, LE_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1678;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, LE_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1678;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1678:;
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
 && (TREE_CODE_CLASS (LE_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1679;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1679;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, LE_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1679;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, LE_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1679;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1679:;
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
 && (TREE_CODE_CLASS (LE_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1680;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1680;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, LE_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1680;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, LE_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1680;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1680:;
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
      tree res = generic_simplify_350 (loc, type, _p0, _p1, captures, LE_EXPR, LT_EXPR);
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
		    tree res = generic_simplify_232 (loc, type, _p0, _p1, captures, LE_EXPR, GE_EXPR);
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
	tree res = generic_simplify_234 (loc, type, _p0, _p1, captures, LE_EXPR);
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
		tree res = generic_simplify_236 (loc, type, _p0, _p1, captures, LE_EXPR);
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
		    tree res = generic_simplify_236 (loc, type, _p0, _p1, captures, LE_EXPR);
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
	    tree res = generic_simplify_237 (loc, type, _p0, _p1, captures, LE_EXPR, GE_EXPR);
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
		    tree res = generic_simplify_238 (loc, type, _p0, _p1, captures, LE_EXPR);
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
			tree res = generic_simplify_238 (loc, type, _p0, _p1, captures, LE_EXPR);
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
		tree res = generic_simplify_237 (loc, type, _p0, _p1, captures, LE_EXPR, GE_EXPR);
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
	  tree res = generic_simplify_239 (loc, type, _p0, _p1, captures, LE_EXPR);
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
		      tree res = generic_simplify_240 (loc, type, _p0, _p1, captures, PLUS_EXPR, LE_EXPR);
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
		    tree res = generic_simplify_334 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
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
		      tree res = generic_simplify_240 (loc, type, _p0, _p1, captures, MINUS_EXPR, LE_EXPR);
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
		      tree res = generic_simplify_241 (loc, type, _p0, _p1, captures, LE_EXPR);
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
		tree res = generic_simplify_244 (loc, type, _p0, _p1, captures, LE_EXPR, LE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case REAL_CST:
	    {
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		tree res = generic_simplify_245 (loc, type, _p0, _p1, captures, LE_EXPR, LE_EXPR);
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
		      tree res = generic_simplify_335 (loc, type, _p0, _p1, captures, LE_EXPR);
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
			    tree res = generic_simplify_336 (loc, type, _p0, _p1, captures, LE_EXPR, GE_EXPR);
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
		tree res = generic_simplify_249 (loc, type, _p0, _p1, captures, LE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _p1 };
	  tree res = generic_simplify_250 (loc, type, _p0, _p1, captures, LE_EXPR);
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
			    tree res = generic_simplify_267 (loc, type, _p0, _p1, captures, LE_EXPR);
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
		      tree res = generic_simplify_267 (loc, type, _p0, _p1, captures, LE_EXPR);
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
	      tree res = generic_simplify_337 (loc, type, _p0, _p1, captures, LE_EXPR);
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
		      tree res = generic_simplify_351 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
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
		tree res = generic_simplify_247 (loc, type, _p0, _p1, captures, LE_EXPR, GE_EXPR);
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
	    tree res = generic_simplify_248 (loc, type, _p0, _p1, captures, LE_EXPR, GE_EXPR);
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
		      tree res = generic_simplify_268 (loc, type, _p0, _p1, captures, LE_EXPR);
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
		tree res = generic_simplify_268 (loc, type, _p0, _p1, captures, LE_EXPR);
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
		      tree res = generic_simplify_242 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTF, LE_EXPR);
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
			    tree res = generic_simplify_243 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTF, LE_EXPR);
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
		      tree res = generic_simplify_242 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTL, LE_EXPR);
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
			    tree res = generic_simplify_243 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTL, LE_EXPR);
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
		      tree res = generic_simplify_242 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRT, LE_EXPR);
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
			    tree res = generic_simplify_243 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRT, LE_EXPR);
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
		      tree res = generic_simplify_242 (loc, type, _p0, _p1, captures, CFN_SQRT, LE_EXPR);
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
			    tree res = generic_simplify_243 (loc, type, _p0, _p1, captures, CFN_SQRT, LE_EXPR);
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
	tree res = generic_simplify_338 (loc, type, _p0, _p1, captures, LE_EXPR);
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
				tree res = generic_simplify_339 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
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
				tree res = generic_simplify_339 (loc, type, _p0, _p1, captures, GE_EXPR, LE_EXPR);
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
		    tree res = generic_simplify_334 (loc, type, _p0, _p1, captures, GE_EXPR, LE_EXPR);
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
	      tree res = generic_simplify_340 (loc, type, _p0, _p1, captures, LE_EXPR);
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
			tree res = generic_simplify_352 (loc, type, _p0, _p1, captures, GE_EXPR);
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
			  tree res = generic_simplify_353 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_354 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _p0, _q40 };
			  tree res = generic_simplify_354 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
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
	      tree res = generic_simplify_355 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
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
		      tree res = generic_simplify_345 (loc, type, _p0, _p1, captures, LE_EXPR, GE_EXPR);
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
		      tree res = generic_simplify_346 (loc, type, _p0, _p1, captures, PLUS_EXPR, MINUS_EXPR, LE_EXPR, GE_EXPR);
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
		      tree res = generic_simplify_346 (loc, type, _p0, _p1, captures, MINUS_EXPR, PLUS_EXPR, LE_EXPR, GE_EXPR);
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
		      tree res = generic_simplify_274 (loc, type, _p0, _p1, captures, LE_EXPR, GE_EXPR);
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
		      tree res = generic_simplify_347 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR, CFN_BUILT_IN_CTZ);
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
		      tree res = generic_simplify_356 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR, NE_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_FFS);
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
		      tree res = generic_simplify_356 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR, NE_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_FFSIMAX);
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
		    tree res = generic_simplify_279 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR, CFN_BUILT_IN_POPCOUNT);
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
		      tree res = generic_simplify_347 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR, CFN_BUILT_IN_CTZL);
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
		      tree res = generic_simplify_356 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR, NE_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_FFSL);
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
		    tree res = generic_simplify_279 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR, CFN_BUILT_IN_POPCOUNTLL);
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
		      tree res = generic_simplify_347 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR, CFN_BUILT_IN_CTZIMAX);
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
		    tree res = generic_simplify_279 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR, CFN_POPCOUNT);
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
		      tree res = generic_simplify_348 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
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
		      tree res = generic_simplify_347 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR, CFN_CTZ);
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
		      tree res = generic_simplify_356 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR, NE_EXPR, BIT_IOR_EXPR, CFN_FFS);
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
		    tree res = generic_simplify_279 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR, CFN_BUILT_IN_POPCOUNTIMAX);
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
		    tree res = generic_simplify_279 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR, CFN_BUILT_IN_POPCOUNTL);
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
		      tree res = generic_simplify_347 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR, CFN_BUILT_IN_CTZLL);
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
		      tree res = generic_simplify_356 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR, NE_EXPR, BIT_IOR_EXPR, CFN_BUILT_IN_FFSLL);
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
generic_simplify_GE_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
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
		    tree res = generic_simplify_322 (loc, type, _p0, _p1, captures, GE_EXPR, LE_EXPR);
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
	      tree res = generic_simplify_323 (loc, type, _p0, _p1, captures, GE_EXPR);
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
		    tree res = generic_simplify_324 (loc, type, _p0, _p1, captures, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
		    tree res = generic_simplify_324 (loc, type, _p0, _p1, captures, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
		    tree res = generic_simplify_324 (loc, type, _p0, _p1, captures, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
		    tree res = generic_simplify_324 (loc, type, _p0, _p1, captures, GE_EXPR);
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
				      tree res = generic_simplify_196 (loc, type, _p0, _p1, captures, GE_EXPR);
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
			  tree res = generic_simplify_196 (loc, type, _p0, _p1, captures, GE_EXPR);
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
		      tree res = generic_simplify_199 (loc, type, _p0, _p1, captures, GE_EXPR);
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
		    tree res = generic_simplify_325 (loc, type, _p0, _p1, captures, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
		    tree res = generic_simplify_325 (loc, type, _p0, _p1, captures, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q50 };
		    tree res = generic_simplify_325 (loc, type, _p0, _p1, captures, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _q51 };
		    tree res = generic_simplify_325 (loc, type, _p0, _p1, captures, GE_EXPR);
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
			  tree res = generic_simplify_326 (loc, type, _p0, _p1, captures, GE_EXPR);
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
	      tree res = generic_simplify_327 (loc, type, _p0, _p1, captures, GE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
	      tree res = generic_simplify_327 (loc, type, _p0, _p1, captures, GE_EXPR);
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
		    tree res = generic_simplify_328 (loc, type, _p0, _p1, captures, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q51 };
		    tree res = generic_simplify_329 (loc, type, _p0, _p1, captures, GE_EXPR);
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
		    tree res = generic_simplify_203 (loc, type, _p0, _p1, captures, GE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q51 };
		    tree res = generic_simplify_204 (loc, type, _p0, _p1, captures, GE_EXPR);
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
	      tree res = generic_simplify_327 (loc, type, _p0, _p1, captures, LE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q31, _p0 };
	      tree res = generic_simplify_327 (loc, type, _p0, _p1, captures, LE_EXPR);
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
	      tree res = generic_simplify_202 (loc, type, _p0, _p1, captures, GE_EXPR);
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
	      tree res = generic_simplify_202 (loc, type, _p0, _p1, captures, LE_EXPR);
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
		  tree res = generic_simplify_330 (loc, type, _p0, _p1, captures, GE_EXPR, BIT_IOR_EXPR);
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
		  tree res = generic_simplify_330 (loc, type, _p0, _p1, captures, GE_EXPR, BIT_IOR_EXPR);
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
		  tree res = generic_simplify_330 (loc, type, _p0, _p1, captures, LE_EXPR, BIT_AND_EXPR);
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
		  tree res = generic_simplify_330 (loc, type, _p0, _p1, captures, LE_EXPR, BIT_AND_EXPR);
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
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MIN_EXPR, GE_EXPR, LE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MIN_EXPR, GE_EXPR, LE_EXPR);
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
		      tree res = generic_simplify_331 (loc, type, _p0, _p1, captures, MIN_EXPR, GE_EXPR, BIT_AND_EXPR);
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
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MAX_EXPR, LE_EXPR, GE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	      tree res = generic_simplify_212 (loc, type, _p0, _p1, captures, MAX_EXPR, LE_EXPR, GE_EXPR);
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
	      tree res = generic_simplify_332 (loc, type, _p0, _p1, captures, MIN_EXPR, GE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	      tree res = generic_simplify_332 (loc, type, _p0, _p1, captures, MIN_EXPR, GE_EXPR);
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
	      tree res = generic_simplify_332 (loc, type, _p0, _p1, captures, MAX_EXPR, LE_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_332 (loc, type, _p0, _p1, captures, MAX_EXPR, LE_EXPR);
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
		      tree res = generic_simplify_331 (loc, type, _p0, _p1, captures, MAX_EXPR, GE_EXPR, BIT_IOR_EXPR);
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
 && (TREE_CODE_CLASS (GE_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1685;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, GE_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1685;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, GE_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1685;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1685:;
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
 && (TREE_CODE_CLASS (GE_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1686;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1686;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, GE_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1686;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, GE_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1686;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1686:;
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
 && (TREE_CODE_CLASS (GE_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1687;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1687;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, GE_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1687;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, GE_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1687;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1687:;
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
      tree res = generic_simplify_333 (loc, type, _p0, _p1, captures, GE_EXPR, GT_EXPR);
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
		    tree res = generic_simplify_232 (loc, type, _p0, _p1, captures, GE_EXPR, LE_EXPR);
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
	tree res = generic_simplify_234 (loc, type, _p0, _p1, captures, GE_EXPR);
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
		tree res = generic_simplify_236 (loc, type, _p0, _p1, captures, GE_EXPR);
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
		    tree res = generic_simplify_236 (loc, type, _p0, _p1, captures, GE_EXPR);
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
	    tree res = generic_simplify_237 (loc, type, _p0, _p1, captures, GE_EXPR, LE_EXPR);
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
		    tree res = generic_simplify_238 (loc, type, _p0, _p1, captures, GE_EXPR);
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
			tree res = generic_simplify_238 (loc, type, _p0, _p1, captures, GE_EXPR);
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
		tree res = generic_simplify_237 (loc, type, _p0, _p1, captures, GE_EXPR, LE_EXPR);
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
	  tree res = generic_simplify_239 (loc, type, _p0, _p1, captures, GE_EXPR);
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
		      tree res = generic_simplify_240 (loc, type, _p0, _p1, captures, PLUS_EXPR, GE_EXPR);
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
		    tree res = generic_simplify_334 (loc, type, _p0, _p1, captures, GE_EXPR, LE_EXPR);
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
		      tree res = generic_simplify_240 (loc, type, _p0, _p1, captures, MINUS_EXPR, GE_EXPR);
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
		      tree res = generic_simplify_241 (loc, type, _p0, _p1, captures, GE_EXPR);
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
		tree res = generic_simplify_244 (loc, type, _p0, _p1, captures, GE_EXPR, GE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case REAL_CST:
	    {
	      {
		tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		tree res = generic_simplify_245 (loc, type, _p0, _p1, captures, GE_EXPR, GE_EXPR);
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
		      tree res = generic_simplify_335 (loc, type, _p0, _p1, captures, GE_EXPR);
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
			    tree res = generic_simplify_336 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR);
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
		tree res = generic_simplify_249 (loc, type, _p0, _p1, captures, GE_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _p1 };
	  tree res = generic_simplify_250 (loc, type, _p0, _p1, captures, GE_EXPR);
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
			    tree res = generic_simplify_267 (loc, type, _p0, _p1, captures, GE_EXPR);
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
		      tree res = generic_simplify_267 (loc, type, _p0, _p1, captures, GE_EXPR);
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
	      tree res = generic_simplify_337 (loc, type, _p0, _p1, captures, GE_EXPR);
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
		tree res = generic_simplify_247 (loc, type, _p0, _p1, captures, GE_EXPR, LE_EXPR);
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
	    tree res = generic_simplify_248 (loc, type, _p0, _p1, captures, GE_EXPR, LE_EXPR);
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
		      tree res = generic_simplify_268 (loc, type, _p0, _p1, captures, GE_EXPR);
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
		tree res = generic_simplify_268 (loc, type, _p0, _p1, captures, GE_EXPR);
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
		      tree res = generic_simplify_242 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTF, GE_EXPR);
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
			    tree res = generic_simplify_243 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTF, GE_EXPR);
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
		      tree res = generic_simplify_242 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTL, GE_EXPR);
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
			    tree res = generic_simplify_243 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRTL, GE_EXPR);
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
		      tree res = generic_simplify_242 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRT, GE_EXPR);
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
			    tree res = generic_simplify_243 (loc, type, _p0, _p1, captures, CFN_BUILT_IN_SQRT, GE_EXPR);
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
		      tree res = generic_simplify_242 (loc, type, _p0, _p1, captures, CFN_SQRT, GE_EXPR);
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
			    tree res = generic_simplify_243 (loc, type, _p0, _p1, captures, CFN_SQRT, GE_EXPR);
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
	tree res = generic_simplify_338 (loc, type, _p0, _p1, captures, GE_EXPR);
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
				tree res = generic_simplify_339 (loc, type, _p0, _p1, captures, GE_EXPR, LE_EXPR);
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
				tree res = generic_simplify_339 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
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
		    tree res = generic_simplify_334 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
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
	      tree res = generic_simplify_340 (loc, type, _p0, _p1, captures, LE_EXPR);
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
			tree res = generic_simplify_341 (loc, type, _p0, _p1, captures, GE_EXPR);
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
			  tree res = generic_simplify_342 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_343 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q31, _q30 };
			  tree res = generic_simplify_343 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
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
	      tree res = generic_simplify_344 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
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
		      tree res = generic_simplify_345 (loc, type, _p0, _p1, captures, GE_EXPR, LE_EXPR);
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
		      tree res = generic_simplify_346 (loc, type, _p0, _p1, captures, PLUS_EXPR, MINUS_EXPR, GE_EXPR, LE_EXPR);
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
		      tree res = generic_simplify_346 (loc, type, _p0, _p1, captures, MINUS_EXPR, PLUS_EXPR, GE_EXPR, LE_EXPR);
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
		      tree res = generic_simplify_274 (loc, type, _p0, _p1, captures, GE_EXPR, LE_EXPR);
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
		      tree res = generic_simplify_347 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR, CFN_BUILT_IN_CTZ);
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
		      tree res = generic_simplify_347 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR, CFN_BUILT_IN_CTZL);
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
		      tree res = generic_simplify_347 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR, CFN_BUILT_IN_CTZIMAX);
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
		      tree res = generic_simplify_347 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR, CFN_CTZ);
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
		      tree res = generic_simplify_348 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
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
		      tree res = generic_simplify_347 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR, CFN_BUILT_IN_CTZLL);
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
generic_simplify_MAX_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_455 (loc, type, _p0, _p1, captures, MAX_EXPR);
	if (res) return res;
      }
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
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
	      tree res = generic_simplify_456 (loc, type, _p0, _p1, captures, MAX_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
	      tree res = generic_simplify_456 (loc, type, _p0, _p1, captures, MAX_EXPR);
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
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p1, _p0, _q31 };
	      tree res = generic_simplify_457 (loc, type, _p0, _p1, captures, MAX_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[3] ATTRIBUTE_UNUSED = { _p1, _p0, _q30 };
	      tree res = generic_simplify_457 (loc, type, _p0, _p1, captures, MAX_EXPR);
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
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_466 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_466 (loc, type, _p0, _p1, captures);
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
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q30, _p0 };
	      tree res = generic_simplify_466 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q31, _p0 };
	      tree res = generic_simplify_466 (loc, type, _p0, _p1, captures);
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
	      tree res = generic_simplify_467 (loc, type, _p0, _p1, captures);
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
	      tree res = generic_simplify_467 (loc, type, _p0, _p1, captures);
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
 && TYPE_MAX_VALUE (type)
 && operand_equal_p (captures[1], TYPE_MAX_VALUE (type), OEP_ONLY_CONST)
)
      {
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1740;
	{
	  tree _r;
	  _r = captures[1];
	  if (TREE_SIDE_EFFECTS (captures[0]))
	    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1740:;
      }
    else
      {
	if (INTEGRAL_TYPE_P (type)
 && TYPE_MIN_VALUE (type)
 && operand_equal_p (captures[1], TYPE_MIN_VALUE (type), OEP_ONLY_CONST)
)
	  {
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1741;
	    {
	      tree _r;
	      _r = captures[0];
	      if (TREE_SIDE_EFFECTS (captures[1]))
		_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
	      return _r;
	    }
next_after_fail1741:;
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
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1742;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1742;
			    {
			      tree _r;
			      _r = captures[1];
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1742:;
			  }
			else
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1743;
			    {
			      tree _r;
			      _r = captures[0];
			      if (TREE_SIDE_EFFECTS (captures[2]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1001, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1743:;
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
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1744;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1744;
			    {
			      tree _r;
			      _r = captures[1];
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1744:;
			  }
			else
			  {
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1745;
			    {
			      tree _r;
			      _r = captures[0];
			      if (TREE_SIDE_EFFECTS (captures[2]))
				_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[2]), _r);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1001, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1745:;
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
			    tree res = generic_simplify_460 (loc, type, _p0, _p1, captures, MAX_EXPR, GT_EXPR);
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
		      tree res = generic_simplify_461 (loc, type, _p0, _p1, captures, MAX_EXPR, GT_EXPR);
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
		      tree res = generic_simplify_462 (loc, type, _p0, _p1, captures, MAX_EXPR, GT_EXPR);
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
		tree res = generic_simplify_463 (loc, type, _p0, _p1, captures, MAX_EXPR, GT_EXPR);
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
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1746;
		    {
		      tree res_op0;
		      {
			tree _o1[2], _r1;
			_o1[0] = captures[1];
			_o1[1] = captures[3];
			_r1 = fold_build2_loc (loc, MIN_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			res_op0 = _r1;
		      }
		      tree _r;
		      _r = fold_build1_loc (loc, NEGATE_EXPR, type, res_op0);
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 997, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1746:;
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
		tree res = generic_simplify_464 (loc, type, _p0, _p1, captures, MAX_EXPR, MIN_EXPR);
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
 && (TREE_CODE_CLASS (MAX_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1747;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1747;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1747;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1747:;
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
 && (TREE_CODE_CLASS (MAX_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1748;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1748;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1748;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1748;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1748:;
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
 && (TREE_CODE_CLASS (MAX_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1749;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1749;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1749;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, MAX_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1749;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1749:;
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
	    tree res = generic_simplify_465 (loc, type, _p0, _p1, captures, MAX_EXPR, BIT_IOR_EXPR);
	    if (res) return res;
	  }
        }
    }
  return NULL_TREE;
}

tree
generic_simplify_RROTATE_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_all_onesp (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	tree res = generic_simplify_498 (loc, type, _p0, _p1, captures, RROTATE_EXPR);
	if (res) return res;
      }
    }
  if (integer_zerop (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_361 (loc, type, _p0, _p1, captures, RROTATE_EXPR);
	if (res) return res;
      }
    }
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	tree res = generic_simplify_362 (loc, type, _p0, _p1, captures, RROTATE_EXPR);
	if (res) return res;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case VECTOR_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  tree res = generic_simplify_363 (loc, type, _p0, _p1, captures, RROTATE_EXPR);
	  if (res) return res;
	}
        break;
      }
    case CONSTRUCTOR:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  tree res = generic_simplify_364 (loc, type, _p0, _p1, captures, RROTATE_EXPR);
	  if (res) return res;
	}
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case RROTATE_EXPR:
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
		      tree res = generic_simplify_365 (loc, type, _p0, _p1, captures, RROTATE_EXPR);
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
generic_simplify_COMPLEX_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case REALPART_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	switch (TREE_CODE (_p1))
	  {
	  case IMAGPART_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_p1, 0);
	      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _q20, 0) && types_match (_q40, _q20)))
		{
		  {
		    tree captures[1] ATTRIBUTE_UNUSED = { _q20 };
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1751;
		    {
		      tree _r;
		      _r = captures[0];
		      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1003, __FILE__, __LINE__, true);
		      return _r;
		    }
next_after_fail1751:;
		  }
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
generic_simplify_ORDERED_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
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
 && (TREE_CODE_CLASS (ORDERED_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1757;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, ORDERED_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1757;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, ORDERED_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1757;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1757:;
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
 && (TREE_CODE_CLASS (ORDERED_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1758;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1758;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, ORDERED_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1758;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, ORDERED_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1758;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1758:;
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
 && (TREE_CODE_CLASS (ORDERED_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1759;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1759;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, ORDERED_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1759;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, ORDERED_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1759;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1759:;
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
		tree res = generic_simplify_244 (loc, type, _p0, _p1, captures, ORDERED_EXPR, ORDERED_EXPR);
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
		tree res = generic_simplify_247 (loc, type, _p0, _p1, captures, ORDERED_EXPR, ORDERED_EXPR);
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
	    tree res = generic_simplify_248 (loc, type, _p0, _p1, captures, ORDERED_EXPR, ORDERED_EXPR);
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
	  tree res = generic_simplify_519 (loc, type, _p0, _p1, captures, ORDERED_EXPR);
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
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1760;
	{
	  tree _r;
	  _r =  constant_boolean_node (false, type);
	  if (TREE_SIDE_EFFECTS (captures[0]))
	    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	  if (TREE_SIDE_EFFECTS (captures[1]))
	    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1006, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1760:;
      }
    else
      {
	if (!tree_expr_maybe_nan_p (captures[0]) && !tree_expr_maybe_nan_p (captures[1])
)
	  {
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1761;
	    {
	      tree _r;
	      _r =  constant_boolean_node (true, type);
	      if (TREE_SIDE_EFFECTS (captures[0]))
		_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	      if (TREE_SIDE_EFFECTS (captures[1]))
		_r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 1007, __FILE__, __LINE__, true);
	      return _r;
	    }
next_after_fail1761:;
	  }
      }
  }
  return NULL_TREE;
}

tree
generic_simplify_UNGE_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
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
 && (TREE_CODE_CLASS (UNGE_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1771;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, UNGE_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1771;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, UNGE_EXPR, type, _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1771;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1771:;
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
 && (TREE_CODE_CLASS (UNGE_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1772;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1772;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, UNGE_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1772;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, UNGE_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1772;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1772:;
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
 && (TREE_CODE_CLASS (UNGE_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1773;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1773;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, UNGE_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1773;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, UNGE_EXPR, type, _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1773;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1773:;
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
	tree res = generic_simplify_521 (loc, type, _p0, _p1, captures, UNGE_EXPR);
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
		tree res = generic_simplify_244 (loc, type, _p0, _p1, captures, UNGE_EXPR, GE_EXPR);
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
		tree res = generic_simplify_247 (loc, type, _p0, _p1, captures, UNGE_EXPR, UNLE_EXPR);
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
	    tree res = generic_simplify_248 (loc, type, _p0, _p1, captures, UNGE_EXPR, UNLE_EXPR);
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
	  tree res = generic_simplify_519 (loc, type, _p0, _p1, captures, UNGE_EXPR);
	  if (res) return res;
	}
        break;
      }
    default:;
    }
  return NULL_TREE;
}

tree
generic_simplify_TRUTH_OR_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
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
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1797;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1797;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1797;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1797;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1797;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1797;
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
next_after_fail1797:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1798;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1798;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1798;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1798;
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
next_after_fail1798:;
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
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1799;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1799;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1799;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1799;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1799;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1799;
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
next_after_fail1799:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1800;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1800;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1800;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1800;
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
next_after_fail1800:;
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
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1801;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1801;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1801;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1801;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1801;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1801;
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
next_after_fail1801:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1802;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1802;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1802;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1802;
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
next_after_fail1802:;
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
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1803;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1803;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1803;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1803;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1803;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1803;
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
next_after_fail1803:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1804;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1804;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1804;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1804;
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
next_after_fail1804:;
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
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1805;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1805;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1805;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1805;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1805;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1805;
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
next_after_fail1805:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1806;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1806;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1806;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1806;
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
next_after_fail1806:;
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
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1807;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1807;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1807;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1807;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1807;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1807;
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
next_after_fail1807:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1808;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1808;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1808;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1808;
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
next_after_fail1808:;
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
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1809;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1809;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1809;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1809;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1809;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1809;
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
next_after_fail1809:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1810;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1810;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1810;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1810;
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
next_after_fail1810:;
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
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1811;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1811;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1811;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1811;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1811;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1811;
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
next_after_fail1811:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1812;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1812;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1812;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1812;
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
next_after_fail1812:;
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
