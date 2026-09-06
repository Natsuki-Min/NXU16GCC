/* Generated automatically by the program `genmatch' from
   a IL pattern matching and simplification description.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-function"

#include "generic-match-auto.h"

bool
tree_logical_inverted_value (tree t, tree *res_ops)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (t)) return false;
  switch (TREE_CODE (t))
    {
    case TRUTH_NOT_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	{
	  tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	  {
	    res_ops[0] = captures[0];
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 23, __FILE__, __LINE__, false);
	    return true;
	  }
	}
        break;
      }
    case BIT_NOT_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	if (tree_truth_valued_p (_p0))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	      {
		res_ops[0] = captures[0];
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 24, __FILE__, __LINE__, false);
		return true;
	      }
	    }
	  }
        break;
      }
    case EQ_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	if (integer_zerop (_p1))
	  {
	    {
	      tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	      {
		res_ops[0] = captures[0];
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 25, __FILE__, __LINE__, false);
		return true;
	      }
	    }
	  }
        break;
      }
    case NE_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	if (tree_truth_valued_p (_p0))
	  {
	    if (integer_truep (_p1))
	      {
		{
		  tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
		  {
		    res_ops[0] = captures[0];
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 26, __FILE__, __LINE__, false);
		    return true;
		  }
		}
	      }
	  }
        break;
      }
    case BIT_XOR_EXPR:
      {
	tree _p0 = TREE_OPERAND (t, 0);
	tree _p1 = TREE_OPERAND (t, 1);
	if (tree_truth_valued_p (_p0))
	  {
	    if (integer_truep (_p1))
	      {
		{
		  tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
		  {
		    res_ops[0] = captures[0];
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 27, __FILE__, __LINE__, false);
		    return true;
		  }
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
tree_signed_integer_sat_sub (tree t, tree *res_ops)
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
			  switch (TREE_CODE (_q31))
			    {
			    case BIT_XOR_EXPR:
			      {
				tree _q70 = TREE_OPERAND (_q31, 0);
				tree _q71 = TREE_OPERAND (_q31, 1);
				if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
				  {
				    {
				      tree _q71_pops[1];
				      if (tree_nop_convert (_q71, _q71_pops))
				        {
					  tree _q90 = _q71_pops[0];
					  switch (TREE_CODE (_q90))
					    {
					    case MINUS_EXPR:
					      {
						tree _q100 = TREE_OPERAND (_q90, 0);
						tree _q101 = TREE_OPERAND (_q90, 1);
						{
						  tree _q100_pops[1];
						  if (tree_nop_convert (_q100, _q100_pops))
						    {
						      tree _q110 = _q100_pops[0];
						      if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
							{
							  {
							    tree _q101_pops[1];
							    if (tree_nop_convert (_q101, _q101_pops))
							      {
								tree _q130 = _q101_pops[0];
								if ((_q130 == _q41 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q41, 0) && types_match (_q130, _q41)))
								  {
								    if (integer_zerop (_q21))
								      {
									switch (TREE_CODE (_p1))
									  {
									  case BIT_XOR_EXPR:
									    {
									      tree _q160 = TREE_OPERAND (_p1, 0);
									      tree _q161 = TREE_OPERAND (_p1, 1);
									      switch (TREE_CODE (_q160))
									        {
										case NEGATE_EXPR:
										  {
										    tree _q170 = TREE_OPERAND (_q160, 0);
										    switch (TREE_CODE (_q170))
										      {
										      CASE_CONVERT:
										        {
											  tree _q180 = TREE_OPERAND (_q170, 0);
											  switch (TREE_CODE (_q180))
											    {
											    case LT_EXPR:
											      {
												tree _q190 = TREE_OPERAND (_q180, 0);
												tree _q191 = TREE_OPERAND (_q180, 1);
												if ((_q190 == _q40 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q40, 0) && types_match (_q190, _q40)))
												  {
												    if (integer_zerop (_q191))
												      {
													if (tree_max_value (_q161))
													  {
													    if ((_p2 == _q71 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q71, 0) && types_match (_p2, _q71)))
													      {
														{
														  tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _q71 };
														  if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
														    {
														      {
															res_ops[0] = captures[0];
															res_ops[1] = captures[1];
															if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 71, __FILE__, __LINE__, false);
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
									    if (tree_max_value (_q160))
									      {
										switch (TREE_CODE (_q161))
										  {
										  case NEGATE_EXPR:
										    {
										      tree _q180 = TREE_OPERAND (_q161, 0);
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
													  if ((_p2 == _q71 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q71, 0) && types_match (_p2, _q71)))
													    {
													      {
														tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _q71 };
														if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
														  {
														    {
														      res_ops[0] = captures[0];
														      res_ops[1] = captures[1];
														      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 71, __FILE__, __LINE__, false);
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
				if ((_q70 == _q41 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q41, 0) && types_match (_q70, _q41)))
				  {
				    {
				      tree _q71_pops[1];
				      if (tree_nop_convert (_q71, _q71_pops))
				        {
					  tree _q90 = _q71_pops[0];
					  switch (TREE_CODE (_q90))
					    {
					    case MINUS_EXPR:
					      {
						tree _q100 = TREE_OPERAND (_q90, 0);
						tree _q101 = TREE_OPERAND (_q90, 1);
						{
						  tree _q100_pops[1];
						  if (tree_nop_convert (_q100, _q100_pops))
						    {
						      tree _q110 = _q100_pops[0];
						      if ((_q110 == _q41 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q41, 0) && types_match (_q110, _q41)))
							{
							  {
							    tree _q101_pops[1];
							    if (tree_nop_convert (_q101, _q101_pops))
							      {
								tree _q130 = _q101_pops[0];
								if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
								  {
								    if (integer_zerop (_q21))
								      {
									switch (TREE_CODE (_p1))
									  {
									  case BIT_XOR_EXPR:
									    {
									      tree _q160 = TREE_OPERAND (_p1, 0);
									      tree _q161 = TREE_OPERAND (_p1, 1);
									      switch (TREE_CODE (_q160))
									        {
										case NEGATE_EXPR:
										  {
										    tree _q170 = TREE_OPERAND (_q160, 0);
										    switch (TREE_CODE (_q170))
										      {
										      CASE_CONVERT:
										        {
											  tree _q180 = TREE_OPERAND (_q170, 0);
											  switch (TREE_CODE (_q180))
											    {
											    case LT_EXPR:
											      {
												tree _q190 = TREE_OPERAND (_q180, 0);
												tree _q191 = TREE_OPERAND (_q180, 1);
												if ((_q190 == _q41 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q41, 0) && types_match (_q190, _q41)))
												  {
												    if (integer_zerop (_q191))
												      {
													if (tree_max_value (_q161))
													  {
													    if ((_p2 == _q71 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q71, 0) && types_match (_p2, _q71)))
													      {
														{
														  tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _q71 };
														  if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
														    {
														      {
															res_ops[0] = captures[0];
															res_ops[1] = captures[1];
															if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 71, __FILE__, __LINE__, false);
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
									    if (tree_max_value (_q160))
									      {
										switch (TREE_CODE (_q161))
										  {
										  case NEGATE_EXPR:
										    {
										      tree _q180 = TREE_OPERAND (_q161, 0);
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
												  if ((_q200 == _q41 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q41, 0) && types_match (_q200, _q41)))
												    {
												      if (integer_zerop (_q201))
													{
													  if ((_p2 == _q71 && ! TREE_SIDE_EFFECTS (_p2)) || (operand_equal_p (_p2, _q71, 0) && types_match (_p2, _q71)))
													    {
													      {
														tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _q71 };
														if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
														  {
														    {
														      res_ops[0] = captures[0];
														      res_ops[1] = captures[1];
														      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 71, __FILE__, __LINE__, false);
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
			        break;
			      }
			    default:;
			    }
			  {
			    tree _q41_pops[1];
			    if (tree_nop_convert (_q41, _q41_pops))
			      {
				tree _q60 = _q41_pops[0];
				switch (TREE_CODE (_q60))
				  {
				  case MINUS_EXPR:
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
							case BIT_XOR_EXPR:
							  {
							    tree _q120 = TREE_OPERAND (_q31, 0);
							    tree _q121 = TREE_OPERAND (_q31, 1);
							    if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
							      {
								if ((_q121 == _q100 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q100, 0) && types_match (_q121, _q100)))
								  {
								    if (integer_zerop (_q21))
								      {
									switch (TREE_CODE (_p1))
									  {
									  case BIT_XOR_EXPR:
									    {
									      tree _q160 = TREE_OPERAND (_p1, 0);
									      tree _q161 = TREE_OPERAND (_p1, 1);
									      switch (TREE_CODE (_q160))
									        {
										case NEGATE_EXPR:
										  {
										    tree _q170 = TREE_OPERAND (_q160, 0);
										    switch (TREE_CODE (_q170))
										      {
										      CASE_CONVERT:
										        {
											  tree _q180 = TREE_OPERAND (_q170, 0);
											  switch (TREE_CODE (_q180))
											    {
											    case LT_EXPR:
											      {
												tree _q190 = TREE_OPERAND (_q180, 0);
												tree _q191 = TREE_OPERAND (_q180, 1);
												if ((_q190 == _q40 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q40, 0) && types_match (_q190, _q40)))
												  {
												    if (integer_zerop (_q191))
												      {
													if (tree_max_value (_q161))
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
															if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 71, __FILE__, __LINE__, false);
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
									    if (tree_max_value (_q160))
									      {
										switch (TREE_CODE (_q161))
										  {
										  case NEGATE_EXPR:
										    {
										      tree _q180 = TREE_OPERAND (_q161, 0);
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
														      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 71, __FILE__, __LINE__, false);
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
							    if ((_q120 == _q100 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q100, 0) && types_match (_q120, _q100)))
							      {
								if ((_q121 == _q40 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q40, 0) && types_match (_q121, _q40)))
								  {
								    if (integer_zerop (_q21))
								      {
									switch (TREE_CODE (_p1))
									  {
									  case BIT_XOR_EXPR:
									    {
									      tree _q160 = TREE_OPERAND (_p1, 0);
									      tree _q161 = TREE_OPERAND (_p1, 1);
									      switch (TREE_CODE (_q160))
									        {
										case NEGATE_EXPR:
										  {
										    tree _q170 = TREE_OPERAND (_q160, 0);
										    switch (TREE_CODE (_q170))
										      {
										      CASE_CONVERT:
										        {
											  tree _q180 = TREE_OPERAND (_q170, 0);
											  switch (TREE_CODE (_q180))
											    {
											    case LT_EXPR:
											      {
												tree _q190 = TREE_OPERAND (_q180, 0);
												tree _q191 = TREE_OPERAND (_q180, 1);
												if ((_q190 == _q40 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q40, 0) && types_match (_q190, _q40)))
												  {
												    if (integer_zerop (_q191))
												      {
													if (tree_max_value (_q161))
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
															if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 71, __FILE__, __LINE__, false);
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
									    if (tree_max_value (_q160))
									      {
										switch (TREE_CODE (_q161))
										  {
										  case NEGATE_EXPR:
										    {
										      tree _q180 = TREE_OPERAND (_q161, 0);
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
														      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 71, __FILE__, __LINE__, false);
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
			  switch (TREE_CODE (_q31))
			    {
			    case BIT_XOR_EXPR:
			      {
				tree _q70 = TREE_OPERAND (_q31, 0);
				tree _q71 = TREE_OPERAND (_q31, 1);
				if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
				  {
				    {
				      tree _q71_pops[1];
				      if (tree_nop_convert (_q71, _q71_pops))
				        {
					  tree _q90 = _q71_pops[0];
					  switch (TREE_CODE (_q90))
					    {
					    case MINUS_EXPR:
					      {
						tree _q100 = TREE_OPERAND (_q90, 0);
						tree _q101 = TREE_OPERAND (_q90, 1);
						{
						  tree _q100_pops[1];
						  if (tree_nop_convert (_q100, _q100_pops))
						    {
						      tree _q110 = _q100_pops[0];
						      if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
							{
							  {
							    tree _q101_pops[1];
							    if (tree_nop_convert (_q101, _q101_pops))
							      {
								tree _q130 = _q101_pops[0];
								if ((_q130 == _q41 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q41, 0) && types_match (_q130, _q41)))
								  {
								    if (integer_zerop (_q21))
								      {
									if ((_p1 == _q71 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q71, 0) && types_match (_p1, _q71)))
									  {
									    switch (TREE_CODE (_p2))
									      {
									      case BIT_XOR_EXPR:
									        {
										  tree _q170 = TREE_OPERAND (_p2, 0);
										  tree _q171 = TREE_OPERAND (_p2, 1);
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
														{
														  tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _q71 };
														  if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
														    {
														      {
															res_ops[0] = captures[0];
															res_ops[1] = captures[1];
															if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
													      {
														tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _q71 };
														if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
														  {
														    {
														      res_ops[0] = captures[0];
														      res_ops[1] = captures[1];
														      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
				if ((_q70 == _q41 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q41, 0) && types_match (_q70, _q41)))
				  {
				    {
				      tree _q71_pops[1];
				      if (tree_nop_convert (_q71, _q71_pops))
				        {
					  tree _q90 = _q71_pops[0];
					  switch (TREE_CODE (_q90))
					    {
					    case MINUS_EXPR:
					      {
						tree _q100 = TREE_OPERAND (_q90, 0);
						tree _q101 = TREE_OPERAND (_q90, 1);
						{
						  tree _q100_pops[1];
						  if (tree_nop_convert (_q100, _q100_pops))
						    {
						      tree _q110 = _q100_pops[0];
						      if ((_q110 == _q41 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q41, 0) && types_match (_q110, _q41)))
							{
							  {
							    tree _q101_pops[1];
							    if (tree_nop_convert (_q101, _q101_pops))
							      {
								tree _q130 = _q101_pops[0];
								if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
								  {
								    if (integer_zerop (_q21))
								      {
									if ((_p1 == _q71 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q71, 0) && types_match (_p1, _q71)))
									  {
									    switch (TREE_CODE (_p2))
									      {
									      case BIT_XOR_EXPR:
									        {
										  tree _q170 = TREE_OPERAND (_p2, 0);
										  tree _q171 = TREE_OPERAND (_p2, 1);
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
												    if ((_q200 == _q41 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q41, 0) && types_match (_q200, _q41)))
												      {
													if (integer_zerop (_q201))
													  {
													    if (tree_max_value (_q171))
													      {
														{
														  tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _q71 };
														  if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
														    {
														      {
															res_ops[0] = captures[0];
															res_ops[1] = captures[1];
															if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
												      if ((_q210 == _q41 && ! TREE_SIDE_EFFECTS (_q210)) || (operand_equal_p (_q210, _q41, 0) && types_match (_q210, _q41)))
													{
													  if (integer_zerop (_q211))
													    {
													      {
														tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _q71 };
														if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
														  {
														    {
														      res_ops[0] = captures[0];
														      res_ops[1] = captures[1];
														      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
			  {
			    tree _q41_pops[1];
			    if (tree_nop_convert (_q41, _q41_pops))
			      {
				tree _q60 = _q41_pops[0];
				switch (TREE_CODE (_q60))
				  {
				  case MINUS_EXPR:
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
							case BIT_XOR_EXPR:
							  {
							    tree _q120 = TREE_OPERAND (_q31, 0);
							    tree _q121 = TREE_OPERAND (_q31, 1);
							    if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
							      {
								if ((_q121 == _q100 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q100, 0) && types_match (_q121, _q100)))
								  {
								    if (integer_zerop (_q21))
								      {
									if ((_p1 == _q41 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q41, 0) && types_match (_p1, _q41)))
									  {
									    switch (TREE_CODE (_p2))
									      {
									      case BIT_XOR_EXPR:
									        {
										  tree _q170 = TREE_OPERAND (_p2, 0);
										  tree _q171 = TREE_OPERAND (_p2, 1);
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
														{
														  tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
														  if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
														    {
														      {
															res_ops[0] = captures[0];
															res_ops[1] = captures[1];
															if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
													      {
														tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
														if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
														  {
														    {
														      res_ops[0] = captures[0];
														      res_ops[1] = captures[1];
														      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
							    if ((_q120 == _q100 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q100, 0) && types_match (_q120, _q100)))
							      {
								if ((_q121 == _q40 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q40, 0) && types_match (_q121, _q40)))
								  {
								    if (integer_zerop (_q21))
								      {
									if ((_p1 == _q41 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q41, 0) && types_match (_p1, _q41)))
									  {
									    switch (TREE_CODE (_p2))
									      {
									      case BIT_XOR_EXPR:
									        {
										  tree _q170 = TREE_OPERAND (_p2, 0);
										  tree _q171 = TREE_OPERAND (_p2, 1);
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
														{
														  tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
														  if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
														    {
														      {
															res_ops[0] = captures[0];
															res_ops[1] = captures[1];
															if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
													      {
														tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
														if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
														  {
														    {
														      res_ops[0] = captures[0];
														      res_ops[1] = captures[1];
														      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
											  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
											if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
				  case MINUS_EXPR:
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
							    case LT_EXPR:
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
																	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 74, __FILE__, __LINE__, false);
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
																	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 74, __FILE__, __LINE__, false);
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
																	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 74, __FILE__, __LINE__, false);
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
																	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 74, __FILE__, __LINE__, false);
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
						      case MINUS_EXPR:
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
																	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 74, __FILE__, __LINE__, false);
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
																	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 74, __FILE__, __LINE__, false);
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
						      case MINUS_EXPR:
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
																	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 74, __FILE__, __LINE__, false);
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
																	      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 74, __FILE__, __LINE__, false);
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
    default:;
    }
  return false;
}

tree
generic_simplify_325 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[0]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail738;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 = captures[2];
	tree _r;
	_r = fold_build2_loc (loc, op, type, res_op0, res_op1);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 527, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail738:;
    }
  return NULL_TREE;
}

tree
generic_simplify_332 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (minmax),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail746;
  {
    tree _r;
    _r =  constant_boolean_node (cmp == GE_EXPR || cmp == LE_EXPR, type);
    if (TREE_SIDE_EFFECTS (captures[0]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 535, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail746:;
  return NULL_TREE;
}

tree
generic_simplify_339 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail785;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail785;
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
next_after_fail785:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_346 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op),
 const enum tree_code ARG_UNUSED (rop),
 const enum tree_code ARG_UNUSED (cmp),
 const enum tree_code ARG_UNUSED (rcmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TREE_OVERFLOW (captures[2]) && !TREE_OVERFLOW (captures[3])
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[1]))
)
    {
      {
 tree res = int_const_binop (rop, captures[3], captures[2]);
	  if (TREE_OVERFLOW (res)
)
	    {
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail793;
	      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail793;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail793;
	      {
		tree _r;
 fold_overflow_warning (("assuming signed overflow does not occur "
 "when simplifying conditional to constant"),
 WARN_STRICT_OVERFLOW_CONDITIONAL);
 bool less = cmp == LE_EXPR || cmp == LT_EXPR;
 bool ovf_high = wi::lt_p (wi::to_wide (captures[2]), 0,
 TYPE_SIGN (TREE_TYPE (captures[2])))
 != (op == MINUS_EXPR);		_r = 
 constant_boolean_node (less == ovf_high, type);
		if (TREE_SIDE_EFFECTS (captures[1]))
		  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 565, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail793:;
	    }
	  else
	    {
	      if (single_use (captures[0])
)
		{
		  {
 fold_overflow_warning (("assuming signed overflow does not occur "
 "when changing X +- C1 cmp C2 to "
 "X cmp C2 -+ C1"),
 WARN_STRICT_OVERFLOW_COMPARISON);
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail794;
		      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail794;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail794;
		      {
			tree res_op0;
			res_op0 = captures[1];
			tree res_op1;
			res_op1 =  res;
			tree _r;
			_r = fold_build2_loc (loc, cmp, type, res_op0, res_op1);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 566, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail794:;
		  }
		}
	    }
      }
    }
  else
    {
      if (TYPE_OVERFLOW_WRAPS (TREE_TYPE (captures[1]))
)
	{
	  {
 wide_int max = wi::max_value (TREE_TYPE (captures[1]));
 wide_int min = wi::min_value (TREE_TYPE (captures[1]));
 wide_int c2 = rop == PLUS_EXPR
 ? wi::add (wi::to_wide (captures[3]), wi::to_wide (captures[2]))
 : wi::sub (wi::to_wide (captures[3]), wi::to_wide (captures[2]));
	      if (((cmp == LE_EXPR || cmp == GT_EXPR) && wi::eq_p (c2, max))
 || ((cmp == LT_EXPR || cmp == GE_EXPR) && wi::eq_p (c2, min))
)
		{
		  {
 wide_int c1 = rop == PLUS_EXPR
 ? wi::add (wi::bit_not (c2), wi::to_wide (captures[2]))
 : wi::sub (wi::bit_not (c2), wi::to_wide (captures[2]));
 tree c1_cst = wide_int_to_tree (TREE_TYPE (captures[1]), c1);
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail795;
		      if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail795;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail795;
		      {
			tree res_op0;
			res_op0 = captures[1];
			tree res_op1;
			res_op1 =  c1_cst;
			tree _r;
			_r = fold_build2_loc (loc, rcmp, type, res_op0, res_op1);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 567, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail795:;
		  }
		}
	  }
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_361 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (shiftrotate))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail818;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail818;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree _r;
    _r = non_lvalue_loc (loc, res_op0);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 586, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail818:;
  return NULL_TREE;
}

tree
generic_simplify_365 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 unsigned int prec = element_precision (type);
      if (wi::ge_p (wi::to_wide (captures[1]), 0, TYPE_SIGN (TREE_TYPE (captures[1])))
 && wi::lt_p (wi::to_wide (captures[1]), prec, TYPE_SIGN (TREE_TYPE (captures[1])))
 && wi::ge_p (wi::to_wide (captures[2]), 0, TYPE_SIGN (TREE_TYPE (captures[2])))
 && wi::lt_p (wi::to_wide (captures[2]), prec, TYPE_SIGN (TREE_TYPE (captures[2])))
)
	{
	  {
 unsigned int low = (tree_to_uhwi (captures[1])
 + tree_to_uhwi (captures[2]));
	      if (low >= prec
)
		{
		  if (op == LROTATE_EXPR || op == RROTATE_EXPR
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail822;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail822;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail822;
		      {
			tree res_op0;
			res_op0 = captures[0];
			tree res_op1;
			res_op1 =  build_int_cst (TREE_TYPE (captures[1]), low % prec);
			tree _r;
			_r = fold_build2_loc (loc, op, type, res_op0, res_op1);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 589, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail822:;
		    }
		  else
		    {
		      if (TYPE_UNSIGNED (type) || op == LSHIFT_EXPR
)
			{
			  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail823;
			  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail823;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail823;
			  {
			    tree _r;
			    _r =  build_zero_cst (type);
			    if (TREE_SIDE_EFFECTS (captures[0]))
			      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 590, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail823:;
			}
		      else
			{
			  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail824;
			  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail824;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail824;
			  {
			    tree res_op0;
			    res_op0 = captures[0];
			    tree res_op1;
			    res_op1 =  build_int_cst (TREE_TYPE (captures[1]), prec - 1);
			    tree _r;
			    _r = fold_build2_loc (loc, op, type, res_op0, res_op1);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 591, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail824:;
			}
		    }
		}
	      else
		{
		  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail825;
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail825;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail825;
		  {
		    tree res_op0;
		    res_op0 = captures[0];
		    tree res_op1;
		    res_op1 =  build_int_cst (TREE_TYPE (captures[1]), low);
		    tree _r;
		    _r = fold_build2_loc (loc, op, type, res_op0, res_op1);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 592, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail825:;
		}
	  }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_378 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (copysigns),
 const combined_fn ARG_UNUSED (coss))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail855;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree _r;
    _r = maybe_build_call_expr_loc (loc, coss, type, 1, res_op0);
    if (!_r)
      goto next_after_fail855;
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 609, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail855:;
  return NULL_TREE;
}

tree
generic_simplify_380 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (coshs),
 const combined_fn ARG_UNUSED (atanhs),
 const combined_fn ARG_UNUSED (sqrts))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!flag_errno_math
)
    {
      {
 tree t_one = build_one_cst (type);
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail857;
	  {
	    if (! tree_invariant_p (captures[1])) goto next_after_fail857;
	    tree res_op0;
	    res_op0 =  t_one;
	    tree res_op1;
	    {
	      tree _o1[1], _r1;
	      {
		tree _o2[2], _r2;
		{
		  tree _o3[2], _r3;
		  _o3[0] =  t_one;
		  _o3[1] = unshare_expr (captures[1]);
		  _r3 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
		  _o2[0] = _r3;
		}
		{
		  tree _o3[2], _r3;
		  _o3[0] =  t_one;
		  _o3[1] = captures[1];
		  _r3 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
		  _o2[1] = _r3;
		}
		_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
		_o1[0] = _r2;
	      }
	      _r1 = maybe_build_call_expr_loc (loc, sqrts, TREE_TYPE (_o1[0]), 1, _o1[0]);
	      if (!_r1)
	        goto next_after_fail857;
	      res_op1 = _r1;
	    }
	    tree _r;
	    _r = fold_build2_loc (loc, RDIV_EXPR, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 611, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail857:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_382 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (POW))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail859;
  {
    tree _r;
    _r = captures[0];
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 613, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail859:;
  return NULL_TREE;
}

tree
generic_simplify_386 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (HYPOT))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail863;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree _r;
    _r = fold_build1_loc (loc, ABS_EXPR, type, res_op0);
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 617, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail863:;
  return NULL_TREE;
}

tree
generic_simplify_390 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && TYPE_PRECISION (TREE_TYPE (captures[0])) == 1
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail867;
      {
	tree res_op0;
	res_op0 = captures[0];
	tree res_op1;
	res_op1 = captures[1];
	tree _r;
	_r = fold_build2_loc (loc, BIT_AND_EXPR, type, res_op0, res_op1);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 621, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail867:;
    }
  return NULL_TREE;
}

tree
generic_simplify_396 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (bitop),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree pmop[2];
 tree utype = fold_bit_and_mask (TREE_TYPE (captures[1]), captures[5], op, captures[1], bitop,
 captures[2], captures[3], captures[4], ERROR_MARK, NULL_TREE,
 NULL_TREE, pmop);
      if (utype
)
	{
	  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail873;
	  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail873;
	  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail873;
	  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail873;
	  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail873;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail873;
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
		_o2[0] = captures[5];
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
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 626, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail873:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_410 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail889;
	      if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail889;
	      if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail889;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail889;
	      {
		tree _r;
		_r =  build_int_cst (type, 0);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 638, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail889:;
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
					  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail890;
					  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail890;
					  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail890;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail890;
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
next_after_fail890:;
					}
				      else
					{
					  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail891;
					  if (TREE_SIDE_EFFECTS (captures[5])) goto next_after_fail891;
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail891;
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
next_after_fail891:;
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
generic_simplify_430 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail950;
  {
    tree res_op0;
    res_op0 = captures[0];
    tree res_op1;
    res_op1 = captures[1];
    tree _r;
    _r = fold_build2_loc (loc, LTGT_EXPR, type, res_op0, res_op1);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 665, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail950:;
  return NULL_TREE;
}

tree
generic_simplify_432 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail952;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail952;
  {
    tree _r;
    _r = captures[1];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 666, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail952:;
  return NULL_TREE;
}

tree
generic_simplify_435 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail956;
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
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 670, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail956:;
  return NULL_TREE;
}

tree
generic_simplify_437 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (element_precision (type) <= element_precision (TREE_TYPE (captures[0]))
 || !TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
    {
      if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail958;
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail958;
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
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 672, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail958:;
    }
  return NULL_TREE;
}

tree
generic_simplify_441 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (element_precision (type) <= element_precision (TREE_TYPE (captures[0]))
 || !TYPE_UNSIGNED (TREE_TYPE (captures[0]))
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail962;
      {
	tree res_op0;
	{
	  tree _o1[2], _r1;
	  _o1[0] = captures[0];
	  _o1[1] =  build_each_one_cst (TREE_TYPE (captures[0]));
	  _r1 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
	  res_op0 = _r1;
	}
	tree _r;
	_r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 676, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail962:;
    }
  return NULL_TREE;
}

tree
generic_simplify_444 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail965;
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
next_after_fail965:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_453 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!TYPE_SATURATING (type)
)
    {
      if (!TYPE_OVERFLOW_SANITIZED (type)
 && !TYPE_OVERFLOW_SANITIZED (TREE_TYPE (captures[0]))
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail976;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree _r;
	    _r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 688, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail976:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_461 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
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
		  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail987;
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail987;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail987;
		  {
		    tree _r;
		    _r = captures[0];
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 694, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail987:;
		}
	      else
		{
		  if (known_gt (off0, off1)
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail988;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail988;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail988;
		      {
			tree _r;
			_r = captures[2];
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 695, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail988:;
		    }
		}
	    }
	  else
	    {
	      if (known_ge (off0, off1)
)
		{
		  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail989;
		  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail989;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail989;
		  {
		    tree _r;
		    _r = captures[0];
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 696, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail989:;
		}
	      else
		{
		  if (known_lt (off0, off1)
)
		    {
		      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail990;
		      if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail990;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail990;
		      {
			tree _r;
			_r = captures[2];
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 697, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail990:;
		    }
		}
	    }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_478 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1018;
  if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1018;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1018;
  {
    tree _r;
    _r = captures[2];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 717, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1018:;
  return NULL_TREE;
}

tree
generic_simplify_482 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1022;
  if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1022;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1022;
  {
    tree _r;
    _r = captures[2];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 721, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1022:;
  return NULL_TREE;
}

tree
generic_simplify_487 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1027;
  if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1027;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1027;
  {
    tree _r;
    _r = captures[2];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 726, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1027:;
  return NULL_TREE;
}

tree
generic_simplify_492 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1033;
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1033;
  if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1033;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1033;
  {
    tree _r;
    _r = captures[0];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 732, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1033:;
  return NULL_TREE;
}

tree
generic_simplify_498 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (rotate))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1041;
  {
    tree _r;
    _r = captures[0];
    if (TREE_SIDE_EFFECTS (captures[1]))
      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 739, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1041:;
  return NULL_TREE;
}

tree
generic_simplify_504 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1049;
  if (TREE_SIDE_EFFECTS (_p2)) goto next_after_fail1049;
  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1049;
  {
    tree _r;
    _r = captures[2];
    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 726, __FILE__, __LINE__, true);
    return _r;
  }
next_after_fail1049:;
  return NULL_TREE;
}

tree
generic_simplify_510 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (cond_op))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 tree op_type = TREE_TYPE (captures[5]);
      if (inverse_conditions_p (captures[0], captures[2])
 && element_precision (type) == element_precision (op_type)
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1055;
	  {
	    tree res_op0;
	    {
	      tree _o1[4], _r1;
	      _o1[0] = captures[2];
	      _o1[1] = captures[3];
	      _o1[2] = captures[4];
	      {
		tree _o2[1], _r2;
		_o2[0] = captures[1];
		_r2 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, op_type, _o2[0]);
		_o1[3] = _r2;
	      }
	      _r1 = maybe_build_call_expr_loc (loc, cond_op, TREE_TYPE (_o1[1]), 4, _o1[0], _o1[1], _o1[2], _o1[3]);
	      if (!_r1)
	        goto next_after_fail1055;
	      res_op0 = _r1;
	    }
	    tree _r;
	    _r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
	    if (TREE_SIDE_EFFECTS (captures[0]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	    if (TREE_SIDE_EFFECTS (captures[5]))
	      _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[5]), _r);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 748, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1055:;
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_519 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree *ARG_UNUSED (captures),
 const enum tree_code ARG_UNUSED (cmp))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (REAL_VALUE_ISNAN (TREE_REAL_CST (captures[1]))
 && (cmp != LTGT_EXPR || ! flag_trapping_math)
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1064;
      {
	tree _r;
	_r =  constant_boolean_node (cmp == ORDERED_EXPR || cmp == LTGT_EXPR
 ? false : true, type);
	if (TREE_SIDE_EFFECTS (captures[0]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	if (TREE_SIDE_EFFECTS (captures[1]))
	  _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[1]), _r);
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 757, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1064:;
    }
  return NULL_TREE;
}

tree
generic_simplify_529 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (sinhs),
 const combined_fn ARG_UNUSED (atanhs),
 const combined_fn ARG_UNUSED (sqrts))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (!flag_errno_math
)
    {
      {
 tree t_one = build_one_cst (type);
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1074;
	  {
	    if (! tree_invariant_p (captures[1])) goto next_after_fail1074;
	    tree res_op0;
	    res_op0 = unshare_expr (captures[1]);
	    tree res_op1;
	    {
	      tree _o1[1], _r1;
	      {
		tree _o2[2], _r2;
		{
		  tree _o3[2], _r3;
		  _o3[0] =  t_one;
		  _o3[1] = unshare_expr (captures[1]);
		  _r3 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
		  _o2[0] = _r3;
		}
		{
		  tree _o3[2], _r3;
		  _o3[0] =  t_one;
		  _o3[1] = captures[1];
		  _r3 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
		  _o2[1] = _r3;
		}
		_r2 = fold_build2_loc (loc, MULT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
		_o1[0] = _r2;
	      }
	      _r1 = maybe_build_call_expr_loc (loc, sqrts, TREE_TYPE (_o1[0]), 1, _o1[0]);
	      if (!_r1)
	        goto next_after_fail1074;
	      res_op1 = _r1;
	    }
	    tree _r;
	    _r = fold_build2_loc (loc, RDIV_EXPR, type, res_op0, res_op1);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 767, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1074:;
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_543 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (rints))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (canonicalize_math_p ()
)
    {
      if (!flag_errno_math
)
	{
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1088;
	  {
	    tree res_op0;
	    res_op0 = captures[0];
	    tree _r;
	    _r = fold_build1_loc (loc, FIX_TRUNC_EXPR, type, res_op0);
	    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 781, __FILE__, __LINE__, true);
	    return _r;
	  }
next_after_fail1088:;
	}
    }
  return NULL_TREE;
}

tree
generic_simplify_550 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree ARG_UNUSED (_p1), tree ARG_UNUSED (_p2), tree *ARG_UNUSED (captures))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (canonicalize_math_after_vectorization_p ()
)
    {
      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1095;
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
	  goto next_after_fail1095;
	if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 788, __FILE__, __LINE__, true);
	return _r;
      }
next_after_fail1095:;
    }
  return NULL_TREE;
}

tree
generic_simplify_559 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (bswap),
 const combined_fn ARG_UNUSED (popcount))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (INTEGRAL_TYPE_P (TREE_TYPE (captures[0]))
 && INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
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
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1104;
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
		_r = maybe_build_call_expr_loc (loc, popcount, type, 1, res_op0);
		if (!_r)
		  goto next_after_fail1104;
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 794, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1104:;
	    }
      }
    }
  return NULL_TREE;
}

tree
generic_simplify_569 (location_t ARG_UNUSED (loc), const tree ARG_UNUSED (type),
 tree ARG_UNUSED (_p0), tree *ARG_UNUSED (captures),
 const combined_fn ARG_UNUSED (reduc))
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  {
 int i = single_nonzero_element (captures[1]);
      if (i >= 0
)
	{
	  {
 tree elt = vector_cst_elt (captures[1], i);
 tree elt_type = TREE_TYPE (elt);
 unsigned int elt_bits = tree_to_uhwi (TYPE_SIZE (elt_type));
 tree size = bitsize_int (elt_bits);
 tree pos = bitsize_int (elt_bits * i);
	      if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1115;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1115;
	      {
		tree res_op0;
		{
		  tree _o1[2], _r1;
		  {
		    tree _o2[3], _r2;
		    _o2[0] = captures[0];
		    _o2[1] =  size;
		    _o2[2] =  pos;
		    _r2 = fold_build3_loc (loc, BIT_FIELD_REF, elt_type, _o2[0], _o2[1], _o2[2]);
		    _o1[0] = _r2;
		  }
		  _o1[1] =  elt;
		  _r1 = fold_build2_loc (loc, BIT_AND_EXPR, elt_type, _o1[0], _o1[1]);
		  res_op0 = _r1;
		}
		tree _r;
		_r = fold_build1_loc (loc, VIEW_CONVERT_EXPR, type, res_op0);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 801, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1115:;
	  }
	}
  }
  return NULL_TREE;
}

tree
generic_simplify_FIX_TRUNC_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0)
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
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1193;
	{
	  tree _r;
	  _r = captures[0];
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 815, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1193:;
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
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1194;
		  {
		    tree res_op0;
		    res_op0 = captures[1];
		    tree _r;
		    _r = fold_build1_loc (loc, FIX_TRUNC_EXPR, type, res_op0);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 816, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1194:;
		}
	      else
		{
		  if (((inter_int && inside_int) || (inter_float && inside_float))
 && (final_int || final_float)
 && inter_prec >= inside_prec
 && (inter_float || inter_unsignedp == inside_unsignedp)
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1195;
		      {
			tree res_op0;
			res_op0 = captures[1];
			tree _r;
			_r = fold_build1_loc (loc, FIX_TRUNC_EXPR, type, res_op0);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 817, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1195:;
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1196;
			  {
			    tree res_op0;
			    res_op0 = captures[1];
			    tree _r;
			    _r = fold_build1_loc (loc, FIX_TRUNC_EXPR, type, res_op0);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 818, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1196:;
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
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1197;
			      {
				tree res_op0;
				res_op0 = captures[1];
				tree _r;
				_r = fold_build1_loc (loc, FIX_TRUNC_EXPR, type, res_op0);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 819, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail1197:;
			    }
			  else
			    {
			      if (inside_int && inter_int && final_int
 && final_prec <= inside_prec
 && inter_prec >= inside_prec
)
				{
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1198;
				  {
				    tree res_op0;
				    res_op0 = captures[1];
				    tree _r;
				    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 820, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail1198:;
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
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1199;
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
next_after_fail1199:;
				    }
				  else
				    {
				      if (0
 && inside_int && inter_float && final_int &&
 (unsigned) significand_size (TYPE_MODE (inter_type))
 >= inside_prec - !inside_unsignedp
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1200;
					  {
					    tree res_op0;
					    res_op0 = captures[1];
					    tree _r;
					    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 822, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1200:;
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
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1201;
		  {
		    tree res_op0;
		    res_op0 = captures[1];
		    tree _r;
		    _r = fold_build1_loc (loc, FIX_TRUNC_EXPR, type, res_op0);
		    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 816, __FILE__, __LINE__, true);
		    return _r;
		  }
next_after_fail1201:;
		}
	      else
		{
		  if (((inter_int && inside_int) || (inter_float && inside_float))
 && (final_int || final_float)
 && inter_prec >= inside_prec
 && (inter_float || inter_unsignedp == inside_unsignedp)
)
		    {
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1202;
		      {
			tree res_op0;
			res_op0 = captures[1];
			tree _r;
			_r = fold_build1_loc (loc, FIX_TRUNC_EXPR, type, res_op0);
			if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 817, __FILE__, __LINE__, true);
			return _r;
		      }
next_after_fail1202:;
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
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1203;
			  {
			    tree res_op0;
			    res_op0 = captures[1];
			    tree _r;
			    _r = fold_build1_loc (loc, FIX_TRUNC_EXPR, type, res_op0);
			    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 818, __FILE__, __LINE__, true);
			    return _r;
			  }
next_after_fail1203:;
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
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1204;
			      {
				tree res_op0;
				res_op0 = captures[1];
				tree _r;
				_r = fold_build1_loc (loc, FIX_TRUNC_EXPR, type, res_op0);
				if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 819, __FILE__, __LINE__, true);
				return _r;
			      }
next_after_fail1204:;
			    }
			  else
			    {
			      if (inside_int && inter_int && final_int
 && final_prec <= inside_prec
 && inter_prec >= inside_prec
)
				{
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1205;
				  {
				    tree res_op0;
				    res_op0 = captures[1];
				    tree _r;
				    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
				    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 820, __FILE__, __LINE__, true);
				    return _r;
				  }
next_after_fail1205:;
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
				      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1206;
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
next_after_fail1206:;
				    }
				  else
				    {
				      if (0
 && inside_int && inter_float && final_int &&
 (unsigned) significand_size (TYPE_MODE (inter_type))
 >= inside_prec - !inside_unsignedp
)
					{
					  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1207;
					  {
					    tree res_op0;
					    res_op0 = captures[1];
					    tree _r;
					    _r = fold_build1_loc (loc, NOP_EXPR, type, res_op0);
					    if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 822, __FILE__, __LINE__, true);
					    return _r;
					  }
next_after_fail1207:;
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
generic_simplify_BIT_IOR_EXPR (location_t ARG_UNUSED (loc), enum tree_code ARG_UNUSED (code), const tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1297;
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1297;
	{
	  tree res_op0;
	  res_op0 = captures[0];
	  tree _r;
	  _r = non_lvalue_loc (loc, res_op0);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 862, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1297:;
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
		    tree res = generic_simplify_63 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
		    tree res = generic_simplify_64 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
	if (integer_zerop (_q21))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case NE_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if (integer_zerop (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
			tree res = generic_simplify_65 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, NE_EXPR);
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
	      case NE_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if (integer_all_onesp (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _q50 };
			tree res = generic_simplify_66 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, NE_EXPR);
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
		tree res = generic_simplify_6 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		if (res) return res;
	      }
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q50 };
		tree res = generic_simplify_6 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		if (res) return res;
	      }
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q51 };
		tree res = generic_simplify_6 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		if (res) return res;
	      }
	      {
		tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q50 };
		tree res = generic_simplify_6 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		if (res) return res;
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
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_67 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_67 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
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
			      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p0 };
			      tree res = generic_simplify_68 (loc, type, _p0, _p1, captures);
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
		    case BIT_NOT_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_q51, 0);
			if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p0 };
			      tree res = generic_simplify_68 (loc, type, _p0, _p1, captures);
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
			      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p0 };
			      tree res = generic_simplify_68 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p0 };
			      tree res = generic_simplify_68 (loc, type, _p0, _p1, captures);
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
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p0, _q20, _q21 };
	  tree res = generic_simplify_69 (loc, type, _p0, _p1, captures);
	  if (res) return res;
	}
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p1, _p0, _q21, _q20 };
	  tree res = generic_simplify_69 (loc, type, _p0, _p1, captures);
	  if (res) return res;
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
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
	      tree res = generic_simplify_67 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	      tree res = generic_simplify_67 (loc, type, _p0, _p1, captures);
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
	switch (TREE_CODE (_q21))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_XOR_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q40, _p1 };
			      tree res = generic_simplify_70 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
		      {
			if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q40, _p1 };
			      tree res = generic_simplify_70 (loc, type, _p0, _p1, captures);
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
		case BIT_XOR_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q30, _p1 };
			      tree res = generic_simplify_70 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q30, _p1 };
			      tree res = generic_simplify_70 (loc, type, _p0, _p1, captures);
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
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q41 };
		    tree res = generic_simplify_71 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q40 };
		    tree res = generic_simplify_71 (loc, type, _p0, _p1, captures);
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
	  case BIT_XOR_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _p0, _q31 };
		    tree res = generic_simplify_71 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q31, _p0, _q30 };
		    tree res = generic_simplify_71 (loc, type, _p0, _p1, captures);
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
		    tree res = generic_simplify_72 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q40 };
		    tree res = generic_simplify_72 (loc, type, _p0, _p1, captures);
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
		    tree res = generic_simplify_72 (loc, type, _p0, _p1, captures);
		    if (res) return res;
		  }
	        }
	      if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q31, _q30 };
		    tree res = generic_simplify_72 (loc, type, _p0, _p1, captures);
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
	      tree res = generic_simplify_72 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	      tree res = generic_simplify_72 (loc, type, _p0, _p1, captures);
	      if (res) return res;
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
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_72 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_72 (loc, type, _p0, _p1, captures);
	      if (res) return res;
	    }
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
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _q30, _q31 };
	  tree res = generic_simplify_69 (loc, type, _p0, _p1, captures);
	  if (res) return res;
	}
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _p1, _q31, _q30 };
	  tree res = generic_simplify_69 (loc, type, _p0, _p1, captures);
	  if (res) return res;
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
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			tree res = generic_simplify_73 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
			if (res) return res;
		      }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			tree res = generic_simplify_73 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
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
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			tree res = generic_simplify_73 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
			if (res) return res;
		      }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			tree res = generic_simplify_73 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q21 };
		    tree res = generic_simplify_74 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q20, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _q20 };
		    tree res = generic_simplify_74 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
	      tree res = generic_simplify_74 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q20, 0))
	  {
	    {
	      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _q20 };
	      tree res = generic_simplify_74 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q21 };
			tree res = generic_simplify_75 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
			if (res) return res;
		      }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q21, _q20 };
			tree res = generic_simplify_75 (loc, type, _p0, _p1, captures, BIT_AND_EXPR);
			if (res) return res;
		      }
		    }
	        }
	      break;
	    }
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
			      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			      tree res = generic_simplify_76 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			      tree res = generic_simplify_76 (loc, type, _p0, _p1, captures);
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
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			tree res = generic_simplify_77 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			tree res = generic_simplify_77 (loc, type, _p0, _p1, captures);
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
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    tree res = generic_simplify_77 (loc, type, _p0, _p1, captures);
			    if (res) return res;
			  }
		        }
		    }
		  if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		    {
		      if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			    tree res = generic_simplify_77 (loc, type, _p0, _p1, captures);
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
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q21 };
			tree res = generic_simplify_75 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
			if (res) return res;
		      }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q21, _q20 };
			tree res = generic_simplify_75 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q61 };
			      tree res = generic_simplify_78 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
		      {
			if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q60 };
			      tree res = generic_simplify_78 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _q61 };
			      tree res = generic_simplify_78 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
		      {
			if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _q60 };
			      tree res = generic_simplify_78 (loc, type, _p0, _p1, captures);
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
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q71 };
			      tree res = generic_simplify_78 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
			if ((_q71 == _q21 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q21, 0) && types_match (_q71, _q21)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _q70 };
			      tree res = generic_simplify_78 (loc, type, _p0, _p1, captures);
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
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _q71 };
			      tree res = generic_simplify_78 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
			if ((_q71 == _q20 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q20, 0) && types_match (_q71, _q20)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _q70 };
			      tree res = generic_simplify_78 (loc, type, _p0, _p1, captures);
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
			      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q21, _q30, _q31 };
			      tree res = generic_simplify_79 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
			if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q21, _q31, _q30 };
			      tree res = generic_simplify_79 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
		      {
			if ((_q71 == _q21 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q21, 0) && types_match (_q71, _q21)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q21, _q30, _q31 };
			      tree res = generic_simplify_79 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
		      {
			if ((_q71 == _q21 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q21, 0) && types_match (_q71, _q21)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q21, _q31, _q30 };
			      tree res = generic_simplify_79 (loc, type, _p0, _p1, captures);
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
			      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q20, _q40, _q41 };
			      tree res = generic_simplify_79 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
			if ((_q71 == _q41 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q41, 0) && types_match (_q71, _q41)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q20, _q41, _q40 };
			      tree res = generic_simplify_79 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
		      {
			if ((_q71 == _q20 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q20, 0) && types_match (_q71, _q20)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q20, _q40, _q41 };
			      tree res = generic_simplify_79 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q70 == _q41 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q41, 0) && types_match (_q70, _q41)))
		      {
			if ((_q71 == _q20 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q20, 0) && types_match (_q71, _q20)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q20, _q41, _q40 };
			      tree res = generic_simplify_79 (loc, type, _p0, _p1, captures);
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
	  case BIT_XOR_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			if ((_q61 == _q31 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q31, 0) && types_match (_q61, _q31)))
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p0 };
			      tree res = generic_simplify_80 (loc, type, _p0, _p1, captures);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q60 == _q31 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q31, 0) && types_match (_q60, _q31)))
		      {
			if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
			  {
			    {
			      tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p0 };
			      tree res = generic_simplify_80 (loc, type, _p0, _p1, captures);
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
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p0 };
			tree res = generic_simplify_81 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p0 };
			tree res = generic_simplify_81 (loc, type, _p0, _p1, captures);
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
	      case BIT_AND_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_p1, 0);
		  tree _q61 = TREE_OPERAND (_p1, 1);
		  if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		    {
		      if ((_q61 == _q31 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q31, 0) && types_match (_q61, _q31)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p0 };
			    tree res = generic_simplify_81 (loc, type, _p0, _p1, captures);
			    if (res) return res;
			  }
		        }
		    }
		  if ((_q60 == _q31 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q31, 0) && types_match (_q60, _q31)))
		    {
		      if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
			{
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p0 };
			    tree res = generic_simplify_81 (loc, type, _p0, _p1, captures);
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
  if (integer_all_onesp (_p1))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1298;
	{
	  tree _r;
	  _r = captures[1];
	  if (TREE_SIDE_EFFECTS (captures[0]))
	    _r = build2_loc (loc, COMPOUND_EXPR, type, fold_ignored_result (captures[0]), _r);
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 907, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1298:;
      }
    }
  if (integer_zerop (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1299;
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1299;
	{
	  tree _r;
	  _r = captures[0];
	  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 908, __FILE__, __LINE__, true);
	  return _r;
	}
next_after_fail1299:;
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
	      {
		tree _q40_pops[1];
		if (tree_maybe_bit_not (_q40, _q40_pops))
		  {
		    tree _q50 = _q40_pops[0];
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q50 };
		      tree res = generic_simplify_82 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
		      tree res = generic_simplify_82 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
	  tree res = generic_simplify_82 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
	  tree res = generic_simplify_82 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
	  if (res) return res;
	}
      }
  }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	tree res = generic_simplify_83 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
	  case BIT_IOR_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q41 };
		    tree res = generic_simplify_84 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q40 };
		    tree res = generic_simplify_84 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
	  case BIT_IOR_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q30, 0) && types_match (_p1, _q30)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q20, _q31 };
		    tree res = generic_simplify_84 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
		{
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _q31, _q20, _q30 };
		    tree res = generic_simplify_84 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q60))
		      {
		      case BIT_NOT_EXPR:
		        {
			  tree _q70 = TREE_OPERAND (_q60, 0);
			  if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
			    {
			      if ((_q61 == _q31 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q31, 0) && types_match (_q61, _q31)))
				{
				  {
				    tree captures[3] ATTRIBUTE_UNUSED = { _q60, _q30, _q31 };
				    tree res = generic_simplify_85 (loc, type, _p0, _p1, captures);
				    if (res) return res;
				  }
			        }
			    }
			  if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
			    {
			      if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
				{
				  {
				    tree captures[3] ATTRIBUTE_UNUSED = { _q60, _q31, _q30 };
				    tree res = generic_simplify_85 (loc, type, _p0, _p1, captures);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    if ((_q60 == _q31 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q31, 0) && types_match (_q60, _q31)))
		      {
			switch (TREE_CODE (_q61))
			  {
			  case BIT_NOT_EXPR:
			    {
			      tree _q80 = TREE_OPERAND (_q61, 0);
			      if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
				{
				  {
				    tree captures[3] ATTRIBUTE_UNUSED = { _q61, _q30, _q31 };
				    tree res = generic_simplify_85 (loc, type, _p0, _p1, captures);
				    if (res) return res;
				  }
			        }
			      break;
			    }
		          default:;
		          }
		      }
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			switch (TREE_CODE (_q61))
			  {
			  case BIT_NOT_EXPR:
			    {
			      tree _q80 = TREE_OPERAND (_q61, 0);
			      if ((_q80 == _q31 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q31, 0) && types_match (_q80, _q31)))
				{
				  {
				    tree captures[3] ATTRIBUTE_UNUSED = { _q61, _q31, _q30 };
				    tree res = generic_simplify_85 (loc, type, _p0, _p1, captures);
				    if (res) return res;
				  }
			        }
			      break;
			    }
		          default:;
		          }
			if ((_q61 == _q31 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q31, 0) && types_match (_q61, _q31)))
			  {
			    {
			      tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q30, _q31, _p0, _q20 };
			      tree res = generic_simplify_86 (loc, type, _p0, _p1, captures);
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
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			if ((_q61 == _q31 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q31, 0) && types_match (_q61, _q31)))
			  {
			    {
			      tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q30, _q31, _p0, _q20 };
			      tree res = generic_simplify_87 (loc, type, _p0, _p1, captures);
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
		      tree res = generic_simplify_88 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
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
		tree res = generic_simplify_88 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
		if (res) return res;
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
	  case BIT_NOT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_NOT_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    switch (TREE_CODE (_q60))
		      {
		      case BIT_IOR_EXPR:
		        {
			  tree _q70 = TREE_OPERAND (_q60, 0);
			  tree _q71 = TREE_OPERAND (_q60, 1);
			  if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
			    {
			      if ((_q71 == _q21 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q21, 0) && types_match (_q71, _q21)))
				{
				  {
				    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q21 };
				    tree res = generic_simplify_89 (loc, type, _p0, _p1, captures);
				    if (res) return res;
				  }
			        }
			    }
			  if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
			    {
			      if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
				{
				  {
				    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q30, _q21 };
				    tree res = generic_simplify_89 (loc, type, _p0, _p1, captures);
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
		case BIT_NOT_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    switch (TREE_CODE (_q60))
		      {
		      case BIT_IOR_EXPR:
		        {
			  tree _q70 = TREE_OPERAND (_q60, 0);
			  tree _q71 = TREE_OPERAND (_q60, 1);
			  if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
			    {
			      if ((_q71 == _q20 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q20, 0) && types_match (_q71, _q20)))
				{
				  {
				    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q40, _q20 };
				    tree res = generic_simplify_89 (loc, type, _p0, _p1, captures);
				    if (res) return res;
				  }
			        }
			    }
			  if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
			    {
			      if ((_q71 == _q40 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q40, 0) && types_match (_q71, _q40)))
				{
				  {
				    tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q40, _q20 };
				    tree res = generic_simplify_89 (loc, type, _p0, _p1, captures);
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
	  case BIT_NOT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
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
			    {
			      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
			      tree res = generic_simplify_86 (loc, type, _p0, _p1, captures);
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
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			tree res = generic_simplify_8 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q20, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q51, _q20 };
		    tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || operand_equal_p (_q51, _q20, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _q50, _q20 };
		    tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q21, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q51, _q21 };
		    tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || operand_equal_p (_q51, _q21, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _q50, _q21 };
		    tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
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
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case BIT_NOT_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
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
			    {
			      tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q50 };
			      tree res = generic_simplify_87 (loc, type, _p0, _p1, captures);
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
			tree res = generic_simplify_8 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
			    tree res = generic_simplify_88 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
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
		      tree res = generic_simplify_88 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
		      if (res) return res;
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
				tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
				if (res) return res;
			      }
			    }
			  if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || operand_equal_p (_q71, _q30, 0))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q70, _q30 };
				tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
				if (res) return res;
			      }
			    }
			  if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || operand_equal_p (_q70, _q31, 0))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q71, _q31 };
				tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
				if (res) return res;
			      }
			    }
			  if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || operand_equal_p (_q71, _q31, 0))
			    {
			      {
				tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q70, _q31 };
				tree res = generic_simplify_90 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
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
	  case BIT_IOR_EXPR:
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
			  tree res = generic_simplify_74 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || operand_equal_p (_q60, _q30, 0))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q31, _q30, _q30 };
			  tree res = generic_simplify_74 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
		    tree res = generic_simplify_74 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_p1 == _q30 && ! TREE_SIDE_EFFECTS (_p1)) || operand_equal_p (_p1, _q30, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q31, _q30, _q30 };
		    tree res = generic_simplify_74 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
		    if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || operand_equal_p (_q51, _q20, 0))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q50, _q51, _q20 };
			  tree res = generic_simplify_91 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || operand_equal_p (_q50, _q20, 0))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q51, _q50, _q20 };
			  tree res = generic_simplify_91 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
	      if ((_q41 == _q20 && ! TREE_SIDE_EFFECTS (_q41)) || operand_equal_p (_q41, _q20, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q40, _q41, _q20 };
		    tree res = generic_simplify_91 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q40 == _q20 && ! TREE_SIDE_EFFECTS (_q40)) || operand_equal_p (_q40, _q20, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q41, _q40, _q20 };
		    tree res = generic_simplify_91 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
	      if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || operand_equal_p (_q41, _p0, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q40, _q41, _p0 };
		    tree res = generic_simplify_91 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || operand_equal_p (_q40, _p0, 0))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q41, _q40, _p0 };
		    tree res = generic_simplify_91 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || operand_equal_p (_q31, _p0, 0))
	  {
	    {
	      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q30, _q31, _p0 };
	      tree res = generic_simplify_91 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || operand_equal_p (_q30, _p0, 0))
	  {
	    {
	      tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q31, _q30, _p0 };
	      tree res = generic_simplify_91 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
	      if (res) return res;
	    }
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
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
	  tree res = generic_simplify_92 (loc, type, _p0, _p1, captures);
	  if (res) return res;
	}
	switch (TREE_CODE (_p1))
	  {
	  CASE_CONVERT:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      switch (TREE_CODE (_q50))
	        {
		case BIT_IOR_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    tree _q61 = TREE_OPERAND (_q50, 1);
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21, _p1, _q50, _q61 };
			  tree res = generic_simplify_93 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21, _p1, _q50, _q60 };
			  tree res = generic_simplify_93 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q20, _p1, _q50, _q61 };
			  tree res = generic_simplify_93 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q20, _p1, _q50, _q60 };
			  tree res = generic_simplify_93 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21, _p1, _p1, _q51 };
		    tree res = generic_simplify_94 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q21, _p1, _p1, _q50 };
		    tree res = generic_simplify_94 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q20, _p1, _p1, _q51 };
		    tree res = generic_simplify_94 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[7] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q20, _p1, _p1, _q50 };
		    tree res = generic_simplify_94 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
    case BIT_IOR_EXPR:
      {
	tree _q30 = TREE_OPERAND (_p1, 0);
	tree _q31 = TREE_OPERAND (_p1, 1);
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p0 };
	  tree res = generic_simplify_92 (loc, type, _p0, _p1, captures);
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
			  if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
			    {
			      {
				tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1, _q60, _q71 };
				tree res = generic_simplify_95 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
				if (res) return res;
			      }
			    }
			  if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
			    {
			      {
				tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1, _q60, _q70 };
				tree res = generic_simplify_95 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
				if (res) return res;
			      }
			    }
			  if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
			    {
			      {
				tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q30, _p1, _q60, _q71 };
				tree res = generic_simplify_95 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
				if (res) return res;
			      }
			    }
			  if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
			    {
			      {
				tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q30, _p1, _q60, _q70 };
				tree res = generic_simplify_95 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1, _p1, _q61 };
			  tree res = generic_simplify_96 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q31, _p1, _p1, _q60 };
			  tree res = generic_simplify_96 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q31 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q31, 0) && types_match (_q60, _q31)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q30, _p1, _p1, _q61 };
			  tree res = generic_simplify_96 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q31 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q31, 0) && types_match (_q61, _q31)))
		      {
			{
			  tree captures[7] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q30, _p1, _p1, _q60 };
			  tree res = generic_simplify_96 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
		tree res = generic_simplify_97 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	{
	  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _p1 };
	  tree res = generic_simplify_98 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
	  if (res) return res;
	}
        break;
      }
    case BIT_AND_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_p1 == _q20 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q20, 0) && types_match (_p1, _q20)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
	      tree res = generic_simplify_99 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
	      tree res = generic_simplify_99 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
	      if (res) return res;
	    }
	  }
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
	  tree res = generic_simplify_100 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
	  if (res) return res;
	}
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
	  tree res = generic_simplify_100 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
	  if (res) return res;
	}
	switch (TREE_CODE (_p1))
	  {
	  case BIT_IOR_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q51 };
		    tree res = generic_simplify_101 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q50 };
		    tree res = generic_simplify_101 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_101 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_101 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
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
		case BIT_AND_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    tree _q61 = TREE_OPERAND (_q50, 1);
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q61 };
			  tree res = generic_simplify_102 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q21 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q21, 0) && types_match (_q61, _q21)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q21, _p1, _q60 };
			  tree res = generic_simplify_102 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _p1, _q61 };
			  tree res = generic_simplify_102 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q20 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q20, 0) && types_match (_q61, _q20)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q20, _p1, _q60 };
			  tree res = generic_simplify_102 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
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
	  tree _q20_pops[1];
	  if (tree_nop_convert (_q20, _q20_pops))
	    {
	      tree _q30 = _q20_pops[0];
	      switch (TREE_CODE (_q30))
	        {
		case BIT_IOR_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q30, _q40, _q41, _q21, _p1 };
		      tree res = generic_simplify_103 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		      if (res) return res;
		    }
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q30, _q41, _q40, _q21, _p1 };
		      tree res = generic_simplify_103 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case BIT_XOR_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q30, _q40, _q41, _q21, _p1 };
		      tree res = generic_simplify_103 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		      if (res) return res;
		    }
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q30, _q41, _q40, _q21, _p1 };
		      tree res = generic_simplify_103 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		      if (res) return res;
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
	      switch (TREE_CODE (_q40))
	        {
		case BIT_IOR_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q40, _q50, _q51, _q20, _p1 };
		      tree res = generic_simplify_103 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		      if (res) return res;
		    }
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q40, _q51, _q50, _q20, _p1 };
		      tree res = generic_simplify_103 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case BIT_XOR_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q40, _q50, _q51, _q20, _p1 };
		      tree res = generic_simplify_103 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		      if (res) return res;
		    }
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q40, _q51, _q50, _q20, _p1 };
		      tree res = generic_simplify_103 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	    }
	}
	switch (TREE_CODE (_q20))
	  {
	  case BIT_IOR_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q20, _q30, _q31, _q21, _p1 };
		tree res = generic_simplify_104 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		if (res) return res;
	      }
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q20, _q31, _q30, _q21, _p1 };
		tree res = generic_simplify_104 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case BIT_XOR_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q20, _q30, _q31, _q21, _p1 };
		tree res = generic_simplify_104 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		if (res) return res;
	      }
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p0, _q20, _q20, _q31, _q30, _q21, _p1 };
		tree res = generic_simplify_104 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q21))
	  {
	  case BIT_IOR_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q21, _q40, _q41, _q20, _p1 };
		tree res = generic_simplify_104 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		if (res) return res;
	      }
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q21, _q41, _q40, _q20, _p1 };
		tree res = generic_simplify_104 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case BIT_XOR_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q21, _q40, _q41, _q20, _p1 };
		tree res = generic_simplify_104 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		if (res) return res;
	      }
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p0, _p0, _q21, _q21, _q41, _q40, _q20, _p1 };
		tree res = generic_simplify_104 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
	if ((_q30 == _p0 && ! TREE_SIDE_EFFECTS (_q30)) || (operand_equal_p (_q30, _p0, 0) && types_match (_q30, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q31 };
	      tree res = generic_simplify_99 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
	      if (res) return res;
	    }
	  }
	if ((_q31 == _p0 && ! TREE_SIDE_EFFECTS (_q31)) || (operand_equal_p (_q31, _p0, 0) && types_match (_q31, _p0)))
	  {
	    {
	      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
	      tree res = generic_simplify_99 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
	      if (res) return res;
	    }
	  }
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _q30, _q31, _p0 };
	  tree res = generic_simplify_100 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
	  if (res) return res;
	}
	{
	  tree captures[3] ATTRIBUTE_UNUSED = { _q31, _q30, _p0 };
	  tree res = generic_simplify_100 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
	  if (res) return res;
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
	switch (TREE_CODE (_p1))
	  {
	  case BIT_AND_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q51, _p0, _q21 };
		    tree res = generic_simplify_105 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q51, _p0, _q20 };
		    tree res = generic_simplify_105 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q50, _p0, _q21 };
		    tree res = generic_simplify_105 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[4] ATTRIBUTE_UNUSED = { _q21, _q50, _p0, _q20 };
		    tree res = generic_simplify_105 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
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
	  case BIT_AND_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q30, _p0, _q31 };
			  tree res = generic_simplify_106 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q31 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q31, 0) && types_match (_q61, _q31)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _q60, _q31, _p0, _q30 };
			  tree res = generic_simplify_106 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _q61, _q30, _p0, _q31 };
			  tree res = generic_simplify_106 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q31 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q31, 0) && types_match (_q60, _q31)))
		      {
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _q61, _q31, _p0, _q30 };
			  tree res = generic_simplify_106 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
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
		    tree res = generic_simplify_107 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q31, _q30 };
		    tree res = generic_simplify_107 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
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
	  case BIT_AND_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      if ((_q40 == _p0 && ! TREE_SIDE_EFFECTS (_q40)) || (operand_equal_p (_q40, _p0, 0) && types_match (_q40, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q41 };
		    tree res = generic_simplify_107 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q41 == _p0 && ! TREE_SIDE_EFFECTS (_q41)) || (operand_equal_p (_q41, _p0, 0) && types_match (_q41, _p0)))
		{
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q40 };
		    tree res = generic_simplify_107 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, BIT_AND_EXPR);
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
	  case BIT_AND_EXPR:
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
		      case BIT_IOR_EXPR:
		        {
			  tree _q50 = TREE_OPERAND (_q40, 0);
			  tree _q51 = TREE_OPERAND (_q40, 1);
			  {
			    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q40, _q50, _q51, _q31, _p1 };
			    tree res = generic_simplify_108 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
			    if (res) return res;
			  }
			  {
			    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q40, _q51, _q50, _q31, _p1 };
			    tree res = generic_simplify_108 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
			    if (res) return res;
			  }
		          break;
		        }
		      case BIT_XOR_EXPR:
		        {
			  tree _q50 = TREE_OPERAND (_q40, 0);
			  tree _q51 = TREE_OPERAND (_q40, 1);
			  {
			    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q40, _q50, _q51, _q31, _p1 };
			    tree res = generic_simplify_108 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
			    if (res) return res;
			  }
			  {
			    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q40, _q51, _q50, _q31, _p1 };
			    tree res = generic_simplify_108 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
		      case BIT_IOR_EXPR:
		        {
			  tree _q60 = TREE_OPERAND (_q50, 0);
			  tree _q61 = TREE_OPERAND (_q50, 1);
			  {
			    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q50, _q60, _q61, _q30, _p1 };
			    tree res = generic_simplify_108 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
			    if (res) return res;
			  }
			  {
			    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q50, _q61, _q60, _q30, _p1 };
			    tree res = generic_simplify_108 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
			    if (res) return res;
			  }
		          break;
		        }
		      case BIT_XOR_EXPR:
		        {
			  tree _q60 = TREE_OPERAND (_q50, 0);
			  tree _q61 = TREE_OPERAND (_q50, 1);
			  {
			    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q50, _q60, _q61, _q30, _p1 };
			    tree res = generic_simplify_108 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
			    if (res) return res;
			  }
			  {
			    tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q50, _q61, _q60, _q30, _p1 };
			    tree res = generic_simplify_108 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
		case BIT_IOR_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q30, _q40, _q41, _q31, _p1 };
		      tree res = generic_simplify_109 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		      if (res) return res;
		    }
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q30, _q41, _q40, _q31, _p1 };
		      tree res = generic_simplify_109 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case BIT_XOR_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q30, _q40, _q41, _q31, _p1 };
		      tree res = generic_simplify_109 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		      if (res) return res;
		    }
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q30, _q30, _q41, _q40, _q31, _p1 };
		      tree res = generic_simplify_109 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q31))
	        {
		case BIT_IOR_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q31, 0);
		    tree _q51 = TREE_OPERAND (_q31, 1);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q31, _q50, _q51, _q30, _p1 };
		      tree res = generic_simplify_109 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		      if (res) return res;
		    }
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q31, _q51, _q50, _q30, _p1 };
		      tree res = generic_simplify_109 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case BIT_XOR_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q31, 0);
		    tree _q51 = TREE_OPERAND (_q31, 1);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q31, _q50, _q51, _q30, _p1 };
		      tree res = generic_simplify_109 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		      if (res) return res;
		    }
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p0, _q20, _q31, _q31, _q51, _q50, _q30, _p1 };
		      tree res = generic_simplify_109 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
	        default:;
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
					    tree res = generic_simplify_111 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
						  tree res = generic_simplify_110 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
				tree res = generic_simplify_112 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
  switch (TREE_CODE (_p1))
    {
    case BIT_AND_EXPR:
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
		case BIT_IOR_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p1, _p1, _q30, _q40, _q50, _q51, _q31, _p0 };
		      tree res = generic_simplify_103 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		      if (res) return res;
		    }
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p1, _p1, _q30, _q40, _q51, _q50, _q31, _p0 };
		      tree res = generic_simplify_103 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case BIT_XOR_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p1, _p1, _q30, _q40, _q50, _q51, _q31, _p0 };
		      tree res = generic_simplify_103 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		      if (res) return res;
		    }
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p1, _p1, _q30, _q40, _q51, _q50, _q31, _p0 };
		      tree res = generic_simplify_103 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
		case BIT_IOR_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    tree _q61 = TREE_OPERAND (_q50, 1);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p1, _p1, _q31, _q50, _q60, _q61, _q30, _p0 };
		      tree res = generic_simplify_103 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		      if (res) return res;
		    }
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p1, _p1, _q31, _q50, _q61, _q60, _q30, _p0 };
		      tree res = generic_simplify_103 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		      if (res) return res;
		    }
		    break;
		  }
		case BIT_XOR_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    tree _q61 = TREE_OPERAND (_q50, 1);
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p1, _p1, _q31, _q50, _q60, _q61, _q30, _p0 };
		      tree res = generic_simplify_103 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		      if (res) return res;
		    }
		    {
		      tree captures[8] ATTRIBUTE_UNUSED = { _p1, _p1, _q31, _q50, _q61, _q60, _q30, _p0 };
		      tree res = generic_simplify_103 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
	  case BIT_IOR_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p1, _p1, _q30, _q30, _q40, _q41, _q31, _p0 };
		tree res = generic_simplify_104 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		if (res) return res;
	      }
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p1, _p1, _q30, _q30, _q41, _q40, _q31, _p0 };
		tree res = generic_simplify_104 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case BIT_XOR_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q30, 0);
	      tree _q41 = TREE_OPERAND (_q30, 1);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p1, _p1, _q30, _q30, _q40, _q41, _q31, _p0 };
		tree res = generic_simplify_104 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		if (res) return res;
	      }
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p1, _p1, _q30, _q30, _q41, _q40, _q31, _p0 };
		tree res = generic_simplify_104 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		if (res) return res;
	      }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q31))
	  {
	  case BIT_IOR_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_q31, 0);
	      tree _q51 = TREE_OPERAND (_q31, 1);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p1, _p1, _q31, _q31, _q50, _q51, _q30, _p0 };
		tree res = generic_simplify_104 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		if (res) return res;
	      }
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p1, _p1, _q31, _q31, _q51, _q50, _q30, _p0 };
		tree res = generic_simplify_104 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		if (res) return res;
	      }
	      break;
	    }
	  case BIT_XOR_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_q31, 0);
	      tree _q51 = TREE_OPERAND (_q31, 1);
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p1, _p1, _q31, _q31, _q50, _q51, _q30, _p0 };
		tree res = generic_simplify_104 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		if (res) return res;
	      }
	      {
		tree captures[8] ATTRIBUTE_UNUSED = { _p1, _p1, _q31, _q31, _q51, _q50, _q30, _p0 };
		tree res = generic_simplify_104 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
	case BIT_AND_EXPR:
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
		    case BIT_IOR_EXPR:
		      {
			tree _q60 = TREE_OPERAND (_q50, 0);
			tree _q61 = TREE_OPERAND (_q50, 1);
			{
			  tree captures[8] ATTRIBUTE_UNUSED = { _p1, _q30, _q40, _q50, _q60, _q61, _q41, _p0 };
			  tree res = generic_simplify_108 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
			  if (res) return res;
			}
			{
			  tree captures[8] ATTRIBUTE_UNUSED = { _p1, _q30, _q40, _q50, _q61, _q60, _q41, _p0 };
			  tree res = generic_simplify_108 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
			  if (res) return res;
			}
		        break;
		      }
		    case BIT_XOR_EXPR:
		      {
			tree _q60 = TREE_OPERAND (_q50, 0);
			tree _q61 = TREE_OPERAND (_q50, 1);
			{
			  tree captures[8] ATTRIBUTE_UNUSED = { _p1, _q30, _q40, _q50, _q60, _q61, _q41, _p0 };
			  tree res = generic_simplify_108 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
			  if (res) return res;
			}
			{
			  tree captures[8] ATTRIBUTE_UNUSED = { _p1, _q30, _q40, _q50, _q61, _q60, _q41, _p0 };
			  tree res = generic_simplify_108 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
		    case BIT_IOR_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_q60, 0);
			tree _q71 = TREE_OPERAND (_q60, 1);
			{
			  tree captures[8] ATTRIBUTE_UNUSED = { _p1, _q30, _q41, _q60, _q70, _q71, _q40, _p0 };
			  tree res = generic_simplify_108 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
			  if (res) return res;
			}
			{
			  tree captures[8] ATTRIBUTE_UNUSED = { _p1, _q30, _q41, _q60, _q71, _q70, _q40, _p0 };
			  tree res = generic_simplify_108 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
			  if (res) return res;
			}
		        break;
		      }
		    case BIT_XOR_EXPR:
		      {
			tree _q70 = TREE_OPERAND (_q60, 0);
			tree _q71 = TREE_OPERAND (_q60, 1);
			{
			  tree captures[8] ATTRIBUTE_UNUSED = { _p1, _q30, _q41, _q60, _q70, _q71, _q40, _p0 };
			  tree res = generic_simplify_108 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
			  if (res) return res;
			}
			{
			  tree captures[8] ATTRIBUTE_UNUSED = { _p1, _q30, _q41, _q60, _q71, _q70, _q40, _p0 };
			  tree res = generic_simplify_108 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
	      case BIT_IOR_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_q40, 0);
		  tree _q51 = TREE_OPERAND (_q40, 1);
		  {
		    tree captures[8] ATTRIBUTE_UNUSED = { _p1, _q30, _q40, _q40, _q50, _q51, _q41, _p0 };
		    tree res = generic_simplify_109 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[8] ATTRIBUTE_UNUSED = { _p1, _q30, _q40, _q40, _q51, _q50, _q41, _p0 };
		    tree res = generic_simplify_109 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	          break;
	        }
	      case BIT_XOR_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_q40, 0);
		  tree _q51 = TREE_OPERAND (_q40, 1);
		  {
		    tree captures[8] ATTRIBUTE_UNUSED = { _p1, _q30, _q40, _q40, _q50, _q51, _q41, _p0 };
		    tree res = generic_simplify_109 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[8] ATTRIBUTE_UNUSED = { _p1, _q30, _q40, _q40, _q51, _q50, _q41, _p0 };
		    tree res = generic_simplify_109 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		    if (res) return res;
		  }
	          break;
	        }
	      default:;
	      }
	    switch (TREE_CODE (_q41))
	      {
	      case BIT_IOR_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_q41, 0);
		  tree _q61 = TREE_OPERAND (_q41, 1);
		  {
		    tree captures[8] ATTRIBUTE_UNUSED = { _p1, _q30, _q41, _q41, _q60, _q61, _q40, _p0 };
		    tree res = generic_simplify_109 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[8] ATTRIBUTE_UNUSED = { _p1, _q30, _q41, _q41, _q61, _q60, _q40, _p0 };
		    tree res = generic_simplify_109 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		    if (res) return res;
		  }
	          break;
	        }
	      case BIT_XOR_EXPR:
	        {
		  tree _q60 = TREE_OPERAND (_q41, 0);
		  tree _q61 = TREE_OPERAND (_q41, 1);
		  {
		    tree captures[8] ATTRIBUTE_UNUSED = { _p1, _q30, _q41, _q41, _q60, _q61, _q40, _p0 };
		    tree res = generic_simplify_109 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[8] ATTRIBUTE_UNUSED = { _p1, _q30, _q41, _q41, _q61, _q60, _q40, _p0 };
		    tree res = generic_simplify_109 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR);
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
		  tree res = generic_simplify_113 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
			    const enum tree_code op = BIT_IOR_EXPR;
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1300;
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1300;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1300;
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
next_after_fail1300:;
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
			    const enum tree_code op = BIT_IOR_EXPR;
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1301;
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1301;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1301;
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
next_after_fail1301:;
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, LT_EXPR, GE_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, LE_EXPR, GT_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, EQ_EXPR, NE_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, NE_EXPR, EQ_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, GE_EXPR, LT_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, GT_EXPR, LE_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, UNORDERED_EXPR, ORDERED_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, ORDERED_EXPR, UNORDERED_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, UNLT_EXPR, GE_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, UNLE_EXPR, GT_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, UNGT_EXPR, LE_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, UNGE_EXPR, LT_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, UNEQ_EXPR, LTGT_EXPR);
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
					    tree res = generic_simplify_14 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, LTGT_EXPR, UNEQ_EXPR);
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
			    const enum tree_code op = BIT_IOR_EXPR;
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1302;
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1302;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1302;
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
next_after_fail1302:;
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
			    const enum tree_code op = BIT_IOR_EXPR;
			    if (TREE_SIDE_EFFECTS (_p0)) goto next_after_fail1303;
			    if (TREE_SIDE_EFFECTS (_p1)) goto next_after_fail1303;
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1303;
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
next_after_fail1303:;
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, LT_EXPR, GE_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, LE_EXPR, GT_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, EQ_EXPR, NE_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, NE_EXPR, EQ_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, GE_EXPR, LT_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, GT_EXPR, LE_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, UNORDERED_EXPR, ORDERED_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, ORDERED_EXPR, UNORDERED_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, UNLT_EXPR, GE_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, UNLE_EXPR, GT_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, UNGT_EXPR, LE_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, UNGE_EXPR, LT_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, UNEQ_EXPR, LTGT_EXPR);
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
					    tree res = generic_simplify_15 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, LTGT_EXPR, UNEQ_EXPR);
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
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case GE_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q40 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q40, 0) && types_match (_q100, _q40)))
					{
					  if ((_q101 == _q41 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q41, 0) && types_match (_q101, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case GE_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
					{
					  if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR);
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
		case LE_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case GT_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q40 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q40, 0) && types_match (_q100, _q40)))
					{
					  if ((_q101 == _q41 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q41, 0) && types_match (_q101, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case GT_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
					{
					  if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
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
		case EQ_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case NE_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q40 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q40, 0) && types_match (_q100, _q40)))
					{
					  if ((_q101 == _q41 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q41, 0) && types_match (_q101, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case NE_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
					{
					  if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
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
		case NE_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case EQ_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q40 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q40, 0) && types_match (_q100, _q40)))
					{
					  if ((_q101 == _q41 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q41, 0) && types_match (_q101, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case EQ_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
					{
					  if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
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
		case GE_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case LT_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q40 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q40, 0) && types_match (_q100, _q40)))
					{
					  if ((_q101 == _q41 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q41, 0) && types_match (_q101, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case LT_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
					{
					  if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR);
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
		case GT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case LE_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q40 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q40, 0) && types_match (_q100, _q40)))
					{
					  if ((_q101 == _q41 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q41, 0) && types_match (_q101, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case LE_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
					{
					  if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
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
		case UNORDERED_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case ORDERED_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q40 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q40, 0) && types_match (_q100, _q40)))
					{
					  if ((_q101 == _q41 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q41, 0) && types_match (_q101, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNORDERED_EXPR, ORDERED_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case ORDERED_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
					{
					  if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNORDERED_EXPR, ORDERED_EXPR);
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
		case ORDERED_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case UNORDERED_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q40 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q40, 0) && types_match (_q100, _q40)))
					{
					  if ((_q101 == _q41 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q41, 0) && types_match (_q101, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, ORDERED_EXPR, UNORDERED_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case UNORDERED_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
					{
					  if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, ORDERED_EXPR, UNORDERED_EXPR);
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
		case UNLT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case GE_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q40 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q40, 0) && types_match (_q100, _q40)))
					{
					  if ((_q101 == _q41 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q41, 0) && types_match (_q101, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNLT_EXPR, GE_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case GE_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
					{
					  if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNLT_EXPR, GE_EXPR);
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
		case UNLE_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case GT_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q40 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q40, 0) && types_match (_q100, _q40)))
					{
					  if ((_q101 == _q41 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q41, 0) && types_match (_q101, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNLE_EXPR, GT_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case GT_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
					{
					  if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNLE_EXPR, GT_EXPR);
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
		case UNGT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case LE_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q40 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q40, 0) && types_match (_q100, _q40)))
					{
					  if ((_q101 == _q41 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q41, 0) && types_match (_q101, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNGT_EXPR, LE_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case LE_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
					{
					  if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNGT_EXPR, LE_EXPR);
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
		case UNGE_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case LT_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q40 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q40, 0) && types_match (_q100, _q40)))
					{
					  if ((_q101 == _q41 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q41, 0) && types_match (_q101, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNGE_EXPR, LT_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case LT_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
					{
					  if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNGE_EXPR, LT_EXPR);
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
		case UNEQ_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case LTGT_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q40 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q40, 0) && types_match (_q100, _q40)))
					{
					  if ((_q101 == _q41 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q41, 0) && types_match (_q101, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNEQ_EXPR, LTGT_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case LTGT_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
					{
					  if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNEQ_EXPR, LTGT_EXPR);
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
		case LTGT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case UNEQ_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q40 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q40, 0) && types_match (_q100, _q40)))
					{
					  if ((_q101 == _q41 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q41, 0) && types_match (_q101, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LTGT_EXPR, UNEQ_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case UNEQ_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
					{
					  if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q30, _q40, _q41, _q21, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LTGT_EXPR, UNEQ_EXPR);
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
	      switch (TREE_CODE (_q40))
	        {
		case LT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case GE_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q50 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q50, 0) && types_match (_q100, _q50)))
					{
					  if ((_q101 == _q51 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q51, 0) && types_match (_q101, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case GE_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q50 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q50, 0) && types_match (_q110, _q50)))
					{
					  if ((_q111 == _q51 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q51, 0) && types_match (_q111, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR);
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
		case LE_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case GT_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q50 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q50, 0) && types_match (_q100, _q50)))
					{
					  if ((_q101 == _q51 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q51, 0) && types_match (_q101, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case GT_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q50 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q50, 0) && types_match (_q110, _q50)))
					{
					  if ((_q111 == _q51 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q51, 0) && types_match (_q111, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
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
		case EQ_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case NE_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q50 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q50, 0) && types_match (_q100, _q50)))
					{
					  if ((_q101 == _q51 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q51, 0) && types_match (_q101, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case NE_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q50 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q50, 0) && types_match (_q110, _q50)))
					{
					  if ((_q111 == _q51 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q51, 0) && types_match (_q111, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
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
		case NE_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case EQ_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q50 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q50, 0) && types_match (_q100, _q50)))
					{
					  if ((_q101 == _q51 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q51, 0) && types_match (_q101, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case EQ_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q50 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q50, 0) && types_match (_q110, _q50)))
					{
					  if ((_q111 == _q51 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q51, 0) && types_match (_q111, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
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
		case GE_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case LT_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q50 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q50, 0) && types_match (_q100, _q50)))
					{
					  if ((_q101 == _q51 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q51, 0) && types_match (_q101, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case LT_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q50 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q50, 0) && types_match (_q110, _q50)))
					{
					  if ((_q111 == _q51 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q51, 0) && types_match (_q111, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR);
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
		case GT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case LE_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q50 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q50, 0) && types_match (_q100, _q50)))
					{
					  if ((_q101 == _q51 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q51, 0) && types_match (_q101, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case LE_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q50 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q50, 0) && types_match (_q110, _q50)))
					{
					  if ((_q111 == _q51 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q51, 0) && types_match (_q111, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
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
		case UNORDERED_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case ORDERED_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q50 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q50, 0) && types_match (_q100, _q50)))
					{
					  if ((_q101 == _q51 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q51, 0) && types_match (_q101, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNORDERED_EXPR, ORDERED_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case ORDERED_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q50 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q50, 0) && types_match (_q110, _q50)))
					{
					  if ((_q111 == _q51 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q51, 0) && types_match (_q111, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNORDERED_EXPR, ORDERED_EXPR);
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
		case ORDERED_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case UNORDERED_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q50 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q50, 0) && types_match (_q100, _q50)))
					{
					  if ((_q101 == _q51 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q51, 0) && types_match (_q101, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, ORDERED_EXPR, UNORDERED_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case UNORDERED_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q50 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q50, 0) && types_match (_q110, _q50)))
					{
					  if ((_q111 == _q51 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q51, 0) && types_match (_q111, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, ORDERED_EXPR, UNORDERED_EXPR);
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
		case UNLT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case GE_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q50 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q50, 0) && types_match (_q100, _q50)))
					{
					  if ((_q101 == _q51 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q51, 0) && types_match (_q101, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNLT_EXPR, GE_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case GE_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q50 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q50, 0) && types_match (_q110, _q50)))
					{
					  if ((_q111 == _q51 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q51, 0) && types_match (_q111, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNLT_EXPR, GE_EXPR);
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
		case UNLE_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case GT_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q50 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q50, 0) && types_match (_q100, _q50)))
					{
					  if ((_q101 == _q51 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q51, 0) && types_match (_q101, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNLE_EXPR, GT_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case GT_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q50 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q50, 0) && types_match (_q110, _q50)))
					{
					  if ((_q111 == _q51 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q51, 0) && types_match (_q111, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNLE_EXPR, GT_EXPR);
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
		case UNGT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case LE_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q50 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q50, 0) && types_match (_q100, _q50)))
					{
					  if ((_q101 == _q51 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q51, 0) && types_match (_q101, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNGT_EXPR, LE_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case LE_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q50 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q50, 0) && types_match (_q110, _q50)))
					{
					  if ((_q111 == _q51 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q51, 0) && types_match (_q111, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNGT_EXPR, LE_EXPR);
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
		case UNGE_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case LT_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q50 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q50, 0) && types_match (_q100, _q50)))
					{
					  if ((_q101 == _q51 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q51, 0) && types_match (_q101, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNGE_EXPR, LT_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case LT_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q50 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q50, 0) && types_match (_q110, _q50)))
					{
					  if ((_q111 == _q51 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q51, 0) && types_match (_q111, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNGE_EXPR, LT_EXPR);
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
		case UNEQ_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case LTGT_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q50 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q50, 0) && types_match (_q100, _q50)))
					{
					  if ((_q101 == _q51 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q51, 0) && types_match (_q101, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNEQ_EXPR, LTGT_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case LTGT_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q50 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q50, 0) && types_match (_q110, _q50)))
					{
					  if ((_q111 == _q51 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q51, 0) && types_match (_q111, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNEQ_EXPR, LTGT_EXPR);
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
		case LTGT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_p1, 0);
			  tree _q81 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q80))
			    {
			    CASE_CONVERT:
			      {
				tree _q90 = TREE_OPERAND (_q80, 0);
				switch (TREE_CODE (_q90))
				  {
				  case UNEQ_EXPR:
				    {
				      tree _q100 = TREE_OPERAND (_q90, 0);
				      tree _q101 = TREE_OPERAND (_q90, 1);
				      if ((_q100 == _q50 && ! TREE_SIDE_EFFECTS (_q100)) || (operand_equal_p (_q100, _q50, 0) && types_match (_q100, _q50)))
					{
					  if ((_q101 == _q51 && ! TREE_SIDE_EFFECTS (_q101)) || (operand_equal_p (_q101, _q51, 0) && types_match (_q101, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q90, _q81 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LTGT_EXPR, UNEQ_EXPR);
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
			  switch (TREE_CODE (_q81))
			    {
			    CASE_CONVERT:
			      {
				tree _q100 = TREE_OPERAND (_q81, 0);
				switch (TREE_CODE (_q100))
				  {
				  case UNEQ_EXPR:
				    {
				      tree _q110 = TREE_OPERAND (_q100, 0);
				      tree _q111 = TREE_OPERAND (_q100, 1);
				      if ((_q110 == _q50 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q50, 0) && types_match (_q110, _q50)))
					{
					  if ((_q111 == _q51 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q51, 0) && types_match (_q111, _q51)))
					    {
					      {
						tree captures[6] ATTRIBUTE_UNUSED = { _q40, _q50, _q51, _q20, _q100, _q80 };
						tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LTGT_EXPR, UNEQ_EXPR);
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
	        default:;
	        }
	      break;
	    }
          default:;
          }
	switch (TREE_CODE (_q20))
	  {
	  case LT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case GE_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
			    {
			      if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case GE_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
			    {
			      if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR);
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
	  case LE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case GT_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
			    {
			      if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case GT_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
			    {
			      if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
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
	  case EQ_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case NE_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
			    {
			      if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case NE_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
			    {
			      if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
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
	  case NE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case EQ_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
			    {
			      if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case EQ_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
			    {
			      if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
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
	  case GE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case LT_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
			    {
			      if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case LT_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
			    {
			      if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR);
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
	  case GT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case LE_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
			    {
			      if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case LE_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
			    {
			      if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
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
	  case UNORDERED_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case ORDERED_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
			    {
			      if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNORDERED_EXPR, ORDERED_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case ORDERED_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
			    {
			      if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNORDERED_EXPR, ORDERED_EXPR);
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
	  case ORDERED_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case UNORDERED_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
			    {
			      if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, ORDERED_EXPR, UNORDERED_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case UNORDERED_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
			    {
			      if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, ORDERED_EXPR, UNORDERED_EXPR);
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
	  case UNLT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case GE_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
			    {
			      if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNLT_EXPR, GE_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case GE_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
			    {
			      if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNLT_EXPR, GE_EXPR);
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
	  case UNLE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case GT_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
			    {
			      if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNLE_EXPR, GT_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case GT_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
			    {
			      if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNLE_EXPR, GT_EXPR);
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
	  case UNGT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case LE_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
			    {
			      if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNGT_EXPR, LE_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case LE_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
			    {
			      if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNGT_EXPR, LE_EXPR);
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
	  case UNGE_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case LT_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
			    {
			      if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNGE_EXPR, LT_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case LT_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
			    {
			      if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNGE_EXPR, LT_EXPR);
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
	  case UNEQ_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case LTGT_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
			    {
			      if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNEQ_EXPR, LTGT_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case LTGT_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
			    {
			      if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNEQ_EXPR, LTGT_EXPR);
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
	  case LTGT_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case UNEQ_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q30 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q30, 0) && types_match (_q80, _q30)))
			    {
			      if ((_q81 == _q31 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q31, 0) && types_match (_q81, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LTGT_EXPR, UNEQ_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case UNEQ_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q30 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q30, 0) && types_match (_q90, _q30)))
			    {
			      if ((_q91 == _q31 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q31, 0) && types_match (_q91, _q31)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q20, _q30, _q31, _q21, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LTGT_EXPR, UNEQ_EXPR);
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
	  case LT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case GE_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
			    {
			      if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case GE_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
			    {
			      if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR);
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
	  case LE_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case GT_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
			    {
			      if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case GT_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
			    {
			      if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
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
	  case EQ_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case NE_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
			    {
			      if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case NE_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
			    {
			      if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
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
	  case NE_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case EQ_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
			    {
			      if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case EQ_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
			    {
			      if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
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
	  case GE_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case LT_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
			    {
			      if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case LT_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
			    {
			      if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR);
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
	  case GT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case LE_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
			    {
			      if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case LE_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
			    {
			      if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
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
	  case UNORDERED_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case ORDERED_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
			    {
			      if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNORDERED_EXPR, ORDERED_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case ORDERED_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
			    {
			      if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNORDERED_EXPR, ORDERED_EXPR);
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
	  case ORDERED_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case UNORDERED_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
			    {
			      if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, ORDERED_EXPR, UNORDERED_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case UNORDERED_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
			    {
			      if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, ORDERED_EXPR, UNORDERED_EXPR);
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
	  case UNLT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case GE_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
			    {
			      if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNLT_EXPR, GE_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case GE_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
			    {
			      if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNLT_EXPR, GE_EXPR);
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
	  case UNLE_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case GT_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
			    {
			      if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNLE_EXPR, GT_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case GT_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
			    {
			      if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNLE_EXPR, GT_EXPR);
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
	  case UNGT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case LE_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
			    {
			      if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNGT_EXPR, LE_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case LE_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
			    {
			      if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNGT_EXPR, LE_EXPR);
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
	  case UNGE_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case LT_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
			    {
			      if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNGE_EXPR, LT_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case LT_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
			    {
			      if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNGE_EXPR, LT_EXPR);
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
	  case UNEQ_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case LTGT_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
			    {
			      if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNEQ_EXPR, LTGT_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case LTGT_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
			    {
			      if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, UNEQ_EXPR, LTGT_EXPR);
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
	  case LTGT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    switch (TREE_CODE (_q70))
		      {
		      case UNEQ_EXPR:
		        {
			  tree _q80 = TREE_OPERAND (_q70, 0);
			  tree _q81 = TREE_OPERAND (_q70, 1);
			  if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
			    {
			      if ((_q81 == _q41 && ! TREE_SIDE_EFFECTS (_q81)) || (operand_equal_p (_q81, _q41, 0) && types_match (_q81, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q70, _q71 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LTGT_EXPR, UNEQ_EXPR);
				    if (res) return res;
				  }
			        }
			    }
		          break;
		        }
		      default:;
		      }
		    switch (TREE_CODE (_q71))
		      {
		      case UNEQ_EXPR:
		        {
			  tree _q90 = TREE_OPERAND (_q71, 0);
			  tree _q91 = TREE_OPERAND (_q71, 1);
			  if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
			    {
			      if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				{
				  {
				    tree captures[6] ATTRIBUTE_UNUSED = { _q21, _q40, _q41, _q20, _q71, _q70 };
				    tree res = generic_simplify_115 (loc, type, _p0, _p1, captures, LTGT_EXPR, UNEQ_EXPR);
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
	switch (TREE_CODE (_q20))
	  {
	  case VEC_COND_EXPR:
	    {
	      tree _q30 = TREE_OPERAND (_q20, 0);
	      tree _q31 = TREE_OPERAND (_q20, 1);
	      tree _q32 = TREE_OPERAND (_q20, 2);
	      switch (TREE_CODE (_q30))
	        {
		case LT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case GE_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
					{
					  if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
					    {
					      if ((_q111 == _q31 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q31, 0) && types_match (_q111, _q31)))
						{
						  if ((_q112 == _q32 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q32, 0) && types_match (_q112, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case GE_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
					{
					  if ((_q131 == _q41 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q41, 0) && types_match (_q131, _q41)))
					    {
					      if ((_q121 == _q31 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q31, 0) && types_match (_q121, _q31)))
						{
						  if ((_q122 == _q32 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q32, 0) && types_match (_q122, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR);
							if (res) return res;
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
		case LE_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case GT_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
					{
					  if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
					    {
					      if ((_q111 == _q31 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q31, 0) && types_match (_q111, _q31)))
						{
						  if ((_q112 == _q32 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q32, 0) && types_match (_q112, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case GT_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
					{
					  if ((_q131 == _q41 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q41, 0) && types_match (_q131, _q41)))
					    {
					      if ((_q121 == _q31 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q31, 0) && types_match (_q121, _q31)))
						{
						  if ((_q122 == _q32 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q32, 0) && types_match (_q122, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
							if (res) return res;
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
		case EQ_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case NE_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
					{
					  if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
					    {
					      if ((_q111 == _q31 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q31, 0) && types_match (_q111, _q31)))
						{
						  if ((_q112 == _q32 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q32, 0) && types_match (_q112, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case NE_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
					{
					  if ((_q131 == _q41 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q41, 0) && types_match (_q131, _q41)))
					    {
					      if ((_q121 == _q31 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q31, 0) && types_match (_q121, _q31)))
						{
						  if ((_q122 == _q32 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q32, 0) && types_match (_q122, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
							if (res) return res;
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
		case NE_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case EQ_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
					{
					  if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
					    {
					      if ((_q111 == _q31 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q31, 0) && types_match (_q111, _q31)))
						{
						  if ((_q112 == _q32 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q32, 0) && types_match (_q112, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case EQ_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
					{
					  if ((_q131 == _q41 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q41, 0) && types_match (_q131, _q41)))
					    {
					      if ((_q121 == _q31 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q31, 0) && types_match (_q121, _q31)))
						{
						  if ((_q122 == _q32 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q32, 0) && types_match (_q122, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
							if (res) return res;
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
		case GE_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case LT_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
					{
					  if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
					    {
					      if ((_q111 == _q31 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q31, 0) && types_match (_q111, _q31)))
						{
						  if ((_q112 == _q32 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q32, 0) && types_match (_q112, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case LT_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
					{
					  if ((_q131 == _q41 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q41, 0) && types_match (_q131, _q41)))
					    {
					      if ((_q121 == _q31 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q31, 0) && types_match (_q121, _q31)))
						{
						  if ((_q122 == _q32 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q32, 0) && types_match (_q122, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR);
							if (res) return res;
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
		case GT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case LE_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
					{
					  if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
					    {
					      if ((_q111 == _q31 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q31, 0) && types_match (_q111, _q31)))
						{
						  if ((_q112 == _q32 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q32, 0) && types_match (_q112, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case LE_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
					{
					  if ((_q131 == _q41 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q41, 0) && types_match (_q131, _q41)))
					    {
					      if ((_q121 == _q31 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q31, 0) && types_match (_q121, _q31)))
						{
						  if ((_q122 == _q32 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q32, 0) && types_match (_q122, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
							if (res) return res;
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
		case UNORDERED_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case ORDERED_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
					{
					  if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
					    {
					      if ((_q111 == _q31 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q31, 0) && types_match (_q111, _q31)))
						{
						  if ((_q112 == _q32 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q32, 0) && types_match (_q112, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNORDERED_EXPR, ORDERED_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case ORDERED_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
					{
					  if ((_q131 == _q41 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q41, 0) && types_match (_q131, _q41)))
					    {
					      if ((_q121 == _q31 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q31, 0) && types_match (_q121, _q31)))
						{
						  if ((_q122 == _q32 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q32, 0) && types_match (_q122, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNORDERED_EXPR, ORDERED_EXPR);
							if (res) return res;
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
		case ORDERED_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case UNORDERED_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
					{
					  if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
					    {
					      if ((_q111 == _q31 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q31, 0) && types_match (_q111, _q31)))
						{
						  if ((_q112 == _q32 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q32, 0) && types_match (_q112, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, ORDERED_EXPR, UNORDERED_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case UNORDERED_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
					{
					  if ((_q131 == _q41 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q41, 0) && types_match (_q131, _q41)))
					    {
					      if ((_q121 == _q31 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q31, 0) && types_match (_q121, _q31)))
						{
						  if ((_q122 == _q32 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q32, 0) && types_match (_q122, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, ORDERED_EXPR, UNORDERED_EXPR);
							if (res) return res;
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
		case UNLT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case GE_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
					{
					  if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
					    {
					      if ((_q111 == _q31 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q31, 0) && types_match (_q111, _q31)))
						{
						  if ((_q112 == _q32 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q32, 0) && types_match (_q112, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNLT_EXPR, GE_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case GE_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
					{
					  if ((_q131 == _q41 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q41, 0) && types_match (_q131, _q41)))
					    {
					      if ((_q121 == _q31 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q31, 0) && types_match (_q121, _q31)))
						{
						  if ((_q122 == _q32 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q32, 0) && types_match (_q122, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNLT_EXPR, GE_EXPR);
							if (res) return res;
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
		case UNLE_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case GT_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
					{
					  if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
					    {
					      if ((_q111 == _q31 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q31, 0) && types_match (_q111, _q31)))
						{
						  if ((_q112 == _q32 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q32, 0) && types_match (_q112, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNLE_EXPR, GT_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case GT_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
					{
					  if ((_q131 == _q41 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q41, 0) && types_match (_q131, _q41)))
					    {
					      if ((_q121 == _q31 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q31, 0) && types_match (_q121, _q31)))
						{
						  if ((_q122 == _q32 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q32, 0) && types_match (_q122, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNLE_EXPR, GT_EXPR);
							if (res) return res;
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
		case UNGT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case LE_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
					{
					  if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
					    {
					      if ((_q111 == _q31 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q31, 0) && types_match (_q111, _q31)))
						{
						  if ((_q112 == _q32 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q32, 0) && types_match (_q112, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNGT_EXPR, LE_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case LE_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
					{
					  if ((_q131 == _q41 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q41, 0) && types_match (_q131, _q41)))
					    {
					      if ((_q121 == _q31 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q31, 0) && types_match (_q121, _q31)))
						{
						  if ((_q122 == _q32 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q32, 0) && types_match (_q122, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNGT_EXPR, LE_EXPR);
							if (res) return res;
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
		case UNGE_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case LT_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
					{
					  if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
					    {
					      if ((_q111 == _q31 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q31, 0) && types_match (_q111, _q31)))
						{
						  if ((_q112 == _q32 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q32, 0) && types_match (_q112, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNGE_EXPR, LT_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case LT_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
					{
					  if ((_q131 == _q41 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q41, 0) && types_match (_q131, _q41)))
					    {
					      if ((_q121 == _q31 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q31, 0) && types_match (_q121, _q31)))
						{
						  if ((_q122 == _q32 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q32, 0) && types_match (_q122, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNGE_EXPR, LT_EXPR);
							if (res) return res;
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
		case UNEQ_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case LTGT_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
					{
					  if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
					    {
					      if ((_q111 == _q31 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q31, 0) && types_match (_q111, _q31)))
						{
						  if ((_q112 == _q32 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q32, 0) && types_match (_q112, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNEQ_EXPR, LTGT_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case LTGT_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
					{
					  if ((_q131 == _q41 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q41, 0) && types_match (_q131, _q41)))
					    {
					      if ((_q121 == _q31 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q31, 0) && types_match (_q121, _q31)))
						{
						  if ((_q122 == _q32 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q32, 0) && types_match (_q122, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNEQ_EXPR, LTGT_EXPR);
							if (res) return res;
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
		case LTGT_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case UNEQ_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
					{
					  if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
					    {
					      if ((_q111 == _q31 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q31, 0) && types_match (_q111, _q31)))
						{
						  if ((_q112 == _q32 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q32, 0) && types_match (_q112, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, LTGT_EXPR, UNEQ_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case UNEQ_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
					{
					  if ((_q131 == _q41 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q41, 0) && types_match (_q131, _q41)))
					    {
					      if ((_q121 == _q31 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q31, 0) && types_match (_q121, _q31)))
						{
						  if ((_q122 == _q32 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q32, 0) && types_match (_q122, _q32)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q20, _q30, _q40, _q41, _q31, _q32, _q21, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, LTGT_EXPR, UNEQ_EXPR);
							if (res) return res;
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
          default:;
          }
	switch (TREE_CODE (_q21))
	  {
	  case VEC_COND_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      tree _q41 = TREE_OPERAND (_q21, 1);
	      tree _q42 = TREE_OPERAND (_q21, 2);
	      switch (TREE_CODE (_q40))
	        {
		case LT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case GE_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q50 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q50, 0) && types_match (_q120, _q50)))
					{
					  if ((_q121 == _q51 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q51, 0) && types_match (_q121, _q51)))
					    {
					      if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
						{
						  if ((_q112 == _q42 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q42, 0) && types_match (_q112, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case GE_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q50 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q50, 0) && types_match (_q130, _q50)))
					{
					  if ((_q131 == _q51 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q51, 0) && types_match (_q131, _q51)))
					    {
					      if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
						{
						  if ((_q122 == _q42 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q42, 0) && types_match (_q122, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR);
							if (res) return res;
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
		case LE_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case GT_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q50 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q50, 0) && types_match (_q120, _q50)))
					{
					  if ((_q121 == _q51 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q51, 0) && types_match (_q121, _q51)))
					    {
					      if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
						{
						  if ((_q112 == _q42 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q42, 0) && types_match (_q112, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case GT_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q50 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q50, 0) && types_match (_q130, _q50)))
					{
					  if ((_q131 == _q51 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q51, 0) && types_match (_q131, _q51)))
					    {
					      if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
						{
						  if ((_q122 == _q42 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q42, 0) && types_match (_q122, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
							if (res) return res;
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
		case EQ_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case NE_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q50 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q50, 0) && types_match (_q120, _q50)))
					{
					  if ((_q121 == _q51 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q51, 0) && types_match (_q121, _q51)))
					    {
					      if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
						{
						  if ((_q112 == _q42 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q42, 0) && types_match (_q112, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case NE_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q50 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q50, 0) && types_match (_q130, _q50)))
					{
					  if ((_q131 == _q51 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q51, 0) && types_match (_q131, _q51)))
					    {
					      if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
						{
						  if ((_q122 == _q42 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q42, 0) && types_match (_q122, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
							if (res) return res;
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
		case NE_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case EQ_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q50 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q50, 0) && types_match (_q120, _q50)))
					{
					  if ((_q121 == _q51 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q51, 0) && types_match (_q121, _q51)))
					    {
					      if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
						{
						  if ((_q112 == _q42 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q42, 0) && types_match (_q112, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case EQ_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q50 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q50, 0) && types_match (_q130, _q50)))
					{
					  if ((_q131 == _q51 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q51, 0) && types_match (_q131, _q51)))
					    {
					      if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
						{
						  if ((_q122 == _q42 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q42, 0) && types_match (_q122, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
							if (res) return res;
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
		case GE_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case LT_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q50 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q50, 0) && types_match (_q120, _q50)))
					{
					  if ((_q121 == _q51 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q51, 0) && types_match (_q121, _q51)))
					    {
					      if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
						{
						  if ((_q112 == _q42 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q42, 0) && types_match (_q112, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case LT_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q50 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q50, 0) && types_match (_q130, _q50)))
					{
					  if ((_q131 == _q51 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q51, 0) && types_match (_q131, _q51)))
					    {
					      if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
						{
						  if ((_q122 == _q42 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q42, 0) && types_match (_q122, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR);
							if (res) return res;
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
		case GT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case LE_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q50 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q50, 0) && types_match (_q120, _q50)))
					{
					  if ((_q121 == _q51 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q51, 0) && types_match (_q121, _q51)))
					    {
					      if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
						{
						  if ((_q112 == _q42 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q42, 0) && types_match (_q112, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case LE_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q50 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q50, 0) && types_match (_q130, _q50)))
					{
					  if ((_q131 == _q51 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q51, 0) && types_match (_q131, _q51)))
					    {
					      if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
						{
						  if ((_q122 == _q42 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q42, 0) && types_match (_q122, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
							if (res) return res;
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
		case UNORDERED_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case ORDERED_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q50 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q50, 0) && types_match (_q120, _q50)))
					{
					  if ((_q121 == _q51 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q51, 0) && types_match (_q121, _q51)))
					    {
					      if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
						{
						  if ((_q112 == _q42 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q42, 0) && types_match (_q112, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNORDERED_EXPR, ORDERED_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case ORDERED_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q50 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q50, 0) && types_match (_q130, _q50)))
					{
					  if ((_q131 == _q51 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q51, 0) && types_match (_q131, _q51)))
					    {
					      if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
						{
						  if ((_q122 == _q42 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q42, 0) && types_match (_q122, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNORDERED_EXPR, ORDERED_EXPR);
							if (res) return res;
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
		case ORDERED_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case UNORDERED_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q50 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q50, 0) && types_match (_q120, _q50)))
					{
					  if ((_q121 == _q51 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q51, 0) && types_match (_q121, _q51)))
					    {
					      if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
						{
						  if ((_q112 == _q42 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q42, 0) && types_match (_q112, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, ORDERED_EXPR, UNORDERED_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case UNORDERED_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q50 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q50, 0) && types_match (_q130, _q50)))
					{
					  if ((_q131 == _q51 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q51, 0) && types_match (_q131, _q51)))
					    {
					      if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
						{
						  if ((_q122 == _q42 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q42, 0) && types_match (_q122, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, ORDERED_EXPR, UNORDERED_EXPR);
							if (res) return res;
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
		case UNLT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case GE_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q50 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q50, 0) && types_match (_q120, _q50)))
					{
					  if ((_q121 == _q51 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q51, 0) && types_match (_q121, _q51)))
					    {
					      if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
						{
						  if ((_q112 == _q42 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q42, 0) && types_match (_q112, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNLT_EXPR, GE_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case GE_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q50 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q50, 0) && types_match (_q130, _q50)))
					{
					  if ((_q131 == _q51 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q51, 0) && types_match (_q131, _q51)))
					    {
					      if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
						{
						  if ((_q122 == _q42 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q42, 0) && types_match (_q122, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNLT_EXPR, GE_EXPR);
							if (res) return res;
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
		case UNLE_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case GT_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q50 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q50, 0) && types_match (_q120, _q50)))
					{
					  if ((_q121 == _q51 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q51, 0) && types_match (_q121, _q51)))
					    {
					      if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
						{
						  if ((_q112 == _q42 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q42, 0) && types_match (_q112, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNLE_EXPR, GT_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case GT_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q50 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q50, 0) && types_match (_q130, _q50)))
					{
					  if ((_q131 == _q51 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q51, 0) && types_match (_q131, _q51)))
					    {
					      if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
						{
						  if ((_q122 == _q42 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q42, 0) && types_match (_q122, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNLE_EXPR, GT_EXPR);
							if (res) return res;
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
		case UNGT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case LE_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q50 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q50, 0) && types_match (_q120, _q50)))
					{
					  if ((_q121 == _q51 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q51, 0) && types_match (_q121, _q51)))
					    {
					      if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
						{
						  if ((_q112 == _q42 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q42, 0) && types_match (_q112, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNGT_EXPR, LE_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case LE_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q50 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q50, 0) && types_match (_q130, _q50)))
					{
					  if ((_q131 == _q51 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q51, 0) && types_match (_q131, _q51)))
					    {
					      if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
						{
						  if ((_q122 == _q42 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q42, 0) && types_match (_q122, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNGT_EXPR, LE_EXPR);
							if (res) return res;
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
		case UNGE_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case LT_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q50 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q50, 0) && types_match (_q120, _q50)))
					{
					  if ((_q121 == _q51 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q51, 0) && types_match (_q121, _q51)))
					    {
					      if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
						{
						  if ((_q112 == _q42 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q42, 0) && types_match (_q112, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNGE_EXPR, LT_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case LT_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q50 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q50, 0) && types_match (_q130, _q50)))
					{
					  if ((_q131 == _q51 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q51, 0) && types_match (_q131, _q51)))
					    {
					      if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
						{
						  if ((_q122 == _q42 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q42, 0) && types_match (_q122, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNGE_EXPR, LT_EXPR);
							if (res) return res;
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
		case UNEQ_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case LTGT_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q50 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q50, 0) && types_match (_q120, _q50)))
					{
					  if ((_q121 == _q51 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q51, 0) && types_match (_q121, _q51)))
					    {
					      if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
						{
						  if ((_q112 == _q42 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q42, 0) && types_match (_q112, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNEQ_EXPR, LTGT_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case LTGT_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q50 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q50, 0) && types_match (_q130, _q50)))
					{
					  if ((_q131 == _q51 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q51, 0) && types_match (_q131, _q51)))
					    {
					      if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
						{
						  if ((_q122 == _q42 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q42, 0) && types_match (_q122, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, UNEQ_EXPR, LTGT_EXPR);
							if (res) return res;
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
		case LTGT_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    switch (TREE_CODE (_p1))
		      {
		      case BIT_AND_EXPR:
		        {
			  tree _q100 = TREE_OPERAND (_p1, 0);
			  tree _q101 = TREE_OPERAND (_p1, 1);
			  switch (TREE_CODE (_q100))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q110 = TREE_OPERAND (_q100, 0);
				tree _q111 = TREE_OPERAND (_q100, 1);
				tree _q112 = TREE_OPERAND (_q100, 2);
				switch (TREE_CODE (_q110))
				  {
				  case UNEQ_EXPR:
				    {
				      tree _q120 = TREE_OPERAND (_q110, 0);
				      tree _q121 = TREE_OPERAND (_q110, 1);
				      if ((_q120 == _q50 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q50, 0) && types_match (_q120, _q50)))
					{
					  if ((_q121 == _q51 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q51, 0) && types_match (_q121, _q51)))
					    {
					      if ((_q111 == _q41 && ! TREE_SIDE_EFFECTS (_q111)) || (operand_equal_p (_q111, _q41, 0) && types_match (_q111, _q41)))
						{
						  if ((_q112 == _q42 && ! TREE_SIDE_EFFECTS (_q112)) || (operand_equal_p (_q112, _q42, 0) && types_match (_q112, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q100, _q110, _q101 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, LTGT_EXPR, UNEQ_EXPR);
							if (res) return res;
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
			  switch (TREE_CODE (_q101))
			    {
			    case VEC_COND_EXPR:
			      {
				tree _q120 = TREE_OPERAND (_q101, 0);
				tree _q121 = TREE_OPERAND (_q101, 1);
				tree _q122 = TREE_OPERAND (_q101, 2);
				switch (TREE_CODE (_q120))
				  {
				  case UNEQ_EXPR:
				    {
				      tree _q130 = TREE_OPERAND (_q120, 0);
				      tree _q131 = TREE_OPERAND (_q120, 1);
				      if ((_q130 == _q50 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q50, 0) && types_match (_q130, _q50)))
					{
					  if ((_q131 == _q51 && ! TREE_SIDE_EFFECTS (_q131)) || (operand_equal_p (_q131, _q51, 0) && types_match (_q131, _q51)))
					    {
					      if ((_q121 == _q41 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q41, 0) && types_match (_q121, _q41)))
						{
						  if ((_q122 == _q42 && ! TREE_SIDE_EFFECTS (_q122)) || (operand_equal_p (_q122, _q42, 0) && types_match (_q122, _q42)))
						    {
						      {
							tree captures[10] ATTRIBUTE_UNUSED = { _q21, _q40, _q50, _q51, _q41, _q42, _q20, _q101, _q120, _q100 };
							tree res = generic_simplify_116 (loc, type, _p0, _p1, captures, LTGT_EXPR, UNEQ_EXPR);
							if (res) return res;
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
	  case BIT_NOT_EXPR:
	    {
	      tree _q40 = TREE_OPERAND (_q21, 0);
	      switch (TREE_CODE (_p1))
	        {
		case BIT_AND_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q40, _p1, _q60 };
			  tree res = generic_simplify_117 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q40 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q40, 0) && types_match (_q60, _q40)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q20, _q40, _p1, _q61 };
			  tree res = generic_simplify_117 (loc, type, _p0, _p1, captures);
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
		case BIT_AND_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_p1, 0);
		    tree _q61 = TREE_OPERAND (_p1, 1);
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q30, _p1, _q60 };
			  tree res = generic_simplify_117 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q30 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q30, 0) && types_match (_q60, _q30)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q30, _p1, _q61 };
			  tree res = generic_simplify_117 (loc, type, _p0, _p1, captures);
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
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      switch (TREE_CODE (_q51))
	        {
		case BIT_NOT_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_q51, 0);
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q50, _q21, _p0, _q20 };
			  tree res = generic_simplify_117 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q20 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q20, 0) && types_match (_q70, _q20)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q50, _q20, _p0, _q21 };
			  tree res = generic_simplify_117 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    break;
		  }
	        default:;
	        }
	      switch (TREE_CODE (_q50))
	        {
		case BIT_NOT_EXPR:
		  {
		    tree _q60 = TREE_OPERAND (_q50, 0);
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q51, _q21, _p0, _q20 };
			  tree res = generic_simplify_117 (loc, type, _p0, _p1, captures);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q20 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q20, 0) && types_match (_q60, _q20)))
		      {
			{
			  tree captures[5] ATTRIBUTE_UNUSED = { _p1, _q51, _q20, _p0, _q21 };
			  tree res = generic_simplify_117 (loc, type, _p0, _p1, captures);
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
		      tree res = generic_simplify_118 (loc, type, _p0, _p1, captures, BIT_XOR_EXPR, BIT_IOR_EXPR);
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
    case GT_EXPR:
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
		  if (tree_min_value (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			tree res = generic_simplify_119 (loc, type, _p0, _p1, captures);
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
			tree res = generic_simplify_120 (loc, type, _p0, _p1, captures);
			if (res) return res;
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
		  if (tree_min_value (_q51))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q20, _p0, _q21 };
			tree res = generic_simplify_121 (loc, type, _p0, _p1, captures);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
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
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
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
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
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
		    tree res = generic_simplify_124 (loc, type, _p0, _p1, captures, LT_EXPR, GT_EXPR);
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
		    tree res = generic_simplify_124 (loc, type, _p0, _p1, captures, LE_EXPR, GT_EXPR);
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
		    tree res = generic_simplify_124 (loc, type, _p0, _p1, captures, GT_EXPR, GT_EXPR);
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
		    tree res = generic_simplify_124 (loc, type, _p0, _p1, captures, GE_EXPR, GT_EXPR);
		    if (res) return res;
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
	      switch (TREE_CODE (_q30))
	        {
		case BIT_XOR_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    if (tree_expr_nonzero_p (_q31))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case NE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_p1, 0);
			      tree _q91 = TREE_OPERAND (_p1, 1);
			      if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
				{
				  if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q40, _q41, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GT_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q41, _q40, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GT_EXPR);
					if (res) return res;
				      }
				    }
			        }
			      if ((_q90 == _q41 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q41, 0) && types_match (_q90, _q41)))
				{
				  if ((_q91 == _q40 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q40, 0) && types_match (_q91, _q40)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q40, _q41, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GT_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q41, _q40, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GT_EXPR);
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
	    if (tree_expr_nonzero_p (_q30))
	      {
		switch (TREE_CODE (_q31))
		  {
		  case BIT_XOR_EXPR:
		    {
		      tree _q50 = TREE_OPERAND (_q31, 0);
		      tree _q51 = TREE_OPERAND (_q31, 1);
		      switch (TREE_CODE (_p1))
		        {
			case NE_EXPR:
			  {
			    tree _q90 = TREE_OPERAND (_p1, 0);
			    tree _q91 = TREE_OPERAND (_p1, 1);
			    if ((_q90 == _q50 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q50, 0) && types_match (_q90, _q50)))
			      {
				if ((_q91 == _q51 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q51, 0) && types_match (_q91, _q51)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GT_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GT_EXPR);
				      if (res) return res;
				    }
				  }
			      }
			    if ((_q90 == _q51 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q51, 0) && types_match (_q90, _q51)))
			      {
				if ((_q91 == _q50 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q50, 0) && types_match (_q91, _q50)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GT_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GT_EXPR);
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
	      switch (TREE_CODE (_q40))
	        {
		case BIT_XOR_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    if (tree_expr_nonzero_p (_q41))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case NE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_p1, 0);
			      tree _q91 = TREE_OPERAND (_p1, 1);
			      if ((_q90 == _q50 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q50, 0) && types_match (_q90, _q50)))
				{
				  if ((_q91 == _q51 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q51, 0) && types_match (_q91, _q51)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LT_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LT_EXPR);
					if (res) return res;
				      }
				    }
			        }
			      if ((_q90 == _q51 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q51, 0) && types_match (_q90, _q51)))
				{
				  if ((_q91 == _q50 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q50, 0) && types_match (_q91, _q50)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LT_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LT_EXPR);
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
	    if (tree_expr_nonzero_p (_q40))
	      {
		switch (TREE_CODE (_q41))
		  {
		  case BIT_XOR_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_q41, 0);
		      tree _q61 = TREE_OPERAND (_q41, 1);
		      switch (TREE_CODE (_p1))
		        {
			case NE_EXPR:
			  {
			    tree _q90 = TREE_OPERAND (_p1, 0);
			    tree _q91 = TREE_OPERAND (_p1, 1);
			    if ((_q90 == _q60 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q60, 0) && types_match (_q90, _q60)))
			      {
				if ((_q91 == _q61 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q61, 0) && types_match (_q91, _q61)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q60, _q61, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LT_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q61, _q60, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LT_EXPR);
				      if (res) return res;
				    }
				  }
			      }
			    if ((_q90 == _q61 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q61, 0) && types_match (_q90, _q61)))
			      {
				if ((_q91 == _q60 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q60, 0) && types_match (_q91, _q60)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q60, _q61, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LT_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q61, _q60, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LT_EXPR);
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
		case NE_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
		      {
			if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, GT_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, GT_EXPR);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
		      {
			if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, GT_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, GT_EXPR);
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
		case NE_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
		      {
			if ((_q71 == _q41 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q41, 0) && types_match (_q71, _q41)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q41, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, LT_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q41, _q40, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, LT_EXPR);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q70 == _q41 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q41, 0) && types_match (_q70, _q41)))
		      {
			if ((_q71 == _q40 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q40, 0) && types_match (_q71, _q40)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q41, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, LT_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q41, _q40, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, LT_EXPR);
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
    case LT_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case NE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if (tree_min_value (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q21, _q20, _p1 };
			tree res = generic_simplify_119 (loc, type, _p0, _p1, captures);
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
			tree res = generic_simplify_120 (loc, type, _p0, _p1, captures);
			if (res) return res;
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
		  if (tree_min_value (_q51))
		    {
		      {
			tree captures[4] ATTRIBUTE_UNUSED = { _p1, _q21, _p0, _q20 };
			tree res = generic_simplify_121 (loc, type, _p0, _p1, captures);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
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
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
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
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
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
		    tree res = generic_simplify_124 (loc, type, _p0, _p1, captures, LT_EXPR, LT_EXPR);
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
		    tree res = generic_simplify_124 (loc, type, _p0, _p1, captures, LE_EXPR, LT_EXPR);
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
		    tree res = generic_simplify_124 (loc, type, _p0, _p1, captures, GT_EXPR, LT_EXPR);
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
		    tree res = generic_simplify_124 (loc, type, _p0, _p1, captures, GE_EXPR, LT_EXPR);
		    if (res) return res;
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
	      switch (TREE_CODE (_q30))
	        {
		case BIT_XOR_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    if (tree_expr_nonzero_p (_q31))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case NE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_p1, 0);
			      tree _q91 = TREE_OPERAND (_p1, 1);
			      if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
				{
				  if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q40, _q41, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LT_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q41, _q40, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LT_EXPR);
					if (res) return res;
				      }
				    }
			        }
			      if ((_q90 == _q41 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q41, 0) && types_match (_q90, _q41)))
				{
				  if ((_q91 == _q40 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q40, 0) && types_match (_q91, _q40)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q40, _q41, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LT_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q41, _q40, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LT_EXPR);
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
	    if (tree_expr_nonzero_p (_q30))
	      {
		switch (TREE_CODE (_q31))
		  {
		  case BIT_XOR_EXPR:
		    {
		      tree _q50 = TREE_OPERAND (_q31, 0);
		      tree _q51 = TREE_OPERAND (_q31, 1);
		      switch (TREE_CODE (_p1))
		        {
			case NE_EXPR:
			  {
			    tree _q90 = TREE_OPERAND (_p1, 0);
			    tree _q91 = TREE_OPERAND (_p1, 1);
			    if ((_q90 == _q50 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q50, 0) && types_match (_q90, _q50)))
			      {
				if ((_q91 == _q51 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q51, 0) && types_match (_q91, _q51)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LT_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LT_EXPR);
				      if (res) return res;
				    }
				  }
			      }
			    if ((_q90 == _q51 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q51, 0) && types_match (_q90, _q51)))
			      {
				if ((_q91 == _q50 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q50, 0) && types_match (_q91, _q50)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LT_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LT_EXPR);
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
	      switch (TREE_CODE (_q40))
	        {
		case BIT_XOR_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    if (tree_expr_nonzero_p (_q41))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case NE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_p1, 0);
			      tree _q91 = TREE_OPERAND (_p1, 1);
			      if ((_q90 == _q50 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q50, 0) && types_match (_q90, _q50)))
				{
				  if ((_q91 == _q51 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q51, 0) && types_match (_q91, _q51)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GT_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GT_EXPR);
					if (res) return res;
				      }
				    }
			        }
			      if ((_q90 == _q51 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q51, 0) && types_match (_q90, _q51)))
				{
				  if ((_q91 == _q50 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q50, 0) && types_match (_q91, _q50)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GT_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GT_EXPR);
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
	    if (tree_expr_nonzero_p (_q40))
	      {
		switch (TREE_CODE (_q41))
		  {
		  case BIT_XOR_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_q41, 0);
		      tree _q61 = TREE_OPERAND (_q41, 1);
		      switch (TREE_CODE (_p1))
		        {
			case NE_EXPR:
			  {
			    tree _q90 = TREE_OPERAND (_p1, 0);
			    tree _q91 = TREE_OPERAND (_p1, 1);
			    if ((_q90 == _q60 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q60, 0) && types_match (_q90, _q60)))
			      {
				if ((_q91 == _q61 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q61, 0) && types_match (_q91, _q61)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q60, _q61, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GT_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q61, _q60, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GT_EXPR);
				      if (res) return res;
				    }
				  }
			      }
			    if ((_q90 == _q61 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q61, 0) && types_match (_q90, _q61)))
			      {
				if ((_q91 == _q60 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q60, 0) && types_match (_q91, _q60)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q60, _q61, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GT_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q61, _q60, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GT_EXPR);
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
		case NE_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
		      {
			if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, LT_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, LT_EXPR);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
		      {
			if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, LT_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, LT_EXPR);
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
		case NE_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
		      {
			if ((_q71 == _q41 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q41, 0) && types_match (_q71, _q41)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q41, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, GT_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q41, _q40, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, GT_EXPR);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q70 == _q41 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q41, 0) && types_match (_q70, _q41)))
		      {
			if ((_q71 == _q40 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q40, 0) && types_match (_q71, _q40)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q41, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, GT_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q41, _q40, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, GT_EXPR);
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
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q51, _p0 };
			tree res = generic_simplify_127 (loc, type, _p0, _p1, captures);
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
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q50, _p0 };
			tree res = generic_simplify_127 (loc, type, _p0, _p1, captures);
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
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q51 };
			tree res = generic_simplify_128 (loc, type, _p0, _p1, captures);
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
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q50 };
			tree res = generic_simplify_128 (loc, type, _p0, _p1, captures);
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
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q51, _p0 };
			tree res = generic_simplify_129 (loc, type, _p0, _p1, captures);
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
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q50, _p0 };
			tree res = generic_simplify_129 (loc, type, _p0, _p1, captures);
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
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q51 };
			tree res = generic_simplify_130 (loc, type, _p0, _p1, captures, NE_EXPR);
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
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q50 };
			tree res = generic_simplify_130 (loc, type, _p0, _p1, captures, NE_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
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
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q51, _p0, _q20, _q21 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
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
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q51, _p0, _q20, _q21 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, NE_EXPR, NE_EXPR);
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
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
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
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, GT_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, LT_EXPR, NE_EXPR);
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
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
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
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
		    if (res) return res;
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
	      switch (TREE_CODE (_q30))
	        {
		case BIT_XOR_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    if (tree_expr_nonzero_p (_q31))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case NE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_p1, 0);
			      tree _q91 = TREE_OPERAND (_p1, 1);
			      if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
				{
				  if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q40, _q41, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, NE_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q41, _q40, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, NE_EXPR);
					if (res) return res;
				      }
				    }
			        }
			      if ((_q90 == _q41 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q41, 0) && types_match (_q90, _q41)))
				{
				  if ((_q91 == _q40 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q40, 0) && types_match (_q91, _q40)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q40, _q41, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, NE_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q41, _q40, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, NE_EXPR);
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
	    if (tree_expr_nonzero_p (_q30))
	      {
		switch (TREE_CODE (_q31))
		  {
		  case BIT_XOR_EXPR:
		    {
		      tree _q50 = TREE_OPERAND (_q31, 0);
		      tree _q51 = TREE_OPERAND (_q31, 1);
		      switch (TREE_CODE (_p1))
		        {
			case NE_EXPR:
			  {
			    tree _q90 = TREE_OPERAND (_p1, 0);
			    tree _q91 = TREE_OPERAND (_p1, 1);
			    if ((_q90 == _q50 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q50, 0) && types_match (_q90, _q50)))
			      {
				if ((_q91 == _q51 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q51, 0) && types_match (_q91, _q51)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, NE_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, NE_EXPR);
				      if (res) return res;
				    }
				  }
			      }
			    if ((_q90 == _q51 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q51, 0) && types_match (_q90, _q51)))
			      {
				if ((_q91 == _q50 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q50, 0) && types_match (_q91, _q50)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, NE_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, NE_EXPR);
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
	      switch (TREE_CODE (_q40))
	        {
		case BIT_XOR_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    if (tree_expr_nonzero_p (_q41))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case NE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_p1, 0);
			      tree _q91 = TREE_OPERAND (_p1, 1);
			      if ((_q90 == _q50 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q50, 0) && types_match (_q90, _q50)))
				{
				  if ((_q91 == _q51 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q51, 0) && types_match (_q91, _q51)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, NE_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, NE_EXPR);
					if (res) return res;
				      }
				    }
			        }
			      if ((_q90 == _q51 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q51, 0) && types_match (_q90, _q51)))
				{
				  if ((_q91 == _q50 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q50, 0) && types_match (_q91, _q50)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, NE_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, NE_EXPR);
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
	    if (tree_expr_nonzero_p (_q40))
	      {
		switch (TREE_CODE (_q41))
		  {
		  case BIT_XOR_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_q41, 0);
		      tree _q61 = TREE_OPERAND (_q41, 1);
		      switch (TREE_CODE (_p1))
		        {
			case NE_EXPR:
			  {
			    tree _q90 = TREE_OPERAND (_p1, 0);
			    tree _q91 = TREE_OPERAND (_p1, 1);
			    if ((_q90 == _q60 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q60, 0) && types_match (_q90, _q60)))
			      {
				if ((_q91 == _q61 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q61, 0) && types_match (_q91, _q61)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q60, _q61, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, NE_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q61, _q60, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, NE_EXPR);
				      if (res) return res;
				    }
				  }
			      }
			    if ((_q90 == _q61 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q61, 0) && types_match (_q90, _q61)))
			      {
				if ((_q91 == _q60 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q60, 0) && types_match (_q91, _q60)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q60, _q61, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, NE_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q61, _q60, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, NE_EXPR);
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
		case NE_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
		      {
			if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, NE_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, NE_EXPR);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
		      {
			if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, NE_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, NE_EXPR);
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
		case NE_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
		      {
			if ((_q71 == _q41 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q41, 0) && types_match (_q71, _q41)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q41, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, NE_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q41, _q40, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, NE_EXPR);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q70 == _q41 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q41, 0) && types_match (_q70, _q41)))
		      {
			if ((_q71 == _q40 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q40, 0) && types_match (_q71, _q40)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q41, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, NE_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q41, _q40, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, NE_EXPR);
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
    case LE_EXPR:
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
		  if (tree_min_value (_q51))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			tree res = generic_simplify_133 (loc, type, _p0, _p1, captures);
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
			tree res = generic_simplify_134 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
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
		  if (tree_min_value (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			tree res = generic_simplify_135 (loc, type, _p0, _p1, captures);
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
			tree res = generic_simplify_134 (loc, type, _p0, _p1, captures, EQ_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
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
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
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
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
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
		    tree res = generic_simplify_124 (loc, type, _p0, _p1, captures, LT_EXPR, LE_EXPR);
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
		    tree res = generic_simplify_124 (loc, type, _p0, _p1, captures, LE_EXPR, LE_EXPR);
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
		    tree res = generic_simplify_124 (loc, type, _p0, _p1, captures, GT_EXPR, LE_EXPR);
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
		    tree res = generic_simplify_124 (loc, type, _p0, _p1, captures, GE_EXPR, LE_EXPR);
		    if (res) return res;
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
	      switch (TREE_CODE (_q30))
	        {
		case BIT_XOR_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    if (tree_expr_nonzero_p (_q31))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case NE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_p1, 0);
			      tree _q91 = TREE_OPERAND (_p1, 1);
			      if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
				{
				  if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q40, _q41, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LE_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q41, _q40, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LE_EXPR);
					if (res) return res;
				      }
				    }
			        }
			      if ((_q90 == _q41 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q41, 0) && types_match (_q90, _q41)))
				{
				  if ((_q91 == _q40 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q40, 0) && types_match (_q91, _q40)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q40, _q41, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LE_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q41, _q40, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LE_EXPR);
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
	    if (tree_expr_nonzero_p (_q30))
	      {
		switch (TREE_CODE (_q31))
		  {
		  case BIT_XOR_EXPR:
		    {
		      tree _q50 = TREE_OPERAND (_q31, 0);
		      tree _q51 = TREE_OPERAND (_q31, 1);
		      switch (TREE_CODE (_p1))
		        {
			case NE_EXPR:
			  {
			    tree _q90 = TREE_OPERAND (_p1, 0);
			    tree _q91 = TREE_OPERAND (_p1, 1);
			    if ((_q90 == _q50 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q50, 0) && types_match (_q90, _q50)))
			      {
				if ((_q91 == _q51 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q51, 0) && types_match (_q91, _q51)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LE_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LE_EXPR);
				      if (res) return res;
				    }
				  }
			      }
			    if ((_q90 == _q51 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q51, 0) && types_match (_q90, _q51)))
			      {
				if ((_q91 == _q50 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q50, 0) && types_match (_q91, _q50)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LE_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LE_EXPR);
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
	      switch (TREE_CODE (_q40))
	        {
		case BIT_XOR_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    if (tree_expr_nonzero_p (_q41))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case NE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_p1, 0);
			      tree _q91 = TREE_OPERAND (_p1, 1);
			      if ((_q90 == _q50 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q50, 0) && types_match (_q90, _q50)))
				{
				  if ((_q91 == _q51 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q51, 0) && types_match (_q91, _q51)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GE_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GE_EXPR);
					if (res) return res;
				      }
				    }
			        }
			      if ((_q90 == _q51 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q51, 0) && types_match (_q90, _q51)))
				{
				  if ((_q91 == _q50 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q50, 0) && types_match (_q91, _q50)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GE_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GE_EXPR);
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
	    if (tree_expr_nonzero_p (_q40))
	      {
		switch (TREE_CODE (_q41))
		  {
		  case BIT_XOR_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_q41, 0);
		      tree _q61 = TREE_OPERAND (_q41, 1);
		      switch (TREE_CODE (_p1))
		        {
			case NE_EXPR:
			  {
			    tree _q90 = TREE_OPERAND (_p1, 0);
			    tree _q91 = TREE_OPERAND (_p1, 1);
			    if ((_q90 == _q60 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q60, 0) && types_match (_q90, _q60)))
			      {
				if ((_q91 == _q61 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q61, 0) && types_match (_q91, _q61)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q60, _q61, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GE_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q61, _q60, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GE_EXPR);
				      if (res) return res;
				    }
				  }
			      }
			    if ((_q90 == _q61 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q61, 0) && types_match (_q90, _q61)))
			      {
				if ((_q91 == _q60 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q60, 0) && types_match (_q91, _q60)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q60, _q61, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GE_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q61, _q60, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GE_EXPR);
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
		case NE_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
		      {
			if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, LE_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, LE_EXPR);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
		      {
			if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, LE_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, LE_EXPR);
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
		case NE_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
		      {
			if ((_q71 == _q41 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q41, 0) && types_match (_q71, _q41)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q41, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, GE_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q41, _q40, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, GE_EXPR);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q70 == _q41 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q41, 0) && types_match (_q70, _q41)))
		      {
			if ((_q71 == _q40 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q40, 0) && types_match (_q71, _q40)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q41, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, GE_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q41, _q40, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, GE_EXPR);
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
    case GE_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	switch (TREE_CODE (_p1))
	  {
	  case NE_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  if (tree_min_value (_q51))
		    {
		      {
			tree captures[2] ATTRIBUTE_UNUSED = { _q21, _q20 };
			tree res = generic_simplify_133 (loc, type, _p0, _p1, captures);
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
			tree res = generic_simplify_134 (loc, type, _p0, _p1, captures, NE_EXPR);
			if (res) return res;
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
		  if (tree_min_value (_q51))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q21, _q20 };
			tree res = generic_simplify_135 (loc, type, _p0, _p1, captures);
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
			tree res = generic_simplify_134 (loc, type, _p0, _p1, captures, EQ_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
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
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
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
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, GE_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, LE_EXPR, NE_EXPR);
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
		    tree res = generic_simplify_124 (loc, type, _p0, _p1, captures, LT_EXPR, GE_EXPR);
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
		    tree res = generic_simplify_124 (loc, type, _p0, _p1, captures, LE_EXPR, GE_EXPR);
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
		    tree res = generic_simplify_124 (loc, type, _p0, _p1, captures, GT_EXPR, GE_EXPR);
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
		    tree res = generic_simplify_124 (loc, type, _p0, _p1, captures, GE_EXPR, GE_EXPR);
		    if (res) return res;
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
	      switch (TREE_CODE (_q30))
	        {
		case BIT_XOR_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    if (tree_expr_nonzero_p (_q31))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case NE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_p1, 0);
			      tree _q91 = TREE_OPERAND (_p1, 1);
			      if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
				{
				  if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q40, _q41, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GE_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q41, _q40, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GE_EXPR);
					if (res) return res;
				      }
				    }
			        }
			      if ((_q90 == _q41 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q41, 0) && types_match (_q90, _q41)))
				{
				  if ((_q91 == _q40 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q40, 0) && types_match (_q91, _q40)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q40, _q41, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GE_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q41, _q40, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GE_EXPR);
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
	    if (tree_expr_nonzero_p (_q30))
	      {
		switch (TREE_CODE (_q31))
		  {
		  case BIT_XOR_EXPR:
		    {
		      tree _q50 = TREE_OPERAND (_q31, 0);
		      tree _q51 = TREE_OPERAND (_q31, 1);
		      switch (TREE_CODE (_p1))
		        {
			case NE_EXPR:
			  {
			    tree _q90 = TREE_OPERAND (_p1, 0);
			    tree _q91 = TREE_OPERAND (_p1, 1);
			    if ((_q90 == _q50 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q50, 0) && types_match (_q90, _q50)))
			      {
				if ((_q91 == _q51 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q51, 0) && types_match (_q91, _q51)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GE_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GE_EXPR);
				      if (res) return res;
				    }
				  }
			      }
			    if ((_q90 == _q51 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q51, 0) && types_match (_q90, _q51)))
			      {
				if ((_q91 == _q50 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q50, 0) && types_match (_q91, _q50)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GE_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, GE_EXPR);
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
	      switch (TREE_CODE (_q40))
	        {
		case BIT_XOR_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    if (tree_expr_nonzero_p (_q41))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case NE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_p1, 0);
			      tree _q91 = TREE_OPERAND (_p1, 1);
			      if ((_q90 == _q50 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q50, 0) && types_match (_q90, _q50)))
				{
				  if ((_q91 == _q51 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q51, 0) && types_match (_q91, _q51)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LE_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LE_EXPR);
					if (res) return res;
				      }
				    }
			        }
			      if ((_q90 == _q51 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q51, 0) && types_match (_q90, _q51)))
				{
				  if ((_q91 == _q50 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q50, 0) && types_match (_q91, _q50)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LE_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LE_EXPR);
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
	    if (tree_expr_nonzero_p (_q40))
	      {
		switch (TREE_CODE (_q41))
		  {
		  case BIT_XOR_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_q41, 0);
		      tree _q61 = TREE_OPERAND (_q41, 1);
		      switch (TREE_CODE (_p1))
		        {
			case NE_EXPR:
			  {
			    tree _q90 = TREE_OPERAND (_p1, 0);
			    tree _q91 = TREE_OPERAND (_p1, 1);
			    if ((_q90 == _q60 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q60, 0) && types_match (_q90, _q60)))
			      {
				if ((_q91 == _q61 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q61, 0) && types_match (_q91, _q61)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q60, _q61, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LE_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q61, _q60, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LE_EXPR);
				      if (res) return res;
				    }
				  }
			      }
			    if ((_q90 == _q61 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q61, 0) && types_match (_q90, _q61)))
			      {
				if ((_q91 == _q60 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q60, 0) && types_match (_q91, _q60)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q60, _q61, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LE_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q61, _q60, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, LE_EXPR);
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
		case NE_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
		      {
			if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, GE_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, GE_EXPR);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
		      {
			if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, GE_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, GE_EXPR);
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
		case NE_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
		      {
			if ((_q71 == _q41 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q41, 0) && types_match (_q71, _q41)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q41, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, LE_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q41, _q40, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, LE_EXPR);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q70 == _q41 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q41, 0) && types_match (_q70, _q41)))
		      {
			if ((_q71 == _q40 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q40, 0) && types_match (_q71, _q40)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q41, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, LE_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q41, _q40, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, LE_EXPR);
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
    case EQ_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if (tree_min_value (_q21))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case LE_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q51 };
			tree res = generic_simplify_135 (loc, type, _p0, _p1, captures);
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
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q50 };
			tree res = generic_simplify_135 (loc, type, _p0, _p1, captures);
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
			tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q50 };
			tree res = generic_simplify_136 (loc, type, _p0, _p1, captures);
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
			tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q51 };
			tree res = generic_simplify_136 (loc, type, _p0, _p1, captures);
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
	      case GE_EXPR:
	        {
		  tree _q50 = TREE_OPERAND (_p1, 0);
		  tree _q51 = TREE_OPERAND (_p1, 1);
		  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q51 };
			tree res = generic_simplify_130 (loc, type, _p0, _p1, captures, EQ_EXPR);
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
			tree captures[3] ATTRIBUTE_UNUSED = { _p1, _q20, _q50 };
			tree res = generic_simplify_130 (loc, type, _p0, _p1, captures, EQ_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q60 == _q21 && ! TREE_SIDE_EFFECTS (_q60)) || (operand_equal_p (_q60, _q21, 0) && types_match (_q60, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q70 == _q21 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q21, 0) && types_match (_q70, _q21)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
			  tree res = generic_simplify_131 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q30 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q30, 0) && types_match (_q61, _q30)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q30, _q60, _p0, _q20, _q21 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
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
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
			  if (res) return res;
			}
		      }
		    if ((_q61 == _q40 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q40, 0) && types_match (_q61, _q40)))
		      {
			{
			  tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q40, _q60, _p0, _q21, _q20 };
			  tree res = generic_simplify_122 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
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
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q51, _p0, _q20, _q21 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, EQ_EXPR, EQ_EXPR);
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
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q51, _p0, _q20, _q21 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q51, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, NE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q21, _q50, _p0, _q21, _q20 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p1, _q20, _q50, _p0, _q20, _q21 };
		    tree res = generic_simplify_123 (loc, type, _p0, _p1, captures, EQ_EXPR, NE_EXPR);
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
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
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
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, GT_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, LT_EXPR, EQ_EXPR);
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
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
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
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q20, _q21, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50, _q51 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, GE_EXPR, EQ_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[6] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51, _q50 };
		    tree res = generic_simplify_132 (loc, type, _p0, _p1, captures, LE_EXPR, EQ_EXPR);
		    if (res) return res;
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
	      switch (TREE_CODE (_q30))
	        {
		case BIT_XOR_EXPR:
		  {
		    tree _q40 = TREE_OPERAND (_q30, 0);
		    tree _q41 = TREE_OPERAND (_q30, 1);
		    if (tree_expr_nonzero_p (_q31))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case NE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_p1, 0);
			      tree _q91 = TREE_OPERAND (_p1, 1);
			      if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
				{
				  if ((_q91 == _q41 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q41, 0) && types_match (_q91, _q41)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q40, _q41, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, EQ_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q41, _q40, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, EQ_EXPR);
					if (res) return res;
				      }
				    }
			        }
			      if ((_q90 == _q41 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q41, 0) && types_match (_q90, _q41)))
				{
				  if ((_q91 == _q40 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q40, 0) && types_match (_q91, _q40)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q40, _q41, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, EQ_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q41, _q40, _q31, _q21, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, EQ_EXPR);
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
	    if (tree_expr_nonzero_p (_q30))
	      {
		switch (TREE_CODE (_q31))
		  {
		  case BIT_XOR_EXPR:
		    {
		      tree _q50 = TREE_OPERAND (_q31, 0);
		      tree _q51 = TREE_OPERAND (_q31, 1);
		      switch (TREE_CODE (_p1))
		        {
			case NE_EXPR:
			  {
			    tree _q90 = TREE_OPERAND (_p1, 0);
			    tree _q91 = TREE_OPERAND (_p1, 1);
			    if ((_q90 == _q50 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q50, 0) && types_match (_q90, _q50)))
			      {
				if ((_q91 == _q51 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q51, 0) && types_match (_q91, _q51)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, EQ_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, EQ_EXPR);
				      if (res) return res;
				    }
				  }
			      }
			    if ((_q90 == _q51 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q51, 0) && types_match (_q90, _q51)))
			      {
				if ((_q91 == _q50 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q50, 0) && types_match (_q91, _q50)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, EQ_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q30, _q21, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, EQ_EXPR);
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
	      switch (TREE_CODE (_q40))
	        {
		case BIT_XOR_EXPR:
		  {
		    tree _q50 = TREE_OPERAND (_q40, 0);
		    tree _q51 = TREE_OPERAND (_q40, 1);
		    if (tree_expr_nonzero_p (_q41))
		      {
			switch (TREE_CODE (_p1))
			  {
			  case NE_EXPR:
			    {
			      tree _q90 = TREE_OPERAND (_p1, 0);
			      tree _q91 = TREE_OPERAND (_p1, 1);
			      if ((_q90 == _q50 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q50, 0) && types_match (_q90, _q50)))
				{
				  if ((_q91 == _q51 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q51, 0) && types_match (_q91, _q51)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, EQ_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, EQ_EXPR);
					if (res) return res;
				      }
				    }
			        }
			      if ((_q90 == _q51 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q51, 0) && types_match (_q90, _q51)))
				{
				  if ((_q91 == _q50 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q50, 0) && types_match (_q91, _q50)))
				    {
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q50, _q51, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, EQ_EXPR);
					if (res) return res;
				      }
				      {
					tree captures[5] ATTRIBUTE_UNUSED = { _q51, _q50, _q41, _q20, _p1 };
					tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, EQ_EXPR);
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
	    if (tree_expr_nonzero_p (_q40))
	      {
		switch (TREE_CODE (_q41))
		  {
		  case BIT_XOR_EXPR:
		    {
		      tree _q60 = TREE_OPERAND (_q41, 0);
		      tree _q61 = TREE_OPERAND (_q41, 1);
		      switch (TREE_CODE (_p1))
		        {
			case NE_EXPR:
			  {
			    tree _q90 = TREE_OPERAND (_p1, 0);
			    tree _q91 = TREE_OPERAND (_p1, 1);
			    if ((_q90 == _q60 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q60, 0) && types_match (_q90, _q60)))
			      {
				if ((_q91 == _q61 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q61, 0) && types_match (_q91, _q61)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q60, _q61, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, EQ_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q61, _q60, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, EQ_EXPR);
				      if (res) return res;
				    }
				  }
			      }
			    if ((_q90 == _q61 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q61, 0) && types_match (_q90, _q61)))
			      {
				if ((_q91 == _q60 && ! TREE_SIDE_EFFECTS (_q91)) || (operand_equal_p (_q91, _q60, 0) && types_match (_q91, _q60)))
				  {
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q60, _q61, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, EQ_EXPR);
				      if (res) return res;
				    }
				    {
				      tree captures[5] ATTRIBUTE_UNUSED = { _q61, _q60, _q40, _q20, _p1 };
				      tree res = generic_simplify_125 (loc, type, _p0, _p1, captures, EQ_EXPR);
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
		case NE_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
		      {
			if ((_q71 == _q31 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q31, 0) && types_match (_q71, _q31)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, EQ_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, EQ_EXPR);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q70 == _q31 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q31, 0) && types_match (_q70, _q31)))
		      {
			if ((_q71 == _q30 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q30, 0) && types_match (_q71, _q30)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q30, _q31, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, EQ_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q31, _q30, _q21, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, EQ_EXPR);
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
		case NE_EXPR:
		  {
		    tree _q70 = TREE_OPERAND (_p1, 0);
		    tree _q71 = TREE_OPERAND (_p1, 1);
		    if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
		      {
			if ((_q71 == _q41 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q41, 0) && types_match (_q71, _q41)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q41, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, EQ_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q41, _q40, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, EQ_EXPR);
			      if (res) return res;
			    }
			  }
		      }
		    if ((_q70 == _q41 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q41, 0) && types_match (_q70, _q41)))
		      {
			if ((_q71 == _q40 && ! TREE_SIDE_EFFECTS (_q71)) || (operand_equal_p (_q71, _q40, 0) && types_match (_q71, _q40)))
			  {
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q40, _q41, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, EQ_EXPR);
			      if (res) return res;
			    }
			    {
			      tree captures[4] ATTRIBUTE_UNUSED = { _q41, _q40, _q20, _p1 };
			      tree res = generic_simplify_126 (loc, type, _p0, _p1, captures, EQ_EXPR);
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
			    tree res = generic_simplify_137 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
				tree res = generic_simplify_138 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
		    tree res = generic_simplify_139 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
				tree res = generic_simplify_138 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
					  tree res = generic_simplify_111 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
		tree res = generic_simplify_114 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		if (res) return res;
	      }
	    }
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
		tree res = generic_simplify_114 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
		if (res) return res;
	      }
	    }
        }
    }
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
		    tree res = generic_simplify_139 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
			      tree res = generic_simplify_112 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
				tree res = generic_simplify_140 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
		    tree res = generic_simplify_141 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
		    tree res = generic_simplify_141 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR);
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
 && (TREE_CODE_CLASS (BIT_IOR_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
		      {
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1304;
			{
			  tree res_op0;
			  res_op0 = captures[1];
			  tree res_op1;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[5];
			    _r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1304;
			    res_op1 = _r1;
			  }
			  tree res_op2;
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[6];
			    _r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    if (EXPR_P (_r1))
			      goto next_after_fail1304;
			    res_op2 = _r1;
			  }
			  tree _r;
			  _r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
			  if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 868, __FILE__, __LINE__, true);
			  return _r;
			}
next_after_fail1304:;
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
 && (TREE_CODE_CLASS (BIT_IOR_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[2]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[1]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[2]),
 TREE_TYPE (captures[1]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1305;
	      {
		if (! tree_invariant_p (captures[4])) goto next_after_fail1305;
		tree res_op0;
		res_op0 = captures[1];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[2];
		  _o1[1] = unshare_expr (captures[4]);
		  _r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1305;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[3];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1305;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 869, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1305:;
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
 && (TREE_CODE_CLASS (BIT_IOR_EXPR) != tcc_comparison
 || types_match (type, TREE_TYPE (captures[3]))
 || expand_vec_cond_expr_p (type, TREE_TYPE (captures[2]), ERROR_MARK)
 || (optimize_vectors_before_lowering_p ()
 && !expand_vec_cond_expr_p (TREE_TYPE (captures[3]),
 TREE_TYPE (captures[2]), ERROR_MARK)))
)
	    {
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1306;
	      {
		if (! tree_invariant_p (captures[0])) goto next_after_fail1306;
		tree res_op0;
		res_op0 = captures[2];
		tree res_op1;
		{
		  tree _o1[2], _r1;
		  _o1[0] = unshare_expr (captures[0]);
		  _o1[1] = captures[3];
		  _r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1306;
		  res_op1 = _r1;
		}
		tree res_op2;
		{
		  tree _o1[2], _r1;
		  _o1[0] = captures[0];
		  _o1[1] = captures[4];
		  _r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
		  if (EXPR_P (_r1))
		    goto next_after_fail1306;
		  res_op2 = _r1;
		}
		tree _r;
		_r = fold_build3_loc (loc, VEC_COND_EXPR, type, res_op0, res_op1, res_op2);
		if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 870, __FILE__, __LINE__, true);
		return _r;
	      }
next_after_fail1306:;
	    }
	}
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case UNORDERED_EXPR:
      {
	tree _q20 = TREE_OPERAND (_p0, 0);
	tree _q21 = TREE_OPERAND (_p0, 1);
	if ((_q21 == _q20 && ! TREE_SIDE_EFFECTS (_q21)) || (operand_equal_p (_q21, _q20, 0) && types_match (_q21, _q20)))
	  {
	    switch (TREE_CODE (_p1))
	      {
	      case UNORDERED_EXPR:
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
			    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1307;
			    {
			      tree res_op0;
			      res_op0 = captures[0];
			      tree res_op1;
			      res_op1 = captures[1];
			      tree _r;
			      _r = fold_build2_loc (loc, UNORDERED_EXPR, type, res_op0, res_op1);
			      if (UNLIKELY (debug_dump)) generic_dump_logs ("match.pd", 909, __FILE__, __LINE__, true);
			      return _r;
			    }
next_after_fail1307:;
			  }
		      }
		    }
		  if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _q51 };
			tree res = generic_simplify_142 (loc, type, _p0, _p1, captures);
			if (res) return res;
		      }
		    }
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p1, _q50 };
			tree res = generic_simplify_142 (loc, type, _p0, _p1, captures);
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
	  case UNORDERED_EXPR:
	    {
	      tree _q50 = TREE_OPERAND (_p1, 0);
	      tree _q51 = TREE_OPERAND (_p1, 1);
	      if ((_q50 == _q20 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q20, 0) && types_match (_q50, _q20)))
		{
		  if ((_q51 == _q20 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q20, 0) && types_match (_q51, _q20)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _q20, _p0, _q21 };
			tree res = generic_simplify_143 (loc, type, _p0, _p1, captures);
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
			tree res = generic_simplify_143 (loc, type, _p0, _p1, captures);
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
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, LT_EXPR, MAX_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, GT_EXPR, MIN_EXPR);
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
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, LT_EXPR, MAX_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, GT_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
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
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1308;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1308;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1308;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1308;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1308;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1308;
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
next_after_fail1308:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1309;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1309;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1309;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1309;
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
next_after_fail1309:;
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
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1310;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1310;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1310;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1310;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1310;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1310;
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
next_after_fail1310:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1311;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1311;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1311;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1311;
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
next_after_fail1311:;
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
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, LE_EXPR, MAX_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, GE_EXPR, MIN_EXPR);
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
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, LE_EXPR, MAX_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, GE_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
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
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1312;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1312;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1312;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1312;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1312;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1312;
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
next_after_fail1312:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1313;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1313;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1313;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1313;
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
next_after_fail1313:;
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
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1314;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1314;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1314;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1314;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1314;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1314;
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
next_after_fail1314:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1315;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1315;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1315;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1315;
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
next_after_fail1315:;
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
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, GT_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, LT_EXPR, MAX_EXPR);
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
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, GT_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, LT_EXPR, MAX_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
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
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1316;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1316;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1316;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1316;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1316;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1316;
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
next_after_fail1316:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1317;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1317;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1317;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1317;
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
next_after_fail1317:;
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
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1318;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1318;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1318;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1318;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1318;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1318;
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
next_after_fail1318:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1319;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1319;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1319;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1319;
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
next_after_fail1319:;
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
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, GE_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q51 == _q21 && ! TREE_SIDE_EFFECTS (_q51)) || (operand_equal_p (_q51, _q21, 0) && types_match (_q51, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q50 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, LE_EXPR, MAX_EXPR);
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
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, GE_EXPR, MIN_EXPR);
		    if (res) return res;
		  }
	        }
	      if ((_q50 == _q21 && ! TREE_SIDE_EFFECTS (_q50)) || (operand_equal_p (_q50, _q21, 0) && types_match (_q50, _q21)))
		{
		  {
		    tree captures[5] ATTRIBUTE_UNUSED = { _p0, _q21, _q20, _p1, _q51 };
		    tree res = generic_simplify_144 (loc, type, _p0, _p1, captures, BIT_IOR_EXPR, LE_EXPR, MAX_EXPR);
		    if (res) return res;
		  }
	        }
	      break;
	    }
          default:;
          }
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
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1320;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1320;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1320;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1320;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1320;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1320;
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
next_after_fail1320:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1321;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1321;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1321;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1321;
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
next_after_fail1321:;
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
								  if (TREE_SIDE_EFFECTS (captures[1])) goto next_after_fail1322;
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1322;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1322;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1322;
								  if (TREE_SIDE_EFFECTS (captures[6])) goto next_after_fail1322;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1322;
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
next_after_fail1322:;
								}
							      else
								{
								  if (TREE_SIDE_EFFECTS (captures[2])) goto next_after_fail1323;
								  if (TREE_SIDE_EFFECTS (captures[3])) goto next_after_fail1323;
								  if (TREE_SIDE_EFFECTS (captures[4])) goto next_after_fail1323;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1323;
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
next_after_fail1323:;
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
				if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail1324;
				{
				  tree res_op0;
				  {
				    tree _o1[2], _r1;
				    _o1[0] = captures[0];
				    _o1[1] = captures[2];
				    _r1 = fold_build2_loc (loc, BIT_IOR_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
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
next_after_fail1324:;
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
#pragma GCC diagnostic pop
