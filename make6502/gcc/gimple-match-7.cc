/* Generated automatically by the program `genmatch' from
   a IL pattern matching and simplification description.  */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-function"

#include "gimple-match-auto.h"

bool
gimple_maybe_cmp (tree t, tree *res_ops, tree (*valueize)(tree) ATTRIBUTE_UNUSED)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (t))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, t))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
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
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { t, _p0, _p1 };
		    {
		      res_ops[0] = captures[0];
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 5, __FILE__, __LINE__, false);
		      return true;
		    }
		  }
	          break;
	        }
	      CASE_CONVERT:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  switch (TREE_CODE (_p0))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p0))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
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
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
				  {
				    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
				    if (tree_nop_conversion_p (type, TREE_TYPE (captures[0]))
)
				      {
					{
					  res_ops[0] = captures[0];
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 6, __FILE__, __LINE__, false);
					  return true;
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
	        }
	      case BIT_XOR_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  if (tree_swap_operands_p (_p0, _p1))
		    std::swap (_p0, _p1);
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { t, _p0, _p1 };
		    if (INTEGRAL_TYPE_P (type)
 && TYPE_PRECISION (type) == 1
)
		      {
			{
			  res_ops[0] = captures[0];
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 7, __FILE__, __LINE__, false);
			  return true;
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
  return false;
}

bool
gimple_signed_integer_sat_sub (tree t, tree *res_ops, tree (*valueize)(tree) ATTRIBUTE_UNUSED)
{
  const tree type = TREE_TYPE (t);
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (t))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, t))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case COND_EXPR:
	        {
		  tree _p0 = gimple_assign_rhs1 (_a1);
		  _p0 = do_valueize (valueize, _p0);
		  tree _p1 = gimple_assign_rhs2 (_a1);
		  _p1 = do_valueize (valueize, _p1);
		  tree _p2 = gimple_assign_rhs3 (_a1);
		  _p2 = do_valueize (valueize, _p2);
		  switch (TREE_CODE (_p0))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p0))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case LT_EXPR:
			        {
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
				  switch (TREE_CODE (_q20))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q20))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case BIT_AND_EXPR:
					        {
						  tree _q30 = gimple_assign_rhs1 (_a3);
						  _q30 = do_valueize (valueize, _q30);
						  tree _q31 = gimple_assign_rhs2 (_a3);
						  _q31 = do_valueize (valueize, _q31);
						  if (tree_swap_operands_p (_q30, _q31))
						    std::swap (_q30, _q31);
						  switch (TREE_CODE (_q30))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _q30))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      case BIT_XOR_EXPR:
							        {
								  tree _q40 = gimple_assign_rhs1 (_a4);
								  _q40 = do_valueize (valueize, _q40);
								  tree _q41 = gimple_assign_rhs2 (_a4);
								  _q41 = do_valueize (valueize, _q41);
								  if (tree_swap_operands_p (_q40, _q41))
								    std::swap (_q40, _q41);
								  switch (TREE_CODE (_q31))
								    {
								    case SSA_NAME:
								      if (gimple *_d5 = get_def (valueize, _q31))
								        {
									  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
									    switch (gimple_assign_rhs_code (_a5))
									      {
									      case BIT_XOR_EXPR:
									        {
										  tree _q70 = gimple_assign_rhs1 (_a5);
										  _q70 = do_valueize (valueize, _q70);
										  tree _q71 = gimple_assign_rhs2 (_a5);
										  _q71 = do_valueize (valueize, _q71);
										  if (tree_swap_operands_p (_q70, _q71))
										    std::swap (_q70, _q71);
										  if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
										    {
										      {
											tree _q71_pops[1];
											if (gimple_nop_convert (_q71, _q71_pops, valueize))
											  {
											    tree _q90 = _q71_pops[0];
											    switch (TREE_CODE (_q90))
											      {
											      case SSA_NAME:
											        if (gimple *_d6 = get_def (valueize, _q90))
											          {
												    if (gassign *_a6 = dyn_cast <gassign *> (_d6))
												      switch (gimple_assign_rhs_code (_a6))
													{
													case MINUS_EXPR:
													  {
													    tree _q100 = gimple_assign_rhs1 (_a6);
													    _q100 = do_valueize (valueize, _q100);
													    tree _q101 = gimple_assign_rhs2 (_a6);
													    _q101 = do_valueize (valueize, _q101);
													    {
													      tree _q100_pops[1];
													      if (gimple_nop_convert (_q100, _q100_pops, valueize))
													        {
														  tree _q110 = _q100_pops[0];
														  if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
														    {
														      {
															tree _q101_pops[1];
															if (gimple_nop_convert (_q101, _q101_pops, valueize))
															  {
															    tree _q130 = _q101_pops[0];
															    if ((_q130 == _q41 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q41, 0) && types_match (_q130, _q41)))
															      {
																if (integer_zerop (_q21))
																  {
																    switch (TREE_CODE (_p1))
																      {
																      case SSA_NAME:
																        if (gimple *_d7 = get_def (valueize, _p1))
																          {
																	    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																	      switch (gimple_assign_rhs_code (_a7))
																		{
																		case BIT_XOR_EXPR:
																		  {
																		    tree _q160 = gimple_assign_rhs1 (_a7);
																		    _q160 = do_valueize (valueize, _q160);
																		    tree _q161 = gimple_assign_rhs2 (_a7);
																		    _q161 = do_valueize (valueize, _q161);
																		    if (tree_swap_operands_p (_q160, _q161))
																		      std::swap (_q160, _q161);
																		    switch (TREE_CODE (_q160))
																		      {
																		      case SSA_NAME:
																		        if (gimple *_d8 = get_def (valueize, _q160))
																		          {
																			    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																			      switch (gimple_assign_rhs_code (_a8))
																				{
																				case NEGATE_EXPR:
																				  {
																				    tree _q170 = gimple_assign_rhs1 (_a8);
																				    _q170 = do_valueize (valueize, _q170);
																				    switch (TREE_CODE (_q170))
																				      {
																				      case SSA_NAME:
																				        if (gimple *_d9 = get_def (valueize, _q170))
																				          {
																					    if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																					      switch (gimple_assign_rhs_code (_a9))
																						{
																						CASE_CONVERT:
																						  {
																						    tree _q180 = gimple_assign_rhs1 (_a9);
																						    _q180 = do_valueize (valueize, _q180);
																						    switch (TREE_CODE (_q180))
																						      {
																						      case SSA_NAME:
																						        if (gimple *_d10 = get_def (valueize, _q180))
																						          {
																							    if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																							      switch (gimple_assign_rhs_code (_a10))
																								{
																								case LT_EXPR:
																								  {
																								    tree _q190 = gimple_assign_rhs1 (_a10);
																								    _q190 = do_valueize (valueize, _q190);
																								    tree _q191 = gimple_assign_rhs2 (_a10);
																								    _q191 = do_valueize (valueize, _q191);
																								    if ((_q190 == _q40 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q40, 0) && types_match (_q190, _q40)))
																								      {
																									if (integer_zerop (_q191))
																									  {
																									    if (gimple_max_value (_q161, valueize))
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
																											    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																		  if (gimple_max_value (_q160, valueize))
																		    {
																		      switch (TREE_CODE (_q161))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d8 = get_def (valueize, _q161))
																			    {
																			      if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																			        switch (gimple_assign_rhs_code (_a8))
																				  {
																				  case NEGATE_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a8);
																				      _q180 = do_valueize (valueize, _q180);
																				      switch (TREE_CODE (_q180))
																				        {
																					case SSA_NAME:
																					  if (gimple *_d9 = get_def (valueize, _q180))
																					    {
																					      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																					        switch (gimple_assign_rhs_code (_a9))
																						  {
																						  CASE_CONVERT:
																						    {
																						      tree _q190 = gimple_assign_rhs1 (_a9);
																						      _q190 = do_valueize (valueize, _q190);
																						      switch (TREE_CODE (_q190))
																						        {
																							case SSA_NAME:
																							  if (gimple *_d10 = get_def (valueize, _q190))
																							    {
																							      if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																							        switch (gimple_assign_rhs_code (_a10))
																								  {
																								  case LT_EXPR:
																								    {
																								      tree _q200 = gimple_assign_rhs1 (_a10);
																								      _q200 = do_valueize (valueize, _q200);
																								      tree _q201 = gimple_assign_rhs2 (_a10);
																								      _q201 = do_valueize (valueize, _q201);
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																							  break;
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
																				    }
																				  default:;
																				  }
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
											      default:;
											      }
											  }
										      }
										    }
										  if ((_q70 == _q41 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q41, 0) && types_match (_q70, _q41)))
										    {
										      {
											tree _q71_pops[1];
											if (gimple_nop_convert (_q71, _q71_pops, valueize))
											  {
											    tree _q90 = _q71_pops[0];
											    switch (TREE_CODE (_q90))
											      {
											      case SSA_NAME:
											        if (gimple *_d6 = get_def (valueize, _q90))
											          {
												    if (gassign *_a6 = dyn_cast <gassign *> (_d6))
												      switch (gimple_assign_rhs_code (_a6))
													{
													case MINUS_EXPR:
													  {
													    tree _q100 = gimple_assign_rhs1 (_a6);
													    _q100 = do_valueize (valueize, _q100);
													    tree _q101 = gimple_assign_rhs2 (_a6);
													    _q101 = do_valueize (valueize, _q101);
													    {
													      tree _q100_pops[1];
													      if (gimple_nop_convert (_q100, _q100_pops, valueize))
													        {
														  tree _q110 = _q100_pops[0];
														  if ((_q110 == _q41 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q41, 0) && types_match (_q110, _q41)))
														    {
														      {
															tree _q101_pops[1];
															if (gimple_nop_convert (_q101, _q101_pops, valueize))
															  {
															    tree _q130 = _q101_pops[0];
															    if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
															      {
																if (integer_zerop (_q21))
																  {
																    switch (TREE_CODE (_p1))
																      {
																      case SSA_NAME:
																        if (gimple *_d7 = get_def (valueize, _p1))
																          {
																	    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																	      switch (gimple_assign_rhs_code (_a7))
																		{
																		case BIT_XOR_EXPR:
																		  {
																		    tree _q160 = gimple_assign_rhs1 (_a7);
																		    _q160 = do_valueize (valueize, _q160);
																		    tree _q161 = gimple_assign_rhs2 (_a7);
																		    _q161 = do_valueize (valueize, _q161);
																		    if (tree_swap_operands_p (_q160, _q161))
																		      std::swap (_q160, _q161);
																		    switch (TREE_CODE (_q160))
																		      {
																		      case SSA_NAME:
																		        if (gimple *_d8 = get_def (valueize, _q160))
																		          {
																			    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																			      switch (gimple_assign_rhs_code (_a8))
																				{
																				case NEGATE_EXPR:
																				  {
																				    tree _q170 = gimple_assign_rhs1 (_a8);
																				    _q170 = do_valueize (valueize, _q170);
																				    switch (TREE_CODE (_q170))
																				      {
																				      case SSA_NAME:
																				        if (gimple *_d9 = get_def (valueize, _q170))
																				          {
																					    if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																					      switch (gimple_assign_rhs_code (_a9))
																						{
																						CASE_CONVERT:
																						  {
																						    tree _q180 = gimple_assign_rhs1 (_a9);
																						    _q180 = do_valueize (valueize, _q180);
																						    switch (TREE_CODE (_q180))
																						      {
																						      case SSA_NAME:
																						        if (gimple *_d10 = get_def (valueize, _q180))
																						          {
																							    if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																							      switch (gimple_assign_rhs_code (_a10))
																								{
																								case LT_EXPR:
																								  {
																								    tree _q190 = gimple_assign_rhs1 (_a10);
																								    _q190 = do_valueize (valueize, _q190);
																								    tree _q191 = gimple_assign_rhs2 (_a10);
																								    _q191 = do_valueize (valueize, _q191);
																								    if ((_q190 == _q41 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q41, 0) && types_match (_q190, _q41)))
																								      {
																									if (integer_zerop (_q191))
																									  {
																									    if (gimple_max_value (_q161, valueize))
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
																											    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																		  if (gimple_max_value (_q160, valueize))
																		    {
																		      switch (TREE_CODE (_q161))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d8 = get_def (valueize, _q161))
																			    {
																			      if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																			        switch (gimple_assign_rhs_code (_a8))
																				  {
																				  case NEGATE_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a8);
																				      _q180 = do_valueize (valueize, _q180);
																				      switch (TREE_CODE (_q180))
																				        {
																					case SSA_NAME:
																					  if (gimple *_d9 = get_def (valueize, _q180))
																					    {
																					      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																					        switch (gimple_assign_rhs_code (_a9))
																						  {
																						  CASE_CONVERT:
																						    {
																						      tree _q190 = gimple_assign_rhs1 (_a9);
																						      _q190 = do_valueize (valueize, _q190);
																						      switch (TREE_CODE (_q190))
																						        {
																							case SSA_NAME:
																							  if (gimple *_d10 = get_def (valueize, _q190))
																							    {
																							      if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																							        switch (gimple_assign_rhs_code (_a10))
																								  {
																								  case LT_EXPR:
																								    {
																								      tree _q200 = gimple_assign_rhs1 (_a10);
																								      _q200 = do_valueize (valueize, _q200);
																								      tree _q201 = gimple_assign_rhs2 (_a10);
																								      _q201 = do_valueize (valueize, _q201);
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																							  break;
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
																				    }
																				  default:;
																				  }
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
								      break;
								    default:;
								    }
								  {
								    tree _q41_pops[1];
								    if (gimple_nop_convert (_q41, _q41_pops, valueize))
								      {
									tree _q60 = _q41_pops[0];
									switch (TREE_CODE (_q60))
									  {
									  case SSA_NAME:
									    if (gimple *_d5 = get_def (valueize, _q60))
									      {
										if (gassign *_a5 = dyn_cast <gassign *> (_d5))
										  switch (gimple_assign_rhs_code (_a5))
										    {
										    case MINUS_EXPR:
										      {
											tree _q70 = gimple_assign_rhs1 (_a5);
											_q70 = do_valueize (valueize, _q70);
											tree _q71 = gimple_assign_rhs2 (_a5);
											_q71 = do_valueize (valueize, _q71);
											{
											  tree _q70_pops[1];
											  if (gimple_nop_convert (_q70, _q70_pops, valueize))
											    {
											      tree _q80 = _q70_pops[0];
											      if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
												{
												  {
												    tree _q71_pops[1];
												    if (gimple_nop_convert (_q71, _q71_pops, valueize))
												      {
													tree _q100 = _q71_pops[0];
													switch (TREE_CODE (_q31))
													  {
													  case SSA_NAME:
													    if (gimple *_d6 = get_def (valueize, _q31))
													      {
														if (gassign *_a6 = dyn_cast <gassign *> (_d6))
														  switch (gimple_assign_rhs_code (_a6))
														    {
														    case BIT_XOR_EXPR:
														      {
															tree _q120 = gimple_assign_rhs1 (_a6);
															_q120 = do_valueize (valueize, _q120);
															tree _q121 = gimple_assign_rhs2 (_a6);
															_q121 = do_valueize (valueize, _q121);
															if (tree_swap_operands_p (_q120, _q121))
															  std::swap (_q120, _q121);
															if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
															  {
															    if ((_q121 == _q100 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q100, 0) && types_match (_q121, _q100)))
															      {
																if (integer_zerop (_q21))
																  {
																    switch (TREE_CODE (_p1))
																      {
																      case SSA_NAME:
																        if (gimple *_d7 = get_def (valueize, _p1))
																          {
																	    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																	      switch (gimple_assign_rhs_code (_a7))
																		{
																		case BIT_XOR_EXPR:
																		  {
																		    tree _q160 = gimple_assign_rhs1 (_a7);
																		    _q160 = do_valueize (valueize, _q160);
																		    tree _q161 = gimple_assign_rhs2 (_a7);
																		    _q161 = do_valueize (valueize, _q161);
																		    if (tree_swap_operands_p (_q160, _q161))
																		      std::swap (_q160, _q161);
																		    switch (TREE_CODE (_q160))
																		      {
																		      case SSA_NAME:
																		        if (gimple *_d8 = get_def (valueize, _q160))
																		          {
																			    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																			      switch (gimple_assign_rhs_code (_a8))
																				{
																				case NEGATE_EXPR:
																				  {
																				    tree _q170 = gimple_assign_rhs1 (_a8);
																				    _q170 = do_valueize (valueize, _q170);
																				    switch (TREE_CODE (_q170))
																				      {
																				      case SSA_NAME:
																				        if (gimple *_d9 = get_def (valueize, _q170))
																				          {
																					    if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																					      switch (gimple_assign_rhs_code (_a9))
																						{
																						CASE_CONVERT:
																						  {
																						    tree _q180 = gimple_assign_rhs1 (_a9);
																						    _q180 = do_valueize (valueize, _q180);
																						    switch (TREE_CODE (_q180))
																						      {
																						      case SSA_NAME:
																						        if (gimple *_d10 = get_def (valueize, _q180))
																						          {
																							    if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																							      switch (gimple_assign_rhs_code (_a10))
																								{
																								case LT_EXPR:
																								  {
																								    tree _q190 = gimple_assign_rhs1 (_a10);
																								    _q190 = do_valueize (valueize, _q190);
																								    tree _q191 = gimple_assign_rhs2 (_a10);
																								    _q191 = do_valueize (valueize, _q191);
																								    if ((_q190 == _q40 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q40, 0) && types_match (_q190, _q40)))
																								      {
																									if (integer_zerop (_q191))
																									  {
																									    if (gimple_max_value (_q161, valueize))
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
																											    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																		  if (gimple_max_value (_q160, valueize))
																		    {
																		      switch (TREE_CODE (_q161))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d8 = get_def (valueize, _q161))
																			    {
																			      if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																			        switch (gimple_assign_rhs_code (_a8))
																				  {
																				  case NEGATE_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a8);
																				      _q180 = do_valueize (valueize, _q180);
																				      switch (TREE_CODE (_q180))
																				        {
																					case SSA_NAME:
																					  if (gimple *_d9 = get_def (valueize, _q180))
																					    {
																					      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																					        switch (gimple_assign_rhs_code (_a9))
																						  {
																						  CASE_CONVERT:
																						    {
																						      tree _q190 = gimple_assign_rhs1 (_a9);
																						      _q190 = do_valueize (valueize, _q190);
																						      switch (TREE_CODE (_q190))
																						        {
																							case SSA_NAME:
																							  if (gimple *_d10 = get_def (valueize, _q190))
																							    {
																							      if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																							        switch (gimple_assign_rhs_code (_a10))
																								  {
																								  case LT_EXPR:
																								    {
																								      tree _q200 = gimple_assign_rhs1 (_a10);
																								      _q200 = do_valueize (valueize, _q200);
																								      tree _q201 = gimple_assign_rhs2 (_a10);
																								      _q201 = do_valueize (valueize, _q201);
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																							  break;
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
																				    }
																				  default:;
																				  }
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
																      case SSA_NAME:
																        if (gimple *_d7 = get_def (valueize, _p1))
																          {
																	    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																	      switch (gimple_assign_rhs_code (_a7))
																		{
																		case BIT_XOR_EXPR:
																		  {
																		    tree _q160 = gimple_assign_rhs1 (_a7);
																		    _q160 = do_valueize (valueize, _q160);
																		    tree _q161 = gimple_assign_rhs2 (_a7);
																		    _q161 = do_valueize (valueize, _q161);
																		    if (tree_swap_operands_p (_q160, _q161))
																		      std::swap (_q160, _q161);
																		    switch (TREE_CODE (_q160))
																		      {
																		      case SSA_NAME:
																		        if (gimple *_d8 = get_def (valueize, _q160))
																		          {
																			    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																			      switch (gimple_assign_rhs_code (_a8))
																				{
																				case NEGATE_EXPR:
																				  {
																				    tree _q170 = gimple_assign_rhs1 (_a8);
																				    _q170 = do_valueize (valueize, _q170);
																				    switch (TREE_CODE (_q170))
																				      {
																				      case SSA_NAME:
																				        if (gimple *_d9 = get_def (valueize, _q170))
																				          {
																					    if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																					      switch (gimple_assign_rhs_code (_a9))
																						{
																						CASE_CONVERT:
																						  {
																						    tree _q180 = gimple_assign_rhs1 (_a9);
																						    _q180 = do_valueize (valueize, _q180);
																						    switch (TREE_CODE (_q180))
																						      {
																						      case SSA_NAME:
																						        if (gimple *_d10 = get_def (valueize, _q180))
																						          {
																							    if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																							      switch (gimple_assign_rhs_code (_a10))
																								{
																								case LT_EXPR:
																								  {
																								    tree _q190 = gimple_assign_rhs1 (_a10);
																								    _q190 = do_valueize (valueize, _q190);
																								    tree _q191 = gimple_assign_rhs2 (_a10);
																								    _q191 = do_valueize (valueize, _q191);
																								    if ((_q190 == _q40 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q40, 0) && types_match (_q190, _q40)))
																								      {
																									if (integer_zerop (_q191))
																									  {
																									    if (gimple_max_value (_q161, valueize))
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
																											    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																		  if (gimple_max_value (_q160, valueize))
																		    {
																		      switch (TREE_CODE (_q161))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d8 = get_def (valueize, _q161))
																			    {
																			      if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																			        switch (gimple_assign_rhs_code (_a8))
																				  {
																				  case NEGATE_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a8);
																				      _q180 = do_valueize (valueize, _q180);
																				      switch (TREE_CODE (_q180))
																				        {
																					case SSA_NAME:
																					  if (gimple *_d9 = get_def (valueize, _q180))
																					    {
																					      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																					        switch (gimple_assign_rhs_code (_a9))
																						  {
																						  CASE_CONVERT:
																						    {
																						      tree _q190 = gimple_assign_rhs1 (_a9);
																						      _q190 = do_valueize (valueize, _q190);
																						      switch (TREE_CODE (_q190))
																						        {
																							case SSA_NAME:
																							  if (gimple *_d10 = get_def (valueize, _q190))
																							    {
																							      if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																							        switch (gimple_assign_rhs_code (_a10))
																								  {
																								  case LT_EXPR:
																								    {
																								      tree _q200 = gimple_assign_rhs1 (_a10);
																								      _q200 = do_valueize (valueize, _q200);
																								      tree _q201 = gimple_assign_rhs2 (_a10);
																								      _q201 = do_valueize (valueize, _q201);
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																							  break;
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
																				    }
																				  default:;
																				  }
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
					        }
					      default:;
					      }
				        }
				      break;
				    default:;
				    }
			          break;
			        }
			      case GE_EXPR:
			        {
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
				  switch (TREE_CODE (_q20))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q20))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case BIT_AND_EXPR:
					        {
						  tree _q30 = gimple_assign_rhs1 (_a3);
						  _q30 = do_valueize (valueize, _q30);
						  tree _q31 = gimple_assign_rhs2 (_a3);
						  _q31 = do_valueize (valueize, _q31);
						  if (tree_swap_operands_p (_q30, _q31))
						    std::swap (_q30, _q31);
						  switch (TREE_CODE (_q30))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _q30))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      case BIT_XOR_EXPR:
							        {
								  tree _q40 = gimple_assign_rhs1 (_a4);
								  _q40 = do_valueize (valueize, _q40);
								  tree _q41 = gimple_assign_rhs2 (_a4);
								  _q41 = do_valueize (valueize, _q41);
								  if (tree_swap_operands_p (_q40, _q41))
								    std::swap (_q40, _q41);
								  switch (TREE_CODE (_q31))
								    {
								    case SSA_NAME:
								      if (gimple *_d5 = get_def (valueize, _q31))
								        {
									  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
									    switch (gimple_assign_rhs_code (_a5))
									      {
									      case BIT_XOR_EXPR:
									        {
										  tree _q70 = gimple_assign_rhs1 (_a5);
										  _q70 = do_valueize (valueize, _q70);
										  tree _q71 = gimple_assign_rhs2 (_a5);
										  _q71 = do_valueize (valueize, _q71);
										  if (tree_swap_operands_p (_q70, _q71))
										    std::swap (_q70, _q71);
										  if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
										    {
										      {
											tree _q71_pops[1];
											if (gimple_nop_convert (_q71, _q71_pops, valueize))
											  {
											    tree _q90 = _q71_pops[0];
											    switch (TREE_CODE (_q90))
											      {
											      case SSA_NAME:
											        if (gimple *_d6 = get_def (valueize, _q90))
											          {
												    if (gassign *_a6 = dyn_cast <gassign *> (_d6))
												      switch (gimple_assign_rhs_code (_a6))
													{
													case MINUS_EXPR:
													  {
													    tree _q100 = gimple_assign_rhs1 (_a6);
													    _q100 = do_valueize (valueize, _q100);
													    tree _q101 = gimple_assign_rhs2 (_a6);
													    _q101 = do_valueize (valueize, _q101);
													    {
													      tree _q100_pops[1];
													      if (gimple_nop_convert (_q100, _q100_pops, valueize))
													        {
														  tree _q110 = _q100_pops[0];
														  if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
														    {
														      {
															tree _q101_pops[1];
															if (gimple_nop_convert (_q101, _q101_pops, valueize))
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
																	  case SSA_NAME:
																	    if (gimple *_d7 = get_def (valueize, _p2))
																	      {
																		if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																		  switch (gimple_assign_rhs_code (_a7))
																		    {
																		    case BIT_XOR_EXPR:
																		      {
																			tree _q170 = gimple_assign_rhs1 (_a7);
																			_q170 = do_valueize (valueize, _q170);
																			tree _q171 = gimple_assign_rhs2 (_a7);
																			_q171 = do_valueize (valueize, _q171);
																			if (tree_swap_operands_p (_q170, _q171))
																			  std::swap (_q170, _q171);
																			switch (TREE_CODE (_q170))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d8 = get_def (valueize, _q170))
																			      {
																				if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				  switch (gimple_assign_rhs_code (_a8))
																				    {
																				    case NEGATE_EXPR:
																				      {
																					tree _q180 = gimple_assign_rhs1 (_a8);
																					_q180 = do_valueize (valueize, _q180);
																					switch (TREE_CODE (_q180))
																					  {
																					  case SSA_NAME:
																					    if (gimple *_d9 = get_def (valueize, _q180))
																					      {
																						if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						  switch (gimple_assign_rhs_code (_a9))
																						    {
																						    CASE_CONVERT:
																						      {
																							tree _q190 = gimple_assign_rhs1 (_a9);
																							_q190 = do_valueize (valueize, _q190);
																							switch (TREE_CODE (_q190))
																							  {
																							  case SSA_NAME:
																							    if (gimple *_d10 = get_def (valueize, _q190))
																							      {
																								if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																								  switch (gimple_assign_rhs_code (_a10))
																								    {
																								    case LT_EXPR:
																								      {
																									tree _q200 = gimple_assign_rhs1 (_a10);
																									_q200 = do_valueize (valueize, _q200);
																									tree _q201 = gimple_assign_rhs2 (_a10);
																									_q201 = do_valueize (valueize, _q201);
																									if ((_q200 == _q40 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q40, 0) && types_match (_q200, _q40)))
																									  {
																									    if (integer_zerop (_q201))
																									      {
																										if (gimple_max_value (_q171, valueize))
																										  {
																										    {
																										      tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _q71 };
																										      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																											{
																											  {
																											    res_ops[0] = captures[0];
																											    res_ops[1] = captures[1];
																											    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																							    break;
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
																				      }
																				    default:;
																				    }
																			      }
																			    break;
																		          default:;
																		          }
																		      if (gimple_max_value (_q170, valueize))
																			{
																			  switch (TREE_CODE (_q171))
																			    {
																			    case SSA_NAME:
																			      if (gimple *_d8 = get_def (valueize, _q171))
																			        {
																				  if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				    switch (gimple_assign_rhs_code (_a8))
																				      {
																				      case NEGATE_EXPR:
																				        {
																					  tree _q190 = gimple_assign_rhs1 (_a8);
																					  _q190 = do_valueize (valueize, _q190);
																					  switch (TREE_CODE (_q190))
																					    {
																					    case SSA_NAME:
																					      if (gimple *_d9 = get_def (valueize, _q190))
																					        {
																						  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						    switch (gimple_assign_rhs_code (_a9))
																						      {
																						      CASE_CONVERT:
																						        {
																							  tree _q200 = gimple_assign_rhs1 (_a9);
																							  _q200 = do_valueize (valueize, _q200);
																							  switch (TREE_CODE (_q200))
																							    {
																							    case SSA_NAME:
																							      if (gimple *_d10 = get_def (valueize, _q200))
																							        {
																								  if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																								    switch (gimple_assign_rhs_code (_a10))
																								      {
																								      case LT_EXPR:
																								        {
																									  tree _q210 = gimple_assign_rhs1 (_a10);
																									  _q210 = do_valueize (valueize, _q210);
																									  tree _q211 = gimple_assign_rhs2 (_a10);
																									  _q211 = do_valueize (valueize, _q211);
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																				        }
																				      default:;
																				      }
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
											      default:;
											      }
											  }
										      }
										    }
										  if ((_q70 == _q41 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q41, 0) && types_match (_q70, _q41)))
										    {
										      {
											tree _q71_pops[1];
											if (gimple_nop_convert (_q71, _q71_pops, valueize))
											  {
											    tree _q90 = _q71_pops[0];
											    switch (TREE_CODE (_q90))
											      {
											      case SSA_NAME:
											        if (gimple *_d6 = get_def (valueize, _q90))
											          {
												    if (gassign *_a6 = dyn_cast <gassign *> (_d6))
												      switch (gimple_assign_rhs_code (_a6))
													{
													case MINUS_EXPR:
													  {
													    tree _q100 = gimple_assign_rhs1 (_a6);
													    _q100 = do_valueize (valueize, _q100);
													    tree _q101 = gimple_assign_rhs2 (_a6);
													    _q101 = do_valueize (valueize, _q101);
													    {
													      tree _q100_pops[1];
													      if (gimple_nop_convert (_q100, _q100_pops, valueize))
													        {
														  tree _q110 = _q100_pops[0];
														  if ((_q110 == _q41 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q41, 0) && types_match (_q110, _q41)))
														    {
														      {
															tree _q101_pops[1];
															if (gimple_nop_convert (_q101, _q101_pops, valueize))
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
																	  case SSA_NAME:
																	    if (gimple *_d7 = get_def (valueize, _p2))
																	      {
																		if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																		  switch (gimple_assign_rhs_code (_a7))
																		    {
																		    case BIT_XOR_EXPR:
																		      {
																			tree _q170 = gimple_assign_rhs1 (_a7);
																			_q170 = do_valueize (valueize, _q170);
																			tree _q171 = gimple_assign_rhs2 (_a7);
																			_q171 = do_valueize (valueize, _q171);
																			if (tree_swap_operands_p (_q170, _q171))
																			  std::swap (_q170, _q171);
																			switch (TREE_CODE (_q170))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d8 = get_def (valueize, _q170))
																			      {
																				if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				  switch (gimple_assign_rhs_code (_a8))
																				    {
																				    case NEGATE_EXPR:
																				      {
																					tree _q180 = gimple_assign_rhs1 (_a8);
																					_q180 = do_valueize (valueize, _q180);
																					switch (TREE_CODE (_q180))
																					  {
																					  case SSA_NAME:
																					    if (gimple *_d9 = get_def (valueize, _q180))
																					      {
																						if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						  switch (gimple_assign_rhs_code (_a9))
																						    {
																						    CASE_CONVERT:
																						      {
																							tree _q190 = gimple_assign_rhs1 (_a9);
																							_q190 = do_valueize (valueize, _q190);
																							switch (TREE_CODE (_q190))
																							  {
																							  case SSA_NAME:
																							    if (gimple *_d10 = get_def (valueize, _q190))
																							      {
																								if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																								  switch (gimple_assign_rhs_code (_a10))
																								    {
																								    case LT_EXPR:
																								      {
																									tree _q200 = gimple_assign_rhs1 (_a10);
																									_q200 = do_valueize (valueize, _q200);
																									tree _q201 = gimple_assign_rhs2 (_a10);
																									_q201 = do_valueize (valueize, _q201);
																									if ((_q200 == _q41 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q41, 0) && types_match (_q200, _q41)))
																									  {
																									    if (integer_zerop (_q201))
																									      {
																										if (gimple_max_value (_q171, valueize))
																										  {
																										    {
																										      tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _q71 };
																										      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																											{
																											  {
																											    res_ops[0] = captures[0];
																											    res_ops[1] = captures[1];
																											    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																							    break;
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
																				      }
																				    default:;
																				    }
																			      }
																			    break;
																		          default:;
																		          }
																		      if (gimple_max_value (_q170, valueize))
																			{
																			  switch (TREE_CODE (_q171))
																			    {
																			    case SSA_NAME:
																			      if (gimple *_d8 = get_def (valueize, _q171))
																			        {
																				  if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				    switch (gimple_assign_rhs_code (_a8))
																				      {
																				      case NEGATE_EXPR:
																				        {
																					  tree _q190 = gimple_assign_rhs1 (_a8);
																					  _q190 = do_valueize (valueize, _q190);
																					  switch (TREE_CODE (_q190))
																					    {
																					    case SSA_NAME:
																					      if (gimple *_d9 = get_def (valueize, _q190))
																					        {
																						  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						    switch (gimple_assign_rhs_code (_a9))
																						      {
																						      CASE_CONVERT:
																						        {
																							  tree _q200 = gimple_assign_rhs1 (_a9);
																							  _q200 = do_valueize (valueize, _q200);
																							  switch (TREE_CODE (_q200))
																							    {
																							    case SSA_NAME:
																							      if (gimple *_d10 = get_def (valueize, _q200))
																							        {
																								  if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																								    switch (gimple_assign_rhs_code (_a10))
																								      {
																								      case LT_EXPR:
																								        {
																									  tree _q210 = gimple_assign_rhs1 (_a10);
																									  _q210 = do_valueize (valueize, _q210);
																									  tree _q211 = gimple_assign_rhs2 (_a10);
																									  _q211 = do_valueize (valueize, _q211);
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																				        }
																				      default:;
																				      }
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
								      break;
								    default:;
								    }
								  {
								    tree _q41_pops[1];
								    if (gimple_nop_convert (_q41, _q41_pops, valueize))
								      {
									tree _q60 = _q41_pops[0];
									switch (TREE_CODE (_q60))
									  {
									  case SSA_NAME:
									    if (gimple *_d5 = get_def (valueize, _q60))
									      {
										if (gassign *_a5 = dyn_cast <gassign *> (_d5))
										  switch (gimple_assign_rhs_code (_a5))
										    {
										    case MINUS_EXPR:
										      {
											tree _q70 = gimple_assign_rhs1 (_a5);
											_q70 = do_valueize (valueize, _q70);
											tree _q71 = gimple_assign_rhs2 (_a5);
											_q71 = do_valueize (valueize, _q71);
											{
											  tree _q70_pops[1];
											  if (gimple_nop_convert (_q70, _q70_pops, valueize))
											    {
											      tree _q80 = _q70_pops[0];
											      if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
												{
												  {
												    tree _q71_pops[1];
												    if (gimple_nop_convert (_q71, _q71_pops, valueize))
												      {
													tree _q100 = _q71_pops[0];
													switch (TREE_CODE (_q31))
													  {
													  case SSA_NAME:
													    if (gimple *_d6 = get_def (valueize, _q31))
													      {
														if (gassign *_a6 = dyn_cast <gassign *> (_d6))
														  switch (gimple_assign_rhs_code (_a6))
														    {
														    case BIT_XOR_EXPR:
														      {
															tree _q120 = gimple_assign_rhs1 (_a6);
															_q120 = do_valueize (valueize, _q120);
															tree _q121 = gimple_assign_rhs2 (_a6);
															_q121 = do_valueize (valueize, _q121);
															if (tree_swap_operands_p (_q120, _q121))
															  std::swap (_q120, _q121);
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
																	  case SSA_NAME:
																	    if (gimple *_d7 = get_def (valueize, _p2))
																	      {
																		if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																		  switch (gimple_assign_rhs_code (_a7))
																		    {
																		    case BIT_XOR_EXPR:
																		      {
																			tree _q170 = gimple_assign_rhs1 (_a7);
																			_q170 = do_valueize (valueize, _q170);
																			tree _q171 = gimple_assign_rhs2 (_a7);
																			_q171 = do_valueize (valueize, _q171);
																			if (tree_swap_operands_p (_q170, _q171))
																			  std::swap (_q170, _q171);
																			switch (TREE_CODE (_q170))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d8 = get_def (valueize, _q170))
																			      {
																				if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				  switch (gimple_assign_rhs_code (_a8))
																				    {
																				    case NEGATE_EXPR:
																				      {
																					tree _q180 = gimple_assign_rhs1 (_a8);
																					_q180 = do_valueize (valueize, _q180);
																					switch (TREE_CODE (_q180))
																					  {
																					  case SSA_NAME:
																					    if (gimple *_d9 = get_def (valueize, _q180))
																					      {
																						if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						  switch (gimple_assign_rhs_code (_a9))
																						    {
																						    CASE_CONVERT:
																						      {
																							tree _q190 = gimple_assign_rhs1 (_a9);
																							_q190 = do_valueize (valueize, _q190);
																							switch (TREE_CODE (_q190))
																							  {
																							  case SSA_NAME:
																							    if (gimple *_d10 = get_def (valueize, _q190))
																							      {
																								if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																								  switch (gimple_assign_rhs_code (_a10))
																								    {
																								    case LT_EXPR:
																								      {
																									tree _q200 = gimple_assign_rhs1 (_a10);
																									_q200 = do_valueize (valueize, _q200);
																									tree _q201 = gimple_assign_rhs2 (_a10);
																									_q201 = do_valueize (valueize, _q201);
																									if ((_q200 == _q40 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q40, 0) && types_match (_q200, _q40)))
																									  {
																									    if (integer_zerop (_q201))
																									      {
																										if (gimple_max_value (_q171, valueize))
																										  {
																										    {
																										      tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
																										      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																											{
																											  {
																											    res_ops[0] = captures[0];
																											    res_ops[1] = captures[1];
																											    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																							    break;
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
																				      }
																				    default:;
																				    }
																			      }
																			    break;
																		          default:;
																		          }
																		      if (gimple_max_value (_q170, valueize))
																			{
																			  switch (TREE_CODE (_q171))
																			    {
																			    case SSA_NAME:
																			      if (gimple *_d8 = get_def (valueize, _q171))
																			        {
																				  if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				    switch (gimple_assign_rhs_code (_a8))
																				      {
																				      case NEGATE_EXPR:
																				        {
																					  tree _q190 = gimple_assign_rhs1 (_a8);
																					  _q190 = do_valueize (valueize, _q190);
																					  switch (TREE_CODE (_q190))
																					    {
																					    case SSA_NAME:
																					      if (gimple *_d9 = get_def (valueize, _q190))
																					        {
																						  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						    switch (gimple_assign_rhs_code (_a9))
																						      {
																						      CASE_CONVERT:
																						        {
																							  tree _q200 = gimple_assign_rhs1 (_a9);
																							  _q200 = do_valueize (valueize, _q200);
																							  switch (TREE_CODE (_q200))
																							    {
																							    case SSA_NAME:
																							      if (gimple *_d10 = get_def (valueize, _q200))
																							        {
																								  if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																								    switch (gimple_assign_rhs_code (_a10))
																								      {
																								      case LT_EXPR:
																								        {
																									  tree _q210 = gimple_assign_rhs1 (_a10);
																									  _q210 = do_valueize (valueize, _q210);
																									  tree _q211 = gimple_assign_rhs2 (_a10);
																									  _q211 = do_valueize (valueize, _q211);
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																				        }
																				      default:;
																				      }
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
																	  case SSA_NAME:
																	    if (gimple *_d7 = get_def (valueize, _p2))
																	      {
																		if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																		  switch (gimple_assign_rhs_code (_a7))
																		    {
																		    case BIT_XOR_EXPR:
																		      {
																			tree _q170 = gimple_assign_rhs1 (_a7);
																			_q170 = do_valueize (valueize, _q170);
																			tree _q171 = gimple_assign_rhs2 (_a7);
																			_q171 = do_valueize (valueize, _q171);
																			if (tree_swap_operands_p (_q170, _q171))
																			  std::swap (_q170, _q171);
																			switch (TREE_CODE (_q170))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d8 = get_def (valueize, _q170))
																			      {
																				if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				  switch (gimple_assign_rhs_code (_a8))
																				    {
																				    case NEGATE_EXPR:
																				      {
																					tree _q180 = gimple_assign_rhs1 (_a8);
																					_q180 = do_valueize (valueize, _q180);
																					switch (TREE_CODE (_q180))
																					  {
																					  case SSA_NAME:
																					    if (gimple *_d9 = get_def (valueize, _q180))
																					      {
																						if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						  switch (gimple_assign_rhs_code (_a9))
																						    {
																						    CASE_CONVERT:
																						      {
																							tree _q190 = gimple_assign_rhs1 (_a9);
																							_q190 = do_valueize (valueize, _q190);
																							switch (TREE_CODE (_q190))
																							  {
																							  case SSA_NAME:
																							    if (gimple *_d10 = get_def (valueize, _q190))
																							      {
																								if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																								  switch (gimple_assign_rhs_code (_a10))
																								    {
																								    case LT_EXPR:
																								      {
																									tree _q200 = gimple_assign_rhs1 (_a10);
																									_q200 = do_valueize (valueize, _q200);
																									tree _q201 = gimple_assign_rhs2 (_a10);
																									_q201 = do_valueize (valueize, _q201);
																									if ((_q200 == _q40 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q40, 0) && types_match (_q200, _q40)))
																									  {
																									    if (integer_zerop (_q201))
																									      {
																										if (gimple_max_value (_q171, valueize))
																										  {
																										    {
																										      tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
																										      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																											{
																											  {
																											    res_ops[0] = captures[0];
																											    res_ops[1] = captures[1];
																											    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																							    break;
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
																				      }
																				    default:;
																				    }
																			      }
																			    break;
																		          default:;
																		          }
																		      if (gimple_max_value (_q170, valueize))
																			{
																			  switch (TREE_CODE (_q171))
																			    {
																			    case SSA_NAME:
																			      if (gimple *_d8 = get_def (valueize, _q171))
																			        {
																				  if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				    switch (gimple_assign_rhs_code (_a8))
																				      {
																				      case NEGATE_EXPR:
																				        {
																					  tree _q190 = gimple_assign_rhs1 (_a8);
																					  _q190 = do_valueize (valueize, _q190);
																					  switch (TREE_CODE (_q190))
																					    {
																					    case SSA_NAME:
																					      if (gimple *_d9 = get_def (valueize, _q190))
																					        {
																						  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						    switch (gimple_assign_rhs_code (_a9))
																						      {
																						      CASE_CONVERT:
																						        {
																							  tree _q200 = gimple_assign_rhs1 (_a9);
																							  _q200 = do_valueize (valueize, _q200);
																							  switch (TREE_CODE (_q200))
																							    {
																							    case SSA_NAME:
																							      if (gimple *_d10 = get_def (valueize, _q200))
																							        {
																								  if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																								    switch (gimple_assign_rhs_code (_a10))
																								      {
																								      case LT_EXPR:
																								        {
																									  tree _q210 = gimple_assign_rhs1 (_a10);
																									  _q210 = do_valueize (valueize, _q210);
																									  tree _q211 = gimple_assign_rhs2 (_a10);
																									  _q211 = do_valueize (valueize, _q211);
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																				        }
																				      default:;
																				      }
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
									    break;
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
					        }
					      default:;
					      }
				        }
				      break;
				    default:;
				    }
			          break;
			        }
			      case NE_EXPR:
			        {
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
				  if (tree_swap_operands_p (_q20, _q21))
				    std::swap (_q20, _q21);
				  switch (TREE_CODE (_q20))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q20))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case IMAGPART_EXPR:
					        {
						  tree _q30 = TREE_OPERAND (gimple_assign_rhs1 (_a3), 0);
						  if ((TREE_CODE (_q30) == SSA_NAME
						       || is_gimple_min_invariant (_q30)))
						    {
						      _q30 = do_valueize (valueize, _q30);
						      switch (TREE_CODE (_q30))
						        {
							case SSA_NAME:
							  if (gimple *_d4 = get_def (valueize, _q30))
							    {
							      if (gcall *_c4 = dyn_cast <gcall *> (_d4))
							        switch (gimple_call_combined_fn (_c4))
								  {
								  case CFN_SUB_OVERFLOW:
								    if (gimple_call_num_args (_c4) == 2)
								      {
									tree _q40 = gimple_call_arg (_c4, 0);
									_q40 = do_valueize (valueize, _q40);
									tree _q41 = gimple_call_arg (_c4, 1);
									_q41 = do_valueize (valueize, _q41);
									if (integer_zerop (_q21))
									  {
									    switch (TREE_CODE (_p1))
									      {
									      case SSA_NAME:
									        if (gimple *_d5 = get_def (valueize, _p1))
									          {
										    if (gassign *_a5 = dyn_cast <gassign *> (_d5))
										      switch (gimple_assign_rhs_code (_a5))
											{
											case BIT_XOR_EXPR:
											  {
											    tree _q80 = gimple_assign_rhs1 (_a5);
											    _q80 = do_valueize (valueize, _q80);
											    tree _q81 = gimple_assign_rhs2 (_a5);
											    _q81 = do_valueize (valueize, _q81);
											    if (tree_swap_operands_p (_q80, _q81))
											      std::swap (_q80, _q81);
											    switch (TREE_CODE (_q80))
											      {
											      case SSA_NAME:
											        if (gimple *_d6 = get_def (valueize, _q80))
											          {
												    if (gassign *_a6 = dyn_cast <gassign *> (_d6))
												      switch (gimple_assign_rhs_code (_a6))
													{
													case NEGATE_EXPR:
													  {
													    tree _q90 = gimple_assign_rhs1 (_a6);
													    _q90 = do_valueize (valueize, _q90);
													    switch (TREE_CODE (_q90))
													      {
													      case SSA_NAME:
													        if (gimple *_d7 = get_def (valueize, _q90))
													          {
														    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
														      switch (gimple_assign_rhs_code (_a7))
															{
															CASE_CONVERT:
															  {
															    tree _q100 = gimple_assign_rhs1 (_a7);
															    _q100 = do_valueize (valueize, _q100);
															    switch (TREE_CODE (_q100))
															      {
															      case SSA_NAME:
															        if (gimple *_d8 = get_def (valueize, _q100))
															          {
																    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																      switch (gimple_assign_rhs_code (_a8))
																	{
																	case LT_EXPR:
																	  {
																	    tree _q110 = gimple_assign_rhs1 (_a8);
																	    _q110 = do_valueize (valueize, _q110);
																	    tree _q111 = gimple_assign_rhs2 (_a8);
																	    _q111 = do_valueize (valueize, _q111);
																	    if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
																	      {
																		if (integer_zerop (_q111))
																		  {
																		    if (gimple_max_value (_q81, valueize))
																		      {
																			switch (TREE_CODE (_p2))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d9 = get_def (valueize, _p2))
																			      {
																				if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																				  switch (gimple_assign_rhs_code (_a9))
																				    {
																				    case REALPART_EXPR:
																				      {
																					tree _q150 = TREE_OPERAND (gimple_assign_rhs1 (_a9), 0);
																					if ((TREE_CODE (_q150) == SSA_NAME
																					     || is_gimple_min_invariant (_q150)))
																					  {
																					    _q150 = do_valueize (valueize, _q150);
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
																							if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 74, __FILE__, __LINE__, false);
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
															        break;
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
													  }
													default:;
													}
											          }
											        break;
											      default:;
											      }
											  if (gimple_max_value (_q80, valueize))
											    {
											      switch (TREE_CODE (_q81))
											        {
												case SSA_NAME:
												  if (gimple *_d6 = get_def (valueize, _q81))
												    {
												      if (gassign *_a6 = dyn_cast <gassign *> (_d6))
												        switch (gimple_assign_rhs_code (_a6))
													  {
													  case NEGATE_EXPR:
													    {
													      tree _q100 = gimple_assign_rhs1 (_a6);
													      _q100 = do_valueize (valueize, _q100);
													      switch (TREE_CODE (_q100))
													        {
														case SSA_NAME:
														  if (gimple *_d7 = get_def (valueize, _q100))
														    {
														      if (gassign *_a7 = dyn_cast <gassign *> (_d7))
														        switch (gimple_assign_rhs_code (_a7))
															  {
															  CASE_CONVERT:
															    {
															      tree _q110 = gimple_assign_rhs1 (_a7);
															      _q110 = do_valueize (valueize, _q110);
															      switch (TREE_CODE (_q110))
															        {
																case SSA_NAME:
																  if (gimple *_d8 = get_def (valueize, _q110))
																    {
																      if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																        switch (gimple_assign_rhs_code (_a8))
																	  {
																	  case LT_EXPR:
																	    {
																	      tree _q120 = gimple_assign_rhs1 (_a8);
																	      _q120 = do_valueize (valueize, _q120);
																	      tree _q121 = gimple_assign_rhs2 (_a8);
																	      _q121 = do_valueize (valueize, _q121);
																	      if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
																		{
																		  if (integer_zerop (_q121))
																		    {
																		      switch (TREE_CODE (_p2))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d9 = get_def (valueize, _p2))
																			    {
																			      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																			        switch (gimple_assign_rhs_code (_a9))
																				  {
																				  case REALPART_EXPR:
																				    {
																				      tree _q150 = TREE_OPERAND (gimple_assign_rhs1 (_a9), 0);
																				      if ((TREE_CODE (_q150) == SSA_NAME
																				           || is_gimple_min_invariant (_q150)))
																				        {
																					  _q150 = do_valueize (valueize, _q150);
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
																						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 74, __FILE__, __LINE__, false);
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
															    }
															  default:;
															  }
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
									  }
								      }
								    break;
								  default:;
								  }
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
			        }
			      case BIT_AND_EXPR:
			        {
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
				  if (tree_swap_operands_p (_q20, _q21))
				    std::swap (_q20, _q21);
				  switch (TREE_CODE (_q20))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q20))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case LT_EXPR:
					        {
						  tree _q30 = gimple_assign_rhs1 (_a3);
						  _q30 = do_valueize (valueize, _q30);
						  tree _q31 = gimple_assign_rhs2 (_a3);
						  _q31 = do_valueize (valueize, _q31);
						  switch (TREE_CODE (_q30))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _q30))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      case BIT_XOR_EXPR:
							        {
								  tree _q40 = gimple_assign_rhs1 (_a4);
								  _q40 = do_valueize (valueize, _q40);
								  tree _q41 = gimple_assign_rhs2 (_a4);
								  _q41 = do_valueize (valueize, _q41);
								  if (tree_swap_operands_p (_q40, _q41))
								    std::swap (_q40, _q41);
								  {
								    tree _q41_pops[1];
								    if (gimple_nop_convert (_q41, _q41_pops, valueize))
								      {
									tree _q60 = _q41_pops[0];
									switch (TREE_CODE (_q60))
									  {
									  case SSA_NAME:
									    if (gimple *_d5 = get_def (valueize, _q60))
									      {
										if (gassign *_a5 = dyn_cast <gassign *> (_d5))
										  switch (gimple_assign_rhs_code (_a5))
										    {
										    case MINUS_EXPR:
										      {
											tree _q70 = gimple_assign_rhs1 (_a5);
											_q70 = do_valueize (valueize, _q70);
											tree _q71 = gimple_assign_rhs2 (_a5);
											_q71 = do_valueize (valueize, _q71);
											{
											  tree _q70_pops[1];
											  if (gimple_nop_convert (_q70, _q70_pops, valueize))
											    {
											      tree _q80 = _q70_pops[0];
											      if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
												{
												  {
												    tree _q71_pops[1];
												    if (gimple_nop_convert (_q71, _q71_pops, valueize))
												      {
													tree _q100 = _q71_pops[0];
													if (integer_zerop (_q31))
													  {
													    switch (TREE_CODE (_q21))
													      {
													      case SSA_NAME:
													        if (gimple *_d6 = get_def (valueize, _q21))
													          {
														    if (gassign *_a6 = dyn_cast <gassign *> (_d6))
														      switch (gimple_assign_rhs_code (_a6))
															{
															case LT_EXPR:
															  {
															    tree _q130 = gimple_assign_rhs1 (_a6);
															    _q130 = do_valueize (valueize, _q130);
															    tree _q131 = gimple_assign_rhs2 (_a6);
															    _q131 = do_valueize (valueize, _q131);
															    switch (TREE_CODE (_q130))
															      {
															      case SSA_NAME:
															        if (gimple *_d7 = get_def (valueize, _q130))
															          {
																    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																      switch (gimple_assign_rhs_code (_a7))
																	{
																	case BIT_XOR_EXPR:
																	  {
																	    tree _q140 = gimple_assign_rhs1 (_a7);
																	    _q140 = do_valueize (valueize, _q140);
																	    tree _q141 = gimple_assign_rhs2 (_a7);
																	    _q141 = do_valueize (valueize, _q141);
																	    if (tree_swap_operands_p (_q140, _q141))
																	      std::swap (_q140, _q141);
																	    if ((_q140 == _q40 && ! TREE_SIDE_EFFECTS (_q140)) || (operand_equal_p (_q140, _q40, 0) && types_match (_q140, _q40)))
																	      {
																		if ((_q141 == _q100 && ! TREE_SIDE_EFFECTS (_q141)) || (operand_equal_p (_q141, _q100, 0) && types_match (_q141, _q100)))
																		  {
																		    if (integer_zerop (_q131))
																		      {
																			switch (TREE_CODE (_p1))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d8 = get_def (valueize, _p1))
																			      {
																				if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				  switch (gimple_assign_rhs_code (_a8))
																				    {
																				    case BIT_XOR_EXPR:
																				      {
																					tree _q180 = gimple_assign_rhs1 (_a8);
																					_q180 = do_valueize (valueize, _q180);
																					tree _q181 = gimple_assign_rhs2 (_a8);
																					_q181 = do_valueize (valueize, _q181);
																					if (tree_swap_operands_p (_q180, _q181))
																					  std::swap (_q180, _q181);
																					{
																					  tree _q180_pops[1];
																					  if (gimple_nop_convert (_q180, _q180_pops, valueize))
																					    {
																					      tree _q190 = _q180_pops[0];
																					      switch (TREE_CODE (_q190))
																					        {
																						case SSA_NAME:
																						  if (gimple *_d9 = get_def (valueize, _q190))
																						    {
																						      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						        switch (gimple_assign_rhs_code (_a9))
																							  {
																							  case NEGATE_EXPR:
																							    {
																							      tree _q200 = gimple_assign_rhs1 (_a9);
																							      _q200 = do_valueize (valueize, _q200);
																							      {
																								tree _q200_pops[1];
																								if (gimple_nop_convert (_q200, _q200_pops, valueize))
																								  {
																								    tree _q210 = _q200_pops[0];
																								    switch (TREE_CODE (_q210))
																								      {
																								      case SSA_NAME:
																								        if (gimple *_d10 = get_def (valueize, _q210))
																								          {
																									    if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																									      switch (gimple_assign_rhs_code (_a10))
																										{
																										CASE_CONVERT:
																										  {
																										    tree _q220 = gimple_assign_rhs1 (_a10);
																										    _q220 = do_valueize (valueize, _q220);
																										    switch (TREE_CODE (_q220))
																										      {
																										      case SSA_NAME:
																										        if (gimple *_d11 = get_def (valueize, _q220))
																										          {
																											    if (gassign *_a11 = dyn_cast <gassign *> (_d11))
																											      switch (gimple_assign_rhs_code (_a11))
																												{
																												case LT_EXPR:
																												  {
																												    tree _q230 = gimple_assign_rhs1 (_a11);
																												    _q230 = do_valueize (valueize, _q230);
																												    tree _q231 = gimple_assign_rhs2 (_a11);
																												    _q231 = do_valueize (valueize, _q231);
																												    if ((_q230 == _q40 && ! TREE_SIDE_EFFECTS (_q230)) || (operand_equal_p (_q230, _q40, 0) && types_match (_q230, _q40)))
																												      {
																													if (integer_zerop (_q231))
																													  {
																													    if (gimple_max_value (_q181, valueize))
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
																															    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																					    }
																					}
																					if (gimple_max_value (_q180, valueize))
																					  {
																					    {
																					      tree _q181_pops[1];
																					      if (gimple_nop_convert (_q181, _q181_pops, valueize))
																					        {
																						  tree _q200 = _q181_pops[0];
																						  switch (TREE_CODE (_q200))
																						    {
																						    case SSA_NAME:
																						      if (gimple *_d9 = get_def (valueize, _q200))
																						        {
																							  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							    switch (gimple_assign_rhs_code (_a9))
																							      {
																							      case NEGATE_EXPR:
																							        {
																								  tree _q210 = gimple_assign_rhs1 (_a9);
																								  _q210 = do_valueize (valueize, _q210);
																								  {
																								    tree _q210_pops[1];
																								    if (gimple_nop_convert (_q210, _q210_pops, valueize))
																								      {
																									tree _q220 = _q210_pops[0];
																									switch (TREE_CODE (_q220))
																									  {
																									  case SSA_NAME:
																									    if (gimple *_d10 = get_def (valueize, _q220))
																									      {
																										if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																										  switch (gimple_assign_rhs_code (_a10))
																										    {
																										    CASE_CONVERT:
																										      {
																											tree _q230 = gimple_assign_rhs1 (_a10);
																											_q230 = do_valueize (valueize, _q230);
																											switch (TREE_CODE (_q230))
																											  {
																											  case SSA_NAME:
																											    if (gimple *_d11 = get_def (valueize, _q230))
																											      {
																												if (gassign *_a11 = dyn_cast <gassign *> (_d11))
																												  switch (gimple_assign_rhs_code (_a11))
																												    {
																												    case LT_EXPR:
																												      {
																													tree _q240 = gimple_assign_rhs1 (_a11);
																													_q240 = do_valueize (valueize, _q240);
																													tree _q241 = gimple_assign_rhs2 (_a11);
																													_q241 = do_valueize (valueize, _q241);
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
																															    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																											    break;
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
																			  case SSA_NAME:
																			    if (gimple *_d8 = get_def (valueize, _p1))
																			      {
																				if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				  switch (gimple_assign_rhs_code (_a8))
																				    {
																				    case BIT_XOR_EXPR:
																				      {
																					tree _q180 = gimple_assign_rhs1 (_a8);
																					_q180 = do_valueize (valueize, _q180);
																					tree _q181 = gimple_assign_rhs2 (_a8);
																					_q181 = do_valueize (valueize, _q181);
																					if (tree_swap_operands_p (_q180, _q181))
																					  std::swap (_q180, _q181);
																					{
																					  tree _q180_pops[1];
																					  if (gimple_nop_convert (_q180, _q180_pops, valueize))
																					    {
																					      tree _q190 = _q180_pops[0];
																					      switch (TREE_CODE (_q190))
																					        {
																						case SSA_NAME:
																						  if (gimple *_d9 = get_def (valueize, _q190))
																						    {
																						      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						        switch (gimple_assign_rhs_code (_a9))
																							  {
																							  case NEGATE_EXPR:
																							    {
																							      tree _q200 = gimple_assign_rhs1 (_a9);
																							      _q200 = do_valueize (valueize, _q200);
																							      {
																								tree _q200_pops[1];
																								if (gimple_nop_convert (_q200, _q200_pops, valueize))
																								  {
																								    tree _q210 = _q200_pops[0];
																								    switch (TREE_CODE (_q210))
																								      {
																								      case SSA_NAME:
																								        if (gimple *_d10 = get_def (valueize, _q210))
																								          {
																									    if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																									      switch (gimple_assign_rhs_code (_a10))
																										{
																										CASE_CONVERT:
																										  {
																										    tree _q220 = gimple_assign_rhs1 (_a10);
																										    _q220 = do_valueize (valueize, _q220);
																										    switch (TREE_CODE (_q220))
																										      {
																										      case SSA_NAME:
																										        if (gimple *_d11 = get_def (valueize, _q220))
																										          {
																											    if (gassign *_a11 = dyn_cast <gassign *> (_d11))
																											      switch (gimple_assign_rhs_code (_a11))
																												{
																												case LT_EXPR:
																												  {
																												    tree _q230 = gimple_assign_rhs1 (_a11);
																												    _q230 = do_valueize (valueize, _q230);
																												    tree _q231 = gimple_assign_rhs2 (_a11);
																												    _q231 = do_valueize (valueize, _q231);
																												    if ((_q230 == _q40 && ! TREE_SIDE_EFFECTS (_q230)) || (operand_equal_p (_q230, _q40, 0) && types_match (_q230, _q40)))
																												      {
																													if (integer_zerop (_q231))
																													  {
																													    if (gimple_max_value (_q181, valueize))
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
																															    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																					    }
																					}
																					if (gimple_max_value (_q180, valueize))
																					  {
																					    {
																					      tree _q181_pops[1];
																					      if (gimple_nop_convert (_q181, _q181_pops, valueize))
																					        {
																						  tree _q200 = _q181_pops[0];
																						  switch (TREE_CODE (_q200))
																						    {
																						    case SSA_NAME:
																						      if (gimple *_d9 = get_def (valueize, _q200))
																						        {
																							  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							    switch (gimple_assign_rhs_code (_a9))
																							      {
																							      case NEGATE_EXPR:
																							        {
																								  tree _q210 = gimple_assign_rhs1 (_a9);
																								  _q210 = do_valueize (valueize, _q210);
																								  {
																								    tree _q210_pops[1];
																								    if (gimple_nop_convert (_q210, _q210_pops, valueize))
																								      {
																									tree _q220 = _q210_pops[0];
																									switch (TREE_CODE (_q220))
																									  {
																									  case SSA_NAME:
																									    if (gimple *_d10 = get_def (valueize, _q220))
																									      {
																										if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																										  switch (gimple_assign_rhs_code (_a10))
																										    {
																										    CASE_CONVERT:
																										      {
																											tree _q230 = gimple_assign_rhs1 (_a10);
																											_q230 = do_valueize (valueize, _q230);
																											switch (TREE_CODE (_q230))
																											  {
																											  case SSA_NAME:
																											    if (gimple *_d11 = get_def (valueize, _q230))
																											      {
																												if (gassign *_a11 = dyn_cast <gassign *> (_d11))
																												  switch (gimple_assign_rhs_code (_a11))
																												    {
																												    case LT_EXPR:
																												      {
																													tree _q240 = gimple_assign_rhs1 (_a11);
																													_q240 = do_valueize (valueize, _q240);
																													tree _q241 = gimple_assign_rhs2 (_a11);
																													_q241 = do_valueize (valueize, _q241);
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
																															    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																											    break;
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
															  }
															default:;
															}
													          }
													        break;
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
									    break;
								          default:;
								          }
								      }
								  }
								  if (integer_zerop (_q31))
								    {
								      switch (TREE_CODE (_q21))
								        {
									case SSA_NAME:
									  if (gimple *_d5 = get_def (valueize, _q21))
									    {
									      if (gassign *_a5 = dyn_cast <gassign *> (_d5))
									        switch (gimple_assign_rhs_code (_a5))
										  {
										  case LT_EXPR:
										    {
										      tree _q80 = gimple_assign_rhs1 (_a5);
										      _q80 = do_valueize (valueize, _q80);
										      tree _q81 = gimple_assign_rhs2 (_a5);
										      _q81 = do_valueize (valueize, _q81);
										      switch (TREE_CODE (_q80))
										        {
											case SSA_NAME:
											  if (gimple *_d6 = get_def (valueize, _q80))
											    {
											      if (gassign *_a6 = dyn_cast <gassign *> (_d6))
											        switch (gimple_assign_rhs_code (_a6))
												  {
												  case BIT_XOR_EXPR:
												    {
												      tree _q90 = gimple_assign_rhs1 (_a6);
												      _q90 = do_valueize (valueize, _q90);
												      tree _q91 = gimple_assign_rhs2 (_a6);
												      _q91 = do_valueize (valueize, _q91);
												      if (tree_swap_operands_p (_q90, _q91))
												        std::swap (_q90, _q91);
												      if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
													{
													  {
													    tree _q91_pops[1];
													    if (gimple_nop_convert (_q91, _q91_pops, valueize))
													      {
														tree _q110 = _q91_pops[0];
														switch (TREE_CODE (_q110))
														  {
														  case SSA_NAME:
														    if (gimple *_d7 = get_def (valueize, _q110))
														      {
															if (gassign *_a7 = dyn_cast <gassign *> (_d7))
															  switch (gimple_assign_rhs_code (_a7))
															    {
															    case MINUS_EXPR:
															      {
																tree _q120 = gimple_assign_rhs1 (_a7);
																_q120 = do_valueize (valueize, _q120);
																tree _q121 = gimple_assign_rhs2 (_a7);
																_q121 = do_valueize (valueize, _q121);
																{
																  tree _q120_pops[1];
																  if (gimple_nop_convert (_q120, _q120_pops, valueize))
																    {
																      tree _q130 = _q120_pops[0];
																      if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
																	{
																	  {
																	    tree _q121_pops[1];
																	    if (gimple_nop_convert (_q121, _q121_pops, valueize))
																	      {
																		tree _q150 = _q121_pops[0];
																		if ((_q150 == _q41 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q41, 0) && types_match (_q150, _q41)))
																		  {
																		    if (integer_zerop (_q81))
																		      {
																			switch (TREE_CODE (_p1))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d8 = get_def (valueize, _p1))
																			      {
																				if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				  switch (gimple_assign_rhs_code (_a8))
																				    {
																				    case BIT_XOR_EXPR:
																				      {
																					tree _q180 = gimple_assign_rhs1 (_a8);
																					_q180 = do_valueize (valueize, _q180);
																					tree _q181 = gimple_assign_rhs2 (_a8);
																					_q181 = do_valueize (valueize, _q181);
																					if (tree_swap_operands_p (_q180, _q181))
																					  std::swap (_q180, _q181);
																					{
																					  tree _q180_pops[1];
																					  if (gimple_nop_convert (_q180, _q180_pops, valueize))
																					    {
																					      tree _q190 = _q180_pops[0];
																					      switch (TREE_CODE (_q190))
																					        {
																						case SSA_NAME:
																						  if (gimple *_d9 = get_def (valueize, _q190))
																						    {
																						      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						        switch (gimple_assign_rhs_code (_a9))
																							  {
																							  case NEGATE_EXPR:
																							    {
																							      tree _q200 = gimple_assign_rhs1 (_a9);
																							      _q200 = do_valueize (valueize, _q200);
																							      {
																								tree _q200_pops[1];
																								if (gimple_nop_convert (_q200, _q200_pops, valueize))
																								  {
																								    tree _q210 = _q200_pops[0];
																								    switch (TREE_CODE (_q210))
																								      {
																								      case SSA_NAME:
																								        if (gimple *_d10 = get_def (valueize, _q210))
																								          {
																									    if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																									      switch (gimple_assign_rhs_code (_a10))
																										{
																										CASE_CONVERT:
																										  {
																										    tree _q220 = gimple_assign_rhs1 (_a10);
																										    _q220 = do_valueize (valueize, _q220);
																										    switch (TREE_CODE (_q220))
																										      {
																										      case SSA_NAME:
																										        if (gimple *_d11 = get_def (valueize, _q220))
																										          {
																											    if (gassign *_a11 = dyn_cast <gassign *> (_d11))
																											      switch (gimple_assign_rhs_code (_a11))
																												{
																												case LT_EXPR:
																												  {
																												    tree _q230 = gimple_assign_rhs1 (_a11);
																												    _q230 = do_valueize (valueize, _q230);
																												    tree _q231 = gimple_assign_rhs2 (_a11);
																												    _q231 = do_valueize (valueize, _q231);
																												    if ((_q230 == _q40 && ! TREE_SIDE_EFFECTS (_q230)) || (operand_equal_p (_q230, _q40, 0) && types_match (_q230, _q40)))
																												      {
																													if (integer_zerop (_q231))
																													  {
																													    if (gimple_max_value (_q181, valueize))
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
																															    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																					    }
																					}
																					if (gimple_max_value (_q180, valueize))
																					  {
																					    {
																					      tree _q181_pops[1];
																					      if (gimple_nop_convert (_q181, _q181_pops, valueize))
																					        {
																						  tree _q200 = _q181_pops[0];
																						  switch (TREE_CODE (_q200))
																						    {
																						    case SSA_NAME:
																						      if (gimple *_d9 = get_def (valueize, _q200))
																						        {
																							  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							    switch (gimple_assign_rhs_code (_a9))
																							      {
																							      case NEGATE_EXPR:
																							        {
																								  tree _q210 = gimple_assign_rhs1 (_a9);
																								  _q210 = do_valueize (valueize, _q210);
																								  {
																								    tree _q210_pops[1];
																								    if (gimple_nop_convert (_q210, _q210_pops, valueize))
																								      {
																									tree _q220 = _q210_pops[0];
																									switch (TREE_CODE (_q220))
																									  {
																									  case SSA_NAME:
																									    if (gimple *_d10 = get_def (valueize, _q220))
																									      {
																										if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																										  switch (gimple_assign_rhs_code (_a10))
																										    {
																										    CASE_CONVERT:
																										      {
																											tree _q230 = gimple_assign_rhs1 (_a10);
																											_q230 = do_valueize (valueize, _q230);
																											switch (TREE_CODE (_q230))
																											  {
																											  case SSA_NAME:
																											    if (gimple *_d11 = get_def (valueize, _q230))
																											      {
																												if (gassign *_a11 = dyn_cast <gassign *> (_d11))
																												  switch (gimple_assign_rhs_code (_a11))
																												    {
																												    case LT_EXPR:
																												      {
																													tree _q240 = gimple_assign_rhs1 (_a11);
																													_q240 = do_valueize (valueize, _q240);
																													tree _q241 = gimple_assign_rhs2 (_a11);
																													_q241 = do_valueize (valueize, _q241);
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
																															    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																											    break;
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
													          default:;
													          }
													      }
													  }
												        }
												      if ((_q90 == _q41 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q41, 0) && types_match (_q90, _q41)))
													{
													  {
													    tree _q91_pops[1];
													    if (gimple_nop_convert (_q91, _q91_pops, valueize))
													      {
														tree _q110 = _q91_pops[0];
														switch (TREE_CODE (_q110))
														  {
														  case SSA_NAME:
														    if (gimple *_d7 = get_def (valueize, _q110))
														      {
															if (gassign *_a7 = dyn_cast <gassign *> (_d7))
															  switch (gimple_assign_rhs_code (_a7))
															    {
															    case MINUS_EXPR:
															      {
																tree _q120 = gimple_assign_rhs1 (_a7);
																_q120 = do_valueize (valueize, _q120);
																tree _q121 = gimple_assign_rhs2 (_a7);
																_q121 = do_valueize (valueize, _q121);
																{
																  tree _q120_pops[1];
																  if (gimple_nop_convert (_q120, _q120_pops, valueize))
																    {
																      tree _q130 = _q120_pops[0];
																      if ((_q130 == _q41 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q41, 0) && types_match (_q130, _q41)))
																	{
																	  {
																	    tree _q121_pops[1];
																	    if (gimple_nop_convert (_q121, _q121_pops, valueize))
																	      {
																		tree _q150 = _q121_pops[0];
																		if ((_q150 == _q40 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q40, 0) && types_match (_q150, _q40)))
																		  {
																		    if (integer_zerop (_q81))
																		      {
																			switch (TREE_CODE (_p1))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d8 = get_def (valueize, _p1))
																			      {
																				if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				  switch (gimple_assign_rhs_code (_a8))
																				    {
																				    case BIT_XOR_EXPR:
																				      {
																					tree _q180 = gimple_assign_rhs1 (_a8);
																					_q180 = do_valueize (valueize, _q180);
																					tree _q181 = gimple_assign_rhs2 (_a8);
																					_q181 = do_valueize (valueize, _q181);
																					if (tree_swap_operands_p (_q180, _q181))
																					  std::swap (_q180, _q181);
																					{
																					  tree _q180_pops[1];
																					  if (gimple_nop_convert (_q180, _q180_pops, valueize))
																					    {
																					      tree _q190 = _q180_pops[0];
																					      switch (TREE_CODE (_q190))
																					        {
																						case SSA_NAME:
																						  if (gimple *_d9 = get_def (valueize, _q190))
																						    {
																						      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						        switch (gimple_assign_rhs_code (_a9))
																							  {
																							  case NEGATE_EXPR:
																							    {
																							      tree _q200 = gimple_assign_rhs1 (_a9);
																							      _q200 = do_valueize (valueize, _q200);
																							      {
																								tree _q200_pops[1];
																								if (gimple_nop_convert (_q200, _q200_pops, valueize))
																								  {
																								    tree _q210 = _q200_pops[0];
																								    switch (TREE_CODE (_q210))
																								      {
																								      case SSA_NAME:
																								        if (gimple *_d10 = get_def (valueize, _q210))
																								          {
																									    if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																									      switch (gimple_assign_rhs_code (_a10))
																										{
																										CASE_CONVERT:
																										  {
																										    tree _q220 = gimple_assign_rhs1 (_a10);
																										    _q220 = do_valueize (valueize, _q220);
																										    switch (TREE_CODE (_q220))
																										      {
																										      case SSA_NAME:
																										        if (gimple *_d11 = get_def (valueize, _q220))
																										          {
																											    if (gassign *_a11 = dyn_cast <gassign *> (_d11))
																											      switch (gimple_assign_rhs_code (_a11))
																												{
																												case LT_EXPR:
																												  {
																												    tree _q230 = gimple_assign_rhs1 (_a11);
																												    _q230 = do_valueize (valueize, _q230);
																												    tree _q231 = gimple_assign_rhs2 (_a11);
																												    _q231 = do_valueize (valueize, _q231);
																												    if ((_q230 == _q41 && ! TREE_SIDE_EFFECTS (_q230)) || (operand_equal_p (_q230, _q41, 0) && types_match (_q230, _q41)))
																												      {
																													if (integer_zerop (_q231))
																													  {
																													    if (gimple_max_value (_q181, valueize))
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
																															    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																					    }
																					}
																					if (gimple_max_value (_q180, valueize))
																					  {
																					    {
																					      tree _q181_pops[1];
																					      if (gimple_nop_convert (_q181, _q181_pops, valueize))
																					        {
																						  tree _q200 = _q181_pops[0];
																						  switch (TREE_CODE (_q200))
																						    {
																						    case SSA_NAME:
																						      if (gimple *_d9 = get_def (valueize, _q200))
																						        {
																							  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							    switch (gimple_assign_rhs_code (_a9))
																							      {
																							      case NEGATE_EXPR:
																							        {
																								  tree _q210 = gimple_assign_rhs1 (_a9);
																								  _q210 = do_valueize (valueize, _q210);
																								  {
																								    tree _q210_pops[1];
																								    if (gimple_nop_convert (_q210, _q210_pops, valueize))
																								      {
																									tree _q220 = _q210_pops[0];
																									switch (TREE_CODE (_q220))
																									  {
																									  case SSA_NAME:
																									    if (gimple *_d10 = get_def (valueize, _q220))
																									      {
																										if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																										  switch (gimple_assign_rhs_code (_a10))
																										    {
																										    CASE_CONVERT:
																										      {
																											tree _q230 = gimple_assign_rhs1 (_a10);
																											_q230 = do_valueize (valueize, _q230);
																											switch (TREE_CODE (_q230))
																											  {
																											  case SSA_NAME:
																											    if (gimple *_d11 = get_def (valueize, _q230))
																											      {
																												if (gassign *_a11 = dyn_cast <gassign *> (_d11))
																												  switch (gimple_assign_rhs_code (_a11))
																												    {
																												    case LT_EXPR:
																												      {
																													tree _q240 = gimple_assign_rhs1 (_a11);
																													_q240 = do_valueize (valueize, _q240);
																													tree _q241 = gimple_assign_rhs2 (_a11);
																													_q241 = do_valueize (valueize, _q241);
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
																															    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																											    break;
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
											  break;
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
					        }
					      default:;
					      }
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
		    case LT_EXPR:
		      {
			tree _q20 = TREE_OPERAND (_p0, 0);
			if ((TREE_CODE (_q20) == SSA_NAME
			     || is_gimple_min_invariant (_q20)))
			  {
			    _q20 = do_valueize (valueize, _q20);
			    tree _q21 = TREE_OPERAND (_p0, 1);
			    if ((TREE_CODE (_q21) == SSA_NAME
			         || is_gimple_min_invariant (_q21)))
			      {
				_q21 = do_valueize (valueize, _q21);
				switch (TREE_CODE (_q20))
				  {
				  case SSA_NAME:
				    if (gimple *_d2 = get_def (valueize, _q20))
				      {
					if (gassign *_a2 = dyn_cast <gassign *> (_d2))
					  switch (gimple_assign_rhs_code (_a2))
					    {
					    case BIT_AND_EXPR:
					      {
						tree _q30 = gimple_assign_rhs1 (_a2);
						_q30 = do_valueize (valueize, _q30);
						tree _q31 = gimple_assign_rhs2 (_a2);
						_q31 = do_valueize (valueize, _q31);
						if (tree_swap_operands_p (_q30, _q31))
						  std::swap (_q30, _q31);
						switch (TREE_CODE (_q30))
						  {
						  case SSA_NAME:
						    if (gimple *_d3 = get_def (valueize, _q30))
						      {
							if (gassign *_a3 = dyn_cast <gassign *> (_d3))
							  switch (gimple_assign_rhs_code (_a3))
							    {
							    case BIT_XOR_EXPR:
							      {
								tree _q40 = gimple_assign_rhs1 (_a3);
								_q40 = do_valueize (valueize, _q40);
								tree _q41 = gimple_assign_rhs2 (_a3);
								_q41 = do_valueize (valueize, _q41);
								if (tree_swap_operands_p (_q40, _q41))
								  std::swap (_q40, _q41);
								switch (TREE_CODE (_q31))
								  {
								  case SSA_NAME:
								    if (gimple *_d4 = get_def (valueize, _q31))
								      {
									if (gassign *_a4 = dyn_cast <gassign *> (_d4))
									  switch (gimple_assign_rhs_code (_a4))
									    {
									    case BIT_XOR_EXPR:
									      {
										tree _q70 = gimple_assign_rhs1 (_a4);
										_q70 = do_valueize (valueize, _q70);
										tree _q71 = gimple_assign_rhs2 (_a4);
										_q71 = do_valueize (valueize, _q71);
										if (tree_swap_operands_p (_q70, _q71))
										  std::swap (_q70, _q71);
										if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
										  {
										    {
										      tree _q71_pops[1];
										      if (gimple_nop_convert (_q71, _q71_pops, valueize))
										        {
											  tree _q90 = _q71_pops[0];
											  switch (TREE_CODE (_q90))
											    {
											    case SSA_NAME:
											      if (gimple *_d5 = get_def (valueize, _q90))
											        {
												  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
												    switch (gimple_assign_rhs_code (_a5))
												      {
												      case MINUS_EXPR:
												        {
													  tree _q100 = gimple_assign_rhs1 (_a5);
													  _q100 = do_valueize (valueize, _q100);
													  tree _q101 = gimple_assign_rhs2 (_a5);
													  _q101 = do_valueize (valueize, _q101);
													  {
													    tree _q100_pops[1];
													    if (gimple_nop_convert (_q100, _q100_pops, valueize))
													      {
														tree _q110 = _q100_pops[0];
														if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
														  {
														    {
														      tree _q101_pops[1];
														      if (gimple_nop_convert (_q101, _q101_pops, valueize))
														        {
															  tree _q130 = _q101_pops[0];
															  if ((_q130 == _q41 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q41, 0) && types_match (_q130, _q41)))
															    {
															      if (integer_zerop (_q21))
																{
																  switch (TREE_CODE (_p1))
																    {
																    case SSA_NAME:
																      if (gimple *_d6 = get_def (valueize, _p1))
																        {
																	  if (gassign *_a6 = dyn_cast <gassign *> (_d6))
																	    switch (gimple_assign_rhs_code (_a6))
																	      {
																	      case BIT_XOR_EXPR:
																	        {
																		  tree _q160 = gimple_assign_rhs1 (_a6);
																		  _q160 = do_valueize (valueize, _q160);
																		  tree _q161 = gimple_assign_rhs2 (_a6);
																		  _q161 = do_valueize (valueize, _q161);
																		  if (tree_swap_operands_p (_q160, _q161))
																		    std::swap (_q160, _q161);
																		  switch (TREE_CODE (_q160))
																		    {
																		    case SSA_NAME:
																		      if (gimple *_d7 = get_def (valueize, _q160))
																		        {
																			  if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			    switch (gimple_assign_rhs_code (_a7))
																			      {
																			      case NEGATE_EXPR:
																			        {
																				  tree _q170 = gimple_assign_rhs1 (_a7);
																				  _q170 = do_valueize (valueize, _q170);
																				  switch (TREE_CODE (_q170))
																				    {
																				    case SSA_NAME:
																				      if (gimple *_d8 = get_def (valueize, _q170))
																				        {
																					  if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					    switch (gimple_assign_rhs_code (_a8))
																					      {
																					      CASE_CONVERT:
																					        {
																						  tree _q180 = gimple_assign_rhs1 (_a8);
																						  _q180 = do_valueize (valueize, _q180);
																						  switch (TREE_CODE (_q180))
																						    {
																						    case SSA_NAME:
																						      if (gimple *_d9 = get_def (valueize, _q180))
																						        {
																							  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							    switch (gimple_assign_rhs_code (_a9))
																							      {
																							      case LT_EXPR:
																							        {
																								  tree _q190 = gimple_assign_rhs1 (_a9);
																								  _q190 = do_valueize (valueize, _q190);
																								  tree _q191 = gimple_assign_rhs2 (_a9);
																								  _q191 = do_valueize (valueize, _q191);
																								  if ((_q190 == _q40 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q40, 0) && types_match (_q190, _q40)))
																								    {
																								      if (integer_zerop (_q191))
																									{
																									  if (gimple_max_value (_q161, valueize))
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																		if (gimple_max_value (_q160, valueize))
																		  {
																		    switch (TREE_CODE (_q161))
																		      {
																		      case SSA_NAME:
																		        if (gimple *_d7 = get_def (valueize, _q161))
																		          {
																			    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			      switch (gimple_assign_rhs_code (_a7))
																				{
																				case NEGATE_EXPR:
																				  {
																				    tree _q180 = gimple_assign_rhs1 (_a7);
																				    _q180 = do_valueize (valueize, _q180);
																				    switch (TREE_CODE (_q180))
																				      {
																				      case SSA_NAME:
																				        if (gimple *_d8 = get_def (valueize, _q180))
																				          {
																					    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					      switch (gimple_assign_rhs_code (_a8))
																						{
																						CASE_CONVERT:
																						  {
																						    tree _q190 = gimple_assign_rhs1 (_a8);
																						    _q190 = do_valueize (valueize, _q190);
																						    switch (TREE_CODE (_q190))
																						      {
																						      case SSA_NAME:
																						        if (gimple *_d9 = get_def (valueize, _q190))
																						          {
																							    if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							      switch (gimple_assign_rhs_code (_a9))
																								{
																								case LT_EXPR:
																								  {
																								    tree _q200 = gimple_assign_rhs1 (_a9);
																								    _q200 = do_valueize (valueize, _q200);
																								    tree _q201 = gimple_assign_rhs2 (_a9);
																								    _q201 = do_valueize (valueize, _q201);
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
																											if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																						        break;
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
																				  }
																				default:;
																				}
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
											    default:;
											    }
										        }
										    }
										  }
										if ((_q70 == _q41 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q41, 0) && types_match (_q70, _q41)))
										  {
										    {
										      tree _q71_pops[1];
										      if (gimple_nop_convert (_q71, _q71_pops, valueize))
										        {
											  tree _q90 = _q71_pops[0];
											  switch (TREE_CODE (_q90))
											    {
											    case SSA_NAME:
											      if (gimple *_d5 = get_def (valueize, _q90))
											        {
												  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
												    switch (gimple_assign_rhs_code (_a5))
												      {
												      case MINUS_EXPR:
												        {
													  tree _q100 = gimple_assign_rhs1 (_a5);
													  _q100 = do_valueize (valueize, _q100);
													  tree _q101 = gimple_assign_rhs2 (_a5);
													  _q101 = do_valueize (valueize, _q101);
													  {
													    tree _q100_pops[1];
													    if (gimple_nop_convert (_q100, _q100_pops, valueize))
													      {
														tree _q110 = _q100_pops[0];
														if ((_q110 == _q41 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q41, 0) && types_match (_q110, _q41)))
														  {
														    {
														      tree _q101_pops[1];
														      if (gimple_nop_convert (_q101, _q101_pops, valueize))
														        {
															  tree _q130 = _q101_pops[0];
															  if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
															    {
															      if (integer_zerop (_q21))
																{
																  switch (TREE_CODE (_p1))
																    {
																    case SSA_NAME:
																      if (gimple *_d6 = get_def (valueize, _p1))
																        {
																	  if (gassign *_a6 = dyn_cast <gassign *> (_d6))
																	    switch (gimple_assign_rhs_code (_a6))
																	      {
																	      case BIT_XOR_EXPR:
																	        {
																		  tree _q160 = gimple_assign_rhs1 (_a6);
																		  _q160 = do_valueize (valueize, _q160);
																		  tree _q161 = gimple_assign_rhs2 (_a6);
																		  _q161 = do_valueize (valueize, _q161);
																		  if (tree_swap_operands_p (_q160, _q161))
																		    std::swap (_q160, _q161);
																		  switch (TREE_CODE (_q160))
																		    {
																		    case SSA_NAME:
																		      if (gimple *_d7 = get_def (valueize, _q160))
																		        {
																			  if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			    switch (gimple_assign_rhs_code (_a7))
																			      {
																			      case NEGATE_EXPR:
																			        {
																				  tree _q170 = gimple_assign_rhs1 (_a7);
																				  _q170 = do_valueize (valueize, _q170);
																				  switch (TREE_CODE (_q170))
																				    {
																				    case SSA_NAME:
																				      if (gimple *_d8 = get_def (valueize, _q170))
																				        {
																					  if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					    switch (gimple_assign_rhs_code (_a8))
																					      {
																					      CASE_CONVERT:
																					        {
																						  tree _q180 = gimple_assign_rhs1 (_a8);
																						  _q180 = do_valueize (valueize, _q180);
																						  switch (TREE_CODE (_q180))
																						    {
																						    case SSA_NAME:
																						      if (gimple *_d9 = get_def (valueize, _q180))
																						        {
																							  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							    switch (gimple_assign_rhs_code (_a9))
																							      {
																							      case LT_EXPR:
																							        {
																								  tree _q190 = gimple_assign_rhs1 (_a9);
																								  _q190 = do_valueize (valueize, _q190);
																								  tree _q191 = gimple_assign_rhs2 (_a9);
																								  _q191 = do_valueize (valueize, _q191);
																								  if ((_q190 == _q41 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q41, 0) && types_match (_q190, _q41)))
																								    {
																								      if (integer_zerop (_q191))
																									{
																									  if (gimple_max_value (_q161, valueize))
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																		if (gimple_max_value (_q160, valueize))
																		  {
																		    switch (TREE_CODE (_q161))
																		      {
																		      case SSA_NAME:
																		        if (gimple *_d7 = get_def (valueize, _q161))
																		          {
																			    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			      switch (gimple_assign_rhs_code (_a7))
																				{
																				case NEGATE_EXPR:
																				  {
																				    tree _q180 = gimple_assign_rhs1 (_a7);
																				    _q180 = do_valueize (valueize, _q180);
																				    switch (TREE_CODE (_q180))
																				      {
																				      case SSA_NAME:
																				        if (gimple *_d8 = get_def (valueize, _q180))
																				          {
																					    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					      switch (gimple_assign_rhs_code (_a8))
																						{
																						CASE_CONVERT:
																						  {
																						    tree _q190 = gimple_assign_rhs1 (_a8);
																						    _q190 = do_valueize (valueize, _q190);
																						    switch (TREE_CODE (_q190))
																						      {
																						      case SSA_NAME:
																						        if (gimple *_d9 = get_def (valueize, _q190))
																						          {
																							    if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							      switch (gimple_assign_rhs_code (_a9))
																								{
																								case LT_EXPR:
																								  {
																								    tree _q200 = gimple_assign_rhs1 (_a9);
																								    _q200 = do_valueize (valueize, _q200);
																								    tree _q201 = gimple_assign_rhs2 (_a9);
																								    _q201 = do_valueize (valueize, _q201);
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
																											if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																						        break;
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
																				  }
																				default:;
																				}
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
								    break;
							          default:;
							          }
								{
								  tree _q41_pops[1];
								  if (gimple_nop_convert (_q41, _q41_pops, valueize))
								    {
								      tree _q60 = _q41_pops[0];
								      switch (TREE_CODE (_q60))
								        {
									case SSA_NAME:
									  if (gimple *_d4 = get_def (valueize, _q60))
									    {
									      if (gassign *_a4 = dyn_cast <gassign *> (_d4))
									        switch (gimple_assign_rhs_code (_a4))
										  {
										  case MINUS_EXPR:
										    {
										      tree _q70 = gimple_assign_rhs1 (_a4);
										      _q70 = do_valueize (valueize, _q70);
										      tree _q71 = gimple_assign_rhs2 (_a4);
										      _q71 = do_valueize (valueize, _q71);
										      {
											tree _q70_pops[1];
											if (gimple_nop_convert (_q70, _q70_pops, valueize))
											  {
											    tree _q80 = _q70_pops[0];
											    if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
											      {
												{
												  tree _q71_pops[1];
												  if (gimple_nop_convert (_q71, _q71_pops, valueize))
												    {
												      tree _q100 = _q71_pops[0];
												      switch (TREE_CODE (_q31))
												        {
													case SSA_NAME:
													  if (gimple *_d5 = get_def (valueize, _q31))
													    {
													      if (gassign *_a5 = dyn_cast <gassign *> (_d5))
													        switch (gimple_assign_rhs_code (_a5))
														  {
														  case BIT_XOR_EXPR:
														    {
														      tree _q120 = gimple_assign_rhs1 (_a5);
														      _q120 = do_valueize (valueize, _q120);
														      tree _q121 = gimple_assign_rhs2 (_a5);
														      _q121 = do_valueize (valueize, _q121);
														      if (tree_swap_operands_p (_q120, _q121))
														        std::swap (_q120, _q121);
														      if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
															{
															  if ((_q121 == _q100 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q100, 0) && types_match (_q121, _q100)))
															    {
															      if (integer_zerop (_q21))
																{
																  switch (TREE_CODE (_p1))
																    {
																    case SSA_NAME:
																      if (gimple *_d6 = get_def (valueize, _p1))
																        {
																	  if (gassign *_a6 = dyn_cast <gassign *> (_d6))
																	    switch (gimple_assign_rhs_code (_a6))
																	      {
																	      case BIT_XOR_EXPR:
																	        {
																		  tree _q160 = gimple_assign_rhs1 (_a6);
																		  _q160 = do_valueize (valueize, _q160);
																		  tree _q161 = gimple_assign_rhs2 (_a6);
																		  _q161 = do_valueize (valueize, _q161);
																		  if (tree_swap_operands_p (_q160, _q161))
																		    std::swap (_q160, _q161);
																		  switch (TREE_CODE (_q160))
																		    {
																		    case SSA_NAME:
																		      if (gimple *_d7 = get_def (valueize, _q160))
																		        {
																			  if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			    switch (gimple_assign_rhs_code (_a7))
																			      {
																			      case NEGATE_EXPR:
																			        {
																				  tree _q170 = gimple_assign_rhs1 (_a7);
																				  _q170 = do_valueize (valueize, _q170);
																				  switch (TREE_CODE (_q170))
																				    {
																				    case SSA_NAME:
																				      if (gimple *_d8 = get_def (valueize, _q170))
																				        {
																					  if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					    switch (gimple_assign_rhs_code (_a8))
																					      {
																					      CASE_CONVERT:
																					        {
																						  tree _q180 = gimple_assign_rhs1 (_a8);
																						  _q180 = do_valueize (valueize, _q180);
																						  switch (TREE_CODE (_q180))
																						    {
																						    case SSA_NAME:
																						      if (gimple *_d9 = get_def (valueize, _q180))
																						        {
																							  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							    switch (gimple_assign_rhs_code (_a9))
																							      {
																							      case LT_EXPR:
																							        {
																								  tree _q190 = gimple_assign_rhs1 (_a9);
																								  _q190 = do_valueize (valueize, _q190);
																								  tree _q191 = gimple_assign_rhs2 (_a9);
																								  _q191 = do_valueize (valueize, _q191);
																								  if ((_q190 == _q40 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q40, 0) && types_match (_q190, _q40)))
																								    {
																								      if (integer_zerop (_q191))
																									{
																									  if (gimple_max_value (_q161, valueize))
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																		if (gimple_max_value (_q160, valueize))
																		  {
																		    switch (TREE_CODE (_q161))
																		      {
																		      case SSA_NAME:
																		        if (gimple *_d7 = get_def (valueize, _q161))
																		          {
																			    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			      switch (gimple_assign_rhs_code (_a7))
																				{
																				case NEGATE_EXPR:
																				  {
																				    tree _q180 = gimple_assign_rhs1 (_a7);
																				    _q180 = do_valueize (valueize, _q180);
																				    switch (TREE_CODE (_q180))
																				      {
																				      case SSA_NAME:
																				        if (gimple *_d8 = get_def (valueize, _q180))
																				          {
																					    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					      switch (gimple_assign_rhs_code (_a8))
																						{
																						CASE_CONVERT:
																						  {
																						    tree _q190 = gimple_assign_rhs1 (_a8);
																						    _q190 = do_valueize (valueize, _q190);
																						    switch (TREE_CODE (_q190))
																						      {
																						      case SSA_NAME:
																						        if (gimple *_d9 = get_def (valueize, _q190))
																						          {
																							    if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							      switch (gimple_assign_rhs_code (_a9))
																								{
																								case LT_EXPR:
																								  {
																								    tree _q200 = gimple_assign_rhs1 (_a9);
																								    _q200 = do_valueize (valueize, _q200);
																								    tree _q201 = gimple_assign_rhs2 (_a9);
																								    _q201 = do_valueize (valueize, _q201);
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
																											if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																						        break;
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
																				  }
																				default:;
																				}
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
																    case SSA_NAME:
																      if (gimple *_d6 = get_def (valueize, _p1))
																        {
																	  if (gassign *_a6 = dyn_cast <gassign *> (_d6))
																	    switch (gimple_assign_rhs_code (_a6))
																	      {
																	      case BIT_XOR_EXPR:
																	        {
																		  tree _q160 = gimple_assign_rhs1 (_a6);
																		  _q160 = do_valueize (valueize, _q160);
																		  tree _q161 = gimple_assign_rhs2 (_a6);
																		  _q161 = do_valueize (valueize, _q161);
																		  if (tree_swap_operands_p (_q160, _q161))
																		    std::swap (_q160, _q161);
																		  switch (TREE_CODE (_q160))
																		    {
																		    case SSA_NAME:
																		      if (gimple *_d7 = get_def (valueize, _q160))
																		        {
																			  if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			    switch (gimple_assign_rhs_code (_a7))
																			      {
																			      case NEGATE_EXPR:
																			        {
																				  tree _q170 = gimple_assign_rhs1 (_a7);
																				  _q170 = do_valueize (valueize, _q170);
																				  switch (TREE_CODE (_q170))
																				    {
																				    case SSA_NAME:
																				      if (gimple *_d8 = get_def (valueize, _q170))
																				        {
																					  if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					    switch (gimple_assign_rhs_code (_a8))
																					      {
																					      CASE_CONVERT:
																					        {
																						  tree _q180 = gimple_assign_rhs1 (_a8);
																						  _q180 = do_valueize (valueize, _q180);
																						  switch (TREE_CODE (_q180))
																						    {
																						    case SSA_NAME:
																						      if (gimple *_d9 = get_def (valueize, _q180))
																						        {
																							  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							    switch (gimple_assign_rhs_code (_a9))
																							      {
																							      case LT_EXPR:
																							        {
																								  tree _q190 = gimple_assign_rhs1 (_a9);
																								  _q190 = do_valueize (valueize, _q190);
																								  tree _q191 = gimple_assign_rhs2 (_a9);
																								  _q191 = do_valueize (valueize, _q191);
																								  if ((_q190 == _q40 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q40, 0) && types_match (_q190, _q40)))
																								    {
																								      if (integer_zerop (_q191))
																									{
																									  if (gimple_max_value (_q161, valueize))
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																		if (gimple_max_value (_q160, valueize))
																		  {
																		    switch (TREE_CODE (_q161))
																		      {
																		      case SSA_NAME:
																		        if (gimple *_d7 = get_def (valueize, _q161))
																		          {
																			    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			      switch (gimple_assign_rhs_code (_a7))
																				{
																				case NEGATE_EXPR:
																				  {
																				    tree _q180 = gimple_assign_rhs1 (_a7);
																				    _q180 = do_valueize (valueize, _q180);
																				    switch (TREE_CODE (_q180))
																				      {
																				      case SSA_NAME:
																				        if (gimple *_d8 = get_def (valueize, _q180))
																				          {
																					    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					      switch (gimple_assign_rhs_code (_a8))
																						{
																						CASE_CONVERT:
																						  {
																						    tree _q190 = gimple_assign_rhs1 (_a8);
																						    _q190 = do_valueize (valueize, _q190);
																						    switch (TREE_CODE (_q190))
																						      {
																						      case SSA_NAME:
																						        if (gimple *_d9 = get_def (valueize, _q190))
																						          {
																							    if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							      switch (gimple_assign_rhs_code (_a9))
																								{
																								case LT_EXPR:
																								  {
																								    tree _q200 = gimple_assign_rhs1 (_a9);
																								    _q200 = do_valueize (valueize, _q200);
																								    tree _q201 = gimple_assign_rhs2 (_a9);
																								    _q201 = do_valueize (valueize, _q201);
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
																											if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																						        break;
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
																				  }
																				default:;
																				}
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
					      }
					    default:;
					    }
				      }
				    break;
			          default:;
			          }
			      }
			  }
		        break;
		      }
		    case GE_EXPR:
		      {
			tree _q20 = TREE_OPERAND (_p0, 0);
			if ((TREE_CODE (_q20) == SSA_NAME
			     || is_gimple_min_invariant (_q20)))
			  {
			    _q20 = do_valueize (valueize, _q20);
			    tree _q21 = TREE_OPERAND (_p0, 1);
			    if ((TREE_CODE (_q21) == SSA_NAME
			         || is_gimple_min_invariant (_q21)))
			      {
				_q21 = do_valueize (valueize, _q21);
				switch (TREE_CODE (_q20))
				  {
				  case SSA_NAME:
				    if (gimple *_d2 = get_def (valueize, _q20))
				      {
					if (gassign *_a2 = dyn_cast <gassign *> (_d2))
					  switch (gimple_assign_rhs_code (_a2))
					    {
					    case BIT_AND_EXPR:
					      {
						tree _q30 = gimple_assign_rhs1 (_a2);
						_q30 = do_valueize (valueize, _q30);
						tree _q31 = gimple_assign_rhs2 (_a2);
						_q31 = do_valueize (valueize, _q31);
						if (tree_swap_operands_p (_q30, _q31))
						  std::swap (_q30, _q31);
						switch (TREE_CODE (_q30))
						  {
						  case SSA_NAME:
						    if (gimple *_d3 = get_def (valueize, _q30))
						      {
							if (gassign *_a3 = dyn_cast <gassign *> (_d3))
							  switch (gimple_assign_rhs_code (_a3))
							    {
							    case BIT_XOR_EXPR:
							      {
								tree _q40 = gimple_assign_rhs1 (_a3);
								_q40 = do_valueize (valueize, _q40);
								tree _q41 = gimple_assign_rhs2 (_a3);
								_q41 = do_valueize (valueize, _q41);
								if (tree_swap_operands_p (_q40, _q41))
								  std::swap (_q40, _q41);
								switch (TREE_CODE (_q31))
								  {
								  case SSA_NAME:
								    if (gimple *_d4 = get_def (valueize, _q31))
								      {
									if (gassign *_a4 = dyn_cast <gassign *> (_d4))
									  switch (gimple_assign_rhs_code (_a4))
									    {
									    case BIT_XOR_EXPR:
									      {
										tree _q70 = gimple_assign_rhs1 (_a4);
										_q70 = do_valueize (valueize, _q70);
										tree _q71 = gimple_assign_rhs2 (_a4);
										_q71 = do_valueize (valueize, _q71);
										if (tree_swap_operands_p (_q70, _q71))
										  std::swap (_q70, _q71);
										if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
										  {
										    {
										      tree _q71_pops[1];
										      if (gimple_nop_convert (_q71, _q71_pops, valueize))
										        {
											  tree _q90 = _q71_pops[0];
											  switch (TREE_CODE (_q90))
											    {
											    case SSA_NAME:
											      if (gimple *_d5 = get_def (valueize, _q90))
											        {
												  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
												    switch (gimple_assign_rhs_code (_a5))
												      {
												      case MINUS_EXPR:
												        {
													  tree _q100 = gimple_assign_rhs1 (_a5);
													  _q100 = do_valueize (valueize, _q100);
													  tree _q101 = gimple_assign_rhs2 (_a5);
													  _q101 = do_valueize (valueize, _q101);
													  {
													    tree _q100_pops[1];
													    if (gimple_nop_convert (_q100, _q100_pops, valueize))
													      {
														tree _q110 = _q100_pops[0];
														if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
														  {
														    {
														      tree _q101_pops[1];
														      if (gimple_nop_convert (_q101, _q101_pops, valueize))
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
																	case SSA_NAME:
																	  if (gimple *_d6 = get_def (valueize, _p2))
																	    {
																	      if (gassign *_a6 = dyn_cast <gassign *> (_d6))
																	        switch (gimple_assign_rhs_code (_a6))
																		  {
																		  case BIT_XOR_EXPR:
																		    {
																		      tree _q170 = gimple_assign_rhs1 (_a6);
																		      _q170 = do_valueize (valueize, _q170);
																		      tree _q171 = gimple_assign_rhs2 (_a6);
																		      _q171 = do_valueize (valueize, _q171);
																		      if (tree_swap_operands_p (_q170, _q171))
																		        std::swap (_q170, _q171);
																		      switch (TREE_CODE (_q170))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d7 = get_def (valueize, _q170))
																			    {
																			      if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			        switch (gimple_assign_rhs_code (_a7))
																				  {
																				  case NEGATE_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a7);
																				      _q180 = do_valueize (valueize, _q180);
																				      switch (TREE_CODE (_q180))
																				        {
																					case SSA_NAME:
																					  if (gimple *_d8 = get_def (valueize, _q180))
																					    {
																					      if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					        switch (gimple_assign_rhs_code (_a8))
																						  {
																						  CASE_CONVERT:
																						    {
																						      tree _q190 = gimple_assign_rhs1 (_a8);
																						      _q190 = do_valueize (valueize, _q190);
																						      switch (TREE_CODE (_q190))
																						        {
																							case SSA_NAME:
																							  if (gimple *_d9 = get_def (valueize, _q190))
																							    {
																							      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							        switch (gimple_assign_rhs_code (_a9))
																								  {
																								  case LT_EXPR:
																								    {
																								      tree _q200 = gimple_assign_rhs1 (_a9);
																								      _q200 = do_valueize (valueize, _q200);
																								      tree _q201 = gimple_assign_rhs2 (_a9);
																								      _q201 = do_valueize (valueize, _q201);
																								      if ((_q200 == _q40 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q40, 0) && types_match (_q200, _q40)))
																									{
																									  if (integer_zerop (_q201))
																									    {
																									      if (gimple_max_value (_q171, valueize))
																										{
																										  {
																										    tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _q71 };
																										    if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																										      {
																											{
																											  res_ops[0] = captures[0];
																											  res_ops[1] = captures[1];
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																							  break;
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
																				    }
																				  default:;
																				  }
																			    }
																			  break;
																		        default:;
																		        }
																		    if (gimple_max_value (_q170, valueize))
																		      {
																			switch (TREE_CODE (_q171))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d7 = get_def (valueize, _q171))
																			      {
																				if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																				  switch (gimple_assign_rhs_code (_a7))
																				    {
																				    case NEGATE_EXPR:
																				      {
																					tree _q190 = gimple_assign_rhs1 (_a7);
																					_q190 = do_valueize (valueize, _q190);
																					switch (TREE_CODE (_q190))
																					  {
																					  case SSA_NAME:
																					    if (gimple *_d8 = get_def (valueize, _q190))
																					      {
																						if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																						  switch (gimple_assign_rhs_code (_a8))
																						    {
																						    CASE_CONVERT:
																						      {
																							tree _q200 = gimple_assign_rhs1 (_a8);
																							_q200 = do_valueize (valueize, _q200);
																							switch (TREE_CODE (_q200))
																							  {
																							  case SSA_NAME:
																							    if (gimple *_d9 = get_def (valueize, _q200))
																							      {
																								if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																								  switch (gimple_assign_rhs_code (_a9))
																								    {
																								    case LT_EXPR:
																								      {
																									tree _q210 = gimple_assign_rhs1 (_a9);
																									_q210 = do_valueize (valueize, _q210);
																									tree _q211 = gimple_assign_rhs2 (_a9);
																									_q211 = do_valueize (valueize, _q211);
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
																											if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																				      }
																				    default:;
																				    }
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
											    default:;
											    }
										        }
										    }
										  }
										if ((_q70 == _q41 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q41, 0) && types_match (_q70, _q41)))
										  {
										    {
										      tree _q71_pops[1];
										      if (gimple_nop_convert (_q71, _q71_pops, valueize))
										        {
											  tree _q90 = _q71_pops[0];
											  switch (TREE_CODE (_q90))
											    {
											    case SSA_NAME:
											      if (gimple *_d5 = get_def (valueize, _q90))
											        {
												  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
												    switch (gimple_assign_rhs_code (_a5))
												      {
												      case MINUS_EXPR:
												        {
													  tree _q100 = gimple_assign_rhs1 (_a5);
													  _q100 = do_valueize (valueize, _q100);
													  tree _q101 = gimple_assign_rhs2 (_a5);
													  _q101 = do_valueize (valueize, _q101);
													  {
													    tree _q100_pops[1];
													    if (gimple_nop_convert (_q100, _q100_pops, valueize))
													      {
														tree _q110 = _q100_pops[0];
														if ((_q110 == _q41 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q41, 0) && types_match (_q110, _q41)))
														  {
														    {
														      tree _q101_pops[1];
														      if (gimple_nop_convert (_q101, _q101_pops, valueize))
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
																	case SSA_NAME:
																	  if (gimple *_d6 = get_def (valueize, _p2))
																	    {
																	      if (gassign *_a6 = dyn_cast <gassign *> (_d6))
																	        switch (gimple_assign_rhs_code (_a6))
																		  {
																		  case BIT_XOR_EXPR:
																		    {
																		      tree _q170 = gimple_assign_rhs1 (_a6);
																		      _q170 = do_valueize (valueize, _q170);
																		      tree _q171 = gimple_assign_rhs2 (_a6);
																		      _q171 = do_valueize (valueize, _q171);
																		      if (tree_swap_operands_p (_q170, _q171))
																		        std::swap (_q170, _q171);
																		      switch (TREE_CODE (_q170))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d7 = get_def (valueize, _q170))
																			    {
																			      if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			        switch (gimple_assign_rhs_code (_a7))
																				  {
																				  case NEGATE_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a7);
																				      _q180 = do_valueize (valueize, _q180);
																				      switch (TREE_CODE (_q180))
																				        {
																					case SSA_NAME:
																					  if (gimple *_d8 = get_def (valueize, _q180))
																					    {
																					      if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					        switch (gimple_assign_rhs_code (_a8))
																						  {
																						  CASE_CONVERT:
																						    {
																						      tree _q190 = gimple_assign_rhs1 (_a8);
																						      _q190 = do_valueize (valueize, _q190);
																						      switch (TREE_CODE (_q190))
																						        {
																							case SSA_NAME:
																							  if (gimple *_d9 = get_def (valueize, _q190))
																							    {
																							      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							        switch (gimple_assign_rhs_code (_a9))
																								  {
																								  case LT_EXPR:
																								    {
																								      tree _q200 = gimple_assign_rhs1 (_a9);
																								      _q200 = do_valueize (valueize, _q200);
																								      tree _q201 = gimple_assign_rhs2 (_a9);
																								      _q201 = do_valueize (valueize, _q201);
																								      if ((_q200 == _q41 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q41, 0) && types_match (_q200, _q41)))
																									{
																									  if (integer_zerop (_q201))
																									    {
																									      if (gimple_max_value (_q171, valueize))
																										{
																										  {
																										    tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _q71 };
																										    if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																										      {
																											{
																											  res_ops[0] = captures[0];
																											  res_ops[1] = captures[1];
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																							  break;
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
																				    }
																				  default:;
																				  }
																			    }
																			  break;
																		        default:;
																		        }
																		    if (gimple_max_value (_q170, valueize))
																		      {
																			switch (TREE_CODE (_q171))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d7 = get_def (valueize, _q171))
																			      {
																				if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																				  switch (gimple_assign_rhs_code (_a7))
																				    {
																				    case NEGATE_EXPR:
																				      {
																					tree _q190 = gimple_assign_rhs1 (_a7);
																					_q190 = do_valueize (valueize, _q190);
																					switch (TREE_CODE (_q190))
																					  {
																					  case SSA_NAME:
																					    if (gimple *_d8 = get_def (valueize, _q190))
																					      {
																						if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																						  switch (gimple_assign_rhs_code (_a8))
																						    {
																						    CASE_CONVERT:
																						      {
																							tree _q200 = gimple_assign_rhs1 (_a8);
																							_q200 = do_valueize (valueize, _q200);
																							switch (TREE_CODE (_q200))
																							  {
																							  case SSA_NAME:
																							    if (gimple *_d9 = get_def (valueize, _q200))
																							      {
																								if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																								  switch (gimple_assign_rhs_code (_a9))
																								    {
																								    case LT_EXPR:
																								      {
																									tree _q210 = gimple_assign_rhs1 (_a9);
																									_q210 = do_valueize (valueize, _q210);
																									tree _q211 = gimple_assign_rhs2 (_a9);
																									_q211 = do_valueize (valueize, _q211);
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
																											if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																				      }
																				    default:;
																				    }
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
								    break;
							          default:;
							          }
								{
								  tree _q41_pops[1];
								  if (gimple_nop_convert (_q41, _q41_pops, valueize))
								    {
								      tree _q60 = _q41_pops[0];
								      switch (TREE_CODE (_q60))
								        {
									case SSA_NAME:
									  if (gimple *_d4 = get_def (valueize, _q60))
									    {
									      if (gassign *_a4 = dyn_cast <gassign *> (_d4))
									        switch (gimple_assign_rhs_code (_a4))
										  {
										  case MINUS_EXPR:
										    {
										      tree _q70 = gimple_assign_rhs1 (_a4);
										      _q70 = do_valueize (valueize, _q70);
										      tree _q71 = gimple_assign_rhs2 (_a4);
										      _q71 = do_valueize (valueize, _q71);
										      {
											tree _q70_pops[1];
											if (gimple_nop_convert (_q70, _q70_pops, valueize))
											  {
											    tree _q80 = _q70_pops[0];
											    if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
											      {
												{
												  tree _q71_pops[1];
												  if (gimple_nop_convert (_q71, _q71_pops, valueize))
												    {
												      tree _q100 = _q71_pops[0];
												      switch (TREE_CODE (_q31))
												        {
													case SSA_NAME:
													  if (gimple *_d5 = get_def (valueize, _q31))
													    {
													      if (gassign *_a5 = dyn_cast <gassign *> (_d5))
													        switch (gimple_assign_rhs_code (_a5))
														  {
														  case BIT_XOR_EXPR:
														    {
														      tree _q120 = gimple_assign_rhs1 (_a5);
														      _q120 = do_valueize (valueize, _q120);
														      tree _q121 = gimple_assign_rhs2 (_a5);
														      _q121 = do_valueize (valueize, _q121);
														      if (tree_swap_operands_p (_q120, _q121))
														        std::swap (_q120, _q121);
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
																	case SSA_NAME:
																	  if (gimple *_d6 = get_def (valueize, _p2))
																	    {
																	      if (gassign *_a6 = dyn_cast <gassign *> (_d6))
																	        switch (gimple_assign_rhs_code (_a6))
																		  {
																		  case BIT_XOR_EXPR:
																		    {
																		      tree _q170 = gimple_assign_rhs1 (_a6);
																		      _q170 = do_valueize (valueize, _q170);
																		      tree _q171 = gimple_assign_rhs2 (_a6);
																		      _q171 = do_valueize (valueize, _q171);
																		      if (tree_swap_operands_p (_q170, _q171))
																		        std::swap (_q170, _q171);
																		      switch (TREE_CODE (_q170))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d7 = get_def (valueize, _q170))
																			    {
																			      if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			        switch (gimple_assign_rhs_code (_a7))
																				  {
																				  case NEGATE_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a7);
																				      _q180 = do_valueize (valueize, _q180);
																				      switch (TREE_CODE (_q180))
																				        {
																					case SSA_NAME:
																					  if (gimple *_d8 = get_def (valueize, _q180))
																					    {
																					      if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					        switch (gimple_assign_rhs_code (_a8))
																						  {
																						  CASE_CONVERT:
																						    {
																						      tree _q190 = gimple_assign_rhs1 (_a8);
																						      _q190 = do_valueize (valueize, _q190);
																						      switch (TREE_CODE (_q190))
																						        {
																							case SSA_NAME:
																							  if (gimple *_d9 = get_def (valueize, _q190))
																							    {
																							      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							        switch (gimple_assign_rhs_code (_a9))
																								  {
																								  case LT_EXPR:
																								    {
																								      tree _q200 = gimple_assign_rhs1 (_a9);
																								      _q200 = do_valueize (valueize, _q200);
																								      tree _q201 = gimple_assign_rhs2 (_a9);
																								      _q201 = do_valueize (valueize, _q201);
																								      if ((_q200 == _q40 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q40, 0) && types_match (_q200, _q40)))
																									{
																									  if (integer_zerop (_q201))
																									    {
																									      if (gimple_max_value (_q171, valueize))
																										{
																										  {
																										    tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
																										    if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																										      {
																											{
																											  res_ops[0] = captures[0];
																											  res_ops[1] = captures[1];
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																							  break;
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
																				    }
																				  default:;
																				  }
																			    }
																			  break;
																		        default:;
																		        }
																		    if (gimple_max_value (_q170, valueize))
																		      {
																			switch (TREE_CODE (_q171))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d7 = get_def (valueize, _q171))
																			      {
																				if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																				  switch (gimple_assign_rhs_code (_a7))
																				    {
																				    case NEGATE_EXPR:
																				      {
																					tree _q190 = gimple_assign_rhs1 (_a7);
																					_q190 = do_valueize (valueize, _q190);
																					switch (TREE_CODE (_q190))
																					  {
																					  case SSA_NAME:
																					    if (gimple *_d8 = get_def (valueize, _q190))
																					      {
																						if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																						  switch (gimple_assign_rhs_code (_a8))
																						    {
																						    CASE_CONVERT:
																						      {
																							tree _q200 = gimple_assign_rhs1 (_a8);
																							_q200 = do_valueize (valueize, _q200);
																							switch (TREE_CODE (_q200))
																							  {
																							  case SSA_NAME:
																							    if (gimple *_d9 = get_def (valueize, _q200))
																							      {
																								if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																								  switch (gimple_assign_rhs_code (_a9))
																								    {
																								    case LT_EXPR:
																								      {
																									tree _q210 = gimple_assign_rhs1 (_a9);
																									_q210 = do_valueize (valueize, _q210);
																									tree _q211 = gimple_assign_rhs2 (_a9);
																									_q211 = do_valueize (valueize, _q211);
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
																											if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																				      }
																				    default:;
																				    }
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
																	case SSA_NAME:
																	  if (gimple *_d6 = get_def (valueize, _p2))
																	    {
																	      if (gassign *_a6 = dyn_cast <gassign *> (_d6))
																	        switch (gimple_assign_rhs_code (_a6))
																		  {
																		  case BIT_XOR_EXPR:
																		    {
																		      tree _q170 = gimple_assign_rhs1 (_a6);
																		      _q170 = do_valueize (valueize, _q170);
																		      tree _q171 = gimple_assign_rhs2 (_a6);
																		      _q171 = do_valueize (valueize, _q171);
																		      if (tree_swap_operands_p (_q170, _q171))
																		        std::swap (_q170, _q171);
																		      switch (TREE_CODE (_q170))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d7 = get_def (valueize, _q170))
																			    {
																			      if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			        switch (gimple_assign_rhs_code (_a7))
																				  {
																				  case NEGATE_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a7);
																				      _q180 = do_valueize (valueize, _q180);
																				      switch (TREE_CODE (_q180))
																				        {
																					case SSA_NAME:
																					  if (gimple *_d8 = get_def (valueize, _q180))
																					    {
																					      if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					        switch (gimple_assign_rhs_code (_a8))
																						  {
																						  CASE_CONVERT:
																						    {
																						      tree _q190 = gimple_assign_rhs1 (_a8);
																						      _q190 = do_valueize (valueize, _q190);
																						      switch (TREE_CODE (_q190))
																						        {
																							case SSA_NAME:
																							  if (gimple *_d9 = get_def (valueize, _q190))
																							    {
																							      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							        switch (gimple_assign_rhs_code (_a9))
																								  {
																								  case LT_EXPR:
																								    {
																								      tree _q200 = gimple_assign_rhs1 (_a9);
																								      _q200 = do_valueize (valueize, _q200);
																								      tree _q201 = gimple_assign_rhs2 (_a9);
																								      _q201 = do_valueize (valueize, _q201);
																								      if ((_q200 == _q40 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q40, 0) && types_match (_q200, _q40)))
																									{
																									  if (integer_zerop (_q201))
																									    {
																									      if (gimple_max_value (_q171, valueize))
																										{
																										  {
																										    tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
																										    if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																										      {
																											{
																											  res_ops[0] = captures[0];
																											  res_ops[1] = captures[1];
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																							  break;
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
																				    }
																				  default:;
																				  }
																			    }
																			  break;
																		        default:;
																		        }
																		    if (gimple_max_value (_q170, valueize))
																		      {
																			switch (TREE_CODE (_q171))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d7 = get_def (valueize, _q171))
																			      {
																				if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																				  switch (gimple_assign_rhs_code (_a7))
																				    {
																				    case NEGATE_EXPR:
																				      {
																					tree _q190 = gimple_assign_rhs1 (_a7);
																					_q190 = do_valueize (valueize, _q190);
																					switch (TREE_CODE (_q190))
																					  {
																					  case SSA_NAME:
																					    if (gimple *_d8 = get_def (valueize, _q190))
																					      {
																						if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																						  switch (gimple_assign_rhs_code (_a8))
																						    {
																						    CASE_CONVERT:
																						      {
																							tree _q200 = gimple_assign_rhs1 (_a8);
																							_q200 = do_valueize (valueize, _q200);
																							switch (TREE_CODE (_q200))
																							  {
																							  case SSA_NAME:
																							    if (gimple *_d9 = get_def (valueize, _q200))
																							      {
																								if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																								  switch (gimple_assign_rhs_code (_a9))
																								    {
																								    case LT_EXPR:
																								      {
																									tree _q210 = gimple_assign_rhs1 (_a9);
																									_q210 = do_valueize (valueize, _q210);
																									tree _q211 = gimple_assign_rhs2 (_a9);
																									_q211 = do_valueize (valueize, _q211);
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
																											if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																				      }
																				    default:;
																				    }
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
									  break;
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
					      }
					    default:;
					    }
				      }
				    break;
			          default:;
			          }
			      }
			  }
		        break;
		      }
		    case NE_EXPR:
		      {
			tree _q20 = TREE_OPERAND (_p0, 0);
			if ((TREE_CODE (_q20) == SSA_NAME
			     || is_gimple_min_invariant (_q20)))
			  {
			    _q20 = do_valueize (valueize, _q20);
			    tree _q21 = TREE_OPERAND (_p0, 1);
			    if ((TREE_CODE (_q21) == SSA_NAME
			         || is_gimple_min_invariant (_q21)))
			      {
				_q21 = do_valueize (valueize, _q21);
				if (tree_swap_operands_p (_q20, _q21))
				  std::swap (_q20, _q21);
				switch (TREE_CODE (_q20))
				  {
				  case SSA_NAME:
				    if (gimple *_d2 = get_def (valueize, _q20))
				      {
					if (gassign *_a2 = dyn_cast <gassign *> (_d2))
					  switch (gimple_assign_rhs_code (_a2))
					    {
					    case IMAGPART_EXPR:
					      {
						tree _q30 = TREE_OPERAND (gimple_assign_rhs1 (_a2), 0);
						if ((TREE_CODE (_q30) == SSA_NAME
						     || is_gimple_min_invariant (_q30)))
						  {
						    _q30 = do_valueize (valueize, _q30);
						    switch (TREE_CODE (_q30))
						      {
						      case SSA_NAME:
						        if (gimple *_d3 = get_def (valueize, _q30))
						          {
							    if (gcall *_c3 = dyn_cast <gcall *> (_d3))
							      switch (gimple_call_combined_fn (_c3))
								{
								case CFN_SUB_OVERFLOW:
								  if (gimple_call_num_args (_c3) == 2)
								    {
								      tree _q40 = gimple_call_arg (_c3, 0);
								      _q40 = do_valueize (valueize, _q40);
								      tree _q41 = gimple_call_arg (_c3, 1);
								      _q41 = do_valueize (valueize, _q41);
								      if (integer_zerop (_q21))
									{
									  switch (TREE_CODE (_p1))
									    {
									    case SSA_NAME:
									      if (gimple *_d4 = get_def (valueize, _p1))
									        {
										  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
										    switch (gimple_assign_rhs_code (_a4))
										      {
										      case BIT_XOR_EXPR:
										        {
											  tree _q80 = gimple_assign_rhs1 (_a4);
											  _q80 = do_valueize (valueize, _q80);
											  tree _q81 = gimple_assign_rhs2 (_a4);
											  _q81 = do_valueize (valueize, _q81);
											  if (tree_swap_operands_p (_q80, _q81))
											    std::swap (_q80, _q81);
											  switch (TREE_CODE (_q80))
											    {
											    case SSA_NAME:
											      if (gimple *_d5 = get_def (valueize, _q80))
											        {
												  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
												    switch (gimple_assign_rhs_code (_a5))
												      {
												      case NEGATE_EXPR:
												        {
													  tree _q90 = gimple_assign_rhs1 (_a5);
													  _q90 = do_valueize (valueize, _q90);
													  switch (TREE_CODE (_q90))
													    {
													    case SSA_NAME:
													      if (gimple *_d6 = get_def (valueize, _q90))
													        {
														  if (gassign *_a6 = dyn_cast <gassign *> (_d6))
														    switch (gimple_assign_rhs_code (_a6))
														      {
														      CASE_CONVERT:
														        {
															  tree _q100 = gimple_assign_rhs1 (_a6);
															  _q100 = do_valueize (valueize, _q100);
															  switch (TREE_CODE (_q100))
															    {
															    case SSA_NAME:
															      if (gimple *_d7 = get_def (valueize, _q100))
															        {
																  if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																    switch (gimple_assign_rhs_code (_a7))
																      {
																      case LT_EXPR:
																        {
																	  tree _q110 = gimple_assign_rhs1 (_a7);
																	  _q110 = do_valueize (valueize, _q110);
																	  tree _q111 = gimple_assign_rhs2 (_a7);
																	  _q111 = do_valueize (valueize, _q111);
																	  if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
																	    {
																	      if (integer_zerop (_q111))
																		{
																		  if (gimple_max_value (_q81, valueize))
																		    {
																		      switch (TREE_CODE (_p2))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d8 = get_def (valueize, _p2))
																			    {
																			      if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																			        switch (gimple_assign_rhs_code (_a8))
																				  {
																				  case REALPART_EXPR:
																				    {
																				      tree _q150 = TREE_OPERAND (gimple_assign_rhs1 (_a8), 0);
																				      if ((TREE_CODE (_q150) == SSA_NAME
																				           || is_gimple_min_invariant (_q150)))
																				        {
																					  _q150 = do_valueize (valueize, _q150);
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
																						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 74, __FILE__, __LINE__, false);
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
															      break;
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
												        }
												      default:;
												      }
											        }
											      break;
											    default:;
											    }
											if (gimple_max_value (_q80, valueize))
											  {
											    switch (TREE_CODE (_q81))
											      {
											      case SSA_NAME:
											        if (gimple *_d5 = get_def (valueize, _q81))
											          {
												    if (gassign *_a5 = dyn_cast <gassign *> (_d5))
												      switch (gimple_assign_rhs_code (_a5))
													{
													case NEGATE_EXPR:
													  {
													    tree _q100 = gimple_assign_rhs1 (_a5);
													    _q100 = do_valueize (valueize, _q100);
													    switch (TREE_CODE (_q100))
													      {
													      case SSA_NAME:
													        if (gimple *_d6 = get_def (valueize, _q100))
													          {
														    if (gassign *_a6 = dyn_cast <gassign *> (_d6))
														      switch (gimple_assign_rhs_code (_a6))
															{
															CASE_CONVERT:
															  {
															    tree _q110 = gimple_assign_rhs1 (_a6);
															    _q110 = do_valueize (valueize, _q110);
															    switch (TREE_CODE (_q110))
															      {
															      case SSA_NAME:
															        if (gimple *_d7 = get_def (valueize, _q110))
															          {
																    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																      switch (gimple_assign_rhs_code (_a7))
																	{
																	case LT_EXPR:
																	  {
																	    tree _q120 = gimple_assign_rhs1 (_a7);
																	    _q120 = do_valueize (valueize, _q120);
																	    tree _q121 = gimple_assign_rhs2 (_a7);
																	    _q121 = do_valueize (valueize, _q121);
																	    if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
																	      {
																		if (integer_zerop (_q121))
																		  {
																		    switch (TREE_CODE (_p2))
																		      {
																		      case SSA_NAME:
																		        if (gimple *_d8 = get_def (valueize, _p2))
																		          {
																			    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																			      switch (gimple_assign_rhs_code (_a8))
																				{
																				case REALPART_EXPR:
																				  {
																				    tree _q150 = TREE_OPERAND (gimple_assign_rhs1 (_a8), 0);
																				    if ((TREE_CODE (_q150) == SSA_NAME
																				         || is_gimple_min_invariant (_q150)))
																				      {
																					_q150 = do_valueize (valueize, _q150);
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
																						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 74, __FILE__, __LINE__, false);
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
															  }
															default:;
															}
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
								        }
								    }
								  break;
								default:;
								}
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
			      }
			  }
		        break;
		      }
		    case BIT_AND_EXPR:
		      {
			tree _q20 = TREE_OPERAND (_p0, 0);
			if ((TREE_CODE (_q20) == SSA_NAME
			     || is_gimple_min_invariant (_q20)))
			  {
			    _q20 = do_valueize (valueize, _q20);
			    tree _q21 = TREE_OPERAND (_p0, 1);
			    if ((TREE_CODE (_q21) == SSA_NAME
			         || is_gimple_min_invariant (_q21)))
			      {
				_q21 = do_valueize (valueize, _q21);
				if (tree_swap_operands_p (_q20, _q21))
				  std::swap (_q20, _q21);
				switch (TREE_CODE (_q20))
				  {
				  case SSA_NAME:
				    if (gimple *_d2 = get_def (valueize, _q20))
				      {
					if (gassign *_a2 = dyn_cast <gassign *> (_d2))
					  switch (gimple_assign_rhs_code (_a2))
					    {
					    case LT_EXPR:
					      {
						tree _q30 = gimple_assign_rhs1 (_a2);
						_q30 = do_valueize (valueize, _q30);
						tree _q31 = gimple_assign_rhs2 (_a2);
						_q31 = do_valueize (valueize, _q31);
						switch (TREE_CODE (_q30))
						  {
						  case SSA_NAME:
						    if (gimple *_d3 = get_def (valueize, _q30))
						      {
							if (gassign *_a3 = dyn_cast <gassign *> (_d3))
							  switch (gimple_assign_rhs_code (_a3))
							    {
							    case BIT_XOR_EXPR:
							      {
								tree _q40 = gimple_assign_rhs1 (_a3);
								_q40 = do_valueize (valueize, _q40);
								tree _q41 = gimple_assign_rhs2 (_a3);
								_q41 = do_valueize (valueize, _q41);
								if (tree_swap_operands_p (_q40, _q41))
								  std::swap (_q40, _q41);
								{
								  tree _q41_pops[1];
								  if (gimple_nop_convert (_q41, _q41_pops, valueize))
								    {
								      tree _q60 = _q41_pops[0];
								      switch (TREE_CODE (_q60))
								        {
									case SSA_NAME:
									  if (gimple *_d4 = get_def (valueize, _q60))
									    {
									      if (gassign *_a4 = dyn_cast <gassign *> (_d4))
									        switch (gimple_assign_rhs_code (_a4))
										  {
										  case MINUS_EXPR:
										    {
										      tree _q70 = gimple_assign_rhs1 (_a4);
										      _q70 = do_valueize (valueize, _q70);
										      tree _q71 = gimple_assign_rhs2 (_a4);
										      _q71 = do_valueize (valueize, _q71);
										      {
											tree _q70_pops[1];
											if (gimple_nop_convert (_q70, _q70_pops, valueize))
											  {
											    tree _q80 = _q70_pops[0];
											    if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
											      {
												{
												  tree _q71_pops[1];
												  if (gimple_nop_convert (_q71, _q71_pops, valueize))
												    {
												      tree _q100 = _q71_pops[0];
												      if (integer_zerop (_q31))
													{
													  switch (TREE_CODE (_q21))
													    {
													    case SSA_NAME:
													      if (gimple *_d5 = get_def (valueize, _q21))
													        {
														  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
														    switch (gimple_assign_rhs_code (_a5))
														      {
														      case LT_EXPR:
														        {
															  tree _q130 = gimple_assign_rhs1 (_a5);
															  _q130 = do_valueize (valueize, _q130);
															  tree _q131 = gimple_assign_rhs2 (_a5);
															  _q131 = do_valueize (valueize, _q131);
															  switch (TREE_CODE (_q130))
															    {
															    case SSA_NAME:
															      if (gimple *_d6 = get_def (valueize, _q130))
															        {
																  if (gassign *_a6 = dyn_cast <gassign *> (_d6))
																    switch (gimple_assign_rhs_code (_a6))
																      {
																      case BIT_XOR_EXPR:
																        {
																	  tree _q140 = gimple_assign_rhs1 (_a6);
																	  _q140 = do_valueize (valueize, _q140);
																	  tree _q141 = gimple_assign_rhs2 (_a6);
																	  _q141 = do_valueize (valueize, _q141);
																	  if (tree_swap_operands_p (_q140, _q141))
																	    std::swap (_q140, _q141);
																	  if ((_q140 == _q40 && ! TREE_SIDE_EFFECTS (_q140)) || (operand_equal_p (_q140, _q40, 0) && types_match (_q140, _q40)))
																	    {
																	      if ((_q141 == _q100 && ! TREE_SIDE_EFFECTS (_q141)) || (operand_equal_p (_q141, _q100, 0) && types_match (_q141, _q100)))
																		{
																		  if (integer_zerop (_q131))
																		    {
																		      switch (TREE_CODE (_p1))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d7 = get_def (valueize, _p1))
																			    {
																			      if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			        switch (gimple_assign_rhs_code (_a7))
																				  {
																				  case BIT_XOR_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a7);
																				      _q180 = do_valueize (valueize, _q180);
																				      tree _q181 = gimple_assign_rhs2 (_a7);
																				      _q181 = do_valueize (valueize, _q181);
																				      if (tree_swap_operands_p (_q180, _q181))
																				        std::swap (_q180, _q181);
																				      {
																					tree _q180_pops[1];
																					if (gimple_nop_convert (_q180, _q180_pops, valueize))
																					  {
																					    tree _q190 = _q180_pops[0];
																					    switch (TREE_CODE (_q190))
																					      {
																					      case SSA_NAME:
																					        if (gimple *_d8 = get_def (valueize, _q190))
																					          {
																						    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																						      switch (gimple_assign_rhs_code (_a8))
																							{
																							case NEGATE_EXPR:
																							  {
																							    tree _q200 = gimple_assign_rhs1 (_a8);
																							    _q200 = do_valueize (valueize, _q200);
																							    {
																							      tree _q200_pops[1];
																							      if (gimple_nop_convert (_q200, _q200_pops, valueize))
																							        {
																								  tree _q210 = _q200_pops[0];
																								  switch (TREE_CODE (_q210))
																								    {
																								    case SSA_NAME:
																								      if (gimple *_d9 = get_def (valueize, _q210))
																								        {
																									  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																									    switch (gimple_assign_rhs_code (_a9))
																									      {
																									      CASE_CONVERT:
																									        {
																										  tree _q220 = gimple_assign_rhs1 (_a9);
																										  _q220 = do_valueize (valueize, _q220);
																										  switch (TREE_CODE (_q220))
																										    {
																										    case SSA_NAME:
																										      if (gimple *_d10 = get_def (valueize, _q220))
																										        {
																											  if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																											    switch (gimple_assign_rhs_code (_a10))
																											      {
																											      case LT_EXPR:
																											        {
																												  tree _q230 = gimple_assign_rhs1 (_a10);
																												  _q230 = do_valueize (valueize, _q230);
																												  tree _q231 = gimple_assign_rhs2 (_a10);
																												  _q231 = do_valueize (valueize, _q231);
																												  if ((_q230 == _q40 && ! TREE_SIDE_EFFECTS (_q230)) || (operand_equal_p (_q230, _q40, 0) && types_match (_q230, _q40)))
																												    {
																												      if (integer_zerop (_q231))
																													{
																													  if (gimple_max_value (_q181, valueize))
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
																															  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																					  }
																				      }
																				      if (gimple_max_value (_q180, valueize))
																					{
																					  {
																					    tree _q181_pops[1];
																					    if (gimple_nop_convert (_q181, _q181_pops, valueize))
																					      {
																						tree _q200 = _q181_pops[0];
																						switch (TREE_CODE (_q200))
																						  {
																						  case SSA_NAME:
																						    if (gimple *_d8 = get_def (valueize, _q200))
																						      {
																							if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																							  switch (gimple_assign_rhs_code (_a8))
																							    {
																							    case NEGATE_EXPR:
																							      {
																								tree _q210 = gimple_assign_rhs1 (_a8);
																								_q210 = do_valueize (valueize, _q210);
																								{
																								  tree _q210_pops[1];
																								  if (gimple_nop_convert (_q210, _q210_pops, valueize))
																								    {
																								      tree _q220 = _q210_pops[0];
																								      switch (TREE_CODE (_q220))
																								        {
																									case SSA_NAME:
																									  if (gimple *_d9 = get_def (valueize, _q220))
																									    {
																									      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																									        switch (gimple_assign_rhs_code (_a9))
																										  {
																										  CASE_CONVERT:
																										    {
																										      tree _q230 = gimple_assign_rhs1 (_a9);
																										      _q230 = do_valueize (valueize, _q230);
																										      switch (TREE_CODE (_q230))
																										        {
																											case SSA_NAME:
																											  if (gimple *_d10 = get_def (valueize, _q230))
																											    {
																											      if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																											        switch (gimple_assign_rhs_code (_a10))
																												  {
																												  case LT_EXPR:
																												    {
																												      tree _q240 = gimple_assign_rhs1 (_a10);
																												      _q240 = do_valueize (valueize, _q240);
																												      tree _q241 = gimple_assign_rhs2 (_a10);
																												      _q241 = do_valueize (valueize, _q241);
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
																															  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																											  break;
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
																			case SSA_NAME:
																			  if (gimple *_d7 = get_def (valueize, _p1))
																			    {
																			      if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			        switch (gimple_assign_rhs_code (_a7))
																				  {
																				  case BIT_XOR_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a7);
																				      _q180 = do_valueize (valueize, _q180);
																				      tree _q181 = gimple_assign_rhs2 (_a7);
																				      _q181 = do_valueize (valueize, _q181);
																				      if (tree_swap_operands_p (_q180, _q181))
																				        std::swap (_q180, _q181);
																				      {
																					tree _q180_pops[1];
																					if (gimple_nop_convert (_q180, _q180_pops, valueize))
																					  {
																					    tree _q190 = _q180_pops[0];
																					    switch (TREE_CODE (_q190))
																					      {
																					      case SSA_NAME:
																					        if (gimple *_d8 = get_def (valueize, _q190))
																					          {
																						    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																						      switch (gimple_assign_rhs_code (_a8))
																							{
																							case NEGATE_EXPR:
																							  {
																							    tree _q200 = gimple_assign_rhs1 (_a8);
																							    _q200 = do_valueize (valueize, _q200);
																							    {
																							      tree _q200_pops[1];
																							      if (gimple_nop_convert (_q200, _q200_pops, valueize))
																							        {
																								  tree _q210 = _q200_pops[0];
																								  switch (TREE_CODE (_q210))
																								    {
																								    case SSA_NAME:
																								      if (gimple *_d9 = get_def (valueize, _q210))
																								        {
																									  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																									    switch (gimple_assign_rhs_code (_a9))
																									      {
																									      CASE_CONVERT:
																									        {
																										  tree _q220 = gimple_assign_rhs1 (_a9);
																										  _q220 = do_valueize (valueize, _q220);
																										  switch (TREE_CODE (_q220))
																										    {
																										    case SSA_NAME:
																										      if (gimple *_d10 = get_def (valueize, _q220))
																										        {
																											  if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																											    switch (gimple_assign_rhs_code (_a10))
																											      {
																											      case LT_EXPR:
																											        {
																												  tree _q230 = gimple_assign_rhs1 (_a10);
																												  _q230 = do_valueize (valueize, _q230);
																												  tree _q231 = gimple_assign_rhs2 (_a10);
																												  _q231 = do_valueize (valueize, _q231);
																												  if ((_q230 == _q40 && ! TREE_SIDE_EFFECTS (_q230)) || (operand_equal_p (_q230, _q40, 0) && types_match (_q230, _q40)))
																												    {
																												      if (integer_zerop (_q231))
																													{
																													  if (gimple_max_value (_q181, valueize))
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
																															  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																					  }
																				      }
																				      if (gimple_max_value (_q180, valueize))
																					{
																					  {
																					    tree _q181_pops[1];
																					    if (gimple_nop_convert (_q181, _q181_pops, valueize))
																					      {
																						tree _q200 = _q181_pops[0];
																						switch (TREE_CODE (_q200))
																						  {
																						  case SSA_NAME:
																						    if (gimple *_d8 = get_def (valueize, _q200))
																						      {
																							if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																							  switch (gimple_assign_rhs_code (_a8))
																							    {
																							    case NEGATE_EXPR:
																							      {
																								tree _q210 = gimple_assign_rhs1 (_a8);
																								_q210 = do_valueize (valueize, _q210);
																								{
																								  tree _q210_pops[1];
																								  if (gimple_nop_convert (_q210, _q210_pops, valueize))
																								    {
																								      tree _q220 = _q210_pops[0];
																								      switch (TREE_CODE (_q220))
																								        {
																									case SSA_NAME:
																									  if (gimple *_d9 = get_def (valueize, _q220))
																									    {
																									      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																									        switch (gimple_assign_rhs_code (_a9))
																										  {
																										  CASE_CONVERT:
																										    {
																										      tree _q230 = gimple_assign_rhs1 (_a9);
																										      _q230 = do_valueize (valueize, _q230);
																										      switch (TREE_CODE (_q230))
																										        {
																											case SSA_NAME:
																											  if (gimple *_d10 = get_def (valueize, _q230))
																											    {
																											      if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																											        switch (gimple_assign_rhs_code (_a10))
																												  {
																												  case LT_EXPR:
																												    {
																												      tree _q240 = gimple_assign_rhs1 (_a10);
																												      _q240 = do_valueize (valueize, _q240);
																												      tree _q241 = gimple_assign_rhs2 (_a10);
																												      _q241 = do_valueize (valueize, _q241);
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
																															  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																											  break;
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
														        }
														      default:;
														      }
													        }
													      break;
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
									  break;
								        default:;
								        }
								    }
								}
								if (integer_zerop (_q31))
								  {
								    switch (TREE_CODE (_q21))
								      {
								      case SSA_NAME:
								        if (gimple *_d4 = get_def (valueize, _q21))
								          {
									    if (gassign *_a4 = dyn_cast <gassign *> (_d4))
									      switch (gimple_assign_rhs_code (_a4))
										{
										case LT_EXPR:
										  {
										    tree _q80 = gimple_assign_rhs1 (_a4);
										    _q80 = do_valueize (valueize, _q80);
										    tree _q81 = gimple_assign_rhs2 (_a4);
										    _q81 = do_valueize (valueize, _q81);
										    switch (TREE_CODE (_q80))
										      {
										      case SSA_NAME:
										        if (gimple *_d5 = get_def (valueize, _q80))
										          {
											    if (gassign *_a5 = dyn_cast <gassign *> (_d5))
											      switch (gimple_assign_rhs_code (_a5))
												{
												case BIT_XOR_EXPR:
												  {
												    tree _q90 = gimple_assign_rhs1 (_a5);
												    _q90 = do_valueize (valueize, _q90);
												    tree _q91 = gimple_assign_rhs2 (_a5);
												    _q91 = do_valueize (valueize, _q91);
												    if (tree_swap_operands_p (_q90, _q91))
												      std::swap (_q90, _q91);
												    if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
												      {
													{
													  tree _q91_pops[1];
													  if (gimple_nop_convert (_q91, _q91_pops, valueize))
													    {
													      tree _q110 = _q91_pops[0];
													      switch (TREE_CODE (_q110))
													        {
														case SSA_NAME:
														  if (gimple *_d6 = get_def (valueize, _q110))
														    {
														      if (gassign *_a6 = dyn_cast <gassign *> (_d6))
														        switch (gimple_assign_rhs_code (_a6))
															  {
															  case MINUS_EXPR:
															    {
															      tree _q120 = gimple_assign_rhs1 (_a6);
															      _q120 = do_valueize (valueize, _q120);
															      tree _q121 = gimple_assign_rhs2 (_a6);
															      _q121 = do_valueize (valueize, _q121);
															      {
																tree _q120_pops[1];
																if (gimple_nop_convert (_q120, _q120_pops, valueize))
																  {
																    tree _q130 = _q120_pops[0];
																    if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
																      {
																	{
																	  tree _q121_pops[1];
																	  if (gimple_nop_convert (_q121, _q121_pops, valueize))
																	    {
																	      tree _q150 = _q121_pops[0];
																	      if ((_q150 == _q41 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q41, 0) && types_match (_q150, _q41)))
																		{
																		  if (integer_zerop (_q81))
																		    {
																		      switch (TREE_CODE (_p1))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d7 = get_def (valueize, _p1))
																			    {
																			      if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			        switch (gimple_assign_rhs_code (_a7))
																				  {
																				  case BIT_XOR_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a7);
																				      _q180 = do_valueize (valueize, _q180);
																				      tree _q181 = gimple_assign_rhs2 (_a7);
																				      _q181 = do_valueize (valueize, _q181);
																				      if (tree_swap_operands_p (_q180, _q181))
																				        std::swap (_q180, _q181);
																				      {
																					tree _q180_pops[1];
																					if (gimple_nop_convert (_q180, _q180_pops, valueize))
																					  {
																					    tree _q190 = _q180_pops[0];
																					    switch (TREE_CODE (_q190))
																					      {
																					      case SSA_NAME:
																					        if (gimple *_d8 = get_def (valueize, _q190))
																					          {
																						    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																						      switch (gimple_assign_rhs_code (_a8))
																							{
																							case NEGATE_EXPR:
																							  {
																							    tree _q200 = gimple_assign_rhs1 (_a8);
																							    _q200 = do_valueize (valueize, _q200);
																							    {
																							      tree _q200_pops[1];
																							      if (gimple_nop_convert (_q200, _q200_pops, valueize))
																							        {
																								  tree _q210 = _q200_pops[0];
																								  switch (TREE_CODE (_q210))
																								    {
																								    case SSA_NAME:
																								      if (gimple *_d9 = get_def (valueize, _q210))
																								        {
																									  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																									    switch (gimple_assign_rhs_code (_a9))
																									      {
																									      CASE_CONVERT:
																									        {
																										  tree _q220 = gimple_assign_rhs1 (_a9);
																										  _q220 = do_valueize (valueize, _q220);
																										  switch (TREE_CODE (_q220))
																										    {
																										    case SSA_NAME:
																										      if (gimple *_d10 = get_def (valueize, _q220))
																										        {
																											  if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																											    switch (gimple_assign_rhs_code (_a10))
																											      {
																											      case LT_EXPR:
																											        {
																												  tree _q230 = gimple_assign_rhs1 (_a10);
																												  _q230 = do_valueize (valueize, _q230);
																												  tree _q231 = gimple_assign_rhs2 (_a10);
																												  _q231 = do_valueize (valueize, _q231);
																												  if ((_q230 == _q40 && ! TREE_SIDE_EFFECTS (_q230)) || (operand_equal_p (_q230, _q40, 0) && types_match (_q230, _q40)))
																												    {
																												      if (integer_zerop (_q231))
																													{
																													  if (gimple_max_value (_q181, valueize))
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
																															  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																					  }
																				      }
																				      if (gimple_max_value (_q180, valueize))
																					{
																					  {
																					    tree _q181_pops[1];
																					    if (gimple_nop_convert (_q181, _q181_pops, valueize))
																					      {
																						tree _q200 = _q181_pops[0];
																						switch (TREE_CODE (_q200))
																						  {
																						  case SSA_NAME:
																						    if (gimple *_d8 = get_def (valueize, _q200))
																						      {
																							if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																							  switch (gimple_assign_rhs_code (_a8))
																							    {
																							    case NEGATE_EXPR:
																							      {
																								tree _q210 = gimple_assign_rhs1 (_a8);
																								_q210 = do_valueize (valueize, _q210);
																								{
																								  tree _q210_pops[1];
																								  if (gimple_nop_convert (_q210, _q210_pops, valueize))
																								    {
																								      tree _q220 = _q210_pops[0];
																								      switch (TREE_CODE (_q220))
																								        {
																									case SSA_NAME:
																									  if (gimple *_d9 = get_def (valueize, _q220))
																									    {
																									      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																									        switch (gimple_assign_rhs_code (_a9))
																										  {
																										  CASE_CONVERT:
																										    {
																										      tree _q230 = gimple_assign_rhs1 (_a9);
																										      _q230 = do_valueize (valueize, _q230);
																										      switch (TREE_CODE (_q230))
																										        {
																											case SSA_NAME:
																											  if (gimple *_d10 = get_def (valueize, _q230))
																											    {
																											      if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																											        switch (gimple_assign_rhs_code (_a10))
																												  {
																												  case LT_EXPR:
																												    {
																												      tree _q240 = gimple_assign_rhs1 (_a10);
																												      _q240 = do_valueize (valueize, _q240);
																												      tree _q241 = gimple_assign_rhs2 (_a10);
																												      _q241 = do_valueize (valueize, _q241);
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
																															  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																											  break;
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
													        default:;
													        }
													    }
													}
												      }
												    if ((_q90 == _q41 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q41, 0) && types_match (_q90, _q41)))
												      {
													{
													  tree _q91_pops[1];
													  if (gimple_nop_convert (_q91, _q91_pops, valueize))
													    {
													      tree _q110 = _q91_pops[0];
													      switch (TREE_CODE (_q110))
													        {
														case SSA_NAME:
														  if (gimple *_d6 = get_def (valueize, _q110))
														    {
														      if (gassign *_a6 = dyn_cast <gassign *> (_d6))
														        switch (gimple_assign_rhs_code (_a6))
															  {
															  case MINUS_EXPR:
															    {
															      tree _q120 = gimple_assign_rhs1 (_a6);
															      _q120 = do_valueize (valueize, _q120);
															      tree _q121 = gimple_assign_rhs2 (_a6);
															      _q121 = do_valueize (valueize, _q121);
															      {
																tree _q120_pops[1];
																if (gimple_nop_convert (_q120, _q120_pops, valueize))
																  {
																    tree _q130 = _q120_pops[0];
																    if ((_q130 == _q41 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q41, 0) && types_match (_q130, _q41)))
																      {
																	{
																	  tree _q121_pops[1];
																	  if (gimple_nop_convert (_q121, _q121_pops, valueize))
																	    {
																	      tree _q150 = _q121_pops[0];
																	      if ((_q150 == _q40 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q40, 0) && types_match (_q150, _q40)))
																		{
																		  if (integer_zerop (_q81))
																		    {
																		      switch (TREE_CODE (_p1))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d7 = get_def (valueize, _p1))
																			    {
																			      if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			        switch (gimple_assign_rhs_code (_a7))
																				  {
																				  case BIT_XOR_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a7);
																				      _q180 = do_valueize (valueize, _q180);
																				      tree _q181 = gimple_assign_rhs2 (_a7);
																				      _q181 = do_valueize (valueize, _q181);
																				      if (tree_swap_operands_p (_q180, _q181))
																				        std::swap (_q180, _q181);
																				      {
																					tree _q180_pops[1];
																					if (gimple_nop_convert (_q180, _q180_pops, valueize))
																					  {
																					    tree _q190 = _q180_pops[0];
																					    switch (TREE_CODE (_q190))
																					      {
																					      case SSA_NAME:
																					        if (gimple *_d8 = get_def (valueize, _q190))
																					          {
																						    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																						      switch (gimple_assign_rhs_code (_a8))
																							{
																							case NEGATE_EXPR:
																							  {
																							    tree _q200 = gimple_assign_rhs1 (_a8);
																							    _q200 = do_valueize (valueize, _q200);
																							    {
																							      tree _q200_pops[1];
																							      if (gimple_nop_convert (_q200, _q200_pops, valueize))
																							        {
																								  tree _q210 = _q200_pops[0];
																								  switch (TREE_CODE (_q210))
																								    {
																								    case SSA_NAME:
																								      if (gimple *_d9 = get_def (valueize, _q210))
																								        {
																									  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																									    switch (gimple_assign_rhs_code (_a9))
																									      {
																									      CASE_CONVERT:
																									        {
																										  tree _q220 = gimple_assign_rhs1 (_a9);
																										  _q220 = do_valueize (valueize, _q220);
																										  switch (TREE_CODE (_q220))
																										    {
																										    case SSA_NAME:
																										      if (gimple *_d10 = get_def (valueize, _q220))
																										        {
																											  if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																											    switch (gimple_assign_rhs_code (_a10))
																											      {
																											      case LT_EXPR:
																											        {
																												  tree _q230 = gimple_assign_rhs1 (_a10);
																												  _q230 = do_valueize (valueize, _q230);
																												  tree _q231 = gimple_assign_rhs2 (_a10);
																												  _q231 = do_valueize (valueize, _q231);
																												  if ((_q230 == _q41 && ! TREE_SIDE_EFFECTS (_q230)) || (operand_equal_p (_q230, _q41, 0) && types_match (_q230, _q41)))
																												    {
																												      if (integer_zerop (_q231))
																													{
																													  if (gimple_max_value (_q181, valueize))
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
																															  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																					  }
																				      }
																				      if (gimple_max_value (_q180, valueize))
																					{
																					  {
																					    tree _q181_pops[1];
																					    if (gimple_nop_convert (_q181, _q181_pops, valueize))
																					      {
																						tree _q200 = _q181_pops[0];
																						switch (TREE_CODE (_q200))
																						  {
																						  case SSA_NAME:
																						    if (gimple *_d8 = get_def (valueize, _q200))
																						      {
																							if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																							  switch (gimple_assign_rhs_code (_a8))
																							    {
																							    case NEGATE_EXPR:
																							      {
																								tree _q210 = gimple_assign_rhs1 (_a8);
																								_q210 = do_valueize (valueize, _q210);
																								{
																								  tree _q210_pops[1];
																								  if (gimple_nop_convert (_q210, _q210_pops, valueize))
																								    {
																								      tree _q220 = _q210_pops[0];
																								      switch (TREE_CODE (_q220))
																								        {
																									case SSA_NAME:
																									  if (gimple *_d9 = get_def (valueize, _q220))
																									    {
																									      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																									        switch (gimple_assign_rhs_code (_a9))
																										  {
																										  CASE_CONVERT:
																										    {
																										      tree _q230 = gimple_assign_rhs1 (_a9);
																										      _q230 = do_valueize (valueize, _q230);
																										      switch (TREE_CODE (_q230))
																										        {
																											case SSA_NAME:
																											  if (gimple *_d10 = get_def (valueize, _q230))
																											    {
																											      if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																											        switch (gimple_assign_rhs_code (_a10))
																												  {
																												  case LT_EXPR:
																												    {
																												      tree _q240 = gimple_assign_rhs1 (_a10);
																												      _q240 = do_valueize (valueize, _q240);
																												      tree _q241 = gimple_assign_rhs2 (_a10);
																												      _q241 = do_valueize (valueize, _q241);
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
																															  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																											  break;
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
										        break;
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
					      }
					    default:;
					    }
				      }
				    break;
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
	  else if (gphi *_a1 = dyn_cast <gphi *> (_d1))
	    {
	      basic_block _b1 = gimple_bb (_a1);
	      tree _p1, _p2;
	      gcond *_cond_1 = match_cond_with_binary_phi (_a1, &_p1, &_p2);
	      if (_cond_1)
		{
		  tree _cond_lhs_1 = gimple_cond_lhs (_cond_1);
		  tree _cond_rhs_1 = gimple_cond_rhs (_cond_1);
		  tree _p0 = build2 (gimple_cond_code (_cond_1), boolean_type_node, _cond_lhs_1, _cond_rhs_1);
		  switch (TREE_CODE (_p0))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p0))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case LT_EXPR:
			        {
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
				  switch (TREE_CODE (_q20))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q20))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case BIT_AND_EXPR:
					        {
						  tree _q30 = gimple_assign_rhs1 (_a3);
						  _q30 = do_valueize (valueize, _q30);
						  tree _q31 = gimple_assign_rhs2 (_a3);
						  _q31 = do_valueize (valueize, _q31);
						  if (tree_swap_operands_p (_q30, _q31))
						    std::swap (_q30, _q31);
						  switch (TREE_CODE (_q30))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _q30))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      case BIT_XOR_EXPR:
							        {
								  tree _q40 = gimple_assign_rhs1 (_a4);
								  _q40 = do_valueize (valueize, _q40);
								  tree _q41 = gimple_assign_rhs2 (_a4);
								  _q41 = do_valueize (valueize, _q41);
								  if (tree_swap_operands_p (_q40, _q41))
								    std::swap (_q40, _q41);
								  switch (TREE_CODE (_q31))
								    {
								    case SSA_NAME:
								      if (gimple *_d5 = get_def (valueize, _q31))
								        {
									  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
									    switch (gimple_assign_rhs_code (_a5))
									      {
									      case BIT_XOR_EXPR:
									        {
										  tree _q70 = gimple_assign_rhs1 (_a5);
										  _q70 = do_valueize (valueize, _q70);
										  tree _q71 = gimple_assign_rhs2 (_a5);
										  _q71 = do_valueize (valueize, _q71);
										  if (tree_swap_operands_p (_q70, _q71))
										    std::swap (_q70, _q71);
										  if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
										    {
										      {
											tree _q71_pops[1];
											if (gimple_nop_convert (_q71, _q71_pops, valueize))
											  {
											    tree _q90 = _q71_pops[0];
											    switch (TREE_CODE (_q90))
											      {
											      case SSA_NAME:
											        if (gimple *_d6 = get_def (valueize, _q90))
											          {
												    if (gassign *_a6 = dyn_cast <gassign *> (_d6))
												      switch (gimple_assign_rhs_code (_a6))
													{
													case MINUS_EXPR:
													  {
													    tree _q100 = gimple_assign_rhs1 (_a6);
													    _q100 = do_valueize (valueize, _q100);
													    tree _q101 = gimple_assign_rhs2 (_a6);
													    _q101 = do_valueize (valueize, _q101);
													    {
													      tree _q100_pops[1];
													      if (gimple_nop_convert (_q100, _q100_pops, valueize))
													        {
														  tree _q110 = _q100_pops[0];
														  if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
														    {
														      {
															tree _q101_pops[1];
															if (gimple_nop_convert (_q101, _q101_pops, valueize))
															  {
															    tree _q130 = _q101_pops[0];
															    if ((_q130 == _q41 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q41, 0) && types_match (_q130, _q41)))
															      {
																if (integer_zerop (_q21))
																  {
																    switch (TREE_CODE (_p1))
																      {
																      case SSA_NAME:
																        if (gimple *_d7 = get_def (valueize, _p1))
																          {
																	    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																	      switch (gimple_assign_rhs_code (_a7))
																		{
																		case BIT_XOR_EXPR:
																		  {
																		    tree _q160 = gimple_assign_rhs1 (_a7);
																		    _q160 = do_valueize (valueize, _q160);
																		    tree _q161 = gimple_assign_rhs2 (_a7);
																		    _q161 = do_valueize (valueize, _q161);
																		    if (tree_swap_operands_p (_q160, _q161))
																		      std::swap (_q160, _q161);
																		    switch (TREE_CODE (_q160))
																		      {
																		      case SSA_NAME:
																		        if (gimple *_d8 = get_def (valueize, _q160))
																		          {
																			    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																			      switch (gimple_assign_rhs_code (_a8))
																				{
																				case NEGATE_EXPR:
																				  {
																				    tree _q170 = gimple_assign_rhs1 (_a8);
																				    _q170 = do_valueize (valueize, _q170);
																				    switch (TREE_CODE (_q170))
																				      {
																				      case SSA_NAME:
																				        if (gimple *_d9 = get_def (valueize, _q170))
																				          {
																					    if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																					      switch (gimple_assign_rhs_code (_a9))
																						{
																						CASE_CONVERT:
																						  {
																						    tree _q180 = gimple_assign_rhs1 (_a9);
																						    _q180 = do_valueize (valueize, _q180);
																						    switch (TREE_CODE (_q180))
																						      {
																						      case SSA_NAME:
																						        if (gimple *_d10 = get_def (valueize, _q180))
																						          {
																							    if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																							      switch (gimple_assign_rhs_code (_a10))
																								{
																								case LT_EXPR:
																								  {
																								    tree _q190 = gimple_assign_rhs1 (_a10);
																								    _q190 = do_valueize (valueize, _q190);
																								    tree _q191 = gimple_assign_rhs2 (_a10);
																								    _q191 = do_valueize (valueize, _q191);
																								    if ((_q190 == _q40 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q40, 0) && types_match (_q190, _q40)))
																								      {
																									if (integer_zerop (_q191))
																									  {
																									    if (gimple_max_value (_q161, valueize))
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
																											    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																		  if (gimple_max_value (_q160, valueize))
																		    {
																		      switch (TREE_CODE (_q161))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d8 = get_def (valueize, _q161))
																			    {
																			      if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																			        switch (gimple_assign_rhs_code (_a8))
																				  {
																				  case NEGATE_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a8);
																				      _q180 = do_valueize (valueize, _q180);
																				      switch (TREE_CODE (_q180))
																				        {
																					case SSA_NAME:
																					  if (gimple *_d9 = get_def (valueize, _q180))
																					    {
																					      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																					        switch (gimple_assign_rhs_code (_a9))
																						  {
																						  CASE_CONVERT:
																						    {
																						      tree _q190 = gimple_assign_rhs1 (_a9);
																						      _q190 = do_valueize (valueize, _q190);
																						      switch (TREE_CODE (_q190))
																						        {
																							case SSA_NAME:
																							  if (gimple *_d10 = get_def (valueize, _q190))
																							    {
																							      if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																							        switch (gimple_assign_rhs_code (_a10))
																								  {
																								  case LT_EXPR:
																								    {
																								      tree _q200 = gimple_assign_rhs1 (_a10);
																								      _q200 = do_valueize (valueize, _q200);
																								      tree _q201 = gimple_assign_rhs2 (_a10);
																								      _q201 = do_valueize (valueize, _q201);
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																							  break;
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
																				    }
																				  default:;
																				  }
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
											      default:;
											      }
											  }
										      }
										    }
										  if ((_q70 == _q41 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q41, 0) && types_match (_q70, _q41)))
										    {
										      {
											tree _q71_pops[1];
											if (gimple_nop_convert (_q71, _q71_pops, valueize))
											  {
											    tree _q90 = _q71_pops[0];
											    switch (TREE_CODE (_q90))
											      {
											      case SSA_NAME:
											        if (gimple *_d6 = get_def (valueize, _q90))
											          {
												    if (gassign *_a6 = dyn_cast <gassign *> (_d6))
												      switch (gimple_assign_rhs_code (_a6))
													{
													case MINUS_EXPR:
													  {
													    tree _q100 = gimple_assign_rhs1 (_a6);
													    _q100 = do_valueize (valueize, _q100);
													    tree _q101 = gimple_assign_rhs2 (_a6);
													    _q101 = do_valueize (valueize, _q101);
													    {
													      tree _q100_pops[1];
													      if (gimple_nop_convert (_q100, _q100_pops, valueize))
													        {
														  tree _q110 = _q100_pops[0];
														  if ((_q110 == _q41 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q41, 0) && types_match (_q110, _q41)))
														    {
														      {
															tree _q101_pops[1];
															if (gimple_nop_convert (_q101, _q101_pops, valueize))
															  {
															    tree _q130 = _q101_pops[0];
															    if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
															      {
																if (integer_zerop (_q21))
																  {
																    switch (TREE_CODE (_p1))
																      {
																      case SSA_NAME:
																        if (gimple *_d7 = get_def (valueize, _p1))
																          {
																	    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																	      switch (gimple_assign_rhs_code (_a7))
																		{
																		case BIT_XOR_EXPR:
																		  {
																		    tree _q160 = gimple_assign_rhs1 (_a7);
																		    _q160 = do_valueize (valueize, _q160);
																		    tree _q161 = gimple_assign_rhs2 (_a7);
																		    _q161 = do_valueize (valueize, _q161);
																		    if (tree_swap_operands_p (_q160, _q161))
																		      std::swap (_q160, _q161);
																		    switch (TREE_CODE (_q160))
																		      {
																		      case SSA_NAME:
																		        if (gimple *_d8 = get_def (valueize, _q160))
																		          {
																			    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																			      switch (gimple_assign_rhs_code (_a8))
																				{
																				case NEGATE_EXPR:
																				  {
																				    tree _q170 = gimple_assign_rhs1 (_a8);
																				    _q170 = do_valueize (valueize, _q170);
																				    switch (TREE_CODE (_q170))
																				      {
																				      case SSA_NAME:
																				        if (gimple *_d9 = get_def (valueize, _q170))
																				          {
																					    if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																					      switch (gimple_assign_rhs_code (_a9))
																						{
																						CASE_CONVERT:
																						  {
																						    tree _q180 = gimple_assign_rhs1 (_a9);
																						    _q180 = do_valueize (valueize, _q180);
																						    switch (TREE_CODE (_q180))
																						      {
																						      case SSA_NAME:
																						        if (gimple *_d10 = get_def (valueize, _q180))
																						          {
																							    if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																							      switch (gimple_assign_rhs_code (_a10))
																								{
																								case LT_EXPR:
																								  {
																								    tree _q190 = gimple_assign_rhs1 (_a10);
																								    _q190 = do_valueize (valueize, _q190);
																								    tree _q191 = gimple_assign_rhs2 (_a10);
																								    _q191 = do_valueize (valueize, _q191);
																								    if ((_q190 == _q41 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q41, 0) && types_match (_q190, _q41)))
																								      {
																									if (integer_zerop (_q191))
																									  {
																									    if (gimple_max_value (_q161, valueize))
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
																											    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																		  if (gimple_max_value (_q160, valueize))
																		    {
																		      switch (TREE_CODE (_q161))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d8 = get_def (valueize, _q161))
																			    {
																			      if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																			        switch (gimple_assign_rhs_code (_a8))
																				  {
																				  case NEGATE_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a8);
																				      _q180 = do_valueize (valueize, _q180);
																				      switch (TREE_CODE (_q180))
																				        {
																					case SSA_NAME:
																					  if (gimple *_d9 = get_def (valueize, _q180))
																					    {
																					      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																					        switch (gimple_assign_rhs_code (_a9))
																						  {
																						  CASE_CONVERT:
																						    {
																						      tree _q190 = gimple_assign_rhs1 (_a9);
																						      _q190 = do_valueize (valueize, _q190);
																						      switch (TREE_CODE (_q190))
																						        {
																							case SSA_NAME:
																							  if (gimple *_d10 = get_def (valueize, _q190))
																							    {
																							      if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																							        switch (gimple_assign_rhs_code (_a10))
																								  {
																								  case LT_EXPR:
																								    {
																								      tree _q200 = gimple_assign_rhs1 (_a10);
																								      _q200 = do_valueize (valueize, _q200);
																								      tree _q201 = gimple_assign_rhs2 (_a10);
																								      _q201 = do_valueize (valueize, _q201);
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																							  break;
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
																				    }
																				  default:;
																				  }
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
								      break;
								    default:;
								    }
								  {
								    tree _q41_pops[1];
								    if (gimple_nop_convert (_q41, _q41_pops, valueize))
								      {
									tree _q60 = _q41_pops[0];
									switch (TREE_CODE (_q60))
									  {
									  case SSA_NAME:
									    if (gimple *_d5 = get_def (valueize, _q60))
									      {
										if (gassign *_a5 = dyn_cast <gassign *> (_d5))
										  switch (gimple_assign_rhs_code (_a5))
										    {
										    case MINUS_EXPR:
										      {
											tree _q70 = gimple_assign_rhs1 (_a5);
											_q70 = do_valueize (valueize, _q70);
											tree _q71 = gimple_assign_rhs2 (_a5);
											_q71 = do_valueize (valueize, _q71);
											{
											  tree _q70_pops[1];
											  if (gimple_nop_convert (_q70, _q70_pops, valueize))
											    {
											      tree _q80 = _q70_pops[0];
											      if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
												{
												  {
												    tree _q71_pops[1];
												    if (gimple_nop_convert (_q71, _q71_pops, valueize))
												      {
													tree _q100 = _q71_pops[0];
													switch (TREE_CODE (_q31))
													  {
													  case SSA_NAME:
													    if (gimple *_d6 = get_def (valueize, _q31))
													      {
														if (gassign *_a6 = dyn_cast <gassign *> (_d6))
														  switch (gimple_assign_rhs_code (_a6))
														    {
														    case BIT_XOR_EXPR:
														      {
															tree _q120 = gimple_assign_rhs1 (_a6);
															_q120 = do_valueize (valueize, _q120);
															tree _q121 = gimple_assign_rhs2 (_a6);
															_q121 = do_valueize (valueize, _q121);
															if (tree_swap_operands_p (_q120, _q121))
															  std::swap (_q120, _q121);
															if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
															  {
															    if ((_q121 == _q100 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q100, 0) && types_match (_q121, _q100)))
															      {
																if (integer_zerop (_q21))
																  {
																    switch (TREE_CODE (_p1))
																      {
																      case SSA_NAME:
																        if (gimple *_d7 = get_def (valueize, _p1))
																          {
																	    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																	      switch (gimple_assign_rhs_code (_a7))
																		{
																		case BIT_XOR_EXPR:
																		  {
																		    tree _q160 = gimple_assign_rhs1 (_a7);
																		    _q160 = do_valueize (valueize, _q160);
																		    tree _q161 = gimple_assign_rhs2 (_a7);
																		    _q161 = do_valueize (valueize, _q161);
																		    if (tree_swap_operands_p (_q160, _q161))
																		      std::swap (_q160, _q161);
																		    switch (TREE_CODE (_q160))
																		      {
																		      case SSA_NAME:
																		        if (gimple *_d8 = get_def (valueize, _q160))
																		          {
																			    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																			      switch (gimple_assign_rhs_code (_a8))
																				{
																				case NEGATE_EXPR:
																				  {
																				    tree _q170 = gimple_assign_rhs1 (_a8);
																				    _q170 = do_valueize (valueize, _q170);
																				    switch (TREE_CODE (_q170))
																				      {
																				      case SSA_NAME:
																				        if (gimple *_d9 = get_def (valueize, _q170))
																				          {
																					    if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																					      switch (gimple_assign_rhs_code (_a9))
																						{
																						CASE_CONVERT:
																						  {
																						    tree _q180 = gimple_assign_rhs1 (_a9);
																						    _q180 = do_valueize (valueize, _q180);
																						    switch (TREE_CODE (_q180))
																						      {
																						      case SSA_NAME:
																						        if (gimple *_d10 = get_def (valueize, _q180))
																						          {
																							    if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																							      switch (gimple_assign_rhs_code (_a10))
																								{
																								case LT_EXPR:
																								  {
																								    tree _q190 = gimple_assign_rhs1 (_a10);
																								    _q190 = do_valueize (valueize, _q190);
																								    tree _q191 = gimple_assign_rhs2 (_a10);
																								    _q191 = do_valueize (valueize, _q191);
																								    if ((_q190 == _q40 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q40, 0) && types_match (_q190, _q40)))
																								      {
																									if (integer_zerop (_q191))
																									  {
																									    if (gimple_max_value (_q161, valueize))
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
																											    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																		  if (gimple_max_value (_q160, valueize))
																		    {
																		      switch (TREE_CODE (_q161))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d8 = get_def (valueize, _q161))
																			    {
																			      if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																			        switch (gimple_assign_rhs_code (_a8))
																				  {
																				  case NEGATE_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a8);
																				      _q180 = do_valueize (valueize, _q180);
																				      switch (TREE_CODE (_q180))
																				        {
																					case SSA_NAME:
																					  if (gimple *_d9 = get_def (valueize, _q180))
																					    {
																					      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																					        switch (gimple_assign_rhs_code (_a9))
																						  {
																						  CASE_CONVERT:
																						    {
																						      tree _q190 = gimple_assign_rhs1 (_a9);
																						      _q190 = do_valueize (valueize, _q190);
																						      switch (TREE_CODE (_q190))
																						        {
																							case SSA_NAME:
																							  if (gimple *_d10 = get_def (valueize, _q190))
																							    {
																							      if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																							        switch (gimple_assign_rhs_code (_a10))
																								  {
																								  case LT_EXPR:
																								    {
																								      tree _q200 = gimple_assign_rhs1 (_a10);
																								      _q200 = do_valueize (valueize, _q200);
																								      tree _q201 = gimple_assign_rhs2 (_a10);
																								      _q201 = do_valueize (valueize, _q201);
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																							  break;
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
																				    }
																				  default:;
																				  }
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
																      case SSA_NAME:
																        if (gimple *_d7 = get_def (valueize, _p1))
																          {
																	    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																	      switch (gimple_assign_rhs_code (_a7))
																		{
																		case BIT_XOR_EXPR:
																		  {
																		    tree _q160 = gimple_assign_rhs1 (_a7);
																		    _q160 = do_valueize (valueize, _q160);
																		    tree _q161 = gimple_assign_rhs2 (_a7);
																		    _q161 = do_valueize (valueize, _q161);
																		    if (tree_swap_operands_p (_q160, _q161))
																		      std::swap (_q160, _q161);
																		    switch (TREE_CODE (_q160))
																		      {
																		      case SSA_NAME:
																		        if (gimple *_d8 = get_def (valueize, _q160))
																		          {
																			    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																			      switch (gimple_assign_rhs_code (_a8))
																				{
																				case NEGATE_EXPR:
																				  {
																				    tree _q170 = gimple_assign_rhs1 (_a8);
																				    _q170 = do_valueize (valueize, _q170);
																				    switch (TREE_CODE (_q170))
																				      {
																				      case SSA_NAME:
																				        if (gimple *_d9 = get_def (valueize, _q170))
																				          {
																					    if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																					      switch (gimple_assign_rhs_code (_a9))
																						{
																						CASE_CONVERT:
																						  {
																						    tree _q180 = gimple_assign_rhs1 (_a9);
																						    _q180 = do_valueize (valueize, _q180);
																						    switch (TREE_CODE (_q180))
																						      {
																						      case SSA_NAME:
																						        if (gimple *_d10 = get_def (valueize, _q180))
																						          {
																							    if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																							      switch (gimple_assign_rhs_code (_a10))
																								{
																								case LT_EXPR:
																								  {
																								    tree _q190 = gimple_assign_rhs1 (_a10);
																								    _q190 = do_valueize (valueize, _q190);
																								    tree _q191 = gimple_assign_rhs2 (_a10);
																								    _q191 = do_valueize (valueize, _q191);
																								    if ((_q190 == _q40 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q40, 0) && types_match (_q190, _q40)))
																								      {
																									if (integer_zerop (_q191))
																									  {
																									    if (gimple_max_value (_q161, valueize))
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
																											    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																		  if (gimple_max_value (_q160, valueize))
																		    {
																		      switch (TREE_CODE (_q161))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d8 = get_def (valueize, _q161))
																			    {
																			      if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																			        switch (gimple_assign_rhs_code (_a8))
																				  {
																				  case NEGATE_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a8);
																				      _q180 = do_valueize (valueize, _q180);
																				      switch (TREE_CODE (_q180))
																				        {
																					case SSA_NAME:
																					  if (gimple *_d9 = get_def (valueize, _q180))
																					    {
																					      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																					        switch (gimple_assign_rhs_code (_a9))
																						  {
																						  CASE_CONVERT:
																						    {
																						      tree _q190 = gimple_assign_rhs1 (_a9);
																						      _q190 = do_valueize (valueize, _q190);
																						      switch (TREE_CODE (_q190))
																						        {
																							case SSA_NAME:
																							  if (gimple *_d10 = get_def (valueize, _q190))
																							    {
																							      if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																							        switch (gimple_assign_rhs_code (_a10))
																								  {
																								  case LT_EXPR:
																								    {
																								      tree _q200 = gimple_assign_rhs1 (_a10);
																								      _q200 = do_valueize (valueize, _q200);
																								      tree _q201 = gimple_assign_rhs2 (_a10);
																								      _q201 = do_valueize (valueize, _q201);
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																							  break;
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
																				    }
																				  default:;
																				  }
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
					        }
					      default:;
					      }
				        }
				      break;
				    default:;
				    }
			          break;
			        }
			      case GE_EXPR:
			        {
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
				  switch (TREE_CODE (_q20))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q20))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case BIT_AND_EXPR:
					        {
						  tree _q30 = gimple_assign_rhs1 (_a3);
						  _q30 = do_valueize (valueize, _q30);
						  tree _q31 = gimple_assign_rhs2 (_a3);
						  _q31 = do_valueize (valueize, _q31);
						  if (tree_swap_operands_p (_q30, _q31))
						    std::swap (_q30, _q31);
						  switch (TREE_CODE (_q30))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _q30))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      case BIT_XOR_EXPR:
							        {
								  tree _q40 = gimple_assign_rhs1 (_a4);
								  _q40 = do_valueize (valueize, _q40);
								  tree _q41 = gimple_assign_rhs2 (_a4);
								  _q41 = do_valueize (valueize, _q41);
								  if (tree_swap_operands_p (_q40, _q41))
								    std::swap (_q40, _q41);
								  switch (TREE_CODE (_q31))
								    {
								    case SSA_NAME:
								      if (gimple *_d5 = get_def (valueize, _q31))
								        {
									  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
									    switch (gimple_assign_rhs_code (_a5))
									      {
									      case BIT_XOR_EXPR:
									        {
										  tree _q70 = gimple_assign_rhs1 (_a5);
										  _q70 = do_valueize (valueize, _q70);
										  tree _q71 = gimple_assign_rhs2 (_a5);
										  _q71 = do_valueize (valueize, _q71);
										  if (tree_swap_operands_p (_q70, _q71))
										    std::swap (_q70, _q71);
										  if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
										    {
										      {
											tree _q71_pops[1];
											if (gimple_nop_convert (_q71, _q71_pops, valueize))
											  {
											    tree _q90 = _q71_pops[0];
											    switch (TREE_CODE (_q90))
											      {
											      case SSA_NAME:
											        if (gimple *_d6 = get_def (valueize, _q90))
											          {
												    if (gassign *_a6 = dyn_cast <gassign *> (_d6))
												      switch (gimple_assign_rhs_code (_a6))
													{
													case MINUS_EXPR:
													  {
													    tree _q100 = gimple_assign_rhs1 (_a6);
													    _q100 = do_valueize (valueize, _q100);
													    tree _q101 = gimple_assign_rhs2 (_a6);
													    _q101 = do_valueize (valueize, _q101);
													    {
													      tree _q100_pops[1];
													      if (gimple_nop_convert (_q100, _q100_pops, valueize))
													        {
														  tree _q110 = _q100_pops[0];
														  if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
														    {
														      {
															tree _q101_pops[1];
															if (gimple_nop_convert (_q101, _q101_pops, valueize))
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
																	  case SSA_NAME:
																	    if (gimple *_d7 = get_def (valueize, _p2))
																	      {
																		if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																		  switch (gimple_assign_rhs_code (_a7))
																		    {
																		    case BIT_XOR_EXPR:
																		      {
																			tree _q170 = gimple_assign_rhs1 (_a7);
																			_q170 = do_valueize (valueize, _q170);
																			tree _q171 = gimple_assign_rhs2 (_a7);
																			_q171 = do_valueize (valueize, _q171);
																			if (tree_swap_operands_p (_q170, _q171))
																			  std::swap (_q170, _q171);
																			switch (TREE_CODE (_q170))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d8 = get_def (valueize, _q170))
																			      {
																				if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				  switch (gimple_assign_rhs_code (_a8))
																				    {
																				    case NEGATE_EXPR:
																				      {
																					tree _q180 = gimple_assign_rhs1 (_a8);
																					_q180 = do_valueize (valueize, _q180);
																					switch (TREE_CODE (_q180))
																					  {
																					  case SSA_NAME:
																					    if (gimple *_d9 = get_def (valueize, _q180))
																					      {
																						if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						  switch (gimple_assign_rhs_code (_a9))
																						    {
																						    CASE_CONVERT:
																						      {
																							tree _q190 = gimple_assign_rhs1 (_a9);
																							_q190 = do_valueize (valueize, _q190);
																							switch (TREE_CODE (_q190))
																							  {
																							  case SSA_NAME:
																							    if (gimple *_d10 = get_def (valueize, _q190))
																							      {
																								if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																								  switch (gimple_assign_rhs_code (_a10))
																								    {
																								    case LT_EXPR:
																								      {
																									tree _q200 = gimple_assign_rhs1 (_a10);
																									_q200 = do_valueize (valueize, _q200);
																									tree _q201 = gimple_assign_rhs2 (_a10);
																									_q201 = do_valueize (valueize, _q201);
																									if ((_q200 == _q40 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q40, 0) && types_match (_q200, _q40)))
																									  {
																									    if (integer_zerop (_q201))
																									      {
																										if (gimple_max_value (_q171, valueize))
																										  {
																										    {
																										      tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _q71 };
																										      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																											{
																											  {
																											    res_ops[0] = captures[0];
																											    res_ops[1] = captures[1];
																											    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																							    break;
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
																				      }
																				    default:;
																				    }
																			      }
																			    break;
																		          default:;
																		          }
																		      if (gimple_max_value (_q170, valueize))
																			{
																			  switch (TREE_CODE (_q171))
																			    {
																			    case SSA_NAME:
																			      if (gimple *_d8 = get_def (valueize, _q171))
																			        {
																				  if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				    switch (gimple_assign_rhs_code (_a8))
																				      {
																				      case NEGATE_EXPR:
																				        {
																					  tree _q190 = gimple_assign_rhs1 (_a8);
																					  _q190 = do_valueize (valueize, _q190);
																					  switch (TREE_CODE (_q190))
																					    {
																					    case SSA_NAME:
																					      if (gimple *_d9 = get_def (valueize, _q190))
																					        {
																						  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						    switch (gimple_assign_rhs_code (_a9))
																						      {
																						      CASE_CONVERT:
																						        {
																							  tree _q200 = gimple_assign_rhs1 (_a9);
																							  _q200 = do_valueize (valueize, _q200);
																							  switch (TREE_CODE (_q200))
																							    {
																							    case SSA_NAME:
																							      if (gimple *_d10 = get_def (valueize, _q200))
																							        {
																								  if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																								    switch (gimple_assign_rhs_code (_a10))
																								      {
																								      case LT_EXPR:
																								        {
																									  tree _q210 = gimple_assign_rhs1 (_a10);
																									  _q210 = do_valueize (valueize, _q210);
																									  tree _q211 = gimple_assign_rhs2 (_a10);
																									  _q211 = do_valueize (valueize, _q211);
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																				        }
																				      default:;
																				      }
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
											      default:;
											      }
											  }
										      }
										    }
										  if ((_q70 == _q41 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q41, 0) && types_match (_q70, _q41)))
										    {
										      {
											tree _q71_pops[1];
											if (gimple_nop_convert (_q71, _q71_pops, valueize))
											  {
											    tree _q90 = _q71_pops[0];
											    switch (TREE_CODE (_q90))
											      {
											      case SSA_NAME:
											        if (gimple *_d6 = get_def (valueize, _q90))
											          {
												    if (gassign *_a6 = dyn_cast <gassign *> (_d6))
												      switch (gimple_assign_rhs_code (_a6))
													{
													case MINUS_EXPR:
													  {
													    tree _q100 = gimple_assign_rhs1 (_a6);
													    _q100 = do_valueize (valueize, _q100);
													    tree _q101 = gimple_assign_rhs2 (_a6);
													    _q101 = do_valueize (valueize, _q101);
													    {
													      tree _q100_pops[1];
													      if (gimple_nop_convert (_q100, _q100_pops, valueize))
													        {
														  tree _q110 = _q100_pops[0];
														  if ((_q110 == _q41 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q41, 0) && types_match (_q110, _q41)))
														    {
														      {
															tree _q101_pops[1];
															if (gimple_nop_convert (_q101, _q101_pops, valueize))
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
																	  case SSA_NAME:
																	    if (gimple *_d7 = get_def (valueize, _p2))
																	      {
																		if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																		  switch (gimple_assign_rhs_code (_a7))
																		    {
																		    case BIT_XOR_EXPR:
																		      {
																			tree _q170 = gimple_assign_rhs1 (_a7);
																			_q170 = do_valueize (valueize, _q170);
																			tree _q171 = gimple_assign_rhs2 (_a7);
																			_q171 = do_valueize (valueize, _q171);
																			if (tree_swap_operands_p (_q170, _q171))
																			  std::swap (_q170, _q171);
																			switch (TREE_CODE (_q170))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d8 = get_def (valueize, _q170))
																			      {
																				if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				  switch (gimple_assign_rhs_code (_a8))
																				    {
																				    case NEGATE_EXPR:
																				      {
																					tree _q180 = gimple_assign_rhs1 (_a8);
																					_q180 = do_valueize (valueize, _q180);
																					switch (TREE_CODE (_q180))
																					  {
																					  case SSA_NAME:
																					    if (gimple *_d9 = get_def (valueize, _q180))
																					      {
																						if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						  switch (gimple_assign_rhs_code (_a9))
																						    {
																						    CASE_CONVERT:
																						      {
																							tree _q190 = gimple_assign_rhs1 (_a9);
																							_q190 = do_valueize (valueize, _q190);
																							switch (TREE_CODE (_q190))
																							  {
																							  case SSA_NAME:
																							    if (gimple *_d10 = get_def (valueize, _q190))
																							      {
																								if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																								  switch (gimple_assign_rhs_code (_a10))
																								    {
																								    case LT_EXPR:
																								      {
																									tree _q200 = gimple_assign_rhs1 (_a10);
																									_q200 = do_valueize (valueize, _q200);
																									tree _q201 = gimple_assign_rhs2 (_a10);
																									_q201 = do_valueize (valueize, _q201);
																									if ((_q200 == _q41 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q41, 0) && types_match (_q200, _q41)))
																									  {
																									    if (integer_zerop (_q201))
																									      {
																										if (gimple_max_value (_q171, valueize))
																										  {
																										    {
																										      tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _q71 };
																										      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																											{
																											  {
																											    res_ops[0] = captures[0];
																											    res_ops[1] = captures[1];
																											    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																							    break;
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
																				      }
																				    default:;
																				    }
																			      }
																			    break;
																		          default:;
																		          }
																		      if (gimple_max_value (_q170, valueize))
																			{
																			  switch (TREE_CODE (_q171))
																			    {
																			    case SSA_NAME:
																			      if (gimple *_d8 = get_def (valueize, _q171))
																			        {
																				  if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				    switch (gimple_assign_rhs_code (_a8))
																				      {
																				      case NEGATE_EXPR:
																				        {
																					  tree _q190 = gimple_assign_rhs1 (_a8);
																					  _q190 = do_valueize (valueize, _q190);
																					  switch (TREE_CODE (_q190))
																					    {
																					    case SSA_NAME:
																					      if (gimple *_d9 = get_def (valueize, _q190))
																					        {
																						  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						    switch (gimple_assign_rhs_code (_a9))
																						      {
																						      CASE_CONVERT:
																						        {
																							  tree _q200 = gimple_assign_rhs1 (_a9);
																							  _q200 = do_valueize (valueize, _q200);
																							  switch (TREE_CODE (_q200))
																							    {
																							    case SSA_NAME:
																							      if (gimple *_d10 = get_def (valueize, _q200))
																							        {
																								  if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																								    switch (gimple_assign_rhs_code (_a10))
																								      {
																								      case LT_EXPR:
																								        {
																									  tree _q210 = gimple_assign_rhs1 (_a10);
																									  _q210 = do_valueize (valueize, _q210);
																									  tree _q211 = gimple_assign_rhs2 (_a10);
																									  _q211 = do_valueize (valueize, _q211);
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																				        }
																				      default:;
																				      }
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
								      break;
								    default:;
								    }
								  {
								    tree _q41_pops[1];
								    if (gimple_nop_convert (_q41, _q41_pops, valueize))
								      {
									tree _q60 = _q41_pops[0];
									switch (TREE_CODE (_q60))
									  {
									  case SSA_NAME:
									    if (gimple *_d5 = get_def (valueize, _q60))
									      {
										if (gassign *_a5 = dyn_cast <gassign *> (_d5))
										  switch (gimple_assign_rhs_code (_a5))
										    {
										    case MINUS_EXPR:
										      {
											tree _q70 = gimple_assign_rhs1 (_a5);
											_q70 = do_valueize (valueize, _q70);
											tree _q71 = gimple_assign_rhs2 (_a5);
											_q71 = do_valueize (valueize, _q71);
											{
											  tree _q70_pops[1];
											  if (gimple_nop_convert (_q70, _q70_pops, valueize))
											    {
											      tree _q80 = _q70_pops[0];
											      if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
												{
												  {
												    tree _q71_pops[1];
												    if (gimple_nop_convert (_q71, _q71_pops, valueize))
												      {
													tree _q100 = _q71_pops[0];
													switch (TREE_CODE (_q31))
													  {
													  case SSA_NAME:
													    if (gimple *_d6 = get_def (valueize, _q31))
													      {
														if (gassign *_a6 = dyn_cast <gassign *> (_d6))
														  switch (gimple_assign_rhs_code (_a6))
														    {
														    case BIT_XOR_EXPR:
														      {
															tree _q120 = gimple_assign_rhs1 (_a6);
															_q120 = do_valueize (valueize, _q120);
															tree _q121 = gimple_assign_rhs2 (_a6);
															_q121 = do_valueize (valueize, _q121);
															if (tree_swap_operands_p (_q120, _q121))
															  std::swap (_q120, _q121);
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
																	  case SSA_NAME:
																	    if (gimple *_d7 = get_def (valueize, _p2))
																	      {
																		if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																		  switch (gimple_assign_rhs_code (_a7))
																		    {
																		    case BIT_XOR_EXPR:
																		      {
																			tree _q170 = gimple_assign_rhs1 (_a7);
																			_q170 = do_valueize (valueize, _q170);
																			tree _q171 = gimple_assign_rhs2 (_a7);
																			_q171 = do_valueize (valueize, _q171);
																			if (tree_swap_operands_p (_q170, _q171))
																			  std::swap (_q170, _q171);
																			switch (TREE_CODE (_q170))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d8 = get_def (valueize, _q170))
																			      {
																				if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				  switch (gimple_assign_rhs_code (_a8))
																				    {
																				    case NEGATE_EXPR:
																				      {
																					tree _q180 = gimple_assign_rhs1 (_a8);
																					_q180 = do_valueize (valueize, _q180);
																					switch (TREE_CODE (_q180))
																					  {
																					  case SSA_NAME:
																					    if (gimple *_d9 = get_def (valueize, _q180))
																					      {
																						if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						  switch (gimple_assign_rhs_code (_a9))
																						    {
																						    CASE_CONVERT:
																						      {
																							tree _q190 = gimple_assign_rhs1 (_a9);
																							_q190 = do_valueize (valueize, _q190);
																							switch (TREE_CODE (_q190))
																							  {
																							  case SSA_NAME:
																							    if (gimple *_d10 = get_def (valueize, _q190))
																							      {
																								if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																								  switch (gimple_assign_rhs_code (_a10))
																								    {
																								    case LT_EXPR:
																								      {
																									tree _q200 = gimple_assign_rhs1 (_a10);
																									_q200 = do_valueize (valueize, _q200);
																									tree _q201 = gimple_assign_rhs2 (_a10);
																									_q201 = do_valueize (valueize, _q201);
																									if ((_q200 == _q40 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q40, 0) && types_match (_q200, _q40)))
																									  {
																									    if (integer_zerop (_q201))
																									      {
																										if (gimple_max_value (_q171, valueize))
																										  {
																										    {
																										      tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
																										      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																											{
																											  {
																											    res_ops[0] = captures[0];
																											    res_ops[1] = captures[1];
																											    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																							    break;
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
																				      }
																				    default:;
																				    }
																			      }
																			    break;
																		          default:;
																		          }
																		      if (gimple_max_value (_q170, valueize))
																			{
																			  switch (TREE_CODE (_q171))
																			    {
																			    case SSA_NAME:
																			      if (gimple *_d8 = get_def (valueize, _q171))
																			        {
																				  if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				    switch (gimple_assign_rhs_code (_a8))
																				      {
																				      case NEGATE_EXPR:
																				        {
																					  tree _q190 = gimple_assign_rhs1 (_a8);
																					  _q190 = do_valueize (valueize, _q190);
																					  switch (TREE_CODE (_q190))
																					    {
																					    case SSA_NAME:
																					      if (gimple *_d9 = get_def (valueize, _q190))
																					        {
																						  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						    switch (gimple_assign_rhs_code (_a9))
																						      {
																						      CASE_CONVERT:
																						        {
																							  tree _q200 = gimple_assign_rhs1 (_a9);
																							  _q200 = do_valueize (valueize, _q200);
																							  switch (TREE_CODE (_q200))
																							    {
																							    case SSA_NAME:
																							      if (gimple *_d10 = get_def (valueize, _q200))
																							        {
																								  if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																								    switch (gimple_assign_rhs_code (_a10))
																								      {
																								      case LT_EXPR:
																								        {
																									  tree _q210 = gimple_assign_rhs1 (_a10);
																									  _q210 = do_valueize (valueize, _q210);
																									  tree _q211 = gimple_assign_rhs2 (_a10);
																									  _q211 = do_valueize (valueize, _q211);
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																				        }
																				      default:;
																				      }
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
																	  case SSA_NAME:
																	    if (gimple *_d7 = get_def (valueize, _p2))
																	      {
																		if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																		  switch (gimple_assign_rhs_code (_a7))
																		    {
																		    case BIT_XOR_EXPR:
																		      {
																			tree _q170 = gimple_assign_rhs1 (_a7);
																			_q170 = do_valueize (valueize, _q170);
																			tree _q171 = gimple_assign_rhs2 (_a7);
																			_q171 = do_valueize (valueize, _q171);
																			if (tree_swap_operands_p (_q170, _q171))
																			  std::swap (_q170, _q171);
																			switch (TREE_CODE (_q170))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d8 = get_def (valueize, _q170))
																			      {
																				if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				  switch (gimple_assign_rhs_code (_a8))
																				    {
																				    case NEGATE_EXPR:
																				      {
																					tree _q180 = gimple_assign_rhs1 (_a8);
																					_q180 = do_valueize (valueize, _q180);
																					switch (TREE_CODE (_q180))
																					  {
																					  case SSA_NAME:
																					    if (gimple *_d9 = get_def (valueize, _q180))
																					      {
																						if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						  switch (gimple_assign_rhs_code (_a9))
																						    {
																						    CASE_CONVERT:
																						      {
																							tree _q190 = gimple_assign_rhs1 (_a9);
																							_q190 = do_valueize (valueize, _q190);
																							switch (TREE_CODE (_q190))
																							  {
																							  case SSA_NAME:
																							    if (gimple *_d10 = get_def (valueize, _q190))
																							      {
																								if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																								  switch (gimple_assign_rhs_code (_a10))
																								    {
																								    case LT_EXPR:
																								      {
																									tree _q200 = gimple_assign_rhs1 (_a10);
																									_q200 = do_valueize (valueize, _q200);
																									tree _q201 = gimple_assign_rhs2 (_a10);
																									_q201 = do_valueize (valueize, _q201);
																									if ((_q200 == _q40 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q40, 0) && types_match (_q200, _q40)))
																									  {
																									    if (integer_zerop (_q201))
																									      {
																										if (gimple_max_value (_q171, valueize))
																										  {
																										    {
																										      tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
																										      if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																											{
																											  {
																											    res_ops[0] = captures[0];
																											    res_ops[1] = captures[1];
																											    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																							    break;
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
																				      }
																				    default:;
																				    }
																			      }
																			    break;
																		          default:;
																		          }
																		      if (gimple_max_value (_q170, valueize))
																			{
																			  switch (TREE_CODE (_q171))
																			    {
																			    case SSA_NAME:
																			      if (gimple *_d8 = get_def (valueize, _q171))
																			        {
																				  if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				    switch (gimple_assign_rhs_code (_a8))
																				      {
																				      case NEGATE_EXPR:
																				        {
																					  tree _q190 = gimple_assign_rhs1 (_a8);
																					  _q190 = do_valueize (valueize, _q190);
																					  switch (TREE_CODE (_q190))
																					    {
																					    case SSA_NAME:
																					      if (gimple *_d9 = get_def (valueize, _q190))
																					        {
																						  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						    switch (gimple_assign_rhs_code (_a9))
																						      {
																						      CASE_CONVERT:
																						        {
																							  tree _q200 = gimple_assign_rhs1 (_a9);
																							  _q200 = do_valueize (valueize, _q200);
																							  switch (TREE_CODE (_q200))
																							    {
																							    case SSA_NAME:
																							      if (gimple *_d10 = get_def (valueize, _q200))
																							        {
																								  if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																								    switch (gimple_assign_rhs_code (_a10))
																								      {
																								      case LT_EXPR:
																								        {
																									  tree _q210 = gimple_assign_rhs1 (_a10);
																									  _q210 = do_valueize (valueize, _q210);
																									  tree _q211 = gimple_assign_rhs2 (_a10);
																									  _q211 = do_valueize (valueize, _q211);
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																				        }
																				      default:;
																				      }
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
									    break;
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
					        }
					      default:;
					      }
				        }
				      break;
				    default:;
				    }
			          break;
			        }
			      case NE_EXPR:
			        {
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
				  if (tree_swap_operands_p (_q20, _q21))
				    std::swap (_q20, _q21);
				  switch (TREE_CODE (_q20))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q20))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case IMAGPART_EXPR:
					        {
						  tree _q30 = TREE_OPERAND (gimple_assign_rhs1 (_a3), 0);
						  if ((TREE_CODE (_q30) == SSA_NAME
						       || is_gimple_min_invariant (_q30)))
						    {
						      _q30 = do_valueize (valueize, _q30);
						      switch (TREE_CODE (_q30))
						        {
							case SSA_NAME:
							  if (gimple *_d4 = get_def (valueize, _q30))
							    {
							      if (gcall *_c4 = dyn_cast <gcall *> (_d4))
							        switch (gimple_call_combined_fn (_c4))
								  {
								  case CFN_SUB_OVERFLOW:
								    if (gimple_call_num_args (_c4) == 2)
								      {
									tree _q40 = gimple_call_arg (_c4, 0);
									_q40 = do_valueize (valueize, _q40);
									tree _q41 = gimple_call_arg (_c4, 1);
									_q41 = do_valueize (valueize, _q41);
									if (integer_zerop (_q21))
									  {
									    switch (TREE_CODE (_p1))
									      {
									      case SSA_NAME:
									        if (gimple *_d5 = get_def (valueize, _p1))
									          {
										    if (gassign *_a5 = dyn_cast <gassign *> (_d5))
										      switch (gimple_assign_rhs_code (_a5))
											{
											case BIT_XOR_EXPR:
											  {
											    tree _q80 = gimple_assign_rhs1 (_a5);
											    _q80 = do_valueize (valueize, _q80);
											    tree _q81 = gimple_assign_rhs2 (_a5);
											    _q81 = do_valueize (valueize, _q81);
											    if (tree_swap_operands_p (_q80, _q81))
											      std::swap (_q80, _q81);
											    switch (TREE_CODE (_q80))
											      {
											      case SSA_NAME:
											        if (gimple *_d6 = get_def (valueize, _q80))
											          {
												    if (gassign *_a6 = dyn_cast <gassign *> (_d6))
												      switch (gimple_assign_rhs_code (_a6))
													{
													case NEGATE_EXPR:
													  {
													    tree _q90 = gimple_assign_rhs1 (_a6);
													    _q90 = do_valueize (valueize, _q90);
													    switch (TREE_CODE (_q90))
													      {
													      case SSA_NAME:
													        if (gimple *_d7 = get_def (valueize, _q90))
													          {
														    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
														      switch (gimple_assign_rhs_code (_a7))
															{
															CASE_CONVERT:
															  {
															    tree _q100 = gimple_assign_rhs1 (_a7);
															    _q100 = do_valueize (valueize, _q100);
															    switch (TREE_CODE (_q100))
															      {
															      case SSA_NAME:
															        if (gimple *_d8 = get_def (valueize, _q100))
															          {
																    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																      switch (gimple_assign_rhs_code (_a8))
																	{
																	case LT_EXPR:
																	  {
																	    tree _q110 = gimple_assign_rhs1 (_a8);
																	    _q110 = do_valueize (valueize, _q110);
																	    tree _q111 = gimple_assign_rhs2 (_a8);
																	    _q111 = do_valueize (valueize, _q111);
																	    if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
																	      {
																		if (integer_zerop (_q111))
																		  {
																		    if (gimple_max_value (_q81, valueize))
																		      {
																			switch (TREE_CODE (_p2))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d9 = get_def (valueize, _p2))
																			      {
																				if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																				  switch (gimple_assign_rhs_code (_a9))
																				    {
																				    case REALPART_EXPR:
																				      {
																					tree _q150 = TREE_OPERAND (gimple_assign_rhs1 (_a9), 0);
																					if ((TREE_CODE (_q150) == SSA_NAME
																					     || is_gimple_min_invariant (_q150)))
																					  {
																					    _q150 = do_valueize (valueize, _q150);
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
																							if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 74, __FILE__, __LINE__, false);
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
															        break;
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
													  }
													default:;
													}
											          }
											        break;
											      default:;
											      }
											  if (gimple_max_value (_q80, valueize))
											    {
											      switch (TREE_CODE (_q81))
											        {
												case SSA_NAME:
												  if (gimple *_d6 = get_def (valueize, _q81))
												    {
												      if (gassign *_a6 = dyn_cast <gassign *> (_d6))
												        switch (gimple_assign_rhs_code (_a6))
													  {
													  case NEGATE_EXPR:
													    {
													      tree _q100 = gimple_assign_rhs1 (_a6);
													      _q100 = do_valueize (valueize, _q100);
													      switch (TREE_CODE (_q100))
													        {
														case SSA_NAME:
														  if (gimple *_d7 = get_def (valueize, _q100))
														    {
														      if (gassign *_a7 = dyn_cast <gassign *> (_d7))
														        switch (gimple_assign_rhs_code (_a7))
															  {
															  CASE_CONVERT:
															    {
															      tree _q110 = gimple_assign_rhs1 (_a7);
															      _q110 = do_valueize (valueize, _q110);
															      switch (TREE_CODE (_q110))
															        {
																case SSA_NAME:
																  if (gimple *_d8 = get_def (valueize, _q110))
																    {
																      if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																        switch (gimple_assign_rhs_code (_a8))
																	  {
																	  case LT_EXPR:
																	    {
																	      tree _q120 = gimple_assign_rhs1 (_a8);
																	      _q120 = do_valueize (valueize, _q120);
																	      tree _q121 = gimple_assign_rhs2 (_a8);
																	      _q121 = do_valueize (valueize, _q121);
																	      if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
																		{
																		  if (integer_zerop (_q121))
																		    {
																		      switch (TREE_CODE (_p2))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d9 = get_def (valueize, _p2))
																			    {
																			      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																			        switch (gimple_assign_rhs_code (_a9))
																				  {
																				  case REALPART_EXPR:
																				    {
																				      tree _q150 = TREE_OPERAND (gimple_assign_rhs1 (_a9), 0);
																				      if ((TREE_CODE (_q150) == SSA_NAME
																				           || is_gimple_min_invariant (_q150)))
																				        {
																					  _q150 = do_valueize (valueize, _q150);
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
																						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 74, __FILE__, __LINE__, false);
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
															    }
															  default:;
															  }
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
									  }
								      }
								    break;
								  default:;
								  }
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
			        }
			      case BIT_AND_EXPR:
			        {
				  tree _q20 = gimple_assign_rhs1 (_a2);
				  _q20 = do_valueize (valueize, _q20);
				  tree _q21 = gimple_assign_rhs2 (_a2);
				  _q21 = do_valueize (valueize, _q21);
				  if (tree_swap_operands_p (_q20, _q21))
				    std::swap (_q20, _q21);
				  switch (TREE_CODE (_q20))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q20))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case LT_EXPR:
					        {
						  tree _q30 = gimple_assign_rhs1 (_a3);
						  _q30 = do_valueize (valueize, _q30);
						  tree _q31 = gimple_assign_rhs2 (_a3);
						  _q31 = do_valueize (valueize, _q31);
						  switch (TREE_CODE (_q30))
						    {
						    case SSA_NAME:
						      if (gimple *_d4 = get_def (valueize, _q30))
						        {
							  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
							    switch (gimple_assign_rhs_code (_a4))
							      {
							      case BIT_XOR_EXPR:
							        {
								  tree _q40 = gimple_assign_rhs1 (_a4);
								  _q40 = do_valueize (valueize, _q40);
								  tree _q41 = gimple_assign_rhs2 (_a4);
								  _q41 = do_valueize (valueize, _q41);
								  if (tree_swap_operands_p (_q40, _q41))
								    std::swap (_q40, _q41);
								  {
								    tree _q41_pops[1];
								    if (gimple_nop_convert (_q41, _q41_pops, valueize))
								      {
									tree _q60 = _q41_pops[0];
									switch (TREE_CODE (_q60))
									  {
									  case SSA_NAME:
									    if (gimple *_d5 = get_def (valueize, _q60))
									      {
										if (gassign *_a5 = dyn_cast <gassign *> (_d5))
										  switch (gimple_assign_rhs_code (_a5))
										    {
										    case MINUS_EXPR:
										      {
											tree _q70 = gimple_assign_rhs1 (_a5);
											_q70 = do_valueize (valueize, _q70);
											tree _q71 = gimple_assign_rhs2 (_a5);
											_q71 = do_valueize (valueize, _q71);
											{
											  tree _q70_pops[1];
											  if (gimple_nop_convert (_q70, _q70_pops, valueize))
											    {
											      tree _q80 = _q70_pops[0];
											      if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
												{
												  {
												    tree _q71_pops[1];
												    if (gimple_nop_convert (_q71, _q71_pops, valueize))
												      {
													tree _q100 = _q71_pops[0];
													if (integer_zerop (_q31))
													  {
													    switch (TREE_CODE (_q21))
													      {
													      case SSA_NAME:
													        if (gimple *_d6 = get_def (valueize, _q21))
													          {
														    if (gassign *_a6 = dyn_cast <gassign *> (_d6))
														      switch (gimple_assign_rhs_code (_a6))
															{
															case LT_EXPR:
															  {
															    tree _q130 = gimple_assign_rhs1 (_a6);
															    _q130 = do_valueize (valueize, _q130);
															    tree _q131 = gimple_assign_rhs2 (_a6);
															    _q131 = do_valueize (valueize, _q131);
															    switch (TREE_CODE (_q130))
															      {
															      case SSA_NAME:
															        if (gimple *_d7 = get_def (valueize, _q130))
															          {
																    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																      switch (gimple_assign_rhs_code (_a7))
																	{
																	case BIT_XOR_EXPR:
																	  {
																	    tree _q140 = gimple_assign_rhs1 (_a7);
																	    _q140 = do_valueize (valueize, _q140);
																	    tree _q141 = gimple_assign_rhs2 (_a7);
																	    _q141 = do_valueize (valueize, _q141);
																	    if (tree_swap_operands_p (_q140, _q141))
																	      std::swap (_q140, _q141);
																	    if ((_q140 == _q40 && ! TREE_SIDE_EFFECTS (_q140)) || (operand_equal_p (_q140, _q40, 0) && types_match (_q140, _q40)))
																	      {
																		if ((_q141 == _q100 && ! TREE_SIDE_EFFECTS (_q141)) || (operand_equal_p (_q141, _q100, 0) && types_match (_q141, _q100)))
																		  {
																		    if (integer_zerop (_q131))
																		      {
																			switch (TREE_CODE (_p1))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d8 = get_def (valueize, _p1))
																			      {
																				if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				  switch (gimple_assign_rhs_code (_a8))
																				    {
																				    case BIT_XOR_EXPR:
																				      {
																					tree _q180 = gimple_assign_rhs1 (_a8);
																					_q180 = do_valueize (valueize, _q180);
																					tree _q181 = gimple_assign_rhs2 (_a8);
																					_q181 = do_valueize (valueize, _q181);
																					if (tree_swap_operands_p (_q180, _q181))
																					  std::swap (_q180, _q181);
																					{
																					  tree _q180_pops[1];
																					  if (gimple_nop_convert (_q180, _q180_pops, valueize))
																					    {
																					      tree _q190 = _q180_pops[0];
																					      switch (TREE_CODE (_q190))
																					        {
																						case SSA_NAME:
																						  if (gimple *_d9 = get_def (valueize, _q190))
																						    {
																						      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						        switch (gimple_assign_rhs_code (_a9))
																							  {
																							  case NEGATE_EXPR:
																							    {
																							      tree _q200 = gimple_assign_rhs1 (_a9);
																							      _q200 = do_valueize (valueize, _q200);
																							      {
																								tree _q200_pops[1];
																								if (gimple_nop_convert (_q200, _q200_pops, valueize))
																								  {
																								    tree _q210 = _q200_pops[0];
																								    switch (TREE_CODE (_q210))
																								      {
																								      case SSA_NAME:
																								        if (gimple *_d10 = get_def (valueize, _q210))
																								          {
																									    if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																									      switch (gimple_assign_rhs_code (_a10))
																										{
																										CASE_CONVERT:
																										  {
																										    tree _q220 = gimple_assign_rhs1 (_a10);
																										    _q220 = do_valueize (valueize, _q220);
																										    switch (TREE_CODE (_q220))
																										      {
																										      case SSA_NAME:
																										        if (gimple *_d11 = get_def (valueize, _q220))
																										          {
																											    if (gassign *_a11 = dyn_cast <gassign *> (_d11))
																											      switch (gimple_assign_rhs_code (_a11))
																												{
																												case LT_EXPR:
																												  {
																												    tree _q230 = gimple_assign_rhs1 (_a11);
																												    _q230 = do_valueize (valueize, _q230);
																												    tree _q231 = gimple_assign_rhs2 (_a11);
																												    _q231 = do_valueize (valueize, _q231);
																												    if ((_q230 == _q40 && ! TREE_SIDE_EFFECTS (_q230)) || (operand_equal_p (_q230, _q40, 0) && types_match (_q230, _q40)))
																												      {
																													if (integer_zerop (_q231))
																													  {
																													    if (gimple_max_value (_q181, valueize))
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
																															    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																					    }
																					}
																					if (gimple_max_value (_q180, valueize))
																					  {
																					    {
																					      tree _q181_pops[1];
																					      if (gimple_nop_convert (_q181, _q181_pops, valueize))
																					        {
																						  tree _q200 = _q181_pops[0];
																						  switch (TREE_CODE (_q200))
																						    {
																						    case SSA_NAME:
																						      if (gimple *_d9 = get_def (valueize, _q200))
																						        {
																							  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							    switch (gimple_assign_rhs_code (_a9))
																							      {
																							      case NEGATE_EXPR:
																							        {
																								  tree _q210 = gimple_assign_rhs1 (_a9);
																								  _q210 = do_valueize (valueize, _q210);
																								  {
																								    tree _q210_pops[1];
																								    if (gimple_nop_convert (_q210, _q210_pops, valueize))
																								      {
																									tree _q220 = _q210_pops[0];
																									switch (TREE_CODE (_q220))
																									  {
																									  case SSA_NAME:
																									    if (gimple *_d10 = get_def (valueize, _q220))
																									      {
																										if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																										  switch (gimple_assign_rhs_code (_a10))
																										    {
																										    CASE_CONVERT:
																										      {
																											tree _q230 = gimple_assign_rhs1 (_a10);
																											_q230 = do_valueize (valueize, _q230);
																											switch (TREE_CODE (_q230))
																											  {
																											  case SSA_NAME:
																											    if (gimple *_d11 = get_def (valueize, _q230))
																											      {
																												if (gassign *_a11 = dyn_cast <gassign *> (_d11))
																												  switch (gimple_assign_rhs_code (_a11))
																												    {
																												    case LT_EXPR:
																												      {
																													tree _q240 = gimple_assign_rhs1 (_a11);
																													_q240 = do_valueize (valueize, _q240);
																													tree _q241 = gimple_assign_rhs2 (_a11);
																													_q241 = do_valueize (valueize, _q241);
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
																															    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																											    break;
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
																			  case SSA_NAME:
																			    if (gimple *_d8 = get_def (valueize, _p1))
																			      {
																				if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				  switch (gimple_assign_rhs_code (_a8))
																				    {
																				    case BIT_XOR_EXPR:
																				      {
																					tree _q180 = gimple_assign_rhs1 (_a8);
																					_q180 = do_valueize (valueize, _q180);
																					tree _q181 = gimple_assign_rhs2 (_a8);
																					_q181 = do_valueize (valueize, _q181);
																					if (tree_swap_operands_p (_q180, _q181))
																					  std::swap (_q180, _q181);
																					{
																					  tree _q180_pops[1];
																					  if (gimple_nop_convert (_q180, _q180_pops, valueize))
																					    {
																					      tree _q190 = _q180_pops[0];
																					      switch (TREE_CODE (_q190))
																					        {
																						case SSA_NAME:
																						  if (gimple *_d9 = get_def (valueize, _q190))
																						    {
																						      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						        switch (gimple_assign_rhs_code (_a9))
																							  {
																							  case NEGATE_EXPR:
																							    {
																							      tree _q200 = gimple_assign_rhs1 (_a9);
																							      _q200 = do_valueize (valueize, _q200);
																							      {
																								tree _q200_pops[1];
																								if (gimple_nop_convert (_q200, _q200_pops, valueize))
																								  {
																								    tree _q210 = _q200_pops[0];
																								    switch (TREE_CODE (_q210))
																								      {
																								      case SSA_NAME:
																								        if (gimple *_d10 = get_def (valueize, _q210))
																								          {
																									    if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																									      switch (gimple_assign_rhs_code (_a10))
																										{
																										CASE_CONVERT:
																										  {
																										    tree _q220 = gimple_assign_rhs1 (_a10);
																										    _q220 = do_valueize (valueize, _q220);
																										    switch (TREE_CODE (_q220))
																										      {
																										      case SSA_NAME:
																										        if (gimple *_d11 = get_def (valueize, _q220))
																										          {
																											    if (gassign *_a11 = dyn_cast <gassign *> (_d11))
																											      switch (gimple_assign_rhs_code (_a11))
																												{
																												case LT_EXPR:
																												  {
																												    tree _q230 = gimple_assign_rhs1 (_a11);
																												    _q230 = do_valueize (valueize, _q230);
																												    tree _q231 = gimple_assign_rhs2 (_a11);
																												    _q231 = do_valueize (valueize, _q231);
																												    if ((_q230 == _q40 && ! TREE_SIDE_EFFECTS (_q230)) || (operand_equal_p (_q230, _q40, 0) && types_match (_q230, _q40)))
																												      {
																													if (integer_zerop (_q231))
																													  {
																													    if (gimple_max_value (_q181, valueize))
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
																															    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																					    }
																					}
																					if (gimple_max_value (_q180, valueize))
																					  {
																					    {
																					      tree _q181_pops[1];
																					      if (gimple_nop_convert (_q181, _q181_pops, valueize))
																					        {
																						  tree _q200 = _q181_pops[0];
																						  switch (TREE_CODE (_q200))
																						    {
																						    case SSA_NAME:
																						      if (gimple *_d9 = get_def (valueize, _q200))
																						        {
																							  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							    switch (gimple_assign_rhs_code (_a9))
																							      {
																							      case NEGATE_EXPR:
																							        {
																								  tree _q210 = gimple_assign_rhs1 (_a9);
																								  _q210 = do_valueize (valueize, _q210);
																								  {
																								    tree _q210_pops[1];
																								    if (gimple_nop_convert (_q210, _q210_pops, valueize))
																								      {
																									tree _q220 = _q210_pops[0];
																									switch (TREE_CODE (_q220))
																									  {
																									  case SSA_NAME:
																									    if (gimple *_d10 = get_def (valueize, _q220))
																									      {
																										if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																										  switch (gimple_assign_rhs_code (_a10))
																										    {
																										    CASE_CONVERT:
																										      {
																											tree _q230 = gimple_assign_rhs1 (_a10);
																											_q230 = do_valueize (valueize, _q230);
																											switch (TREE_CODE (_q230))
																											  {
																											  case SSA_NAME:
																											    if (gimple *_d11 = get_def (valueize, _q230))
																											      {
																												if (gassign *_a11 = dyn_cast <gassign *> (_d11))
																												  switch (gimple_assign_rhs_code (_a11))
																												    {
																												    case LT_EXPR:
																												      {
																													tree _q240 = gimple_assign_rhs1 (_a11);
																													_q240 = do_valueize (valueize, _q240);
																													tree _q241 = gimple_assign_rhs2 (_a11);
																													_q241 = do_valueize (valueize, _q241);
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
																															    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																											    break;
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
															  }
															default:;
															}
													          }
													        break;
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
									    break;
								          default:;
								          }
								      }
								  }
								  if (integer_zerop (_q31))
								    {
								      switch (TREE_CODE (_q21))
								        {
									case SSA_NAME:
									  if (gimple *_d5 = get_def (valueize, _q21))
									    {
									      if (gassign *_a5 = dyn_cast <gassign *> (_d5))
									        switch (gimple_assign_rhs_code (_a5))
										  {
										  case LT_EXPR:
										    {
										      tree _q80 = gimple_assign_rhs1 (_a5);
										      _q80 = do_valueize (valueize, _q80);
										      tree _q81 = gimple_assign_rhs2 (_a5);
										      _q81 = do_valueize (valueize, _q81);
										      switch (TREE_CODE (_q80))
										        {
											case SSA_NAME:
											  if (gimple *_d6 = get_def (valueize, _q80))
											    {
											      if (gassign *_a6 = dyn_cast <gassign *> (_d6))
											        switch (gimple_assign_rhs_code (_a6))
												  {
												  case BIT_XOR_EXPR:
												    {
												      tree _q90 = gimple_assign_rhs1 (_a6);
												      _q90 = do_valueize (valueize, _q90);
												      tree _q91 = gimple_assign_rhs2 (_a6);
												      _q91 = do_valueize (valueize, _q91);
												      if (tree_swap_operands_p (_q90, _q91))
												        std::swap (_q90, _q91);
												      if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
													{
													  {
													    tree _q91_pops[1];
													    if (gimple_nop_convert (_q91, _q91_pops, valueize))
													      {
														tree _q110 = _q91_pops[0];
														switch (TREE_CODE (_q110))
														  {
														  case SSA_NAME:
														    if (gimple *_d7 = get_def (valueize, _q110))
														      {
															if (gassign *_a7 = dyn_cast <gassign *> (_d7))
															  switch (gimple_assign_rhs_code (_a7))
															    {
															    case MINUS_EXPR:
															      {
																tree _q120 = gimple_assign_rhs1 (_a7);
																_q120 = do_valueize (valueize, _q120);
																tree _q121 = gimple_assign_rhs2 (_a7);
																_q121 = do_valueize (valueize, _q121);
																{
																  tree _q120_pops[1];
																  if (gimple_nop_convert (_q120, _q120_pops, valueize))
																    {
																      tree _q130 = _q120_pops[0];
																      if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
																	{
																	  {
																	    tree _q121_pops[1];
																	    if (gimple_nop_convert (_q121, _q121_pops, valueize))
																	      {
																		tree _q150 = _q121_pops[0];
																		if ((_q150 == _q41 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q41, 0) && types_match (_q150, _q41)))
																		  {
																		    if (integer_zerop (_q81))
																		      {
																			switch (TREE_CODE (_p1))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d8 = get_def (valueize, _p1))
																			      {
																				if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				  switch (gimple_assign_rhs_code (_a8))
																				    {
																				    case BIT_XOR_EXPR:
																				      {
																					tree _q180 = gimple_assign_rhs1 (_a8);
																					_q180 = do_valueize (valueize, _q180);
																					tree _q181 = gimple_assign_rhs2 (_a8);
																					_q181 = do_valueize (valueize, _q181);
																					if (tree_swap_operands_p (_q180, _q181))
																					  std::swap (_q180, _q181);
																					{
																					  tree _q180_pops[1];
																					  if (gimple_nop_convert (_q180, _q180_pops, valueize))
																					    {
																					      tree _q190 = _q180_pops[0];
																					      switch (TREE_CODE (_q190))
																					        {
																						case SSA_NAME:
																						  if (gimple *_d9 = get_def (valueize, _q190))
																						    {
																						      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						        switch (gimple_assign_rhs_code (_a9))
																							  {
																							  case NEGATE_EXPR:
																							    {
																							      tree _q200 = gimple_assign_rhs1 (_a9);
																							      _q200 = do_valueize (valueize, _q200);
																							      {
																								tree _q200_pops[1];
																								if (gimple_nop_convert (_q200, _q200_pops, valueize))
																								  {
																								    tree _q210 = _q200_pops[0];
																								    switch (TREE_CODE (_q210))
																								      {
																								      case SSA_NAME:
																								        if (gimple *_d10 = get_def (valueize, _q210))
																								          {
																									    if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																									      switch (gimple_assign_rhs_code (_a10))
																										{
																										CASE_CONVERT:
																										  {
																										    tree _q220 = gimple_assign_rhs1 (_a10);
																										    _q220 = do_valueize (valueize, _q220);
																										    switch (TREE_CODE (_q220))
																										      {
																										      case SSA_NAME:
																										        if (gimple *_d11 = get_def (valueize, _q220))
																										          {
																											    if (gassign *_a11 = dyn_cast <gassign *> (_d11))
																											      switch (gimple_assign_rhs_code (_a11))
																												{
																												case LT_EXPR:
																												  {
																												    tree _q230 = gimple_assign_rhs1 (_a11);
																												    _q230 = do_valueize (valueize, _q230);
																												    tree _q231 = gimple_assign_rhs2 (_a11);
																												    _q231 = do_valueize (valueize, _q231);
																												    if ((_q230 == _q40 && ! TREE_SIDE_EFFECTS (_q230)) || (operand_equal_p (_q230, _q40, 0) && types_match (_q230, _q40)))
																												      {
																													if (integer_zerop (_q231))
																													  {
																													    if (gimple_max_value (_q181, valueize))
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
																															    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																					    }
																					}
																					if (gimple_max_value (_q180, valueize))
																					  {
																					    {
																					      tree _q181_pops[1];
																					      if (gimple_nop_convert (_q181, _q181_pops, valueize))
																					        {
																						  tree _q200 = _q181_pops[0];
																						  switch (TREE_CODE (_q200))
																						    {
																						    case SSA_NAME:
																						      if (gimple *_d9 = get_def (valueize, _q200))
																						        {
																							  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							    switch (gimple_assign_rhs_code (_a9))
																							      {
																							      case NEGATE_EXPR:
																							        {
																								  tree _q210 = gimple_assign_rhs1 (_a9);
																								  _q210 = do_valueize (valueize, _q210);
																								  {
																								    tree _q210_pops[1];
																								    if (gimple_nop_convert (_q210, _q210_pops, valueize))
																								      {
																									tree _q220 = _q210_pops[0];
																									switch (TREE_CODE (_q220))
																									  {
																									  case SSA_NAME:
																									    if (gimple *_d10 = get_def (valueize, _q220))
																									      {
																										if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																										  switch (gimple_assign_rhs_code (_a10))
																										    {
																										    CASE_CONVERT:
																										      {
																											tree _q230 = gimple_assign_rhs1 (_a10);
																											_q230 = do_valueize (valueize, _q230);
																											switch (TREE_CODE (_q230))
																											  {
																											  case SSA_NAME:
																											    if (gimple *_d11 = get_def (valueize, _q230))
																											      {
																												if (gassign *_a11 = dyn_cast <gassign *> (_d11))
																												  switch (gimple_assign_rhs_code (_a11))
																												    {
																												    case LT_EXPR:
																												      {
																													tree _q240 = gimple_assign_rhs1 (_a11);
																													_q240 = do_valueize (valueize, _q240);
																													tree _q241 = gimple_assign_rhs2 (_a11);
																													_q241 = do_valueize (valueize, _q241);
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
																															    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																											    break;
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
													          default:;
													          }
													      }
													  }
												        }
												      if ((_q90 == _q41 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q41, 0) && types_match (_q90, _q41)))
													{
													  {
													    tree _q91_pops[1];
													    if (gimple_nop_convert (_q91, _q91_pops, valueize))
													      {
														tree _q110 = _q91_pops[0];
														switch (TREE_CODE (_q110))
														  {
														  case SSA_NAME:
														    if (gimple *_d7 = get_def (valueize, _q110))
														      {
															if (gassign *_a7 = dyn_cast <gassign *> (_d7))
															  switch (gimple_assign_rhs_code (_a7))
															    {
															    case MINUS_EXPR:
															      {
																tree _q120 = gimple_assign_rhs1 (_a7);
																_q120 = do_valueize (valueize, _q120);
																tree _q121 = gimple_assign_rhs2 (_a7);
																_q121 = do_valueize (valueize, _q121);
																{
																  tree _q120_pops[1];
																  if (gimple_nop_convert (_q120, _q120_pops, valueize))
																    {
																      tree _q130 = _q120_pops[0];
																      if ((_q130 == _q41 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q41, 0) && types_match (_q130, _q41)))
																	{
																	  {
																	    tree _q121_pops[1];
																	    if (gimple_nop_convert (_q121, _q121_pops, valueize))
																	      {
																		tree _q150 = _q121_pops[0];
																		if ((_q150 == _q40 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q40, 0) && types_match (_q150, _q40)))
																		  {
																		    if (integer_zerop (_q81))
																		      {
																			switch (TREE_CODE (_p1))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d8 = get_def (valueize, _p1))
																			      {
																				if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																				  switch (gimple_assign_rhs_code (_a8))
																				    {
																				    case BIT_XOR_EXPR:
																				      {
																					tree _q180 = gimple_assign_rhs1 (_a8);
																					_q180 = do_valueize (valueize, _q180);
																					tree _q181 = gimple_assign_rhs2 (_a8);
																					_q181 = do_valueize (valueize, _q181);
																					if (tree_swap_operands_p (_q180, _q181))
																					  std::swap (_q180, _q181);
																					{
																					  tree _q180_pops[1];
																					  if (gimple_nop_convert (_q180, _q180_pops, valueize))
																					    {
																					      tree _q190 = _q180_pops[0];
																					      switch (TREE_CODE (_q190))
																					        {
																						case SSA_NAME:
																						  if (gimple *_d9 = get_def (valueize, _q190))
																						    {
																						      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																						        switch (gimple_assign_rhs_code (_a9))
																							  {
																							  case NEGATE_EXPR:
																							    {
																							      tree _q200 = gimple_assign_rhs1 (_a9);
																							      _q200 = do_valueize (valueize, _q200);
																							      {
																								tree _q200_pops[1];
																								if (gimple_nop_convert (_q200, _q200_pops, valueize))
																								  {
																								    tree _q210 = _q200_pops[0];
																								    switch (TREE_CODE (_q210))
																								      {
																								      case SSA_NAME:
																								        if (gimple *_d10 = get_def (valueize, _q210))
																								          {
																									    if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																									      switch (gimple_assign_rhs_code (_a10))
																										{
																										CASE_CONVERT:
																										  {
																										    tree _q220 = gimple_assign_rhs1 (_a10);
																										    _q220 = do_valueize (valueize, _q220);
																										    switch (TREE_CODE (_q220))
																										      {
																										      case SSA_NAME:
																										        if (gimple *_d11 = get_def (valueize, _q220))
																										          {
																											    if (gassign *_a11 = dyn_cast <gassign *> (_d11))
																											      switch (gimple_assign_rhs_code (_a11))
																												{
																												case LT_EXPR:
																												  {
																												    tree _q230 = gimple_assign_rhs1 (_a11);
																												    _q230 = do_valueize (valueize, _q230);
																												    tree _q231 = gimple_assign_rhs2 (_a11);
																												    _q231 = do_valueize (valueize, _q231);
																												    if ((_q230 == _q41 && ! TREE_SIDE_EFFECTS (_q230)) || (operand_equal_p (_q230, _q41, 0) && types_match (_q230, _q41)))
																												      {
																													if (integer_zerop (_q231))
																													  {
																													    if (gimple_max_value (_q181, valueize))
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
																															    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																					    }
																					}
																					if (gimple_max_value (_q180, valueize))
																					  {
																					    {
																					      tree _q181_pops[1];
																					      if (gimple_nop_convert (_q181, _q181_pops, valueize))
																					        {
																						  tree _q200 = _q181_pops[0];
																						  switch (TREE_CODE (_q200))
																						    {
																						    case SSA_NAME:
																						      if (gimple *_d9 = get_def (valueize, _q200))
																						        {
																							  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							    switch (gimple_assign_rhs_code (_a9))
																							      {
																							      case NEGATE_EXPR:
																							        {
																								  tree _q210 = gimple_assign_rhs1 (_a9);
																								  _q210 = do_valueize (valueize, _q210);
																								  {
																								    tree _q210_pops[1];
																								    if (gimple_nop_convert (_q210, _q210_pops, valueize))
																								      {
																									tree _q220 = _q210_pops[0];
																									switch (TREE_CODE (_q220))
																									  {
																									  case SSA_NAME:
																									    if (gimple *_d10 = get_def (valueize, _q220))
																									      {
																										if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																										  switch (gimple_assign_rhs_code (_a10))
																										    {
																										    CASE_CONVERT:
																										      {
																											tree _q230 = gimple_assign_rhs1 (_a10);
																											_q230 = do_valueize (valueize, _q230);
																											switch (TREE_CODE (_q230))
																											  {
																											  case SSA_NAME:
																											    if (gimple *_d11 = get_def (valueize, _q230))
																											      {
																												if (gassign *_a11 = dyn_cast <gassign *> (_d11))
																												  switch (gimple_assign_rhs_code (_a11))
																												    {
																												    case LT_EXPR:
																												      {
																													tree _q240 = gimple_assign_rhs1 (_a11);
																													_q240 = do_valueize (valueize, _q240);
																													tree _q241 = gimple_assign_rhs2 (_a11);
																													_q241 = do_valueize (valueize, _q241);
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
																															    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																											    break;
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
											  break;
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
					        }
					      default:;
					      }
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
		    case LT_EXPR:
		      {
			tree _q20 = TREE_OPERAND (_p0, 0);
			if ((TREE_CODE (_q20) == SSA_NAME
			     || is_gimple_min_invariant (_q20)))
			  {
			    _q20 = do_valueize (valueize, _q20);
			    tree _q21 = TREE_OPERAND (_p0, 1);
			    if ((TREE_CODE (_q21) == SSA_NAME
			         || is_gimple_min_invariant (_q21)))
			      {
				_q21 = do_valueize (valueize, _q21);
				switch (TREE_CODE (_q20))
				  {
				  case SSA_NAME:
				    if (gimple *_d2 = get_def (valueize, _q20))
				      {
					if (gassign *_a2 = dyn_cast <gassign *> (_d2))
					  switch (gimple_assign_rhs_code (_a2))
					    {
					    case BIT_AND_EXPR:
					      {
						tree _q30 = gimple_assign_rhs1 (_a2);
						_q30 = do_valueize (valueize, _q30);
						tree _q31 = gimple_assign_rhs2 (_a2);
						_q31 = do_valueize (valueize, _q31);
						if (tree_swap_operands_p (_q30, _q31))
						  std::swap (_q30, _q31);
						switch (TREE_CODE (_q30))
						  {
						  case SSA_NAME:
						    if (gimple *_d3 = get_def (valueize, _q30))
						      {
							if (gassign *_a3 = dyn_cast <gassign *> (_d3))
							  switch (gimple_assign_rhs_code (_a3))
							    {
							    case BIT_XOR_EXPR:
							      {
								tree _q40 = gimple_assign_rhs1 (_a3);
								_q40 = do_valueize (valueize, _q40);
								tree _q41 = gimple_assign_rhs2 (_a3);
								_q41 = do_valueize (valueize, _q41);
								if (tree_swap_operands_p (_q40, _q41))
								  std::swap (_q40, _q41);
								switch (TREE_CODE (_q31))
								  {
								  case SSA_NAME:
								    if (gimple *_d4 = get_def (valueize, _q31))
								      {
									if (gassign *_a4 = dyn_cast <gassign *> (_d4))
									  switch (gimple_assign_rhs_code (_a4))
									    {
									    case BIT_XOR_EXPR:
									      {
										tree _q70 = gimple_assign_rhs1 (_a4);
										_q70 = do_valueize (valueize, _q70);
										tree _q71 = gimple_assign_rhs2 (_a4);
										_q71 = do_valueize (valueize, _q71);
										if (tree_swap_operands_p (_q70, _q71))
										  std::swap (_q70, _q71);
										if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
										  {
										    {
										      tree _q71_pops[1];
										      if (gimple_nop_convert (_q71, _q71_pops, valueize))
										        {
											  tree _q90 = _q71_pops[0];
											  switch (TREE_CODE (_q90))
											    {
											    case SSA_NAME:
											      if (gimple *_d5 = get_def (valueize, _q90))
											        {
												  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
												    switch (gimple_assign_rhs_code (_a5))
												      {
												      case MINUS_EXPR:
												        {
													  tree _q100 = gimple_assign_rhs1 (_a5);
													  _q100 = do_valueize (valueize, _q100);
													  tree _q101 = gimple_assign_rhs2 (_a5);
													  _q101 = do_valueize (valueize, _q101);
													  {
													    tree _q100_pops[1];
													    if (gimple_nop_convert (_q100, _q100_pops, valueize))
													      {
														tree _q110 = _q100_pops[0];
														if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
														  {
														    {
														      tree _q101_pops[1];
														      if (gimple_nop_convert (_q101, _q101_pops, valueize))
														        {
															  tree _q130 = _q101_pops[0];
															  if ((_q130 == _q41 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q41, 0) && types_match (_q130, _q41)))
															    {
															      if (integer_zerop (_q21))
																{
																  switch (TREE_CODE (_p1))
																    {
																    case SSA_NAME:
																      if (gimple *_d6 = get_def (valueize, _p1))
																        {
																	  if (gassign *_a6 = dyn_cast <gassign *> (_d6))
																	    switch (gimple_assign_rhs_code (_a6))
																	      {
																	      case BIT_XOR_EXPR:
																	        {
																		  tree _q160 = gimple_assign_rhs1 (_a6);
																		  _q160 = do_valueize (valueize, _q160);
																		  tree _q161 = gimple_assign_rhs2 (_a6);
																		  _q161 = do_valueize (valueize, _q161);
																		  if (tree_swap_operands_p (_q160, _q161))
																		    std::swap (_q160, _q161);
																		  switch (TREE_CODE (_q160))
																		    {
																		    case SSA_NAME:
																		      if (gimple *_d7 = get_def (valueize, _q160))
																		        {
																			  if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			    switch (gimple_assign_rhs_code (_a7))
																			      {
																			      case NEGATE_EXPR:
																			        {
																				  tree _q170 = gimple_assign_rhs1 (_a7);
																				  _q170 = do_valueize (valueize, _q170);
																				  switch (TREE_CODE (_q170))
																				    {
																				    case SSA_NAME:
																				      if (gimple *_d8 = get_def (valueize, _q170))
																				        {
																					  if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					    switch (gimple_assign_rhs_code (_a8))
																					      {
																					      CASE_CONVERT:
																					        {
																						  tree _q180 = gimple_assign_rhs1 (_a8);
																						  _q180 = do_valueize (valueize, _q180);
																						  switch (TREE_CODE (_q180))
																						    {
																						    case SSA_NAME:
																						      if (gimple *_d9 = get_def (valueize, _q180))
																						        {
																							  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							    switch (gimple_assign_rhs_code (_a9))
																							      {
																							      case LT_EXPR:
																							        {
																								  tree _q190 = gimple_assign_rhs1 (_a9);
																								  _q190 = do_valueize (valueize, _q190);
																								  tree _q191 = gimple_assign_rhs2 (_a9);
																								  _q191 = do_valueize (valueize, _q191);
																								  if ((_q190 == _q40 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q40, 0) && types_match (_q190, _q40)))
																								    {
																								      if (integer_zerop (_q191))
																									{
																									  if (gimple_max_value (_q161, valueize))
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																		if (gimple_max_value (_q160, valueize))
																		  {
																		    switch (TREE_CODE (_q161))
																		      {
																		      case SSA_NAME:
																		        if (gimple *_d7 = get_def (valueize, _q161))
																		          {
																			    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			      switch (gimple_assign_rhs_code (_a7))
																				{
																				case NEGATE_EXPR:
																				  {
																				    tree _q180 = gimple_assign_rhs1 (_a7);
																				    _q180 = do_valueize (valueize, _q180);
																				    switch (TREE_CODE (_q180))
																				      {
																				      case SSA_NAME:
																				        if (gimple *_d8 = get_def (valueize, _q180))
																				          {
																					    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					      switch (gimple_assign_rhs_code (_a8))
																						{
																						CASE_CONVERT:
																						  {
																						    tree _q190 = gimple_assign_rhs1 (_a8);
																						    _q190 = do_valueize (valueize, _q190);
																						    switch (TREE_CODE (_q190))
																						      {
																						      case SSA_NAME:
																						        if (gimple *_d9 = get_def (valueize, _q190))
																						          {
																							    if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							      switch (gimple_assign_rhs_code (_a9))
																								{
																								case LT_EXPR:
																								  {
																								    tree _q200 = gimple_assign_rhs1 (_a9);
																								    _q200 = do_valueize (valueize, _q200);
																								    tree _q201 = gimple_assign_rhs2 (_a9);
																								    _q201 = do_valueize (valueize, _q201);
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
																											if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																						        break;
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
																				  }
																				default:;
																				}
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
											    default:;
											    }
										        }
										    }
										  }
										if ((_q70 == _q41 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q41, 0) && types_match (_q70, _q41)))
										  {
										    {
										      tree _q71_pops[1];
										      if (gimple_nop_convert (_q71, _q71_pops, valueize))
										        {
											  tree _q90 = _q71_pops[0];
											  switch (TREE_CODE (_q90))
											    {
											    case SSA_NAME:
											      if (gimple *_d5 = get_def (valueize, _q90))
											        {
												  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
												    switch (gimple_assign_rhs_code (_a5))
												      {
												      case MINUS_EXPR:
												        {
													  tree _q100 = gimple_assign_rhs1 (_a5);
													  _q100 = do_valueize (valueize, _q100);
													  tree _q101 = gimple_assign_rhs2 (_a5);
													  _q101 = do_valueize (valueize, _q101);
													  {
													    tree _q100_pops[1];
													    if (gimple_nop_convert (_q100, _q100_pops, valueize))
													      {
														tree _q110 = _q100_pops[0];
														if ((_q110 == _q41 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q41, 0) && types_match (_q110, _q41)))
														  {
														    {
														      tree _q101_pops[1];
														      if (gimple_nop_convert (_q101, _q101_pops, valueize))
														        {
															  tree _q130 = _q101_pops[0];
															  if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
															    {
															      if (integer_zerop (_q21))
																{
																  switch (TREE_CODE (_p1))
																    {
																    case SSA_NAME:
																      if (gimple *_d6 = get_def (valueize, _p1))
																        {
																	  if (gassign *_a6 = dyn_cast <gassign *> (_d6))
																	    switch (gimple_assign_rhs_code (_a6))
																	      {
																	      case BIT_XOR_EXPR:
																	        {
																		  tree _q160 = gimple_assign_rhs1 (_a6);
																		  _q160 = do_valueize (valueize, _q160);
																		  tree _q161 = gimple_assign_rhs2 (_a6);
																		  _q161 = do_valueize (valueize, _q161);
																		  if (tree_swap_operands_p (_q160, _q161))
																		    std::swap (_q160, _q161);
																		  switch (TREE_CODE (_q160))
																		    {
																		    case SSA_NAME:
																		      if (gimple *_d7 = get_def (valueize, _q160))
																		        {
																			  if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			    switch (gimple_assign_rhs_code (_a7))
																			      {
																			      case NEGATE_EXPR:
																			        {
																				  tree _q170 = gimple_assign_rhs1 (_a7);
																				  _q170 = do_valueize (valueize, _q170);
																				  switch (TREE_CODE (_q170))
																				    {
																				    case SSA_NAME:
																				      if (gimple *_d8 = get_def (valueize, _q170))
																				        {
																					  if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					    switch (gimple_assign_rhs_code (_a8))
																					      {
																					      CASE_CONVERT:
																					        {
																						  tree _q180 = gimple_assign_rhs1 (_a8);
																						  _q180 = do_valueize (valueize, _q180);
																						  switch (TREE_CODE (_q180))
																						    {
																						    case SSA_NAME:
																						      if (gimple *_d9 = get_def (valueize, _q180))
																						        {
																							  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							    switch (gimple_assign_rhs_code (_a9))
																							      {
																							      case LT_EXPR:
																							        {
																								  tree _q190 = gimple_assign_rhs1 (_a9);
																								  _q190 = do_valueize (valueize, _q190);
																								  tree _q191 = gimple_assign_rhs2 (_a9);
																								  _q191 = do_valueize (valueize, _q191);
																								  if ((_q190 == _q41 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q41, 0) && types_match (_q190, _q41)))
																								    {
																								      if (integer_zerop (_q191))
																									{
																									  if (gimple_max_value (_q161, valueize))
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																		if (gimple_max_value (_q160, valueize))
																		  {
																		    switch (TREE_CODE (_q161))
																		      {
																		      case SSA_NAME:
																		        if (gimple *_d7 = get_def (valueize, _q161))
																		          {
																			    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			      switch (gimple_assign_rhs_code (_a7))
																				{
																				case NEGATE_EXPR:
																				  {
																				    tree _q180 = gimple_assign_rhs1 (_a7);
																				    _q180 = do_valueize (valueize, _q180);
																				    switch (TREE_CODE (_q180))
																				      {
																				      case SSA_NAME:
																				        if (gimple *_d8 = get_def (valueize, _q180))
																				          {
																					    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					      switch (gimple_assign_rhs_code (_a8))
																						{
																						CASE_CONVERT:
																						  {
																						    tree _q190 = gimple_assign_rhs1 (_a8);
																						    _q190 = do_valueize (valueize, _q190);
																						    switch (TREE_CODE (_q190))
																						      {
																						      case SSA_NAME:
																						        if (gimple *_d9 = get_def (valueize, _q190))
																						          {
																							    if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							      switch (gimple_assign_rhs_code (_a9))
																								{
																								case LT_EXPR:
																								  {
																								    tree _q200 = gimple_assign_rhs1 (_a9);
																								    _q200 = do_valueize (valueize, _q200);
																								    tree _q201 = gimple_assign_rhs2 (_a9);
																								    _q201 = do_valueize (valueize, _q201);
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
																											if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																						        break;
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
																				  }
																				default:;
																				}
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
								    break;
							          default:;
							          }
								{
								  tree _q41_pops[1];
								  if (gimple_nop_convert (_q41, _q41_pops, valueize))
								    {
								      tree _q60 = _q41_pops[0];
								      switch (TREE_CODE (_q60))
								        {
									case SSA_NAME:
									  if (gimple *_d4 = get_def (valueize, _q60))
									    {
									      if (gassign *_a4 = dyn_cast <gassign *> (_d4))
									        switch (gimple_assign_rhs_code (_a4))
										  {
										  case MINUS_EXPR:
										    {
										      tree _q70 = gimple_assign_rhs1 (_a4);
										      _q70 = do_valueize (valueize, _q70);
										      tree _q71 = gimple_assign_rhs2 (_a4);
										      _q71 = do_valueize (valueize, _q71);
										      {
											tree _q70_pops[1];
											if (gimple_nop_convert (_q70, _q70_pops, valueize))
											  {
											    tree _q80 = _q70_pops[0];
											    if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
											      {
												{
												  tree _q71_pops[1];
												  if (gimple_nop_convert (_q71, _q71_pops, valueize))
												    {
												      tree _q100 = _q71_pops[0];
												      switch (TREE_CODE (_q31))
												        {
													case SSA_NAME:
													  if (gimple *_d5 = get_def (valueize, _q31))
													    {
													      if (gassign *_a5 = dyn_cast <gassign *> (_d5))
													        switch (gimple_assign_rhs_code (_a5))
														  {
														  case BIT_XOR_EXPR:
														    {
														      tree _q120 = gimple_assign_rhs1 (_a5);
														      _q120 = do_valueize (valueize, _q120);
														      tree _q121 = gimple_assign_rhs2 (_a5);
														      _q121 = do_valueize (valueize, _q121);
														      if (tree_swap_operands_p (_q120, _q121))
														        std::swap (_q120, _q121);
														      if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
															{
															  if ((_q121 == _q100 && ! TREE_SIDE_EFFECTS (_q121)) || (operand_equal_p (_q121, _q100, 0) && types_match (_q121, _q100)))
															    {
															      if (integer_zerop (_q21))
																{
																  switch (TREE_CODE (_p1))
																    {
																    case SSA_NAME:
																      if (gimple *_d6 = get_def (valueize, _p1))
																        {
																	  if (gassign *_a6 = dyn_cast <gassign *> (_d6))
																	    switch (gimple_assign_rhs_code (_a6))
																	      {
																	      case BIT_XOR_EXPR:
																	        {
																		  tree _q160 = gimple_assign_rhs1 (_a6);
																		  _q160 = do_valueize (valueize, _q160);
																		  tree _q161 = gimple_assign_rhs2 (_a6);
																		  _q161 = do_valueize (valueize, _q161);
																		  if (tree_swap_operands_p (_q160, _q161))
																		    std::swap (_q160, _q161);
																		  switch (TREE_CODE (_q160))
																		    {
																		    case SSA_NAME:
																		      if (gimple *_d7 = get_def (valueize, _q160))
																		        {
																			  if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			    switch (gimple_assign_rhs_code (_a7))
																			      {
																			      case NEGATE_EXPR:
																			        {
																				  tree _q170 = gimple_assign_rhs1 (_a7);
																				  _q170 = do_valueize (valueize, _q170);
																				  switch (TREE_CODE (_q170))
																				    {
																				    case SSA_NAME:
																				      if (gimple *_d8 = get_def (valueize, _q170))
																				        {
																					  if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					    switch (gimple_assign_rhs_code (_a8))
																					      {
																					      CASE_CONVERT:
																					        {
																						  tree _q180 = gimple_assign_rhs1 (_a8);
																						  _q180 = do_valueize (valueize, _q180);
																						  switch (TREE_CODE (_q180))
																						    {
																						    case SSA_NAME:
																						      if (gimple *_d9 = get_def (valueize, _q180))
																						        {
																							  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							    switch (gimple_assign_rhs_code (_a9))
																							      {
																							      case LT_EXPR:
																							        {
																								  tree _q190 = gimple_assign_rhs1 (_a9);
																								  _q190 = do_valueize (valueize, _q190);
																								  tree _q191 = gimple_assign_rhs2 (_a9);
																								  _q191 = do_valueize (valueize, _q191);
																								  if ((_q190 == _q40 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q40, 0) && types_match (_q190, _q40)))
																								    {
																								      if (integer_zerop (_q191))
																									{
																									  if (gimple_max_value (_q161, valueize))
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																		if (gimple_max_value (_q160, valueize))
																		  {
																		    switch (TREE_CODE (_q161))
																		      {
																		      case SSA_NAME:
																		        if (gimple *_d7 = get_def (valueize, _q161))
																		          {
																			    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			      switch (gimple_assign_rhs_code (_a7))
																				{
																				case NEGATE_EXPR:
																				  {
																				    tree _q180 = gimple_assign_rhs1 (_a7);
																				    _q180 = do_valueize (valueize, _q180);
																				    switch (TREE_CODE (_q180))
																				      {
																				      case SSA_NAME:
																				        if (gimple *_d8 = get_def (valueize, _q180))
																				          {
																					    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					      switch (gimple_assign_rhs_code (_a8))
																						{
																						CASE_CONVERT:
																						  {
																						    tree _q190 = gimple_assign_rhs1 (_a8);
																						    _q190 = do_valueize (valueize, _q190);
																						    switch (TREE_CODE (_q190))
																						      {
																						      case SSA_NAME:
																						        if (gimple *_d9 = get_def (valueize, _q190))
																						          {
																							    if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							      switch (gimple_assign_rhs_code (_a9))
																								{
																								case LT_EXPR:
																								  {
																								    tree _q200 = gimple_assign_rhs1 (_a9);
																								    _q200 = do_valueize (valueize, _q200);
																								    tree _q201 = gimple_assign_rhs2 (_a9);
																								    _q201 = do_valueize (valueize, _q201);
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
																											if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																						        break;
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
																				  }
																				default:;
																				}
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
																    case SSA_NAME:
																      if (gimple *_d6 = get_def (valueize, _p1))
																        {
																	  if (gassign *_a6 = dyn_cast <gassign *> (_d6))
																	    switch (gimple_assign_rhs_code (_a6))
																	      {
																	      case BIT_XOR_EXPR:
																	        {
																		  tree _q160 = gimple_assign_rhs1 (_a6);
																		  _q160 = do_valueize (valueize, _q160);
																		  tree _q161 = gimple_assign_rhs2 (_a6);
																		  _q161 = do_valueize (valueize, _q161);
																		  if (tree_swap_operands_p (_q160, _q161))
																		    std::swap (_q160, _q161);
																		  switch (TREE_CODE (_q160))
																		    {
																		    case SSA_NAME:
																		      if (gimple *_d7 = get_def (valueize, _q160))
																		        {
																			  if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			    switch (gimple_assign_rhs_code (_a7))
																			      {
																			      case NEGATE_EXPR:
																			        {
																				  tree _q170 = gimple_assign_rhs1 (_a7);
																				  _q170 = do_valueize (valueize, _q170);
																				  switch (TREE_CODE (_q170))
																				    {
																				    case SSA_NAME:
																				      if (gimple *_d8 = get_def (valueize, _q170))
																				        {
																					  if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					    switch (gimple_assign_rhs_code (_a8))
																					      {
																					      CASE_CONVERT:
																					        {
																						  tree _q180 = gimple_assign_rhs1 (_a8);
																						  _q180 = do_valueize (valueize, _q180);
																						  switch (TREE_CODE (_q180))
																						    {
																						    case SSA_NAME:
																						      if (gimple *_d9 = get_def (valueize, _q180))
																						        {
																							  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							    switch (gimple_assign_rhs_code (_a9))
																							      {
																							      case LT_EXPR:
																							        {
																								  tree _q190 = gimple_assign_rhs1 (_a9);
																								  _q190 = do_valueize (valueize, _q190);
																								  tree _q191 = gimple_assign_rhs2 (_a9);
																								  _q191 = do_valueize (valueize, _q191);
																								  if ((_q190 == _q40 && ! TREE_SIDE_EFFECTS (_q190)) || (operand_equal_p (_q190, _q40, 0) && types_match (_q190, _q40)))
																								    {
																								      if (integer_zerop (_q191))
																									{
																									  if (gimple_max_value (_q161, valueize))
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
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																		if (gimple_max_value (_q160, valueize))
																		  {
																		    switch (TREE_CODE (_q161))
																		      {
																		      case SSA_NAME:
																		        if (gimple *_d7 = get_def (valueize, _q161))
																		          {
																			    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			      switch (gimple_assign_rhs_code (_a7))
																				{
																				case NEGATE_EXPR:
																				  {
																				    tree _q180 = gimple_assign_rhs1 (_a7);
																				    _q180 = do_valueize (valueize, _q180);
																				    switch (TREE_CODE (_q180))
																				      {
																				      case SSA_NAME:
																				        if (gimple *_d8 = get_def (valueize, _q180))
																				          {
																					    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					      switch (gimple_assign_rhs_code (_a8))
																						{
																						CASE_CONVERT:
																						  {
																						    tree _q190 = gimple_assign_rhs1 (_a8);
																						    _q190 = do_valueize (valueize, _q190);
																						    switch (TREE_CODE (_q190))
																						      {
																						      case SSA_NAME:
																						        if (gimple *_d9 = get_def (valueize, _q190))
																						          {
																							    if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							      switch (gimple_assign_rhs_code (_a9))
																								{
																								case LT_EXPR:
																								  {
																								    tree _q200 = gimple_assign_rhs1 (_a9);
																								    _q200 = do_valueize (valueize, _q200);
																								    tree _q201 = gimple_assign_rhs2 (_a9);
																								    _q201 = do_valueize (valueize, _q201);
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
																											if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 72, __FILE__, __LINE__, false);
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
																						        break;
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
																				  }
																				default:;
																				}
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
					      }
					    default:;
					    }
				      }
				    break;
			          default:;
			          }
			      }
			  }
		        break;
		      }
		    case GE_EXPR:
		      {
			tree _q20 = TREE_OPERAND (_p0, 0);
			if ((TREE_CODE (_q20) == SSA_NAME
			     || is_gimple_min_invariant (_q20)))
			  {
			    _q20 = do_valueize (valueize, _q20);
			    tree _q21 = TREE_OPERAND (_p0, 1);
			    if ((TREE_CODE (_q21) == SSA_NAME
			         || is_gimple_min_invariant (_q21)))
			      {
				_q21 = do_valueize (valueize, _q21);
				switch (TREE_CODE (_q20))
				  {
				  case SSA_NAME:
				    if (gimple *_d2 = get_def (valueize, _q20))
				      {
					if (gassign *_a2 = dyn_cast <gassign *> (_d2))
					  switch (gimple_assign_rhs_code (_a2))
					    {
					    case BIT_AND_EXPR:
					      {
						tree _q30 = gimple_assign_rhs1 (_a2);
						_q30 = do_valueize (valueize, _q30);
						tree _q31 = gimple_assign_rhs2 (_a2);
						_q31 = do_valueize (valueize, _q31);
						if (tree_swap_operands_p (_q30, _q31))
						  std::swap (_q30, _q31);
						switch (TREE_CODE (_q30))
						  {
						  case SSA_NAME:
						    if (gimple *_d3 = get_def (valueize, _q30))
						      {
							if (gassign *_a3 = dyn_cast <gassign *> (_d3))
							  switch (gimple_assign_rhs_code (_a3))
							    {
							    case BIT_XOR_EXPR:
							      {
								tree _q40 = gimple_assign_rhs1 (_a3);
								_q40 = do_valueize (valueize, _q40);
								tree _q41 = gimple_assign_rhs2 (_a3);
								_q41 = do_valueize (valueize, _q41);
								if (tree_swap_operands_p (_q40, _q41))
								  std::swap (_q40, _q41);
								switch (TREE_CODE (_q31))
								  {
								  case SSA_NAME:
								    if (gimple *_d4 = get_def (valueize, _q31))
								      {
									if (gassign *_a4 = dyn_cast <gassign *> (_d4))
									  switch (gimple_assign_rhs_code (_a4))
									    {
									    case BIT_XOR_EXPR:
									      {
										tree _q70 = gimple_assign_rhs1 (_a4);
										_q70 = do_valueize (valueize, _q70);
										tree _q71 = gimple_assign_rhs2 (_a4);
										_q71 = do_valueize (valueize, _q71);
										if (tree_swap_operands_p (_q70, _q71))
										  std::swap (_q70, _q71);
										if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
										  {
										    {
										      tree _q71_pops[1];
										      if (gimple_nop_convert (_q71, _q71_pops, valueize))
										        {
											  tree _q90 = _q71_pops[0];
											  switch (TREE_CODE (_q90))
											    {
											    case SSA_NAME:
											      if (gimple *_d5 = get_def (valueize, _q90))
											        {
												  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
												    switch (gimple_assign_rhs_code (_a5))
												      {
												      case MINUS_EXPR:
												        {
													  tree _q100 = gimple_assign_rhs1 (_a5);
													  _q100 = do_valueize (valueize, _q100);
													  tree _q101 = gimple_assign_rhs2 (_a5);
													  _q101 = do_valueize (valueize, _q101);
													  {
													    tree _q100_pops[1];
													    if (gimple_nop_convert (_q100, _q100_pops, valueize))
													      {
														tree _q110 = _q100_pops[0];
														if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
														  {
														    {
														      tree _q101_pops[1];
														      if (gimple_nop_convert (_q101, _q101_pops, valueize))
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
																	case SSA_NAME:
																	  if (gimple *_d6 = get_def (valueize, _p2))
																	    {
																	      if (gassign *_a6 = dyn_cast <gassign *> (_d6))
																	        switch (gimple_assign_rhs_code (_a6))
																		  {
																		  case BIT_XOR_EXPR:
																		    {
																		      tree _q170 = gimple_assign_rhs1 (_a6);
																		      _q170 = do_valueize (valueize, _q170);
																		      tree _q171 = gimple_assign_rhs2 (_a6);
																		      _q171 = do_valueize (valueize, _q171);
																		      if (tree_swap_operands_p (_q170, _q171))
																		        std::swap (_q170, _q171);
																		      switch (TREE_CODE (_q170))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d7 = get_def (valueize, _q170))
																			    {
																			      if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			        switch (gimple_assign_rhs_code (_a7))
																				  {
																				  case NEGATE_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a7);
																				      _q180 = do_valueize (valueize, _q180);
																				      switch (TREE_CODE (_q180))
																				        {
																					case SSA_NAME:
																					  if (gimple *_d8 = get_def (valueize, _q180))
																					    {
																					      if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					        switch (gimple_assign_rhs_code (_a8))
																						  {
																						  CASE_CONVERT:
																						    {
																						      tree _q190 = gimple_assign_rhs1 (_a8);
																						      _q190 = do_valueize (valueize, _q190);
																						      switch (TREE_CODE (_q190))
																						        {
																							case SSA_NAME:
																							  if (gimple *_d9 = get_def (valueize, _q190))
																							    {
																							      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							        switch (gimple_assign_rhs_code (_a9))
																								  {
																								  case LT_EXPR:
																								    {
																								      tree _q200 = gimple_assign_rhs1 (_a9);
																								      _q200 = do_valueize (valueize, _q200);
																								      tree _q201 = gimple_assign_rhs2 (_a9);
																								      _q201 = do_valueize (valueize, _q201);
																								      if ((_q200 == _q40 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q40, 0) && types_match (_q200, _q40)))
																									{
																									  if (integer_zerop (_q201))
																									    {
																									      if (gimple_max_value (_q171, valueize))
																										{
																										  {
																										    tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q41, _q71 };
																										    if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																										      {
																											{
																											  res_ops[0] = captures[0];
																											  res_ops[1] = captures[1];
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																							  break;
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
																				    }
																				  default:;
																				  }
																			    }
																			  break;
																		        default:;
																		        }
																		    if (gimple_max_value (_q170, valueize))
																		      {
																			switch (TREE_CODE (_q171))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d7 = get_def (valueize, _q171))
																			      {
																				if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																				  switch (gimple_assign_rhs_code (_a7))
																				    {
																				    case NEGATE_EXPR:
																				      {
																					tree _q190 = gimple_assign_rhs1 (_a7);
																					_q190 = do_valueize (valueize, _q190);
																					switch (TREE_CODE (_q190))
																					  {
																					  case SSA_NAME:
																					    if (gimple *_d8 = get_def (valueize, _q190))
																					      {
																						if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																						  switch (gimple_assign_rhs_code (_a8))
																						    {
																						    CASE_CONVERT:
																						      {
																							tree _q200 = gimple_assign_rhs1 (_a8);
																							_q200 = do_valueize (valueize, _q200);
																							switch (TREE_CODE (_q200))
																							  {
																							  case SSA_NAME:
																							    if (gimple *_d9 = get_def (valueize, _q200))
																							      {
																								if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																								  switch (gimple_assign_rhs_code (_a9))
																								    {
																								    case LT_EXPR:
																								      {
																									tree _q210 = gimple_assign_rhs1 (_a9);
																									_q210 = do_valueize (valueize, _q210);
																									tree _q211 = gimple_assign_rhs2 (_a9);
																									_q211 = do_valueize (valueize, _q211);
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
																											if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																				      }
																				    default:;
																				    }
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
											    default:;
											    }
										        }
										    }
										  }
										if ((_q70 == _q41 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q41, 0) && types_match (_q70, _q41)))
										  {
										    {
										      tree _q71_pops[1];
										      if (gimple_nop_convert (_q71, _q71_pops, valueize))
										        {
											  tree _q90 = _q71_pops[0];
											  switch (TREE_CODE (_q90))
											    {
											    case SSA_NAME:
											      if (gimple *_d5 = get_def (valueize, _q90))
											        {
												  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
												    switch (gimple_assign_rhs_code (_a5))
												      {
												      case MINUS_EXPR:
												        {
													  tree _q100 = gimple_assign_rhs1 (_a5);
													  _q100 = do_valueize (valueize, _q100);
													  tree _q101 = gimple_assign_rhs2 (_a5);
													  _q101 = do_valueize (valueize, _q101);
													  {
													    tree _q100_pops[1];
													    if (gimple_nop_convert (_q100, _q100_pops, valueize))
													      {
														tree _q110 = _q100_pops[0];
														if ((_q110 == _q41 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q41, 0) && types_match (_q110, _q41)))
														  {
														    {
														      tree _q101_pops[1];
														      if (gimple_nop_convert (_q101, _q101_pops, valueize))
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
																	case SSA_NAME:
																	  if (gimple *_d6 = get_def (valueize, _p2))
																	    {
																	      if (gassign *_a6 = dyn_cast <gassign *> (_d6))
																	        switch (gimple_assign_rhs_code (_a6))
																		  {
																		  case BIT_XOR_EXPR:
																		    {
																		      tree _q170 = gimple_assign_rhs1 (_a6);
																		      _q170 = do_valueize (valueize, _q170);
																		      tree _q171 = gimple_assign_rhs2 (_a6);
																		      _q171 = do_valueize (valueize, _q171);
																		      if (tree_swap_operands_p (_q170, _q171))
																		        std::swap (_q170, _q171);
																		      switch (TREE_CODE (_q170))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d7 = get_def (valueize, _q170))
																			    {
																			      if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			        switch (gimple_assign_rhs_code (_a7))
																				  {
																				  case NEGATE_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a7);
																				      _q180 = do_valueize (valueize, _q180);
																				      switch (TREE_CODE (_q180))
																				        {
																					case SSA_NAME:
																					  if (gimple *_d8 = get_def (valueize, _q180))
																					    {
																					      if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					        switch (gimple_assign_rhs_code (_a8))
																						  {
																						  CASE_CONVERT:
																						    {
																						      tree _q190 = gimple_assign_rhs1 (_a8);
																						      _q190 = do_valueize (valueize, _q190);
																						      switch (TREE_CODE (_q190))
																						        {
																							case SSA_NAME:
																							  if (gimple *_d9 = get_def (valueize, _q190))
																							    {
																							      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							        switch (gimple_assign_rhs_code (_a9))
																								  {
																								  case LT_EXPR:
																								    {
																								      tree _q200 = gimple_assign_rhs1 (_a9);
																								      _q200 = do_valueize (valueize, _q200);
																								      tree _q201 = gimple_assign_rhs2 (_a9);
																								      _q201 = do_valueize (valueize, _q201);
																								      if ((_q200 == _q41 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q41, 0) && types_match (_q200, _q41)))
																									{
																									  if (integer_zerop (_q201))
																									    {
																									      if (gimple_max_value (_q171, valueize))
																										{
																										  {
																										    tree captures[3] ATTRIBUTE_UNUSED = { _q41, _q40, _q71 };
																										    if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																										      {
																											{
																											  res_ops[0] = captures[0];
																											  res_ops[1] = captures[1];
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																							  break;
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
																				    }
																				  default:;
																				  }
																			    }
																			  break;
																		        default:;
																		        }
																		    if (gimple_max_value (_q170, valueize))
																		      {
																			switch (TREE_CODE (_q171))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d7 = get_def (valueize, _q171))
																			      {
																				if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																				  switch (gimple_assign_rhs_code (_a7))
																				    {
																				    case NEGATE_EXPR:
																				      {
																					tree _q190 = gimple_assign_rhs1 (_a7);
																					_q190 = do_valueize (valueize, _q190);
																					switch (TREE_CODE (_q190))
																					  {
																					  case SSA_NAME:
																					    if (gimple *_d8 = get_def (valueize, _q190))
																					      {
																						if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																						  switch (gimple_assign_rhs_code (_a8))
																						    {
																						    CASE_CONVERT:
																						      {
																							tree _q200 = gimple_assign_rhs1 (_a8);
																							_q200 = do_valueize (valueize, _q200);
																							switch (TREE_CODE (_q200))
																							  {
																							  case SSA_NAME:
																							    if (gimple *_d9 = get_def (valueize, _q200))
																							      {
																								if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																								  switch (gimple_assign_rhs_code (_a9))
																								    {
																								    case LT_EXPR:
																								      {
																									tree _q210 = gimple_assign_rhs1 (_a9);
																									_q210 = do_valueize (valueize, _q210);
																									tree _q211 = gimple_assign_rhs2 (_a9);
																									_q211 = do_valueize (valueize, _q211);
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
																											if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																				      }
																				    default:;
																				    }
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
								    break;
							          default:;
							          }
								{
								  tree _q41_pops[1];
								  if (gimple_nop_convert (_q41, _q41_pops, valueize))
								    {
								      tree _q60 = _q41_pops[0];
								      switch (TREE_CODE (_q60))
								        {
									case SSA_NAME:
									  if (gimple *_d4 = get_def (valueize, _q60))
									    {
									      if (gassign *_a4 = dyn_cast <gassign *> (_d4))
									        switch (gimple_assign_rhs_code (_a4))
										  {
										  case MINUS_EXPR:
										    {
										      tree _q70 = gimple_assign_rhs1 (_a4);
										      _q70 = do_valueize (valueize, _q70);
										      tree _q71 = gimple_assign_rhs2 (_a4);
										      _q71 = do_valueize (valueize, _q71);
										      {
											tree _q70_pops[1];
											if (gimple_nop_convert (_q70, _q70_pops, valueize))
											  {
											    tree _q80 = _q70_pops[0];
											    if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
											      {
												{
												  tree _q71_pops[1];
												  if (gimple_nop_convert (_q71, _q71_pops, valueize))
												    {
												      tree _q100 = _q71_pops[0];
												      switch (TREE_CODE (_q31))
												        {
													case SSA_NAME:
													  if (gimple *_d5 = get_def (valueize, _q31))
													    {
													      if (gassign *_a5 = dyn_cast <gassign *> (_d5))
													        switch (gimple_assign_rhs_code (_a5))
														  {
														  case BIT_XOR_EXPR:
														    {
														      tree _q120 = gimple_assign_rhs1 (_a5);
														      _q120 = do_valueize (valueize, _q120);
														      tree _q121 = gimple_assign_rhs2 (_a5);
														      _q121 = do_valueize (valueize, _q121);
														      if (tree_swap_operands_p (_q120, _q121))
														        std::swap (_q120, _q121);
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
																	case SSA_NAME:
																	  if (gimple *_d6 = get_def (valueize, _p2))
																	    {
																	      if (gassign *_a6 = dyn_cast <gassign *> (_d6))
																	        switch (gimple_assign_rhs_code (_a6))
																		  {
																		  case BIT_XOR_EXPR:
																		    {
																		      tree _q170 = gimple_assign_rhs1 (_a6);
																		      _q170 = do_valueize (valueize, _q170);
																		      tree _q171 = gimple_assign_rhs2 (_a6);
																		      _q171 = do_valueize (valueize, _q171);
																		      if (tree_swap_operands_p (_q170, _q171))
																		        std::swap (_q170, _q171);
																		      switch (TREE_CODE (_q170))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d7 = get_def (valueize, _q170))
																			    {
																			      if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			        switch (gimple_assign_rhs_code (_a7))
																				  {
																				  case NEGATE_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a7);
																				      _q180 = do_valueize (valueize, _q180);
																				      switch (TREE_CODE (_q180))
																				        {
																					case SSA_NAME:
																					  if (gimple *_d8 = get_def (valueize, _q180))
																					    {
																					      if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					        switch (gimple_assign_rhs_code (_a8))
																						  {
																						  CASE_CONVERT:
																						    {
																						      tree _q190 = gimple_assign_rhs1 (_a8);
																						      _q190 = do_valueize (valueize, _q190);
																						      switch (TREE_CODE (_q190))
																						        {
																							case SSA_NAME:
																							  if (gimple *_d9 = get_def (valueize, _q190))
																							    {
																							      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							        switch (gimple_assign_rhs_code (_a9))
																								  {
																								  case LT_EXPR:
																								    {
																								      tree _q200 = gimple_assign_rhs1 (_a9);
																								      _q200 = do_valueize (valueize, _q200);
																								      tree _q201 = gimple_assign_rhs2 (_a9);
																								      _q201 = do_valueize (valueize, _q201);
																								      if ((_q200 == _q40 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q40, 0) && types_match (_q200, _q40)))
																									{
																									  if (integer_zerop (_q201))
																									    {
																									      if (gimple_max_value (_q171, valueize))
																										{
																										  {
																										    tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
																										    if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																										      {
																											{
																											  res_ops[0] = captures[0];
																											  res_ops[1] = captures[1];
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																							  break;
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
																				    }
																				  default:;
																				  }
																			    }
																			  break;
																		        default:;
																		        }
																		    if (gimple_max_value (_q170, valueize))
																		      {
																			switch (TREE_CODE (_q171))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d7 = get_def (valueize, _q171))
																			      {
																				if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																				  switch (gimple_assign_rhs_code (_a7))
																				    {
																				    case NEGATE_EXPR:
																				      {
																					tree _q190 = gimple_assign_rhs1 (_a7);
																					_q190 = do_valueize (valueize, _q190);
																					switch (TREE_CODE (_q190))
																					  {
																					  case SSA_NAME:
																					    if (gimple *_d8 = get_def (valueize, _q190))
																					      {
																						if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																						  switch (gimple_assign_rhs_code (_a8))
																						    {
																						    CASE_CONVERT:
																						      {
																							tree _q200 = gimple_assign_rhs1 (_a8);
																							_q200 = do_valueize (valueize, _q200);
																							switch (TREE_CODE (_q200))
																							  {
																							  case SSA_NAME:
																							    if (gimple *_d9 = get_def (valueize, _q200))
																							      {
																								if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																								  switch (gimple_assign_rhs_code (_a9))
																								    {
																								    case LT_EXPR:
																								      {
																									tree _q210 = gimple_assign_rhs1 (_a9);
																									_q210 = do_valueize (valueize, _q210);
																									tree _q211 = gimple_assign_rhs2 (_a9);
																									_q211 = do_valueize (valueize, _q211);
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
																											if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																				      }
																				    default:;
																				    }
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
																	case SSA_NAME:
																	  if (gimple *_d6 = get_def (valueize, _p2))
																	    {
																	      if (gassign *_a6 = dyn_cast <gassign *> (_d6))
																	        switch (gimple_assign_rhs_code (_a6))
																		  {
																		  case BIT_XOR_EXPR:
																		    {
																		      tree _q170 = gimple_assign_rhs1 (_a6);
																		      _q170 = do_valueize (valueize, _q170);
																		      tree _q171 = gimple_assign_rhs2 (_a6);
																		      _q171 = do_valueize (valueize, _q171);
																		      if (tree_swap_operands_p (_q170, _q171))
																		        std::swap (_q170, _q171);
																		      switch (TREE_CODE (_q170))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d7 = get_def (valueize, _q170))
																			    {
																			      if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			        switch (gimple_assign_rhs_code (_a7))
																				  {
																				  case NEGATE_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a7);
																				      _q180 = do_valueize (valueize, _q180);
																				      switch (TREE_CODE (_q180))
																				        {
																					case SSA_NAME:
																					  if (gimple *_d8 = get_def (valueize, _q180))
																					    {
																					      if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																					        switch (gimple_assign_rhs_code (_a8))
																						  {
																						  CASE_CONVERT:
																						    {
																						      tree _q190 = gimple_assign_rhs1 (_a8);
																						      _q190 = do_valueize (valueize, _q190);
																						      switch (TREE_CODE (_q190))
																						        {
																							case SSA_NAME:
																							  if (gimple *_d9 = get_def (valueize, _q190))
																							    {
																							      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																							        switch (gimple_assign_rhs_code (_a9))
																								  {
																								  case LT_EXPR:
																								    {
																								      tree _q200 = gimple_assign_rhs1 (_a9);
																								      _q200 = do_valueize (valueize, _q200);
																								      tree _q201 = gimple_assign_rhs2 (_a9);
																								      _q201 = do_valueize (valueize, _q201);
																								      if ((_q200 == _q40 && ! TREE_SIDE_EFFECTS (_q200)) || (operand_equal_p (_q200, _q40, 0) && types_match (_q200, _q40)))
																									{
																									  if (integer_zerop (_q201))
																									    {
																									      if (gimple_max_value (_q171, valueize))
																										{
																										  {
																										    tree captures[3] ATTRIBUTE_UNUSED = { _q40, _q100, _q41 };
																										    if (INTEGRAL_TYPE_P (type) && !TYPE_UNSIGNED (type)
)
																										      {
																											{
																											  res_ops[0] = captures[0];
																											  res_ops[1] = captures[1];
																											  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																							  break;
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
																				    }
																				  default:;
																				  }
																			    }
																			  break;
																		        default:;
																		        }
																		    if (gimple_max_value (_q170, valueize))
																		      {
																			switch (TREE_CODE (_q171))
																			  {
																			  case SSA_NAME:
																			    if (gimple *_d7 = get_def (valueize, _q171))
																			      {
																				if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																				  switch (gimple_assign_rhs_code (_a7))
																				    {
																				    case NEGATE_EXPR:
																				      {
																					tree _q190 = gimple_assign_rhs1 (_a7);
																					_q190 = do_valueize (valueize, _q190);
																					switch (TREE_CODE (_q190))
																					  {
																					  case SSA_NAME:
																					    if (gimple *_d8 = get_def (valueize, _q190))
																					      {
																						if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																						  switch (gimple_assign_rhs_code (_a8))
																						    {
																						    CASE_CONVERT:
																						      {
																							tree _q200 = gimple_assign_rhs1 (_a8);
																							_q200 = do_valueize (valueize, _q200);
																							switch (TREE_CODE (_q200))
																							  {
																							  case SSA_NAME:
																							    if (gimple *_d9 = get_def (valueize, _q200))
																							      {
																								if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																								  switch (gimple_assign_rhs_code (_a9))
																								    {
																								    case LT_EXPR:
																								      {
																									tree _q210 = gimple_assign_rhs1 (_a9);
																									_q210 = do_valueize (valueize, _q210);
																									tree _q211 = gimple_assign_rhs2 (_a9);
																									_q211 = do_valueize (valueize, _q211);
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
																											if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 73, __FILE__, __LINE__, false);
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
																				      }
																				    default:;
																				    }
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
									  break;
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
					      }
					    default:;
					    }
				      }
				    break;
			          default:;
			          }
			      }
			  }
		        break;
		      }
		    case NE_EXPR:
		      {
			tree _q20 = TREE_OPERAND (_p0, 0);
			if ((TREE_CODE (_q20) == SSA_NAME
			     || is_gimple_min_invariant (_q20)))
			  {
			    _q20 = do_valueize (valueize, _q20);
			    tree _q21 = TREE_OPERAND (_p0, 1);
			    if ((TREE_CODE (_q21) == SSA_NAME
			         || is_gimple_min_invariant (_q21)))
			      {
				_q21 = do_valueize (valueize, _q21);
				if (tree_swap_operands_p (_q20, _q21))
				  std::swap (_q20, _q21);
				switch (TREE_CODE (_q20))
				  {
				  case SSA_NAME:
				    if (gimple *_d2 = get_def (valueize, _q20))
				      {
					if (gassign *_a2 = dyn_cast <gassign *> (_d2))
					  switch (gimple_assign_rhs_code (_a2))
					    {
					    case IMAGPART_EXPR:
					      {
						tree _q30 = TREE_OPERAND (gimple_assign_rhs1 (_a2), 0);
						if ((TREE_CODE (_q30) == SSA_NAME
						     || is_gimple_min_invariant (_q30)))
						  {
						    _q30 = do_valueize (valueize, _q30);
						    switch (TREE_CODE (_q30))
						      {
						      case SSA_NAME:
						        if (gimple *_d3 = get_def (valueize, _q30))
						          {
							    if (gcall *_c3 = dyn_cast <gcall *> (_d3))
							      switch (gimple_call_combined_fn (_c3))
								{
								case CFN_SUB_OVERFLOW:
								  if (gimple_call_num_args (_c3) == 2)
								    {
								      tree _q40 = gimple_call_arg (_c3, 0);
								      _q40 = do_valueize (valueize, _q40);
								      tree _q41 = gimple_call_arg (_c3, 1);
								      _q41 = do_valueize (valueize, _q41);
								      if (integer_zerop (_q21))
									{
									  switch (TREE_CODE (_p1))
									    {
									    case SSA_NAME:
									      if (gimple *_d4 = get_def (valueize, _p1))
									        {
										  if (gassign *_a4 = dyn_cast <gassign *> (_d4))
										    switch (gimple_assign_rhs_code (_a4))
										      {
										      case BIT_XOR_EXPR:
										        {
											  tree _q80 = gimple_assign_rhs1 (_a4);
											  _q80 = do_valueize (valueize, _q80);
											  tree _q81 = gimple_assign_rhs2 (_a4);
											  _q81 = do_valueize (valueize, _q81);
											  if (tree_swap_operands_p (_q80, _q81))
											    std::swap (_q80, _q81);
											  switch (TREE_CODE (_q80))
											    {
											    case SSA_NAME:
											      if (gimple *_d5 = get_def (valueize, _q80))
											        {
												  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
												    switch (gimple_assign_rhs_code (_a5))
												      {
												      case NEGATE_EXPR:
												        {
													  tree _q90 = gimple_assign_rhs1 (_a5);
													  _q90 = do_valueize (valueize, _q90);
													  switch (TREE_CODE (_q90))
													    {
													    case SSA_NAME:
													      if (gimple *_d6 = get_def (valueize, _q90))
													        {
														  if (gassign *_a6 = dyn_cast <gassign *> (_d6))
														    switch (gimple_assign_rhs_code (_a6))
														      {
														      CASE_CONVERT:
														        {
															  tree _q100 = gimple_assign_rhs1 (_a6);
															  _q100 = do_valueize (valueize, _q100);
															  switch (TREE_CODE (_q100))
															    {
															    case SSA_NAME:
															      if (gimple *_d7 = get_def (valueize, _q100))
															        {
																  if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																    switch (gimple_assign_rhs_code (_a7))
																      {
																      case LT_EXPR:
																        {
																	  tree _q110 = gimple_assign_rhs1 (_a7);
																	  _q110 = do_valueize (valueize, _q110);
																	  tree _q111 = gimple_assign_rhs2 (_a7);
																	  _q111 = do_valueize (valueize, _q111);
																	  if ((_q110 == _q40 && ! TREE_SIDE_EFFECTS (_q110)) || (operand_equal_p (_q110, _q40, 0) && types_match (_q110, _q40)))
																	    {
																	      if (integer_zerop (_q111))
																		{
																		  if (gimple_max_value (_q81, valueize))
																		    {
																		      switch (TREE_CODE (_p2))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d8 = get_def (valueize, _p2))
																			    {
																			      if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																			        switch (gimple_assign_rhs_code (_a8))
																				  {
																				  case REALPART_EXPR:
																				    {
																				      tree _q150 = TREE_OPERAND (gimple_assign_rhs1 (_a8), 0);
																				      if ((TREE_CODE (_q150) == SSA_NAME
																				           || is_gimple_min_invariant (_q150)))
																				        {
																					  _q150 = do_valueize (valueize, _q150);
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
																						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 74, __FILE__, __LINE__, false);
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
															      break;
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
												        }
												      default:;
												      }
											        }
											      break;
											    default:;
											    }
											if (gimple_max_value (_q80, valueize))
											  {
											    switch (TREE_CODE (_q81))
											      {
											      case SSA_NAME:
											        if (gimple *_d5 = get_def (valueize, _q81))
											          {
												    if (gassign *_a5 = dyn_cast <gassign *> (_d5))
												      switch (gimple_assign_rhs_code (_a5))
													{
													case NEGATE_EXPR:
													  {
													    tree _q100 = gimple_assign_rhs1 (_a5);
													    _q100 = do_valueize (valueize, _q100);
													    switch (TREE_CODE (_q100))
													      {
													      case SSA_NAME:
													        if (gimple *_d6 = get_def (valueize, _q100))
													          {
														    if (gassign *_a6 = dyn_cast <gassign *> (_d6))
														      switch (gimple_assign_rhs_code (_a6))
															{
															CASE_CONVERT:
															  {
															    tree _q110 = gimple_assign_rhs1 (_a6);
															    _q110 = do_valueize (valueize, _q110);
															    switch (TREE_CODE (_q110))
															      {
															      case SSA_NAME:
															        if (gimple *_d7 = get_def (valueize, _q110))
															          {
																    if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																      switch (gimple_assign_rhs_code (_a7))
																	{
																	case LT_EXPR:
																	  {
																	    tree _q120 = gimple_assign_rhs1 (_a7);
																	    _q120 = do_valueize (valueize, _q120);
																	    tree _q121 = gimple_assign_rhs2 (_a7);
																	    _q121 = do_valueize (valueize, _q121);
																	    if ((_q120 == _q40 && ! TREE_SIDE_EFFECTS (_q120)) || (operand_equal_p (_q120, _q40, 0) && types_match (_q120, _q40)))
																	      {
																		if (integer_zerop (_q121))
																		  {
																		    switch (TREE_CODE (_p2))
																		      {
																		      case SSA_NAME:
																		        if (gimple *_d8 = get_def (valueize, _p2))
																		          {
																			    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																			      switch (gimple_assign_rhs_code (_a8))
																				{
																				case REALPART_EXPR:
																				  {
																				    tree _q150 = TREE_OPERAND (gimple_assign_rhs1 (_a8), 0);
																				    if ((TREE_CODE (_q150) == SSA_NAME
																				         || is_gimple_min_invariant (_q150)))
																				      {
																					_q150 = do_valueize (valueize, _q150);
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
																						    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 74, __FILE__, __LINE__, false);
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
															  }
															default:;
															}
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
								        }
								    }
								  break;
								default:;
								}
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
			      }
			  }
		        break;
		      }
		    case BIT_AND_EXPR:
		      {
			tree _q20 = TREE_OPERAND (_p0, 0);
			if ((TREE_CODE (_q20) == SSA_NAME
			     || is_gimple_min_invariant (_q20)))
			  {
			    _q20 = do_valueize (valueize, _q20);
			    tree _q21 = TREE_OPERAND (_p0, 1);
			    if ((TREE_CODE (_q21) == SSA_NAME
			         || is_gimple_min_invariant (_q21)))
			      {
				_q21 = do_valueize (valueize, _q21);
				if (tree_swap_operands_p (_q20, _q21))
				  std::swap (_q20, _q21);
				switch (TREE_CODE (_q20))
				  {
				  case SSA_NAME:
				    if (gimple *_d2 = get_def (valueize, _q20))
				      {
					if (gassign *_a2 = dyn_cast <gassign *> (_d2))
					  switch (gimple_assign_rhs_code (_a2))
					    {
					    case LT_EXPR:
					      {
						tree _q30 = gimple_assign_rhs1 (_a2);
						_q30 = do_valueize (valueize, _q30);
						tree _q31 = gimple_assign_rhs2 (_a2);
						_q31 = do_valueize (valueize, _q31);
						switch (TREE_CODE (_q30))
						  {
						  case SSA_NAME:
						    if (gimple *_d3 = get_def (valueize, _q30))
						      {
							if (gassign *_a3 = dyn_cast <gassign *> (_d3))
							  switch (gimple_assign_rhs_code (_a3))
							    {
							    case BIT_XOR_EXPR:
							      {
								tree _q40 = gimple_assign_rhs1 (_a3);
								_q40 = do_valueize (valueize, _q40);
								tree _q41 = gimple_assign_rhs2 (_a3);
								_q41 = do_valueize (valueize, _q41);
								if (tree_swap_operands_p (_q40, _q41))
								  std::swap (_q40, _q41);
								{
								  tree _q41_pops[1];
								  if (gimple_nop_convert (_q41, _q41_pops, valueize))
								    {
								      tree _q60 = _q41_pops[0];
								      switch (TREE_CODE (_q60))
								        {
									case SSA_NAME:
									  if (gimple *_d4 = get_def (valueize, _q60))
									    {
									      if (gassign *_a4 = dyn_cast <gassign *> (_d4))
									        switch (gimple_assign_rhs_code (_a4))
										  {
										  case MINUS_EXPR:
										    {
										      tree _q70 = gimple_assign_rhs1 (_a4);
										      _q70 = do_valueize (valueize, _q70);
										      tree _q71 = gimple_assign_rhs2 (_a4);
										      _q71 = do_valueize (valueize, _q71);
										      {
											tree _q70_pops[1];
											if (gimple_nop_convert (_q70, _q70_pops, valueize))
											  {
											    tree _q80 = _q70_pops[0];
											    if ((_q80 == _q40 && ! TREE_SIDE_EFFECTS (_q80)) || (operand_equal_p (_q80, _q40, 0) && types_match (_q80, _q40)))
											      {
												{
												  tree _q71_pops[1];
												  if (gimple_nop_convert (_q71, _q71_pops, valueize))
												    {
												      tree _q100 = _q71_pops[0];
												      if (integer_zerop (_q31))
													{
													  switch (TREE_CODE (_q21))
													    {
													    case SSA_NAME:
													      if (gimple *_d5 = get_def (valueize, _q21))
													        {
														  if (gassign *_a5 = dyn_cast <gassign *> (_d5))
														    switch (gimple_assign_rhs_code (_a5))
														      {
														      case LT_EXPR:
														        {
															  tree _q130 = gimple_assign_rhs1 (_a5);
															  _q130 = do_valueize (valueize, _q130);
															  tree _q131 = gimple_assign_rhs2 (_a5);
															  _q131 = do_valueize (valueize, _q131);
															  switch (TREE_CODE (_q130))
															    {
															    case SSA_NAME:
															      if (gimple *_d6 = get_def (valueize, _q130))
															        {
																  if (gassign *_a6 = dyn_cast <gassign *> (_d6))
																    switch (gimple_assign_rhs_code (_a6))
																      {
																      case BIT_XOR_EXPR:
																        {
																	  tree _q140 = gimple_assign_rhs1 (_a6);
																	  _q140 = do_valueize (valueize, _q140);
																	  tree _q141 = gimple_assign_rhs2 (_a6);
																	  _q141 = do_valueize (valueize, _q141);
																	  if (tree_swap_operands_p (_q140, _q141))
																	    std::swap (_q140, _q141);
																	  if ((_q140 == _q40 && ! TREE_SIDE_EFFECTS (_q140)) || (operand_equal_p (_q140, _q40, 0) && types_match (_q140, _q40)))
																	    {
																	      if ((_q141 == _q100 && ! TREE_SIDE_EFFECTS (_q141)) || (operand_equal_p (_q141, _q100, 0) && types_match (_q141, _q100)))
																		{
																		  if (integer_zerop (_q131))
																		    {
																		      switch (TREE_CODE (_p1))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d7 = get_def (valueize, _p1))
																			    {
																			      if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			        switch (gimple_assign_rhs_code (_a7))
																				  {
																				  case BIT_XOR_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a7);
																				      _q180 = do_valueize (valueize, _q180);
																				      tree _q181 = gimple_assign_rhs2 (_a7);
																				      _q181 = do_valueize (valueize, _q181);
																				      if (tree_swap_operands_p (_q180, _q181))
																				        std::swap (_q180, _q181);
																				      {
																					tree _q180_pops[1];
																					if (gimple_nop_convert (_q180, _q180_pops, valueize))
																					  {
																					    tree _q190 = _q180_pops[0];
																					    switch (TREE_CODE (_q190))
																					      {
																					      case SSA_NAME:
																					        if (gimple *_d8 = get_def (valueize, _q190))
																					          {
																						    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																						      switch (gimple_assign_rhs_code (_a8))
																							{
																							case NEGATE_EXPR:
																							  {
																							    tree _q200 = gimple_assign_rhs1 (_a8);
																							    _q200 = do_valueize (valueize, _q200);
																							    {
																							      tree _q200_pops[1];
																							      if (gimple_nop_convert (_q200, _q200_pops, valueize))
																							        {
																								  tree _q210 = _q200_pops[0];
																								  switch (TREE_CODE (_q210))
																								    {
																								    case SSA_NAME:
																								      if (gimple *_d9 = get_def (valueize, _q210))
																								        {
																									  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																									    switch (gimple_assign_rhs_code (_a9))
																									      {
																									      CASE_CONVERT:
																									        {
																										  tree _q220 = gimple_assign_rhs1 (_a9);
																										  _q220 = do_valueize (valueize, _q220);
																										  switch (TREE_CODE (_q220))
																										    {
																										    case SSA_NAME:
																										      if (gimple *_d10 = get_def (valueize, _q220))
																										        {
																											  if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																											    switch (gimple_assign_rhs_code (_a10))
																											      {
																											      case LT_EXPR:
																											        {
																												  tree _q230 = gimple_assign_rhs1 (_a10);
																												  _q230 = do_valueize (valueize, _q230);
																												  tree _q231 = gimple_assign_rhs2 (_a10);
																												  _q231 = do_valueize (valueize, _q231);
																												  if ((_q230 == _q40 && ! TREE_SIDE_EFFECTS (_q230)) || (operand_equal_p (_q230, _q40, 0) && types_match (_q230, _q40)))
																												    {
																												      if (integer_zerop (_q231))
																													{
																													  if (gimple_max_value (_q181, valueize))
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
																															  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																					  }
																				      }
																				      if (gimple_max_value (_q180, valueize))
																					{
																					  {
																					    tree _q181_pops[1];
																					    if (gimple_nop_convert (_q181, _q181_pops, valueize))
																					      {
																						tree _q200 = _q181_pops[0];
																						switch (TREE_CODE (_q200))
																						  {
																						  case SSA_NAME:
																						    if (gimple *_d8 = get_def (valueize, _q200))
																						      {
																							if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																							  switch (gimple_assign_rhs_code (_a8))
																							    {
																							    case NEGATE_EXPR:
																							      {
																								tree _q210 = gimple_assign_rhs1 (_a8);
																								_q210 = do_valueize (valueize, _q210);
																								{
																								  tree _q210_pops[1];
																								  if (gimple_nop_convert (_q210, _q210_pops, valueize))
																								    {
																								      tree _q220 = _q210_pops[0];
																								      switch (TREE_CODE (_q220))
																								        {
																									case SSA_NAME:
																									  if (gimple *_d9 = get_def (valueize, _q220))
																									    {
																									      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																									        switch (gimple_assign_rhs_code (_a9))
																										  {
																										  CASE_CONVERT:
																										    {
																										      tree _q230 = gimple_assign_rhs1 (_a9);
																										      _q230 = do_valueize (valueize, _q230);
																										      switch (TREE_CODE (_q230))
																										        {
																											case SSA_NAME:
																											  if (gimple *_d10 = get_def (valueize, _q230))
																											    {
																											      if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																											        switch (gimple_assign_rhs_code (_a10))
																												  {
																												  case LT_EXPR:
																												    {
																												      tree _q240 = gimple_assign_rhs1 (_a10);
																												      _q240 = do_valueize (valueize, _q240);
																												      tree _q241 = gimple_assign_rhs2 (_a10);
																												      _q241 = do_valueize (valueize, _q241);
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
																															  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																											  break;
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
																			case SSA_NAME:
																			  if (gimple *_d7 = get_def (valueize, _p1))
																			    {
																			      if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			        switch (gimple_assign_rhs_code (_a7))
																				  {
																				  case BIT_XOR_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a7);
																				      _q180 = do_valueize (valueize, _q180);
																				      tree _q181 = gimple_assign_rhs2 (_a7);
																				      _q181 = do_valueize (valueize, _q181);
																				      if (tree_swap_operands_p (_q180, _q181))
																				        std::swap (_q180, _q181);
																				      {
																					tree _q180_pops[1];
																					if (gimple_nop_convert (_q180, _q180_pops, valueize))
																					  {
																					    tree _q190 = _q180_pops[0];
																					    switch (TREE_CODE (_q190))
																					      {
																					      case SSA_NAME:
																					        if (gimple *_d8 = get_def (valueize, _q190))
																					          {
																						    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																						      switch (gimple_assign_rhs_code (_a8))
																							{
																							case NEGATE_EXPR:
																							  {
																							    tree _q200 = gimple_assign_rhs1 (_a8);
																							    _q200 = do_valueize (valueize, _q200);
																							    {
																							      tree _q200_pops[1];
																							      if (gimple_nop_convert (_q200, _q200_pops, valueize))
																							        {
																								  tree _q210 = _q200_pops[0];
																								  switch (TREE_CODE (_q210))
																								    {
																								    case SSA_NAME:
																								      if (gimple *_d9 = get_def (valueize, _q210))
																								        {
																									  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																									    switch (gimple_assign_rhs_code (_a9))
																									      {
																									      CASE_CONVERT:
																									        {
																										  tree _q220 = gimple_assign_rhs1 (_a9);
																										  _q220 = do_valueize (valueize, _q220);
																										  switch (TREE_CODE (_q220))
																										    {
																										    case SSA_NAME:
																										      if (gimple *_d10 = get_def (valueize, _q220))
																										        {
																											  if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																											    switch (gimple_assign_rhs_code (_a10))
																											      {
																											      case LT_EXPR:
																											        {
																												  tree _q230 = gimple_assign_rhs1 (_a10);
																												  _q230 = do_valueize (valueize, _q230);
																												  tree _q231 = gimple_assign_rhs2 (_a10);
																												  _q231 = do_valueize (valueize, _q231);
																												  if ((_q230 == _q40 && ! TREE_SIDE_EFFECTS (_q230)) || (operand_equal_p (_q230, _q40, 0) && types_match (_q230, _q40)))
																												    {
																												      if (integer_zerop (_q231))
																													{
																													  if (gimple_max_value (_q181, valueize))
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
																															  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																					  }
																				      }
																				      if (gimple_max_value (_q180, valueize))
																					{
																					  {
																					    tree _q181_pops[1];
																					    if (gimple_nop_convert (_q181, _q181_pops, valueize))
																					      {
																						tree _q200 = _q181_pops[0];
																						switch (TREE_CODE (_q200))
																						  {
																						  case SSA_NAME:
																						    if (gimple *_d8 = get_def (valueize, _q200))
																						      {
																							if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																							  switch (gimple_assign_rhs_code (_a8))
																							    {
																							    case NEGATE_EXPR:
																							      {
																								tree _q210 = gimple_assign_rhs1 (_a8);
																								_q210 = do_valueize (valueize, _q210);
																								{
																								  tree _q210_pops[1];
																								  if (gimple_nop_convert (_q210, _q210_pops, valueize))
																								    {
																								      tree _q220 = _q210_pops[0];
																								      switch (TREE_CODE (_q220))
																								        {
																									case SSA_NAME:
																									  if (gimple *_d9 = get_def (valueize, _q220))
																									    {
																									      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																									        switch (gimple_assign_rhs_code (_a9))
																										  {
																										  CASE_CONVERT:
																										    {
																										      tree _q230 = gimple_assign_rhs1 (_a9);
																										      _q230 = do_valueize (valueize, _q230);
																										      switch (TREE_CODE (_q230))
																										        {
																											case SSA_NAME:
																											  if (gimple *_d10 = get_def (valueize, _q230))
																											    {
																											      if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																											        switch (gimple_assign_rhs_code (_a10))
																												  {
																												  case LT_EXPR:
																												    {
																												      tree _q240 = gimple_assign_rhs1 (_a10);
																												      _q240 = do_valueize (valueize, _q240);
																												      tree _q241 = gimple_assign_rhs2 (_a10);
																												      _q241 = do_valueize (valueize, _q241);
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
																															  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																											  break;
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
														        }
														      default:;
														      }
													        }
													      break;
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
									  break;
								        default:;
								        }
								    }
								}
								if (integer_zerop (_q31))
								  {
								    switch (TREE_CODE (_q21))
								      {
								      case SSA_NAME:
								        if (gimple *_d4 = get_def (valueize, _q21))
								          {
									    if (gassign *_a4 = dyn_cast <gassign *> (_d4))
									      switch (gimple_assign_rhs_code (_a4))
										{
										case LT_EXPR:
										  {
										    tree _q80 = gimple_assign_rhs1 (_a4);
										    _q80 = do_valueize (valueize, _q80);
										    tree _q81 = gimple_assign_rhs2 (_a4);
										    _q81 = do_valueize (valueize, _q81);
										    switch (TREE_CODE (_q80))
										      {
										      case SSA_NAME:
										        if (gimple *_d5 = get_def (valueize, _q80))
										          {
											    if (gassign *_a5 = dyn_cast <gassign *> (_d5))
											      switch (gimple_assign_rhs_code (_a5))
												{
												case BIT_XOR_EXPR:
												  {
												    tree _q90 = gimple_assign_rhs1 (_a5);
												    _q90 = do_valueize (valueize, _q90);
												    tree _q91 = gimple_assign_rhs2 (_a5);
												    _q91 = do_valueize (valueize, _q91);
												    if (tree_swap_operands_p (_q90, _q91))
												      std::swap (_q90, _q91);
												    if ((_q90 == _q40 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q40, 0) && types_match (_q90, _q40)))
												      {
													{
													  tree _q91_pops[1];
													  if (gimple_nop_convert (_q91, _q91_pops, valueize))
													    {
													      tree _q110 = _q91_pops[0];
													      switch (TREE_CODE (_q110))
													        {
														case SSA_NAME:
														  if (gimple *_d6 = get_def (valueize, _q110))
														    {
														      if (gassign *_a6 = dyn_cast <gassign *> (_d6))
														        switch (gimple_assign_rhs_code (_a6))
															  {
															  case MINUS_EXPR:
															    {
															      tree _q120 = gimple_assign_rhs1 (_a6);
															      _q120 = do_valueize (valueize, _q120);
															      tree _q121 = gimple_assign_rhs2 (_a6);
															      _q121 = do_valueize (valueize, _q121);
															      {
																tree _q120_pops[1];
																if (gimple_nop_convert (_q120, _q120_pops, valueize))
																  {
																    tree _q130 = _q120_pops[0];
																    if ((_q130 == _q40 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q40, 0) && types_match (_q130, _q40)))
																      {
																	{
																	  tree _q121_pops[1];
																	  if (gimple_nop_convert (_q121, _q121_pops, valueize))
																	    {
																	      tree _q150 = _q121_pops[0];
																	      if ((_q150 == _q41 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q41, 0) && types_match (_q150, _q41)))
																		{
																		  if (integer_zerop (_q81))
																		    {
																		      switch (TREE_CODE (_p1))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d7 = get_def (valueize, _p1))
																			    {
																			      if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			        switch (gimple_assign_rhs_code (_a7))
																				  {
																				  case BIT_XOR_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a7);
																				      _q180 = do_valueize (valueize, _q180);
																				      tree _q181 = gimple_assign_rhs2 (_a7);
																				      _q181 = do_valueize (valueize, _q181);
																				      if (tree_swap_operands_p (_q180, _q181))
																				        std::swap (_q180, _q181);
																				      {
																					tree _q180_pops[1];
																					if (gimple_nop_convert (_q180, _q180_pops, valueize))
																					  {
																					    tree _q190 = _q180_pops[0];
																					    switch (TREE_CODE (_q190))
																					      {
																					      case SSA_NAME:
																					        if (gimple *_d8 = get_def (valueize, _q190))
																					          {
																						    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																						      switch (gimple_assign_rhs_code (_a8))
																							{
																							case NEGATE_EXPR:
																							  {
																							    tree _q200 = gimple_assign_rhs1 (_a8);
																							    _q200 = do_valueize (valueize, _q200);
																							    {
																							      tree _q200_pops[1];
																							      if (gimple_nop_convert (_q200, _q200_pops, valueize))
																							        {
																								  tree _q210 = _q200_pops[0];
																								  switch (TREE_CODE (_q210))
																								    {
																								    case SSA_NAME:
																								      if (gimple *_d9 = get_def (valueize, _q210))
																								        {
																									  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																									    switch (gimple_assign_rhs_code (_a9))
																									      {
																									      CASE_CONVERT:
																									        {
																										  tree _q220 = gimple_assign_rhs1 (_a9);
																										  _q220 = do_valueize (valueize, _q220);
																										  switch (TREE_CODE (_q220))
																										    {
																										    case SSA_NAME:
																										      if (gimple *_d10 = get_def (valueize, _q220))
																										        {
																											  if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																											    switch (gimple_assign_rhs_code (_a10))
																											      {
																											      case LT_EXPR:
																											        {
																												  tree _q230 = gimple_assign_rhs1 (_a10);
																												  _q230 = do_valueize (valueize, _q230);
																												  tree _q231 = gimple_assign_rhs2 (_a10);
																												  _q231 = do_valueize (valueize, _q231);
																												  if ((_q230 == _q40 && ! TREE_SIDE_EFFECTS (_q230)) || (operand_equal_p (_q230, _q40, 0) && types_match (_q230, _q40)))
																												    {
																												      if (integer_zerop (_q231))
																													{
																													  if (gimple_max_value (_q181, valueize))
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
																															  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																					  }
																				      }
																				      if (gimple_max_value (_q180, valueize))
																					{
																					  {
																					    tree _q181_pops[1];
																					    if (gimple_nop_convert (_q181, _q181_pops, valueize))
																					      {
																						tree _q200 = _q181_pops[0];
																						switch (TREE_CODE (_q200))
																						  {
																						  case SSA_NAME:
																						    if (gimple *_d8 = get_def (valueize, _q200))
																						      {
																							if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																							  switch (gimple_assign_rhs_code (_a8))
																							    {
																							    case NEGATE_EXPR:
																							      {
																								tree _q210 = gimple_assign_rhs1 (_a8);
																								_q210 = do_valueize (valueize, _q210);
																								{
																								  tree _q210_pops[1];
																								  if (gimple_nop_convert (_q210, _q210_pops, valueize))
																								    {
																								      tree _q220 = _q210_pops[0];
																								      switch (TREE_CODE (_q220))
																								        {
																									case SSA_NAME:
																									  if (gimple *_d9 = get_def (valueize, _q220))
																									    {
																									      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																									        switch (gimple_assign_rhs_code (_a9))
																										  {
																										  CASE_CONVERT:
																										    {
																										      tree _q230 = gimple_assign_rhs1 (_a9);
																										      _q230 = do_valueize (valueize, _q230);
																										      switch (TREE_CODE (_q230))
																										        {
																											case SSA_NAME:
																											  if (gimple *_d10 = get_def (valueize, _q230))
																											    {
																											      if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																											        switch (gimple_assign_rhs_code (_a10))
																												  {
																												  case LT_EXPR:
																												    {
																												      tree _q240 = gimple_assign_rhs1 (_a10);
																												      _q240 = do_valueize (valueize, _q240);
																												      tree _q241 = gimple_assign_rhs2 (_a10);
																												      _q241 = do_valueize (valueize, _q241);
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
																															  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																											  break;
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
													        default:;
													        }
													    }
													}
												      }
												    if ((_q90 == _q41 && ! TREE_SIDE_EFFECTS (_q90)) || (operand_equal_p (_q90, _q41, 0) && types_match (_q90, _q41)))
												      {
													{
													  tree _q91_pops[1];
													  if (gimple_nop_convert (_q91, _q91_pops, valueize))
													    {
													      tree _q110 = _q91_pops[0];
													      switch (TREE_CODE (_q110))
													        {
														case SSA_NAME:
														  if (gimple *_d6 = get_def (valueize, _q110))
														    {
														      if (gassign *_a6 = dyn_cast <gassign *> (_d6))
														        switch (gimple_assign_rhs_code (_a6))
															  {
															  case MINUS_EXPR:
															    {
															      tree _q120 = gimple_assign_rhs1 (_a6);
															      _q120 = do_valueize (valueize, _q120);
															      tree _q121 = gimple_assign_rhs2 (_a6);
															      _q121 = do_valueize (valueize, _q121);
															      {
																tree _q120_pops[1];
																if (gimple_nop_convert (_q120, _q120_pops, valueize))
																  {
																    tree _q130 = _q120_pops[0];
																    if ((_q130 == _q41 && ! TREE_SIDE_EFFECTS (_q130)) || (operand_equal_p (_q130, _q41, 0) && types_match (_q130, _q41)))
																      {
																	{
																	  tree _q121_pops[1];
																	  if (gimple_nop_convert (_q121, _q121_pops, valueize))
																	    {
																	      tree _q150 = _q121_pops[0];
																	      if ((_q150 == _q40 && ! TREE_SIDE_EFFECTS (_q150)) || (operand_equal_p (_q150, _q40, 0) && types_match (_q150, _q40)))
																		{
																		  if (integer_zerop (_q81))
																		    {
																		      switch (TREE_CODE (_p1))
																		        {
																			case SSA_NAME:
																			  if (gimple *_d7 = get_def (valueize, _p1))
																			    {
																			      if (gassign *_a7 = dyn_cast <gassign *> (_d7))
																			        switch (gimple_assign_rhs_code (_a7))
																				  {
																				  case BIT_XOR_EXPR:
																				    {
																				      tree _q180 = gimple_assign_rhs1 (_a7);
																				      _q180 = do_valueize (valueize, _q180);
																				      tree _q181 = gimple_assign_rhs2 (_a7);
																				      _q181 = do_valueize (valueize, _q181);
																				      if (tree_swap_operands_p (_q180, _q181))
																				        std::swap (_q180, _q181);
																				      {
																					tree _q180_pops[1];
																					if (gimple_nop_convert (_q180, _q180_pops, valueize))
																					  {
																					    tree _q190 = _q180_pops[0];
																					    switch (TREE_CODE (_q190))
																					      {
																					      case SSA_NAME:
																					        if (gimple *_d8 = get_def (valueize, _q190))
																					          {
																						    if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																						      switch (gimple_assign_rhs_code (_a8))
																							{
																							case NEGATE_EXPR:
																							  {
																							    tree _q200 = gimple_assign_rhs1 (_a8);
																							    _q200 = do_valueize (valueize, _q200);
																							    {
																							      tree _q200_pops[1];
																							      if (gimple_nop_convert (_q200, _q200_pops, valueize))
																							        {
																								  tree _q210 = _q200_pops[0];
																								  switch (TREE_CODE (_q210))
																								    {
																								    case SSA_NAME:
																								      if (gimple *_d9 = get_def (valueize, _q210))
																								        {
																									  if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																									    switch (gimple_assign_rhs_code (_a9))
																									      {
																									      CASE_CONVERT:
																									        {
																										  tree _q220 = gimple_assign_rhs1 (_a9);
																										  _q220 = do_valueize (valueize, _q220);
																										  switch (TREE_CODE (_q220))
																										    {
																										    case SSA_NAME:
																										      if (gimple *_d10 = get_def (valueize, _q220))
																										        {
																											  if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																											    switch (gimple_assign_rhs_code (_a10))
																											      {
																											      case LT_EXPR:
																											        {
																												  tree _q230 = gimple_assign_rhs1 (_a10);
																												  _q230 = do_valueize (valueize, _q230);
																												  tree _q231 = gimple_assign_rhs2 (_a10);
																												  _q231 = do_valueize (valueize, _q231);
																												  if ((_q230 == _q41 && ! TREE_SIDE_EFFECTS (_q230)) || (operand_equal_p (_q230, _q41, 0) && types_match (_q230, _q41)))
																												    {
																												      if (integer_zerop (_q231))
																													{
																													  if (gimple_max_value (_q181, valueize))
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
																															  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																					  }
																				      }
																				      if (gimple_max_value (_q180, valueize))
																					{
																					  {
																					    tree _q181_pops[1];
																					    if (gimple_nop_convert (_q181, _q181_pops, valueize))
																					      {
																						tree _q200 = _q181_pops[0];
																						switch (TREE_CODE (_q200))
																						  {
																						  case SSA_NAME:
																						    if (gimple *_d8 = get_def (valueize, _q200))
																						      {
																							if (gassign *_a8 = dyn_cast <gassign *> (_d8))
																							  switch (gimple_assign_rhs_code (_a8))
																							    {
																							    case NEGATE_EXPR:
																							      {
																								tree _q210 = gimple_assign_rhs1 (_a8);
																								_q210 = do_valueize (valueize, _q210);
																								{
																								  tree _q210_pops[1];
																								  if (gimple_nop_convert (_q210, _q210_pops, valueize))
																								    {
																								      tree _q220 = _q210_pops[0];
																								      switch (TREE_CODE (_q220))
																								        {
																									case SSA_NAME:
																									  if (gimple *_d9 = get_def (valueize, _q220))
																									    {
																									      if (gassign *_a9 = dyn_cast <gassign *> (_d9))
																									        switch (gimple_assign_rhs_code (_a9))
																										  {
																										  CASE_CONVERT:
																										    {
																										      tree _q230 = gimple_assign_rhs1 (_a9);
																										      _q230 = do_valueize (valueize, _q230);
																										      switch (TREE_CODE (_q230))
																										        {
																											case SSA_NAME:
																											  if (gimple *_d10 = get_def (valueize, _q230))
																											    {
																											      if (gassign *_a10 = dyn_cast <gassign *> (_d10))
																											        switch (gimple_assign_rhs_code (_a10))
																												  {
																												  case LT_EXPR:
																												    {
																												      tree _q240 = gimple_assign_rhs1 (_a10);
																												      _q240 = do_valueize (valueize, _q240);
																												      tree _q241 = gimple_assign_rhs2 (_a10);
																												      _q241 = do_valueize (valueize, _q241);
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
																															  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 75, __FILE__, __LINE__, false);
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
																											  break;
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
										        break;
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
					      }
					    default:;
					    }
				      }
				    break;
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
    default:;
    }
  return false;
}

bool
gimple_simplify_ROUND_MOD_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_329 (res_op, seq, valueize, type, captures, ROUND_MOD_EXPR))
	  return true;
      }
    }
  if (integer_onep (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_330 (res_op, seq, valueize, type, captures, ROUND_MOD_EXPR))
	  return true;
      }
    }
  if (integer_minus_onep (_p1))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_331 (res_op, seq, valueize, type, captures, ROUND_MOD_EXPR))
	  return true;
      }
    }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_332 (res_op, seq, valueize, type, captures, ROUND_MOD_EXPR))
	  return true;
      }
    }
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case ROUND_MOD_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
		    {
		      {
			tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q21 };
			if (gimple_simplify_333 (res_op, seq, valueize, type, captures, ROUND_MOD_EXPR))
			  return true;
		      }
		    }
	          break;
	        }
	      case MULT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
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
				if (gimple_simplify_334 (res_op, seq, valueize, type, captures, ROUND_MOD_EXPR))
				  return true;
			      }
			      break;
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
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  tree _q22 = gimple_assign_rhs3 (_a1);
		  _q22 = do_valueize (valueize, _q22);
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case VEC_COND_EXPR:
			        {
				  tree _q60 = gimple_assign_rhs1 (_a2);
				  _q60 = do_valueize (valueize, _q60);
				  tree _q61 = gimple_assign_rhs2 (_a2);
				  _q61 = do_valueize (valueize, _q61);
				  tree _q62 = gimple_assign_rhs3 (_a2);
				  _q62 = do_valueize (valueize, _q62);
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
					    gimple_seq *lseq = seq;
					    if (lseq
					        && (!single_use (captures[0])
					            || !single_use (captures[4])))
					      lseq = NULL;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2454;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), ROUND_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2454;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), ROUND_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2454;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2454:;
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
			gimple_seq *lseq = seq;
			if (lseq
			    && (!single_use (captures[0])))
			  lseq = NULL;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2455;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), ROUND_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2455;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), ROUND_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2455;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2455:;
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
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p1))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case VEC_COND_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  tree _q32 = gimple_assign_rhs3 (_a1);
		  _q32 = do_valueize (valueize, _q32);
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
			gimple_seq *lseq = seq;
			if (lseq
			    && (!single_use (captures[1])))
			  lseq = NULL;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2456;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), ROUND_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2456;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), ROUND_MOD_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2456;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2456:;
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
  return false;
}

bool
gimple_simplify_LSHIFT_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p1))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case TRUNC_MOD_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  {
		    tree _q31_pops[1];
		    if (gimple_power_of_two_cand (_q31, _q31_pops, valueize))
		      {
			tree _q50 = _q31_pops[0];
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q30, _q31, _q50 };
			  if (gimple_simplify_383 (res_op, seq, valueize, type, captures, LSHIFT_EXPR, TRUNC_MOD_EXPR))
			    return true;
			}
		      }
		  }
	          break;
	        }
	      case FLOOR_MOD_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  {
		    tree _q31_pops[1];
		    if (gimple_power_of_two_cand (_q31, _q31_pops, valueize))
		      {
			tree _q50 = _q31_pops[0];
			{
			  tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q30, _q31, _q50 };
			  if (gimple_simplify_383 (res_op, seq, valueize, type, captures, LSHIFT_EXPR, FLOOR_MOD_EXPR))
			    return true;
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
if (uniform_integer_cst_p (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (gimple_simplify_384 (res_op, seq, valueize, type, captures, LSHIFT_EXPR))
        return true;
    }
  }
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      CASE_CONVERT:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  switch (TREE_CODE (_q20))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q20))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case LT_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
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
						      gimple_seq *lseq = seq;
						      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2461;
						      {
							res_op->set_op (BIT_AND_EXPR, type, 2);
							{
							  tree _o1[1], _r1;
							  _o1[0] = captures[0];
							  if (type != TREE_TYPE (_o1[0])
							      && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
							    {
							      gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
							      tem_op.resimplify (lseq, valueize);
							      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
							      if (!_r1) goto next_after_fail2461;
							    }
							  else
							    _r1 = _o1[0];
							  res_op->ops[0] = _r1;
							}
							res_op->ops[1] =  wide_int_to_tree (type,
 wi::lshift (wone, wi::to_wide (captures[2])));
							res_op->resimplify (lseq, valueize);
							if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1109, __FILE__, __LINE__, true);
							return true;
						      }
next_after_fail2461:;
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
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  switch (TREE_CODE (_q30))
				    {
				    case SSA_NAME:
				      if (gimple *_d3 = get_def (valueize, _q30))
				        {
					  if (gassign *_a3 = dyn_cast <gassign *> (_d3))
					    switch (gimple_assign_rhs_code (_a3))
					      {
					      case RSHIFT_EXPR:
					        {
						  tree _q40 = gimple_assign_rhs1 (_a3);
						  _q40 = do_valueize (valueize, _q40);
						  tree _q41 = gimple_assign_rhs2 (_a3);
						  _q41 = do_valueize (valueize, _q41);
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
								  gimple_seq *lseq = seq;
								  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2462;
								  {
								    res_op->set_op (BIT_AND_EXPR, type, 2);
								    {
								      tree _o1[1], _r1;
								      _o1[0] = captures[1];
								      if (type != TREE_TYPE (_o1[0])
								          && !useless_type_conversion_p (type, TREE_TYPE (_o1[0])))
									{
									  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, type, _o1[0]);
									  tem_op.resimplify (lseq, valueize);
									  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
									  if (!_r1) goto next_after_fail2462;
								        }
								      else
								        _r1 = _o1[0];
								      res_op->ops[0] = _r1;
								    }
								    {
								      tree _o1[2], _r1;
								      _o1[0] =  build_minus_one_cst (type);
								      _o1[1] = captures[2];
								      gimple_match_op tem_op (res_op->cond.any_else (), LSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
								      tem_op.resimplify (lseq, valueize);
								      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
								      if (!_r1) goto next_after_fail2462;
								      res_op->ops[1] = _r1;
								    }
								    res_op->resimplify (lseq, valueize);
								    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1110, __FILE__, __LINE__, true);
								    return true;
								  }
next_after_fail2462:;
								}
							    }
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
			        }
			      default:;
			      }
		        }
		      break;
		    default:;
		    }
	          break;
	        }
	      case RSHIFT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  switch (TREE_CODE (_q21))
		    {
		    case INTEGER_CST:
		      {
			if ((_p1 == _q21 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q21, 0) && types_match (_p1, _q21)))
			  {
			    {
			      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q21 };
			      if (gimple_simplify_385 (res_op, seq, valueize, type, captures))
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
    default:;
    }
{
  tree _p0_pops[1];
  if (gimple_nop_convert (_p0, _p0_pops, valueize))
    {
      tree _q20 = _p0_pops[0];
      switch (TREE_CODE (_q20))
        {
	case SSA_NAME:
	  if (gimple *_d1 = get_def (valueize, _q20))
	    {
	      if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	        switch (gimple_assign_rhs_code (_a1))
		  {
		  case RSHIFT_EXPR:
		    {
		      tree _q30 = gimple_assign_rhs1 (_a1);
		      _q30 = do_valueize (valueize, _q30);
		      tree _q31 = gimple_assign_rhs2 (_a1);
		      _q31 = do_valueize (valueize, _q31);
		      switch (TREE_CODE (_q31))
		        {
			case INTEGER_CST:
			  {
			    if ((_p1 == _q31 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _q31, 0) && types_match (_p1, _q31)))
			      {
				{
				  tree captures[2] ATTRIBUTE_UNUSED = { _q30, _q31 };
				  if (gimple_simplify_385 (res_op, seq, valueize, type, captures))
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
        default:;
        }
    }
}
if (integer_onep (_p0))
  {
    switch (TREE_CODE (_p1))
      {
      case SSA_NAME:
        if (gimple *_d1 = get_def (valueize, _p1))
          {
	    if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	      switch (gimple_assign_rhs_code (_a1))
		{
		case MINUS_EXPR:
		  {
		    tree _q30 = gimple_assign_rhs1 (_a1);
		    _q30 = do_valueize (valueize, _q30);
		    tree _q31 = gimple_assign_rhs2 (_a1);
		    _q31 = do_valueize (valueize, _q31);
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
				    gimple_seq *lseq = seq;
				    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2463;
				    {
				      res_op->set_op (RSHIFT_EXPR, type, 2);
				      {
					tree _o1[2], _r1;
					_o1[0] = captures[0];
					_o1[1] = captures[2];
					gimple_match_op tem_op (res_op->cond.any_else (), LSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					tem_op.resimplify (lseq, valueize);
					_r1 = maybe_push_res_to_seq (&tem_op, lseq);
					if (!_r1) goto next_after_fail2463;
					res_op->ops[0] = _r1;
				      }
				      res_op->ops[1] = captures[3];
				      res_op->resimplify (lseq, valueize);
				      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1111, __FILE__, __LINE__, true);
				      return true;
				    }
next_after_fail2463:;
				  }
				else
				  {
				    {
 tree utype = unsigned_type_for (type);
					gimple_seq *lseq = seq;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2464;
					{
					  res_op->set_op (NOP_EXPR, type, 1);
					  {
					    tree _o1[2], _r1;
					    {
					      tree _o2[2], _r2;
					      {
						tree _o3[1], _r3;
						_o3[0] = captures[0];
						if (utype != TREE_TYPE (_o3[0])
						    && !useless_type_conversion_p (utype, TREE_TYPE (_o3[0])))
						  {
						    gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, utype, _o3[0]);
						    tem_op.resimplify (lseq, valueize);
						    _r3 = maybe_push_res_to_seq (&tem_op, lseq);
						    if (!_r3) goto next_after_fail2464;
						  }
						else
						  _r3 = _o3[0];
						_o2[0] = _r3;
					      }
					      _o2[1] = captures[2];
					      gimple_match_op tem_op (res_op->cond.any_else (), LSHIFT_EXPR, TREE_TYPE (_o2[0]), _o2[0], _o2[1]);
					      tem_op.resimplify (lseq, valueize);
					      _r2 = maybe_push_res_to_seq (&tem_op, lseq);
					      if (!_r2) goto next_after_fail2464;
					      _o1[0] = _r2;
					    }
					    _o1[1] = captures[3];
					    gimple_match_op tem_op (res_op->cond.any_else (), RSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail2464;
					    res_op->ops[0] = _r1;
					  }
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1112, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail2464:;
				    }
				  }
			      }
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
  }
  if (integer_zerop (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_386 (res_op, seq, valueize, type, captures, LSHIFT_EXPR))
	  return true;
      }
    }
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_387 (res_op, seq, valueize, type, captures, LSHIFT_EXPR))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p1))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case CONSTRUCTOR:
	        {
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    if (gimple_simplify_389 (res_op, seq, valueize, type, captures, LSHIFT_EXPR))
		      return true;
		  }
	          break;
	        }
	      default:;
	      }
        }
	{
	  {
	    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	    if (gimple_simplify_390 (res_op, seq, valueize, type, captures, LSHIFT_EXPR))
	      return true;
	  }
	}
      break;
    case VECTOR_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_388 (res_op, seq, valueize, type, captures, LSHIFT_EXPR))
	    return true;
	}
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case LSHIFT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
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
				if (gimple_simplify_391 (res_op, seq, valueize, type, captures, LSHIFT_EXPR))
				  return true;
			      }
			      break;
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
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  switch (TREE_CODE (_q20))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q20))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case BIT_AND_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
				  if (tree_swap_operands_p (_q30, _q31))
				    std::swap (_q30, _q31);
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
						if (gimple_simplify_392 (res_op, seq, valueize, type, captures, BIT_AND_EXPR, LSHIFT_EXPR))
						  return true;
					      }
					      break;
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
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
				  if (tree_swap_operands_p (_q30, _q31))
				    std::swap (_q30, _q31);
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
						if (gimple_simplify_392 (res_op, seq, valueize, type, captures, BIT_XOR_EXPR, LSHIFT_EXPR))
						  return true;
					      }
					      break;
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
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
				  if (tree_swap_operands_p (_q30, _q31))
				    std::swap (_q30, _q31);
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
						if (gimple_simplify_392 (res_op, seq, valueize, type, captures, BIT_IOR_EXPR, LSHIFT_EXPR))
						  return true;
					      }
					      break;
					    }
				          default:;
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
	        }
	      case BIT_AND_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
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
				if (gimple_simplify_393 (res_op, seq, valueize, type, captures, BIT_AND_EXPR, LSHIFT_EXPR))
				  return true;
			      }
			      break;
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
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
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
				if (gimple_simplify_393 (res_op, seq, valueize, type, captures, BIT_XOR_EXPR, LSHIFT_EXPR))
				  return true;
			      }
			      break;
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
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  if (tree_swap_operands_p (_q20, _q21))
		    std::swap (_q20, _q21);
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
				if (gimple_simplify_393 (res_op, seq, valueize, type, captures, BIT_IOR_EXPR, LSHIFT_EXPR))
				  return true;
			      }
			      break;
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
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  tree _q22 = gimple_assign_rhs3 (_a1);
		  _q22 = do_valueize (valueize, _q22);
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case VEC_COND_EXPR:
			        {
				  tree _q60 = gimple_assign_rhs1 (_a2);
				  _q60 = do_valueize (valueize, _q60);
				  tree _q61 = gimple_assign_rhs2 (_a2);
				  _q61 = do_valueize (valueize, _q61);
				  tree _q62 = gimple_assign_rhs3 (_a2);
				  _q62 = do_valueize (valueize, _q62);
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
					    gimple_seq *lseq = seq;
					    if (lseq
					        && (!single_use (captures[0])
					            || !single_use (captures[4])))
					      lseq = NULL;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2465;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), LSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2465;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), LSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2465;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2465:;
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
			gimple_seq *lseq = seq;
			if (lseq
			    && (!single_use (captures[0])))
			  lseq = NULL;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2466;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), LSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2466;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), LSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2466;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2466:;
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
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p1))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case VEC_COND_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  tree _q32 = gimple_assign_rhs3 (_a1);
		  _q32 = do_valueize (valueize, _q32);
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
			gimple_seq *lseq = seq;
			if (lseq
			    && (!single_use (captures[1])))
			  lseq = NULL;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2467;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), LSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2467;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), LSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2467;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2467:;
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
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case VEC_PERM_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  tree _q22 = gimple_assign_rhs3 (_a1);
		  _q22 = do_valueize (valueize, _q22);
		  if ((_q21 == _q20 && ! TREE_SIDE_EFFECTS (_q21)) || (operand_equal_p (_q21, _q20, 0) && types_match (_q21, _q20)))
		    {
		      switch (TREE_CODE (_p1))
		        {
			case SSA_NAME:
			  if (gimple *_d2 = get_def (valueize, _p1))
			    {
			      if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			        switch (gimple_assign_rhs_code (_a2))
				  {
				  case VEC_PERM_EXPR:
				    {
				      tree _q60 = gimple_assign_rhs1 (_a2);
				      _q60 = do_valueize (valueize, _q60);
				      tree _q61 = gimple_assign_rhs2 (_a2);
				      _q61 = do_valueize (valueize, _q61);
				      tree _q62 = gimple_assign_rhs3 (_a2);
				      _q62 = do_valueize (valueize, _q62);
				      if ((_q61 == _q60 && ! TREE_SIDE_EFFECTS (_q61)) || (operand_equal_p (_q61, _q60, 0) && types_match (_q61, _q60)))
					{
					  if ((_q62 == _q22 && ! TREE_SIDE_EFFECTS (_q62)) || (operand_equal_p (_q62, _q22, 0) && types_match (_q62, _q22)))
					    {
					      {
						tree captures[4] ATTRIBUTE_UNUSED = { _q20, _q22, _q60 };
						if (VECTOR_INTEGER_TYPE_P (type)
)
						  {
						    gimple_seq *lseq = seq;
						    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2468;
						    {
						      res_op->set_op (VEC_PERM_EXPR, type, 3);
						      {
							tree _o1[2], _r1;
							_o1[0] = captures[0];
							_o1[1] = captures[2];
							gimple_match_op tem_op (res_op->cond.any_else (), LSHIFT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
							tem_op.resimplify (lseq, valueize);
							_r1 = maybe_push_res_to_seq (&tem_op, lseq);
							if (!_r1) goto next_after_fail2468;
							captures[3] = _r1;
						      }
						      res_op->ops[0] = captures[3];
						      res_op->ops[1] = captures[3];
						      res_op->ops[2] = captures[1];
						      res_op->resimplify (lseq, valueize);
						      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1001, __FILE__, __LINE__, true);
						      return true;
						    }
next_after_fail2468:;
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
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_POWL (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  switch (TREE_CODE (_p1))
		    {
		    case REAL_CST:
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			  const combined_fn pows = CFN_BUILT_IN_POWL;
			  {
 HOST_WIDE_INT n;
			      if (real_isinteger (&TREE_REAL_CST (captures[1]), &n) && (n & 1) == 0
)
				{
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2509;
				  {
				    res_op->set_op (pows, type, 2);
				    res_op->ops[0] = captures[0];
				    res_op->ops[1] = captures[1];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1117, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail2509:;
				}
			  }
			}
		        break;
		      }
		    default:;
		    }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  switch (TREE_CODE (_p1))
		    {
		    case REAL_CST:
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			  const combined_fn pows = CFN_BUILT_IN_POWL;
			  {
 HOST_WIDE_INT n;
			      if (real_isinteger (&TREE_REAL_CST (captures[1]), &n) && (n & 1) == 0
)
				{
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2510;
				  {
				    res_op->set_op (pows, type, 2);
				    res_op->ops[0] = captures[0];
				    res_op->ops[1] = captures[1];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1117, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail2510:;
				}
			  }
			}
		        break;
		      }
		    default:;
		    }
	          break;
	        }
	      default:;
	      }
	  else if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_SQRTL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		      if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2511;
			  {
			    res_op->set_op (CFN_BUILT_IN_POWL, type, 2);
			    res_op->ops[0] = captures[0];
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[1];
			      _o1[1] =  build_real (type, dconsthalf);
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2511;
			      res_op->ops[1] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1118, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2511:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_EXP10L:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2512;
			  {
			    res_op->set_op (CFN_BUILT_IN_EXP10L, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] = captures[1];
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2512;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1119, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2512:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_EXPL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2513;
			  {
			    res_op->set_op (CFN_BUILT_IN_EXPL, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] = captures[1];
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2513;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1119, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2513:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_POWL:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    if (tree_expr_nonnegative_p (_q20))
		      {
			{
			  tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			  if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2514;
			      {
				res_op->set_op (CFN_BUILT_IN_POWL, type, 2);
				res_op->ops[0] = captures[0];
				{
				  tree _o1[2], _r1;
				  _o1[0] = captures[1];
				  _o1[1] = captures[2];
				  gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				  tem_op.resimplify (lseq, valueize);
				  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r1) goto next_after_fail2514;
				  res_op->ops[1] = _r1;
				}
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1120, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail2514:;
			    }
			}
		      }
	          }
	        break;
	      case CFN_BUILT_IN_POW10L:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2515;
			  {
			    res_op->set_op (CFN_BUILT_IN_POW10L, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] = captures[1];
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2515;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1119, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2515:;
			}
		    }
	          }
	        break;
	      case CFN_BUILT_IN_COPYSIGN:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    switch (TREE_CODE (_p1))
		      {
		      case REAL_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    if (gimple_simplify_412 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGN, CFN_BUILT_IN_POWL))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_COPYSIGNF:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    switch (TREE_CODE (_p1))
		      {
		      case REAL_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    if (gimple_simplify_412 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF, CFN_BUILT_IN_POWL))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_COPYSIGNL:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    switch (TREE_CODE (_p1))
		      {
		      case REAL_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    if (gimple_simplify_412 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNL, CFN_BUILT_IN_POWL))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      case CFN_BUILT_IN_CBRTL:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    if (tree_expr_nonnegative_p (_q20))
		      {
			{
			  tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
			  if (flag_unsafe_math_optimizations && canonicalize_math_p ()
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2516;
			      {
				res_op->set_op (CFN_BUILT_IN_POWL, type, 2);
				res_op->ops[0] = captures[0];
				{
				  tree _o1[2], _r1;
				  _o1[0] = captures[1];
				  _o1[1] =  build_real_truncate (type, dconst_third ());
				  gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
				  tem_op.resimplify (lseq, valueize);
				  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
				  if (!_r1) goto next_after_fail2516;
				  res_op->ops[1] = _r1;
				}
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1121, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail2516:;
			    }
			}
		      }
	          }
	        break;
	      case CFN_BUILT_IN_EXP2L:
	        if (gimple_call_num_args (_c1) == 1)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		      if (flag_unsafe_math_optimizations
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2517;
			  {
			    res_op->set_op (CFN_BUILT_IN_EXP2L, type, 1);
			    {
			      tree _o1[2], _r1;
			      _o1[0] = captures[0];
			      _o1[1] = captures[1];
			      gimple_match_op tem_op (res_op->cond.any_else (), MULT_EXPR, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
			      tem_op.resimplify (lseq, valueize);
			      _r1 = maybe_push_res_to_seq (&tem_op, lseq);
			      if (!_r1) goto next_after_fail2517;
			      res_op->ops[0] = _r1;
			    }
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1119, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2517:;
			}
		    }
	          }
	        break;
	      case CFN_COPYSIGN:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    switch (TREE_CODE (_p1))
		      {
		      case REAL_CST:
		        {
			  {
			    tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
			    if (gimple_simplify_412 (res_op, seq, valueize, type, captures, CFN_COPYSIGN, CFN_BUILT_IN_POWL))
			      return true;
			  }
		          break;
		        }
		      default:;
		      }
	          }
	        break;
	      default:;
	      }
        }
      break;
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_413 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_POWL, CFN_BUILT_IN_EXPL, CFN_BUILT_IN_LOGL, CFN_BUILT_IN_EXP2L, CFN_BUILT_IN_LOG2L))
	    return true;
	}
        break;
      }
    default:;
    }
if (real_onep (_p0))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (gimple_simplify_414 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_POWL))
        return true;
    }
  }
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  {
 const REAL_VALUE_TYPE *value = TREE_REAL_CST_PTR (captures[1]);
 REAL_VALUE_TYPE tmp;
	      if (real_equal (value, &dconst0)
)
		{
		  gimple_seq *lseq = seq;
		  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2518;
		  {
		    tree tem;
		    tem =  build_real (type, dconst1);
		    res_op->set_value (tem);
		    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1122, __FILE__, __LINE__, true);
		    return true;
		  }
next_after_fail2518:;
		}
	      else
		{
		  if (real_equal (value, &dconst1)
)
		    {
		      gimple_seq *lseq = seq;
		      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2519;
		      {
			tree tem;
			tem = captures[0];
			res_op->set_value (tem);
			if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1123, __FILE__, __LINE__, true);
			return true;
		      }
next_after_fail2519:;
		    }
		  else
		    {
		      if (real_equal (value, &dconstm1)
)
			{
			  gimple_seq *lseq = seq;
			  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2520;
			  {
			    res_op->set_op (RDIV_EXPR, type, 2);
			    res_op->ops[0] =  build_real (type, dconst1);
			    res_op->ops[1] = captures[0];
			    res_op->resimplify (lseq, valueize);
			    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1124, __FILE__, __LINE__, true);
			    return true;
			  }
next_after_fail2520:;
			}
		      else
			{
			  if (flag_unsafe_math_optimizations
 && canonicalize_math_p ()
 && real_equal (value, &dconsthalf)
)
			    {
			      gimple_seq *lseq = seq;
			      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2521;
			      {
				res_op->set_op (CFN_BUILT_IN_SQRTL, type, 1);
				res_op->ops[0] = captures[0];
				res_op->resimplify (lseq, valueize);
				if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1125, __FILE__, __LINE__, true);
				return true;
			      }
next_after_fail2521:;
			    }
			  else
			    {
			      if (flag_unsafe_math_optimizations
 && canonicalize_math_p ()
 && (tmp = real_value_truncate (TYPE_MODE (type), dconst_third ()),
 real_equal (value, &tmp))
)
				{
				  gimple_seq *lseq = seq;
				  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2522;
				  {
				    res_op->set_op (CFN_BUILT_IN_CBRTL, type, 1);
				    res_op->ops[0] = captures[0];
				    res_op->resimplify (lseq, valueize);
				    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1126, __FILE__, __LINE__, true);
				    return true;
				  }
next_after_fail2522:;
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
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_HYPOT (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		    const combined_fn hypots = CFN_BUILT_IN_HYPOT;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2561;
		    {
		      res_op->set_op (hypots, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1132, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2561:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		    const combined_fn hypots = CFN_BUILT_IN_HYPOT;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2562;
		    {
		      res_op->set_op (hypots, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1132, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2562:;
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p1))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
		    const combined_fn hypots = CFN_BUILT_IN_HYPOT;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2563;
		    {
		      res_op->set_op (hypots, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1133, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2563:;
		  }
	          break;
	        }
	      case ABS_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _q30 };
		    const combined_fn hypots = CFN_BUILT_IN_HYPOT;
		    gimple_seq *lseq = seq;
		    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2564;
		    {
		      res_op->set_op (hypots, type, 2);
		      res_op->ops[0] = captures[0];
		      res_op->ops[1] = captures[1];
		      res_op->resimplify (lseq, valueize);
		      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1133, __FILE__, __LINE__, true);
		      return true;
		    }
next_after_fail2564:;
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_COPYSIGN:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      if (gimple_simplify_416 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGN, CFN_BUILT_IN_HYPOT))
		        return true;
		    }
	          }
	        break;
	      case CFN_BUILT_IN_COPYSIGNF:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      if (gimple_simplify_416 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF, CFN_BUILT_IN_HYPOT))
		        return true;
		    }
	          }
	        break;
	      case CFN_BUILT_IN_COPYSIGNL:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      if (gimple_simplify_416 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNL, CFN_BUILT_IN_HYPOT))
		        return true;
		    }
	          }
	        break;
	      case CFN_COPYSIGN:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q20 = gimple_call_arg (_c1, 0);
		    _q20 = do_valueize (valueize, _q20);
		    tree _q21 = gimple_call_arg (_c1, 1);
		    _q21 = do_valueize (valueize, _q21);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _q20, _q21, _p1 };
		      if (gimple_simplify_416 (res_op, seq, valueize, type, captures, CFN_COPYSIGN, CFN_BUILT_IN_HYPOT))
		        return true;
		    }
	          }
	        break;
	      default:;
	      }
        }
      break;
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p1))
        {
	  if (gcall *_c1 = dyn_cast <gcall *> (_d1))
	    switch (gimple_call_combined_fn (_c1))
	      {
	      case CFN_BUILT_IN_COPYSIGN:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    tree _q31 = gimple_call_arg (_c1, 1);
		    _q31 = do_valueize (valueize, _q31);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q31 };
		      if (gimple_simplify_417 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGN, CFN_BUILT_IN_HYPOT))
		        return true;
		    }
	          }
	        break;
	      case CFN_BUILT_IN_COPYSIGNF:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    tree _q31 = gimple_call_arg (_c1, 1);
		    _q31 = do_valueize (valueize, _q31);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q31 };
		      if (gimple_simplify_417 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNF, CFN_BUILT_IN_HYPOT))
		        return true;
		    }
	          }
	        break;
	      case CFN_BUILT_IN_COPYSIGNL:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    tree _q31 = gimple_call_arg (_c1, 1);
		    _q31 = do_valueize (valueize, _q31);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q31 };
		      if (gimple_simplify_417 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_COPYSIGNL, CFN_BUILT_IN_HYPOT))
		        return true;
		    }
	          }
	        break;
	      case CFN_COPYSIGN:
	        if (gimple_call_num_args (_c1) == 2)
	          {
		    tree _q30 = gimple_call_arg (_c1, 0);
		    _q30 = do_valueize (valueize, _q30);
		    tree _q31 = gimple_call_arg (_c1, 1);
		    _q31 = do_valueize (valueize, _q31);
		    {
		      tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q30, _q31 };
		      if (gimple_simplify_417 (res_op, seq, valueize, type, captures, CFN_COPYSIGN, CFN_BUILT_IN_HYPOT))
		        return true;
		    }
	          }
	        break;
	      default:;
	      }
        }
      break;
    default:;
    }
if (real_zerop (_p1))
  {
    {
      tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
      if (gimple_simplify_418 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_HYPOT))
        return true;
    }
  }
  if (real_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p1, _p0 };
	if (gimple_simplify_418 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_HYPOT))
	  return true;
      }
    }
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_419 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_HYPOT))
	  return true;
      }
    }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_FMIN (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_504 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMIN))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMIN))
	    return true;
	}
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p1, _p0 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMIN))
	    return true;
	}
        break;
      }
    default:;
    }
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (gimple_simplify_506 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMIN))
      return true;
  }
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case NEGATE_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  {
				    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
				    if (FLOAT_TYPE_P (TREE_TYPE (captures[1]))
 || (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[1])))
)
				      {
					gimple_seq *lseq = seq;
					if (lseq
					    && (!single_use (captures[0])
					        || !single_use (captures[2])))
					  lseq = NULL;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2609;
					{
					  res_op->set_op (NEGATE_EXPR, type, 1);
					  {
					    tree _o1[2], _r1;
					    _o1[0] = captures[1];
					    _o1[1] = captures[3];
					    gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_FMAX, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail2609;
					    res_op->ops[0] = _r1;
					  }
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1148, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail2609:;
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
	        }
	      default:;
	      }
        }
      break;
    default:;
    }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_FMINF64 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_504 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINF64))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINF64))
	    return true;
	}
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p1, _p0 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINF64))
	    return true;
	}
        break;
      }
    default:;
    }
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (gimple_simplify_506 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINF64))
      return true;
  }
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case NEGATE_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  {
				    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
				    if (FLOAT_TYPE_P (TREE_TYPE (captures[1]))
 || (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[1])))
)
				      {
					gimple_seq *lseq = seq;
					if (lseq
					    && (!single_use (captures[0])
					        || !single_use (captures[2])))
					  lseq = NULL;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2613;
					{
					  res_op->set_op (NEGATE_EXPR, type, 1);
					  {
					    tree _o1[2], _r1;
					    _o1[0] = captures[1];
					    _o1[1] = captures[3];
					    gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_FMAXF64, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail2613;
					    res_op->ops[0] = _r1;
					  }
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1148, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail2613:;
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
	        }
	      default:;
	      }
        }
      break;
    default:;
    }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_FMINF128X (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_504 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINF128X))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINF128X))
	    return true;
	}
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p1, _p0 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINF128X))
	    return true;
	}
        break;
      }
    default:;
    }
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (gimple_simplify_506 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMINF128X))
      return true;
  }
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case NEGATE_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  {
				    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
				    if (FLOAT_TYPE_P (TREE_TYPE (captures[1]))
 || (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[1])))
)
				      {
					gimple_seq *lseq = seq;
					if (lseq
					    && (!single_use (captures[0])
					        || !single_use (captures[2])))
					  lseq = NULL;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2617;
					{
					  res_op->set_op (NEGATE_EXPR, type, 1);
					  {
					    tree _o1[2], _r1;
					    _o1[0] = captures[1];
					    _o1[1] = captures[3];
					    gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_FMAXF128X, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail2617;
					    res_op->ops[0] = _r1;
					  }
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1148, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail2617:;
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
	        }
	      default:;
	      }
        }
      break;
    default:;
    }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_FMAXF16 (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_504 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXF16))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXF16))
	    return true;
	}
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p1, _p0 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXF16))
	    return true;
	}
        break;
      }
    default:;
    }
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (gimple_simplify_507 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXF16))
      return true;
  }
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case NEGATE_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  {
				    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
				    if (FLOAT_TYPE_P (TREE_TYPE (captures[1]))
 || (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[1])))
)
				      {
					gimple_seq *lseq = seq;
					if (lseq
					    && (!single_use (captures[0])
					        || !single_use (captures[2])))
					  lseq = NULL;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2622;
					{
					  res_op->set_op (NEGATE_EXPR, type, 1);
					  {
					    tree _o1[2], _r1;
					    _o1[0] = captures[1];
					    _o1[1] = captures[3];
					    gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_FMINF16, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail2622;
					    res_op->ops[0] = _r1;
					  }
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1148, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail2622:;
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
	        }
	      default:;
	      }
        }
      break;
    default:;
    }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_FMAXF32X (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if ((_p1 == _p0 && ! TREE_SIDE_EFFECTS (_p1)) || (operand_equal_p (_p1, _p0, 0) && types_match (_p1, _p0)))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_504 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXF32X))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXF32X))
	    return true;
	}
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p1, _p0 };
	  if (gimple_simplify_505 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXF32X))
	    return true;
	}
        break;
      }
    default:;
    }
  {
    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
    if (gimple_simplify_507 (res_op, seq, valueize, type, captures, CFN_BUILT_IN_FMAXF32X))
      return true;
  }
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case NEGATE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case NEGATE_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  {
				    tree captures[4] ATTRIBUTE_UNUSED = { _p0, _q20, _p1, _q40 };
				    if (FLOAT_TYPE_P (TREE_TYPE (captures[1]))
 || (ANY_INTEGRAL_TYPE_P (TREE_TYPE (captures[1]))
 && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (captures[1])))
)
				      {
					gimple_seq *lseq = seq;
					if (lseq
					    && (!single_use (captures[0])
					        || !single_use (captures[2])))
					  lseq = NULL;
					if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2626;
					{
					  res_op->set_op (NEGATE_EXPR, type, 1);
					  {
					    tree _o1[2], _r1;
					    _o1[0] = captures[1];
					    _o1[1] = captures[3];
					    gimple_match_op tem_op (res_op->cond.any_else (), CFN_BUILT_IN_FMINF32X, TREE_TYPE (_o1[0]), _o1[0], _o1[1]);
					    tem_op.resimplify (lseq, valueize);
					    _r1 = maybe_push_res_to_seq (&tem_op, lseq);
					    if (!_r1) goto next_after_fail2626;
					    res_op->ops[0] = _r1;
					  }
					  res_op->resimplify (lseq, valueize);
					  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1148, __FILE__, __LINE__, true);
					  return true;
					}
next_after_fail2626:;
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
	        }
	      default:;
	      }
        }
      break;
    default:;
    }
  return false;
}

bool
gimple_simplify_LROTATE_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (integer_all_onesp (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_588 (res_op, seq, valueize, type, captures, LROTATE_EXPR))
	  return true;
      }
    }
  if (integer_zerop (_p1))
    {
      {
	tree captures[1] ATTRIBUTE_UNUSED = { _p0 };
	if (gimple_simplify_386 (res_op, seq, valueize, type, captures, LROTATE_EXPR))
	  return true;
      }
    }
  if (integer_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_387 (res_op, seq, valueize, type, captures, LROTATE_EXPR))
	  return true;
      }
    }
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p1))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case CONSTRUCTOR:
	        {
		  {
		    tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
		    if (gimple_simplify_389 (res_op, seq, valueize, type, captures, LROTATE_EXPR))
		      return true;
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    case VECTOR_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_388 (res_op, seq, valueize, type, captures, LROTATE_EXPR))
	    return true;
	}
        break;
      }
    case INTEGER_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  gimple_seq *lseq = seq;
	  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2630;
	  {
	    res_op->set_op (RROTATE_EXPR, type, 2);
	    res_op->ops[0] = captures[0];
	    res_op->ops[1] =  const_binop (MINUS_EXPR, TREE_TYPE (captures[1]),
 build_int_cst (TREE_TYPE (captures[1]),
 element_precision (type)), captures[1]);
	    res_op->resimplify (lseq, valueize);
	    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1153, __FILE__, __LINE__, true);
	    return true;
	  }
next_after_fail2630:;
	}
        break;
      }
    default:;
    }
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case LROTATE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
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
				if (gimple_simplify_391 (res_op, seq, valueize, type, captures, LROTATE_EXPR))
				  return true;
			      }
			      break;
			    }
		          default:;
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
  return false;
}

bool
gimple_simplify_ORDERED_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case VEC_COND_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  tree _q22 = gimple_assign_rhs3 (_a1);
		  _q22 = do_valueize (valueize, _q22);
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case VEC_COND_EXPR:
			        {
				  tree _q60 = gimple_assign_rhs1 (_a2);
				  _q60 = do_valueize (valueize, _q60);
				  tree _q61 = gimple_assign_rhs2 (_a2);
				  _q61 = do_valueize (valueize, _q61);
				  tree _q62 = gimple_assign_rhs3 (_a2);
				  _q62 = do_valueize (valueize, _q62);
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
					    gimple_seq *lseq = seq;
					    if (lseq
					        && (!single_use (captures[0])
					            || !single_use (captures[4])))
					      lseq = NULL;
					    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2637;
					    {
					      res_op->set_op (VEC_COND_EXPR, type, 3);
					      res_op->ops[0] = captures[1];
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[2];
						_o1[1] = captures[5];
						gimple_match_op tem_op (res_op->cond.any_else (), ORDERED_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2637;
						res_op->ops[1] = _r1;
					      }
					      {
						tree _o1[2], _r1;
						_o1[0] = captures[3];
						_o1[1] = captures[6];
						gimple_match_op tem_op (res_op->cond.any_else (), ORDERED_EXPR, type, _o1[0], _o1[1]);
						tem_op.resimplify (NULL, valueize);
						_r1 = maybe_push_res_to_seq (&tem_op, NULL);
						if (!_r1) goto next_after_fail2637;
						res_op->ops[2] = _r1;
					      }
					      res_op->resimplify (lseq, valueize);
					      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 998, __FILE__, __LINE__, true);
					      return true;
					    }
next_after_fail2637:;
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
			gimple_seq *lseq = seq;
			if (lseq
			    && (!single_use (captures[0])))
			  lseq = NULL;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2638;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[1];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[2];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), ORDERED_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2638;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[3];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), ORDERED_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2638;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 999, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2638:;
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
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p1))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case VEC_COND_EXPR:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  tree _q31 = gimple_assign_rhs2 (_a1);
		  _q31 = do_valueize (valueize, _q31);
		  tree _q32 = gimple_assign_rhs3 (_a1);
		  _q32 = do_valueize (valueize, _q32);
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
			gimple_seq *lseq = seq;
			if (lseq
			    && (!single_use (captures[1])))
			  lseq = NULL;
			if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2639;
			{
			  res_op->set_op (VEC_COND_EXPR, type, 3);
			  res_op->ops[0] = captures[2];
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[3];
			    gimple_match_op tem_op (res_op->cond.any_else (), ORDERED_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2639;
			    res_op->ops[1] = _r1;
			  }
			  {
			    tree _o1[2], _r1;
			    _o1[0] = captures[0];
			    _o1[1] = captures[4];
			    gimple_match_op tem_op (res_op->cond.any_else (), ORDERED_EXPR, type, _o1[0], _o1[1]);
			    tem_op.resimplify (NULL, valueize);
			    _r1 = maybe_push_res_to_seq (&tem_op, NULL);
			    if (!_r1) goto next_after_fail2639;
			    res_op->ops[2] = _r1;
			  }
			  res_op->resimplify (lseq, valueize);
			  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1000, __FILE__, __LINE__, true);
			  return true;
			}
next_after_fail2639:;
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
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case FLOAT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case FLOAT_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  {
				    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _q40 };
				    if (gimple_simplify_265 (res_op, seq, valueize, type, captures, ORDERED_EXPR, ORDERED_EXPR))
				      return true;
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
	        }
	      case NEGATE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  switch (TREE_CODE (_p1))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _p1))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case NEGATE_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  {
				    tree captures[2] ATTRIBUTE_UNUSED = { _q20, _q40 };
				    if (gimple_simplify_268 (res_op, seq, valueize, type, captures, ORDERED_EXPR, ORDERED_EXPR))
				      return true;
				  }
			          break;
			        }
			      default:;
			      }
		        }
		      break;
		    default:;
		    }
		if (CONSTANT_CLASS_P (_p1))
		  {
		    {
		      tree captures[2] ATTRIBUTE_UNUSED = { _q20, _p1 };
		      if (gimple_simplify_269 (res_op, seq, valueize, type, captures, ORDERED_EXPR, ORDERED_EXPR))
		        return true;
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
  switch (TREE_CODE (_p1))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (gimple_simplify_614 (res_op, seq, valueize, type, captures, ORDERED_EXPR))
	    return true;
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
	gimple_seq *lseq = seq;
	if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2640;
	{
	  tree tem;
	  tem =  constant_boolean_node (false, type);
	  res_op->set_value (tem);
	  if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1157, __FILE__, __LINE__, true);
	  return true;
	}
next_after_fail2640:;
      }
    else
      {
	if (!tree_expr_maybe_nan_p (captures[0]) && !tree_expr_maybe_nan_p (captures[1])
)
	  {
	    gimple_seq *lseq = seq;
	    if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2641;
	    {
	      tree tem;
	      tem =  constant_boolean_node (true, type);
	      res_op->set_value (tem);
	      if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1158, __FILE__, __LINE__, true);
	      return true;
	    }
next_after_fail2641:;
	  }
      }
  }
  return false;
}

bool
gimple_simplify_CFN_SUB_OVERFLOW (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      CASE_CONVERT:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _q20, _p1 };
		    if (gimple_simplify_617 (res_op, seq, valueize, type, captures, CFN_SUB_OVERFLOW))
		      return true;
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    default:;
    }
  switch (TREE_CODE (_p1))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p1))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      CASE_CONVERT:
	        {
		  tree _q30 = gimple_assign_rhs1 (_a1);
		  _q30 = do_valueize (valueize, _q30);
		  {
		    tree captures[3] ATTRIBUTE_UNUSED = { _p0, _p1, _q30 };
		    if (gimple_simplify_618 (res_op, seq, valueize, type, captures, CFN_SUB_OVERFLOW))
		      return true;
		  }
	          break;
	        }
	      default:;
	      }
        }
      break;
    default:;
    }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_LDEXP (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (real_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_630 (res_op, seq, valueize, type, captures))
	  return true;
      }
    }
  if (integer_zerop (_p1))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_631 (res_op, seq, valueize, type, captures))
	  return true;
      }
    }
  switch (TREE_CODE (_p0))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (!real_isfinite (TREE_REAL_CST_PTR (captures[0]))
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2662;
	      {
		tree tem;
		tem = captures[0];
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1160, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail2662:;
	    }
	}
        break;
      }
    default:;
    }
  return false;
}

bool
gimple_simplify_CFN_BUILT_IN_SCALBN (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  if (real_zerop (_p0))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_630 (res_op, seq, valueize, type, captures))
	  return true;
      }
    }
  if (integer_zerop (_p1))
    {
      {
	tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	if (gimple_simplify_631 (res_op, seq, valueize, type, captures))
	  return true;
      }
    }
  switch (TREE_CODE (_p0))
    {
    case REAL_CST:
      {
	{
	  tree captures[2] ATTRIBUTE_UNUSED = { _p0, _p1 };
	  if (!real_isfinite (TREE_REAL_CST_PTR (captures[0]))
)
	    {
	      gimple_seq *lseq = seq;
	      if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2666;
	      {
		tree tem;
		tem = captures[0];
		res_op->set_value (tem);
		if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1160, __FILE__, __LINE__, true);
		return true;
	      }
next_after_fail2666:;
	    }
	}
        break;
      }
    default:;
    }
  return false;
}

bool
gimple_simplify_TRUTH_ORIF_EXPR (gimple_match_op *res_op, gimple_seq *seq,
                 tree (*valueize)(tree) ATTRIBUTE_UNUSED,
                 code_helper ARG_UNUSED (code), tree ARG_UNUSED (type), tree _p0, tree _p1)
{
  const bool debug_dump = dump_file && (dump_flags & TDF_FOLDING);
  switch (TREE_CODE (_p0))
    {
    case SSA_NAME:
      if (gimple *_d1 = get_def (valueize, _p0))
        {
	  if (gassign *_a1 = dyn_cast <gassign *> (_d1))
	    switch (gimple_assign_rhs_code (_a1))
	      {
	      case LE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  switch (TREE_CODE (_q20))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q20))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case POINTER_PLUS_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
				  switch (TREE_CODE (_q31))
				    {
				    case INTEGER_CST:
				      {
					switch (TREE_CODE (_p1))
					  {
					  case SSA_NAME:
					    if (gimple *_d3 = get_def (valueize, _p1))
					      {
						if (gassign *_a3 = dyn_cast <gassign *> (_d3))
						  switch (gimple_assign_rhs_code (_a3))
						    {
						    case LE_EXPR:
						      {
							tree _q70 = gimple_assign_rhs1 (_a3);
							_q70 = do_valueize (valueize, _q70);
							tree _q71 = gimple_assign_rhs2 (_a3);
							_q71 = do_valueize (valueize, _q71);
							switch (TREE_CODE (_q70))
							  {
							  case SSA_NAME:
							    if (gimple *_d4 = get_def (valueize, _q70))
							      {
								if (gassign *_a4 = dyn_cast <gassign *> (_d4))
								  switch (gimple_assign_rhs_code (_a4))
								    {
								    case POINTER_PLUS_EXPR:
								      {
									tree _q80 = gimple_assign_rhs1 (_a4);
									_q80 = do_valueize (valueize, _q80);
									tree _q81 = gimple_assign_rhs2 (_a4);
									_q81 = do_valueize (valueize, _q81);
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
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2671;
													  {
													    res_op->set_op (GT_EXPR, type, 2);
													    {
													      tree _o1[1], _r1;
													      {
														tree _o2[2], _r2;
														_o2[0] =  swap_p ? captures[6] : captures[1];
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2671;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2671;
													        }
													      else
													        _r1 = _o1[0];
													      res_op->ops[0] = _r1;
													    }
													    res_op->ops[1] =  rhs_tree;
													    res_op->resimplify (lseq, valueize);
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1046, __FILE__, __LINE__, true);
													    return true;
													  }
next_after_fail2671:;
													}
												      else
													{
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2672;
													  {
													    res_op->set_op (GT_EXPR, type, 2);
													    {
													      tree _o1[1], _r1;
													      {
														tree _o2[2], _r2;
														{
														  tree _o3[2], _r3;
														  _o3[0] =  swap_p ? captures[4] : captures[2];
														  _o3[1] =  wide_int_to_tree (sizetype, off);
														  gimple_match_op tem_op (res_op->cond.any_else (), POINTER_PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
														  tem_op.resimplify (lseq, valueize);
														  _r3 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r3) goto next_after_fail2672;
														  _o2[0] = _r3;
														}
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2672;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2672;
													        }
													      else
													        _r1 = _o1[0];
													      res_op->ops[0] = _r1;
													    }
													    res_op->ops[1] =  rhs_tree;
													    res_op->resimplify (lseq, valueize);
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1047, __FILE__, __LINE__, true);
													    return true;
													  }
next_after_fail2672:;
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
							    break;
						          default:;
						          }
						        break;
						      }
						    case GE_EXPR:
						      {
							tree _q70 = gimple_assign_rhs1 (_a3);
							_q70 = do_valueize (valueize, _q70);
							tree _q71 = gimple_assign_rhs2 (_a3);
							_q71 = do_valueize (valueize, _q71);
							if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
							  {
							    switch (TREE_CODE (_q71))
							      {
							      case SSA_NAME:
							        if (gimple *_d4 = get_def (valueize, _q71))
							          {
								    if (gassign *_a4 = dyn_cast <gassign *> (_d4))
								      switch (gimple_assign_rhs_code (_a4))
									{
									case POINTER_PLUS_EXPR:
									  {
									    tree _q90 = gimple_assign_rhs1 (_a4);
									    _q90 = do_valueize (valueize, _q90);
									    tree _q91 = gimple_assign_rhs2 (_a4);
									    _q91 = do_valueize (valueize, _q91);
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
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2673;
													  {
													    res_op->set_op (GT_EXPR, type, 2);
													    {
													      tree _o1[1], _r1;
													      {
														tree _o2[2], _r2;
														_o2[0] =  swap_p ? captures[6] : captures[1];
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2673;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2673;
													        }
													      else
													        _r1 = _o1[0];
													      res_op->ops[0] = _r1;
													    }
													    res_op->ops[1] =  rhs_tree;
													    res_op->resimplify (lseq, valueize);
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1046, __FILE__, __LINE__, true);
													    return true;
													  }
next_after_fail2673:;
													}
												      else
													{
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2674;
													  {
													    res_op->set_op (GT_EXPR, type, 2);
													    {
													      tree _o1[1], _r1;
													      {
														tree _o2[2], _r2;
														{
														  tree _o3[2], _r3;
														  _o3[0] =  swap_p ? captures[4] : captures[2];
														  _o3[1] =  wide_int_to_tree (sizetype, off);
														  gimple_match_op tem_op (res_op->cond.any_else (), POINTER_PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
														  tem_op.resimplify (lseq, valueize);
														  _r3 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r3) goto next_after_fail2674;
														  _o2[0] = _r3;
														}
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2674;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2674;
													        }
													      else
													        _r1 = _o1[0];
													      res_op->ops[0] = _r1;
													    }
													    res_op->ops[1] =  rhs_tree;
													    res_op->resimplify (lseq, valueize);
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1047, __FILE__, __LINE__, true);
													    return true;
													  }
next_after_fail2674:;
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
	        }
	      case LT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  switch (TREE_CODE (_q20))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q20))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case POINTER_PLUS_EXPR:
			        {
				  tree _q30 = gimple_assign_rhs1 (_a2);
				  _q30 = do_valueize (valueize, _q30);
				  tree _q31 = gimple_assign_rhs2 (_a2);
				  _q31 = do_valueize (valueize, _q31);
				  switch (TREE_CODE (_q31))
				    {
				    case INTEGER_CST:
				      {
					switch (TREE_CODE (_p1))
					  {
					  case SSA_NAME:
					    if (gimple *_d3 = get_def (valueize, _p1))
					      {
						if (gassign *_a3 = dyn_cast <gassign *> (_d3))
						  switch (gimple_assign_rhs_code (_a3))
						    {
						    case LT_EXPR:
						      {
							tree _q70 = gimple_assign_rhs1 (_a3);
							_q70 = do_valueize (valueize, _q70);
							tree _q71 = gimple_assign_rhs2 (_a3);
							_q71 = do_valueize (valueize, _q71);
							switch (TREE_CODE (_q70))
							  {
							  case SSA_NAME:
							    if (gimple *_d4 = get_def (valueize, _q70))
							      {
								if (gassign *_a4 = dyn_cast <gassign *> (_d4))
								  switch (gimple_assign_rhs_code (_a4))
								    {
								    case POINTER_PLUS_EXPR:
								      {
									tree _q80 = gimple_assign_rhs1 (_a4);
									_q80 = do_valueize (valueize, _q80);
									tree _q81 = gimple_assign_rhs2 (_a4);
									_q81 = do_valueize (valueize, _q81);
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
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2675;
													  {
													    res_op->set_op (GT_EXPR, type, 2);
													    {
													      tree _o1[1], _r1;
													      {
														tree _o2[2], _r2;
														_o2[0] =  swap_p ? captures[6] : captures[1];
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2675;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2675;
													        }
													      else
													        _r1 = _o1[0];
													      res_op->ops[0] = _r1;
													    }
													    res_op->ops[1] =  rhs_tree;
													    res_op->resimplify (lseq, valueize);
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1046, __FILE__, __LINE__, true);
													    return true;
													  }
next_after_fail2675:;
													}
												      else
													{
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2676;
													  {
													    res_op->set_op (GT_EXPR, type, 2);
													    {
													      tree _o1[1], _r1;
													      {
														tree _o2[2], _r2;
														{
														  tree _o3[2], _r3;
														  _o3[0] =  swap_p ? captures[4] : captures[2];
														  _o3[1] =  wide_int_to_tree (sizetype, off);
														  gimple_match_op tem_op (res_op->cond.any_else (), POINTER_PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
														  tem_op.resimplify (lseq, valueize);
														  _r3 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r3) goto next_after_fail2676;
														  _o2[0] = _r3;
														}
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2676;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2676;
													        }
													      else
													        _r1 = _o1[0];
													      res_op->ops[0] = _r1;
													    }
													    res_op->ops[1] =  rhs_tree;
													    res_op->resimplify (lseq, valueize);
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1047, __FILE__, __LINE__, true);
													    return true;
													  }
next_after_fail2676:;
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
							    break;
						          default:;
						          }
						        break;
						      }
						    case GT_EXPR:
						      {
							tree _q70 = gimple_assign_rhs1 (_a3);
							_q70 = do_valueize (valueize, _q70);
							tree _q71 = gimple_assign_rhs2 (_a3);
							_q71 = do_valueize (valueize, _q71);
							if ((_q70 == _q30 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q30, 0) && types_match (_q70, _q30)))
							  {
							    switch (TREE_CODE (_q71))
							      {
							      case SSA_NAME:
							        if (gimple *_d4 = get_def (valueize, _q71))
							          {
								    if (gassign *_a4 = dyn_cast <gassign *> (_d4))
								      switch (gimple_assign_rhs_code (_a4))
									{
									case POINTER_PLUS_EXPR:
									  {
									    tree _q90 = gimple_assign_rhs1 (_a4);
									    _q90 = do_valueize (valueize, _q90);
									    tree _q91 = gimple_assign_rhs2 (_a4);
									    _q91 = do_valueize (valueize, _q91);
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
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2677;
													  {
													    res_op->set_op (GT_EXPR, type, 2);
													    {
													      tree _o1[1], _r1;
													      {
														tree _o2[2], _r2;
														_o2[0] =  swap_p ? captures[6] : captures[1];
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2677;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2677;
													        }
													      else
													        _r1 = _o1[0];
													      res_op->ops[0] = _r1;
													    }
													    res_op->ops[1] =  rhs_tree;
													    res_op->resimplify (lseq, valueize);
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1046, __FILE__, __LINE__, true);
													    return true;
													  }
next_after_fail2677:;
													}
												      else
													{
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2678;
													  {
													    res_op->set_op (GT_EXPR, type, 2);
													    {
													      tree _o1[1], _r1;
													      {
														tree _o2[2], _r2;
														{
														  tree _o3[2], _r3;
														  _o3[0] =  swap_p ? captures[4] : captures[2];
														  _o3[1] =  wide_int_to_tree (sizetype, off);
														  gimple_match_op tem_op (res_op->cond.any_else (), POINTER_PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
														  tem_op.resimplify (lseq, valueize);
														  _r3 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r3) goto next_after_fail2678;
														  _o2[0] = _r3;
														}
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2678;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2678;
													        }
													      else
													        _r1 = _o1[0];
													      res_op->ops[0] = _r1;
													    }
													    res_op->ops[1] =  rhs_tree;
													    res_op->resimplify (lseq, valueize);
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1047, __FILE__, __LINE__, true);
													    return true;
													  }
next_after_fail2678:;
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
	        }
	      case GE_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  switch (TREE_CODE (_q21))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q21))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case POINTER_PLUS_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  switch (TREE_CODE (_q41))
				    {
				    case INTEGER_CST:
				      {
					switch (TREE_CODE (_p1))
					  {
					  case SSA_NAME:
					    if (gimple *_d3 = get_def (valueize, _p1))
					      {
						if (gassign *_a3 = dyn_cast <gassign *> (_d3))
						  switch (gimple_assign_rhs_code (_a3))
						    {
						    case LE_EXPR:
						      {
							tree _q70 = gimple_assign_rhs1 (_a3);
							_q70 = do_valueize (valueize, _q70);
							tree _q71 = gimple_assign_rhs2 (_a3);
							_q71 = do_valueize (valueize, _q71);
							switch (TREE_CODE (_q70))
							  {
							  case SSA_NAME:
							    if (gimple *_d4 = get_def (valueize, _q70))
							      {
								if (gassign *_a4 = dyn_cast <gassign *> (_d4))
								  switch (gimple_assign_rhs_code (_a4))
								    {
								    case POINTER_PLUS_EXPR:
								      {
									tree _q80 = gimple_assign_rhs1 (_a4);
									_q80 = do_valueize (valueize, _q80);
									tree _q81 = gimple_assign_rhs2 (_a4);
									_q81 = do_valueize (valueize, _q81);
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
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2679;
													  {
													    res_op->set_op (GT_EXPR, type, 2);
													    {
													      tree _o1[1], _r1;
													      {
														tree _o2[2], _r2;
														_o2[0] =  swap_p ? captures[6] : captures[1];
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2679;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2679;
													        }
													      else
													        _r1 = _o1[0];
													      res_op->ops[0] = _r1;
													    }
													    res_op->ops[1] =  rhs_tree;
													    res_op->resimplify (lseq, valueize);
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1046, __FILE__, __LINE__, true);
													    return true;
													  }
next_after_fail2679:;
													}
												      else
													{
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2680;
													  {
													    res_op->set_op (GT_EXPR, type, 2);
													    {
													      tree _o1[1], _r1;
													      {
														tree _o2[2], _r2;
														{
														  tree _o3[2], _r3;
														  _o3[0] =  swap_p ? captures[4] : captures[2];
														  _o3[1] =  wide_int_to_tree (sizetype, off);
														  gimple_match_op tem_op (res_op->cond.any_else (), POINTER_PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
														  tem_op.resimplify (lseq, valueize);
														  _r3 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r3) goto next_after_fail2680;
														  _o2[0] = _r3;
														}
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2680;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2680;
													        }
													      else
													        _r1 = _o1[0];
													      res_op->ops[0] = _r1;
													    }
													    res_op->ops[1] =  rhs_tree;
													    res_op->resimplify (lseq, valueize);
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1047, __FILE__, __LINE__, true);
													    return true;
													  }
next_after_fail2680:;
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
							    break;
						          default:;
						          }
						        break;
						      }
						    case GE_EXPR:
						      {
							tree _q70 = gimple_assign_rhs1 (_a3);
							_q70 = do_valueize (valueize, _q70);
							tree _q71 = gimple_assign_rhs2 (_a3);
							_q71 = do_valueize (valueize, _q71);
							if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
							  {
							    switch (TREE_CODE (_q71))
							      {
							      case SSA_NAME:
							        if (gimple *_d4 = get_def (valueize, _q71))
							          {
								    if (gassign *_a4 = dyn_cast <gassign *> (_d4))
								      switch (gimple_assign_rhs_code (_a4))
									{
									case POINTER_PLUS_EXPR:
									  {
									    tree _q90 = gimple_assign_rhs1 (_a4);
									    _q90 = do_valueize (valueize, _q90);
									    tree _q91 = gimple_assign_rhs2 (_a4);
									    _q91 = do_valueize (valueize, _q91);
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
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2681;
													  {
													    res_op->set_op (GT_EXPR, type, 2);
													    {
													      tree _o1[1], _r1;
													      {
														tree _o2[2], _r2;
														_o2[0] =  swap_p ? captures[6] : captures[1];
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2681;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2681;
													        }
													      else
													        _r1 = _o1[0];
													      res_op->ops[0] = _r1;
													    }
													    res_op->ops[1] =  rhs_tree;
													    res_op->resimplify (lseq, valueize);
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1046, __FILE__, __LINE__, true);
													    return true;
													  }
next_after_fail2681:;
													}
												      else
													{
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2682;
													  {
													    res_op->set_op (GT_EXPR, type, 2);
													    {
													      tree _o1[1], _r1;
													      {
														tree _o2[2], _r2;
														{
														  tree _o3[2], _r3;
														  _o3[0] =  swap_p ? captures[4] : captures[2];
														  _o3[1] =  wide_int_to_tree (sizetype, off);
														  gimple_match_op tem_op (res_op->cond.any_else (), POINTER_PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
														  tem_op.resimplify (lseq, valueize);
														  _r3 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r3) goto next_after_fail2682;
														  _o2[0] = _r3;
														}
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2682;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2682;
													        }
													      else
													        _r1 = _o1[0];
													      res_op->ops[0] = _r1;
													    }
													    res_op->ops[1] =  rhs_tree;
													    res_op->resimplify (lseq, valueize);
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1047, __FILE__, __LINE__, true);
													    return true;
													  }
next_after_fail2682:;
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
	        }
	      case GT_EXPR:
	        {
		  tree _q20 = gimple_assign_rhs1 (_a1);
		  _q20 = do_valueize (valueize, _q20);
		  tree _q21 = gimple_assign_rhs2 (_a1);
		  _q21 = do_valueize (valueize, _q21);
		  switch (TREE_CODE (_q21))
		    {
		    case SSA_NAME:
		      if (gimple *_d2 = get_def (valueize, _q21))
		        {
			  if (gassign *_a2 = dyn_cast <gassign *> (_d2))
			    switch (gimple_assign_rhs_code (_a2))
			      {
			      case POINTER_PLUS_EXPR:
			        {
				  tree _q40 = gimple_assign_rhs1 (_a2);
				  _q40 = do_valueize (valueize, _q40);
				  tree _q41 = gimple_assign_rhs2 (_a2);
				  _q41 = do_valueize (valueize, _q41);
				  switch (TREE_CODE (_q41))
				    {
				    case INTEGER_CST:
				      {
					switch (TREE_CODE (_p1))
					  {
					  case SSA_NAME:
					    if (gimple *_d3 = get_def (valueize, _p1))
					      {
						if (gassign *_a3 = dyn_cast <gassign *> (_d3))
						  switch (gimple_assign_rhs_code (_a3))
						    {
						    case LT_EXPR:
						      {
							tree _q70 = gimple_assign_rhs1 (_a3);
							_q70 = do_valueize (valueize, _q70);
							tree _q71 = gimple_assign_rhs2 (_a3);
							_q71 = do_valueize (valueize, _q71);
							switch (TREE_CODE (_q70))
							  {
							  case SSA_NAME:
							    if (gimple *_d4 = get_def (valueize, _q70))
							      {
								if (gassign *_a4 = dyn_cast <gassign *> (_d4))
								  switch (gimple_assign_rhs_code (_a4))
								    {
								    case POINTER_PLUS_EXPR:
								      {
									tree _q80 = gimple_assign_rhs1 (_a4);
									_q80 = do_valueize (valueize, _q80);
									tree _q81 = gimple_assign_rhs2 (_a4);
									_q81 = do_valueize (valueize, _q81);
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
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2683;
													  {
													    res_op->set_op (GT_EXPR, type, 2);
													    {
													      tree _o1[1], _r1;
													      {
														tree _o2[2], _r2;
														_o2[0] =  swap_p ? captures[6] : captures[1];
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2683;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2683;
													        }
													      else
													        _r1 = _o1[0];
													      res_op->ops[0] = _r1;
													    }
													    res_op->ops[1] =  rhs_tree;
													    res_op->resimplify (lseq, valueize);
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1046, __FILE__, __LINE__, true);
													    return true;
													  }
next_after_fail2683:;
													}
												      else
													{
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2684;
													  {
													    res_op->set_op (GT_EXPR, type, 2);
													    {
													      tree _o1[1], _r1;
													      {
														tree _o2[2], _r2;
														{
														  tree _o3[2], _r3;
														  _o3[0] =  swap_p ? captures[4] : captures[2];
														  _o3[1] =  wide_int_to_tree (sizetype, off);
														  gimple_match_op tem_op (res_op->cond.any_else (), POINTER_PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
														  tem_op.resimplify (lseq, valueize);
														  _r3 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r3) goto next_after_fail2684;
														  _o2[0] = _r3;
														}
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2684;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2684;
													        }
													      else
													        _r1 = _o1[0];
													      res_op->ops[0] = _r1;
													    }
													    res_op->ops[1] =  rhs_tree;
													    res_op->resimplify (lseq, valueize);
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1047, __FILE__, __LINE__, true);
													    return true;
													  }
next_after_fail2684:;
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
							    break;
						          default:;
						          }
						        break;
						      }
						    case GT_EXPR:
						      {
							tree _q70 = gimple_assign_rhs1 (_a3);
							_q70 = do_valueize (valueize, _q70);
							tree _q71 = gimple_assign_rhs2 (_a3);
							_q71 = do_valueize (valueize, _q71);
							if ((_q70 == _q40 && ! TREE_SIDE_EFFECTS (_q70)) || (operand_equal_p (_q70, _q40, 0) && types_match (_q70, _q40)))
							  {
							    switch (TREE_CODE (_q71))
							      {
							      case SSA_NAME:
							        if (gimple *_d4 = get_def (valueize, _q71))
							          {
								    if (gassign *_a4 = dyn_cast <gassign *> (_d4))
								      switch (gimple_assign_rhs_code (_a4))
									{
									case POINTER_PLUS_EXPR:
									  {
									    tree _q90 = gimple_assign_rhs1 (_a4);
									    _q90 = do_valueize (valueize, _q90);
									    tree _q91 = gimple_assign_rhs2 (_a4);
									    _q91 = do_valueize (valueize, _q91);
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
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2685;
													  {
													    res_op->set_op (GT_EXPR, type, 2);
													    {
													      tree _o1[1], _r1;
													      {
														tree _o2[2], _r2;
														_o2[0] =  swap_p ? captures[6] : captures[1];
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2685;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2685;
													        }
													      else
													        _r1 = _o1[0];
													      res_op->ops[0] = _r1;
													    }
													    res_op->ops[1] =  rhs_tree;
													    res_op->resimplify (lseq, valueize);
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1046, __FILE__, __LINE__, true);
													    return true;
													  }
next_after_fail2685:;
													}
												      else
													{
													  gimple_seq *lseq = seq;
													  if (lseq
													      && (!single_use (captures[0])
													          || !single_use (captures[5])))
													    lseq = NULL;
													  if (UNLIKELY (!dbg_cnt (match))) goto next_after_fail2686;
													  {
													    res_op->set_op (GT_EXPR, type, 2);
													    {
													      tree _o1[1], _r1;
													      {
														tree _o2[2], _r2;
														{
														  tree _o3[2], _r3;
														  _o3[0] =  swap_p ? captures[4] : captures[2];
														  _o3[1] =  wide_int_to_tree (sizetype, off);
														  gimple_match_op tem_op (res_op->cond.any_else (), POINTER_PLUS_EXPR, TREE_TYPE (_o3[0]), _o3[0], _o3[1]);
														  tem_op.resimplify (lseq, valueize);
														  _r3 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r3) goto next_after_fail2686;
														  _o2[0] = _r3;
														}
														_o2[1] =  swap_p ? captures[2] : captures[4];
														gimple_match_op tem_op (res_op->cond.any_else (), POINTER_DIFF_EXPR, ssizetype, _o2[0], _o2[1]);
														tem_op.resimplify (lseq, valueize);
														_r2 = maybe_push_res_to_seq (&tem_op, lseq);
														if (!_r2) goto next_after_fail2686;
														_o1[0] = _r2;
													      }
													      if (sizetype != TREE_TYPE (_o1[0])
													          && !useless_type_conversion_p (sizetype, TREE_TYPE (_o1[0])))
														{
														  gimple_match_op tem_op (res_op->cond.any_else (), NOP_EXPR, sizetype, _o1[0]);
														  tem_op.resimplify (lseq, valueize);
														  _r1 = maybe_push_res_to_seq (&tem_op, lseq);
														  if (!_r1) goto next_after_fail2686;
													        }
													      else
													        _r1 = _o1[0];
													      res_op->ops[0] = _r1;
													    }
													    res_op->ops[1] =  rhs_tree;
													    res_op->resimplify (lseq, valueize);
													    if (UNLIKELY (debug_dump)) gimple_dump_logs ("match.pd", 1047, __FILE__, __LINE__, true);
													    return true;
													  }
next_after_fail2686:;
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
	        }
	      default:;
	      }
        }
      break;
    default:;
    }
  return false;
}
#pragma GCC diagnostic pop
